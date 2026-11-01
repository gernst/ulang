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

const struct u_name n_parse_46_ParseError = {"parse", "ParseError", 0};

const struct u_name n_parse_46_Scope = {"parse", "Scope", 0};

const struct u_name n_parse_46_Operand = {"parse", "Operand", 0};

const struct u_name n_parse_46_Operator = {"parse", "Operator", 1};

const struct u_name n_parse_46_Ops = {"parse", "Ops", 0};

const struct u_name n_parse_46_Loader = {"parse", "Loader", 0};

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

u_value w_f_check_46_funs_named_2(u_value *env, u_value *args);

u_value w_f_check_46_constrs_named_2(u_value *env, u_value *args);

u_value w_f_check_46_procs_named_2(u_value *env, u_value *args);

u_value w_f_check_46_sorts_named_2(u_value *env, u_value *args);

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

u_value w_f_parse_46_Scope_46_params_1(u_value *env, u_value *args);

u_value w_f_parse_46_Scope_46_notations_1(u_value *env, u_value *args);

u_value w_f_parse_46_keywords_0(u_value *env, u_value *args);

u_value w_f_parse_46_is_keyword_1(u_value *env, u_value *args);

u_value w_f_parse_46_is_reserved_1(u_value *env, u_value *args);

u_value w_f_parse_46_prefix_prec_2(u_value *env, u_value *args);

u_value w_f_parse_46_postfix_prec_2(u_value *env, u_value *args);

u_value w_f_parse_46_infix_of_2(u_value *env, u_value *args);

u_value w_f_parse_46_declared_2(u_value *env, u_value *args);

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

u_value w_f_parse_46_Loader_46_modules_1(u_value *env, u_value *args);

u_value w_f_parse_46_Loader_46_loading_1(u_value *env, u_value *args);

u_value w_f_parse_46_Loader_46_base_1(u_value *env, u_value *args);

u_value w_f_parse_46_Loader_46_dir_1(u_value *env, u_value *args);

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

u_value wc_parse_46_ParseError(u_value *env, u_value *args);

u_value wc_parse_46_Scope(u_value *env, u_value *args);

u_value wc_parse_46_Operand(u_value *env, u_value *args);

u_value wc_parse_46_Operator(u_value *env, u_value *args);

u_value wc_parse_46_Ops(u_value *env, u_value *args);

u_value wc_parse_46_Loader(u_value *env, u_value *args);

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

u_value wu_error(u_value *env, u_value *args);

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

u_value f_check_46_funs_named_2(u_value v_c, u_value v_n);

u_value f_check_46_constrs_named_2(u_value v_c, u_value v_n);

u_value f_check_46_procs_named_2(u_value v_c, u_value v_n);

u_value f_check_46_sorts_named_2(u_value v_c, u_value v_n);

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

u_value f_parse_46_Scope_46_params_1(u_value v_x);

u_value f_parse_46_Scope_46_notations_1(u_value v_x);

u_value f_parse_46_keywords_0(void);

u_value f_parse_46_is_keyword_1(u_value v_n);

u_value f_parse_46_is_reserved_1(u_value v_s);

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op);

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op);

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op);

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op);

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

u_value f_parse_46_Loader_46_modules_1(u_value v_x);

u_value f_parse_46_Loader_46_loading_1(u_value v_x);

u_value f_parse_46_Loader_46_base_1(u_value v_x);

u_value f_parse_46_Loader_46_dir_1(u_value v_x);

u_value f_parse_46_modules_of_1(u_value v_c);

u_value f_parse_46_own_notations_2(u_value v_ld, u_value v_n);

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

void p_parse_46_parse_command(u_value v_ns, u_value v_ld, u_value *r_cs, u_value *r_ts);

void p_parse_46_declare_all(u_value v_cs, u_value *r_ns, u_value *r_ld);

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_ns, u_value *r_ld, u_value *r_ts);

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text);

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text);

void p_parse_46_load_module(u_value v_n, u_value v_ns, u_value v_ld, u_value v_at, u_value *r_cs);

void p_parse_46_parse_text(u_value v_text, u_value v_ns, u_value v_ld, u_value *r_p);

void p_parse_46_prelude_scope(u_value v_pre, u_value *r_ns, u_value *r_ms);

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

u_value lam993(u_value *env, u_value *args);

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

u_value w_f_check_46_funs_named_2(u_value *env, u_value *args) {
  return f_check_46_funs_named_2(args[0], args[1]);
}

u_value w_f_check_46_constrs_named_2(u_value *env, u_value *args) {
  return f_check_46_constrs_named_2(args[0], args[1]);
}

u_value w_f_check_46_procs_named_2(u_value *env, u_value *args) {
  return f_check_46_procs_named_2(args[0], args[1]);
}

u_value w_f_check_46_sorts_named_2(u_value *env, u_value *args) {
  return f_check_46_sorts_named_2(args[0], args[1]);
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

u_value w_f_parse_46_Loader_46_modules_1(u_value *env, u_value *args) {
  return f_parse_46_Loader_46_modules_1(args[0]);
}

u_value w_f_parse_46_Loader_46_loading_1(u_value *env, u_value *args) {
  return f_parse_46_Loader_46_loading_1(args[0]);
}

u_value w_f_parse_46_Loader_46_base_1(u_value *env, u_value *args) {
  return f_parse_46_Loader_46_base_1(args[0]);
}

u_value w_f_parse_46_Loader_46_dir_1(u_value *env, u_value *args) {
  return f_parse_46_Loader_46_dir_1(args[0]);
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

u_value wc_parse_46_ParseError(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_ParseError, 2, args);
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

u_value wc_parse_46_Loader(u_value *env, u_value *args) {
  return u_mk_con_array(&n_parse_46_Loader, 4, args);
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

u_value wu_error(u_value *env, u_value *args) {
  return u_error(args[0]);
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

u_value f_list_46_distinct_1(u_value v_xs) {
  u_value s29 = v_xs;
  u_value t30 = NULL;
  if ((!t30) && u_is_con(s29, &n_list_46_Nil)) {
    t30 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t30) && u_is_con(s29, &n_list_46_Cons)) {
    u_value v_y = u_field(s29, 0);
    u_value v_ys = u_field(s29, 1);
    u_value t31 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e32 = &t31;
    u_value l33 = v_ys;
    while (u_is_con(l33, &n_list_46_Cons)) {
      u_value v_z = u_field(l33, 0);
      if (u_truth(f_prelude_46__33__61__2(v_z, v_y))) {
        e32 = u_append(e32, v_z);
      }
      l33 = u_field(l33, 1);
    }
    t30 = u_mk_con(&n_list_46_Cons, 2, v_y, f_list_46_distinct_1(t31));
  }
  if (!t30) {
    u_no_case(s29);
  }
  return t30;
}

u_value f_list_46_repeated_1(u_value v_xs) {
  u_value s34 = v_xs;
  u_value t35 = NULL;
  if ((!t35) && u_is_con(s34, &n_list_46_Nil)) {
    t35 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t35) && u_is_con(s34, &n_list_46_Cons)) {
    u_value v_y = u_field(s34, 0);
    u_value v_ys = u_field(s34, 1);
    u_value t36;
    if (u_truth(f_list_46_contains_2(v_y, v_ys))) {
      t36 = u_mk_con(&n_option_46_Some, 1, v_y);
    } else {
      t36 = f_list_46_repeated_1(v_ys);
    }
    t35 = t36;
  }
  if (!t35) {
    u_no_case(s34);
  }
  return t35;
}

u_value f_list_46_diff_2(u_value v_xs, u_value v_ys) {
  u_value t37 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e38 = &t37;
  u_value l39 = v_xs;
  while (u_is_con(l39, &n_list_46_Cons)) {
    u_value v_x = u_field(l39, 0);
    if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_x, v_ys)))) {
      e38 = u_append(e38, v_x);
    }
    l39 = u_field(l39, 1);
  }
  return t37;
}

u_value f_list_46_concat_1(u_value v_xss) {
  u_value s40 = v_xss;
  u_value t41 = NULL;
  if ((!t41) && u_is_con(s40, &n_list_46_Nil)) {
    t41 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t41) && u_is_con(s40, &n_list_46_Cons)) {
    u_value v_xs = u_field(s40, 0);
    u_value v_yss = u_field(s40, 1);
    t41 = f_list_46__43__43__2(v_xs, f_list_46_concat_1(v_yss));
  }
  if (!t41) {
    u_no_case(s40);
  }
  return t41;
}

u_value f_list_46_reverse_onto_2(u_value v_xs, u_value v_ys) {
  u_value s42 = v_xs;
  u_value t43 = NULL;
  if ((!t43) && u_is_con(s42, &n_list_46_Nil)) {
    t43 = v_ys;
  }
  if ((!t43) && u_is_con(s42, &n_list_46_Cons)) {
    u_value v_x = u_field(s42, 0);
    u_value v_rest = u_field(s42, 1);
    t43 = f_list_46_reverse_onto_2(v_rest, u_mk_con(&n_list_46_Cons, 2, v_x, v_ys));
  }
  if (!t43) {
    u_no_case(s42);
  }
  return t43;
}

u_value f_list_46_reverse_1(u_value v_xs) {
  return f_list_46_reverse_onto_2(v_xs, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_list_46_zip_2(u_value v_xs, u_value v_ys) {
  u_value s44 = u_mk_tuple(2, v_xs, v_ys);
  u_value t45 = NULL;
  if ((!t45) && (u_is_con(u_field(s44, 0), &n_list_46_Cons) && u_is_con(u_field(s44, 1), &n_list_46_Cons))) {
    u_value v_x = u_field(u_field(s44, 0), 0);
    u_value v_xs1 = u_field(u_field(s44, 0), 1);
    u_value v_y = u_field(u_field(s44, 1), 0);
    u_value v_ys1 = u_field(u_field(s44, 1), 1);
    t45 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_y), f_list_46_zip_2(v_xs1, v_ys1));
  }
  if (!t45) {
    t45 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t45) {
    u_no_case(s44);
  }
  return t45;
}

u_value f_list_46_take_2(u_value v_n, u_value v_xs) {
  u_value s46 = v_xs;
  u_value t47 = NULL;
  if ((!t47) && u_is_con(s46, &n_list_46_Nil)) {
    t47 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t47) && u_is_con(s46, &n_list_46_Cons)) {
    u_value v_y = u_field(s46, 0);
    u_value v_ys = u_field(s46, 1);
    u_value t48;
    if (u_truth(u_eq(v_n, u_mk_int(0)))) {
      t48 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t48 = u_mk_con(&n_list_46_Cons, 2, v_y, f_list_46_take_2(u_sub(v_n, u_mk_int(1)), v_ys));
    }
    t47 = t48;
  }
  if (!t47) {
    u_no_case(s46);
  }
  return t47;
}

u_value f_list_46_drop_2(u_value v_n, u_value v_xs) {
  u_value s49 = v_xs;
  u_value t50 = NULL;
  if ((!t50) && u_is_con(s49, &n_list_46_Nil)) {
    t50 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t50) && u_is_con(s49, &n_list_46_Cons)) {
    u_value v_ys = u_field(s49, 1);
    u_value t51;
    if (u_truth(u_eq(v_n, u_mk_int(0)))) {
      t51 = v_xs;
    } else {
      t51 = f_list_46_drop_2(u_sub(v_n, u_mk_int(1)), v_ys);
    }
    t50 = t51;
  }
  if (!t50) {
    u_no_case(s49);
  }
  return t50;
}

u_value f_list_46_replicate_2(u_value v_n, u_value v_x) {
  u_value t52;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t52 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t52 = u_mk_con(&n_list_46_Cons, 2, v_x, f_list_46_replicate_2(u_sub(v_n, u_mk_int(1)), v_x));
  }
  return t52;
}

u_value f_list_46_firsts_1(u_value v_ps) {
  u_value s53 = v_ps;
  u_value t54 = NULL;
  if ((!t54) && u_is_con(s53, &n_list_46_Nil)) {
    t54 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t54) && u_is_con(s53, &n_list_46_Cons)) {
    u_value v_a = u_field(u_field(s53, 0), 0);
    u_value v_rest = u_field(s53, 1);
    t54 = u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_firsts_1(v_rest));
  }
  if (!t54) {
    u_no_case(s53);
  }
  return t54;
}

u_value f_list_46_seconds_1(u_value v_ps) {
  u_value s55 = v_ps;
  u_value t56 = NULL;
  if ((!t56) && u_is_con(s55, &n_list_46_Nil)) {
    t56 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t56) && u_is_con(s55, &n_list_46_Cons)) {
    u_value v_b = u_field(u_field(s55, 0), 1);
    u_value v_rest = u_field(s55, 1);
    t56 = u_mk_con(&n_list_46_Cons, 2, v_b, f_list_46_seconds_1(v_rest));
  }
  if (!t56) {
    u_no_case(s55);
  }
  return t56;
}

u_value f_syntax_46_Name_46_module_1(u_value v_x) {
  u_value s57 = v_x;
  u_value t58 = NULL;
  if ((!t58) && u_is_con(s57, &n_syntax_46_Name)) {
    u_value v_y = u_field(s57, 0);
    t58 = v_y;
  }
  if (!t58) {
    u_no_case(s57);
  }
  return t58;
}

u_value f_syntax_46_Name_46_base_1(u_value v_x) {
  u_value s59 = v_x;
  u_value t60 = NULL;
  if ((!t60) && u_is_con(s59, &n_syntax_46_Name)) {
    u_value v_y = u_field(s59, 1);
    t60 = v_y;
  }
  if (!t60) {
    u_no_case(s59);
  }
  return t60;
}

u_value f_syntax_46_Var_46_name_1(u_value v_x) {
  u_value s61 = v_x;
  u_value t62 = NULL;
  if ((!t62) && u_is_con(s61, &n_syntax_46_Var)) {
    u_value v_y = u_field(s61, 0);
    t62 = v_y;
  }
  if (!t62) {
    u_no_case(s61);
  }
  return t62;
}

u_value f_syntax_46_Var_46_typ_1(u_value v_x) {
  u_value s63 = v_x;
  u_value t64 = NULL;
  if ((!t64) && u_is_con(s63, &n_syntax_46_Var)) {
    u_value v_y = u_field(s63, 1);
    t64 = v_y;
  }
  if (!t64) {
    u_no_case(s63);
  }
  return t64;
}

u_value f_syntax_46_Branch_46_pat_1(u_value v_x) {
  u_value s65 = v_x;
  u_value t66 = NULL;
  if ((!t66) && u_is_con(s65, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s65, 0);
    t66 = v_y;
  }
  if (!t66) {
    u_no_case(s65);
  }
  return t66;
}

u_value f_syntax_46_Branch_46_guard_1(u_value v_x) {
  u_value s67 = v_x;
  u_value t68 = NULL;
  if ((!t68) && u_is_con(s67, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s67, 1);
    t68 = v_y;
  }
  if (!t68) {
    u_no_case(s67);
  }
  return t68;
}

u_value f_syntax_46_Branch_46_body_1(u_value v_x) {
  u_value s69 = v_x;
  u_value t70 = NULL;
  if ((!t70) && u_is_con(s69, &n_syntax_46_Branch)) {
    u_value v_y = u_field(s69, 2);
    t70 = v_y;
  }
  if (!t70) {
    u_no_case(s69);
  }
  return t70;
}

u_value f_syntax_46_Handler_46_typ_1(u_value v_x) {
  u_value s71 = v_x;
  u_value t72 = NULL;
  if ((!t72) && u_is_con(s71, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s71, 0);
    t72 = v_y;
  }
  if (!t72) {
    u_no_case(s71);
  }
  return t72;
}

u_value f_syntax_46_Handler_46_pat_1(u_value v_x) {
  u_value s73 = v_x;
  u_value t74 = NULL;
  if ((!t74) && u_is_con(s73, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s73, 1);
    t74 = v_y;
  }
  if (!t74) {
    u_no_case(s73);
  }
  return t74;
}

u_value f_syntax_46_Handler_46_guard_1(u_value v_x) {
  u_value s75 = v_x;
  u_value t76 = NULL;
  if ((!t76) && u_is_con(s75, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s75, 2);
    t76 = v_y;
  }
  if (!t76) {
    u_no_case(s75);
  }
  return t76;
}

u_value f_syntax_46_Handler_46_body_1(u_value v_x) {
  u_value s77 = v_x;
  u_value t78 = NULL;
  if ((!t78) && u_is_con(s77, &n_syntax_46_Handler)) {
    u_value v_y = u_field(s77, 3);
    t78 = v_y;
  }
  if (!t78) {
    u_no_case(s77);
  }
  return t78;
}

u_value f_syntax_46_Constr_46_name_1(u_value v_x) {
  u_value s79 = v_x;
  u_value t80 = NULL;
  if ((!t80) && u_is_con(s79, &n_syntax_46_Constr)) {
    u_value v_y = u_field(s79, 0);
    t80 = v_y;
  }
  if (!t80) {
    u_no_case(s79);
  }
  return t80;
}

u_value f_syntax_46_Constr_46_args_1(u_value v_x) {
  u_value s81 = v_x;
  u_value t82 = NULL;
  if ((!t82) && u_is_con(s81, &n_syntax_46_Constr)) {
    u_value v_y = u_field(s81, 1);
    t82 = v_y;
  }
  if (!t82) {
    u_no_case(s81);
  }
  return t82;
}

u_value f_syntax_46_Program_46_cmds_1(u_value v_x) {
  u_value s83 = v_x;
  u_value t84 = NULL;
  if ((!t84) && u_is_con(s83, &n_syntax_46_Program)) {
    u_value v_y = u_field(s83, 0);
    t84 = v_y;
  }
  if (!t84) {
    u_no_case(s83);
  }
  return t84;
}

u_value f_syntax_46_unqualified_1(u_value v_x) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_None, 0), v_x);
}

u_value f_syntax_46_qualified_1(u_value v_n) {
  u_value s85 = v_n;
  u_value t86 = NULL;
  if ((!t86) && (u_is_con(s85, &n_syntax_46_Name) && u_is_con(u_field(s85, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s85, 0), 0);
    u_value v_b = u_field(s85, 1);
    static struct u_object l__46__87 = U_LIT(".");
    t86 = u_concat(v_m, u_concat(&l__46__87, v_b));
  }
  if ((!t86) && (u_is_con(s85, &n_syntax_46_Name) && u_is_con(u_field(s85, 0), &n_option_46_None))) {
    u_value v_b = u_field(s85, 1);
    t86 = v_b;
  }
  if (!t86) {
    u_no_case(s85);
  }
  return t86;
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
  static struct u_object l_list_88 = U_LIT("list");
  static struct u_object l_List_89 = U_LIT("List");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_88), &l_List_89);
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
  static struct u_object l_list_90 = U_LIT("list");
  static struct u_object l_Nil_91 = U_LIT("Nil");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_90), &l_Nil_91);
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
  static struct u_object l_list_92 = U_LIT("list");
  static struct u_object l_Cons_93 = U_LIT("Cons");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_92), &l_Cons_93);
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
  static struct u_object l_option_94 = U_LIT("option");
  static struct u_object l_Option_95 = U_LIT("Option");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_94), &l_Option_95);
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
  static struct u_object l_option_96 = U_LIT("option");
  static struct u_object l_None_97 = U_LIT("None");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_96), &l_None_97);
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
  static struct u_object l_option_98 = U_LIT("option");
  static struct u_object l_Some_99 = U_LIT("Some");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_98), &l_Some_99);
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
  static struct u_object l_os_100 = U_LIT("os");
  static struct u_object l_OsError_101 = U_LIT("OsError");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_os_100), &l_OsError_101);
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
  static struct u_object l__44__102 = U_LIT(",");
  cache = f_syntax_46_unqualified_1(&l__44__102);
  return cache;
}

u_value f_syntax_46_names_1(u_value v_xs) {
  u_value t103 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e104 = &t103;
  u_value l105 = v_xs;
  while (u_is_con(l105, &n_list_46_Cons)) {
    u_value v_x = u_field(l105, 0);
    if (u_truth(u_mk_bool(1))) {
      e104 = u_append(e104, f_syntax_46_Var_46_name_1(v_x));
    }
    l105 = u_field(l105, 1);
  }
  return t103;
}

u_value f_syntax_46_types_1(u_value v_xs) {
  u_value t106 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e107 = &t106;
  u_value l108 = v_xs;
  while (u_is_con(l108, &n_list_46_Cons)) {
    u_value v_x = u_field(l108, 0);
    if (u_truth(u_mk_bool(1))) {
      e107 = u_append(e107, f_syntax_46_Var_46_typ_1(v_x));
    }
    l108 = u_field(l108, 1);
  }
  return t106;
}

u_value f_syntax_46_arity_1(u_value v_t) {
  u_value s109 = v_t;
  u_value t110 = NULL;
  if ((!t110) && u_is_con(s109, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s109, 0);
    t110 = f_list_46_length_1(v_args);
  }
  if (!t110) {
    t110 = f_prelude_46__45__1(u_mk_int(1));
  }
  if (!t110) {
    u_no_case(s109);
  }
  return t110;
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
  cache = u_mk_con(&n_map_46_MLeaf, 0);
  return cache;
}

u_value f_map_46_get_2(u_value v_k, u_value v_m) {
  u_value s111 = v_m;
  u_value t112 = NULL;
  if ((!t112) && u_is_con(s111, &n_map_46_MLeaf)) {
    t112 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t112) && u_is_con(s111, &n_map_46_MNode)) {
    u_value v_l = u_field(s111, 0);
    u_value v_j = u_field(s111, 1);
    u_value v_v = u_field(s111, 2);
    u_value v_r = u_field(s111, 3);
    u_value t116 = u_compare(v_k, v_j);
    u_value t115;
    {
      u_value v_c = t116;
      u_value t114;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t114 = f_map_46_get_2(v_k, v_l);
      } else {
        u_value t113;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t113 = f_map_46_get_2(v_k, v_r);
        } else {
          t113 = u_mk_con(&n_option_46_Some, 1, v_v);
        }
        t114 = t113;
      }
      t115 = t114;
    }
    t112 = t115;
  }
  if (!t112) {
    u_no_case(s111);
  }
  return t112;
}

u_value f_map_46_has_2(u_value v_k, u_value v_m) {
  return f_prelude_46__33__61__2(f_map_46_get_2(v_k, v_m), u_mk_con(&n_option_46_None, 0));
}

u_value f_map_46_put_3(u_value v_k, u_value v_v, u_value v_m) {
  u_value s117 = v_m;
  u_value t118 = NULL;
  if ((!t118) && u_is_con(s117, &n_map_46_MLeaf)) {
    t118 = u_mk_con(&n_map_46_MNode, 5, u_mk_con(&n_map_46_MLeaf, 0), v_k, v_v, u_mk_con(&n_map_46_MLeaf, 0), u_mk_int(1));
  }
  if ((!t118) && u_is_con(s117, &n_map_46_MNode)) {
    u_value v_l = u_field(s117, 0);
    u_value v_j = u_field(s117, 1);
    u_value v_w = u_field(s117, 2);
    u_value v_r = u_field(s117, 3);
    u_value v_h = u_field(s117, 4);
    u_value t122 = u_compare(v_k, v_j);
    u_value t121;
    {
      u_value v_c = t122;
      u_value t120;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t120 = f_map_46_balance_4(f_map_46_put_3(v_k, v_v, v_l), v_j, v_w, v_r);
      } else {
        u_value t119;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t119 = f_map_46_balance_4(v_l, v_j, v_w, f_map_46_put_3(v_k, v_v, v_r));
        } else {
          t119 = u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, v_h);
        }
        t120 = t119;
      }
      t121 = t120;
    }
    t118 = t121;
  }
  if (!t118) {
    u_no_case(s117);
  }
  return t118;
}

u_value f_map_46_entries_1(u_value v_m) {
  return f_map_46_entries_onto_2(v_m, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest) {
  u_value s123 = v_m;
  u_value t124 = NULL;
  if ((!t124) && u_is_con(s123, &n_map_46_MLeaf)) {
    t124 = v_rest;
  }
  if ((!t124) && u_is_con(s123, &n_map_46_MNode)) {
    u_value v_l = u_field(s123, 0);
    u_value v_k = u_field(s123, 1);
    u_value v_v = u_field(s123, 2);
    u_value v_r = u_field(s123, 3);
    t124 = f_map_46_entries_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), f_map_46_entries_onto_2(v_r, v_rest)));
  }
  if (!t124) {
    u_no_case(s123);
  }
  return t124;
}

u_value f_map_46_size_1(u_value v_m) {
  u_value s125 = v_m;
  u_value t126 = NULL;
  if ((!t126) && u_is_con(s125, &n_map_46_MLeaf)) {
    t126 = u_mk_int(0);
  }
  if ((!t126) && u_is_con(s125, &n_map_46_MNode)) {
    u_value v_l = u_field(s125, 0);
    u_value v_r = u_field(s125, 3);
    t126 = u_add(u_add(f_map_46_size_1(v_l), u_mk_int(1)), f_map_46_size_1(v_r));
  }
  if (!t126) {
    u_no_case(s125);
  }
  return t126;
}

u_value f_map_46_height_1(u_value v_m) {
  u_value s127 = v_m;
  u_value t128 = NULL;
  if ((!t128) && u_is_con(s127, &n_map_46_MLeaf)) {
    t128 = u_mk_int(0);
  }
  if ((!t128) && u_is_con(s127, &n_map_46_MNode)) {
    u_value v_h = u_field(s127, 4);
    t128 = v_h;
  }
  if (!t128) {
    u_no_case(s127);
  }
  return t128;
}

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  return u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, f_map_46_mnode_height_2(v_l, v_r));
}

u_value f_map_46_rotate_left_1(u_value v_m) {
  u_value s129 = v_m;
  u_value t130 = NULL;
  if ((!t130) && (u_is_con(s129, &n_map_46_MNode) && u_is_con(u_field(s129, 3), &n_map_46_MNode))) {
    u_value v_a = u_field(s129, 0);
    u_value v_j = u_field(s129, 1);
    u_value v_v = u_field(s129, 2);
    u_value v_b = u_field(u_field(s129, 3), 0);
    u_value v_k = u_field(u_field(s129, 3), 1);
    u_value v_w = u_field(u_field(s129, 3), 2);
    u_value v_c = u_field(u_field(s129, 3), 3);
    t130 = f_map_46_mnode_4(f_map_46_mnode_4(v_a, v_j, v_v, v_b), v_k, v_w, v_c);
  }
  if (!t130) {
    t130 = v_m;
  }
  if (!t130) {
    u_no_case(s129);
  }
  return t130;
}

u_value f_map_46_rotate_right_1(u_value v_m) {
  u_value s131 = v_m;
  u_value t132 = NULL;
  if ((!t132) && (u_is_con(s131, &n_map_46_MNode) && u_is_con(u_field(s131, 0), &n_map_46_MNode))) {
    u_value v_a = u_field(u_field(s131, 0), 0);
    u_value v_j = u_field(u_field(s131, 0), 1);
    u_value v_v = u_field(u_field(s131, 0), 2);
    u_value v_b = u_field(u_field(s131, 0), 3);
    u_value v_k = u_field(s131, 1);
    u_value v_w = u_field(s131, 2);
    u_value v_c = u_field(s131, 3);
    t132 = f_map_46_mnode_4(v_a, v_j, v_v, f_map_46_mnode_4(v_b, v_k, v_w, v_c));
  }
  if (!t132) {
    t132 = v_m;
  }
  if (!t132) {
    u_no_case(s131);
  }
  return t132;
}

u_value f_map_46_left_of_1(u_value v_m) {
  u_value s133 = v_m;
  u_value t134 = NULL;
  if ((!t134) && u_is_con(s133, &n_map_46_MNode)) {
    u_value v_l = u_field(s133, 0);
    t134 = v_l;
  }
  if (!t134) {
    t134 = v_m;
  }
  if (!t134) {
    u_no_case(s133);
  }
  return t134;
}

u_value f_map_46_right_of_1(u_value v_m) {
  u_value s135 = v_m;
  u_value t136 = NULL;
  if ((!t136) && u_is_con(s135, &n_map_46_MNode)) {
    u_value v_r = u_field(s135, 3);
    t136 = v_r;
  }
  if (!t136) {
    t136 = v_m;
  }
  if (!t136) {
    u_no_case(s135);
  }
  return t136;
}

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  u_value t140;
  if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1))))) {
    u_value t137;
    if (u_truth(u_lt(f_map_46_height_1(f_map_46_left_of_1(v_l)), f_map_46_height_1(f_map_46_right_of_1(v_l))))) {
      t137 = f_map_46_rotate_left_1(v_l);
    } else {
      t137 = v_l;
    }
    t140 = f_map_46_rotate_right_1(f_map_46_mnode_4(t137, v_k, v_v, v_r));
  } else {
    u_value t139;
    if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1))))) {
      u_value t138;
      if (u_truth(u_lt(f_map_46_height_1(f_map_46_right_of_1(v_r)), f_map_46_height_1(f_map_46_left_of_1(v_r))))) {
        t138 = f_map_46_rotate_right_1(v_r);
      } else {
        t138 = v_r;
      }
      t139 = f_map_46_rotate_left_1(f_map_46_mnode_4(v_l, v_k, v_v, t138));
    } else {
      t139 = f_map_46_mnode_4(v_l, v_k, v_v, v_r);
    }
    t140 = t139;
  }
  return t140;
}

u_value f_map_46_balanced_1(u_value v_m) {
  u_value s141 = v_m;
  u_value t142 = NULL;
  if ((!t142) && u_is_con(s141, &n_map_46_MLeaf)) {
    t142 = u_mk_bool(1);
  }
  if ((!t142) && u_is_con(s141, &n_map_46_MNode)) {
    u_value v_l = u_field(s141, 0);
    u_value v_r = u_field(s141, 3);
    u_value v_h = u_field(s141, 4);
    u_value t143 = f_map_46_balanced_1(v_l);
    if (u_truth(t143)) {
      t143 = f_map_46_balanced_1(v_r);
    }
    u_value t144 = t143;
    if (u_truth(t144)) {
      t144 = u_eq(v_h, f_map_46_mnode_height_2(v_l, v_r));
    }
    u_value t145 = t144;
    if (u_truth(t145)) {
      t145 = f_prelude_46__60__61__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1)));
    }
    u_value t146 = t145;
    if (u_truth(t146)) {
      t146 = f_prelude_46__60__61__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1)));
    }
    t142 = t146;
  }
  if (!t142) {
    u_no_case(s141);
  }
  return t142;
}

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r) {
  u_value t147;
  if (u_truth(u_lt(f_map_46_height_1(v_l), f_map_46_height_1(v_r)))) {
    t147 = f_map_46_height_1(v_r);
  } else {
    t147 = f_map_46_height_1(v_l);
  }
  return u_add(u_mk_int(1), t147);
}

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m) {
  u_value s148 = v_ps;
  u_value t149 = NULL;
  if ((!t149) && u_is_con(s148, &n_list_46_Nil)) {
    t149 = v_m;
  }
  if ((!t149) && u_is_con(s148, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s148, 0), 0);
    u_value v_v = u_field(u_field(s148, 0), 1);
    u_value v_rest = u_field(s148, 1);
    t149 = f_map_46_put_all_2(v_rest, f_map_46_put_3(v_k, v_v, v_m));
  }
  if (!t149) {
    u_no_case(s148);
  }
  return t149;
}

u_value f_map_46_numbers_1(u_value v_n) {
  u_value t150;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t150 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t150 = f_list_46__43__43__2(f_map_46_numbers_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t150;
}

u_value f_ctx_46_Ctx_46_aliases_1(u_value v_x) {
  u_value s151 = v_x;
  u_value t152 = NULL;
  if ((!t152) && u_is_con(s151, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s151, 0);
    t152 = v_y;
  }
  if (!t152) {
    u_no_case(s151);
  }
  return t152;
}

u_value f_ctx_46_Ctx_46_funs_1(u_value v_x) {
  u_value s153 = v_x;
  u_value t154 = NULL;
  if ((!t154) && u_is_con(s153, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s153, 1);
    t154 = v_y;
  }
  if (!t154) {
    u_no_case(s153);
  }
  return t154;
}

u_value f_ctx_46_Ctx_46_procs_1(u_value v_x) {
  u_value s155 = v_x;
  u_value t156 = NULL;
  if ((!t156) && u_is_con(s155, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s155, 2);
    t156 = v_y;
  }
  if (!t156) {
    u_no_case(s155);
  }
  return t156;
}

u_value f_ctx_46_Ctx_46_datas_1(u_value v_x) {
  u_value s157 = v_x;
  u_value t158 = NULL;
  if ((!t158) && u_is_con(s157, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s157, 3);
    t158 = v_y;
  }
  if (!t158) {
    u_no_case(s157);
  }
  return t158;
}

u_value f_ctx_46_Ctx_46_members_1(u_value v_x) {
  u_value s159 = v_x;
  u_value t160 = NULL;
  if ((!t160) && u_is_con(s159, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s159, 4);
    t160 = v_y;
  }
  if (!t160) {
    u_no_case(s159);
  }
  return t160;
}

u_value f_ctx_46_Ctx_46_constrs_1(u_value v_x) {
  u_value s161 = v_x;
  u_value t162 = NULL;
  if ((!t162) && u_is_con(s161, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s161, 5);
    t162 = v_y;
  }
  if (!t162) {
    u_no_case(s161);
  }
  return t162;
}

u_value f_ctx_46_Ctx_46_named_1(u_value v_x) {
  u_value s163 = v_x;
  u_value t164 = NULL;
  if ((!t164) && u_is_con(s163, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s163, 6);
    t164 = v_y;
  }
  if (!t164) {
    u_no_case(s163);
  }
  return t164;
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
  cache = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0(), f_map_46_empty_0());
  return cache;
}

u_value f_ctx_46_alias_of_2(u_value v_c, u_value v_n) {
  return f_map_46_get_2(v_n, f_ctx_46_Ctx_46_aliases_1(v_c));
}

u_value f_ctx_46_schemes_of_2(u_value v_c, u_value v_n) {
  u_value s165 = f_map_46_get_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
  u_value t166 = NULL;
  if ((!t166) && u_is_con(s165, &n_option_46_Some)) {
    u_value v_ss = u_field(s165, 0);
    t166 = v_ss;
  }
  if ((!t166) && u_is_con(s165, &n_option_46_None)) {
    t166 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t166) {
    u_no_case(s165);
  }
  return t166;
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
  return f_prelude_46__33__61__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_ctx_46_is_proc_2(u_value v_c, u_value v_n) {
  return f_prelude_46__33__61__2(f_ctx_46_proc_of_2(v_c, v_n), u_mk_con(&n_option_46_None, 0));
}

u_value f_ctx_46_is_data_2(u_value v_c, u_value v_n) {
  return f_prelude_46__33__61__2(f_ctx_46_data_of_2(v_c, v_n), u_mk_con(&n_option_46_None, 0));
}

u_value f_ctx_46_is_alias_2(u_value v_c, u_value v_n) {
  return f_prelude_46__33__61__2(f_ctx_46_alias_of_2(v_c, v_n), u_mk_con(&n_option_46_None, 0));
}

u_value f_ctx_46_is_constr_2(u_value v_c, u_value v_n) {
  return f_prelude_46__33__61__2(f_ctx_46_constr_of_2(v_c, v_n), u_mk_con(&n_option_46_None, 0));
}

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b) {
  u_value s167 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t168 = NULL;
  if ((!t168) && u_is_con(s167, &n_option_46_Some)) {
    u_value v_ns = u_field(s167, 0);
    t168 = v_ns;
  }
  if ((!t168) && u_is_con(s167, &n_option_46_None)) {
    t168 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t168) {
    u_no_case(s167);
  }
  return t168;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s169 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t170 = NULL;
  if ((!t170) && u_is_con(s169, &n_option_46_Some)) {
    u_value v_ns = u_field(s169, 0);
    u_value t171;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t171 = v_bs;
    } else {
      t171 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, v_ns), v_bs);
    }
    t170 = t171;
  }
  if ((!t170) && u_is_con(s169, &n_option_46_None)) {
    t170 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t170) {
    u_no_case(s169);
  }
  return t170;
}

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t) {
  u_value s172 = v_c;
  u_value t173 = NULL;
  if ((!t173) && u_is_con(s172, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s172, 0);
    u_value v_f = u_field(s172, 1);
    u_value v_p = u_field(s172, 2);
    u_value v_d = u_field(s172, 3);
    u_value v_m = u_field(s172, 4);
    u_value v_k = u_field(s172, 5);
    u_value v_bs = u_field(s172, 6);
    t173 = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), v_a), v_f, v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t173) {
    u_no_case(s172);
  }
  return t173;
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
  u_value s174 = v_c;
  u_value t175 = NULL;
  if ((!t175) && u_is_con(s174, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s174, 0);
    u_value v_f = u_field(s174, 1);
    u_value v_p = u_field(s174, 2);
    u_value v_d = u_field(s174, 3);
    u_value v_m = u_field(s174, 4);
    u_value v_k = u_field(s174, 5);
    u_value v_bs = u_field(s174, 6);
    t175 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, u_mk_con(&n_list_46_Nil, 0))), v_f), v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t175) {
    u_no_case(s174);
  }
  return t175;
}

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g) {
  u_value s176 = v_c;
  u_value t177 = NULL;
  if ((!t177) && u_is_con(s176, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s176, 0);
    u_value v_f = u_field(s176, 1);
    u_value v_p = u_field(s176, 2);
    u_value v_d = u_field(s176, 3);
    u_value v_m = u_field(s176, 4);
    u_value v_k = u_field(s176, 5);
    u_value v_bs = u_field(s176, 6);
    t177 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, f_map_46_put_3(v_n, v_g, v_p), v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t177) {
    u_no_case(s176);
  }
  return t177;
}

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity) {
  u_value s178 = v_c;
  u_value t179 = NULL;
  if ((!t179) && u_is_con(s178, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s178, 0);
    u_value v_f = u_field(s178, 1);
    u_value v_p = u_field(s178, 2);
    u_value v_d = u_field(s178, 3);
    u_value v_m = u_field(s178, 4);
    u_value v_k = u_field(s178, 5);
    u_value v_bs = u_field(s178, 6);
    t179 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, f_map_46_put_3(v_n, v_arity, v_d), v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t179) {
    u_no_case(s178);
  }
  return t179;
}

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs) {
  u_value s180 = v_c;
  u_value t181 = NULL;
  if ((!t181) && u_is_con(s180, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s180, 0);
    u_value v_f = u_field(s180, 1);
    u_value v_p = u_field(s180, 2);
    u_value v_d = u_field(s180, 3);
    u_value v_m = u_field(s180, 4);
    u_value v_k = u_field(s180, 5);
    u_value v_bs = u_field(s180, 6);
    t181 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, f_map_46_put_3(v_n, v_cs, v_m), v_k, v_bs);
  }
  if (!t181) {
    u_no_case(s180);
  }
  return t181;
}

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc) {
  u_value s182 = v_c;
  u_value t183 = NULL;
  if ((!t183) && u_is_con(s182, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s182, 0);
    u_value v_f = u_field(s182, 1);
    u_value v_p = u_field(s182, 2);
    u_value v_d = u_field(s182, 3);
    u_value v_m = u_field(s182, 4);
    u_value v_k = u_field(s182, 5);
    u_value v_bs = u_field(s182, 6);
    t183 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, v_m, f_map_46_put_3(v_n, v_kc, v_k), f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t183) {
    u_no_case(s182);
  }
  return t183;
}

u_value f_check_46_State_46_ctx_1(u_value v_x) {
  u_value s184 = v_x;
  u_value t185 = NULL;
  if ((!t185) && u_is_con(s184, &n_check_46_State)) {
    u_value v_y = u_field(s184, 0);
    t185 = v_y;
  }
  if (!t185) {
    u_no_case(s184);
  }
  return t185;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s186 = v_x;
  u_value t187 = NULL;
  if ((!t187) && u_is_con(s186, &n_check_46_State)) {
    u_value v_y = u_field(s186, 1);
    t187 = v_y;
  }
  if (!t187) {
    u_no_case(s186);
  }
  return t187;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s188 = v_x;
  u_value t189 = NULL;
  if ((!t189) && u_is_con(s188, &n_check_46_State)) {
    u_value v_y = u_field(s188, 2);
    t189 = v_y;
  }
  if (!t189) {
    u_no_case(s188);
  }
  return t189;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s190 = v_x;
  u_value t191 = NULL;
  if ((!t191) && u_is_con(s190, &n_check_46_State)) {
    u_value v_y = u_field(s190, 3);
    t191 = v_y;
  }
  if (!t191) {
    u_no_case(s190);
  }
  return t191;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s192 = v_x;
  u_value t193 = NULL;
  if ((!t193) && u_is_con(s192, &n_check_46_State)) {
    u_value v_y = u_field(s192, 4);
    t193 = v_y;
  }
  if (!t193) {
    u_no_case(s192);
  }
  return t193;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s194 = v_x;
  u_value t195 = NULL;
  if ((!t195) && u_is_con(s194, &n_check_46_State)) {
    u_value v_y = u_field(s194, 5);
    t195 = v_y;
  }
  if (!t195) {
    u_no_case(s194);
  }
  return t195;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  u_value s196 = v_s;
  u_value t197 = NULL;
  if ((!t197) && u_is_con(s196, &n_check_46_State)) {
    u_value v_su = u_field(s196, 1);
    u_value v_n = u_field(s196, 2);
    u_value v_m = u_field(s196, 3);
    u_value v_ms = u_field(s196, 4);
    u_value v_vs = u_field(s196, 5);
    t197 = u_mk_con(&n_check_46_State, 6, v_c, v_su, v_n, v_m, v_ms, v_vs);
  }
  if (!t197) {
    u_no_case(s196);
  }
  return t197;
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  u_value s198 = v_s;
  u_value t199 = NULL;
  if ((!t199) && u_is_con(s198, &n_check_46_State)) {
    u_value v_c = u_field(s198, 0);
    u_value v_vs = u_field(s198, 5);
    t199 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_empty_0(), u_mk_int(0), v_m, f_check_46_sees_2(v_m, v_vs), v_vs);
  }
  if (!t199) {
    u_no_case(s198);
  }
  return t199;
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s200 = f_list_46_lookup_2(v_m, v_vs);
  u_value t201 = NULL;
  if ((!t201) && u_is_con(s200, &n_option_46_Some)) {
    u_value v_ms = u_field(s200, 0);
    t201 = v_ms;
  }
  if ((!t201) && u_is_con(s200, &n_option_46_None)) {
    t201 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t201) {
    u_no_case(s200);
  }
  return t201;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s202 = v_cs;
  u_value t203 = NULL;
  if ((!t203) && u_is_con(s202, &n_list_46_Nil)) {
    t203 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t203) && (u_is_con(s202, &n_list_46_Cons) && u_is_con(u_field(s202, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s202, 0), 0);
    u_value v_ds = u_field(u_field(s202, 0), 1);
    u_value v_rest = u_field(s202, 1);
    t203 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t203) && u_is_con(s202, &n_list_46_Cons)) {
    u_value v_c = u_field(s202, 0);
    u_value v_rest = u_field(s202, 1);
    t203 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t203) {
    u_no_case(s202);
  }
  return t203;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s204 = v_c;
  u_value t205 = NULL;
  if ((!t205) && u_is_con(s204, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s204, 0);
    u_value v_ps = u_field(s204, 1);
    u_value v_t = u_field(s204, 2);
    t205 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t205) && u_is_con(s204, &n_syntax_46_CData)) {
    u_value v_n = u_field(s204, 0);
    u_value v_ps = u_field(s204, 1);
    u_value v_ks = u_field(s204, 2);
    u_value t206 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e207 = &t206;
    u_value l208 = v_ks;
    while (u_is_con(l208, &n_list_46_Cons)) {
      u_value v_k = u_field(l208, 0);
      if (u_truth(u_mk_bool(1))) {
        e207 = u_append(e207, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l208 = u_field(l208, 1);
    }
    t205 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t206);
  }
  if ((!t205) && u_is_con(s204, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s204, 0);
    u_value v_ps = u_field(s204, 1);
    u_value v_xss = u_field(s204, 2);
    u_value v_r = u_field(s204, 3);
    u_value v_b = u_field(s204, 4);
    t205 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t205) && u_is_con(s204, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s204, 0);
    u_value v_ps = u_field(s204, 1);
    u_value v_ins = u_field(s204, 2);
    u_value v_outs = u_field(s204, 3);
    u_value v_refs = u_field(s204, 4);
    u_value v_b = u_field(s204, 5);
    t205 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t205) {
    t205 = v_c;
  }
  if (!t205) {
    u_no_case(s204);
  }
  return t205;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  u_value s209 = v_k;
  u_value t210 = NULL;
  if ((!t210) && u_is_con(s209, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s209, 0);
    u_value v_ts = u_field(s209, 1);
    t210 = u_mk_con(&n_syntax_46_Constr, 2, f_check_46_in_module_2(v_m, v_n), v_ts);
  }
  if (!t210) {
    u_no_case(s209);
  }
  return t210;
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s211 = v_ds;
  u_value t212 = NULL;
  if ((!t212) && u_is_con(s211, &n_list_46_Nil)) {
    t212 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t212) && (u_is_con(s211, &n_list_46_Cons) && u_is_con(u_field(u_field(s211, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s211, 0), 0);
    u_value v_n = u_field(u_field(u_field(s211, 0), 1), 0);
    u_value v_rest = u_field(s211, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t212 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t212) && u_is_con(s211, &n_list_46_Cons)) {
    u_value v_rest = u_field(s211, 1);
    t212 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t212) {
    u_no_case(s211);
  }
  return t212;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t213 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e214 = &t213;
  u_value l215 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l215, &n_list_46_Cons)) {
    u_value v_m = u_field(l215, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_216 = U_LIT("prelude");
      static struct u_object l_prelude_217 = U_LIT("prelude");
      e214 = u_append(e214, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_216, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_217)))))));
    }
    l215 = u_field(l215, 1);
  }
  return t213;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value s218 = v_n;
  u_value t219 = NULL;
  if ((!t219) && (u_is_con(s218, &n_syntax_46_Name) && u_is_con(u_field(s218, 0), &n_option_46_Some))) {
    u_value t220 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e221 = &t220;
    u_value l222 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    while (u_is_con(l222, &n_list_46_Cons)) {
      u_value v_d = u_field(l222, 0);
      if (u_truth(f_list_46_contains_2(v_d, v_declared))) {
        e221 = u_append(e221, v_d);
      }
      l222 = u_field(l222, 1);
    }
    t219 = t220;
  }
  if ((!t219) && (u_is_con(s218, &n_syntax_46_Name) && u_is_con(u_field(s218, 0), &n_option_46_None))) {
    u_value v_b = u_field(s218, 1);
    u_value t229;
    if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_declared))) {
      t229 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
    } else {
      u_value t223 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e224 = &t223;
      u_value t225 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e226 = &t225;
      u_value l227 = f_list_46_drop_2(u_mk_int(1), f_check_46_State_46_visible_1(v_s));
      while (u_is_con(l227, &n_list_46_Cons)) {
        u_value v_m = u_field(l227, 0);
        if (u_truth(u_mk_bool(1))) {
          e226 = u_append(e226, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_b));
        }
        l227 = u_field(l227, 1);
      }
      u_value l228 = t225;
      while (u_is_con(l228, &n_list_46_Cons)) {
        u_value v_d = u_field(l228, 0);
        if (u_truth(f_list_46_contains_2(v_d, v_declared))) {
          e224 = u_append(e224, v_d);
        }
        l228 = u_field(l228, 1);
      }
      t229 = t223;
    }
    t219 = t229;
  }
  if (!t219) {
    u_no_case(s218);
  }
  return t219;
}

u_value f_check_46_funs_named_2(u_value v_c, u_value v_n) {
  u_value t230 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e231 = &t230;
  u_value l232 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l232, &n_list_46_Cons)) {
    u_value v_d = u_field(l232, 0);
    if (u_truth(f_ctx_46_is_fun_2(v_c, v_d))) {
      e231 = u_append(e231, v_d);
    }
    l232 = u_field(l232, 1);
  }
  return t230;
}

u_value f_check_46_constrs_named_2(u_value v_c, u_value v_n) {
  u_value t233 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e234 = &t233;
  u_value l235 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l235, &n_list_46_Cons)) {
    u_value v_d = u_field(l235, 0);
    if (u_truth(f_ctx_46_is_constr_2(v_c, v_d))) {
      e234 = u_append(e234, v_d);
    }
    l235 = u_field(l235, 1);
  }
  return t233;
}

u_value f_check_46_procs_named_2(u_value v_c, u_value v_n) {
  u_value t236 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e237 = &t236;
  u_value l238 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l238, &n_list_46_Cons)) {
    u_value v_d = u_field(l238, 0);
    if (u_truth(f_ctx_46_is_proc_2(v_c, v_d))) {
      e237 = u_append(e237, v_d);
    }
    l238 = u_field(l238, 1);
  }
  return t236;
}

u_value f_check_46_sorts_named_2(u_value v_c, u_value v_n) {
  u_value t239 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e240 = &t239;
  u_value l241 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l241, &n_list_46_Cons)) {
    u_value v_d = u_field(l241, 0);
    u_value t242 = f_ctx_46_is_data_2(v_c, v_d);
    if (!u_truth(t242)) {
      t242 = f_ctx_46_is_alias_2(v_c, v_d);
    }
    if (u_truth(t242)) {
      e240 = u_append(e240, v_d);
    }
    l241 = u_field(l241, 1);
  }
  return t239;
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
  static struct u_object l_Bool_243 = U_LIT("Bool");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_243), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_244 = U_LIT("Int");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_244), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_245 = U_LIT("Str");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_245), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_246 = U_LIT("Char");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_246), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t247 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e248 = &t247;
  u_value l249 = v_ps;
  while (u_is_con(l249, &n_list_46_Cons)) {
    u_value v_p = u_field(l249, 0);
    if (u_truth(u_mk_bool(1))) {
      e248 = u_append(e248, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l249 = u_field(l249, 1);
  }
  return t247;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s250 = v_n;
  u_value t251 = NULL;
  if ((!t251) && (u_is_con(s250, &n_syntax_46_Name) && u_is_con(u_field(s250, 0), &n_option_46_None))) {
    u_value v_b = u_field(s250, 1);
    static struct u_object l_Bool_252 = U_LIT("Bool");
    static struct u_object l_Int_253 = U_LIT("Int");
    static struct u_object l_Str_254 = U_LIT("Str");
    static struct u_object l_Char_255 = U_LIT("Char");
    t251 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_252, u_mk_con(&n_list_46_Cons, 2, &l_Int_253, u_mk_con(&n_list_46_Cons, 2, &l_Str_254, u_mk_con(&n_list_46_Cons, 2, &l_Char_255, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t251) {
    t251 = u_mk_bool(0);
  }
  if (!t251) {
    u_no_case(s250);
  }
  return t251;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s256 = v_v;
  u_value t257 = NULL;
  if ((!t257) && u_is_con(s256, &n_syntax_46_VUnit)) {
    t257 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t257) && u_is_con(s256, &n_syntax_46_VBool)) {
    t257 = f_check_46_tbool_0();
  }
  if ((!t257) && u_is_con(s256, &n_syntax_46_VInt)) {
    t257 = f_check_46_tint_0();
  }
  if ((!t257) && u_is_con(s256, &n_syntax_46_VStr)) {
    t257 = f_check_46_tstr_0();
  }
  if ((!t257) && u_is_con(s256, &n_syntax_46_VChar)) {
    t257 = f_check_46_tchar_0();
  }
  if (!t257) {
    u_no_case(s256);
  }
  return t257;
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
  cache = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tbool_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tbool_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0()));
  return cache;
}

u_value f_check_46_builtin_1(u_value v_n) {
  u_value s258 = v_n;
  u_value t259 = NULL;
  if ((!t259) && (u_is_con(s258, &n_syntax_46_Name) && u_is_con(u_field(s258, 0), &n_option_46_None))) {
    u_value v_b = u_field(s258, 1);
    t259 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t259) {
    t259 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t259) {
    u_no_case(s258);
  }
  return t259;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_260 = U_LIT("A");
  u_value t264 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_260);
  u_value t263;
  {
    u_value v_a = t264;
    u_value s261 = v_b;
    u_value t262 = NULL;
    if ((!t262) && u_equals(s261, u_mk_lit("+"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("-"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("*"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("/"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("%"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("<"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("=="))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("compare"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("error"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), v_a)), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("and"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("or"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("chars"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("string"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("ord"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("chr"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("^"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_size"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_join"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_slice"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_split"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_get"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_contains"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_compare"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_show"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_quote"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("str_number"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("is_digit"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("is_letter"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t262) && u_equals(s261, u_mk_lit("is_space"))) {
      t262 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t262) {
      t262 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t262) {
      u_no_case(s261);
    }
    t263 = t262;
  }
  return t263;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s265 = v_t;
  u_value t266 = NULL;
  if ((!t266) && u_is_con(s265, &n_syntax_46_TMeta)) {
    t266 = u_mk_bool(1);
  }
  if (!t266) {
    t266 = u_mk_bool(0);
  }
  if (!t266) {
    u_no_case(s265);
  }
  return t266;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s267 = v_t;
  u_value t268 = NULL;
  if ((!t268) && u_is_con(s267, &n_syntax_46_TParam)) {
    t268 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t268) && u_is_con(s267, &n_syntax_46_TMeta)) {
    t268 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t268) && u_is_con(s267, &n_syntax_46_THole)) {
    t268 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t268) && u_is_con(s267, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s267, 1);
    u_value t269 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e270 = &t269;
    u_value l271 = v_args;
    while (u_is_con(l271, &n_list_46_Cons)) {
      u_value v_a = u_field(l271, 0);
      if (u_truth(u_mk_bool(1))) {
        e270 = u_append(e270, f_check_46_free_1(v_a));
      }
      l271 = u_field(l271, 1);
    }
    t268 = f_list_46_distinct_1(f_list_46_concat_1(t269));
  }
  if ((!t268) && u_is_con(s267, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s267, 0);
    u_value v_r = u_field(s267, 1);
    u_value t272 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e273 = &t272;
    u_value l274 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l274, &n_list_46_Cons)) {
      u_value v_a = u_field(l274, 0);
      if (u_truth(u_mk_bool(1))) {
        e273 = u_append(e273, f_check_46_free_1(v_a));
      }
      l274 = u_field(l274, 1);
    }
    t268 = f_list_46_distinct_1(f_list_46_concat_1(t272));
  }
  if ((!t268) && u_is_con(s267, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s267, 0);
    u_value t275 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e276 = &t275;
    u_value l277 = v_ts;
    while (u_is_con(l277, &n_list_46_Cons)) {
      u_value v_a = u_field(l277, 0);
      if (u_truth(u_mk_bool(1))) {
        e276 = u_append(e276, f_check_46_free_1(v_a));
      }
      l277 = u_field(l277, 1);
    }
    t268 = f_list_46_distinct_1(f_list_46_concat_1(t275));
  }
  if (!t268) {
    u_no_case(s267);
  }
  return t268;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s278 = v_t;
  u_value t279 = NULL;
  if ((!t279) && u_is_con(s278, &n_syntax_46_THole)) {
    t279 = u_mk_bool(1);
  }
  if ((!t279) && u_is_con(s278, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s278, 1);
    u_value t280 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e281 = &t280;
    u_value l282 = v_args;
    while (u_is_con(l282, &n_list_46_Cons)) {
      u_value v_a = u_field(l282, 0);
      if (u_truth(u_mk_bool(1))) {
        e281 = u_append(e281, f_check_46_holes_1(v_a));
      }
      l282 = u_field(l282, 1);
    }
    t279 = f_list_46_contains_2(u_mk_bool(1), t280);
  }
  if ((!t279) && u_is_con(s278, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s278, 0);
    u_value v_r = u_field(s278, 1);
    u_value t283 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e284 = &t283;
    u_value l285 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l285, &n_list_46_Cons)) {
      u_value v_a = u_field(l285, 0);
      if (u_truth(u_mk_bool(1))) {
        e284 = u_append(e284, f_check_46_holes_1(v_a));
      }
      l285 = u_field(l285, 1);
    }
    t279 = f_list_46_contains_2(u_mk_bool(1), t283);
  }
  if ((!t279) && u_is_con(s278, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s278, 0);
    u_value t286 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e287 = &t286;
    u_value l288 = v_ts;
    while (u_is_con(l288, &n_list_46_Cons)) {
      u_value v_a = u_field(l288, 0);
      if (u_truth(u_mk_bool(1))) {
        e287 = u_append(e287, f_check_46_holes_1(v_a));
      }
      l288 = u_field(l288, 1);
    }
    t279 = f_list_46_contains_2(u_mk_bool(1), t286);
  }
  if (!t279) {
    t279 = u_mk_bool(0);
  }
  if (!t279) {
    u_no_case(s278);
  }
  return t279;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s289 = v_t;
  u_value t290 = NULL;
  if ((!t290) && u_is_con(s289, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s289, 0);
    u_value v_args = u_field(s289, 1);
    u_value t291 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e292 = &t291;
    u_value l293 = v_args;
    while (u_is_con(l293, &n_list_46_Cons)) {
      u_value v_a = u_field(l293, 0);
      if (u_truth(u_mk_bool(1))) {
        e292 = u_append(e292, f_check_46_subst_2(v_a, v_m));
      }
      l293 = u_field(l293, 1);
    }
    t290 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t291);
  }
  if ((!t290) && u_is_con(s289, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s289, 0);
    u_value v_r = u_field(s289, 1);
    u_value t294 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e295 = &t294;
    u_value l296 = v_args;
    while (u_is_con(l296, &n_list_46_Cons)) {
      u_value v_a = u_field(l296, 0);
      if (u_truth(u_mk_bool(1))) {
        e295 = u_append(e295, f_check_46_subst_2(v_a, v_m));
      }
      l296 = u_field(l296, 1);
    }
    t290 = u_mk_con(&n_syntax_46_TFun, 2, t294, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t290) && u_is_con(s289, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s289, 0);
    u_value t297 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e298 = &t297;
    u_value l299 = v_ts;
    while (u_is_con(l299, &n_list_46_Cons)) {
      u_value v_a = u_field(l299, 0);
      if (u_truth(u_mk_bool(1))) {
        e298 = u_append(e298, f_check_46_subst_2(v_a, v_m));
      }
      l299 = u_field(l299, 1);
    }
    t290 = u_mk_con(&n_syntax_46_TTuple, 1, t297);
  }
  if (!t290) {
    u_value s300 = f_list_46_lookup_2(v_t, v_m);
    u_value t301 = NULL;
    if ((!t301) && u_is_con(s300, &n_option_46_Some)) {
      u_value v_u = u_field(s300, 0);
      t301 = v_u;
    }
    if ((!t301) && u_is_con(s300, &n_option_46_None)) {
      t301 = v_t;
    }
    if (!t301) {
      u_no_case(s300);
    }
    t290 = t301;
  }
  if (!t290) {
    u_no_case(s289);
  }
  return t290;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s302 = v_t;
  u_value t303 = NULL;
  if ((!t303) && u_is_con(s302, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s302, 0);
    u_value s304 = f_map_46_get_2(v_i, v_su);
    u_value t305 = NULL;
    if ((!t305) && u_is_con(s304, &n_option_46_Some)) {
      u_value v_u = u_field(s304, 0);
      t305 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t305) && u_is_con(s304, &n_option_46_None)) {
      t305 = v_t;
    }
    if (!t305) {
      u_no_case(s304);
    }
    t303 = t305;
  }
  if (!t303) {
    t303 = v_t;
  }
  if (!t303) {
    u_no_case(s302);
  }
  return t303;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s306 = f_check_46_resolve_2(v_su, v_t);
  u_value t307 = NULL;
  if ((!t307) && u_is_con(s306, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s306, 0);
    t307 = u_eq(v_i, v_j);
  }
  if ((!t307) && u_is_con(s306, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s306, 1);
    u_value t308 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e309 = &t308;
    u_value l310 = v_args;
    while (u_is_con(l310, &n_list_46_Cons)) {
      u_value v_a = u_field(l310, 0);
      if (u_truth(u_mk_bool(1))) {
        e309 = u_append(e309, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l310 = u_field(l310, 1);
    }
    t307 = f_list_46_contains_2(u_mk_bool(1), t308);
  }
  if ((!t307) && u_is_con(s306, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s306, 0);
    u_value v_r = u_field(s306, 1);
    u_value t311 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e312 = &t311;
    u_value l313 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l313, &n_list_46_Cons)) {
      u_value v_a = u_field(l313, 0);
      if (u_truth(u_mk_bool(1))) {
        e312 = u_append(e312, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l313 = u_field(l313, 1);
    }
    t307 = f_list_46_contains_2(u_mk_bool(1), t311);
  }
  if ((!t307) && u_is_con(s306, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s306, 0);
    u_value t314 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e315 = &t314;
    u_value l316 = v_ts;
    while (u_is_con(l316, &n_list_46_Cons)) {
      u_value v_a = u_field(l316, 0);
      if (u_truth(u_mk_bool(1))) {
        e315 = u_append(e315, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l316 = u_field(l316, 1);
    }
    t307 = f_list_46_contains_2(u_mk_bool(1), t314);
  }
  if (!t307) {
    t307 = u_mk_bool(0);
  }
  if (!t307) {
    u_no_case(s306);
  }
  return t307;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s317 = v_k;
  u_value t318 = NULL;
  if (!t318) {
    u_value v_ps = u_field(s317, 0);
    u_value v_ks = u_field(s317, 1);
    u_value v_r = u_field(s317, 2);
    u_value t319;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t319 = v_r;
    } else {
      t319 = f_check_46_fun_2(v_ks, v_r);
    }
    t318 = u_mk_tuple(2, v_ps, t319);
  }
  if (!t318) {
    u_no_case(s317);
  }
  return t318;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s320 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t321 = NULL;
  if ((!t321) && u_is_con(s320, &n_option_46_Some)) {
    u_value v_kc = u_field(s320, 0);
    t321 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t321) && u_is_con(s320, &n_option_46_None)) {
    t321 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t321) {
    u_no_case(s320);
  }
  return t321;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s322 = f_list_46_lookup_2(v_n, v_env);
  u_value t323 = NULL;
  if ((!t323) && u_is_con(s322, &n_option_46_Some)) {
    u_value v_sc = u_field(s322, 0);
    t323 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t323) && u_is_con(s322, &n_option_46_None)) {
    u_value t324 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e325 = &t324;
    u_value l326 = f_check_46_visible_names_3(v_s, v_n, f_check_46_funs_named_2(f_check_46_State_46_ctx_1(v_s), v_n));
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
        e325 = u_append(e325, t327);
      }
      l326 = u_field(l326, 1);
    }
    u_value t330 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e331 = &t330;
    u_value l332 = f_check_46_visible_names_3(v_s, v_n, f_check_46_constrs_named_2(f_check_46_State_46_ctx_1(v_s), v_n));
    while (u_is_con(l332, &n_list_46_Cons)) {
      u_value v_k = u_field(l332, 0);
      if (u_truth(u_mk_bool(1))) {
        e331 = u_append(e331, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l332 = u_field(l332, 1);
    }
    u_value t333 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e334 = &t333;
    u_value l335 = f_check_46_builtin_1(v_n);
    while (u_is_con(l335, &n_list_46_Cons)) {
      u_value v_sc = u_field(l335, 0);
      if (u_truth(u_mk_bool(1))) {
        e334 = u_append(e334, u_mk_tuple(2, v_n, v_sc));
      }
      l335 = u_field(l335, 1);
    }
    t323 = f_list_46__43__43__2(f_list_46_concat_1(t324), f_list_46__43__43__2(t330, t333));
  }
  if (!t323) {
    u_no_case(s322);
  }
  return t323;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s336 = v_cs;
  u_value t337 = NULL;
  if ((!t337) && u_is_con(s336, &n_list_46_Nil)) {
    t337 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t337) && u_is_con(s336, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s336, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s336, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s336, 0), 1), 1);
    u_value v_rest = u_field(s336, 1);
    u_value t338;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t338 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t338 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t337 = t338;
  }
  if (!t337) {
    u_no_case(s336);
  }
  return t337;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s339 = v_scs;
  u_value t340 = NULL;
  if ((!t340) && u_is_con(s339, &n_list_46_Nil)) {
    t340 = u_mk_bool(0);
  }
  if ((!t340) && u_is_con(s339, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s339, 0), 1);
    u_value v_rest = u_field(s339, 1);
    u_value t341 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t341)) {
      t341 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t340 = t341;
  }
  if (!t340) {
    u_no_case(s339);
  }
  return t340;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t342 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e343 = &t342;
  u_value l344 = v_xs;
  while (u_is_con(l344, &n_list_46_Cons)) {
    u_value v_x = u_field(l344, 0);
    if (u_truth(u_mk_bool(1))) {
      e343 = u_append(e343, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l344 = u_field(l344, 1);
  }
  return t342;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s345 = v_env;
  u_value t346 = NULL;
  if ((!t346) && u_is_con(s345, &n_list_46_Nil)) {
    t346 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t346) && u_is_con(s345, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s345, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s345, 0), 1), 1);
    u_value v_rest = u_field(s345, 1);
    t346 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t346) {
    u_no_case(s345);
  }
  return t346;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t352 = f_check_46_fixed_2(v_su, v_env);
  u_value t353 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t351;
  {
    u_value v_fx = t352;
    u_value v_u = t353;
    u_value t347 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e348 = &t347;
    u_value l349 = f_check_46_free_1(v_u);
    while (u_is_con(l349, &n_list_46_Cons)) {
      u_value v_m = u_field(l349, 0);
      u_value t350 = f_check_46_is_meta_1(v_m);
      if (u_truth(t350)) {
        t350 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t350)) {
        e348 = u_append(e348, v_m);
      }
      l349 = u_field(l349, 1);
    }
    t351 = u_mk_tuple(2, t347, v_u);
  }
  return t351;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t354 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e355 = &t354;
  u_value l356 = v_xs;
  while (u_is_con(l356, &n_list_46_Cons)) {
    u_value v_x = u_field(l356, 0);
    if (u_truth(u_mk_bool(1))) {
      e355 = u_append(e355, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l356 = u_field(l356, 1);
  }
  return t354;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s357 = v_b;
  u_value t358 = NULL;
  if ((!t358) && u_is_con(s357, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s357, 0);
    u_value v_g = u_field(s357, 1);
    t358 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t358) {
    u_no_case(s357);
  }
  return t358;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s359 = v_g;
  u_value t360 = NULL;
  if ((!t360) && u_is_con(s359, &n_option_46_Some)) {
    u_value v_e = u_field(s359, 0);
    t360 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t360) && u_is_con(s359, &n_option_46_None)) {
    t360 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t360) {
    u_no_case(s359);
  }
  return t360;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s361 = f_check_46_resolve_2(v_su, v_t);
  u_value t362 = NULL;
  if ((!t362) && u_is_con(s361, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s361, 0);
    u_value v_args = u_field(s361, 1);
    u_value t363 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e364 = &t363;
    u_value l365 = v_args;
    while (u_is_con(l365, &n_list_46_Cons)) {
      u_value v_a = u_field(l365, 0);
      if (u_truth(u_mk_bool(1))) {
        e364 = u_append(e364, f_check_46_zonk_type_2(v_su, v_a));
      }
      l365 = u_field(l365, 1);
    }
    t362 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t363);
  }
  if ((!t362) && u_is_con(s361, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s361, 0);
    u_value v_r = u_field(s361, 1);
    u_value t366 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e367 = &t366;
    u_value l368 = v_args;
    while (u_is_con(l368, &n_list_46_Cons)) {
      u_value v_a = u_field(l368, 0);
      if (u_truth(u_mk_bool(1))) {
        e367 = u_append(e367, f_check_46_zonk_type_2(v_su, v_a));
      }
      l368 = u_field(l368, 1);
    }
    t362 = u_mk_con(&n_syntax_46_TFun, 2, t366, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t362) && u_is_con(s361, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s361, 0);
    u_value t369 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e370 = &t369;
    u_value l371 = v_ts;
    while (u_is_con(l371, &n_list_46_Cons)) {
      u_value v_a = u_field(l371, 0);
      if (u_truth(u_mk_bool(1))) {
        e370 = u_append(e370, f_check_46_zonk_type_2(v_su, v_a));
      }
      l371 = u_field(l371, 1);
    }
    t362 = u_mk_con(&n_syntax_46_TTuple, 1, t369);
  }
  if (!t362) {
    u_value v_u = s361;
    t362 = v_u;
  }
  if (!t362) {
    u_no_case(s361);
  }
  return t362;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  u_value s372 = v_x;
  u_value t373 = NULL;
  if ((!t373) && u_is_con(s372, &n_syntax_46_Var)) {
    u_value v_n = u_field(s372, 0);
    u_value v_t = u_field(s372, 1);
    t373 = u_mk_con(&n_syntax_46_Var, 2, v_n, f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t373) {
    u_no_case(s372);
  }
  return t373;
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t374 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e375 = &t374;
  u_value l376 = v_xs;
  while (u_is_con(l376, &n_list_46_Cons)) {
    u_value v_x = u_field(l376, 0);
    if (u_truth(u_mk_bool(1))) {
      e375 = u_append(e375, f_check_46_zonk_var_2(v_su, v_x));
    }
    l376 = u_field(l376, 1);
  }
  return t374;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s377 = v_e;
  u_value t378 = NULL;
  if ((!t378) && u_is_con(s377, &n_syntax_46_ELit)) {
    t378 = v_e;
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s377, 0);
    t378 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s377, 0);
    u_value v_args = u_field(s377, 1);
    u_value t379 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e380 = &t379;
    u_value l381 = v_args;
    while (u_is_con(l381, &n_list_46_Cons)) {
      u_value v_a = u_field(l381, 0);
      if (u_truth(u_mk_bool(1))) {
        e380 = u_append(e380, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l381 = u_field(l381, 1);
    }
    t378 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t379);
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s377, 0);
    u_value v_b = u_field(s377, 1);
    t378 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s377, 0);
    u_value v_b = u_field(s377, 1);
    u_value t382 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e383 = &t382;
    u_value l384 = v_ps;
    while (u_is_con(l384, &n_list_46_Cons)) {
      u_value v_p = u_field(l384, 0);
      if (u_truth(u_mk_bool(1))) {
        e383 = u_append(e383, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l384 = u_field(l384, 1);
    }
    t378 = u_mk_con(&n_syntax_46_ELet, 2, t382, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s377, 0);
    u_value v_l = u_field(s377, 1);
    u_value v_r = u_field(s377, 2);
    t378 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s377, 0);
    u_value v_bs = u_field(s377, 1);
    u_value t385 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e386 = &t385;
    u_value l387 = v_bs;
    while (u_is_con(l387, &n_list_46_Cons)) {
      u_value v_b = u_field(l387, 0);
      if (u_truth(u_mk_bool(1))) {
        e386 = u_append(e386, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l387 = u_field(l387, 1);
    }
    t378 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t385);
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s377, 0);
    u_value v_xs = u_field(s377, 1);
    u_value v_ls = u_field(s377, 2);
    u_value v_c = u_field(s377, 3);
    u_value t388 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e389 = &t388;
    u_value l390 = v_ls;
    while (u_is_con(l390, &n_list_46_Cons)) {
      u_value v_l = u_field(l390, 0);
      if (u_truth(u_mk_bool(1))) {
        e389 = u_append(e389, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l390 = u_field(l390, 1);
    }
    t378 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t388, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t378) && u_is_con(s377, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s377, 0);
    u_value t391 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e392 = &t391;
    u_value l393 = v_es;
    while (u_is_con(l393, &n_list_46_Cons)) {
      u_value v_e = u_field(l393, 0);
      if (u_truth(u_mk_bool(1))) {
        e392 = u_append(e392, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l393 = u_field(l393, 1);
    }
    t378 = u_mk_con(&n_syntax_46_ETuple, 1, t391);
  }
  if (!t378) {
    u_no_case(s377);
  }
  return t378;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s394 = v_p;
  u_value t395 = NULL;
  if (!t395) {
    u_value v_x = u_field(s394, 0);
    u_value v_e = u_field(s394, 1);
    t395 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t395) {
    u_no_case(s394);
  }
  return t395;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s396 = v_b;
  u_value t397 = NULL;
  if ((!t397) && u_is_con(s396, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s396, 0);
    u_value v_g = u_field(s396, 1);
    u_value v_e = u_field(s396, 2);
    t397 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t397) {
    u_no_case(s396);
  }
  return t397;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s398 = v_p;
  u_value t399 = NULL;
  if ((!t399) && u_is_con(s398, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s398, 0);
    t399 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t399) && u_is_con(s398, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s398, 0);
    u_value v_args = u_field(s398, 1);
    u_value t400 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e401 = &t400;
    u_value l402 = v_args;
    while (u_is_con(l402, &n_list_46_Cons)) {
      u_value v_a = u_field(l402, 0);
      if (u_truth(u_mk_bool(1))) {
        e401 = u_append(e401, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l402 = u_field(l402, 1);
    }
    t399 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t400);
  }
  if ((!t399) && u_is_con(s398, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s398, 0);
    u_value t403 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e404 = &t403;
    u_value l405 = v_ps;
    while (u_is_con(l405, &n_list_46_Cons)) {
      u_value v_q = u_field(l405, 0);
      if (u_truth(u_mk_bool(1))) {
        e404 = u_append(e404, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l405 = u_field(l405, 1);
    }
    t399 = u_mk_con(&n_syntax_46_PTuple, 1, t403);
  }
  if (!t399) {
    t399 = v_p;
  }
  if (!t399) {
    u_no_case(s398);
  }
  return t399;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s406 = v_x;
  u_value t407 = NULL;
  if ((!t407) && u_is_con(s406, &n_syntax_46_SSkip)) {
    t407 = v_x;
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s406, 0);
    u_value t408 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e409 = &t408;
    u_value l410 = v_ps;
    while (u_is_con(l410, &n_list_46_Cons)) {
      u_value v_p = u_field(l410, 0);
      if (u_truth(u_mk_bool(1))) {
        e409 = u_append(e409, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l410 = u_field(l410, 1);
    }
    t407 = u_mk_con(&n_syntax_46_SAssign, 1, t408);
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s406, 0);
    u_value v_ys = u_field(s406, 1);
    u_value t411 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e412 = &t411;
    u_value l413 = v_ys;
    while (u_is_con(l413, &n_list_46_Cons)) {
      u_value v_y = u_field(l413, 0);
      if (u_truth(u_mk_bool(1))) {
        e412 = u_append(e412, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l413 = u_field(l413, 1);
    }
    t407 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t411);
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s406, 0);
    u_value v_l = u_field(s406, 1);
    u_value v_r = u_field(s406, 2);
    t407 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s406, 0);
    u_value v_b = u_field(s406, 1);
    t407 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s406, 0);
    u_value v_ins = u_field(s406, 1);
    u_value v_outs = u_field(s406, 2);
    u_value v_refs = u_field(s406, 3);
    u_value t414 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e415 = &t414;
    u_value l416 = v_ins;
    while (u_is_con(l416, &n_list_46_Cons)) {
      u_value v_e = u_field(l416, 0);
      if (u_truth(u_mk_bool(1))) {
        e415 = u_append(e415, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l416 = u_field(l416, 1);
    }
    t407 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t414, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s406, 0);
    u_value v_bs = u_field(s406, 1);
    u_value t417 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e418 = &t417;
    u_value l419 = v_bs;
    while (u_is_con(l419, &n_list_46_Cons)) {
      u_value v_b = u_field(l419, 0);
      if (u_truth(u_mk_bool(1))) {
        e418 = u_append(e418, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l419 = u_field(l419, 1);
    }
    t407 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t417);
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s406, 0);
    t407 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s406, 0);
    u_value v_t = u_field(s406, 1);
    t407 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_STry)) {
    u_value v_b = u_field(s406, 0);
    u_value v_hs = u_field(s406, 1);
    u_value t420 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e421 = &t420;
    u_value l422 = v_hs;
    while (u_is_con(l422, &n_list_46_Cons)) {
      u_value v_h = u_field(l422, 0);
      if (u_truth(u_mk_bool(1))) {
        e421 = u_append(e421, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l422 = u_field(l422, 1);
    }
    t407 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t420);
  }
  if (!t407) {
    u_no_case(s406);
  }
  return t407;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s423 = v_b;
  u_value t424 = NULL;
  if ((!t424) && u_is_con(s423, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s423, 0);
    u_value v_g = u_field(s423, 1);
    u_value v_y = u_field(s423, 2);
    t424 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t424) {
    u_no_case(s423);
  }
  return t424;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s425 = v_h;
  u_value t426 = NULL;
  if ((!t426) && u_is_con(s425, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s425, 0);
    u_value v_p = u_field(s425, 1);
    u_value v_g = u_field(s425, 2);
    u_value v_b = u_field(s425, 3);
    t426 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t426) {
    u_no_case(s425);
  }
  return t426;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s427 = v_c;
  u_value t428 = NULL;
  if ((!t428) && u_is_con(s427, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s427, 0);
    u_value v_ps = u_field(s427, 1);
    u_value v_xss = u_field(s427, 2);
    u_value v_r = u_field(s427, 3);
    u_value v_b = u_field(s427, 4);
    u_value t429 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e430 = &t429;
    u_value l431 = v_xss;
    while (u_is_con(l431, &n_list_46_Cons)) {
      u_value v_xs = u_field(l431, 0);
      if (u_truth(u_mk_bool(1))) {
        e430 = u_append(e430, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l431 = u_field(l431, 1);
    }
    t428 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t429, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t428) && u_is_con(s427, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s427, 0);
    u_value v_ps = u_field(s427, 1);
    u_value v_ins = u_field(s427, 2);
    u_value v_outs = u_field(s427, 3);
    u_value v_refs = u_field(s427, 4);
    u_value v_b = u_field(s427, 5);
    t428 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t428) && u_is_con(s427, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s427, 0);
    t428 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t428) && u_is_con(s427, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s427, 0);
    u_value v_b = u_field(s427, 1);
    t428 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t428) && u_is_con(s427, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s427, 0);
    u_value v_b = u_field(s427, 1);
    t428 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t428) {
    t428 = v_c;
  }
  if (!t428) {
    u_no_case(s427);
  }
  return t428;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t432;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t432 = u_mk_con(&n_option_46_None, 0);
  } else {
    t432 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t432;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s433 = v_x;
  u_value t434 = NULL;
  if ((!t434) && u_is_con(s433, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s433, 1);
    t434 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t434) && u_is_con(s433, &n_syntax_46_SIf)) {
    u_value v_l = u_field(s433, 1);
    u_value v_r = u_field(s433, 2);
    t434 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r));
  }
  if ((!t434) && u_is_con(s433, &n_syntax_46_SWhile)) {
    u_value v_b = u_field(s433, 1);
    t434 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t434) && u_is_con(s433, &n_syntax_46_SMatch)) {
    u_value v_bs = u_field(s433, 1);
    t434 = f_check_46_unknown_in_branches_1(v_bs);
  }
  if ((!t434) && u_is_con(s433, &n_syntax_46_SRaise)) {
    u_value v_t = u_field(s433, 1);
    t434 = f_check_46_unknown_type_1(v_t);
  }
  if ((!t434) && u_is_con(s433, &n_syntax_46_STry)) {
    u_value v_b = u_field(s433, 0);
    u_value v_hs = u_field(s433, 1);
    t434 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t434) {
    t434 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t434) {
    u_no_case(s433);
  }
  return t434;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s435 = v_xs;
  u_value t436 = NULL;
  if ((!t436) && u_is_con(s435, &n_list_46_Nil)) {
    t436 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t436) && u_is_con(s435, &n_list_46_Cons)) {
    u_value v_y = u_field(s435, 0);
    u_value v_ys = u_field(s435, 1);
    t436 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t436) {
    u_no_case(s435);
  }
  return t436;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s437 = v_bs;
  u_value t438 = NULL;
  if ((!t438) && u_is_con(s437, &n_list_46_Nil)) {
    t438 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t438) && (u_is_con(s437, &n_list_46_Cons) && u_is_con(u_field(s437, 0), &n_syntax_46_Branch))) {
    u_value v_y = u_field(u_field(s437, 0), 2);
    u_value v_rest = u_field(s437, 1);
    t438 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t438) {
    u_no_case(s437);
  }
  return t438;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s439 = v_hs;
  u_value t440 = NULL;
  if ((!t440) && u_is_con(s439, &n_list_46_Nil)) {
    t440 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t440) && (u_is_con(s439, &n_list_46_Cons) && (u_is_con(u_field(s439, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s439, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s439, 0), 3);
    u_value v_rest = u_field(s439, 1);
    t440 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t440) && (u_is_con(s439, &n_list_46_Cons) && u_is_con(u_field(s439, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s439, 0), 0);
    u_value v_b = u_field(u_field(s439, 0), 3);
    u_value v_rest = u_field(s439, 1);
    t440 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t440) {
    u_no_case(s439);
  }
  return t440;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s441 = v_c;
  u_value t442 = NULL;
  if ((!t442) && u_is_con(s441, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s441, 5);
    t442 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s441, 1);
    t442 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s441, 1);
    t442 = f_check_46_unknown_in_1(v_b);
  }
  if (!t442) {
    t442 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t442) {
    u_no_case(s441);
  }
  return t442;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s443 = v_p;
  u_value t444 = NULL;
  if ((!t444) && u_is_con(s443, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s443, 0);
    u_value v_args = u_field(s443, 1);
    t444 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s443, 0);
    t444 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s443, 0);
    t444 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t444) {
    t444 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t444) {
    u_no_case(s443);
  }
  return t444;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s445 = v_rows;
  u_value t446 = NULL;
  if ((!t446) && u_is_con(s445, &n_list_46_Nil)) {
    t446 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t446) && (u_is_con(s445, &n_list_46_Cons) && u_is_con(u_field(s445, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s445, 0), 0);
    u_value v_rest = u_field(s445, 1);
    u_value s447 = f_check_46_head_of_1(v_p);
    u_value t448 = NULL;
    if ((!t448) && u_is_con(s447, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s447, 0), 0);
      t448 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t448) && u_is_con(s447, &n_option_46_None)) {
      t448 = f_check_46_heads_1(v_rest);
    }
    if (!t448) {
      u_no_case(s447);
    }
    t446 = t448;
  }
  if ((!t446) && (u_is_con(s445, &n_list_46_Cons) && u_is_con(u_field(s445, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s445, 1);
    t446 = f_check_46_heads_1(v_rest);
  }
  if (!t446) {
    u_no_case(s445);
  }
  return t446;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s449 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t450 = NULL;
  if ((!t450) && u_is_con(s449, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s449, 0), 0);
    u_value v_ks = u_field(u_field(s449, 0), 1);
    u_value t451 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e452 = &t451;
    u_value l453 = v_ks;
    while (u_is_con(l453, &n_list_46_Cons)) {
      u_value v_a = u_field(l453, 0);
      if (u_truth(u_mk_bool(1))) {
        e452 = u_append(e452, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l453 = u_field(l453, 1);
    }
    t450 = t451;
  }
  if ((!t450) && u_is_con(s449, &n_option_46_None)) {
    t450 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t450) {
    u_no_case(s449);
  }
  return t450;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s454 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t455 = NULL;
  if ((!t455) && (u_is_con(s454, &n_syntax_46_TSort) && (u_is_con(u_field(s454, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s454, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s454, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s454, 1), &n_list_46_Nil)))))) {
    t455 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t455) && u_is_con(s454, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s454, 0);
    t455 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t455) && u_is_con(s454, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s454, 0);
    u_value v_args = u_field(s454, 1);
    u_value s456 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t457 = NULL;
    if ((!t457) && u_is_con(s456, &n_option_46_Some)) {
      u_value v_ks = u_field(s456, 0);
      u_value t458 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e459 = &t458;
      u_value l460 = v_ks;
      while (u_is_con(l460, &n_list_46_Cons)) {
        u_value v_k = u_field(l460, 0);
        if (u_truth(u_mk_bool(1))) {
          e459 = u_append(e459, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l460 = u_field(l460, 1);
      }
      t457 = u_mk_con(&n_option_46_Some, 1, t458);
    }
    if ((!t457) && u_is_con(s456, &n_option_46_None)) {
      t457 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t457) {
      u_no_case(s456);
    }
    t455 = t457;
  }
  if (!t455) {
    t455 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t455) {
    u_no_case(s454);
  }
  return t455;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s461 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t462 = NULL;
  if ((!t462) && u_is_con(s461, &n_option_46_Some)) {
    u_value v_ks = u_field(s461, 0);
    u_value s463 = f_list_46_lookup_2(v_h, v_ks);
    u_value t464 = NULL;
    if ((!t464) && u_is_con(s463, &n_option_46_Some)) {
      u_value v_ts = u_field(s463, 0);
      t464 = v_ts;
    }
    if ((!t464) && u_is_con(s463, &n_option_46_None)) {
      t464 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t464) {
      u_no_case(s463);
    }
    t462 = t464;
  }
  if ((!t462) && u_is_con(s461, &n_option_46_None)) {
    t462 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t462) {
    u_no_case(s461);
  }
  return t462;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s465 = v_ks;
  u_value t466 = NULL;
  if ((!t466) && u_is_con(s465, &n_list_46_Nil)) {
    t466 = u_mk_bool(1);
  }
  if ((!t466) && u_is_con(s465, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s465, 0), 0);
    u_value v_rest = u_field(s465, 1);
    u_value t467 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t467)) {
      t467 = f_check_46_covers_2(v_hs, v_rest);
    }
    t466 = t467;
  }
  if (!t466) {
    u_no_case(s465);
  }
  return t466;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s468 = v_rows;
  u_value t469 = NULL;
  if ((!t469) && u_is_con(s468, &n_list_46_Nil)) {
    t469 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t469) && (u_is_con(s468, &n_list_46_Cons) && u_is_con(u_field(s468, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s468, 0), 0);
    u_value v_ps = u_field(u_field(s468, 0), 1);
    u_value v_rest = u_field(s468, 1);
    u_value s470 = f_check_46_head_of_1(v_p);
    u_value t471 = NULL;
    if ((!t471) && u_is_con(s470, &n_option_46_Some)) {
      t471 = f_check_46_defaults_1(v_rest);
    }
    if ((!t471) && u_is_con(s470, &n_option_46_None)) {
      t471 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t471) {
      u_no_case(s470);
    }
    t469 = t471;
  }
  if ((!t469) && (u_is_con(s468, &n_list_46_Cons) && u_is_con(u_field(s468, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s468, 1);
    t469 = f_check_46_defaults_1(v_rest);
  }
  if (!t469) {
    u_no_case(s468);
  }
  return t469;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s472 = v_rows;
  u_value t473 = NULL;
  if ((!t473) && u_is_con(s472, &n_list_46_Nil)) {
    t473 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t473) && (u_is_con(s472, &n_list_46_Cons) && u_is_con(u_field(s472, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s472, 0), 0);
    u_value v_ps = u_field(u_field(s472, 0), 1);
    u_value v_rest = u_field(s472, 1);
    u_value s474 = f_check_46_head_of_1(v_p);
    u_value t475 = NULL;
    if ((!t475) && u_is_con(s474, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s474, 0), 0);
      u_value v_args = u_field(u_field(s474, 0), 1);
      u_value t476;
      if (u_truth(u_eq(v_g, v_h))) {
        t476 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t476 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t475 = t476;
    }
    if ((!t475) && u_is_con(s474, &n_option_46_None)) {
      t475 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t475) {
      u_no_case(s474);
    }
    t473 = t475;
  }
  if ((!t473) && (u_is_con(s472, &n_list_46_Cons) && u_is_con(u_field(s472, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s472, 1);
    t473 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t473) {
    u_no_case(s472);
  }
  return t473;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s477 = u_mk_tuple(2, v_q, v_ts);
  u_value t478 = NULL;
  if ((!t478) && u_is_con(u_field(s477, 0), &n_list_46_Nil)) {
    t478 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t478) && (u_is_con(u_field(s477, 0), &n_list_46_Cons) && u_is_con(u_field(s477, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s477, 0), 0);
    u_value v_ps = u_field(u_field(s477, 0), 1);
    u_value v_t = u_field(u_field(s477, 1), 0);
    u_value v_rest = u_field(u_field(s477, 1), 1);
    u_value s479 = f_check_46_head_of_1(v_p);
    u_value t480 = NULL;
    if ((!t480) && u_is_con(s479, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s479, 0), 0);
      u_value v_args = u_field(u_field(s479, 0), 1);
      t480 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t480) && u_is_con(s479, &n_option_46_None)) {
      u_value s481 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t482 = NULL;
      if ((!t482) && u_is_con(s481, &n_option_46_Some)) {
        u_value v_ks = u_field(s481, 0);
        u_value t483;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t483 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t483 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t482 = t483;
      }
      if ((!t482) && u_is_con(s481, &n_option_46_None)) {
        t482 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t482) {
        u_no_case(s481);
      }
      t480 = t482;
    }
    if (!t480) {
      u_no_case(s479);
    }
    t478 = t480;
  }
  if ((!t478) && (u_is_con(u_field(s477, 0), &n_list_46_Cons) && u_is_con(u_field(s477, 1), &n_list_46_Nil))) {
    t478 = u_mk_bool(0);
  }
  if (!t478) {
    u_no_case(s477);
  }
  return t478;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s484 = v_ks;
  u_value t485 = NULL;
  if ((!t485) && u_is_con(s484, &n_list_46_Nil)) {
    t485 = u_mk_bool(0);
  }
  if ((!t485) && u_is_con(s484, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s484, 0), 0);
    u_value v_args = u_field(u_field(s484, 0), 1);
    u_value v_more = u_field(s484, 1);
    u_value t486 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t486)) {
      t486 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t485 = t486;
  }
  if (!t485) {
    u_no_case(s484);
  }
  return t485;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s487 = v_h;
  u_value t488 = NULL;
  if ((!t488) && u_is_con(s487, &n_check_46_HCon)) {
    u_value v_n = u_field(s487, 0);
    u_value t489;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t489 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t489 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t488 = t489;
  }
  if ((!t488) && u_is_con(s487, &n_check_46_HLit)) {
    u_value v_v = u_field(s487, 0);
    t488 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t488) {
    u_no_case(s487);
  }
  return t488;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s490 = v_ts;
  u_value t491 = NULL;
  if ((!t491) && u_is_con(s490, &n_list_46_Nil)) {
    u_value t492;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t492 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t492 = u_mk_con(&n_option_46_None, 0);
    }
    t491 = t492;
  }
  if ((!t491) && u_is_con(s490, &n_list_46_Cons)) {
    u_value v_t = u_field(s490, 0);
    u_value v_rest = u_field(s490, 1);
    t491 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t491) {
    u_no_case(s490);
  }
  return t491;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s493 = v_cs;
  u_value t494 = NULL;
  if ((!t494) && u_is_con(s493, &n_option_46_Some)) {
    u_value v_ks = u_field(s493, 0);
    u_value t495;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t495 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t495 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t494 = t495;
  }
  if ((!t494) && u_is_con(s493, &n_option_46_None)) {
    t494 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t494) {
    u_no_case(s493);
  }
  return t494;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s496 = v_ks;
  u_value t497 = NULL;
  if ((!t497) && u_is_con(s496, &n_list_46_Nil)) {
    t497 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t497) && u_is_con(s496, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s496, 0), 0);
    u_value v_args = u_field(u_field(s496, 0), 1);
    u_value v_more = u_field(s496, 1);
    u_value s498 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t499 = NULL;
    if ((!t499) && u_is_con(s498, &n_option_46_Some)) {
      u_value v_ws = u_field(s498, 0);
      t499 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t499) && u_is_con(s498, &n_option_46_None)) {
      t499 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t499) {
      u_no_case(s498);
    }
    t497 = t499;
  }
  if (!t497) {
    u_no_case(s496);
  }
  return t497;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s500 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t501 = NULL;
  if ((!t501) && u_is_con(s500, &n_option_46_Some)) {
    u_value v_ws = u_field(s500, 0);
    t501 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t501) && u_is_con(s500, &n_option_46_None)) {
    t501 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t501) {
    u_no_case(s500);
  }
  return t501;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s502 = v_cs;
  u_value t503 = NULL;
  if ((!t503) && u_is_con(s502, &n_option_46_Some)) {
    u_value v_ks = u_field(s502, 0);
    u_value t504;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t504 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t504 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t503 = t504;
  }
  if ((!t503) && u_is_con(s502, &n_option_46_None)) {
    t503 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t503) {
    u_no_case(s502);
  }
  return t503;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s505 = v_ks;
  u_value t506 = NULL;
  if ((!t506) && u_is_con(s505, &n_list_46_Nil)) {
    t506 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t506) && u_is_con(s505, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s505, 0), 0);
    u_value v_args = u_field(u_field(s505, 0), 1);
    u_value v_rest = u_field(s505, 1);
    u_value t507;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t507 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t507 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t506 = t507;
  }
  if (!t506) {
    u_no_case(s505);
  }
  return t506;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value s508 = *r_s;
    int d509 = 0;
    if ((!d509) && u_is_con(s508, &n_check_46_State)) {
      u_value v_c = u_field(s508, 0);
      u_value v_su = u_field(s508, 1);
      u_value v_n = u_field(s508, 2);
      u_value v_m = u_field(s508, 3);
      u_value v_ms = u_field(s508, 4);
      u_value v_vs = u_field(s508, 5);
      d509 = 1;
      {
        u_value t510 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(v_n, u_mk_int(1)));
        u_value t511 = u_mk_con(&n_check_46_State, 6, v_c, v_su, u_add(v_n, u_mk_int(1)), v_m, v_ms, v_vs);
        *r_t = t510;
        *r_s = t511;
      }
    }
    if (!d509) {
      u_no_case(s508);
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
    u_value s512 = *r_s;
    int d513 = 0;
    if ((!d513) && u_is_con(s512, &n_check_46_State)) {
      u_value v_c = u_field(s512, 0);
      u_value v_su = u_field(s512, 1);
      u_value v_n = u_field(s512, 2);
      u_value v_m = u_field(s512, 3);
      u_value v_ms = u_field(s512, 4);
      u_value v_vs = u_field(s512, 5);
      d513 = 1;
      {
        u_value t514 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_put_3(v_i, v_t, v_su), v_n, v_m, v_ms, v_vs);
        *r_s = t514;
      }
    }
    if (!d513) {
      u_no_case(s512);
    }
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t515 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t515;
    u_value t516 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t516;
    u_value s517 = u_mk_tuple(2, v_a, v_b);
    int d518 = 0;
    if ((!d518) && (u_is_con(u_field(s517, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s517, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s517, 0), 0);
      u_value v_j = u_field(u_field(s517, 1), 0);
      d518 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d518) && u_is_con(u_field(s517, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s517, 0), 0);
      d518 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d518) && u_is_con(u_field(s517, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s517, 1), 0);
      d518 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d518) && (u_is_con(u_field(s517, 0), &n_syntax_46_TParam) && u_is_con(u_field(s517, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s517, 0), 0);
      u_value v_y = u_field(u_field(s517, 1), 0);
      d518 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d518) && (u_is_con(u_field(s517, 0), &n_syntax_46_TSort) && u_is_con(u_field(s517, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s517, 0), 0);
      u_value v_xs = u_field(u_field(s517, 0), 1);
      u_value v_g = u_field(u_field(s517, 1), 0);
      u_value v_ys = u_field(u_field(s517, 1), 1);
      d518 = 1;
      {
        u_value t519 = u_eq(v_f, v_g);
        if (u_truth(t519)) {
          t519 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t519)) {
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
    if ((!d518) && (u_is_con(u_field(s517, 0), &n_syntax_46_TFun) && u_is_con(u_field(s517, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s517, 0), 0);
      u_value v_r1 = u_field(u_field(s517, 0), 1);
      u_value v_ys = u_field(u_field(s517, 1), 0);
      u_value v_r2 = u_field(u_field(s517, 1), 1);
      d518 = 1;
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
    if ((!d518) && (u_is_con(u_field(s517, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s517, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s517, 0), 0);
      u_value v_ys = u_field(u_field(s517, 1), 0);
      d518 = 1;
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
    if (!d518) {
      d518 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d518) {
      u_no_case(s517);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s520 = u_mk_tuple(2, v_xs, v_ys);
    int d521 = 0;
    if ((!d521) && (u_is_con(u_field(s520, 0), &n_list_46_Cons) && u_is_con(u_field(s520, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s520, 0), 0);
      u_value v_xs1 = u_field(u_field(s520, 0), 1);
      u_value v_y = u_field(u_field(s520, 1), 0);
      u_value v_ys1 = u_field(u_field(s520, 1), 1);
      d521 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d521) && (u_is_con(u_field(s520, 0), &n_list_46_Nil) && u_is_con(u_field(s520, 1), &n_list_46_Nil))) {
      d521 = 1;
      {
      }
    }
    if (!d521) {
      d521 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d521) {
      u_no_case(s520);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s522 = v_xs;
    int d523 = 0;
    if ((!d523) && u_is_con(s522, &n_list_46_Nil)) {
      d523 = 1;
      {
        u_value t524 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t524;
      }
    }
    if ((!d523) && u_is_con(s522, &n_list_46_Cons)) {
      u_value v_more = u_field(s522, 1);
      d523 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t525 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t525;
      }
    }
    if (!d523) {
      u_no_case(s522);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s526 = v_ps;
    int d527 = 0;
    if ((!d527) && u_is_con(s526, &n_list_46_Nil)) {
      d527 = 1;
      {
        u_value t528 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t528;
      }
    }
    if ((!d527) && u_is_con(s526, &n_list_46_Cons)) {
      u_value v_p = u_field(s526, 0);
      u_value v_qs = u_field(s526, 1);
      d527 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t529 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t529;
      }
    }
    if (!d527) {
      u_no_case(s526);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s530 = v_sc;
    int d531 = 0;
    if (!d531) {
      u_value v_ps = u_field(s530, 0);
      u_value v_u = u_field(s530, 1);
      d531 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t532 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t532;
      }
    }
    if (!d531) {
      u_no_case(s530);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s533 = f_list_46_repeated_1(v_ns);
    int d534 = 0;
    if ((!d534) && u_is_con(s533, &n_option_46_Some)) {
      u_value v_n = u_field(s533, 0);
      d534 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d534) && u_is_con(s533, &n_option_46_None)) {
      d534 = 1;
      {
      }
    }
    if (!d534) {
      u_no_case(s533);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s535 = f_list_46_repeated_1(v_ns);
    int d536 = 0;
    if ((!d536) && u_is_con(s535, &n_option_46_Some)) {
      u_value v_n = u_field(s535, 0);
      d536 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d536) && u_is_con(s535, &n_option_46_None)) {
      d536 = 1;
      {
      }
    }
    if (!d536) {
      u_no_case(s535);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s537 = v_ns;
    int d538 = 0;
    if ((!d538) && (u_is_con(s537, &n_list_46_Cons) && u_is_con(u_field(s537, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s537, 0);
      d538 = 1;
      {
        u_value t539 = v_k;
        *r_m = t539;
      }
    }
    if ((!d538) && u_is_con(s537, &n_list_46_Nil)) {
      d538 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d538) {
      d538 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d538) {
      u_no_case(s537);
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
    u_value s540 = v_t;
    int d541 = 0;
    if ((!d541) && u_is_con(s540, &n_syntax_46_THole)) {
      d541 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d541) && u_is_con(s540, &n_syntax_46_TParam)) {
      d541 = 1;
      {
        u_value t542 = v_t;
        *r_u = t542;
      }
    }
    if ((!d541) && u_is_con(s540, &n_syntax_46_TMeta)) {
      d541 = 1;
      {
        u_value t543 = v_t;
        *r_u = t543;
      }
    }
    if ((!d541) && u_is_con(s540, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s540, 0);
      u_value v_r = u_field(s540, 1);
      d541 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t544 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t544;
      }
    }
    if ((!d541) && u_is_con(s540, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s540, 0);
      d541 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t545 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t545;
      }
    }
    if ((!d541) && u_is_con(s540, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s540, 0);
      u_value v_args = u_field(s540, 1);
      d541 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t546 = v_t;
            *r_u = t546;
          }
        } else {
          {
            u_value s547 = f_check_46_visible_names_3(*r_s, v_n, f_check_46_sorts_named_2(f_check_46_State_46_ctx_1(*r_s), v_n));
            int d548 = 0;
            if ((!d548) && (u_is_con(s547, &n_list_46_Cons) && u_is_con(u_field(s547, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s547, 0);
              d548 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d548) && u_is_con(s547, &n_list_46_Nil)) {
              d548 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d548) {
              u_value v_ms = s547;
              d548 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d548) {
              u_no_case(s547);
            }
          }
        }
      }
    }
    if (!d541) {
      u_no_case(s540);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s549 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d550 = 0;
    if ((!d550) && u_is_con(s549, &n_option_46_Some)) {
      u_value v_k = u_field(s549, 0);
      d550 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t551 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t551;
      }
    }
    if ((!d550) && u_is_con(s549, &n_option_46_None)) {
      d550 = 1;
      {
        u_value s552 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d553 = 0;
        if ((!d553) && u_is_con(s552, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s552, 0), 0);
          u_value v_rhs = u_field(u_field(s552, 0), 1);
          d553 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t554 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t554;
          }
        }
        if ((!d553) && u_is_con(s552, &n_option_46_None)) {
          d553 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d553) {
          u_no_case(s552);
        }
      }
    }
    if (!d550) {
      u_no_case(s549);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s555 = v_ts;
    int d556 = 0;
    if ((!d556) && u_is_con(s555, &n_list_46_Nil)) {
      d556 = 1;
      {
        u_value t557 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t557;
      }
    }
    if ((!d556) && u_is_con(s555, &n_list_46_Cons)) {
      u_value v_t = u_field(s555, 0);
      u_value v_more = u_field(s555, 1);
      d556 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t558 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t558;
      }
    }
    if (!d556) {
      u_no_case(s555);
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
    u_value s559 = v_ts;
    int d560 = 0;
    if ((!d560) && u_is_con(s559, &n_list_46_Nil)) {
      d560 = 1;
      {
        u_value t561 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t561;
      }
    }
    if ((!d560) && u_is_con(s559, &n_list_46_Cons)) {
      u_value v_t = u_field(s559, 0);
      u_value v_more = u_field(s559, 1);
      d560 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t562 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t562;
      }
    }
    if (!d560) {
      u_no_case(s559);
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
    u_value s563 = v_xs;
    int d564 = 0;
    if ((!d564) && u_is_con(s563, &n_list_46_Nil)) {
      d564 = 1;
      {
        u_value t565 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t565;
      }
    }
    if ((!d564) && (u_is_con(s563, &n_list_46_Cons) && u_is_con(u_field(s563, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s563, 0), 0);
      u_value v_t = u_field(u_field(s563, 0), 1);
      u_value v_more = u_field(s563, 1);
      d564 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t566 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t566;
      }
    }
    if (!d564) {
      u_no_case(s563);
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
    u_value s567 = v_xs;
    int d568 = 0;
    if ((!d568) && u_is_con(s567, &n_list_46_Nil)) {
      d568 = 1;
      {
        u_value t569 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t569;
      }
    }
    if ((!d568) && (u_is_con(s567, &n_list_46_Cons) && u_is_con(u_field(s567, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s567, 0), 0);
      u_value v_t = u_field(u_field(s567, 0), 1);
      u_value v_more = u_field(s567, 1);
      d568 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t570 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t570;
      }
    }
    if (!d568) {
      u_no_case(s567);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s571 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d572 = 0;
    if ((!d572) && u_is_con(s571, &n_list_46_Nil)) {
      d572 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d572) && (u_is_con(s571, &n_list_46_Cons) && u_is_con(u_field(s571, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s571, 0), 0);
      u_value v_one = u_field(u_field(s571, 0), 1);
      d572 = 1;
      {
        u_value t573 = v_m1;
        u_value t574 = v_one;
        *r_m = t573;
        *r_sc = t574;
      }
    }
    if (!d572) {
      u_value v_cs = s571;
      d572 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s575 = f_check_46_with_arity_2(v_cs, v_k);
        int d576 = 0;
        if ((!d576) && (u_is_con(s575, &n_list_46_Cons) && u_is_con(u_field(s575, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s575, 0), 0);
          u_value v_one = u_field(u_field(s575, 0), 1);
          d576 = 1;
          {
            u_value t577 = v_m1;
            u_value t578 = v_one;
            *r_m = t577;
            *r_sc = t578;
          }
        }
        if ((!d576) && u_is_con(s575, &n_list_46_Nil)) {
          d576 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d576) {
          u_value v_more = s575;
          d576 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d576) {
          u_no_case(s575);
        }
      }
    }
    if (!d572) {
      u_no_case(s571);
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
    u_value t579 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t579;
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
    u_value v_es = NULL;
    u_value v_ts = NULL;
    u_value v_bs = NULL;
    u_value s580 = v_e;
    int d581 = 0;
    if ((!d581) && u_is_con(s580, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s580, 0);
      d581 = 1;
      {
        u_value t582 = v_e;
        u_value t583 = f_check_46_literal_1(v_v);
        *r_e1 = t582;
        *r_t = t583;
      }
    }
    if ((!d581) && (u_is_con(s580, &n_syntax_46_EVar) && u_is_con(u_field(s580, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s580, 0), 0);
      u_value v_a = u_field(u_field(s580, 0), 1);
      d581 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, f_prelude_46__45__1(u_mk_int(1)), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t584 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t585 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t584;
        *r_t = t585;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s580, 0);
      u_value v_args = u_field(s580, 1);
      d581 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s580, 0);
      u_value v_body = u_field(s580, 1);
      d581 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t586 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t587 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t586;
        *r_t = t587;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s580, 0);
      u_value v_body = u_field(s580, 1);
      d581 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t588 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t589 = v_u;
        *r_e1 = t588;
        *r_t = t589;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s580, 0);
      u_value v_left = u_field(s580, 1);
      u_value v_right = u_field(s580, 2);
      d581 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t590 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t591 = v_u;
        *r_e1 = t590;
        *r_t = t591;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s580, 0);
      u_value v_bound = u_field(s580, 1);
      u_value v_lists = u_field(s580, 2);
      u_value v_cond = u_field(s580, 3);
      d581 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t592 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t593 = f_check_46_tlist_1(v_u);
        *r_e1 = t592;
        *r_t = t593;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s580, 0);
      d581 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t594 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t595 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t594;
        *r_t = t595;
      }
    }
    if ((!d581) && u_is_con(s580, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s580, 0);
      u_value v_cases = u_field(s580, 1);
      d581 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t596 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e597 = &t596;
        u_value l598 = v_bs;
        while (u_is_con(l598, &n_list_46_Cons)) {
          u_value v_br = u_field(l598, 0);
          if (u_truth(u_mk_bool(1))) {
            e597 = u_append(e597, f_check_46_case_of_1(v_br));
          }
          l598 = u_field(l598, 1);
        }
        p_check_46_coverage(*r_s, v_u, t596);
        u_value t599 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t599;
      }
    }
    if (!d581) {
      u_no_case(s580);
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
    u_value s600 = v_f;
    int d601 = 0;
    if ((!d601) && (u_is_con(s600, &n_syntax_46_EVar) && (u_is_con(u_field(s600, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s600, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s600, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s600, 0), 0), 1);
      u_value v_a = u_field(u_field(s600, 0), 1);
      u_value t602 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t602)) {
        t602 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t602)) {
        d601 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d601) && (u_is_con(s600, &n_syntax_46_EVar) && u_is_con(u_field(s600, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s600, 0), 0);
      u_value v_a = u_field(u_field(s600, 0), 1);
      d601 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t603 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t603;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t604 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t604;
          }
        }
      }
    }
    if (!d601) {
      d601 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t605 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t605;
      }
    }
    if (!d601) {
      u_no_case(s600);
    }
  }
}

void p_check_46_field_access(u_value v_env, u_value v_f, u_value v_a, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_arg1 = NULL;
    u_value v_u = NULL;
    u_value v_acc = NULL;
    u_value v_x = NULL;
    u_value s606 = v_args;
    int d607 = 0;
    if ((!d607) && (u_is_con(s606, &n_list_46_Cons) && u_is_con(u_field(s606, 1), &n_list_46_Nil))) {
      u_value v_arg = u_field(s606, 0);
      d607 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s608 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d609 = 0;
        if ((!d609) && (u_is_con(s608, &n_syntax_46_TSort) && (u_is_con(u_field(s608, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s608, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s608, 0), 0), 0);
          u_value v_tn = u_field(u_field(s608, 0), 1);
          d609 = 1;
          {
            u_value t610 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t610;
          }
        }
        if (!d609) {
          u_value v_v = s608;
          d609 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d609) {
          u_no_case(s608);
        }
        u_value s611 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d612 = 0;
        if ((!d612) && (u_is_con(s611, &n_list_46_Cons) && u_is_con(u_field(s611, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s611, 0);
          d612 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d612) {
          d612 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d612) {
          u_no_case(s611);
        }
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)), *r_t), r_s);
        u_value t613 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, u_mk_con(&n_list_46_Nil, 0)));
        *r_e1 = t613;
      }
    }
    if (!d607) {
      d607 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d607) {
      u_no_case(s606);
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
    u_value s614 = u_mk_tuple(2, v_es, v_ts);
    int d615 = 0;
    if ((!d615) && (u_is_con(u_field(s614, 0), &n_list_46_Cons) && u_is_con(u_field(s614, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s614, 0), 0);
      u_value v_more = u_field(u_field(s614, 0), 1);
      u_value v_t = u_field(u_field(s614, 1), 0);
      u_value v_ts1 = u_field(u_field(s614, 1), 1);
      d615 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t616 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t616;
      }
    }
    if ((!d615) && (u_is_con(u_field(s614, 0), &n_list_46_Nil) && u_is_con(u_field(s614, 1), &n_list_46_Nil))) {
      d615 = 1;
      {
        u_value t617 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t617;
      }
    }
    if (!d615) {
      d615 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d615) {
      u_no_case(s614);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s618 = v_es;
    int d619 = 0;
    if ((!d619) && u_is_con(s618, &n_list_46_Nil)) {
      d619 = 1;
      {
        u_value t620 = u_mk_con(&n_list_46_Nil, 0);
        u_value t621 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t620;
        *r_ts = t621;
      }
    }
    if ((!d619) && u_is_con(s618, &n_list_46_Cons)) {
      u_value v_e = u_field(s618, 0);
      u_value v_more = u_field(s618, 1);
      d619 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t622 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t623 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t622;
        *r_ts = t623;
      }
    }
    if (!d619) {
      u_no_case(s618);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s624 = u_mk_tuple(2, v_xs, v_lists);
    int d625 = 0;
    if ((!d625) && (u_is_con(u_field(s624, 0), &n_list_46_Cons) && u_is_con(u_field(s624, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s624, 0), 0);
      u_value v_ys = u_field(u_field(s624, 0), 1);
      u_value v_e = u_field(u_field(s624, 1), 0);
      u_value v_es = u_field(u_field(s624, 1), 1);
      d625 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t626 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t626;
      }
    }
    if (!d625) {
      d625 = 1;
      {
        u_value t627 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t627;
      }
    }
    if (!d625) {
      u_no_case(s624);
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
    u_value s628 = v_cases;
    int d629 = 0;
    if ((!d629) && u_is_con(s628, &n_list_46_Nil)) {
      d629 = 1;
      {
        u_value t630 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t630;
      }
    }
    if ((!d629) && (u_is_con(s628, &n_list_46_Cons) && u_is_con(u_field(s628, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s628, 0), 0);
      u_value v_g = u_field(u_field(s628, 0), 1);
      u_value v_b = u_field(u_field(s628, 0), 2);
      u_value v_more = u_field(s628, 1);
      d629 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t631 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t631;
      }
    }
    if (!d629) {
      u_no_case(s628);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s632 = v_g;
    int d633 = 0;
    if ((!d633) && u_is_con(s632, &n_option_46_Some)) {
      u_value v_e = u_field(s632, 0);
      d633 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t634 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t634;
      }
    }
    if ((!d633) && u_is_con(s632, &n_option_46_None)) {
      d633 = 1;
      {
        u_value t635 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t635;
      }
    }
    if (!d633) {
      u_no_case(s632);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t636 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e637 = &t636;
    u_value l638 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l638, &n_list_46_Cons)) {
      u_value v_p = u_field(l638, 0);
      if (u_truth(u_mk_bool(1))) {
        e637 = u_append(e637, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l638 = u_field(l638, 1);
    }
    u_value s639 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t636, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d640 = 0;
    if ((!d640) && (u_is_con(s639, &n_option_46_Some) && u_is_con(u_field(s639, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s639, 0), 0);
      d640 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d640) {
      d640 = 1;
      {
      }
    }
    if (!d640) {
      u_no_case(s639);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s641 = v_cs;
  u_value t642 = NULL;
  if ((!t642) && u_is_con(s641, &n_list_46_Nil)) {
    t642 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t642) && (u_is_con(s641, &n_list_46_Cons) && u_equals(u_field(u_field(s641, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s641, 0), 0);
    u_value v_rest = u_field(s641, 1);
    t642 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t642) && (u_is_con(s641, &n_list_46_Cons) && u_equals(u_field(u_field(s641, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s641, 1);
    t642 = f_check_46_unguarded_1(v_rest);
  }
  if (!t642) {
    u_no_case(s641);
  }
  return t642;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s643 = v_cs;
    int d644 = 0;
    if ((!d644) && u_is_con(s643, &n_list_46_Nil)) {
      d644 = 1;
      {
      }
    }
    if ((!d644) && u_is_con(s643, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s643, 0), 0);
      u_value v_open = u_field(u_field(s643, 0), 1);
      u_value v_rest = u_field(s643, 1);
      d644 = 1;
      {
        u_value t645 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e646 = &t645;
        u_value l647 = v_before;
        while (u_is_con(l647, &n_list_46_Cons)) {
          u_value v_q = u_field(l647, 0);
          if (u_truth(u_mk_bool(1))) {
            e646 = u_append(e646, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l647 = u_field(l647, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t645, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d644) {
      u_no_case(s643);
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
    u_value s648 = v_p;
    int d649 = 0;
    if ((!d649) && u_is_con(s648, &n_syntax_46_PWild)) {
      d649 = 1;
      {
        u_value t650 = v_p;
        u_value t651 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t650;
        *r_xs = t651;
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s648, 0);
      d649 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t652 = v_p;
        u_value t653 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t652;
        *r_xs = t653;
      }
    }
    if ((!d649) && (u_is_con(s648, &n_syntax_46_PVar) && u_is_con(u_field(s648, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s648, 0), 0);
      u_value v_a = u_field(u_field(s648, 0), 1);
      d649 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_State_46_ctx_1(*r_s), v_n)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t654 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t655 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t654;
            *r_xs = t655;
          }
        }
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s648, 0);
      u_value v_args = u_field(s648, 1);
      d649 = 1;
      {
        u_value s656 = f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_State_46_ctx_1(*r_s), v_n));
        int d657 = 0;
        if ((!d657) && (u_is_con(s656, &n_list_46_Cons) && u_is_con(u_field(s656, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s656, 0);
          d657 = 1;
          {
            u_value t658 = v_k1;
            v_k = t658;
          }
        }
        if ((!d657) && u_is_con(s656, &n_list_46_Nil)) {
          d657 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d657) {
          u_value v_ks = s656;
          d657 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d657) {
          u_no_case(s656);
        }
        u_value s659 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d660 = 0;
        if ((!d660) && u_is_con(s659, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s659, 0), 0);
          u_value v_ks = u_field(u_field(s659, 0), 1);
          u_value v_r = u_field(u_field(s659, 0), 2);
          d660 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t661 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e662 = &t661;
            u_value l663 = v_ks;
            while (u_is_con(l663, &n_list_46_Cons)) {
              u_value v_kt = u_field(l663, 0);
              if (u_truth(u_mk_bool(1))) {
                e662 = u_append(e662, f_check_46_subst_2(v_kt, v_m));
              }
              l663 = u_field(l663, 1);
            }
            p_check_46_binder_list(v_args, t661, &v_qs, r_xs, r_s);
            u_value t664 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t664;
          }
        }
        if ((!d660) && u_is_con(s659, &n_option_46_None)) {
          d660 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d660) {
          u_no_case(s659);
        }
      }
    }
    if ((!d649) && u_is_con(s648, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s648, 0);
      d649 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t665 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t665;
      }
    }
    if (!d649) {
      u_no_case(s648);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s666 = u_mk_tuple(2, v_ps, v_ts);
    int d667 = 0;
    if ((!d667) && (u_is_con(u_field(s666, 0), &n_list_46_Cons) && u_is_con(u_field(s666, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s666, 0), 0);
      u_value v_more = u_field(u_field(s666, 0), 1);
      u_value v_t = u_field(u_field(s666, 1), 0);
      u_value v_ts1 = u_field(u_field(s666, 1), 1);
      d667 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t668 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t669 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t668;
        *r_xs = t669;
      }
    }
    if ((!d667) && (u_is_con(u_field(s666, 0), &n_list_46_Nil) && u_is_con(u_field(s666, 1), &n_list_46_Nil))) {
      d667 = 1;
      {
        u_value t670 = u_mk_con(&n_list_46_Nil, 0);
        u_value t671 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t670;
        *r_xs = t671;
      }
    }
    if (!d667) {
      d667 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d667) {
      u_no_case(s666);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s672 = v_x;
    int d673 = 0;
    if ((!d673) && u_is_con(s672, &n_syntax_46_Var)) {
      u_value v_n = u_field(s672, 0);
      u_value v_a = u_field(s672, 1);
      d673 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s674 = f_list_46_lookup_2(v_n, v_env);
        int d675 = 0;
        if ((!d675) && u_is_con(s674, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s674, 0), 1);
          d675 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t676 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t676;
          }
        }
        if ((!d675) && u_is_con(s674, &n_option_46_None)) {
          d675 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d675) {
          u_no_case(s674);
        }
      }
    }
    if (!d673) {
      u_no_case(s672);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s677 = v_xs;
    int d678 = 0;
    if ((!d678) && u_is_con(s677, &n_list_46_Nil)) {
      d678 = 1;
      {
        u_value t679 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t679;
      }
    }
    if ((!d678) && u_is_con(s677, &n_list_46_Cons)) {
      u_value v_x = u_field(s677, 0);
      u_value v_more = u_field(s677, 1);
      d678 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t680 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t680;
      }
    }
    if (!d678) {
      u_no_case(s677);
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
    u_value s681 = v_x;
    int d682 = 0;
    if ((!d682) && u_is_con(s681, &n_syntax_46_SSkip)) {
      d682 = 1;
      {
        u_value t683 = v_x;
        *r_y = t683;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s681, 0);
      d682 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t684 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t684;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s681, 0);
      u_value v_body = u_field(s681, 1);
      d682 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t685 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t685;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s681, 0);
      u_value v_left = u_field(s681, 1);
      u_value v_right = u_field(s681, 2);
      d682 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t686 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t686;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s681, 0);
      u_value v_body = u_field(s681, 1);
      d682 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t687 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t687;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s681, 0);
      u_value v_ins = u_field(s681, 1);
      u_value v_outs = u_field(s681, 2);
      u_value v_refs = u_field(s681, 3);
      d682 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s681, 0);
      u_value v_cases = u_field(s681, 1);
      d682 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t688 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e689 = &t688;
        u_value l690 = v_bs;
        while (u_is_con(l690, &n_list_46_Cons)) {
          u_value v_br = u_field(l690, 0);
          if (u_truth(u_mk_bool(1))) {
            e689 = u_append(e689, f_check_46_case_of_1(v_br));
          }
          l690 = u_field(l690, 1);
        }
        p_check_46_coverage(*r_s, v_t, t688);
        u_value t691 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t691;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s681, 0);
      d682 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t692 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t692;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s681, 0);
      u_value v_a = u_field(s681, 1);
      d682 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t693 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t693;
      }
    }
    if ((!d682) && u_is_con(s681, &n_syntax_46_STry)) {
      u_value v_body = u_field(s681, 0);
      u_value v_handlers = u_field(s681, 1);
      d682 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t694 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t694;
      }
    }
    if (!d682) {
      u_no_case(s681);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s695 = v_xs;
  u_value t696 = NULL;
  if ((!t696) && (u_is_con(s695, &n_list_46_Cons) && (u_is_con(u_field(s695, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s695, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s695, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s695, 0), 0), 0), 0);
    u_value v_rest = u_field(s695, 1);
    t696 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t696) {
    t696 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t696) {
    u_no_case(s695);
  }
  return t696;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s697 = v_xs;
    int d698 = 0;
    if ((!d698) && u_is_con(s697, &n_list_46_Nil)) {
      d698 = 1;
      {
      }
    }
    if ((!d698) && (u_is_con(s697, &n_list_46_Cons) && u_is_con(u_field(s697, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s697, 0), 0);
      u_value v_t = u_field(u_field(s697, 0), 1);
      u_value v_rest = u_field(s697, 1);
      d698 = 1;
      {
        u_value t699 = f_check_46_holes_1(v_t);
        if (u_truth(t699)) {
          t699 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t699)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d698) {
      u_no_case(s697);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s700 = v_xs;
    int d701 = 0;
    if ((!d701) && u_is_con(s700, &n_list_46_Nil)) {
      d701 = 1;
      {
        u_value t702 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t702;
      }
    }
    if ((!d701) && u_is_con(s700, &n_list_46_Cons)) {
      u_value v_x = u_field(s700, 0);
      u_value v_more = u_field(s700, 1);
      d701 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t703 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t703;
      }
    }
    if (!d701) {
      u_no_case(s700);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t708;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_704 = U_LIT("os_args");
    static struct u_object l_os_read_705 = U_LIT("os_read");
    static struct u_object l_os_write_706 = U_LIT("os_write");
    static struct u_object l_os_library_707 = U_LIT("os_library");
    t708 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_704), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_705), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_706), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_707), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t708 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t708;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t709 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e710 = &t709;
  static struct u_object l_Program_711 = U_LIT("Program");
  u_value l712 = f_ctx_46_names_of_2(v_c, &l_Program_711);
  while (u_is_con(l712, &n_list_46_Cons)) {
    u_value v_d = u_field(l712, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e710 = u_append(e710, v_d);
    }
    l712 = u_field(l712, 1);
  }
  u_value s713 = t709;
  u_value t714 = NULL;
  if ((!t714) && u_is_con(s713, &n_list_46_Cons)) {
    u_value v_d = u_field(s713, 0);
    static struct u_object l_read_715 = U_LIT("read");
    static struct u_object l_library_716 = U_LIT("library");
    t714 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_715), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_716), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t714) {
    t714 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t714) {
    u_no_case(s713);
  }
  return t714;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s717 = f_check_46_visible_names_3(*r_s, v_p, f_check_46_procs_named_2(f_check_46_State_46_ctx_1(*r_s), v_p));
    int d718 = 0;
    if ((!d718) && (u_is_con(s717, &n_list_46_Cons) && u_is_con(u_field(s717, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s717, 0);
      d718 = 1;
      {
        u_value t719 = v_q1;
        v_q = t719;
      }
    }
    if ((!d718) && u_is_con(s717, &n_list_46_Nil)) {
      d718 = 1;
      {
        u_value t720 = v_p;
        v_q = t720;
      }
    }
    if (!d718) {
      u_value v_qs = s717;
      d718 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d718) {
      u_no_case(s717);
    }
    u_value s721 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d722 = 0;
    if ((!d722) && u_is_con(s721, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s721, 0), 0);
      u_value v_is = u_field(u_field(s721, 0), 1);
      u_value v_os = u_field(u_field(s721, 0), 2);
      u_value v_rs = u_field(u_field(s721, 0), 3);
      d722 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t723 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e724 = &t723;
        u_value l725 = v_is;
        while (u_is_con(l725, &n_list_46_Cons)) {
          u_value v_t = u_field(l725, 0);
          if (u_truth(u_mk_bool(1))) {
            e724 = u_append(e724, f_check_46_subst_2(v_t, v_m));
          }
          l725 = u_field(l725, 1);
        }
        p_check_46_checks(v_env, v_ins, t723, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t726 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e727 = &t726;
        u_value l728 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l728, &n_list_46_Cons)) {
          u_value v_t = u_field(l728, 0);
          if (u_truth(u_mk_bool(1))) {
            e727 = u_append(e727, f_check_46_subst_2(v_t, v_m));
          }
          l728 = u_field(l728, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t726, r_s);
        u_value t729 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t729;
      }
    }
    if ((!d722) && u_is_con(s721, &n_option_46_None)) {
      d722 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d722) {
      u_no_case(s721);
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
    u_value s730 = v_cases;
    int d731 = 0;
    if ((!d731) && u_is_con(s730, &n_list_46_Nil)) {
      d731 = 1;
      {
        u_value t732 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t732;
      }
    }
    if ((!d731) && (u_is_con(s730, &n_list_46_Cons) && u_is_con(u_field(s730, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s730, 0), 0);
      u_value v_g = u_field(u_field(s730, 0), 1);
      u_value v_b = u_field(u_field(s730, 0), 2);
      u_value v_more = u_field(s730, 1);
      d731 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t733 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t733;
      }
    }
    if (!d731) {
      u_no_case(s730);
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
    u_value s734 = v_hs;
    int d735 = 0;
    if ((!d735) && u_is_con(s734, &n_list_46_Nil)) {
      d735 = 1;
      {
        u_value t736 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t736;
      }
    }
    if ((!d735) && (u_is_con(s734, &n_list_46_Cons) && u_is_con(u_field(s734, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s734, 0), 0);
      u_value v_p = u_field(u_field(s734, 0), 1);
      u_value v_g = u_field(u_field(s734, 0), 2);
      u_value v_b = u_field(u_field(s734, 0), 3);
      u_value v_more = u_field(s734, 1);
      d735 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t737 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t737;
      }
    }
    if (!d735) {
      u_no_case(s734);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t738 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t738)) {
      t738 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t739 = t738;
    if (!u_truth(t739)) {
      t739 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t740 = t739;
    if (!u_truth(t740)) {
      t740 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t740)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t741 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t741)) {
      t741 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t742 = t741;
    if (!u_truth(t742)) {
      t742 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t742)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_743 = U_LIT("Bool");
    static struct u_object l_Int_744 = U_LIT("Int");
    static struct u_object l_Str_745 = U_LIT("Str");
    static struct u_object l_Char_746 = U_LIT("Char");
    static struct u_object l__45__62__747 = U_LIT("->");
    u_value t748 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_743, u_mk_con(&n_list_46_Cons, 2, &l_Int_744, u_mk_con(&n_list_46_Cons, 2, &l_Str_745, u_mk_con(&n_list_46_Cons, 2, &l_Char_746, u_mk_con(&n_list_46_Cons, 2, &l__45__62__747, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t748)) {
      t748 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t749 = t748;
    if (!u_truth(t749)) {
      t749 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t749)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t750 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e751 = &t750;
    u_value l752 = v_ts;
    while (u_is_con(l752, &n_list_46_Cons)) {
      u_value v_t = u_field(l752, 0);
      if (u_truth(u_mk_bool(1))) {
        e751 = u_append(e751, f_check_46_free_1(v_t));
      }
      l752 = u_field(l752, 1);
    }
    u_value t753 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t750)), f_check_46_params_1(v_ps));
    v_undeclared = t753;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s754 = v_cs;
    int d755 = 0;
    if ((!d755) && u_is_con(s754, &n_list_46_Nil)) {
      d755 = 1;
      {
      }
    }
    if ((!d755) && (u_is_con(s754, &n_list_46_Cons) && u_is_con(u_field(u_field(s754, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s754, 0), 0);
      u_value v_n = u_field(u_field(u_field(s754, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s754, 0), 1), 1);
      u_value v_rest = u_field(s754, 1);
      d755 = 1;
      {
        u_value t756 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t756;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t757 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t757;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d755) && u_is_con(s754, &n_list_46_Cons)) {
      u_value v_rest = u_field(s754, 1);
      d755 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d755) {
      u_no_case(s754);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s758 = v_cs;
    int d759 = 0;
    if ((!d759) && u_is_con(s758, &n_list_46_Nil)) {
      d759 = 1;
      {
      }
    }
    if ((!d759) && (u_is_con(s758, &n_list_46_Cons) && u_is_con(u_field(u_field(s758, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s758, 0), 0);
      u_value v_n = u_field(u_field(u_field(s758, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s758, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s758, 0), 1), 2);
      u_value v_rest = u_field(s758, 1);
      d759 = 1;
      {
        u_value t760 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t760;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t761 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t761;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d759) && u_is_con(s758, &n_list_46_Cons)) {
      u_value v_rest = u_field(s758, 1);
      d759 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d759) {
      u_no_case(s758);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s762 = v_cs;
    int d763 = 0;
    if ((!d763) && u_is_con(s762, &n_list_46_Nil)) {
      d763 = 1;
      {
      }
    }
    if ((!d763) && (u_is_con(s762, &n_list_46_Cons) && u_is_con(u_field(u_field(s762, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s762, 0), 0);
      u_value v_n = u_field(u_field(u_field(s762, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s762, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s762, 0), 1), 2);
      u_value v_rest = u_field(s762, 1);
      d763 = 1;
      {
        u_value t764 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t764;
        p_check_46_unique_ids(v_ps);
        u_value t765 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e766 = &t765;
        u_value l767 = v_ks;
        while (u_is_con(l767, &n_list_46_Cons)) {
          u_value v_k = u_field(l767, 0);
          if (u_truth(u_mk_bool(1))) {
            e766 = u_append(e766, f_syntax_46_Constr_46_name_1(v_k));
          }
          l767 = u_field(l767, 1);
        }
        u_value t768 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t765));
        *r_s = t768;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d763) && u_is_con(s762, &n_list_46_Cons)) {
      u_value v_rest = u_field(s762, 1);
      d763 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d763) {
      u_no_case(s762);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s769 = v_ks;
    int d770 = 0;
    if ((!d770) && u_is_con(s769, &n_list_46_Nil)) {
      d770 = 1;
      {
      }
    }
    if ((!d770) && (u_is_con(s769, &n_list_46_Cons) && u_is_con(u_field(s769, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s769, 0), 0);
      u_value v_args = u_field(u_field(s769, 0), 1);
      u_value v_rest = u_field(s769, 1);
      d770 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t771 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t771;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d770) {
      u_no_case(s769);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s772 = v_xss;
  u_value t773 = NULL;
  if ((!t773) && u_is_con(s772, &n_list_46_Nil)) {
    t773 = v_r;
  }
  if ((!t773) && u_is_con(s772, &n_list_46_Cons)) {
    u_value v_xs = u_field(s772, 0);
    u_value v_rest = u_field(s772, 1);
    t773 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t773) {
    u_no_case(s772);
  }
  return t773;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s774 = v_xss;
  u_value t775 = NULL;
  if ((!t775) && u_is_con(s774, &n_list_46_Nil)) {
    t775 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t775) && u_is_con(s774, &n_list_46_Cons)) {
    u_value v_xs = u_field(s774, 0);
    t775 = v_xs;
  }
  if (!t775) {
    u_no_case(s774);
  }
  return t775;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s776 = v_xss;
    int d777 = 0;
    if ((!d777) && u_is_con(s776, &n_list_46_Nil)) {
      d777 = 1;
      {
        u_value t778 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t778;
      }
    }
    if ((!d777) && u_is_con(s776, &n_list_46_Cons)) {
      u_value v_xs = u_field(s776, 0);
      u_value v_more = u_field(s776, 1);
      d777 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t779 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t779;
      }
    }
    if (!d777) {
      u_no_case(s776);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s780 = v_cs;
    int d781 = 0;
    if ((!d781) && u_is_con(s780, &n_list_46_Nil)) {
      d781 = 1;
      {
      }
    }
    if ((!d781) && (u_is_con(s780, &n_list_46_Cons) && u_is_con(u_field(u_field(s780, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s780, 0), 0);
      u_value v_n = u_field(u_field(u_field(s780, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s780, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s780, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s780, 0), 1), 3);
      u_value v_rest = u_field(s780, 1);
      d781 = 1;
      {
        u_value t782 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t782;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t783 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t783;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d781) && u_is_con(s780, &n_list_46_Cons)) {
      u_value v_rest = u_field(s780, 1);
      d781 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d781) {
      u_no_case(s780);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s784 = v_cs;
    int d785 = 0;
    if ((!d785) && u_is_con(s784, &n_list_46_Nil)) {
      d785 = 1;
      {
      }
    }
    if ((!d785) && (u_is_con(s784, &n_list_46_Cons) && u_is_con(u_field(u_field(s784, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s784, 0), 0);
      u_value v_n = u_field(u_field(u_field(s784, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s784, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s784, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s784, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s784, 0), 1), 4);
      u_value v_rest = u_field(s784, 1);
      d785 = 1;
      {
        u_value t786 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t786;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t787 = f_syntax_46_types_1(v_xs);
        v_ts = t787;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t788 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t788;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d785) && u_is_con(s784, &n_list_46_Cons)) {
      u_value v_rest = u_field(s784, 1);
      d785 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d785) {
      u_no_case(s784);
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
    u_value s789 = v_c;
    int d790 = 0;
    if ((!d790) && u_is_con(s789, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s789, 0);
      u_value v_ps = u_field(s789, 1);
      u_value v_bound = u_field(s789, 2);
      u_value v_res = u_field(s789, 3);
      u_value v_body = u_field(s789, 4);
      d790 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t791 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t791;
      }
    }
    if ((!d790) && u_is_con(s789, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s789, 0);
      u_value v_ps = u_field(s789, 1);
      u_value v_ins = u_field(s789, 2);
      u_value v_outs = u_field(s789, 3);
      u_value v_refs = u_field(s789, 4);
      u_value v_body = u_field(s789, 5);
      d790 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t792 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t792;
      }
    }
    if ((!d790) && u_is_con(s789, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s789, 0);
      d790 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t793 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t793;
      }
    }
    if ((!d790) && u_is_con(s789, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s789, 0);
      u_value v_body = u_field(s789, 1);
      d790 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t794 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t794;
      }
    }
    if ((!d790) && u_is_con(s789, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s789, 0);
      u_value v_body = u_field(s789, 1);
      d790 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t795 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t795;
      }
    }
    if (!d790) {
      d790 = 1;
      {
        u_value t796 = v_c;
        *r_c1 = t796;
      }
    }
    if (!d790) {
      u_no_case(s789);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s797 = v_cs;
    int d798 = 0;
    if ((!d798) && u_is_con(s797, &n_list_46_Nil)) {
      d798 = 1;
      {
        u_value t799 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t799;
      }
    }
    if ((!d798) && u_is_con(s797, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s797, 0), 0);
      u_value v_c = u_field(u_field(s797, 0), 1);
      u_value v_more = u_field(s797, 1);
      d798 = 1;
      {
        u_value t800 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t800;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t801 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t801;
        u_value s802 = f_check_46_unknown_in_cmd_1(v_c2);
        int d803 = 0;
        if ((!d803) && u_is_con(s802, &n_option_46_Some)) {
          u_value v_t = u_field(s802, 0);
          d803 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d803) && u_is_con(s802, &n_option_46_None)) {
          d803 = 1;
          {
          }
        }
        if (!d803) {
          u_no_case(s802);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t804 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t804;
      }
    }
    if (!d798) {
      u_no_case(s797);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s805 = v_cs;
  u_value t806 = NULL;
  if ((!t806) && u_is_con(s805, &n_list_46_Nil)) {
    t806 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t806) && (u_is_con(s805, &n_list_46_Cons) && u_is_con(u_field(u_field(s805, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s805, 0), 1), 0);
    u_value v_more = u_field(s805, 1);
    t806 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t806) && (u_is_con(s805, &n_list_46_Cons) && u_is_con(u_field(u_field(s805, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s805, 0), 1), 0);
    u_value v_more = u_field(s805, 1);
    t806 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t806) && u_is_con(s805, &n_list_46_Cons)) {
    u_value v_more = u_field(s805, 1);
    t806 = f_check_46_block_names_1(v_more);
  }
  if (!t806) {
    u_no_case(s805);
  }
  return t806;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s807 = v_p;
    int d808 = 0;
    if ((!d808) && u_is_con(s807, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s807, 0);
      d808 = 1;
      {
        u_value t809 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t809;
        u_value t810 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t810;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t811 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t811;
      }
    }
    if (!d808) {
      u_no_case(s807);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_812 = U_LIT("main");
    u_value t813 = f_check_46_start_2(&l_main_812, u_mk_con(&n_list_46_Nil, 0));
    v_s = t813;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t814 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t814;
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
  static struct u_object l_List_815 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_815), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_816 = U_LIT("list");
  static struct u_object l_List_817 = U_LIT("List");
  static struct u_object l_A_818 = U_LIT("A");
  static struct u_object l_Nil_819 = U_LIT("Nil");
  static struct u_object l_Cons_820 = U_LIT("Cons");
  static struct u_object l_A_821 = U_LIT("A");
  static struct u_object l_A_822 = U_LIT("A");
  static struct u_object l_list_823 = U_LIT("list");
  static struct u_object l_len_824 = U_LIT("len");
  static struct u_object l_A_825 = U_LIT("A");
  static struct u_object l_xs_826 = U_LIT("xs");
  static struct u_object l_A_827 = U_LIT("A");
  static struct u_object l_xs_828 = U_LIT("xs");
  static struct u_object l_Nil_829 = U_LIT("Nil");
  static struct u_object l_Cons_830 = U_LIT("Cons");
  static struct u_object l_ys_831 = U_LIT("ys");
  static struct u_object l__43__832 = U_LIT("+");
  static struct u_object l_len_833 = U_LIT("len");
  static struct u_object l_ys_834 = U_LIT("ys");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_816, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_817), u_mk_con(&n_list_46_Cons, 2, &l_A_818, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_819), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_820), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_821), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_822)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_823), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_824), u_mk_con(&n_list_46_Cons, 2, &l_A_825, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_826), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_827))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_828), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_829), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_830), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_831), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__832, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_833, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_834), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s835 = v_t;
  u_value t836 = NULL;
  if ((!t836) && u_is_con(s835, &n_c_46_CtVoid)) {
    static struct u_object l_void_837 = U_LIT("void");
    t836 = &l_void_837;
  }
  if ((!t836) && u_is_con(s835, &n_c_46_CtInt)) {
    static struct u_object l_int_838 = U_LIT("int");
    t836 = &l_int_838;
  }
  if ((!t836) && u_is_con(s835, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s835, 0);
    t836 = v_n;
  }
  if ((!t836) && (u_is_con(s835, &n_c_46_CtPtr) && u_is_con(u_field(s835, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s835, 0), 0);
    static struct u_object l__42__839 = U_LIT("*");
    t836 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__839);
  }
  if ((!t836) && u_is_con(s835, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s835, 0);
    static struct u_object l__32__42__840 = U_LIT(" *");
    t836 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__840);
  }
  if (!t836) {
    u_no_case(s835);
  }
  return t836;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s841 = v_t;
  u_value t842 = NULL;
  if ((!t842) && u_is_con(s841, &n_c_46_CtPtr)) {
    t842 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t842) {
    static struct u_object l__32__843 = U_LIT(" ");
    t842 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__843, v_x));
  }
  if (!t842) {
    u_no_case(s841);
  }
  return t842;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s844 = v_e;
  u_value t845 = NULL;
  if ((!t845) && u_is_con(s844, &n_c_46_CeVar)) {
    u_value v_x = u_field(s844, 0);
    t845 = v_x;
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeNum)) {
    u_value v_n = u_field(s844, 0);
    t845 = u_str_show(v_n);
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeStr)) {
    u_value v_s = u_field(s844, 0);
    t845 = u_str_quote(v_s);
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeCall)) {
    u_value v_f = u_field(s844, 0);
    u_value v_args = u_field(s844, 1);
    static struct u_object l__40__846 = U_LIT("(");
    static struct u_object l__44__32__847 = U_LIT(", ");
    u_value t848 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e849 = &t848;
    u_value l850 = v_args;
    while (u_is_con(l850, &n_list_46_Cons)) {
      u_value v_a = u_field(l850, 0);
      if (u_truth(u_mk_bool(1))) {
        e849 = u_append(e849, f_c_46_c_expr_1(v_a));
      }
      l850 = u_field(l850, 1);
    }
    static struct u_object l__41__851 = U_LIT(")");
    t845 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__846, u_concat(u_str_join(&l__44__32__847, t848), &l__41__851)));
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s844, 0);
    u_value v_a = u_field(s844, 1);
    t845 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s844, 0);
    u_value v_a = u_field(s844, 1);
    u_value v_b = u_field(s844, 2);
    static struct u_object l__32__852 = U_LIT(" ");
    static struct u_object l__32__853 = U_LIT(" ");
    t845 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__852, u_concat(v_o, u_concat(&l__32__853, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s844, 0);
    u_value v_i = u_field(s844, 1);
    static struct u_object l__91__854 = U_LIT("[");
    static struct u_object l__93__855 = U_LIT("]");
    t845 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__854, u_concat(f_c_46_c_expr_1(v_i), &l__93__855)));
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s844, 0);
    u_value v_f = u_field(s844, 1);
    static struct u_object l__45__62__856 = U_LIT("->");
    t845 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__856, v_f));
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeCast)) {
    u_value v_t = u_field(s844, 0);
    u_value v_a = u_field(s844, 1);
    static struct u_object l__40__857 = U_LIT("(");
    static struct u_object l__41__858 = U_LIT(")");
    t845 = u_concat(&l__40__857, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__858, f_c_46_c_operand_1(v_a))));
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeCond)) {
    u_value v_c = u_field(s844, 0);
    u_value v_a = u_field(s844, 1);
    u_value v_b = u_field(s844, 2);
    static struct u_object l__32__63__32__859 = U_LIT(" ? ");
    static struct u_object l__32__58__32__860 = U_LIT(" : ");
    t845 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__859, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__860, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t845) && u_is_con(s844, &n_c_46_CeInit)) {
    u_value v_es = u_field(s844, 0);
    static struct u_object l__123__861 = U_LIT("{");
    static struct u_object l__44__32__862 = U_LIT(", ");
    u_value t863 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e864 = &t863;
    u_value l865 = v_es;
    while (u_is_con(l865, &n_list_46_Cons)) {
      u_value v_a = u_field(l865, 0);
      if (u_truth(u_mk_bool(1))) {
        e864 = u_append(e864, f_c_46_c_expr_1(v_a));
      }
      l865 = u_field(l865, 1);
    }
    static struct u_object l__125__866 = U_LIT("}");
    t845 = u_concat(&l__123__861, u_concat(u_str_join(&l__44__32__862, t863), &l__125__866));
  }
  if (!t845) {
    u_no_case(s844);
  }
  return t845;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s867 = v_e;
  u_value t868 = NULL;
  if ((!t868) && u_is_con(s867, &n_c_46_CeVar)) {
    t868 = f_c_46_c_expr_1(v_e);
  }
  if ((!t868) && u_is_con(s867, &n_c_46_CeNum)) {
    u_value v_n = u_field(s867, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t868 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t868) && u_is_con(s867, &n_c_46_CeStr)) {
    t868 = f_c_46_c_expr_1(v_e);
  }
  if ((!t868) && u_is_con(s867, &n_c_46_CeCall)) {
    t868 = f_c_46_c_expr_1(v_e);
  }
  if ((!t868) && u_is_con(s867, &n_c_46_CeIndex)) {
    t868 = f_c_46_c_expr_1(v_e);
  }
  if ((!t868) && u_is_con(s867, &n_c_46_CeArrow)) {
    t868 = f_c_46_c_expr_1(v_e);
  }
  if (!t868) {
    static struct u_object l__40__869 = U_LIT("(");
    static struct u_object l__41__870 = U_LIT(")");
    t868 = u_concat(&l__40__869, u_concat(f_c_46_c_expr_1(v_e), &l__41__870));
  }
  if (!t868) {
    u_no_case(s867);
  }
  return t868;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s871 = v_s;
  u_value t872 = NULL;
  if ((!t872) && u_is_con(s871, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s871, 0);
    static struct u_object l__59__873 = U_LIT(";");
    t872 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__873)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t872) && (u_is_con(s871, &n_c_46_CsDecl) && u_is_con(u_field(s871, 2), &n_option_46_None))) {
    u_value v_t = u_field(s871, 0);
    u_value v_x = u_field(s871, 1);
    static struct u_object l__59__874 = U_LIT(";");
    t872 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__874)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t872) && (u_is_con(s871, &n_c_46_CsDecl) && u_is_con(u_field(s871, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s871, 0);
    u_value v_x = u_field(s871, 1);
    u_value v_e = u_field(u_field(s871, 2), 0);
    static struct u_object l__32__61__32__875 = U_LIT(" = ");
    static struct u_object l__59__876 = U_LIT(";");
    t872 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__875, u_concat(f_c_46_c_expr_1(v_e), &l__59__876)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t872) && u_is_con(s871, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s871, 0);
    u_value v_e = u_field(s871, 1);
    static struct u_object l__32__61__32__877 = U_LIT(" = ");
    static struct u_object l__59__878 = U_LIT(";");
    t872 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__877, u_concat(f_c_46_c_expr_1(v_e), &l__59__878)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t872) && (u_is_con(s871, &n_c_46_CsIf) && u_is_con(u_field(s871, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s871, 0);
    u_value v_l = u_field(s871, 1);
    static struct u_object l_if_32__40__879 = U_LIT("if (");
    static struct u_object l__41__32__123__880 = U_LIT(") {");
    static struct u_object l__32__32__881 = U_LIT("  ");
    static struct u_object l__125__882 = U_LIT("}");
    t872 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__879, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__880))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__881), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__882), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t872) && u_is_con(s871, &n_c_46_CsIf)) {
    u_value v_c = u_field(s871, 0);
    u_value v_l = u_field(s871, 1);
    u_value v_r = u_field(s871, 2);
    static struct u_object l_if_32__40__883 = U_LIT("if (");
    static struct u_object l__41__32__123__884 = U_LIT(") {");
    static struct u_object l__32__32__885 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__886 = U_LIT("} else {");
    static struct u_object l__32__32__887 = U_LIT("  ");
    static struct u_object l__125__888 = U_LIT("}");
    t872 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__883, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__884))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__885), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__886), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__887), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__888), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t872) && u_is_con(s871, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s871, 0);
    u_value v_b = u_field(s871, 1);
    static struct u_object l_while_32__40__889 = U_LIT("while (");
    static struct u_object l__41__32__123__890 = U_LIT(") {");
    static struct u_object l__32__32__891 = U_LIT("  ");
    static struct u_object l__125__892 = U_LIT("}");
    t872 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__889, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__890))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__891), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__892), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t872) && (u_is_con(s871, &n_c_46_CsReturn) && u_is_con(u_field(s871, 0), &n_option_46_None))) {
    static struct u_object l_return_59__893 = U_LIT("return;");
    t872 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__893), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t872) && (u_is_con(s871, &n_c_46_CsReturn) && u_is_con(u_field(s871, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s871, 0), 0);
    static struct u_object l_return_32__894 = U_LIT("return ");
    static struct u_object l__59__895 = U_LIT(";");
    t872 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__894, u_concat(f_c_46_c_expr_1(v_e), &l__59__895))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t872) && u_is_con(s871, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s871, 0);
    static struct u_object l__123__896 = U_LIT("{");
    static struct u_object l__32__32__897 = U_LIT("  ");
    static struct u_object l__125__898 = U_LIT("}");
    t872 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__896), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__897), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__898), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t872) {
    u_no_case(s871);
  }
  return t872;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t899 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e900 = &t899;
  u_value l901 = v_ss;
  while (u_is_con(l901, &n_list_46_Cons)) {
    u_value v_s = u_field(l901, 0);
    if (u_truth(u_mk_bool(1))) {
      e900 = u_append(e900, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l901 = u_field(l901, 1);
  }
  return f_list_46_concat_1(t899);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s902 = v_ps;
  u_value t903 = NULL;
  if ((!t903) && u_is_con(s902, &n_list_46_Nil)) {
    static struct u_object l_void_904 = U_LIT("void");
    t903 = &l_void_904;
  }
  if (!t903) {
    static struct u_object l__44__32__905 = U_LIT(", ");
    u_value t906 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e907 = &t906;
    u_value l908 = v_ps;
    while (u_is_con(l908, &n_list_46_Cons)) {
      u_value v_p = u_field(l908, 0);
      if (u_truth(u_mk_bool(1))) {
        e907 = u_append(e907, f_c_46_c_param_1(v_p));
      }
      l908 = u_field(l908, 1);
    }
    t903 = u_str_join(&l__44__32__905, t906);
  }
  if (!t903) {
    u_no_case(s902);
  }
  return t903;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s909 = v_p;
  u_value t910 = NULL;
  if (!t910) {
    u_value v_t = u_field(s909, 0);
    u_value v_x = u_field(s909, 1);
    t910 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t910) {
    u_no_case(s909);
  }
  return t910;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s911 = v_d;
  u_value t912 = NULL;
  if ((!t912) && u_is_con(s911, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s911, 0);
    static struct u_object l__35_include_32__913 = U_LIT("#include ");
    t912 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__913, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t912) && u_is_con(s911, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s911, 0);
    u_value v_f = u_field(s911, 1);
    u_value v_ps = u_field(s911, 2);
    static struct u_object l__40__914 = U_LIT("(");
    static struct u_object l__41__59__915 = U_LIT(");");
    t912 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__914, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__915))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t912) && u_is_con(s911, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s911, 0);
    u_value v_f = u_field(s911, 1);
    u_value v_ps = u_field(s911, 2);
    u_value v_b = u_field(s911, 3);
    static struct u_object l__40__916 = U_LIT("(");
    static struct u_object l__41__32__123__917 = U_LIT(") {");
    static struct u_object l__32__32__918 = U_LIT("  ");
    static struct u_object l__125__919 = U_LIT("}");
    t912 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__916, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__917))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__918, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__919, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t912) && (u_is_con(s911, &n_c_46_CdGlobal) && u_is_con(u_field(s911, 2), &n_option_46_None))) {
    u_value v_t = u_field(s911, 0);
    u_value v_x = u_field(s911, 1);
    static struct u_object l__59__920 = U_LIT(";");
    t912 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__920), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t912) && (u_is_con(s911, &n_c_46_CdGlobal) && u_is_con(u_field(s911, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s911, 0);
    u_value v_x = u_field(s911, 1);
    u_value v_e = u_field(u_field(s911, 2), 0);
    static struct u_object l__32__61__32__921 = U_LIT(" = ");
    static struct u_object l__59__922 = U_LIT(";");
    t912 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__921, u_concat(f_c_46_c_expr_1(v_e), &l__59__922))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t912) {
    u_no_case(s911);
  }
  return t912;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s923 = v_f;
  u_value t924 = NULL;
  if ((!t924) && u_is_con(s923, &n_c_46_CFile)) {
    u_value v_ds = u_field(s923, 0);
    static struct u_object l__10__10__925 = U_LIT("\n\n");
    u_value t926 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e927 = &t926;
    u_value l928 = v_ds;
    while (u_is_con(l928, &n_list_46_Cons)) {
      u_value v_d = u_field(l928, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__929 = U_LIT("\n");
        e927 = u_append(e927, u_str_join(&l__10__929, f_c_46_c_decl_1(v_d)));
      }
      l928 = u_field(l928, 1);
    }
    static struct u_object l__10__930 = U_LIT("\n");
    t924 = u_concat(u_str_join(&l__10__10__925, t926), &l__10__930);
  }
  if (!t924) {
    u_no_case(s923);
  }
  return t924;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__931 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__931, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__46__932 = U_LIT("()[],;.");
  return u_str_contains(&l__40__41__91__93__44__59__46__932, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t933 = u_is_letter(v_c);
  if (!u_truth(t933)) {
    t933 = u_is_digit(v_c);
  }
  u_value t934 = t933;
  if (!u_truth(t934)) {
    t934 = u_eq(v_c, u_mk_char(95));
  }
  return t934;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t935;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t935 = u_str_get(v_s, v_i);
  } else {
    t935 = u_chr(u_mk_int(0));
  }
  return t935;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s936 = v_c;
  u_value t937 = NULL;
  if ((!t937) && u_equals(s936, u_mk_char(34))) {
    t937 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t937) && u_equals(s936, u_mk_char(39))) {
    t937 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t937) && u_equals(s936, u_mk_char(92))) {
    t937 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t937) && u_equals(s936, u_mk_char(110))) {
    t937 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t937) && u_equals(s936, u_mk_char(116))) {
    t937 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t937) && u_equals(s936, u_mk_char(114))) {
    t937 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t937) {
    t937 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t937) {
    u_no_case(s936);
  }
  return t937;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t938;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t938 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t938 = v_i;
  }
  return t938;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t939 = u_mk_int(0);
    u_value t940 = u_mk_int(1);
    u_value t941 = u_mk_int(1);
    v_j = t939;
    *r_line = t940;
    *r_column = t941;
    u_value t946 = u_lt(v_j, v_i);
    while (u_truth(t946)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t942 = u_add(*r_line, u_mk_int(1));
            u_value t943 = u_mk_int(1);
            *r_line = t942;
            *r_column = t943;
          }
        } else {
          {
            u_value t944 = u_add(*r_column, u_mk_int(1));
            *r_column = t944;
          }
        }
        u_value t945 = u_add(v_j, u_mk_int(1));
        v_j = t945;
      }
      t946 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t947 = *r_i;
    u_value t948 = u_add(*r_i, u_mk_int(1));
    u_value t949 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t947;
    *r_i = t948;
    v_parts = t949;
    u_value t950 = *r_i;
    v_from = t950;
    u_value t961 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t961)) {
      {
        u_value t951 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t951;
        u_value t952 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t952)) {
          t952 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t952)) {
          {
            static struct u_object l_unterminated_32_lit_953 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_953, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s954 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d955 = 0;
            if ((!d955) && u_is_con(s954, &n_option_46_Some)) {
              u_value v_d = u_field(s954, 0);
              d955 = 1;
              {
                u_value t956 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t957 = u_add(*r_i, u_mk_int(2));
                v_parts = t956;
                *r_i = t957;
                u_value t958 = *r_i;
                v_from = t958;
              }
            }
            if ((!d955) && u_is_con(s954, &n_option_46_None)) {
              d955 = 1;
              {
                static struct u_object l_invalid_32_escape_959 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_959, *r_i), "lex.LexError");
              }
            }
            if (!d955) {
              u_no_case(s954);
            }
          }
        } else {
          {
            u_value t960 = u_add(*r_i, u_mk_int(1));
            *r_i = t960;
          }
        }
      }
      t961 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__962 = U_LIT("");
    u_value t963 = u_str_join(&l__962, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t964 = u_add(*r_i, u_mk_int(1));
    *r_text = t963;
    *r_i = t964;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t965 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t965;
    u_value t966 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t967 = v_j;
    *r_t = t966;
    *r_i = t967;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t968 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t968;
    u_value s969 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d970 = 0;
    if ((!d970) && u_is_con(s969, &n_option_46_Some)) {
      u_value v_n = u_field(s969, 0);
      d970 = 1;
      {
        u_value t971 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t972 = v_j;
        *r_t = t971;
        *r_i = t972;
      }
    }
    if ((!d970) && u_is_con(s969, &n_option_46_None)) {
      d970 = 1;
      {
        static struct u_object l_invalid_32_number_973 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_973, *r_i), "lex.LexError");
      }
    }
    if (!d970) {
      u_no_case(s969);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t974 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t974;
    u_value t975 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t976 = v_j;
    *r_t = t975;
    *r_i = t976;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t977 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t977;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t978 = *r_i;
    v_start = t978;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_979 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_979, v_start), "lex.LexError");
      }
    }
    u_value t980 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t980;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t981 = u_str_get(v_s, *r_i);
    v_c = t981;
    u_value s982 = v_c;
    int d983 = 0;
    if (!d983) {
      u_value t984 = u_is_letter(v_c);
      if (!u_truth(t984)) {
        t984 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t984)) {
        d983 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d983) {
      if (u_truth(u_is_digit(v_c))) {
        d983 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d983) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d983 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d983) && u_equals(s982, u_mk_char(34))) {
      d983 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d983) && u_equals(s982, u_mk_char(39))) {
      d983 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d983) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d983 = 1;
        {
          u_value t985 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t986 = u_add(*r_i, u_mk_int(1));
          *r_t = t985;
          *r_i = t986;
        }
      }
    }
    if (!d983) {
      d983 = 1;
      {
        static struct u_object l_unexpected_32_chara_987 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_987, *r_i), "lex.LexError");
      }
    }
    if (!d983) {
      u_no_case(s982);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t988 = u_mk_int(0);
    v_i = t988;
    u_value t989 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t989;
    u_value t996 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t996)) {
      {
        u_value t990 = v_i;
        v_start = t990;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t991 = u_add(v_i, u_mk_int(1));
            v_i = t991;
          }
        } else {
          u_value t992 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t992)) {
            t992 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t992)) {
            {
              u_value t994 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam993, 0));
              v_i = t994;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t995 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t995;
            }
          }
        }
      }
      t996 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t997 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t997;
  }
}

u_value f_lex_46_tokens_1(u_value v_ts) {
  return f_list_46_firsts_1(v_ts);
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s998 = v_x;
  u_value t999 = NULL;
  if ((!t999) && u_is_con(s998, &n_parse_46_Scope)) {
    u_value v_y = u_field(s998, 0);
    t999 = v_y;
  }
  if (!t999) {
    u_no_case(s998);
  }
  return t999;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1000 = v_x;
  u_value t1001 = NULL;
  if ((!t1001) && u_is_con(s1000, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1000, 1);
    t1001 = v_y;
  }
  if (!t1001) {
    u_no_case(s1000);
  }
  return t1001;
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
  static struct u_object l___1002 = U_LIT("_");
  static struct u_object l_assert_1003 = U_LIT("assert");
  static struct u_object l_assign_1004 = U_LIT("assign");
  static struct u_object l_call_1005 = U_LIT("call");
  static struct u_object l_case_1006 = U_LIT("case");
  static struct u_object l_catch_1007 = U_LIT("catch");
  static struct u_object l_data_1008 = U_LIT("data");
  static struct u_object l_else_1009 = U_LIT("else");
  static struct u_object l_end_1010 = U_LIT("end");
  static struct u_object l_eval_1011 = U_LIT("eval");
  static struct u_object l_false_1012 = U_LIT("false");
  static struct u_object l_for_1013 = U_LIT("for");
  static struct u_object l_function_1014 = U_LIT("function");
  static struct u_object l_if_1015 = U_LIT("if");
  static struct u_object l_import_1016 = U_LIT("import");
  static struct u_object l_in_1017 = U_LIT("in");
  static struct u_object l_infix_1018 = U_LIT("infix");
  static struct u_object l_lambda_1019 = U_LIT("lambda");
  static struct u_object l_let_1020 = U_LIT("let");
  static struct u_object l_local_1021 = U_LIT("local");
  static struct u_object l_match_1022 = U_LIT("match");
  static struct u_object l_out_1023 = U_LIT("out");
  static struct u_object l_postfix_1024 = U_LIT("postfix");
  static struct u_object l_prefix_1025 = U_LIT("prefix");
  static struct u_object l_procedure_1026 = U_LIT("procedure");
  static struct u_object l_raise_1027 = U_LIT("raise");
  static struct u_object l_ref_1028 = U_LIT("ref");
  static struct u_object l_run_1029 = U_LIT("run");
  static struct u_object l_skip_1030 = U_LIT("skip");
  static struct u_object l_test_1031 = U_LIT("test");
  static struct u_object l_then_1032 = U_LIT("then");
  static struct u_object l_true_1033 = U_LIT("true");
  static struct u_object l_type_1034 = U_LIT("type");
  static struct u_object l_try_1035 = U_LIT("try");
  static struct u_object l_while_1036 = U_LIT("while");
  cache = u_mk_con(&n_list_46_Cons, 2, &l___1002, u_mk_con(&n_list_46_Cons, 2, &l_assert_1003, u_mk_con(&n_list_46_Cons, 2, &l_assign_1004, u_mk_con(&n_list_46_Cons, 2, &l_call_1005, u_mk_con(&n_list_46_Cons, 2, &l_case_1006, u_mk_con(&n_list_46_Cons, 2, &l_catch_1007, u_mk_con(&n_list_46_Cons, 2, &l_data_1008, u_mk_con(&n_list_46_Cons, 2, &l_else_1009, u_mk_con(&n_list_46_Cons, 2, &l_end_1010, u_mk_con(&n_list_46_Cons, 2, &l_eval_1011, u_mk_con(&n_list_46_Cons, 2, &l_false_1012, u_mk_con(&n_list_46_Cons, 2, &l_for_1013, u_mk_con(&n_list_46_Cons, 2, &l_function_1014, u_mk_con(&n_list_46_Cons, 2, &l_if_1015, u_mk_con(&n_list_46_Cons, 2, &l_import_1016, u_mk_con(&n_list_46_Cons, 2, &l_in_1017, u_mk_con(&n_list_46_Cons, 2, &l_infix_1018, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1019, u_mk_con(&n_list_46_Cons, 2, &l_let_1020, u_mk_con(&n_list_46_Cons, 2, &l_local_1021, u_mk_con(&n_list_46_Cons, 2, &l_match_1022, u_mk_con(&n_list_46_Cons, 2, &l_out_1023, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1024, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1025, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1026, u_mk_con(&n_list_46_Cons, 2, &l_raise_1027, u_mk_con(&n_list_46_Cons, 2, &l_ref_1028, u_mk_con(&n_list_46_Cons, 2, &l_run_1029, u_mk_con(&n_list_46_Cons, 2, &l_skip_1030, u_mk_con(&n_list_46_Cons, 2, &l_test_1031, u_mk_con(&n_list_46_Cons, 2, &l_then_1032, u_mk_con(&n_list_46_Cons, 2, &l_true_1033, u_mk_con(&n_list_46_Cons, 2, &l_type_1034, u_mk_con(&n_list_46_Cons, 2, &l_try_1035, u_mk_con(&n_list_46_Cons, 2, &l_while_1036, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1037 = U_LIT("=");
  static struct u_object l__61__62__1038 = U_LIT("=>");
  static struct u_object l__58__61__1039 = U_LIT(":=");
  static struct u_object l__58__1040 = U_LIT(":");
  static struct u_object l__124__1041 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1037, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1038, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1039, u_mk_con(&n_list_46_Cons, 2, &l__58__1040, u_mk_con(&n_list_46_Cons, 2, &l__124__1041, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1042 = v_ns;
  u_value t1043 = NULL;
  if ((!t1043) && u_is_con(s1042, &n_list_46_Nil)) {
    t1043 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1043) && (u_is_con(s1042, &n_list_46_Cons) && (u_is_con(u_field(s1042, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1042, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1042, 0), 1);
    u_value v_ops = u_field(u_field(s1042, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1043 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1043) && u_is_con(s1042, &n_list_46_Cons)) {
    u_value v_more = u_field(s1042, 1);
    t1043 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1043) {
    u_no_case(s1042);
  }
  return t1043;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1044 = v_ns;
  u_value t1045 = NULL;
  if ((!t1045) && u_is_con(s1044, &n_list_46_Nil)) {
    t1045 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1045) && (u_is_con(s1044, &n_list_46_Cons) && (u_is_con(u_field(s1044, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1044, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1044, 0), 1);
    u_value v_ops = u_field(u_field(s1044, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1045 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1045) && u_is_con(s1044, &n_list_46_Cons)) {
    u_value v_more = u_field(s1044, 1);
    t1045 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1045) {
    u_no_case(s1044);
  }
  return t1045;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1046 = v_ns;
  u_value t1047 = NULL;
  if ((!t1047) && u_is_con(s1046, &n_list_46_Nil)) {
    t1047 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1047) && (u_is_con(s1046, &n_list_46_Cons) && u_is_con(u_field(s1046, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1046, 0), 0);
    u_value v_p = u_field(u_field(s1046, 0), 1);
    u_value v_ops = u_field(u_field(s1046, 0), 2);
    u_value t1048 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1048)) {
      t1048 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1049 = t1048;
    if (u_truth(t1049)) {
      t1049 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1049)) {
      t1047 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1047) && u_is_con(s1046, &n_list_46_Cons)) {
    u_value v_more = u_field(s1046, 1);
    t1047 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1047) {
    u_no_case(s1046);
  }
  return t1047;
}

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op) {
  u_value s1050 = v_ns;
  u_value t1051 = NULL;
  if ((!t1051) && u_is_con(s1050, &n_list_46_Nil)) {
    t1051 = u_mk_bool(0);
  }
  if ((!t1051) && (u_is_con(s1050, &n_list_46_Cons) && u_is_con(u_field(s1050, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1050, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1051 = u_mk_bool(1);
    }
  }
  if ((!t1051) && u_is_con(s1050, &n_list_46_Cons)) {
    u_value v_more = u_field(s1050, 1);
    t1051 = f_parse_46_declared_2(v_more, v_op);
  }
  if (!t1051) {
    u_no_case(s1050);
  }
  return t1051;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1052 = v_ts;
  u_value t1053 = NULL;
  if ((!t1053) && u_is_con(s1052, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1052, 0), 0);
    t1053 = v_t;
  }
  if (!t1053) {
    t1053 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1053) {
    u_no_case(s1052);
  }
  return t1053;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1054 = v_ts;
  u_value t1055 = NULL;
  if ((!t1055) && (u_is_con(s1054, &n_list_46_Cons) && u_is_con(u_field(s1054, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1054, 1), 0), 0);
    t1055 = v_t;
  }
  if (!t1055) {
    t1055 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1055) {
    u_no_case(s1054);
  }
  return t1055;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1056 = v_ts;
  u_value t1057 = NULL;
  if ((!t1057) && u_is_con(s1056, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1056, 0), 1);
    t1057 = v_i;
  }
  if (!t1057) {
    t1057 = u_mk_int(0);
  }
  if (!t1057) {
    u_no_case(s1056);
  }
  return t1057;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1058 = v_t;
  u_value t1059 = NULL;
  if ((!t1059) && u_is_con(s1058, &n_lex_46_TName)) {
    u_value v_n = u_field(s1058, 0);
    t1059 = v_n;
  }
  if ((!t1059) && u_is_con(s1058, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1058, 0);
    t1059 = v_s;
  }
  if ((!t1059) && u_is_con(s1058, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1058, 0);
    t1059 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1059) && u_is_con(s1058, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1058, 0);
    t1059 = u_str_show(v_n);
  }
  if ((!t1059) && u_is_con(s1058, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1058, 0);
    t1059 = u_str_quote(v_s);
  }
  if ((!t1059) && u_is_con(s1058, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1058, 0);
    static struct u_object l__39__1060 = U_LIT("'");
    static struct u_object l__39__1061 = U_LIT("'");
    t1059 = u_concat(&l__39__1060, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1061));
  }
  if ((!t1059) && u_is_con(s1058, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1062 = U_LIT("end of input");
    t1059 = &l_end_32_of_32_input_1062;
  }
  if (!t1059) {
    u_no_case(s1058);
  }
  return t1059;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1063 = f_parse_46_head_token_1(v_ts);
  u_value t1064 = NULL;
  if ((!t1064) && u_is_con(s1063, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1063, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1064 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1064) && u_is_con(s1063, &n_lex_46_TName)) {
    u_value v_n = u_field(s1063, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1064 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1064) {
    t1064 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1064) {
    u_no_case(s1063);
  }
  return t1064;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1065 = *r_ts;
    int d1066 = 0;
    if ((!d1066) && u_is_con(s1065, &n_list_46_Cons)) {
      u_value v_more = u_field(s1065, 1);
      d1066 = 1;
      {
        u_value t1067 = v_more;
        *r_ts = t1067;
      }
    }
    if (!d1066) {
      d1066 = 1;
      {
      }
    }
    if (!d1066) {
      u_no_case(s1065);
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
        static struct u_object l__32_expected_32_but_32__1068 = U_LIT(" expected but ");
        static struct u_object l__32_found_1069 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1068, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1069))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1070 = f_parse_46_head_token_1(*r_ts);
    int d1071 = 0;
    if ((!d1071) && u_is_con(s1070, &n_lex_46_TName)) {
      u_value v_m = u_field(s1070, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1071 = 1;
        {
          u_value t1072 = v_m;
          *r_n = t1072;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1071) {
      d1071 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1073 = U_LIT("name expected but ");
        static struct u_object l__32_found_1074 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1073, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1074)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1071) {
      u_no_case(s1070);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1075 = f_parse_46_head_token_1(*r_ts);
    int d1076 = 0;
    if ((!d1076) && u_is_con(s1075, &n_lex_46_TName)) {
      u_value v_m = u_field(s1075, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1076 = 1;
        {
          static struct u_object l_operator_32__1077 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1078 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1077, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1078)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1076) {
      d1076 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1076) {
      u_no_case(s1075);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1079 = f_parse_46_head_token_1(*r_ts);
    int d1080 = 0;
    if ((!d1080) && u_is_con(s1079, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1079, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1080 = 1;
        {
          u_value t1081 = v_s;
          *r_n = t1081;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1080) {
      d1080 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1080) {
      u_no_case(s1079);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1082 = v_x;
  u_value t1083 = NULL;
  if ((!t1083) && u_is_con(s1082, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1082, 0);
    t1083 = v_y;
  }
  if (!t1083) {
    u_no_case(s1082);
  }
  return t1083;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1084 = v_x;
  u_value t1085 = NULL;
  if ((!t1085) && u_is_con(s1084, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1084, 1);
    t1085 = v_y;
  }
  if (!t1085) {
    u_no_case(s1084);
  }
  return t1085;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1086 = v_x;
  u_value t1087 = NULL;
  if ((!t1087) && u_is_con(s1086, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1086, 2);
    t1087 = v_y;
  }
  if (!t1087) {
    u_no_case(s1086);
  }
  return t1087;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1088 = v_x;
  u_value t1089 = NULL;
  if ((!t1089) && u_is_con(s1088, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1088, 3);
    t1089 = v_y;
  }
  if (!t1089) {
    u_no_case(s1088);
  }
  return t1089;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1090 = v_rest;
    int d1091 = 0;
    if ((!d1091) && u_is_con(s1090, &n_list_46_Nil)) {
      d1091 = 1;
      {
      }
    }
    if ((!d1091) && (u_is_con(s1090, &n_list_46_Cons) && u_is_con(u_field(s1090, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1090, 0), 0);
      u_value v_i = u_field(u_field(s1090, 0), 1);
      d1091 = 1;
      {
        static struct u_object l_unexpected_32_opera_1092 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1092, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1091) {
      d1091 = 1;
      {
        static struct u_object l_missing_32_operator_1093 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1093, v_at), "parse.ParseError");
      }
    }
    if (!d1091) {
      u_no_case(s1090);
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
    u_value s1094 = v_items;
    int d1095 = 0;
    if ((!d1095) && (u_is_con(s1094, &n_list_46_Cons) && u_is_con(u_field(s1094, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1094, 0), 0);
      u_value v_more = u_field(s1094, 1);
      d1095 = 1;
      {
        u_value t1096 = v_x;
        u_value t1097 = v_more;
        *r_a = t1096;
        *r_rest = t1097;
      }
    }
    if ((!d1095) && (u_is_con(s1094, &n_list_46_Cons) && u_is_con(u_field(s1094, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1094, 0), 0);
      u_value v_i = u_field(u_field(s1094, 0), 1);
      u_value v_more = u_field(s1094, 1);
      d1095 = 1;
      {
        u_value s1098 = f_parse_46_prefix_prec_2(f_parse_46_Ops_46_notations_1(v_o), v_op);
        int d1099 = 0;
        if ((!d1099) && u_is_con(s1098, &n_option_46_Some)) {
          u_value v_p = u_field(s1098, 0);
          d1099 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1100 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1100;
          }
        }
        if ((!d1099) && u_is_con(s1098, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1099 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1101 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1101), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1099) && u_is_con(s1098, &n_option_46_None)) {
          d1099 = 1;
          {
            static struct u_object l_undeclared_32_opera_1102 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1102, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1099) {
          u_no_case(s1098);
        }
      }
    }
    if ((!d1095) && u_is_con(s1094, &n_list_46_Nil)) {
      d1095 = 1;
      {
        static struct u_object l_missing_32_operand_1103 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1103, v_at), "parse.ParseError");
      }
    }
    if (!d1095) {
      u_no_case(s1094);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1104 = v_items;
    int d1105 = 0;
    if ((!d1105) && (u_is_con(s1104, &n_list_46_Cons) && u_is_con(u_field(s1104, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1104, 0), 0);
      u_value v_i = u_field(u_field(s1104, 0), 1);
      u_value v_tail = u_field(s1104, 1);
      d1105 = 1;
      {
        u_value s1106 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Ops_46_notations_1(v_o), v_op), f_parse_46_infix_of_2(f_parse_46_Ops_46_notations_1(v_o), v_op));
        int d1107 = 0;
        if ((!d1107) && u_is_con(u_field(s1106, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1106, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1107 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1107) && u_is_con(u_field(s1106, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1106, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1106, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1107 = 1;
            {
              u_value t1108;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1108 = v_p;
              } else {
                t1108 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1108, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Ops_46_notations_1(v_o));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1107) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1107 = 1;
            {
              static struct u_object l_undeclared_32_opera_1109 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1109, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1107) {
          d1107 = 1;
          {
            u_value t1110 = v_lhs;
            u_value t1111 = v_items;
            *r_a = t1110;
            *r_rest = t1111;
          }
        }
        if (!d1107) {
          u_no_case(s1106);
        }
      }
    }
    if (!d1105) {
      d1105 = 1;
      {
        u_value t1112 = v_lhs;
        u_value t1113 = v_items;
        *r_a = t1112;
        *r_rest = t1113;
      }
    }
    if (!d1105) {
      u_no_case(s1104);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1114 = v_rest;
    int d1115 = 0;
    if ((!d1115) && (u_is_con(s1114, &n_list_46_Cons) && u_is_con(u_field(s1114, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1114, 0), 0);
      u_value v_i = u_field(u_field(s1114, 0), 1);
      u_value t1118 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1118)) {
        t1118 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1118)) {
        d1115 = 1;
        {
          static struct u_object l_operator_32__1116 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1117 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1116, u_concat(v_op, &l__32_is_32_not_32_associat_1117)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1115) {
      d1115 = 1;
      {
      }
    }
    if (!d1115) {
      u_no_case(s1114);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1119 = v_fp;
  u_value t1120 = NULL;
  if ((!t1120) && u_is_con(s1119, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1119, 0), 1);
    t1120 = u_eq(v_q, v_p);
  }
  if (!t1120) {
    t1120 = u_mk_bool(0);
  }
  if (!t1120) {
    u_no_case(s1119);
  }
  return t1120;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1121 = v_ts;
  u_value t1122 = NULL;
  if ((!t1122) && (u_is_con(s1121, &n_list_46_Cons) && (u_is_con(u_field(s1121, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1121, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1121, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1121, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1121, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1121, 0), 1);
    t1122 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1122) && (u_is_con(s1121, &n_list_46_Cons) && u_is_con(u_field(s1121, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1121, 0);
    t1122 = v_t;
  }
  if ((!t1122) && u_is_con(s1121, &n_list_46_Nil)) {
    t1122 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1122) {
    static struct u_object l__40_invalid_41__1123 = U_LIT("(invalid)");
    t1122 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1123), v_ts);
  }
  if (!t1122) {
    u_no_case(s1121);
  }
  return t1122;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1124 = v_ts;
  u_value t1125 = NULL;
  if ((!t1125) && (u_is_con(s1124, &n_list_46_Cons) && (u_is_con(u_field(s1124, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1124, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1124, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1124, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1124, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1124, 0), 1);
    t1125 = v_cs;
  }
  if (!t1125) {
    t1125 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1125) {
    u_no_case(s1124);
  }
  return t1125;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1126 = U_LIT("->");
  u_value t1133;
  if (u_truth(u_eq(v_op, &l__45__62__1126))) {
    u_value t1127 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1128 = &t1127;
    u_value l1129 = v_l;
    while (u_is_con(l1129, &n_list_46_Cons)) {
      u_value v_t = u_field(l1129, 0);
      if (u_truth(u_mk_bool(1))) {
        e1128 = u_append(e1128, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1129 = u_field(l1129, 1);
    }
    t1133 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1127, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1130 = U_LIT("*");
    u_value t1132;
    if (u_truth(u_eq(v_op, &l__42__1130))) {
      static struct u_object l__40_product_41__1131 = U_LIT("(product)");
      t1132 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1131), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1132 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1133 = t1132;
  }
  return t1133;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1134 = v_t;
  u_value t1135 = NULL;
  if ((!t1135) && (u_is_con(s1134, &n_syntax_46_TSort) && (u_is_con(u_field(s1134, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1134, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1134, 0), 1), u_mk_lit("(invalid)")))))) {
    t1135 = u_mk_bool(1);
  }
  if ((!t1135) && u_is_con(s1134, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1134, 1);
    t1135 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1135) && u_is_con(s1134, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1134, 0);
    u_value v_r = u_field(s1134, 1);
    u_value t1136 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1136)) {
      t1136 = f_parse_46_has_invalid_1(v_r);
    }
    t1135 = t1136;
  }
  if ((!t1135) && u_is_con(s1134, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1134, 0);
    t1135 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1135) {
    t1135 = u_mk_bool(0);
  }
  if (!t1135) {
    u_no_case(s1134);
  }
  return t1135;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1137 = v_ts;
  u_value t1138 = NULL;
  if ((!t1138) && u_is_con(s1137, &n_list_46_Nil)) {
    t1138 = u_mk_bool(0);
  }
  if ((!t1138) && u_is_con(s1137, &n_list_46_Cons)) {
    u_value v_t = u_field(s1137, 0);
    u_value v_more = u_field(s1137, 1);
    u_value t1139 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1139)) {
      t1139 = f_parse_46_any_invalid_1(v_more);
    }
    t1138 = t1139;
  }
  if (!t1138) {
    u_no_case(s1137);
  }
  return t1138;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1140 = v_t;
  u_value t1141 = NULL;
  if ((!t1141) && u_is_con(s1140, &n_lex_46_TName)) {
    u_value v_n = u_field(s1140, 0);
    u_value t1142 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1142)) {
      t1142 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1141 = t1142;
  }
  if ((!t1141) && (u_is_con(s1140, &n_lex_46_TPunct) && u_equals(u_field(s1140, 0), u_mk_char(40)))) {
    t1141 = u_mk_bool(1);
  }
  if (!t1141) {
    t1141 = u_mk_bool(0);
  }
  if (!t1141) {
    u_no_case(s1140);
  }
  return t1141;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1143 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1143;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1144 = f_parse_46_type_one_1(v_r);
    *r_t = t1144;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1145 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1145, v_start), "parse.ParseError");
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
    u_value t1146 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1146;
    u_value t1147 = u_mk_bool(1);
    v_next = t1147;
    u_value t1148 = u_mk_bool(1);
    v_go = t1148;
    u_value t1157 = v_go;
    while (u_truth(t1157)) {
      {
        u_value s1149 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1150 = 0;
        if ((!d1150) && u_is_con(s1149, &n_option_46_Some)) {
          u_value v_op = u_field(s1149, 0);
          d1150 = 1;
          {
            u_value t1151 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1152 = u_mk_bool(1);
            v_rev = t1151;
            v_next = t1152;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1150) && u_is_con(s1149, &n_option_46_None)) {
          u_value t1155 = v_next;
          if (u_truth(t1155)) {
            t1155 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1155)) {
            d1150 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1153 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1154 = u_mk_bool(0);
              v_rev = t1153;
              v_next = t1154;
            }
          }
        }
        if (!d1150) {
          d1150 = 1;
          {
            u_value t1156 = u_mk_bool(0);
            v_go = t1156;
          }
        }
        if (!d1150) {
          u_no_case(s1149);
        }
      }
      t1157 = v_go;
    }
    u_value s1158 = v_rev;
    int d1159 = 0;
    if ((!d1159) && u_is_con(s1158, &n_list_46_Nil)) {
      d1159 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1160 = U_LIT("type expected but ");
        static struct u_object l__32_found_1161 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1160, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1161)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1159) {
      d1159 = 1;
      {
        u_value t1162 = f_list_46_reverse_1(v_rev);
        *r_items = t1162;
      }
    }
    if (!d1159) {
      u_no_case(s1158);
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
            u_value t1163 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1163;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1164 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1164;
            }
          } else {
            {
              u_value t1165 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1165;
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
    u_value t1166 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1166;
    u_value t1167 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1167)) {
      t1167 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1167)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1168 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1168;
        u_value t1170 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1170)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1169 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1169;
          }
          t1170 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1171 = f_list_46_reverse_1(v_rev);
    *r_r = t1171;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1172 = v_es;
  u_value t1173 = NULL;
  if ((!t1173) && u_is_con(s1172, &n_list_46_Nil)) {
    t1173 = v_rest;
  }
  if ((!t1173) && u_is_con(s1172, &n_list_46_Cons)) {
    u_value v_e = u_field(s1172, 0);
    u_value v_more = u_field(s1172, 1);
    static struct u_object l_Cons_1174 = U_LIT("Cons");
    t1173 = f_parse_46_expr_op2_3(&l_Cons_1174, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1173) {
    u_no_case(s1172);
  }
  return t1173;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1175 = v_t;
  u_value t1176 = NULL;
  if ((!t1176) && u_is_con(s1175, &n_lex_46_TName)) {
    u_value v_n = u_field(s1175, 0);
    static struct u_object l_let_1177 = U_LIT("let");
    static struct u_object l_if_1178 = U_LIT("if");
    static struct u_object l_lambda_1179 = U_LIT("lambda");
    static struct u_object l_match_1180 = U_LIT("match");
    static struct u_object l_true_1181 = U_LIT("true");
    static struct u_object l_false_1182 = U_LIT("false");
    u_value t1184 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1177, u_mk_con(&n_list_46_Cons, 2, &l_if_1178, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1179, u_mk_con(&n_list_46_Cons, 2, &l_match_1180, u_mk_con(&n_list_46_Cons, 2, &l_true_1181, u_mk_con(&n_list_46_Cons, 2, &l_false_1182, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1184)) {
      u_value t1183 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1183)) {
        t1183 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1184 = t1183;
    }
    t1176 = t1184;
  }
  if ((!t1176) && u_is_con(s1175, &n_lex_46_TNum)) {
    t1176 = u_mk_bool(1);
  }
  if ((!t1176) && u_is_con(s1175, &n_lex_46_TStr)) {
    t1176 = u_mk_bool(1);
  }
  if ((!t1176) && u_is_con(s1175, &n_lex_46_TChar)) {
    t1176 = u_mk_bool(1);
  }
  if ((!t1176) && u_is_con(s1175, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1175, 0);
    u_value t1185 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1185)) {
      t1185 = u_eq(v_c, u_mk_char(91));
    }
    t1176 = t1185;
  }
  if (!t1176) {
    t1176 = u_mk_bool(0);
  }
  if (!t1176) {
    u_no_case(s1175);
  }
  return t1176;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
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
    u_value t1186 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1186;
    u_value t1187 = u_mk_bool(1);
    v_next = t1187;
    u_value t1188 = u_mk_bool(1);
    v_go = t1188;
    u_value t1197 = v_go;
    while (u_truth(t1197)) {
      {
        u_value s1189 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1190 = 0;
        if ((!d1190) && u_is_con(s1189, &n_option_46_Some)) {
          u_value v_op = u_field(s1189, 0);
          d1190 = 1;
          {
            u_value t1191 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1192 = u_mk_bool(1);
            v_rev = t1191;
            v_next = t1192;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1190) && u_is_con(s1189, &n_option_46_None)) {
          u_value t1195 = v_next;
          if (u_truth(t1195)) {
            t1195 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1195)) {
            d1190 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1193 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1194 = u_mk_bool(0);
              v_rev = t1193;
              v_next = t1194;
            }
          }
        }
        if (!d1190) {
          d1190 = 1;
          {
            u_value t1196 = u_mk_bool(0);
            v_go = t1196;
          }
        }
        if (!d1190) {
          u_no_case(s1189);
        }
      }
      t1197 = v_go;
    }
    u_value s1198 = v_rev;
    int d1199 = 0;
    if ((!d1199) && u_is_con(s1198, &n_list_46_Nil)) {
      d1199 = 1;
      {
        static struct u_object l_expression_32_expec_1200 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1201 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1200, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1201)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1199) {
      d1199 = 1;
      {
        u_value t1202 = f_list_46_reverse_1(v_rev);
        *r_items = t1202;
      }
    }
    if (!d1199) {
      u_no_case(s1198);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1203 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1203;
    u_value t1205 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1205)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1204 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1204;
      }
      t1205 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1206 = f_list_46_reverse_1(v_rev);
    *r_es = t1206;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1207 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1207;
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
    u_value t1208 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1208)) {
      t1208 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1212 = t1208;
    while (u_truth(t1212)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1209 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1209;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1210 = U_LIT(".");
            u_value t1211 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1210, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1211;
          }
        }
      }
      u_value t1213 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1213)) {
        t1213 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1212 = t1213;
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
    u_value s1214 = f_parse_46_head_token_1(*r_ts);
    int d1215 = 0;
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TName) && u_equals(u_field(s1214, 0), u_mk_lit("let")))) {
      d1215 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TName) && u_equals(u_field(s1214, 0), u_mk_lit("if")))) {
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1216 = U_LIT("then");
        p_parse_46_keyword(&l_then_1216, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1217 = U_LIT("else");
        p_parse_46_keyword(&l_else_1217, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1218 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1218;
      }
    }
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TName) && u_equals(u_field(s1214, 0), u_mk_lit("lambda")))) {
      d1215 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TName) && u_equals(u_field(s1214, 0), u_mk_lit("match")))) {
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1219 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1219;
      }
    }
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TName) && u_equals(u_field(s1214, 0), u_mk_lit("true")))) {
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1220 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1220;
      }
    }
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TName) && u_equals(u_field(s1214, 0), u_mk_lit("false")))) {
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1221 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1221;
      }
    }
    if ((!d1215) && u_is_con(s1214, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1214, 0);
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1222 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1222;
      }
    }
    if ((!d1215) && u_is_con(s1214, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1214, 0);
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1223 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1223;
      }
    }
    if ((!d1215) && u_is_con(s1214, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1214, 0);
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1224 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1224;
      }
    }
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TPunct) && u_equals(u_field(s1214, 0), u_mk_char(40)))) {
      d1215 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1225 = v_es;
        int d1226 = 0;
        if ((!d1226) && (u_is_con(s1225, &n_list_46_Cons) && u_is_con(u_field(s1225, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1225, 0);
          d1226 = 1;
          {
            u_value t1227 = v_x;
            *r_e = t1227;
          }
        }
        if (!d1226) {
          d1226 = 1;
          {
            u_value t1228 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1228;
          }
        }
        if (!d1226) {
          u_no_case(s1225);
        }
      }
    }
    if ((!d1215) && (u_is_con(s1214, &n_lex_46_TPunct) && u_equals(u_field(s1214, 0), u_mk_char(91)))) {
      d1215 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1215) && u_is_con(s1214, &n_lex_46_TName)) {
      u_value v_n = u_field(s1214, 0);
      u_value t1230 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1230)) {
        t1230 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1230)) {
        d1215 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1229 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1229;
        }
      }
    }
    if (!d1215) {
      d1215 = 1;
      {
        static struct u_object l_expression_32_expec_1231 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1232 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1231, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1232)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1215) {
      u_no_case(s1214);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1233 = v_ps;
  u_value t1234 = NULL;
  if ((!t1234) && u_is_con(s1233, &n_list_46_Nil)) {
    t1234 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1234) && (u_is_con(s1233, &n_list_46_Cons) && u_is_con(u_field(s1233, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1233, 0), 0);
    u_value v_more = u_field(s1233, 1);
    u_value s1235 = f_parse_46_pattern_vars_1(v_more);
    u_value t1236 = NULL;
    if ((!t1236) && u_is_con(s1235, &n_option_46_Some)) {
      u_value v_xs = u_field(s1235, 0);
      t1236 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1236) && u_is_con(s1235, &n_option_46_None)) {
      t1236 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1236) {
      u_no_case(s1235);
    }
    t1234 = t1236;
  }
  if (!t1234) {
    t1234 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1234) {
    u_no_case(s1233);
  }
  return t1234;
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
    static struct u_object l__58__61__1237 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1237), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1238 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1239 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1238;
    v_es = t1239;
    u_value t1243 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1243)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1240 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1240), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1241 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1242 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1241;
        v_es = t1242;
      }
      t1243 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1244 = U_LIT("in");
    p_parse_46_keyword(&l_in_1244, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1245 = f_list_46_reverse_1(v_ps);
    u_value t1246 = f_list_46_reverse_1(v_es);
    v_ps = t1245;
    v_es = t1246;
    u_value s1247 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1248 = 0;
    if ((!d1248) && u_is_con(u_field(s1247, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1247, 0), 0);
      d1248 = 1;
      {
        u_value t1249 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1249;
      }
    }
    if ((!d1248) && (u_is_con(u_field(s1247, 0), &n_option_46_None) && (u_is_con(u_field(s1247, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1247, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1247, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1247, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1247, 1), 0);
      u_value v_c = u_field(u_field(s1247, 2), 0);
      d1248 = 1;
      {
        u_value t1250 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1250;
      }
    }
    if (!d1248) {
      d1248 = 1;
      {
        u_value t1251 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1251;
      }
    }
    if (!d1248) {
      u_no_case(s1247);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1252 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1252;
    static struct u_object l__61__62__1253 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1253)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1254 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1254;
        u_value t1256 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1256)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1255 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1255;
          }
          t1256 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1257 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1257), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1258 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1258;
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
        static struct u_object l_Nil_1259 = U_LIT("Nil");
        u_value t1260 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1259), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1260;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1261 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1261))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1262 = U_LIT("in");
            p_parse_46_keyword(&l_in_1262, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1263 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1264 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1263;
            v_lists = t1264;
            u_value t1268 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1268)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1265 = U_LIT("in");
                p_parse_46_keyword(&l_in_1265, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1266 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1267 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1266;
                v_lists = t1267;
              }
              t1268 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1269 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1269;
            static struct u_object l_if_1270 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1270))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1271 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1271;
          }
        } else {
          {
            u_value t1272 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1272;
            u_value t1274 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1274)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1273 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1273;
              }
              t1274 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1275 = U_LIT("Nil");
            u_value t1276 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1275), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1276;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1277 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1277;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1278 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1278))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1279 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1279;
      }
    } else {
      {
        u_value t1280 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1280;
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
    u_value t1281 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1281;
    static struct u_object l_case_1282 = U_LIT("case");
    u_value t1285 = f_parse_46_at_keyword_2(*r_ts, &l_case_1282);
    while (u_truth(t1285)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1283 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1283), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1284 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1284;
      }
      static struct u_object l_case_1286 = U_LIT("case");
      t1285 = f_parse_46_at_keyword_2(*r_ts, &l_case_1286);
    }
    static struct u_object l_else_1287 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1287))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1288 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1288;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1289 = U_LIT("end");
    p_parse_46_keyword(&l_end_1289, r_ts);
    u_value t1290 = f_list_46_reverse_1(v_rev);
    *r_cs = t1290;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1291 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1291))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1292 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1292, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1293 = v_ps;
  u_value t1294 = NULL;
  if ((!t1294) && u_is_con(s1293, &n_list_46_Nil)) {
    t1294 = v_rest;
  }
  if ((!t1294) && u_is_con(s1293, &n_list_46_Cons)) {
    u_value v_p = u_field(s1293, 0);
    u_value v_more = u_field(s1293, 1);
    static struct u_object l_Cons_1295 = U_LIT("Cons");
    t1294 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1295), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1294) {
    u_no_case(s1293);
  }
  return t1294;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1296 = v_t;
  u_value t1297 = NULL;
  if ((!t1297) && u_is_con(s1296, &n_lex_46_TName)) {
    u_value v_n = u_field(s1296, 0);
    static struct u_object l___1298 = U_LIT("_");
    static struct u_object l_true_1299 = U_LIT("true");
    static struct u_object l_false_1300 = U_LIT("false");
    u_value t1302 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1298, u_mk_con(&n_list_46_Cons, 2, &l_true_1299, u_mk_con(&n_list_46_Cons, 2, &l_false_1300, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1302)) {
      u_value t1301 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1301)) {
        t1301 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1302 = t1301;
    }
    t1297 = t1302;
  }
  if ((!t1297) && u_is_con(s1296, &n_lex_46_TNum)) {
    t1297 = u_mk_bool(1);
  }
  if ((!t1297) && u_is_con(s1296, &n_lex_46_TStr)) {
    t1297 = u_mk_bool(1);
  }
  if ((!t1297) && u_is_con(s1296, &n_lex_46_TChar)) {
    t1297 = u_mk_bool(1);
  }
  if ((!t1297) && u_is_con(s1296, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1296, 0);
    u_value t1303 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1303)) {
      t1303 = u_eq(v_c, u_mk_char(91));
    }
    t1297 = t1303;
  }
  if (!t1297) {
    t1297 = u_mk_bool(0);
  }
  if (!t1297) {
    u_no_case(s1296);
  }
  return t1297;
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
    u_value t1304 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1304;
    u_value t1305 = u_mk_bool(1);
    v_next = t1305;
    u_value t1306 = u_mk_bool(1);
    v_go = t1306;
    u_value t1315 = v_go;
    while (u_truth(t1315)) {
      {
        u_value s1307 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1308 = 0;
        if ((!d1308) && u_is_con(s1307, &n_option_46_Some)) {
          u_value v_op = u_field(s1307, 0);
          d1308 = 1;
          {
            u_value t1309 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1310 = u_mk_bool(1);
            v_rev = t1309;
            v_next = t1310;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1308) && u_is_con(s1307, &n_option_46_None)) {
          u_value t1313 = v_next;
          if (u_truth(t1313)) {
            t1313 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1313)) {
            d1308 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1311 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1312 = u_mk_bool(0);
              v_rev = t1311;
              v_next = t1312;
            }
          }
        }
        if (!d1308) {
          d1308 = 1;
          {
            u_value t1314 = u_mk_bool(0);
            v_go = t1314;
          }
        }
        if (!d1308) {
          u_no_case(s1307);
        }
      }
      t1315 = v_go;
    }
    u_value s1316 = v_rev;
    int d1317 = 0;
    if ((!d1317) && u_is_con(s1316, &n_list_46_Nil)) {
      d1317 = 1;
      {
        static struct u_object l_pattern_32_expected_1318 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1319 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1318, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1319)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1317) {
      d1317 = 1;
      {
        u_value t1320 = f_list_46_reverse_1(v_rev);
        *r_items = t1320;
      }
    }
    if (!d1317) {
      u_no_case(s1316);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1321 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1321;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1322 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1322;
        u_value t1324 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1324)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1323 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1323;
          }
          t1324 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1325 = f_list_46_reverse_1(v_rev);
    *r_ps = t1325;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1326 = f_parse_46_head_token_1(*r_ts);
    int d1327 = 0;
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("_")))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1328 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1328;
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("true")))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1329 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1329;
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TName) && u_equals(u_field(s1326, 0), u_mk_lit("false")))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1330 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1330;
      }
    }
    if ((!d1327) && u_is_con(s1326, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1326, 0);
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1331 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1331;
      }
    }
    if ((!d1327) && u_is_con(s1326, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1326, 0);
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1332 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1332;
      }
    }
    if ((!d1327) && u_is_con(s1326, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1326, 0);
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1333 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1333;
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TPunct) && u_equals(u_field(s1326, 0), u_mk_char(40)))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1334 = v_ps;
        int d1335 = 0;
        if ((!d1335) && (u_is_con(s1334, &n_list_46_Cons) && u_is_con(u_field(s1334, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1334, 0);
          d1335 = 1;
          {
            u_value t1336 = v_q;
            *r_p = t1336;
          }
        }
        if (!d1335) {
          d1335 = 1;
          {
            u_value t1337 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1337;
          }
        }
        if (!d1335) {
          u_no_case(s1334);
        }
      }
    }
    if ((!d1327) && (u_is_con(s1326, &n_lex_46_TPunct) && u_equals(u_field(s1326, 0), u_mk_char(91)))) {
      d1327 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1338 = U_LIT("Nil");
            u_value t1339 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1338), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1339;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1340 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1340;
            u_value t1342 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1342)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1341 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1341;
              }
              t1342 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1343 = U_LIT("Nil");
            u_value t1344 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1343), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1344;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1345 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1345;
          }
        }
      }
    }
    if (!d1327) {
      d1327 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1346 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1346;
          }
        } else {
          {
            u_value t1347 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1347;
          }
        }
      }
    }
    if (!d1327) {
      u_no_case(s1326);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1348 = v_t;
  u_value t1349 = NULL;
  if ((!t1349) && u_is_con(s1348, &n_lex_46_TName)) {
    u_value v_n = u_field(s1348, 0);
    static struct u_object l_skip_1350 = U_LIT("skip");
    static struct u_object l_if_1351 = U_LIT("if");
    static struct u_object l_while_1352 = U_LIT("while");
    static struct u_object l_call_1353 = U_LIT("call");
    static struct u_object l_match_1354 = U_LIT("match");
    static struct u_object l_assert_1355 = U_LIT("assert");
    static struct u_object l_assign_1356 = U_LIT("assign");
    static struct u_object l_raise_1357 = U_LIT("raise");
    static struct u_object l_try_1358 = U_LIT("try");
    t1349 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1350, u_mk_con(&n_list_46_Cons, 2, &l_if_1351, u_mk_con(&n_list_46_Cons, 2, &l_while_1352, u_mk_con(&n_list_46_Cons, 2, &l_call_1353, u_mk_con(&n_list_46_Cons, 2, &l_match_1354, u_mk_con(&n_list_46_Cons, 2, &l_assert_1355, u_mk_con(&n_list_46_Cons, 2, &l_assign_1356, u_mk_con(&n_list_46_Cons, 2, &l_raise_1357, u_mk_con(&n_list_46_Cons, 2, &l_try_1358, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1349) {
    t1349 = u_mk_bool(0);
  }
  if (!t1349) {
    u_no_case(s1348);
  }
  return t1349;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1359 = v_cs;
  u_value t1360 = NULL;
  if ((!t1360) && u_is_con(s1359, &n_list_46_Nil)) {
    t1360 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1360) && (u_is_con(s1359, &n_list_46_Cons) && u_is_con(u_field(s1359, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1359, 0), 0);
    u_value v_g = u_field(u_field(s1359, 0), 1);
    u_value v_b = u_field(u_field(s1359, 0), 2);
    u_value v_more = u_field(s1359, 1);
    t1360 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1360) {
    u_no_case(s1359);
  }
  return t1360;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1361 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1361;
    u_value t1362 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1362;
    u_value t1363 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1363;
    static struct u_object l_local_1364 = U_LIT("local");
    u_value t1366 = f_parse_46_at_keyword_2(*r_ts, &l_local_1364);
    while (u_truth(t1366)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1365 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1365)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1365 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1367 = U_LIT("local");
      t1366 = f_parse_46_at_keyword_2(*r_ts, &l_local_1367);
    }
    u_value t1369 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1369)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1368 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1368;
      }
      t1369 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1370 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1370;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1371 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1371;
    static struct u_object l__58__1372 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1372)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1373 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1373)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1374 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1374;
      }
    }
    u_value t1375 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1375;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1376 = f_parse_46_head_token_1(*r_ts);
    int d1377 = 0;
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("skip")))) {
      d1377 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1378 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1378;
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("if")))) {
      d1377 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("while")))) {
      d1377 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1379 = U_LIT("end");
        p_parse_46_keyword(&l_end_1379, r_ts);
        u_value t1380 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1380;
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("call")))) {
      d1377 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("match")))) {
      d1377 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1381 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1381;
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("assert")))) {
      d1377 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1382 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1382;
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("assign")))) {
      d1377 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("raise")))) {
      d1377 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1383 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1383;
      }
    }
    if ((!d1377) && (u_is_con(s1376, &n_lex_46_TName) && u_equals(u_field(s1376, 0), u_mk_lit("try")))) {
      d1377 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1384 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1384, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1385 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1385;
      }
    }
    if (!d1377) {
      d1377 = 1;
      {
        static struct u_object l_statement_32_expect_1386 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1387 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1386, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1387)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1377) {
      u_no_case(s1376);
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
    static struct u_object l_else_1388 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1388)))) {
      {
        static struct u_object l_end_1389 = U_LIT("end");
        p_parse_46_keyword(&l_end_1389, r_ts);
        u_value t1390 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1390;
      }
    } else {
      static struct u_object l_if_1391 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1391)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1392 = U_LIT("end");
          p_parse_46_keyword(&l_end_1392, r_ts);
        }
      }
    }
    u_value t1393 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1393;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1394 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1394), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1395 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1395;
    u_value t1398 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1398)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1396 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1396), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1397 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1397;
      }
      t1398 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1399 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1399;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1400 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1400;
    static struct u_object l_case_1401 = U_LIT("case");
    u_value t1404 = f_parse_46_at_keyword_2(*r_ts, &l_case_1401);
    while (u_truth(t1404)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1402 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1402), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1403 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1403;
      }
      static struct u_object l_case_1405 = U_LIT("case");
      t1404 = f_parse_46_at_keyword_2(*r_ts, &l_case_1405);
    }
    static struct u_object l_else_1406 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1406))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1407 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1407;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1408 = U_LIT("end");
    p_parse_46_keyword(&l_end_1408, r_ts);
    u_value t1409 = f_list_46_reverse_1(v_rev);
    *r_cs = t1409;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1411;
  if (u_truth(v_first)) {
    t1411 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1410 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1410)) {
      t1410 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1411 = t1410;
  }
  return t1411;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1413 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1413)) {
    static struct u_object l_out_1412 = U_LIT("out");
    t1413 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1412));
  }
  u_value t1415 = t1413;
  if (u_truth(t1415)) {
    static struct u_object l_ref_1414 = U_LIT("ref");
    t1415 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1414));
  }
  return t1415;
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
    u_value t1416 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1417 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1418 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1419 = u_mk_bool(1);
    v_ins = t1416;
    v_outs = t1417;
    v_refs = t1418;
    v_first = t1419;
    static struct u_object l_in_1420 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1420))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1421 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1422 = u_mk_bool(0);
        v_ins = t1421;
        v_first = t1422;
        u_value t1424 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1424)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1423 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1423;
          }
          t1424 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1425 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1425, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1426 = u_mk_bool(0);
        v_first = t1426;
      }
    }
    static struct u_object l_ref_1427 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1427, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1428 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1428;
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
    u_value t1429 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1429;
    u_value t1431 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1431)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1430 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1430;
      }
      t1431 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1432 = f_list_46_reverse_1(v_rev);
    *r_xs = t1432;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1433 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1433;
    static struct u_object l__58__1434 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1434)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1435 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1435;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1436 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1436;
    u_value t1438 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1438)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1437 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1437;
      }
      t1438 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1439 = f_list_46_reverse_1(v_rev);
    *r_xs = t1439;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1440 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1440;
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
  u_value s1441 = v_cs;
  u_value t1442 = NULL;
  if ((!t1442) && u_is_con(s1441, &n_list_46_Nil)) {
    t1442 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1442) && (u_is_con(s1441, &n_list_46_Cons) && u_is_con(u_field(s1441, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1441, 1);
    t1442 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1442) && (u_is_con(s1441, &n_list_46_Cons) && u_is_con(u_field(s1441, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1441, 1);
    t1442 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1442) && (u_is_con(s1441, &n_list_46_Cons) && u_is_con(u_field(s1441, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1441, 1);
    t1442 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1442) && u_is_con(s1441, &n_list_46_Cons)) {
    u_value v_c = u_field(s1441, 0);
    u_value v_more = u_field(s1441, 1);
    t1442 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1442) {
    u_no_case(s1441);
  }
  return t1442;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1443 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1444 = &t1443;
  u_value l1445 = v_cs;
  while (u_is_con(l1445, &n_list_46_Cons)) {
    u_value v_c = u_field(l1445, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1444 = u_append(e1444, v_c);
    }
    l1445 = u_field(l1445, 1);
  }
  return t1443;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1446 = v_c;
  u_value t1447 = NULL;
  if ((!t1447) && u_is_con(s1446, &n_syntax_46_CNotation)) {
    t1447 = u_mk_bool(1);
  }
  if (!t1447) {
    t1447 = u_mk_bool(0);
  }
  if (!t1447) {
    u_no_case(s1446);
  }
  return t1447;
}

u_value f_parse_46_Loader_46_modules_1(u_value v_x) {
  u_value s1448 = v_x;
  u_value t1449 = NULL;
  if ((!t1449) && u_is_con(s1448, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1448, 0);
    t1449 = v_y;
  }
  if (!t1449) {
    u_no_case(s1448);
  }
  return t1449;
}

u_value f_parse_46_Loader_46_loading_1(u_value v_x) {
  u_value s1450 = v_x;
  u_value t1451 = NULL;
  if ((!t1451) && u_is_con(s1450, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1450, 1);
    t1451 = v_y;
  }
  if (!t1451) {
    u_no_case(s1450);
  }
  return t1451;
}

u_value f_parse_46_Loader_46_base_1(u_value v_x) {
  u_value s1452 = v_x;
  u_value t1453 = NULL;
  if ((!t1453) && u_is_con(s1452, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1452, 2);
    t1453 = v_y;
  }
  if (!t1453) {
    u_no_case(s1452);
  }
  return t1453;
}

u_value f_parse_46_Loader_46_dir_1(u_value v_x) {
  u_value s1454 = v_x;
  u_value t1455 = NULL;
  if ((!t1455) && u_is_con(s1454, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1454, 3);
    t1455 = v_y;
  }
  if (!t1455) {
    u_no_case(s1454);
  }
  return t1455;
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1456 = v_c;
  u_value t1457 = NULL;
  if ((!t1457) && u_is_con(s1456, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1456, 0);
    u_value v_cs = u_field(s1456, 1);
    u_value t1458 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1459 = &t1458;
    u_value l1460 = v_cs;
    while (u_is_con(l1460, &n_list_46_Cons)) {
      u_value v_d = u_field(l1460, 0);
      if (u_truth(u_mk_bool(1))) {
        e1459 = u_append(e1459, f_parse_46_modules_of_1(v_d));
      }
      l1460 = u_field(l1460, 1);
    }
    t1457 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1458));
  }
  if (!t1457) {
    t1457 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1457) {
    u_no_case(s1456);
  }
  return t1457;
}

u_value f_parse_46_own_notations_2(u_value v_ld, u_value v_n) {
  u_value s1461 = f_list_46_lookup_2(v_n, f_parse_46_Loader_46_modules_1(v_ld));
  u_value t1462 = NULL;
  if ((!t1462) && u_is_con(s1461, &n_option_46_Some)) {
    u_value v_ns = u_field(s1461, 0);
    t1462 = v_ns;
  }
  if ((!t1462) && u_is_con(s1461, &n_option_46_None)) {
    t1462 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1462) {
    u_no_case(s1461);
  }
  return t1462;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1463 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1463;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1464 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1464;
        u_value t1466 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1466)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1465 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1465;
          }
          t1466 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1467 = f_list_46_reverse_1(v_rev);
    *r_ps = t1467;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1468 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1468;
    u_value s1469 = f_parse_46_head_token_1(*r_ts);
    int d1470 = 0;
    if ((!d1470) && (u_is_con(s1469, &n_lex_46_TName) && u_equals(u_field(s1469, 0), u_mk_lit("prefix")))) {
      d1470 = 1;
      {
        u_value t1471 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1471;
      }
    }
    if ((!d1470) && (u_is_con(s1469, &n_lex_46_TName) && u_equals(u_field(s1469, 0), u_mk_lit("postfix")))) {
      d1470 = 1;
      {
        u_value t1472 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1472;
      }
    }
    if (!d1470) {
      d1470 = 1;
      {
        u_value s1473 = f_parse_46_second_token_1(*r_ts);
        int d1474 = 0;
        if ((!d1474) && (u_is_con(s1473, &n_lex_46_TName) && u_equals(u_field(s1473, 0), u_mk_lit("left")))) {
          d1474 = 1;
          {
            u_value t1475 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1475;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1474) && (u_is_con(s1473, &n_lex_46_TName) && u_equals(u_field(s1473, 0), u_mk_lit("right")))) {
          d1474 = 1;
          {
            u_value t1476 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1476;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1474) {
          d1474 = 1;
          {
          }
        }
        if (!d1474) {
          u_no_case(s1473);
        }
      }
    }
    if (!d1470) {
      u_no_case(s1469);
    }
    p_parse_46_advance(r_ts);
    u_value s1477 = f_parse_46_head_token_1(*r_ts);
    int d1478 = 0;
    if ((!d1478) && u_is_con(s1477, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1477, 0);
      d1478 = 1;
      {
        u_value t1479 = v_n;
        v_p = t1479;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1478) {
      d1478 = 1;
      {
        static struct u_object l_precedence_32_expec_1480 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1481 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1480, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1481)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1478) {
      u_no_case(s1477);
    }
    u_value t1482 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1483 = u_mk_bool(1);
    v_ops = t1482;
    v_go = t1483;
    u_value t1489 = v_go;
    while (u_truth(t1489)) {
      {
        u_value s1484 = f_parse_46_head_token_1(*r_ts);
        int d1485 = 0;
        if ((!d1485) && u_is_con(s1484, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1484, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1485 = 1;
            {
              u_value t1486 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1486;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1485) && u_is_con(s1484, &n_lex_46_TName)) {
          u_value v_n = u_field(s1484, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1485 = 1;
            {
              u_value t1487 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1487;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1485) {
          d1485 = 1;
          {
            u_value t1488 = u_mk_bool(0);
            v_go = t1488;
          }
        }
        if (!d1485) {
          u_no_case(s1484);
        }
      }
      t1489 = v_go;
    }
    u_value s1490 = v_ops;
    int d1491 = 0;
    if ((!d1491) && u_is_con(s1490, &n_list_46_Nil)) {
      d1491 = 1;
      {
        static struct u_object l_operator_32_expecte_1492 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1493 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1492, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1493)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1491) {
      d1491 = 1;
      {
        u_value t1494 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1494;
      }
    }
    if (!d1491) {
      u_no_case(s1490);
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
    u_value t1495 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1496 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1495;
    *r_fs = t1496;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1497 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1498 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1497;
            *r_fs = t1498;
            u_value t1501 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1501)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1499 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1500 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1499;
                *r_fs = t1500;
              }
              t1501 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1502 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1503 = f_list_46_reverse_1(*r_fs);
    *r_k = t1502;
    *r_fs = t1503;
    u_value s1504 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1505 = 0;
    if ((!d1505) && u_is_con(s1504, &n_option_46_Some)) {
      u_value v_f = u_field(s1504, 0);
      d1505 = 1;
      {
        static struct u_object l_repeated_32_field_32__1506 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1507 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1506, u_concat(v_f, u_concat(&l__32_of_32__1507, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1505) && u_is_con(s1504, &n_option_46_None)) {
      d1505 = 1;
      {
      }
    }
    if (!d1505) {
      u_no_case(s1504);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1508 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1509 = 0;
    if ((!d1509) && (u_is_con(u_field(s1508, 0), &n_lex_46_TName) && (u_is_con(u_field(s1508, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1508, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1508, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1509 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1510 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1510;
        }
      }
    }
    if (!d1509) {
      d1509 = 1;
      {
        u_value t1511 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1511;
      }
    }
    if (!d1509) {
      u_no_case(s1508);
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
    static struct u_object l__58__61__1512 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1512), r_ts);
    static struct u_object l__124__1513 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1513)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1514 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1514;
    static struct u_object l__124__1515 = U_LIT("|");
    u_value t1517 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1515));
    while (u_truth(t1517)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1516 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1516;
      }
      static struct u_object l__124__1518 = U_LIT("|");
      t1517 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1518));
    }
    u_value s1519 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1520 = 0;
    if ((!d1520) && u_is_con(s1519, &n_option_46_Some)) {
      u_value v_f = u_field(s1519, 0);
      d1520 = 1;
      {
        static struct u_object l_field_32__1521 = U_LIT("field ");
        static struct u_object l__32_of_32__1522 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1523 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1521, u_concat(v_f, u_concat(&l__32_of_32__1522, u_concat(v_n, &l__32_has_32_different_32_t_1523)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1520) && u_is_con(s1519, &n_option_46_None)) {
      d1520 = 1;
      {
      }
    }
    if (!d1520) {
      u_no_case(s1519);
    }
    u_value t1524 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)));
    *r_cs = t1524;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1525 = v_fs;
  u_value t1526 = NULL;
  if ((!t1526) && u_is_con(s1525, &n_list_46_Nil)) {
    t1526 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1526) && (u_is_con(s1525, &n_list_46_Cons) && u_is_con(u_field(s1525, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1525, 0), 0);
    u_value v_rest = u_field(s1525, 1);
    t1526 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1526) && (u_is_con(s1525, &n_list_46_Cons) && u_is_con(u_field(s1525, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1525, 1);
    t1526 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1526) {
    u_no_case(s1525);
  }
  return t1526;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1527 = v_fs;
  u_value t1528 = NULL;
  if ((!t1528) && u_is_con(s1527, &n_list_46_Nil)) {
    t1528 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1528) && u_is_con(s1527, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1527, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1527, 0), 1), 3);
    u_value v_rest = u_field(s1527, 1);
    u_value t1529 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1530 = &t1529;
    u_value l1531 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1531, &n_list_46_Cons)) {
      u_value v_e = u_field(l1531, 0);
      if (u_truth(u_mk_bool(1))) {
        e1530 = u_append(e1530, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1531 = u_field(l1531, 1);
    }
    u_value t1532;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1529))) {
      t1532 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1532 = f_parse_46_differing_1(v_rest);
    }
    t1528 = t1532;
  }
  if (!t1528) {
    u_no_case(s1527);
  }
  return t1528;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1533 = v_e;
  u_value t1534 = NULL;
  if (!t1534) {
    u_value v_t = u_field(u_field(s1533, 1), 3);
    t1534 = v_t;
  }
  if (!t1534) {
    u_no_case(s1533);
  }
  return t1534;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1535 = v_fs;
  u_value t1536 = NULL;
  if ((!t1536) && u_is_con(s1535, &n_list_46_Nil)) {
    t1536 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1536) && u_is_con(s1535, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1535, 0), 0);
    u_value v_d = u_field(u_field(s1535, 0), 1);
    u_value v_rest = u_field(s1535, 1);
    u_value t1537;
    if (u_truth(u_eq(v_g, v_f))) {
      t1537 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1537 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1536 = t1537;
  }
  if (!t1536) {
    u_no_case(s1535);
  }
  return t1536;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1538 = v_ks;
  u_value t1539 = NULL;
  if ((!t1539) && u_is_con(s1538, &n_list_46_Nil)) {
    t1539 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1539) && (u_is_con(s1538, &n_list_46_Cons) && u_is_con(u_field(u_field(s1538, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1538, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1538, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1538, 0), 1);
    u_value v_rest = u_field(s1538, 1);
    t1539 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1539) {
    u_no_case(s1538);
  }
  return t1539;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1540 = v_fs;
  u_value t1541 = NULL;
  if ((!t1541) && u_is_con(s1540, &n_list_46_Nil)) {
    t1541 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1541) && (u_is_con(s1540, &n_list_46_Cons) && u_is_con(u_field(u_field(s1540, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1540, 0), 0), 0);
    u_value v_t = u_field(u_field(s1540, 0), 1);
    u_value v_rest = u_field(s1540, 1);
    t1541 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1541) && (u_is_con(s1540, &n_list_46_Cons) && u_is_con(u_field(u_field(s1540, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1540, 1);
    t1541 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1541) {
    u_no_case(s1540);
  }
  return t1541;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1546 = f_parse_46_fields_of_1(v_ks);
  u_value t1545;
  {
    u_value v_fs = t1546;
    u_value t1542 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1543 = &t1542;
    u_value l1544 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1544, &n_list_46_Cons)) {
      u_value v_f = u_field(l1544, 0);
      if (u_truth(u_mk_bool(1))) {
        e1543 = u_append(e1543, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1544 = u_field(l1544, 1);
    }
    t1545 = t1542;
  }
  return t1545;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1547 = U_LIT("x");
  u_value t1548 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1549 = &t1548;
  u_value l1550 = v_ps;
  while (u_is_con(l1550, &n_list_46_Cons)) {
    u_value v_p = u_field(l1550, 0);
    if (u_truth(u_mk_bool(1))) {
      e1549 = u_append(e1549, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1550 = u_field(l1550, 1);
  }
  u_value t1558 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1547), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1559 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1548);
  u_value t1557;
  {
    u_value v_x = t1558;
    u_value v_owner = t1559;
    static struct u_object l__46__1551 = U_LIT(".");
    static struct u_object l_x_1552 = U_LIT("x");
    u_value t1553 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1554 = &t1553;
    u_value l1555 = v_es;
    while (u_is_con(l1555, &n_list_46_Cons)) {
      u_value v_e = u_field(l1555, 0);
      if (u_truth(u_mk_bool(1))) {
        e1554 = u_append(e1554, f_parse_46_field_case_1(v_e));
      }
      l1555 = u_field(l1555, 1);
    }
    u_value t1556;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1556 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1556 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1557 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1551, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1552), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1553, t1556)));
  }
  return t1557;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1560 = v_es;
  u_value t1561 = NULL;
  if ((!t1561) && u_is_con(s1560, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1560, 0), 1), 3);
    t1561 = v_t;
  }
  if (!t1561) {
    t1561 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1561) {
    u_no_case(s1560);
  }
  return t1561;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1562 = v_e;
  u_value t1563 = NULL;
  if (!t1563) {
    u_value v_k = u_field(u_field(s1562, 1), 0);
    u_value v_i = u_field(u_field(s1562, 1), 1);
    u_value v_n = u_field(u_field(s1562, 1), 2);
    u_value t1564 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1565 = &t1564;
    u_value l1566 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1566, &n_list_46_Cons)) {
      u_value v_j = u_field(l1566, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1568;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1567 = U_LIT("y");
          t1568 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1567), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1568 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1565 = u_append(e1565, t1568);
      }
      l1566 = u_field(l1566, 1);
    }
    static struct u_object l_y_1569 = U_LIT("y");
    t1563 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1564), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1569), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1563) {
    u_no_case(s1562);
  }
  return t1563;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_error_1570 = U_LIT("error");
  static struct u_object l_no_32_field_32__1571 = U_LIT("no field ");
  static struct u_object l__32_of_32__1572 = U_LIT(" of ");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_error_1570), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(&l_no_32_field_32__1571, u_concat(v_f, u_concat(&l__32_of_32__1572, v_t))))), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1573;
  if (u_truth(u_eq(v_i, v_n))) {
    t1573 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1573 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1573;
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
    u_value t1574 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1574;
    u_value t1576 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1576)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1575 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1575;
      }
      t1576 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1577 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1577), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1578 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1578), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1579 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1579;
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
    u_value t1580 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1581 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1582 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1583 = u_mk_bool(1);
    v_ins = t1580;
    v_outs = t1581;
    v_refs = t1582;
    v_first = t1583;
    static struct u_object l_in_1584 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1584))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1585 = u_mk_bool(0);
        v_first = t1585;
      }
    }
    static struct u_object l_out_1586 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1586, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1587 = u_mk_bool(0);
        v_first = t1587;
      }
    }
    static struct u_object l_ref_1588 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1588, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1589 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1589), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1590 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1590;
  }
}

void p_parse_46_parse_command(u_value v_ns, u_value v_ld, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_t = NULL;
    u_value v_c = NULL;
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_at = NULL;
    u_value s1591 = f_parse_46_head_token_1(*r_ts);
    int d1592 = 0;
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("import")))) {
      d1592 = 1;
      {
        u_value t1593 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1593;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Loader_46_modules_1(v_ld))))) {
          {
            u_value t1594 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1594;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_ns, v_ld, v_at, r_cs);
            u_value t1595 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1595;
          }
        }
      }
    }
    if ((!d1592) && u_is_con(s1591, &n_lex_46_TName)) {
      u_value v_w = u_field(s1591, 0);
      static struct u_object l_prefix_1597 = U_LIT("prefix");
      static struct u_object l_postfix_1598 = U_LIT("postfix");
      static struct u_object l_infix_1599 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1597, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1598, u_mk_con(&n_list_46_Cons, 2, &l_infix_1599, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1592 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1596 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1596;
        }
      }
    }
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("type")))) {
      d1592 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1600 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1600), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1601 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1601;
      }
    }
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("data")))) {
      d1592 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("function")))) {
      d1592 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1602 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1602;
      }
    }
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("procedure")))) {
      d1592 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1603 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1603;
      }
    }
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("eval")))) {
      d1592 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1604 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1604;
      }
    }
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("run")))) {
      d1592 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1605 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1605), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1606 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1606;
      }
    }
    if ((!d1592) && (u_is_con(s1591, &n_lex_46_TName) && u_equals(u_field(s1591, 0), u_mk_lit("test")))) {
      d1592 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1607 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1607), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1608 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1608;
      }
    }
    if (!d1592) {
      d1592 = 1;
      {
        static struct u_object l_declaration_32_expe_1609 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1610 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1609, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1610)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1592) {
      u_no_case(s1591);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_ns, u_value *r_ld) {
  {
    u_value s1611 = v_cs;
    int d1612 = 0;
    if ((!d1612) && u_is_con(s1611, &n_list_46_Nil)) {
      d1612 = 1;
      {
      }
    }
    if ((!d1612) && (u_is_con(s1611, &n_list_46_Cons) && u_is_con(u_field(s1611, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1611, 0), 0);
      u_value v_p = u_field(u_field(s1611, 0), 1);
      u_value v_ops = u_field(u_field(s1611, 0), 2);
      u_value v_more = u_field(s1611, 1);
      d1612 = 1;
      {
        u_value t1613 = f_list_46__43__43__2(*r_ns, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_ns = t1613;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1612) && (u_is_con(s1611, &n_list_46_Cons) && u_is_con(u_field(s1611, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1611, 0), 0);
      u_value v_ds = u_field(u_field(s1611, 0), 1);
      u_value v_more = u_field(s1611, 1);
      d1612 = 1;
      {
        u_value t1614 = u_mk_con(&n_parse_46_Loader, 4, f_list_46__43__43__2(f_parse_46_modules_of_1(u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds)), f_parse_46_Loader_46_modules_1(*r_ld)), f_parse_46_Loader_46_loading_1(*r_ld), f_parse_46_Loader_46_base_1(*r_ld), f_parse_46_Loader_46_dir_1(*r_ld));
        *r_ld = t1614;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1612) && (u_is_con(s1611, &n_list_46_Cons) && u_is_con(u_field(s1611, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1611, 0), 0);
      u_value v_more = u_field(s1611, 1);
      d1612 = 1;
      {
        u_value t1615 = f_list_46__43__43__2(*r_ns, f_parse_46_own_notations_2(*r_ld, v_n));
        *r_ns = t1615;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1612) && u_is_con(s1611, &n_list_46_Cons)) {
      u_value v_more = u_field(s1611, 1);
      d1612 = 1;
      {
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if (!d1612) {
      u_no_case(s1611);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_ns, u_value *r_ld, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1616 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1616;
    u_value t1618 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1618)) {
      {
        p_parse_46_parse_command(*r_ns, *r_ld, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_ns, r_ld);
        u_value t1617 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1617;
      }
      t1618 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1619 = f_list_46_reverse_1(v_rev);
    *r_cs = t1619;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1620 = v_dir;
    int d1621 = 0;
    if ((!d1621) && u_is_con(s1620, &n_option_46_Some)) {
      u_value v_d = u_field(s1620, 0);
      d1621 = 1;
      {
        {
          struct u_handler *h1624 = u_push();
          if (setjmp(h1624->env) == 0) {
            {
              static struct u_object l__47__1622 = U_LIT("/");
              static struct u_object l__46_u_1623 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1622, u_concat(v_n, &l__46_u_1623))), r_text);
            }
            u_pop(h1624);
          } else {
            u_pop(h1624);
            int d1625 = 0;
            if ((!d1625) && (u_has_type(h1624, "os.OsError") && u_is_con(u_caught(h1624), &n_os_46_OsError))) {
              d1625 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1625) {
              u_reraise(h1624);
            }
          }
        }
      }
    }
    if ((!d1621) && u_is_con(s1620, &n_option_46_None)) {
      d1621 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1621) {
      u_no_case(s1620);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1626 = u_push();
      if (setjmp(h1626->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1626);
      } else {
        u_pop(h1626);
        int d1627 = 0;
        if ((!d1627) && (u_has_type(h1626, "os.OsError") && u_is_con(u_caught(h1626), &n_os_46_OsError))) {
          d1627 = 1;
          {
            static struct u_object l_unknown_32_module_32__1628 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1628, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1627) {
          u_reraise(h1626);
        }
      }
    }
  }
}

void p_parse_46_load_module(u_value v_n, u_value v_ns, u_value v_ld, u_value v_at, u_value *r_cs) {
  {
    u_value v_text = NULL;
    u_value v_mns = NULL;
    u_value v_mld = NULL;
    u_value v_mts = NULL;
    if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Loader_46_loading_1(v_ld)))) {
      {
        static struct u_object l_cyclic_32_import_32_of_1629 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1629, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Loader_46_dir_1(v_ld), v_at, &v_text);
    u_value t1630 = u_mk_con(&n_parse_46_Loader, 4, f_parse_46_Loader_46_modules_1(v_ld), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Loader_46_loading_1(v_ld)), f_parse_46_Loader_46_base_1(v_ld), f_parse_46_Loader_46_dir_1(v_ld));
    v_mld = t1630;
    u_value s1631 = f_parse_46_Loader_46_base_1(v_ld);
    int d1632 = 0;
    if ((!d1632) && u_is_con(s1631, &n_option_46_Some)) {
      u_value v_b = u_field(s1631, 0);
      d1632 = 1;
      {
        u_value t1633 = v_b;
        v_mns = t1633;
      }
    }
    if ((!d1632) && u_is_con(s1631, &n_option_46_None)) {
      d1632 = 1;
      {
        u_value t1634 = v_ns;
        v_mns = t1634;
      }
    }
    if (!d1632) {
      u_no_case(s1631);
    }
    {
      struct u_handler *h1635 = u_push();
      if (setjmp(h1635->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mns, &v_mld, &v_mts);
        }
        u_pop(h1635);
      } else {
        u_pop(h1635);
        int d1636 = 0;
        if ((!d1636) && (u_has_type(h1635, "lex.LexError") && u_is_con(u_caught(h1635), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1635), 0);
          u_value v_i = u_field(u_caught(h1635), 1);
          d1636 = 1;
          {
            static struct u_object l__32_in_32__1637 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1637, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1636) && (u_has_type(h1635, "parse.ParseError") && u_is_con(u_caught(h1635), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1635), 0);
          u_value v_i = u_field(u_caught(h1635), 1);
          d1636 = 1;
          {
            static struct u_object l__32_in_32__1638 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1638, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1636) {
          u_reraise(h1635);
        }
      }
    }
  }
}

void p_parse_46_parse_text(u_value v_text, u_value v_ns, u_value v_ld, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_ns1 = NULL;
    u_value v_ld1 = NULL;
    {
      struct u_handler *h1639 = u_push();
      if (setjmp(h1639->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1639);
      } else {
        u_pop(h1639);
        int d1640 = 0;
        if ((!d1640) && (u_has_type(h1639, "lex.LexError") && u_is_con(u_caught(h1639), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1639), 0);
          u_value v_i = u_field(u_caught(h1639), 1);
          d1640 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1640) {
          u_reraise(h1639);
        }
      }
    }
    u_value t1641 = v_ns;
    u_value t1642 = v_ld;
    v_ns1 = t1641;
    v_ld1 = t1642;
    p_parse_46_parse_commands(&v_cs, &v_ns1, &v_ld1, &v_ts);
    u_value t1643 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1643;
  }
}

void p_parse_46_prelude_scope(u_value v_pre, u_value *r_ns, u_value *r_ms) {
  {
    u_value v_ld = NULL;
    u_value t1644 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1645 = u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    *r_ns = t1644;
    v_ld = t1645;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), r_ns, &v_ld);
    u_value t1646 = f_parse_46_Loader_46_modules_1(v_ld);
    *r_ms = t1646;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_ns = NULL;
    u_value v_ms = NULL;
    p_parse_46_prelude_scope(v_pre, &v_ns, &v_ms);
    p_parse_46_parse_text(v_text, v_ns, u_mk_con(&n_parse_46_Loader, 4, v_ms, u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_Some, 1, v_ns), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1647 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1647))) {
      {
        static struct u_object l_prelude_1648 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1648, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1649 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1649, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1650 = v_xs;
  u_value t1651 = NULL;
  if ((!t1651) && (u_is_con(s1650, &n_list_46_Cons) && u_is_con(u_field(s1650, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1650, 0);
    t1651 = v_x;
  }
  if ((!t1651) && u_is_con(s1650, &n_list_46_Cons)) {
    u_value v_more = u_field(s1650, 1);
    t1651 = f_parse_46_last_part_1(v_more);
  }
  if (!t1651) {
    static struct u_object l__1652 = U_LIT("");
    t1651 = &l__1652;
  }
  if (!t1651) {
    u_no_case(s1650);
  }
  return t1651;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1657 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1656;
  {
    u_value v_f = t1657;
    u_value t1654 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1654)) {
      static struct u_object l__46_u_1653 = U_LIT(".u");
      t1654 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1653);
    }
    u_value t1655;
    if (u_truth(t1654)) {
      t1655 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1655 = v_f;
    }
    t1656 = t1655;
  }
  return t1656;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1662 = u_str_split(v_path, u_mk_char(47));
  u_value t1661;
  {
    u_value v_parts = t1662;
    u_value t1660;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1658 = U_LIT(".");
      t1660 = &l__46__1658;
    } else {
      static struct u_object l__47__1659 = U_LIT("/");
      t1660 = u_str_join(&l__47__1659, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1661 = t1660;
  }
  return t1661;
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
    static struct u_object l_prelude_1663 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1663, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1664 = u_mk_tuple(2, v_cs, v_ds);
    int d1665 = 0;
    if ((!d1665) && (u_is_con(u_field(s1664, 0), &n_list_46_Cons) && u_is_con(u_field(s1664, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1664, 0), 0);
      u_value v_more = u_field(u_field(s1664, 0), 1);
      u_value v_d = u_field(u_field(s1664, 1), 0);
      u_value v_rest = u_field(u_field(s1664, 1), 1);
      d1665 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1665) {
      d1665 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1665) {
      u_no_case(s1664);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1666 = v_paths;
    int d1667 = 0;
    if ((!d1667) && u_is_con(s1666, &n_list_46_Nil)) {
      d1667 = 1;
      {
      }
    }
    if ((!d1667) && u_is_con(s1666, &n_list_46_Cons)) {
      u_value v_path = u_field(s1666, 0);
      u_value v_more = u_field(s1666, 1);
      d1667 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1667) {
      u_no_case(s1666);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1668 = v_paths;
    int d1669 = 0;
    if ((!d1669) && u_is_con(s1668, &n_list_46_Nil)) {
      d1669 = 1;
      {
      }
    }
    if ((!d1669) && u_is_con(s1668, &n_list_46_Cons)) {
      u_value v_path = u_field(s1668, 0);
      u_value v_more = u_field(s1668, 1);
      d1669 = 1;
      {
        u_value t1670 = u_mk_bool(0);
        v_failed = t1670;
        {
          struct u_handler *h1671 = u_push();
          if (setjmp(h1671->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1671);
          } else {
            u_pop(h1671);
            int d1672 = 0;
            if ((!d1672) && (u_has_type(h1671, "parse.ParseError") && u_is_con(u_caught(h1671), &n_parse_46_ParseError))) {
              d1672 = 1;
              {
                u_value t1673 = u_mk_bool(1);
                v_failed = t1673;
              }
            }
            if (!d1672) {
              u_reraise(h1671);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1669) {
      u_no_case(s1668);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1674 = v_x;
  u_value t1675 = NULL;
  if ((!t1675) && u_is_con(s1674, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1674, 0);
    t1675 = v_y;
  }
  if (!t1675) {
    u_no_case(s1674);
  }
  return t1675;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1676 = v_x;
  u_value t1677 = NULL;
  if ((!t1677) && u_is_con(s1676, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1676, 1);
    t1677 = v_y;
  }
  if (!t1677) {
    u_no_case(s1676);
  }
  return t1677;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1678 = v_x;
  u_value t1679 = NULL;
  if ((!t1679) && u_is_con(s1678, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1678, 2);
    t1679 = v_y;
  }
  if (!t1679) {
    u_no_case(s1678);
  }
  return t1679;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1680 = v_x;
  u_value t1681 = NULL;
  if ((!t1681) && u_is_con(s1680, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1680, 3);
    t1681 = v_y;
  }
  if (!t1681) {
    u_no_case(s1680);
  }
  return t1681;
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
  static struct u_object l_u_value_1682 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1682);
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
  static struct u_object l_u_value_1683 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1683));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1684 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1685 = &t1684;
  u_value l1686 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1686, &n_list_46_Cons)) {
    u_value v_c = u_field(l1686, 0);
    if (u_truth(u_mk_bool(1))) {
      e1685 = u_append(e1685, f_compile_46_mangle_char_1(v_c));
    }
    l1686 = u_field(l1686, 1);
  }
  return u_string(f_list_46_concat_1(t1684));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1687 = u_is_letter(v_c);
  if (!u_truth(t1687)) {
    t1687 = u_is_digit(v_c);
  }
  u_value t1688 = t1687;
  if (!u_truth(t1688)) {
    t1688 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1691;
  if (u_truth(t1688)) {
    t1691 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1689 = U_LIT("_");
    static struct u_object l___1690 = U_LIT("_");
    t1691 = u_chars(u_concat(&l___1689, u_concat(u_str_show(u_ord(v_c)), &l___1690)));
  }
  return t1691;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1692 = U_LIT("f_");
  static struct u_object l___1693 = U_LIT("_");
  return u_concat(&l_f__1692, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1693, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1694 = U_LIT("p_");
  return u_concat(&l_p__1694, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1695 = U_LIT("v_");
  return u_concat(&l_v__1695, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1696 = U_LIT("r_");
  return u_concat(&l_r__1696, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1697 = U_LIT("+");
  static struct u_object l_u_add_1698 = U_LIT("u_add");
  static struct u_object l__45__1699 = U_LIT("-");
  static struct u_object l_u_sub_1700 = U_LIT("u_sub");
  static struct u_object l__42__1701 = U_LIT("*");
  static struct u_object l_u_mul_1702 = U_LIT("u_mul");
  static struct u_object l__47__1703 = U_LIT("/");
  static struct u_object l_u_div_1704 = U_LIT("u_div");
  static struct u_object l__37__1705 = U_LIT("%");
  static struct u_object l_u_mod_1706 = U_LIT("u_mod");
  static struct u_object l__60__1707 = U_LIT("<");
  static struct u_object l_u_lt_1708 = U_LIT("u_lt");
  static struct u_object l__61__61__1709 = U_LIT("==");
  static struct u_object l_u_eq_1710 = U_LIT("u_eq");
  static struct u_object l_compare_1711 = U_LIT("compare");
  static struct u_object l_u_compare_1712 = U_LIT("u_compare");
  static struct u_object l_error_1713 = U_LIT("error");
  static struct u_object l_u_error_1714 = U_LIT("u_error");
  static struct u_object l_chars_1715 = U_LIT("chars");
  static struct u_object l_u_chars_1716 = U_LIT("u_chars");
  static struct u_object l_string_1717 = U_LIT("string");
  static struct u_object l_u_string_1718 = U_LIT("u_string");
  static struct u_object l_ord_1719 = U_LIT("ord");
  static struct u_object l_u_ord_1720 = U_LIT("u_ord");
  static struct u_object l_chr_1721 = U_LIT("chr");
  static struct u_object l_u_chr_1722 = U_LIT("u_chr");
  static struct u_object l__94__1723 = U_LIT("^");
  static struct u_object l_u_concat_1724 = U_LIT("u_concat");
  static struct u_object l_str_size_1725 = U_LIT("str_size");
  static struct u_object l_u_str_size_1726 = U_LIT("u_str_size");
  static struct u_object l_str_join_1727 = U_LIT("str_join");
  static struct u_object l_u_str_join_1728 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1729 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1730 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1731 = U_LIT("str_split");
  static struct u_object l_u_str_split_1732 = U_LIT("u_str_split");
  static struct u_object l_str_get_1733 = U_LIT("str_get");
  static struct u_object l_u_str_get_1734 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1735 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1736 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1737 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1738 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1739 = U_LIT("str_show");
  static struct u_object l_u_str_show_1740 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1741 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1742 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1743 = U_LIT("str_number");
  static struct u_object l_u_str_number_1744 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1745 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1746 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1747 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1748 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1749 = U_LIT("is_space");
  static struct u_object l_u_is_space_1750 = U_LIT("u_is_space");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1697), u_mk_tuple(2, &l_u_add_1698, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1699), u_mk_tuple(2, &l_u_sub_1700, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1701), u_mk_tuple(2, &l_u_mul_1702, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1703), u_mk_tuple(2, &l_u_div_1704, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1705), u_mk_tuple(2, &l_u_mod_1706, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1707), u_mk_tuple(2, &l_u_lt_1708, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1709), u_mk_tuple(2, &l_u_eq_1710, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1711), u_mk_tuple(2, &l_u_compare_1712, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_error_1713), u_mk_tuple(2, &l_u_error_1714, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1715), u_mk_tuple(2, &l_u_chars_1716, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1717), u_mk_tuple(2, &l_u_string_1718, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1719), u_mk_tuple(2, &l_u_ord_1720, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1721), u_mk_tuple(2, &l_u_chr_1722, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1723), u_mk_tuple(2, &l_u_concat_1724, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1725), u_mk_tuple(2, &l_u_str_size_1726, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1727), u_mk_tuple(2, &l_u_str_join_1728, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1729), u_mk_tuple(2, &l_u_str_slice_1730, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1731), u_mk_tuple(2, &l_u_str_split_1732, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1733), u_mk_tuple(2, &l_u_str_get_1734, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1735), u_mk_tuple(2, &l_u_str_contains_1736, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1737), u_mk_tuple(2, &l_u_str_compare_1738, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1739), u_mk_tuple(2, &l_u_str_show_1740, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1741), u_mk_tuple(2, &l_u_str_quote_1742, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1743), u_mk_tuple(2, &l_u_str_number_1744, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1745), u_mk_tuple(2, &l_u_is_digit_1746, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1747), u_mk_tuple(2, &l_u_is_letter_1748, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1749), u_mk_tuple(2, &l_u_is_space_1750, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1751 = U_LIT("&");
  static struct u_object l_n__1752 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1751, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1752, f_compile_46_mangle_1(v_n))));
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
  cache = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_nil_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_cons_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_none_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_some_name_0(), u_mk_con(&n_list_46_Cons, 2, f_syntax_46_os_error_name_0(), u_mk_con(&n_list_46_Nil, 0))))));
  return cache;
}

u_value f_compile_46_descriptor_decl_2(u_value v_n, u_value v_i) {
  u_value s1753 = v_n;
  u_value t1754 = NULL;
  if ((!t1754) && (u_is_con(s1753, &n_syntax_46_Name) && u_is_con(u_field(s1753, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1753, 0), 0);
    u_value v_b = u_field(s1753, 1);
    static struct u_object l_const_32_struct_32_u_n_1755 = U_LIT("const struct u_name");
    static struct u_object l_n__1756 = U_LIT("n_");
    t1754 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1755), u_concat(&l_n__1756, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1754) && (u_is_con(s1753, &n_syntax_46_Name) && u_is_con(u_field(s1753, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1753, 1);
    static struct u_object l_const_32_struct_32_u_n_1757 = U_LIT("const struct u_name");
    static struct u_object l_n__1758 = U_LIT("n_");
    static struct u_object l_NULL_1759 = U_LIT("NULL");
    t1754 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1757), u_concat(&l_n__1758, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1759), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1754) {
    u_no_case(s1753);
  }
  return t1754;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1760 = v_cs;
  u_value t1761 = NULL;
  if ((!t1761) && u_is_con(s1760, &n_list_46_Nil)) {
    t1761 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1761) && (u_is_con(s1760, &n_list_46_Cons) && u_is_con(u_field(s1760, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1760, 0), 2);
    u_value v_rest = u_field(s1760, 1);
    u_value t1762 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1763 = &t1762;
    u_value l1764 = v_ks;
    while (u_is_con(l1764, &n_list_46_Cons)) {
      u_value v_k = u_field(l1764, 0);
      if (u_truth(u_mk_bool(1))) {
        e1763 = u_append(e1763, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1764 = u_field(l1764, 1);
    }
    t1761 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1762, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1761) && u_is_con(s1760, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1760, 1);
    t1761 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1761) {
    u_no_case(s1760);
  }
  return t1761;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1765 = v_ks;
  u_value t1766 = NULL;
  if ((!t1766) && u_is_con(s1765, &n_list_46_Nil)) {
    t1766 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1766) && u_is_con(s1765, &n_list_46_Cons)) {
    u_value v_k = u_field(s1765, 0);
    u_value v_rest = u_field(s1765, 1);
    u_value t1767;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1767 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1767 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1766 = t1767;
  }
  if (!t1766) {
    u_no_case(s1765);
  }
  return t1766;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1768 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1769 = &t1768;
  u_value l1770 = v_xs;
  while (u_is_con(l1770, &n_list_46_Cons)) {
    u_value v_x = u_field(l1770, 0);
    if (u_truth(u_mk_bool(1))) {
      e1769 = u_append(e1769, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1770 = u_field(l1770, 1);
  }
  return t1768;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1771 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1771, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1772 = f_list_46_lookup_2(v_x, v_env);
  u_value t1773 = NULL;
  if ((!t1773) && u_is_con(s1772, &n_option_46_Some)) {
    u_value v_c = u_field(s1772, 0);
    t1773 = v_c;
  }
  if (!t1773) {
    t1773 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1773) {
    u_no_case(s1772);
  }
  return t1773;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1774 = v_c;
  u_value t1775 = NULL;
  if ((!t1775) && (u_is_con(s1774, &n_c_46_CeUnary) && u_equals(u_field(s1774, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1774, 1);
    t1775 = v_p;
  }
  if (!t1775) {
    static struct u_object l__38__1776 = U_LIT("&");
    t1775 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1776, v_c);
  }
  if (!t1775) {
    u_no_case(s1774);
  }
  return t1775;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1777 = v_t;
  u_value t1778 = NULL;
  if ((!t1778) && (u_is_con(s1777, &n_syntax_46_TSort) && u_is_con(u_field(s1777, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1777, 0);
    t1778 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1778) && u_is_con(s1777, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1777, 0);
    u_value v_args = u_field(s1777, 1);
    static struct u_object l__91__1779 = U_LIT("[");
    static struct u_object l__44__32__1780 = U_LIT(", ");
    u_value t1781 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1782 = &t1781;
    u_value l1783 = v_args;
    while (u_is_con(l1783, &n_list_46_Cons)) {
      u_value v_a = u_field(l1783, 0);
      if (u_truth(u_mk_bool(1))) {
        e1782 = u_append(e1782, f_compile_46_type_key_1(v_a));
      }
      l1783 = u_field(l1783, 1);
    }
    static struct u_object l__93__1784 = U_LIT("]");
    t1778 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1779, u_concat(u_str_join(&l__44__32__1780, t1781), &l__93__1784)));
  }
  if ((!t1778) && u_is_con(s1777, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1777, 0);
    u_value v_r = u_field(s1777, 1);
    static struct u_object l__40__1785 = U_LIT("(");
    static struct u_object l__44__32__1786 = U_LIT(", ");
    u_value t1787 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1788 = &t1787;
    u_value l1789 = v_args;
    while (u_is_con(l1789, &n_list_46_Cons)) {
      u_value v_a = u_field(l1789, 0);
      if (u_truth(u_mk_bool(1))) {
        e1788 = u_append(e1788, f_compile_46_type_key_1(v_a));
      }
      l1789 = u_field(l1789, 1);
    }
    static struct u_object l__41__32__45__62__32__1790 = U_LIT(") -> ");
    t1778 = u_concat(&l__40__1785, u_concat(u_str_join(&l__44__32__1786, t1787), u_concat(&l__41__32__45__62__32__1790, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1778) && u_is_con(s1777, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1777, 0);
    static struct u_object l__40__1791 = U_LIT("(");
    static struct u_object l__32__42__32__1792 = U_LIT(" * ");
    u_value t1793 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1794 = &t1793;
    u_value l1795 = v_ts;
    while (u_is_con(l1795, &n_list_46_Cons)) {
      u_value v_a = u_field(l1795, 0);
      if (u_truth(u_mk_bool(1))) {
        e1794 = u_append(e1794, f_compile_46_type_key_1(v_a));
      }
      l1795 = u_field(l1795, 1);
    }
    static struct u_object l__41__1796 = U_LIT(")");
    t1778 = u_concat(&l__40__1791, u_concat(u_str_join(&l__32__42__32__1792, t1793), &l__41__1796));
  }
  if (!t1778) {
    static struct u_object l__63__1797 = U_LIT("?");
    t1778 = &l__63__1797;
  }
  if (!t1778) {
    u_no_case(s1777);
  }
  return t1778;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1798 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1799 = &t1798;
  u_value l1800 = v_cs;
  while (u_is_con(l1800, &n_list_46_Cons)) {
    u_value v_c = u_field(l1800, 0);
    if (u_truth(u_mk_bool(1))) {
      e1799 = u_append(e1799, f_compile_46_fun_arities_1(v_c));
    }
    l1800 = u_field(l1800, 1);
  }
  u_value t1801 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1802 = &t1801;
  u_value l1803 = v_cs;
  while (u_is_con(l1803, &n_list_46_Cons)) {
    u_value v_c = u_field(l1803, 0);
    if (u_truth(u_mk_bool(1))) {
      e1802 = u_append(e1802, f_compile_46_constr_arities_1(v_c));
    }
    l1803 = u_field(l1803, 1);
  }
  u_value t1805 = f_list_46_concat_1(t1798);
  u_value t1806 = f_list_46_concat_1(t1801);
  u_value t1804;
  {
    u_value v_fs = t1805;
    u_value v_ks = t1806;
    t1804 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1804;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1807 = v_fs;
  u_value t1808 = NULL;
  if ((!t1808) && u_is_con(s1807, &n_list_46_Nil)) {
    t1808 = v_m;
  }
  if ((!t1808) && u_is_con(s1807, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1807, 0), 0);
    u_value v_k = u_field(u_field(s1807, 0), 1);
    u_value v_rest = u_field(s1807, 1);
    u_value s1809 = f_map_46_get_2(v_f, v_m);
    u_value t1810 = NULL;
    if ((!t1810) && u_is_con(s1809, &n_option_46_Some)) {
      u_value v_ks = u_field(s1809, 0);
      t1810 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1810) && u_is_con(s1809, &n_option_46_None)) {
      t1810 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1810) {
      u_no_case(s1809);
    }
    t1808 = t1810;
  }
  if (!t1808) {
    u_no_case(s1807);
  }
  return t1808;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1811 = v_gl;
  u_value t1812 = NULL;
  if ((!t1812) && u_is_con(s1811, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1811, 2);
    u_value s1813 = f_map_46_get_2(v_n, v_fm);
    u_value t1814 = NULL;
    if ((!t1814) && u_is_con(s1813, &n_option_46_Some)) {
      u_value v_ks = u_field(s1813, 0);
      t1814 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1814) && u_is_con(s1813, &n_option_46_None)) {
      t1814 = u_mk_bool(0);
    }
    if (!t1814) {
      u_no_case(s1813);
    }
    t1812 = t1814;
  }
  if (!t1812) {
    u_no_case(s1811);
  }
  return t1812;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1815 = v_gl;
  u_value t1816 = NULL;
  if ((!t1816) && u_is_con(s1815, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1815, 3);
    t1816 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1816) {
    u_no_case(s1815);
  }
  return t1816;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1817 = v_c;
  u_value t1818 = NULL;
  if ((!t1818) && u_is_con(s1817, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1817, 0);
    u_value v_xss = u_field(s1817, 2);
    t1818 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1818) {
    t1818 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1818) {
    u_no_case(s1817);
  }
  return t1818;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1819 = v_c;
  u_value t1820 = NULL;
  if ((!t1820) && u_is_con(s1819, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1819, 2);
    u_value t1821 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1822 = &t1821;
    u_value l1823 = v_ks;
    while (u_is_con(l1823, &n_list_46_Cons)) {
      u_value v_k = u_field(l1823, 0);
      if (u_truth(u_mk_bool(1))) {
        e1822 = u_append(e1822, f_compile_46_constr_arity_1(v_k));
      }
      l1823 = u_field(l1823, 1);
    }
    t1820 = t1821;
  }
  if (!t1820) {
    t1820 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1820) {
    u_no_case(s1819);
  }
  return t1820;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1824 = v_k;
  u_value t1825 = NULL;
  if ((!t1825) && u_is_con(s1824, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1824, 0);
    u_value v_ts = u_field(s1824, 1);
    t1825 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1825) {
    u_no_case(s1824);
  }
  return t1825;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1826 = v_xs;
  u_value t1827 = NULL;
  if ((!t1827) && u_is_con(s1826, &n_list_46_Nil)) {
    t1827 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1827) && u_is_con(s1826, &n_list_46_Cons)) {
    u_value v_x = u_field(s1826, 0);
    u_value v_rest = u_field(s1826, 1);
    t1827 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1827) {
    u_no_case(s1826);
  }
  return t1827;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1828 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1828, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1829 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1829, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1830 = v_cs;
  u_value t1831 = NULL;
  if ((!t1831) && u_is_con(s1830, &n_list_46_Nil)) {
    t1831 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1831) && (u_is_con(s1830, &n_list_46_Cons) && u_is_con(u_field(s1830, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1830, 0);
    t1831 = v_d;
  }
  if ((!t1831) && u_is_con(s1830, &n_list_46_Cons)) {
    u_value v_d = u_field(s1830, 0);
    u_value v_rest = u_field(s1830, 1);
    static struct u_object l__38__38__1832 = U_LIT("&&");
    t1831 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1832, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1831) {
    u_no_case(s1830);
  }
  return t1831;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1833 = v_l;
  u_value t1834 = NULL;
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1835 = U_LIT("u_mk_tuple");
    t1834 = f_c_46_c_call_2(&l_u_mk_tuple_1835, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1833, 0);
    static struct u_object l_u_mk_bool_1836 = U_LIT("u_mk_bool");
    u_value t1837;
    if (u_truth(v_b)) {
      t1837 = u_mk_int(1);
    } else {
      t1837 = u_mk_int(0);
    }
    t1834 = f_c_46_c_call_2(&l_u_mk_bool_1836, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1837), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1833, 0);
    u_value t1838 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1838)) {
      t1838 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1841;
    if (u_truth(t1838)) {
      static struct u_object l_u_mk_int_1839 = U_LIT("u_mk_int");
      t1841 = f_c_46_c_call_2(&l_u_mk_int_1839, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1840 = U_LIT("u_mk_int_str");
      t1841 = f_c_46_c_call_2(&l_u_mk_int_str_1840, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1834 = t1841;
  }
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1833, 0);
    static struct u_object l_u_mk_lit_1842 = U_LIT("u_mk_lit");
    t1834 = f_c_46_c_call_2(&l_u_mk_lit_1842, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1834) && u_is_con(s1833, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1833, 0);
    static struct u_object l_u_mk_char_1843 = U_LIT("u_mk_char");
    t1834 = f_c_46_c_call_2(&l_u_mk_char_1843, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1834) {
    u_no_case(s1833);
  }
  return t1834;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1844 = v_p;
  u_value t1845 = NULL;
  if ((!t1845) && u_is_con(s1844, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1844, 0);
    static struct u_object l_u_equals_1846 = U_LIT("u_equals");
    t1845 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1846, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1845) && u_is_con(s1844, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1844, 0);
    u_value v_ps = u_field(s1844, 1);
    static struct u_object l_u_is_con_1847 = U_LIT("u_is_con");
    t1845 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1847, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1845) && u_is_con(s1844, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1844, 0);
    t1845 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1845) {
    t1845 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1845) {
    u_no_case(s1844);
  }
  return t1845;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1848 = v_ps;
  u_value t1849 = NULL;
  if ((!t1849) && u_is_con(s1848, &n_list_46_Nil)) {
    t1849 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1849) && u_is_con(s1848, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1848, 0), 0);
    u_value v_i = u_field(u_field(s1848, 0), 1);
    u_value v_rest = u_field(s1848, 1);
    t1849 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1849) {
    u_no_case(s1848);
  }
  return t1849;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1850 = v_p;
  u_value t1851 = NULL;
  if ((!t1851) && (u_is_con(s1850, &n_syntax_46_PVar) && u_is_con(u_field(s1850, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1850, 0), 0);
    t1851 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1851) && u_is_con(s1850, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1850, 1);
    t1851 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1851) && u_is_con(s1850, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1850, 0);
    t1851 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1851) {
    t1851 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1851) {
    u_no_case(s1850);
  }
  return t1851;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1852 = v_ps;
  u_value t1853 = NULL;
  if ((!t1853) && u_is_con(s1852, &n_list_46_Nil)) {
    t1853 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1853) && u_is_con(s1852, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1852, 0), 0);
    u_value v_i = u_field(u_field(s1852, 0), 1);
    u_value v_rest = u_field(s1852, 1);
    t1853 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1853) {
    u_no_case(s1852);
  }
  return t1853;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1854 = v_p;
  u_value t1855 = NULL;
  if ((!t1855) && (u_is_con(s1854, &n_syntax_46_PVar) && u_is_con(u_field(s1854, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1854, 0), 0);
    t1855 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1855) && u_is_con(s1854, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1854, 1);
    u_value t1856 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1857 = &t1856;
    u_value l1858 = v_ps;
    while (u_is_con(l1858, &n_list_46_Cons)) {
      u_value v_q = u_field(l1858, 0);
      if (u_truth(u_mk_bool(1))) {
        e1857 = u_append(e1857, f_compile_46_pattern_names_1(v_q));
      }
      l1858 = u_field(l1858, 1);
    }
    t1855 = f_list_46_concat_1(t1856);
  }
  if ((!t1855) && u_is_con(s1854, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1854, 0);
    u_value t1859 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1860 = &t1859;
    u_value l1861 = v_ps;
    while (u_is_con(l1861, &n_list_46_Cons)) {
      u_value v_q = u_field(l1861, 0);
      if (u_truth(u_mk_bool(1))) {
        e1860 = u_append(e1860, f_compile_46_pattern_names_1(v_q));
      }
      l1861 = u_field(l1861, 1);
    }
    t1855 = f_list_46_concat_1(t1859);
  }
  if (!t1855) {
    t1855 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1855) {
    u_no_case(s1854);
  }
  return t1855;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1862 = v_e;
  u_value t1863 = NULL;
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_ELit)) {
    t1863 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1863) && (u_is_con(s1862, &n_syntax_46_EVar) && u_is_con(u_field(s1862, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1862, 0), 0);
    u_value t1864;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1864 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1864 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1863 = t1864;
  }
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1862, 0);
    u_value v_args = u_field(s1862, 1);
    u_value t1865 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1866 = &t1865;
    u_value l1867 = v_args;
    while (u_is_con(l1867, &n_list_46_Cons)) {
      u_value v_a = u_field(l1867, 0);
      if (u_truth(u_mk_bool(1))) {
        e1866 = u_append(e1866, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1867 = u_field(l1867, 1);
    }
    t1863 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1865));
  }
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1862, 0);
    u_value v_b = u_field(s1862, 1);
    t1863 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1862, 0);
    u_value v_b = u_field(s1862, 1);
    u_value t1868 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1869 = &t1868;
    u_value l1870 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1870, &n_list_46_Cons)) {
      u_value v_a = u_field(l1870, 0);
      if (u_truth(u_mk_bool(1))) {
        e1869 = u_append(e1869, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1870 = u_field(l1870, 1);
    }
    t1863 = f_list_46__43__43__2(f_list_46_concat_1(t1868), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1862, 0);
    u_value v_l = u_field(s1862, 1);
    u_value v_r = u_field(s1862, 2);
    t1863 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1862, 0);
    u_value v_cs = u_field(s1862, 1);
    u_value t1871 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1872 = &t1871;
    u_value l1873 = v_cs;
    while (u_is_con(l1873, &n_list_46_Cons)) {
      u_value v_b = u_field(l1873, 0);
      if (u_truth(u_mk_bool(1))) {
        e1872 = u_append(e1872, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1873 = u_field(l1873, 1);
    }
    t1863 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1871));
  }
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1862, 0);
    u_value v_xs = u_field(s1862, 1);
    u_value v_ls = u_field(s1862, 2);
    u_value v_c = u_field(s1862, 3);
    u_value t1874 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1875 = &t1874;
    u_value l1876 = v_ls;
    while (u_is_con(l1876, &n_list_46_Cons)) {
      u_value v_l = u_field(l1876, 0);
      if (u_truth(u_mk_bool(1))) {
        e1875 = u_append(e1875, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1876 = u_field(l1876, 1);
    }
    t1863 = f_list_46__43__43__2(f_list_46_concat_1(t1874), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1863) && u_is_con(s1862, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1862, 0);
    u_value t1877 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1878 = &t1877;
    u_value l1879 = v_es;
    while (u_is_con(l1879, &n_list_46_Cons)) {
      u_value v_a = u_field(l1879, 0);
      if (u_truth(u_mk_bool(1))) {
        e1878 = u_append(e1878, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1879 = u_field(l1879, 1);
    }
    t1863 = f_list_46_concat_1(t1877);
  }
  if (!t1863) {
    u_no_case(s1862);
  }
  return t1863;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1880 = v_b;
  u_value t1881 = NULL;
  if ((!t1881) && (u_is_con(s1880, &n_syntax_46_Branch) && u_is_con(u_field(s1880, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1880, 0);
    u_value v_g = u_field(u_field(s1880, 1), 0);
    u_value v_e = u_field(s1880, 2);
    t1881 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1881) && (u_is_con(s1880, &n_syntax_46_Branch) && u_is_con(u_field(s1880, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1880, 0);
    u_value v_e = u_field(s1880, 2);
    t1881 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1881) {
    u_no_case(s1880);
  }
  return t1881;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1882 = v_xss;
  u_value t1883 = NULL;
  if ((!t1883) && u_is_con(s1882, &n_list_46_Nil)) {
    t1883 = v_b;
  }
  if ((!t1883) && u_is_con(s1882, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1882, 0);
    u_value v_rest = u_field(s1882, 1);
    t1883 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1883) {
    u_no_case(s1882);
  }
  return t1883;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1884 = *r_g;
    int d1885 = 0;
    if ((!d1885) && u_is_con(s1884, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1884, 0);
      u_value v_ds = u_field(s1884, 1);
      d1885 = 1;
      {
        u_value t1886 = u_concat(v_stem, u_str_show(v_n));
        u_value t1887 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1886;
        *r_g = t1887;
      }
    }
    if (!d1885) {
      u_no_case(s1884);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1888 = *r_g;
    int d1889 = 0;
    if ((!d1889) && u_is_con(s1888, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1888, 0);
      u_value v_ds = u_field(s1888, 1);
      d1889 = 1;
      {
        u_value t1890 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1890;
      }
    }
    if (!d1889) {
      u_no_case(s1888);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1891 = U_LIT("u_mk_closure");
        static struct u_object l_w__1892 = U_LIT("w_");
        u_value t1893 = f_c_46_c_call_2(&l_u_mk_closure_1891, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1892, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1893;
      }
    } else {
      {
        u_value s1894 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1895 = 0;
        if ((!d1895) && (u_is_con(s1894, &n_option_46_Some) && u_equals(u_field(s1894, 0), u_mk_int(0)))) {
          d1895 = 1;
          {
            static struct u_object l_u_mk_con_1896 = U_LIT("u_mk_con");
            u_value t1897 = f_c_46_c_call_2(&l_u_mk_con_1896, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1897;
          }
        }
        if ((!d1895) && u_is_con(s1894, &n_option_46_Some)) {
          d1895 = 1;
          {
            static struct u_object l_u_mk_closure_1898 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1899 = U_LIT("wc_");
            u_value t1900 = f_c_46_c_call_2(&l_u_mk_closure_1898, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1899, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1900;
          }
        }
        if ((!d1895) && u_is_con(s1894, &n_option_46_None)) {
          d1895 = 1;
          {
            u_value s1901 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1902 = 0;
            if ((!d1902) && u_is_con(s1901, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1901, 0), 0);
              d1902 = 1;
              {
                static struct u_object l_u_mk_closure_1903 = U_LIT("u_mk_closure");
                static struct u_object l_w_1904 = U_LIT("w");
                u_value t1905 = f_c_46_c_call_2(&l_u_mk_closure_1903, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1904, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1905;
              }
            }
            if ((!d1902) && u_is_con(s1901, &n_option_46_None)) {
              d1902 = 1;
              {
                static struct u_object l_the_32_variable_32__1906 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1906, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1902) {
              u_no_case(s1901);
            }
          }
        }
        if (!d1895) {
          u_no_case(s1894);
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
    u_value s1907 = v_e;
    int d1908 = 0;
    if ((!d1908) && (u_is_con(s1907, &n_syntax_46_ELit) && u_is_con(u_field(s1907, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1907, 0), 0);
      d1908 = 1;
      {
        static struct u_object l_l__1909 = U_LIT("l_");
        static struct u_object l___1910 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1909, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), u_mk_int(16)))), &l___1910)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1911 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1912 = U_LIT("U_LIT");
        static struct u_object l__38__1913 = U_LIT("&");
        u_value t1914 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1911), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1912, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1915 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1913, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1914;
        *r_r = t1915;
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1907, 0);
      d1908 = 1;
      {
        u_value t1916 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1917 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1916;
        *r_r = t1917;
      }
    }
    if ((!d1908) && (u_is_con(s1907, &n_syntax_46_EVar) && u_is_con(u_field(s1907, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1907, 0), 0);
      u_value v_ty = u_field(u_field(s1907, 0), 1);
      d1908 = 1;
      {
        u_value s1918 = f_list_46_lookup_2(v_n, v_env);
        int d1919 = 0;
        if ((!d1919) && u_is_con(s1918, &n_option_46_Some)) {
          u_value v_c = u_field(s1918, 0);
          d1919 = 1;
          {
            u_value t1920 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1921 = v_c;
            *r_ss = t1920;
            *r_r = t1921;
          }
        }
        if ((!d1919) && u_is_con(s1918, &n_option_46_None)) {
          d1919 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1922 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1922;
          }
        }
        if (!d1919) {
          u_no_case(s1918);
        }
      }
    }
    if ((!d1908) && (u_is_con(s1907, &n_syntax_46_EApp) && (u_is_con(u_field(s1907, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1907, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1907, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1907, 0), 0), 1);
      u_value v_args = u_field(s1907, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1908 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1907, 0);
      u_value v_args = u_field(s1907, 1);
      d1908 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1923 = U_LIT("u_apply");
        u_value t1924 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1925 = f_c_46_c_call_2(&l_u_apply_1923, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1924;
        *r_r = t1925;
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1907, 0);
      u_value v_b = u_field(s1907, 1);
      d1908 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1907, 0);
      u_value v_b = u_field(s1907, 1);
      d1908 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1926 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1926, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1927 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1927;
        u_value t1928 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1928;
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1907, 0);
      u_value v_l = u_field(s1907, 1);
      u_value v_rr = u_field(s1907, 2);
      d1908 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1929 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1929, &v_t, r_g);
        u_value t1930 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1930;
        u_value t1931 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1931;
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1907, 0);
      u_value v_cases = u_field(s1907, 1);
      d1908 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1932 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1932, &v_d, r_g);
        static struct u_object l_t_1933 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1933, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1934 = U_LIT("NULL");
        u_value t1935 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1934))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1935;
        static struct u_object l__33__1936 = U_LIT("!");
        static struct u_object l_u_no_case_1937 = U_LIT("u_no_case");
        u_value t1938 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1936, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1937, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1938;
        u_value t1939 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1939;
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1907, 0);
      u_value v_xs = u_field(s1907, 1);
      u_value v_lists = u_field(s1907, 2);
      u_value v_c = u_field(s1907, 3);
      d1908 = 1;
      {
        static struct u_object l_t_1940 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1940, &v_t, r_g);
        static struct u_object l_e_1941 = U_LIT("e");
        p_compile_46_gen_name(&l_e_1941, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_1942 = U_LIT("u_mk_con");
        static struct u_object l__38__1943 = U_LIT("&");
        u_value t1944 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_1942, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__1943, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t1944;
        u_value t1945 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1945;
      }
    }
    if ((!d1908) && u_is_con(s1907, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1907, 0);
      d1908 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_1946 = U_LIT("u_mk_tuple");
        u_value t1947 = f_c_46_c_call_2(&l_u_mk_tuple_1946, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t1947;
      }
    }
    if (!d1908) {
      u_no_case(s1907);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s1948 = v_es;
    int d1949 = 0;
    if ((!d1949) && u_is_con(s1948, &n_list_46_Nil)) {
      d1949 = 1;
      {
        u_value t1950 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1951 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1950;
        *r_rs = t1951;
      }
    }
    if ((!d1949) && u_is_con(s1948, &n_list_46_Cons)) {
      u_value v_e = u_field(s1948, 0);
      u_value v_rest = u_field(s1948, 1);
      d1949 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t1952 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1953 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t1952;
        *r_rs = t1953;
      }
    }
    if (!d1949) {
      u_no_case(s1948);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s1954 = v_rs;
    int d1955 = 0;
    if ((!d1955) && u_is_con(s1954, &n_list_46_Nil)) {
      d1955 = 1;
      {
        u_value t1956 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1956;
      }
    }
    if ((!d1955) && u_is_con(s1954, &n_list_46_Cons)) {
      u_value v_r = u_field(s1954, 0);
      u_value v_rest = u_field(s1954, 1);
      d1955 = 1;
      {
        static struct u_object l_t_1957 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1957, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t1958 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t1958;
      }
    }
    if (!d1955) {
      u_no_case(s1954);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s1959 = u_mk_tuple(2, v_xs, v_ts);
  u_value t1960 = NULL;
  if ((!t1960) && (u_is_con(u_field(s1959, 0), &n_list_46_Cons) && (u_is_con(u_field(s1959, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1959, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s1959, 0), 0);
    u_value v_more = u_field(u_field(s1959, 0), 1);
    u_value v_t = u_field(u_field(u_field(s1959, 1), 0), 1);
    u_value v_rest = u_field(u_field(s1959, 1), 1);
    t1960 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t1960) {
    t1960 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1960) {
    u_no_case(s1959);
  }
  return t1960;
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
    u_value s1961 = v_args;
    int d1962 = 0;
    if ((!d1962) && (u_is_con(s1961, &n_list_46_Cons) && (u_is_con(u_field(s1961, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1961, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s1961, 0);
      u_value v_b = u_field(u_field(s1961, 1), 0);
      static struct u_object l_and_1969 = U_LIT("and");
      u_value t1971 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1969));
      if (!u_truth(t1971)) {
        static struct u_object l_or_1970 = U_LIT("or");
        t1971 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_1970));
      }
      if (u_truth(t1971)) {
        d1962 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_1963 = U_LIT("t");
          p_compile_46_gen_name(&l_t_1963, &v_t, r_g);
          static struct u_object l_and_1964 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1964)))) {
            {
              u_value t1965 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1965;
            }
          } else {
            {
              static struct u_object l__33__1966 = U_LIT("!");
              u_value t1967 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1966, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1967;
            }
          }
          u_value t1968 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t1968;
        }
      }
    }
    if (!d1962) {
      d1962 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t1972 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t1972)) {
          t1972 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t1972)) {
          {
            u_value t1973 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t1973;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_1974 = U_LIT("u_mk_con");
              u_value t1975 = f_c_46_c_call_2(&l_u_mk_con_1974, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t1975;
            }
          } else {
            {
              u_value s1976 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d1977 = 0;
              if ((!d1977) && u_is_con(s1976, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s1976, 0), 0);
                u_value v_k = u_field(u_field(s1976, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d1977 = 1;
                  {
                    u_value t1978 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t1978;
                  }
                }
              }
              if (!d1977) {
                d1977 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_1979 = U_LIT("u_apply");
                  u_value t1980 = f_c_46_c_call_2(&l_u_apply_1979, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t1980;
                }
              }
              if (!d1977) {
                u_no_case(s1976);
              }
            }
          }
        }
      }
    }
    if (!d1962) {
      u_no_case(s1961);
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
    u_value t1981 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1982 = &t1981;
    u_value l1983 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l1983, &n_list_46_Cons)) {
      u_value v_x = u_field(l1983, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e1982 = u_append(e1982, v_x);
      }
      l1983 = u_field(l1983, 1);
    }
    u_value t1984 = t1981;
    v_caps = t1984;
    static struct u_object l_lam_1985 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_1985, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_1986 = U_LIT("env");
    static struct u_object l_args_1987 = U_LIT("args");
    u_value t1988 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_1986), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_1987));
    v_decls = t1988;
    static struct u_object l_env_1989 = U_LIT("env");
    static struct u_object l_args_1990 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_1989), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_1990), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_1991 = U_LIT("u_mk_closure");
    u_value t1992 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1993 = &t1992;
    u_value l1994 = v_caps;
    while (u_is_con(l1994, &n_list_46_Cons)) {
      u_value v_x = u_field(l1994, 0);
      if (u_truth(u_mk_bool(1))) {
        e1993 = u_append(e1993, f_compile_46_var_expr_2(v_env, v_x));
      }
      l1994 = u_field(l1994, 1);
    }
    u_value t1995 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1996 = f_c_46_c_call_2(&l_u_mk_closure_1991, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t1992));
    *r_ss = t1995;
    *r_r = t1996;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s1997 = v_xs;
  u_value t1998 = NULL;
  if ((!t1998) && u_is_con(s1997, &n_list_46_Nil)) {
    t1998 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1998) && u_is_con(s1997, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s1997, 0), 0);
    u_value v_i = u_field(u_field(s1997, 0), 1);
    u_value v_rest = u_field(s1997, 1);
    t1998 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t1998) {
    u_no_case(s1997);
  }
  return t1998;
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
    u_value s1999 = v_cases;
    int d2000 = 0;
    if ((!d2000) && u_is_con(s1999, &n_list_46_Nil)) {
      d2000 = 1;
      {
        u_value t2001 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2001;
      }
    }
    if ((!d2000) && (u_is_con(s1999, &n_list_46_Cons) && u_is_con(u_field(s1999, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1999, 0), 0);
      u_value v_guard = u_field(u_field(s1999, 0), 1);
      u_value v_e = u_field(u_field(s1999, 0), 2);
      u_value v_more = u_field(s1999, 1);
      d2000 = 1;
      {
        u_value t2002 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2002;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2003 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2003;
        u_value s2004 = v_guard;
        int d2005 = 0;
        if ((!d2005) && u_is_con(s2004, &n_option_46_Some)) {
          u_value v_c = u_field(s2004, 0);
          d2005 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2006 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2006;
          }
        }
        if ((!d2005) && u_is_con(s2004, &n_option_46_None)) {
          d2005 = 1;
          {
          }
        }
        if (!d2005) {
          u_no_case(s2004);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2007 = U_LIT("!");
        u_value t2008 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2007, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2008;
      }
    }
    if (!d2000) {
      u_no_case(s1999);
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
    u_value s2009 = u_mk_tuple(2, v_xs, v_lists);
    int d2010 = 0;
    if ((!d2010) && (u_is_con(u_field(s2009, 0), &n_list_46_Cons) && u_is_con(u_field(s2009, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2009, 0), 0);
      u_value v_more = u_field(u_field(s2009, 0), 1);
      u_value v_list = u_field(u_field(s2009, 1), 0);
      u_value v_others = u_field(u_field(s2009, 1), 1);
      d2010 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2011 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2011, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2012 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2012;
        static struct u_object l_u_is_con_2013 = U_LIT("u_is_con");
        u_value t2014 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2013, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2014;
      }
    }
    if (!d2010) {
      d2010 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2015 = U_LIT("u_append");
        u_value t2016 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2015, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2016;
      }
    }
    if (!d2010) {
      u_no_case(s2009);
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
    u_value s2017 = v_s;
    int d2018 = 0;
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SSkip)) {
      d2018 = 1;
      {
        u_value t2019 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2019;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2017, 0);
      d2018 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2020 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2020;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2017, 0);
      u_value v_body = u_field(s2017, 1);
      d2018 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2021 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2022 = &t2021;
        u_value l2023 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2023, &n_list_46_Cons)) {
          u_value v_x = u_field(l2023, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2024 = U_LIT("NULL");
            e2022 = u_append(e2022, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2024))));
          }
          l2023 = u_field(l2023, 1);
        }
        u_value t2025 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2021, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2025;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2017, 0);
      u_value v_l = u_field(s2017, 1);
      u_value v_r = u_field(s2017, 2);
      d2018 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2026 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2026;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2017, 0);
      u_value v_b = u_field(s2017, 1);
      d2018 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2027 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2027, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2028 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2028;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2017, 0);
      u_value v_ins = u_field(s2017, 1);
      u_value v_outs = u_field(s2017, 2);
      u_value v_refs = u_field(s2017, 3);
      d2018 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2029 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2030 = &t2029;
        u_value l2031 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2031, &n_list_46_Cons)) {
          u_value v_x = u_field(l2031, 0);
          if (u_truth(u_mk_bool(1))) {
            e2030 = u_append(e2030, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2031 = u_field(l2031, 1);
        }
        u_value t2032 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2029))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2032;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2017, 0);
      u_value v_cases = u_field(s2017, 1);
      d2018 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2033 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2033, &v_d, r_g);
        static struct u_object l_d_2034 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2034, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2035 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2035;
        static struct u_object l__33__2036 = U_LIT("!");
        static struct u_object l_u_no_case_2037 = U_LIT("u_no_case");
        u_value t2038 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2036, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2037, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2038;
      }
    }
    if ((!d2018) && (u_is_con(s2017, &n_syntax_46_SAssert) && (u_is_con(u_field(s2017, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2017, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2017, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2017, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2017, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2017, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2017, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2017, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2017, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2017, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2017, 0), 1), 1), 0);
      d2018 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2039 = U_LIT("u_assert_equal");
        u_value t2040 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2039, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2040;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2017, 0);
      d2018 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2041 = U_LIT("u_assert_true");
        u_value t2042 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2041, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2042;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2017, 0);
      u_value v_ty = u_field(s2017, 1);
      d2018 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2043 = U_LIT("u_raise");
        u_value t2044 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2043, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2044;
      }
    }
    if ((!d2018) && u_is_con(s2017, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2017, 0);
      u_value v_hs = u_field(s2017, 1);
      d2018 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2045 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2045, &v_d, r_g);
        static struct u_object l_d_2046 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2046, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2047 = U_LIT("u_pop");
        static struct u_object l__33__2048 = U_LIT("!");
        static struct u_object l_u_reraise_2049 = U_LIT("u_reraise");
        u_value t2050 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2047, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2048, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2049, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2050;
        static struct u_object l_struct_32_u_handler_2051 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2052 = U_LIT("u_push");
        static struct u_object l__61__61__2053 = U_LIT("==");
        static struct u_object l_setjmp_2054 = U_LIT("setjmp");
        static struct u_object l_env_2055 = U_LIT("env");
        static struct u_object l_u_pop_2056 = U_LIT("u_pop");
        u_value t2057 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2051)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2052, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2053, f_c_46_c_call_2(&l_setjmp_2054, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2055), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2056, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2057;
      }
    }
    if (!d2018) {
      u_no_case(s2017);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2058 = v_xs;
    int d2059 = 0;
    if ((!d2059) && u_is_con(s2058, &n_list_46_Nil)) {
      d2059 = 1;
      {
        u_value t2060 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2060;
      }
    }
    if ((!d2059) && u_is_con(s2058, &n_list_46_Cons)) {
      u_value v_x = u_field(s2058, 0);
      u_value v_rest = u_field(s2058, 1);
      d2059 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2061 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2061;
      }
    }
    if (!d2059) {
      u_no_case(s2058);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2062 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2063 = NULL;
  if ((!t2063) && (u_is_con(u_field(s2062, 0), &n_list_46_Cons) && (u_is_con(u_field(s2062, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2062, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2062, 0), 0);
    u_value v_more = u_field(u_field(s2062, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2062, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2062, 1), 1);
    t2063 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2063) {
    t2063 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2063) {
    u_no_case(s2062);
  }
  return t2063;
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
    static struct u_object l_u_caught_2064 = U_LIT("u_caught");
    u_value t2065 = f_c_46_c_call_2(&l_u_caught_2064, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2065;
    u_value s2066 = v_hs;
    int d2067 = 0;
    if ((!d2067) && u_is_con(s2066, &n_list_46_Nil)) {
      d2067 = 1;
      {
        u_value t2068 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2068;
      }
    }
    if ((!d2067) && (u_is_con(s2066, &n_list_46_Cons) && u_is_con(u_field(s2066, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2066, 0), 0);
      u_value v_p = u_field(u_field(s2066, 0), 1);
      u_value v_guard = u_field(u_field(s2066, 0), 2);
      u_value v_s = u_field(u_field(s2066, 0), 3);
      u_value v_more = u_field(s2066, 1);
      d2067 = 1;
      {
        u_value t2069 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2069;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2070 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2070;
        u_value s2071 = v_guard;
        int d2072 = 0;
        if ((!d2072) && u_is_con(s2071, &n_option_46_Some)) {
          u_value v_c = u_field(s2071, 0);
          d2072 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2073 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2073;
          }
        }
        if ((!d2072) && u_is_con(s2071, &n_option_46_None)) {
          d2072 = 1;
          {
          }
        }
        if (!d2072) {
          u_no_case(s2071);
        }
        u_value s2074 = v_p;
        int d2075 = 0;
        if ((!d2075) && u_is_con(s2074, &n_syntax_46_PWild)) {
          d2075 = 1;
          {
            u_value t2076 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2076;
          }
        }
        if (!d2075) {
          d2075 = 1;
          {
            static struct u_object l_u_has_type_2077 = U_LIT("u_has_type");
            u_value t2078 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2077, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2078;
          }
        }
        if (!d2075) {
          u_no_case(s2074);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2079 = U_LIT("!");
        u_value t2080 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2079, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2080;
      }
    }
    if (!d2067) {
      u_no_case(s2066);
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
    u_value s2081 = v_cases;
    int d2082 = 0;
    if ((!d2082) && u_is_con(s2081, &n_list_46_Nil)) {
      d2082 = 1;
      {
        u_value t2083 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2083;
      }
    }
    if ((!d2082) && (u_is_con(s2081, &n_list_46_Cons) && u_is_con(u_field(s2081, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2081, 0), 0);
      u_value v_guard = u_field(u_field(s2081, 0), 1);
      u_value v_s = u_field(u_field(s2081, 0), 2);
      u_value v_more = u_field(s2081, 1);
      d2082 = 1;
      {
        u_value t2084 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2084;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2085 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2085;
        u_value s2086 = v_guard;
        int d2087 = 0;
        if ((!d2087) && u_is_con(s2086, &n_option_46_Some)) {
          u_value v_c = u_field(s2086, 0);
          d2087 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2088 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2088;
          }
        }
        if ((!d2087) && u_is_con(s2086, &n_option_46_None)) {
          d2087 = 1;
          {
          }
        }
        if (!d2087) {
          u_no_case(s2086);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2089 = U_LIT("!");
        u_value t2090 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2089, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2090;
      }
    }
    if (!d2082) {
      u_no_case(s2081);
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
    u_value s2091 = v_c;
    int d2092 = 0;
    if ((!d2092) && u_is_con(s2091, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2091, 0);
      u_value v_xss = u_field(s2091, 2);
      u_value v_b = u_field(s2091, 4);
      d2092 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2093 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2094 = &t2093;
        u_value l2095 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2095, &n_list_46_Cons)) {
          u_value v_x = u_field(l2095, 0);
          if (u_truth(u_mk_bool(1))) {
            e2094 = u_append(e2094, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2095 = u_field(l2095, 1);
        }
        u_value t2096 = t2093;
        v_params = t2096;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2097 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2097;
          }
        } else {
          {
            u_value t2098 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2098;
          }
        }
        u_value t2099 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2100 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2099;
        *r_ms = t2100;
      }
    }
    if ((!d2092) && u_is_con(s2091, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2091, 0);
      u_value v_ins = u_field(s2091, 2);
      u_value v_outs = u_field(s2091, 3);
      u_value v_refs = u_field(s2091, 4);
      u_value v_body = u_field(s2091, 5);
      d2092 = 1;
      {
        u_value t2101 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2102 = &t2101;
        u_value l2103 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2103, &n_list_46_Cons)) {
          u_value v_x = u_field(l2103, 0);
          if (u_truth(u_mk_bool(1))) {
            e2102 = u_append(e2102, f_compile_46_pointer_1(v_x));
          }
          l2103 = u_field(l2103, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2101), v_body, &v_bs, r_g);
        u_value t2104 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2105 = &t2104;
        u_value l2106 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2106, &n_list_46_Cons)) {
          u_value v_x = u_field(l2106, 0);
          if (u_truth(u_mk_bool(1))) {
            e2105 = u_append(e2105, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2106 = u_field(l2106, 1);
        }
        u_value t2107 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2108 = &t2107;
        u_value l2109 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2109, &n_list_46_Cons)) {
          u_value v_x = u_field(l2109, 0);
          if (u_truth(u_mk_bool(1))) {
            e2108 = u_append(e2108, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2109 = u_field(l2109, 1);
        }
        u_value t2110 = f_list_46__43__43__2(t2104, t2107);
        v_params = t2110;
        u_value t2111 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2112 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2111;
        *r_ms = t2112;
      }
    }
    if ((!d2092) && u_is_con(s2091, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2091, 0);
      d2092 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2113 = U_LIT("u_print_line");
        u_value t2114 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2115 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2113, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2114;
        *r_ms = t2115;
      }
    }
    if ((!d2092) && (u_is_con(s2091, &n_syntax_46_CRun) && u_is_con(u_field(s2091, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2091, 1), 0);
      u_value v_body = u_field(u_field(s2091, 1), 1);
      d2092 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2116 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2117 = &t2116;
        u_value l2118 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2118, &n_list_46_Cons)) {
          u_value v_x = u_field(l2118, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2119 = U_LIT("NULL");
            e2117 = u_append(e2117, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2119))));
          }
          l2118 = u_field(l2118, 1);
        }
        u_value t2120 = t2116;
        v_copy = t2120;
        u_value t2121 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2122 = &t2121;
        u_value l2123 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2123, &n_list_46_Cons)) {
          u_value v_x = u_field(l2123, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2124 = U_LIT("u_print_binding");
            e2122 = u_append(e2122, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2124, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2123 = u_field(l2123, 1);
        }
        u_value t2125 = t2121;
        v_back = t2125;
        u_value t2126 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2127 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2126;
        *r_ms = t2127;
      }
    }
    if ((!d2092) && u_is_con(s2091, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2091, 1);
      d2092 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2128 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2129 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2128;
        *r_ms = t2129;
      }
    }
    if ((!d2092) && u_is_con(s2091, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2091, 1);
      d2092 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2130 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2131 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2130;
        *r_ms = t2131;
      }
    }
    if (!d2092) {
      d2092 = 1;
      {
        u_value t2132 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2133 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2132;
        *r_ms = t2133;
      }
    }
    if (!d2092) {
      u_no_case(s2091);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2134 = v_cs;
    int d2135 = 0;
    if ((!d2135) && u_is_con(s2134, &n_list_46_Nil)) {
      d2135 = 1;
      {
        u_value t2136 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2137 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2136;
        *r_ms = t2137;
      }
    }
    if ((!d2135) && u_is_con(s2134, &n_list_46_Cons)) {
      u_value v_c = u_field(s2134, 0);
      u_value v_rest = u_field(s2134, 1);
      d2135 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2138 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2139 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2138;
        *r_ms = t2139;
      }
    }
    if (!d2135) {
      u_no_case(s2134);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_static_32_u_value_2140 = U_LIT("static u_value");
  static struct u_object l_cache_2141 = U_LIT("cache");
  static struct u_object l_NULL_2142 = U_LIT("NULL");
  static struct u_object l__61__61__2143 = U_LIT("==");
  static struct u_object l_cache_2144 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2145 = U_LIT("U_COMPUTING");
  static struct u_object l_u_fail_2146 = U_LIT("u_fail");
  static struct u_object l_recursive_32_initia_2147 = U_LIT("recursive initialization of ");
  static struct u_object l_cache_2148 = U_LIT("cache");
  static struct u_object l_cache_2149 = U_LIT("cache");
  static struct u_object l_cache_2150 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2151 = U_LIT("U_COMPUTING");
  static struct u_object l_cache_2152 = U_LIT("cache");
  static struct u_object l_cache_2153 = U_LIT("cache");
  return f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2140), &l_cache_2141, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2142))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2143, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2144), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2145)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2146, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2147, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2148), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2149))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2150), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2151)), u_mk_con(&n_list_46_Nil, 0))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2152), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2153))), u_mk_con(&n_list_46_Nil, 0)))));
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2154 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2155 = &t2154;
  u_value l2156 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2156, &n_list_46_Cons)) {
    u_value v_p = u_field(l2156, 0);
    if (u_truth(u_mk_bool(1))) {
      e2155 = u_append(e2155, f_compile_46_fun_wrapper_1(v_p));
    }
    l2156 = u_field(l2156, 1);
  }
  u_value t2157 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2158 = &t2157;
  u_value l2159 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2159, &n_list_46_Cons)) {
    u_value v_p = u_field(l2159, 0);
    if (u_truth(u_mk_bool(1))) {
      e2158 = u_append(e2158, f_compile_46_constr_wrapper_1(v_p));
    }
    l2159 = u_field(l2159, 1);
  }
  u_value t2160 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2161 = &t2160;
  u_value l2162 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2162, &n_list_46_Cons)) {
    u_value v_p = u_field(l2162, 0);
    if (u_truth(u_mk_bool(1))) {
      e2161 = u_append(e2161, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2162 = u_field(l2162, 1);
  }
  return f_list_46__43__43__2(t2154, f_list_46__43__43__2(f_list_46_concat_1(t2157), t2160));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2163 = v_p;
  u_value t2164 = NULL;
  if (!t2164) {
    u_value v_n = u_field(s2163, 0);
    u_value v_k = u_field(s2163, 1);
    static struct u_object l_w__2165 = U_LIT("w_");
    t2164 = f_compile_46_wrapper_3(u_concat(&l_w__2165, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2164) {
    u_no_case(s2163);
  }
  return t2164;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2166 = v_p;
  u_value t2167 = NULL;
  if (!t2167) {
    u_value v_f = u_field(s2166, 0);
    u_value v_k = u_field(s2166, 1);
    static struct u_object l_w_2168 = U_LIT("w");
    t2167 = f_compile_46_wrapper_3(u_concat(&l_w_2168, v_f), v_f, v_k);
  }
  if (!t2167) {
    u_no_case(s2166);
  }
  return t2167;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2169 = U_LIT("env");
  static struct u_object l_args_2170 = U_LIT("args");
  u_value t2171 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2172 = &t2171;
  u_value l2173 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2173, &n_list_46_Cons)) {
    u_value v_i = u_field(l2173, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2174 = U_LIT("args");
      e2172 = u_append(e2172, f_compile_46_index_2(&l_args_2174, v_i));
    }
    l2173 = u_field(l2173, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2169), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2170), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2171))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2175 = v_p;
  u_value t2176 = NULL;
  if ((!t2176) && u_equals(u_field(s2175, 1), u_mk_int(0))) {
    t2176 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2176) {
    u_value v_n = u_field(s2175, 0);
    u_value v_k = u_field(s2175, 1);
    static struct u_object l_wc__2177 = U_LIT("wc_");
    static struct u_object l_env_2178 = U_LIT("env");
    static struct u_object l_args_2179 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2180 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2181 = U_LIT("args");
    t2176 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2177, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2178), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2179), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2180, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2181), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2176) {
    u_no_case(s2175);
  }
  return t2176;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2182;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2182 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2182 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2182;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2183 = v_d;
  u_value t2184 = NULL;
  if ((!t2184) && u_is_con(s2183, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2183, 0);
    u_value v_f = u_field(s2183, 1);
    u_value v_ps = u_field(s2183, 2);
    t2184 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2184) {
    t2184 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2184) {
    u_no_case(s2183);
  }
  return t2184;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2185 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2185;
    u_value t2186 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2186;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2187 = v_g;
    int d2188 = 0;
    if ((!d2188) && u_is_con(s2187, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2187, 1);
      d2188 = 1;
      {
        u_value t2189 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2189;
      }
    }
    if (!d2188) {
      u_no_case(s2187);
    }
    static struct u_object l_ulang_46_h_2190 = U_LIT("ulang.h");
    u_value t2191 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2192 = &t2191;
    u_value l2193 = v_fs;
    while (u_is_con(l2193, &n_list_46_Cons)) {
      u_value v_d = u_field(l2193, 0);
      if (u_truth(u_mk_bool(1))) {
        e2192 = u_append(e2192, f_compile_46_prototype_1(v_d));
      }
      l2193 = u_field(l2193, 1);
    }
    static struct u_object l_main_2194 = U_LIT("main");
    static struct u_object l_argc_2195 = U_LIT("argc");
    static struct u_object l_char_2196 = U_LIT("char");
    static struct u_object l_argv_2197 = U_LIT("argv");
    static struct u_object l_u_args_2198 = U_LIT("u_args");
    static struct u_object l_argc_2199 = U_LIT("argc");
    static struct u_object l_argv_2200 = U_LIT("argv");
    u_value t2201 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2190), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2191), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2194, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2195), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2196))), &l_argv_2197), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2198, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2199), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2200), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2201;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2202 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2202, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2203 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2203, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam993(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2204 = v_args;
    int d2205 = 0;
    if ((!d2205) && u_is_con(s2204, &n_list_46_Nil)) {
      d2205 = 1;
      {
      }
    }
    if ((!d2205) && (u_is_con(s2204, &n_list_46_Cons) && (u_is_con(u_field(s2204, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2204, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2204, 0);
      u_value v_target = u_field(u_field(s2204, 1), 0);
      d2205 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2205) {
      d2205 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2206 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2206), "os.OsError");
      }
    }
    if (!d2205) {
      u_no_case(s2204);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
