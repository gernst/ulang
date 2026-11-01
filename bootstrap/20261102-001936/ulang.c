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

const struct u_name n_check_46_CountMismatch = {"check", "CountMismatch", 13};

const struct u_name n_check_46_UnknownConstructor = {"check", "UnknownConstructor", 14};

const struct u_name n_check_46_NotExhaustive = {"check", "NotExhaustive", 15};

const struct u_name n_check_46_Unreachable = {"check", "Unreachable", 16};

const struct u_name n_check_46_NotAssignable = {"check", "NotAssignable", 17};

const struct u_name n_check_46_UnknownProcedure = {"check", "UnknownProcedure", 18};

const struct u_name n_check_46_UnknownException = {"check", "UnknownException", 19};

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

u_value w_f_syntax_46_name_1(u_value *env, u_value *args);

u_value w_f_syntax_46_unqualified_1(u_value *env, u_value *args);

u_value w_f_syntax_46_base_1(u_value *env, u_value *args);

u_value w_f_syntax_46_qualified_1(u_value *env, u_value *args);

u_value w_f_syntax_46_list_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_nil_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_cons_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_option_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_none_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_some_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_os_error_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_tuple_name_0(u_value *env, u_value *args);

u_value w_f_syntax_46_typ_1(u_value *env, u_value *args);

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

u_value w_f_ctx_46_named_2(u_value *env, u_value *args);

u_value w_f_ctx_46_with_named_2(u_value *env, u_value *args);

u_value w_f_ctx_46_with_alias_4(u_value *env, u_value *args);

u_value w_f_ctx_46_with_fun_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_proc_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_data_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_members_3(u_value *env, u_value *args);

u_value w_f_ctx_46_with_constr_3(u_value *env, u_value *args);

u_value w_f_check_46_ctx_of_1(u_value *env, u_value *args);

u_value w_f_check_46_sub_of_1(u_value *env, u_value *args);

u_value w_f_check_46_module_of_1(u_value *env, u_value *args);

u_value w_f_check_46_visible_of_1(u_value *env, u_value *args);

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

u_value w_f_check_46_os_procs_1(u_value *env, u_value *args);

u_value w_f_check_46_syntax_procs_1(u_value *env, u_value *args);

u_value w_f_check_46_constr_name_1(u_value *env, u_value *args);

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

u_value w_f_parse_46_scope_params_1(u_value *env, u_value *args);

u_value w_f_parse_46_scope_notations_1(u_value *env, u_value *args);

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

u_value w_f_parse_46_ops_notations_1(u_value *env, u_value *args);

u_value w_f_parse_46_apply_prefix_3(u_value *env, u_value *args);

u_value w_f_parse_46_apply_postfix_3(u_value *env, u_value *args);

u_value w_f_parse_46_apply_infix_4(u_value *env, u_value *args);

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

u_value w_f_parse_46_loaded_modules_1(u_value *env, u_value *args);

u_value w_f_parse_46_loading_1(u_value *env, u_value *args);

u_value w_f_parse_46_base_notations_1(u_value *env, u_value *args);

u_value w_f_parse_46_directory_1(u_value *env, u_value *args);

u_value w_f_parse_46_modules_of_1(u_value *env, u_value *args);

u_value w_f_parse_46_own_notations_2(u_value *env, u_value *args);

u_value w_f_parse_46_last_part_1(u_value *env, u_value *args);

u_value w_f_parse_46_base_name_1(u_value *env, u_value *args);

u_value w_f_parse_46_dir_name_1(u_value *env, u_value *args);

u_value w_f_parse_46_commands_of_1(u_value *env, u_value *args);

u_value w_f_compile_46_value_type_0(u_value *env, u_value *args);

u_value w_f_compile_46_value_ptr_0(u_value *env, u_value *args);

u_value w_f_compile_46_mangle_1(u_value *env, u_value *args);

u_value w_f_compile_46_mangle_char_1(u_value *env, u_value *args);

u_value w_f_compile_46_fun_name_2(u_value *env, u_value *args);

u_value w_f_compile_46_proc_name_1(u_value *env, u_value *args);

u_value w_f_compile_46_var_name_1(u_value *env, u_value *args);

u_value w_f_compile_46_ref_name_1(u_value *env, u_value *args);

u_value w_f_compile_46_builtin_functions_0(u_value *env, u_value *args);

u_value w_f_compile_46_funs_of_1(u_value *env, u_value *args);

u_value w_f_compile_46_constrs_of_1(u_value *env, u_value *args);

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

u_value w_f_compile_46_program_cmds_1(u_value *env, u_value *args);

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

u_value f_syntax_46_name_1(u_value v_x);

u_value f_syntax_46_unqualified_1(u_value v_x);

u_value f_syntax_46_base_1(u_value v_n);

u_value f_syntax_46_qualified_1(u_value v_n);

u_value f_syntax_46_list_name_0(void);

u_value f_syntax_46_nil_name_0(void);

u_value f_syntax_46_cons_name_0(void);

u_value f_syntax_46_option_name_0(void);

u_value f_syntax_46_none_name_0(void);

u_value f_syntax_46_some_name_0(void);

u_value f_syntax_46_os_error_name_0(void);

u_value f_syntax_46_tuple_name_0(void);

u_value f_syntax_46_typ_1(u_value v_x);

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

u_value f_ctx_46_named_2(u_value v_c, u_value v_b);

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n);

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t);

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc);

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g);

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity);

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs);

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc);

u_value f_check_46_ctx_of_1(u_value v_s);

u_value f_check_46_sub_of_1(u_value v_s);

u_value f_check_46_module_of_1(u_value v_s);

u_value f_check_46_visible_of_1(u_value v_s);

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

u_value f_check_46_constr_name_1(u_value v_k);

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

u_value f_parse_46_scope_params_1(u_value v_sc);

u_value f_parse_46_scope_notations_1(u_value v_sc);

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

u_value f_parse_46_ops_notations_1(u_value v_o);

u_value f_parse_46_apply_prefix_3(u_value v_o, u_value v_op, u_value v_a);

u_value f_parse_46_apply_postfix_3(u_value v_o, u_value v_op, u_value v_a);

u_value f_parse_46_apply_infix_4(u_value v_o, u_value v_op, u_value v_a, u_value v_b);

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

u_value f_parse_46_loaded_modules_1(u_value v_ld);

u_value f_parse_46_loading_1(u_value v_ld);

u_value f_parse_46_base_notations_1(u_value v_ld);

u_value f_parse_46_directory_1(u_value v_ld);

u_value f_parse_46_modules_of_1(u_value v_c);

u_value f_parse_46_own_notations_2(u_value v_ld, u_value v_n);

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts);

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts);

void p_parse_46_constructor(u_value v_sc, u_value *r_k, u_value *r_ts);

void p_parse_46_parse_data(u_value v_ns, u_value *r_c, u_value *r_ts);

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

u_value f_parse_46_commands_of_1(u_value v_p);

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds);

void p_parse_46_same_parses(u_value v_paths, u_value v_pre);

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre);

u_value f_compile_46_value_type_0(void);

u_value f_compile_46_value_ptr_0(void);

u_value f_compile_46_mangle_1(u_value v_n);

u_value f_compile_46_mangle_char_1(u_value v_c);

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k);

u_value f_compile_46_proc_name_1(u_value v_n);

u_value f_compile_46_var_name_1(u_value v_n);

u_value f_compile_46_ref_name_1(u_value v_n);

u_value f_compile_46_builtin_functions_0(void);

u_value f_compile_46_funs_of_1(u_value v_gl);

u_value f_compile_46_constrs_of_1(u_value v_gl);

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

u_value f_compile_46_program_cmds_1(u_value v_p);

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

u_value lam955(u_value *env, u_value *args);

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

u_value w_f_syntax_46_name_1(u_value *env, u_value *args) {
  return f_syntax_46_name_1(args[0]);
}

u_value w_f_syntax_46_unqualified_1(u_value *env, u_value *args) {
  return f_syntax_46_unqualified_1(args[0]);
}

u_value w_f_syntax_46_base_1(u_value *env, u_value *args) {
  return f_syntax_46_base_1(args[0]);
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

u_value w_f_syntax_46_typ_1(u_value *env, u_value *args) {
  return f_syntax_46_typ_1(args[0]);
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

u_value w_f_ctx_46_named_2(u_value *env, u_value *args) {
  return f_ctx_46_named_2(args[0], args[1]);
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

u_value w_f_check_46_ctx_of_1(u_value *env, u_value *args) {
  return f_check_46_ctx_of_1(args[0]);
}

u_value w_f_check_46_sub_of_1(u_value *env, u_value *args) {
  return f_check_46_sub_of_1(args[0]);
}

u_value w_f_check_46_module_of_1(u_value *env, u_value *args) {
  return f_check_46_module_of_1(args[0]);
}

u_value w_f_check_46_visible_of_1(u_value *env, u_value *args) {
  return f_check_46_visible_of_1(args[0]);
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

u_value w_f_check_46_os_procs_1(u_value *env, u_value *args) {
  return f_check_46_os_procs_1(args[0]);
}

u_value w_f_check_46_syntax_procs_1(u_value *env, u_value *args) {
  return f_check_46_syntax_procs_1(args[0]);
}

u_value w_f_check_46_constr_name_1(u_value *env, u_value *args) {
  return f_check_46_constr_name_1(args[0]);
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

u_value w_f_parse_46_scope_params_1(u_value *env, u_value *args) {
  return f_parse_46_scope_params_1(args[0]);
}

u_value w_f_parse_46_scope_notations_1(u_value *env, u_value *args) {
  return f_parse_46_scope_notations_1(args[0]);
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

u_value w_f_parse_46_ops_notations_1(u_value *env, u_value *args) {
  return f_parse_46_ops_notations_1(args[0]);
}

u_value w_f_parse_46_apply_prefix_3(u_value *env, u_value *args) {
  return f_parse_46_apply_prefix_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_apply_postfix_3(u_value *env, u_value *args) {
  return f_parse_46_apply_postfix_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_apply_infix_4(u_value *env, u_value *args) {
  return f_parse_46_apply_infix_4(args[0], args[1], args[2], args[3]);
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

u_value w_f_parse_46_loaded_modules_1(u_value *env, u_value *args) {
  return f_parse_46_loaded_modules_1(args[0]);
}

u_value w_f_parse_46_loading_1(u_value *env, u_value *args) {
  return f_parse_46_loading_1(args[0]);
}

u_value w_f_parse_46_base_notations_1(u_value *env, u_value *args) {
  return f_parse_46_base_notations_1(args[0]);
}

u_value w_f_parse_46_directory_1(u_value *env, u_value *args) {
  return f_parse_46_directory_1(args[0]);
}

u_value w_f_parse_46_modules_of_1(u_value *env, u_value *args) {
  return f_parse_46_modules_of_1(args[0]);
}

u_value w_f_parse_46_own_notations_2(u_value *env, u_value *args) {
  return f_parse_46_own_notations_2(args[0], args[1]);
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

u_value w_f_parse_46_commands_of_1(u_value *env, u_value *args) {
  return f_parse_46_commands_of_1(args[0]);
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

u_value w_f_compile_46_funs_of_1(u_value *env, u_value *args) {
  return f_compile_46_funs_of_1(args[0]);
}

u_value w_f_compile_46_constrs_of_1(u_value *env, u_value *args) {
  return f_compile_46_constrs_of_1(args[0]);
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

u_value w_f_compile_46_program_cmds_1(u_value *env, u_value *args) {
  return f_compile_46_program_cmds_1(args[0]);
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

u_value f_syntax_46_name_1(u_value v_x) {
  u_value s57 = v_x;
  u_value t58 = NULL;
  if ((!t58) && u_is_con(s57, &n_syntax_46_Var)) {
    u_value v_n = u_field(s57, 0);
    t58 = v_n;
  }
  if (!t58) {
    u_no_case(s57);
  }
  return t58;
}

u_value f_syntax_46_unqualified_1(u_value v_x) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_None, 0), v_x);
}

u_value f_syntax_46_base_1(u_value v_n) {
  u_value s59 = v_n;
  u_value t60 = NULL;
  if ((!t60) && u_is_con(s59, &n_syntax_46_Name)) {
    u_value v_b = u_field(s59, 1);
    t60 = v_b;
  }
  if (!t60) {
    u_no_case(s59);
  }
  return t60;
}

u_value f_syntax_46_qualified_1(u_value v_n) {
  u_value s61 = v_n;
  u_value t62 = NULL;
  if ((!t62) && (u_is_con(s61, &n_syntax_46_Name) && u_is_con(u_field(s61, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s61, 0), 0);
    u_value v_b = u_field(s61, 1);
    static struct u_object l__46__63 = U_LIT(".");
    t62 = u_concat(v_m, u_concat(&l__46__63, v_b));
  }
  if ((!t62) && (u_is_con(s61, &n_syntax_46_Name) && u_is_con(u_field(s61, 0), &n_option_46_None))) {
    u_value v_b = u_field(s61, 1);
    t62 = v_b;
  }
  if (!t62) {
    u_no_case(s61);
  }
  return t62;
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
  static struct u_object l_list_64 = U_LIT("list");
  static struct u_object l_List_65 = U_LIT("List");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_64), &l_List_65);
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
  static struct u_object l_list_66 = U_LIT("list");
  static struct u_object l_Nil_67 = U_LIT("Nil");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_66), &l_Nil_67);
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
  static struct u_object l_list_68 = U_LIT("list");
  static struct u_object l_Cons_69 = U_LIT("Cons");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_list_68), &l_Cons_69);
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
  static struct u_object l_option_70 = U_LIT("option");
  static struct u_object l_Option_71 = U_LIT("Option");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_70), &l_Option_71);
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
  static struct u_object l_option_72 = U_LIT("option");
  static struct u_object l_None_73 = U_LIT("None");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_72), &l_None_73);
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
  static struct u_object l_option_74 = U_LIT("option");
  static struct u_object l_Some_75 = U_LIT("Some");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_option_74), &l_Some_75);
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
  static struct u_object l_os_76 = U_LIT("os");
  static struct u_object l_OsError_77 = U_LIT("OsError");
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_os_76), &l_OsError_77);
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
  static struct u_object l__44__78 = U_LIT(",");
  cache = f_syntax_46_unqualified_1(&l__44__78);
  return cache;
}

u_value f_syntax_46_typ_1(u_value v_x) {
  u_value s79 = v_x;
  u_value t80 = NULL;
  if ((!t80) && u_is_con(s79, &n_syntax_46_Var)) {
    u_value v_t = u_field(s79, 1);
    t80 = v_t;
  }
  if (!t80) {
    u_no_case(s79);
  }
  return t80;
}

u_value f_syntax_46_names_1(u_value v_xs) {
  u_value t81 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e82 = &t81;
  u_value l83 = v_xs;
  while (u_is_con(l83, &n_list_46_Cons)) {
    u_value v_x = u_field(l83, 0);
    if (u_truth(u_mk_bool(1))) {
      e82 = u_append(e82, f_syntax_46_name_1(v_x));
    }
    l83 = u_field(l83, 1);
  }
  return t81;
}

u_value f_syntax_46_types_1(u_value v_xs) {
  u_value t84 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e85 = &t84;
  u_value l86 = v_xs;
  while (u_is_con(l86, &n_list_46_Cons)) {
    u_value v_x = u_field(l86, 0);
    if (u_truth(u_mk_bool(1))) {
      e85 = u_append(e85, f_syntax_46_typ_1(v_x));
    }
    l86 = u_field(l86, 1);
  }
  return t84;
}

u_value f_syntax_46_arity_1(u_value v_t) {
  u_value s87 = v_t;
  u_value t88 = NULL;
  if ((!t88) && u_is_con(s87, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s87, 0);
    t88 = f_list_46_length_1(v_args);
  }
  if (!t88) {
    t88 = f_prelude_46__45__1(u_mk_int(1));
  }
  if (!t88) {
    u_no_case(s87);
  }
  return t88;
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
  u_value s89 = v_m;
  u_value t90 = NULL;
  if ((!t90) && u_is_con(s89, &n_map_46_MLeaf)) {
    t90 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t90) && u_is_con(s89, &n_map_46_MNode)) {
    u_value v_l = u_field(s89, 0);
    u_value v_j = u_field(s89, 1);
    u_value v_v = u_field(s89, 2);
    u_value v_r = u_field(s89, 3);
    u_value t94 = u_compare(v_k, v_j);
    u_value t93;
    {
      u_value v_c = t94;
      u_value t92;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t92 = f_map_46_get_2(v_k, v_l);
      } else {
        u_value t91;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t91 = f_map_46_get_2(v_k, v_r);
        } else {
          t91 = u_mk_con(&n_option_46_Some, 1, v_v);
        }
        t92 = t91;
      }
      t93 = t92;
    }
    t90 = t93;
  }
  if (!t90) {
    u_no_case(s89);
  }
  return t90;
}

u_value f_map_46_has_2(u_value v_k, u_value v_m) {
  return f_prelude_46__33__61__2(f_map_46_get_2(v_k, v_m), u_mk_con(&n_option_46_None, 0));
}

u_value f_map_46_put_3(u_value v_k, u_value v_v, u_value v_m) {
  u_value s95 = v_m;
  u_value t96 = NULL;
  if ((!t96) && u_is_con(s95, &n_map_46_MLeaf)) {
    t96 = u_mk_con(&n_map_46_MNode, 5, u_mk_con(&n_map_46_MLeaf, 0), v_k, v_v, u_mk_con(&n_map_46_MLeaf, 0), u_mk_int(1));
  }
  if ((!t96) && u_is_con(s95, &n_map_46_MNode)) {
    u_value v_l = u_field(s95, 0);
    u_value v_j = u_field(s95, 1);
    u_value v_w = u_field(s95, 2);
    u_value v_r = u_field(s95, 3);
    u_value v_h = u_field(s95, 4);
    u_value t100 = u_compare(v_k, v_j);
    u_value t99;
    {
      u_value v_c = t100;
      u_value t98;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t98 = f_map_46_balance_4(f_map_46_put_3(v_k, v_v, v_l), v_j, v_w, v_r);
      } else {
        u_value t97;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t97 = f_map_46_balance_4(v_l, v_j, v_w, f_map_46_put_3(v_k, v_v, v_r));
        } else {
          t97 = u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, v_h);
        }
        t98 = t97;
      }
      t99 = t98;
    }
    t96 = t99;
  }
  if (!t96) {
    u_no_case(s95);
  }
  return t96;
}

u_value f_map_46_entries_1(u_value v_m) {
  return f_map_46_entries_onto_2(v_m, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest) {
  u_value s101 = v_m;
  u_value t102 = NULL;
  if ((!t102) && u_is_con(s101, &n_map_46_MLeaf)) {
    t102 = v_rest;
  }
  if ((!t102) && u_is_con(s101, &n_map_46_MNode)) {
    u_value v_l = u_field(s101, 0);
    u_value v_k = u_field(s101, 1);
    u_value v_v = u_field(s101, 2);
    u_value v_r = u_field(s101, 3);
    t102 = f_map_46_entries_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), f_map_46_entries_onto_2(v_r, v_rest)));
  }
  if (!t102) {
    u_no_case(s101);
  }
  return t102;
}

u_value f_map_46_size_1(u_value v_m) {
  u_value s103 = v_m;
  u_value t104 = NULL;
  if ((!t104) && u_is_con(s103, &n_map_46_MLeaf)) {
    t104 = u_mk_int(0);
  }
  if ((!t104) && u_is_con(s103, &n_map_46_MNode)) {
    u_value v_l = u_field(s103, 0);
    u_value v_r = u_field(s103, 3);
    t104 = u_add(u_add(f_map_46_size_1(v_l), u_mk_int(1)), f_map_46_size_1(v_r));
  }
  if (!t104) {
    u_no_case(s103);
  }
  return t104;
}

u_value f_map_46_height_1(u_value v_m) {
  u_value s105 = v_m;
  u_value t106 = NULL;
  if ((!t106) && u_is_con(s105, &n_map_46_MLeaf)) {
    t106 = u_mk_int(0);
  }
  if ((!t106) && u_is_con(s105, &n_map_46_MNode)) {
    u_value v_h = u_field(s105, 4);
    t106 = v_h;
  }
  if (!t106) {
    u_no_case(s105);
  }
  return t106;
}

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  return u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, f_map_46_mnode_height_2(v_l, v_r));
}

u_value f_map_46_rotate_left_1(u_value v_m) {
  u_value s107 = v_m;
  u_value t108 = NULL;
  if ((!t108) && (u_is_con(s107, &n_map_46_MNode) && u_is_con(u_field(s107, 3), &n_map_46_MNode))) {
    u_value v_a = u_field(s107, 0);
    u_value v_j = u_field(s107, 1);
    u_value v_v = u_field(s107, 2);
    u_value v_b = u_field(u_field(s107, 3), 0);
    u_value v_k = u_field(u_field(s107, 3), 1);
    u_value v_w = u_field(u_field(s107, 3), 2);
    u_value v_c = u_field(u_field(s107, 3), 3);
    t108 = f_map_46_mnode_4(f_map_46_mnode_4(v_a, v_j, v_v, v_b), v_k, v_w, v_c);
  }
  if (!t108) {
    t108 = v_m;
  }
  if (!t108) {
    u_no_case(s107);
  }
  return t108;
}

u_value f_map_46_rotate_right_1(u_value v_m) {
  u_value s109 = v_m;
  u_value t110 = NULL;
  if ((!t110) && (u_is_con(s109, &n_map_46_MNode) && u_is_con(u_field(s109, 0), &n_map_46_MNode))) {
    u_value v_a = u_field(u_field(s109, 0), 0);
    u_value v_j = u_field(u_field(s109, 0), 1);
    u_value v_v = u_field(u_field(s109, 0), 2);
    u_value v_b = u_field(u_field(s109, 0), 3);
    u_value v_k = u_field(s109, 1);
    u_value v_w = u_field(s109, 2);
    u_value v_c = u_field(s109, 3);
    t110 = f_map_46_mnode_4(v_a, v_j, v_v, f_map_46_mnode_4(v_b, v_k, v_w, v_c));
  }
  if (!t110) {
    t110 = v_m;
  }
  if (!t110) {
    u_no_case(s109);
  }
  return t110;
}

u_value f_map_46_left_of_1(u_value v_m) {
  u_value s111 = v_m;
  u_value t112 = NULL;
  if ((!t112) && u_is_con(s111, &n_map_46_MNode)) {
    u_value v_l = u_field(s111, 0);
    t112 = v_l;
  }
  if (!t112) {
    t112 = v_m;
  }
  if (!t112) {
    u_no_case(s111);
  }
  return t112;
}

u_value f_map_46_right_of_1(u_value v_m) {
  u_value s113 = v_m;
  u_value t114 = NULL;
  if ((!t114) && u_is_con(s113, &n_map_46_MNode)) {
    u_value v_r = u_field(s113, 3);
    t114 = v_r;
  }
  if (!t114) {
    t114 = v_m;
  }
  if (!t114) {
    u_no_case(s113);
  }
  return t114;
}

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  u_value t118;
  if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1))))) {
    u_value t115;
    if (u_truth(u_lt(f_map_46_height_1(f_map_46_left_of_1(v_l)), f_map_46_height_1(f_map_46_right_of_1(v_l))))) {
      t115 = f_map_46_rotate_left_1(v_l);
    } else {
      t115 = v_l;
    }
    t118 = f_map_46_rotate_right_1(f_map_46_mnode_4(t115, v_k, v_v, v_r));
  } else {
    u_value t117;
    if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1))))) {
      u_value t116;
      if (u_truth(u_lt(f_map_46_height_1(f_map_46_right_of_1(v_r)), f_map_46_height_1(f_map_46_left_of_1(v_r))))) {
        t116 = f_map_46_rotate_right_1(v_r);
      } else {
        t116 = v_r;
      }
      t117 = f_map_46_rotate_left_1(f_map_46_mnode_4(v_l, v_k, v_v, t116));
    } else {
      t117 = f_map_46_mnode_4(v_l, v_k, v_v, v_r);
    }
    t118 = t117;
  }
  return t118;
}

u_value f_map_46_balanced_1(u_value v_m) {
  u_value s119 = v_m;
  u_value t120 = NULL;
  if ((!t120) && u_is_con(s119, &n_map_46_MLeaf)) {
    t120 = u_mk_bool(1);
  }
  if ((!t120) && u_is_con(s119, &n_map_46_MNode)) {
    u_value v_l = u_field(s119, 0);
    u_value v_r = u_field(s119, 3);
    u_value v_h = u_field(s119, 4);
    u_value t121 = f_map_46_balanced_1(v_l);
    if (u_truth(t121)) {
      t121 = f_map_46_balanced_1(v_r);
    }
    u_value t122 = t121;
    if (u_truth(t122)) {
      t122 = u_eq(v_h, f_map_46_mnode_height_2(v_l, v_r));
    }
    u_value t123 = t122;
    if (u_truth(t123)) {
      t123 = f_prelude_46__60__61__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1)));
    }
    u_value t124 = t123;
    if (u_truth(t124)) {
      t124 = f_prelude_46__60__61__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1)));
    }
    t120 = t124;
  }
  if (!t120) {
    u_no_case(s119);
  }
  return t120;
}

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r) {
  u_value t125;
  if (u_truth(u_lt(f_map_46_height_1(v_l), f_map_46_height_1(v_r)))) {
    t125 = f_map_46_height_1(v_r);
  } else {
    t125 = f_map_46_height_1(v_l);
  }
  return u_add(u_mk_int(1), t125);
}

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m) {
  u_value s126 = v_ps;
  u_value t127 = NULL;
  if ((!t127) && u_is_con(s126, &n_list_46_Nil)) {
    t127 = v_m;
  }
  if ((!t127) && u_is_con(s126, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s126, 0), 0);
    u_value v_v = u_field(u_field(s126, 0), 1);
    u_value v_rest = u_field(s126, 1);
    t127 = f_map_46_put_all_2(v_rest, f_map_46_put_3(v_k, v_v, v_m));
  }
  if (!t127) {
    u_no_case(s126);
  }
  return t127;
}

u_value f_map_46_numbers_1(u_value v_n) {
  u_value t128;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t128 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t128 = f_list_46__43__43__2(f_map_46_numbers_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t128;
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
  u_value s129 = v_c;
  u_value t130 = NULL;
  if ((!t130) && u_is_con(s129, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s129, 0);
    t130 = f_map_46_get_2(v_n, v_a);
  }
  if (!t130) {
    u_no_case(s129);
  }
  return t130;
}

u_value f_ctx_46_schemes_of_2(u_value v_c, u_value v_n) {
  u_value s131 = v_c;
  u_value t132 = NULL;
  if ((!t132) && u_is_con(s131, &n_ctx_46_Ctx)) {
    u_value v_f = u_field(s131, 1);
    u_value s133 = f_map_46_get_2(v_n, v_f);
    u_value t134 = NULL;
    if ((!t134) && u_is_con(s133, &n_option_46_Some)) {
      u_value v_ss = u_field(s133, 0);
      t134 = v_ss;
    }
    if ((!t134) && u_is_con(s133, &n_option_46_None)) {
      t134 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t134) {
      u_no_case(s133);
    }
    t132 = t134;
  }
  if (!t132) {
    u_no_case(s131);
  }
  return t132;
}

u_value f_ctx_46_proc_of_2(u_value v_c, u_value v_n) {
  u_value s135 = v_c;
  u_value t136 = NULL;
  if ((!t136) && u_is_con(s135, &n_ctx_46_Ctx)) {
    u_value v_p = u_field(s135, 2);
    t136 = f_map_46_get_2(v_n, v_p);
  }
  if (!t136) {
    u_no_case(s135);
  }
  return t136;
}

u_value f_ctx_46_data_of_2(u_value v_c, u_value v_n) {
  u_value s137 = v_c;
  u_value t138 = NULL;
  if ((!t138) && u_is_con(s137, &n_ctx_46_Ctx)) {
    u_value v_d = u_field(s137, 3);
    t138 = f_map_46_get_2(v_n, v_d);
  }
  if (!t138) {
    u_no_case(s137);
  }
  return t138;
}

u_value f_ctx_46_members_of_2(u_value v_c, u_value v_n) {
  u_value s139 = v_c;
  u_value t140 = NULL;
  if ((!t140) && u_is_con(s139, &n_ctx_46_Ctx)) {
    u_value v_m = u_field(s139, 4);
    t140 = f_map_46_get_2(v_n, v_m);
  }
  if (!t140) {
    u_no_case(s139);
  }
  return t140;
}

u_value f_ctx_46_constr_of_2(u_value v_c, u_value v_n) {
  u_value s141 = v_c;
  u_value t142 = NULL;
  if ((!t142) && u_is_con(s141, &n_ctx_46_Ctx)) {
    u_value v_k = u_field(s141, 5);
    t142 = f_map_46_get_2(v_n, v_k);
  }
  if (!t142) {
    u_no_case(s141);
  }
  return t142;
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

u_value f_ctx_46_named_2(u_value v_c, u_value v_b) {
  u_value s143 = v_c;
  u_value t144 = NULL;
  if ((!t144) && u_is_con(s143, &n_ctx_46_Ctx)) {
    u_value v_bs = u_field(s143, 6);
    u_value s145 = f_map_46_get_2(v_b, v_bs);
    u_value t146 = NULL;
    if ((!t146) && u_is_con(s145, &n_option_46_Some)) {
      u_value v_ns = u_field(s145, 0);
      t146 = v_ns;
    }
    if ((!t146) && u_is_con(s145, &n_option_46_None)) {
      t146 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t146) {
      u_no_case(s145);
    }
    t144 = t146;
  }
  if (!t144) {
    u_no_case(s143);
  }
  return t144;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s147 = f_map_46_get_2(f_syntax_46_base_1(v_n), v_bs);
  u_value t148 = NULL;
  if ((!t148) && u_is_con(s147, &n_option_46_Some)) {
    u_value v_ns = u_field(s147, 0);
    u_value t149;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t149 = v_bs;
    } else {
      t149 = f_map_46_put_3(f_syntax_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, v_ns), v_bs);
    }
    t148 = t149;
  }
  if ((!t148) && u_is_con(s147, &n_option_46_None)) {
    t148 = f_map_46_put_3(f_syntax_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t148) {
    u_no_case(s147);
  }
  return t148;
}

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t) {
  u_value s150 = v_c;
  u_value t151 = NULL;
  if ((!t151) && u_is_con(s150, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s150, 0);
    u_value v_f = u_field(s150, 1);
    u_value v_p = u_field(s150, 2);
    u_value v_d = u_field(s150, 3);
    u_value v_m = u_field(s150, 4);
    u_value v_k = u_field(s150, 5);
    u_value v_bs = u_field(s150, 6);
    t151 = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), v_a), v_f, v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t151) {
    u_no_case(s150);
  }
  return t151;
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
  u_value s152 = v_c;
  u_value t153 = NULL;
  if ((!t153) && u_is_con(s152, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s152, 0);
    u_value v_f = u_field(s152, 1);
    u_value v_p = u_field(s152, 2);
    u_value v_d = u_field(s152, 3);
    u_value v_m = u_field(s152, 4);
    u_value v_k = u_field(s152, 5);
    u_value v_bs = u_field(s152, 6);
    t153 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, u_mk_con(&n_list_46_Nil, 0))), v_f), v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t153) {
    u_no_case(s152);
  }
  return t153;
}

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g) {
  u_value s154 = v_c;
  u_value t155 = NULL;
  if ((!t155) && u_is_con(s154, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s154, 0);
    u_value v_f = u_field(s154, 1);
    u_value v_p = u_field(s154, 2);
    u_value v_d = u_field(s154, 3);
    u_value v_m = u_field(s154, 4);
    u_value v_k = u_field(s154, 5);
    u_value v_bs = u_field(s154, 6);
    t155 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, f_map_46_put_3(v_n, v_g, v_p), v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t155) {
    u_no_case(s154);
  }
  return t155;
}

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity) {
  u_value s156 = v_c;
  u_value t157 = NULL;
  if ((!t157) && u_is_con(s156, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s156, 0);
    u_value v_f = u_field(s156, 1);
    u_value v_p = u_field(s156, 2);
    u_value v_d = u_field(s156, 3);
    u_value v_m = u_field(s156, 4);
    u_value v_k = u_field(s156, 5);
    u_value v_bs = u_field(s156, 6);
    t157 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, f_map_46_put_3(v_n, v_arity, v_d), v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t157) {
    u_no_case(s156);
  }
  return t157;
}

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs) {
  u_value s158 = v_c;
  u_value t159 = NULL;
  if ((!t159) && u_is_con(s158, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s158, 0);
    u_value v_f = u_field(s158, 1);
    u_value v_p = u_field(s158, 2);
    u_value v_d = u_field(s158, 3);
    u_value v_m = u_field(s158, 4);
    u_value v_k = u_field(s158, 5);
    u_value v_bs = u_field(s158, 6);
    t159 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, f_map_46_put_3(v_n, v_cs, v_m), v_k, v_bs);
  }
  if (!t159) {
    u_no_case(s158);
  }
  return t159;
}

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc) {
  u_value s160 = v_c;
  u_value t161 = NULL;
  if ((!t161) && u_is_con(s160, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s160, 0);
    u_value v_f = u_field(s160, 1);
    u_value v_p = u_field(s160, 2);
    u_value v_d = u_field(s160, 3);
    u_value v_m = u_field(s160, 4);
    u_value v_k = u_field(s160, 5);
    u_value v_bs = u_field(s160, 6);
    t161 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, v_m, f_map_46_put_3(v_n, v_kc, v_k), f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t161) {
    u_no_case(s160);
  }
  return t161;
}

u_value f_check_46_ctx_of_1(u_value v_s) {
  u_value s162 = v_s;
  u_value t163 = NULL;
  if ((!t163) && u_is_con(s162, &n_check_46_State)) {
    u_value v_c = u_field(s162, 0);
    t163 = v_c;
  }
  if (!t163) {
    u_no_case(s162);
  }
  return t163;
}

u_value f_check_46_sub_of_1(u_value v_s) {
  u_value s164 = v_s;
  u_value t165 = NULL;
  if ((!t165) && u_is_con(s164, &n_check_46_State)) {
    u_value v_su = u_field(s164, 1);
    t165 = v_su;
  }
  if (!t165) {
    u_no_case(s164);
  }
  return t165;
}

u_value f_check_46_module_of_1(u_value v_s) {
  u_value s166 = v_s;
  u_value t167 = NULL;
  if ((!t167) && u_is_con(s166, &n_check_46_State)) {
    u_value v_m = u_field(s166, 3);
    t167 = v_m;
  }
  if (!t167) {
    u_no_case(s166);
  }
  return t167;
}

u_value f_check_46_visible_of_1(u_value v_s) {
  u_value s168 = v_s;
  u_value t169 = NULL;
  if ((!t169) && u_is_con(s168, &n_check_46_State)) {
    u_value v_ms = u_field(s168, 4);
    t169 = v_ms;
  }
  if (!t169) {
    u_no_case(s168);
  }
  return t169;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  u_value s170 = v_s;
  u_value t171 = NULL;
  if ((!t171) && u_is_con(s170, &n_check_46_State)) {
    u_value v_su = u_field(s170, 1);
    u_value v_n = u_field(s170, 2);
    u_value v_m = u_field(s170, 3);
    u_value v_ms = u_field(s170, 4);
    u_value v_vs = u_field(s170, 5);
    t171 = u_mk_con(&n_check_46_State, 6, v_c, v_su, v_n, v_m, v_ms, v_vs);
  }
  if (!t171) {
    u_no_case(s170);
  }
  return t171;
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  u_value s172 = v_s;
  u_value t173 = NULL;
  if ((!t173) && u_is_con(s172, &n_check_46_State)) {
    u_value v_c = u_field(s172, 0);
    u_value v_vs = u_field(s172, 5);
    t173 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_empty_0(), u_mk_int(0), v_m, f_check_46_sees_2(v_m, v_vs), v_vs);
  }
  if (!t173) {
    u_no_case(s172);
  }
  return t173;
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s174 = f_list_46_lookup_2(v_m, v_vs);
  u_value t175 = NULL;
  if ((!t175) && u_is_con(s174, &n_option_46_Some)) {
    u_value v_ms = u_field(s174, 0);
    t175 = v_ms;
  }
  if ((!t175) && u_is_con(s174, &n_option_46_None)) {
    t175 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t175) {
    u_no_case(s174);
  }
  return t175;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s176 = v_cs;
  u_value t177 = NULL;
  if ((!t177) && u_is_con(s176, &n_list_46_Nil)) {
    t177 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t177) && (u_is_con(s176, &n_list_46_Cons) && u_is_con(u_field(s176, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s176, 0), 0);
    u_value v_ds = u_field(u_field(s176, 0), 1);
    u_value v_rest = u_field(s176, 1);
    t177 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t177) && u_is_con(s176, &n_list_46_Cons)) {
    u_value v_c = u_field(s176, 0);
    u_value v_rest = u_field(s176, 1);
    t177 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t177) {
    u_no_case(s176);
  }
  return t177;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s178 = v_c;
  u_value t179 = NULL;
  if ((!t179) && u_is_con(s178, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s178, 0);
    u_value v_ps = u_field(s178, 1);
    u_value v_t = u_field(s178, 2);
    t179 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t179) && u_is_con(s178, &n_syntax_46_CData)) {
    u_value v_n = u_field(s178, 0);
    u_value v_ps = u_field(s178, 1);
    u_value v_ks = u_field(s178, 2);
    u_value t180 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e181 = &t180;
    u_value l182 = v_ks;
    while (u_is_con(l182, &n_list_46_Cons)) {
      u_value v_k = u_field(l182, 0);
      if (u_truth(u_mk_bool(1))) {
        e181 = u_append(e181, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l182 = u_field(l182, 1);
    }
    t179 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t180);
  }
  if ((!t179) && u_is_con(s178, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s178, 0);
    u_value v_ps = u_field(s178, 1);
    u_value v_xss = u_field(s178, 2);
    u_value v_r = u_field(s178, 3);
    u_value v_b = u_field(s178, 4);
    t179 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t179) && u_is_con(s178, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s178, 0);
    u_value v_ps = u_field(s178, 1);
    u_value v_ins = u_field(s178, 2);
    u_value v_outs = u_field(s178, 3);
    u_value v_refs = u_field(s178, 4);
    u_value v_b = u_field(s178, 5);
    t179 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t179) {
    t179 = v_c;
  }
  if (!t179) {
    u_no_case(s178);
  }
  return t179;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  u_value s183 = v_k;
  u_value t184 = NULL;
  if ((!t184) && u_is_con(s183, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s183, 0);
    u_value v_ts = u_field(s183, 1);
    t184 = u_mk_con(&n_syntax_46_Constr, 2, f_check_46_in_module_2(v_m, v_n), v_ts);
  }
  if (!t184) {
    u_no_case(s183);
  }
  return t184;
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s185 = v_ds;
  u_value t186 = NULL;
  if ((!t186) && u_is_con(s185, &n_list_46_Nil)) {
    t186 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t186) && (u_is_con(s185, &n_list_46_Cons) && u_is_con(u_field(u_field(s185, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s185, 0), 0);
    u_value v_n = u_field(u_field(u_field(s185, 0), 1), 0);
    u_value v_rest = u_field(s185, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t186 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t186) && u_is_con(s185, &n_list_46_Cons)) {
    u_value v_rest = u_field(s185, 1);
    t186 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t186) {
    u_no_case(s185);
  }
  return t186;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t187 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e188 = &t187;
  u_value l189 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l189, &n_list_46_Cons)) {
    u_value v_m = u_field(l189, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_190 = U_LIT("prelude");
      static struct u_object l_prelude_191 = U_LIT("prelude");
      e188 = u_append(e188, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_190, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_191)))))));
    }
    l189 = u_field(l189, 1);
  }
  return t187;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value s192 = v_n;
  u_value t193 = NULL;
  if ((!t193) && (u_is_con(s192, &n_syntax_46_Name) && u_is_con(u_field(s192, 0), &n_option_46_Some))) {
    u_value t194 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e195 = &t194;
    u_value l196 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    while (u_is_con(l196, &n_list_46_Cons)) {
      u_value v_d = u_field(l196, 0);
      if (u_truth(f_list_46_contains_2(v_d, v_declared))) {
        e195 = u_append(e195, v_d);
      }
      l196 = u_field(l196, 1);
    }
    t193 = t194;
  }
  if ((!t193) && (u_is_con(s192, &n_syntax_46_Name) && u_is_con(u_field(s192, 0), &n_option_46_None))) {
    u_value v_b = u_field(s192, 1);
    u_value t203;
    if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_module_of_1(v_s)), v_b), v_declared))) {
      t203 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_module_of_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
    } else {
      u_value t197 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e198 = &t197;
      u_value t199 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e200 = &t199;
      u_value l201 = f_list_46_drop_2(u_mk_int(1), f_check_46_visible_of_1(v_s));
      while (u_is_con(l201, &n_list_46_Cons)) {
        u_value v_m = u_field(l201, 0);
        if (u_truth(u_mk_bool(1))) {
          e200 = u_append(e200, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_b));
        }
        l201 = u_field(l201, 1);
      }
      u_value l202 = t199;
      while (u_is_con(l202, &n_list_46_Cons)) {
        u_value v_d = u_field(l202, 0);
        if (u_truth(f_list_46_contains_2(v_d, v_declared))) {
          e198 = u_append(e198, v_d);
        }
        l202 = u_field(l202, 1);
      }
      t203 = t197;
    }
    t193 = t203;
  }
  if (!t193) {
    u_no_case(s192);
  }
  return t193;
}

u_value f_check_46_funs_named_2(u_value v_c, u_value v_n) {
  u_value t204 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e205 = &t204;
  u_value l206 = f_ctx_46_named_2(v_c, f_syntax_46_base_1(v_n));
  while (u_is_con(l206, &n_list_46_Cons)) {
    u_value v_d = u_field(l206, 0);
    if (u_truth(f_ctx_46_is_fun_2(v_c, v_d))) {
      e205 = u_append(e205, v_d);
    }
    l206 = u_field(l206, 1);
  }
  return t204;
}

u_value f_check_46_constrs_named_2(u_value v_c, u_value v_n) {
  u_value t207 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e208 = &t207;
  u_value l209 = f_ctx_46_named_2(v_c, f_syntax_46_base_1(v_n));
  while (u_is_con(l209, &n_list_46_Cons)) {
    u_value v_d = u_field(l209, 0);
    if (u_truth(f_ctx_46_is_constr_2(v_c, v_d))) {
      e208 = u_append(e208, v_d);
    }
    l209 = u_field(l209, 1);
  }
  return t207;
}

u_value f_check_46_procs_named_2(u_value v_c, u_value v_n) {
  u_value t210 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e211 = &t210;
  u_value l212 = f_ctx_46_named_2(v_c, f_syntax_46_base_1(v_n));
  while (u_is_con(l212, &n_list_46_Cons)) {
    u_value v_d = u_field(l212, 0);
    if (u_truth(f_ctx_46_is_proc_2(v_c, v_d))) {
      e211 = u_append(e211, v_d);
    }
    l212 = u_field(l212, 1);
  }
  return t210;
}

u_value f_check_46_sorts_named_2(u_value v_c, u_value v_n) {
  u_value t213 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e214 = &t213;
  u_value l215 = f_ctx_46_named_2(v_c, f_syntax_46_base_1(v_n));
  while (u_is_con(l215, &n_list_46_Cons)) {
    u_value v_d = u_field(l215, 0);
    u_value t216 = f_ctx_46_is_data_2(v_c, v_d);
    if (!u_truth(t216)) {
      t216 = f_ctx_46_is_alias_2(v_c, v_d);
    }
    if (u_truth(t216)) {
      e214 = u_append(e214, v_d);
    }
    l215 = u_field(l215, 1);
  }
  return t213;
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
  static struct u_object l_Bool_217 = U_LIT("Bool");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_217), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_218 = U_LIT("Int");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_218), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_219 = U_LIT("Str");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_219), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_220 = U_LIT("Char");
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_220), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t221 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e222 = &t221;
  u_value l223 = v_ps;
  while (u_is_con(l223, &n_list_46_Cons)) {
    u_value v_p = u_field(l223, 0);
    if (u_truth(u_mk_bool(1))) {
      e222 = u_append(e222, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l223 = u_field(l223, 1);
  }
  return t221;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s224 = v_n;
  u_value t225 = NULL;
  if ((!t225) && (u_is_con(s224, &n_syntax_46_Name) && u_is_con(u_field(s224, 0), &n_option_46_None))) {
    u_value v_b = u_field(s224, 1);
    static struct u_object l_Bool_226 = U_LIT("Bool");
    static struct u_object l_Int_227 = U_LIT("Int");
    static struct u_object l_Str_228 = U_LIT("Str");
    static struct u_object l_Char_229 = U_LIT("Char");
    t225 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_226, u_mk_con(&n_list_46_Cons, 2, &l_Int_227, u_mk_con(&n_list_46_Cons, 2, &l_Str_228, u_mk_con(&n_list_46_Cons, 2, &l_Char_229, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t225) {
    t225 = u_mk_bool(0);
  }
  if (!t225) {
    u_no_case(s224);
  }
  return t225;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s230 = v_v;
  u_value t231 = NULL;
  if ((!t231) && u_is_con(s230, &n_syntax_46_VUnit)) {
    t231 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t231) && u_is_con(s230, &n_syntax_46_VBool)) {
    t231 = f_check_46_tbool_0();
  }
  if ((!t231) && u_is_con(s230, &n_syntax_46_VInt)) {
    t231 = f_check_46_tint_0();
  }
  if ((!t231) && u_is_con(s230, &n_syntax_46_VStr)) {
    t231 = f_check_46_tstr_0();
  }
  if ((!t231) && u_is_con(s230, &n_syntax_46_VChar)) {
    t231 = f_check_46_tchar_0();
  }
  if (!t231) {
    u_no_case(s230);
  }
  return t231;
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
  u_value s232 = v_n;
  u_value t233 = NULL;
  if ((!t233) && (u_is_con(s232, &n_syntax_46_Name) && u_is_con(u_field(s232, 0), &n_option_46_None))) {
    u_value v_b = u_field(s232, 1);
    t233 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t233) {
    t233 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t233) {
    u_no_case(s232);
  }
  return t233;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_234 = U_LIT("A");
  u_value t238 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_234);
  u_value t237;
  {
    u_value v_a = t238;
    u_value s235 = v_b;
    u_value t236 = NULL;
    if ((!t236) && u_equals(s235, u_mk_lit("+"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("-"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("*"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("/"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("%"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("<"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("=="))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("compare"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("and"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("or"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("chars"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("string"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("ord"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("chr"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("^"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_size"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_join"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_slice"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_split"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_get"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_contains"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_compare"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_show"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_quote"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("str_number"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("is_digit"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("is_letter"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t236) && u_equals(s235, u_mk_lit("is_space"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t236) {
      t236 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t236) {
      u_no_case(s235);
    }
    t237 = t236;
  }
  return t237;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s239 = v_t;
  u_value t240 = NULL;
  if ((!t240) && u_is_con(s239, &n_syntax_46_TMeta)) {
    t240 = u_mk_bool(1);
  }
  if (!t240) {
    t240 = u_mk_bool(0);
  }
  if (!t240) {
    u_no_case(s239);
  }
  return t240;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s241 = v_t;
  u_value t242 = NULL;
  if ((!t242) && u_is_con(s241, &n_syntax_46_TParam)) {
    t242 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t242) && u_is_con(s241, &n_syntax_46_TMeta)) {
    t242 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t242) && u_is_con(s241, &n_syntax_46_THole)) {
    t242 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t242) && u_is_con(s241, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s241, 1);
    u_value t243 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e244 = &t243;
    u_value l245 = v_args;
    while (u_is_con(l245, &n_list_46_Cons)) {
      u_value v_a = u_field(l245, 0);
      if (u_truth(u_mk_bool(1))) {
        e244 = u_append(e244, f_check_46_free_1(v_a));
      }
      l245 = u_field(l245, 1);
    }
    t242 = f_list_46_distinct_1(f_list_46_concat_1(t243));
  }
  if ((!t242) && u_is_con(s241, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s241, 0);
    u_value v_r = u_field(s241, 1);
    u_value t246 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e247 = &t246;
    u_value l248 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l248, &n_list_46_Cons)) {
      u_value v_a = u_field(l248, 0);
      if (u_truth(u_mk_bool(1))) {
        e247 = u_append(e247, f_check_46_free_1(v_a));
      }
      l248 = u_field(l248, 1);
    }
    t242 = f_list_46_distinct_1(f_list_46_concat_1(t246));
  }
  if ((!t242) && u_is_con(s241, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s241, 0);
    u_value t249 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e250 = &t249;
    u_value l251 = v_ts;
    while (u_is_con(l251, &n_list_46_Cons)) {
      u_value v_a = u_field(l251, 0);
      if (u_truth(u_mk_bool(1))) {
        e250 = u_append(e250, f_check_46_free_1(v_a));
      }
      l251 = u_field(l251, 1);
    }
    t242 = f_list_46_distinct_1(f_list_46_concat_1(t249));
  }
  if (!t242) {
    u_no_case(s241);
  }
  return t242;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s252 = v_t;
  u_value t253 = NULL;
  if ((!t253) && u_is_con(s252, &n_syntax_46_THole)) {
    t253 = u_mk_bool(1);
  }
  if ((!t253) && u_is_con(s252, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s252, 1);
    u_value t254 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e255 = &t254;
    u_value l256 = v_args;
    while (u_is_con(l256, &n_list_46_Cons)) {
      u_value v_a = u_field(l256, 0);
      if (u_truth(u_mk_bool(1))) {
        e255 = u_append(e255, f_check_46_holes_1(v_a));
      }
      l256 = u_field(l256, 1);
    }
    t253 = f_list_46_contains_2(u_mk_bool(1), t254);
  }
  if ((!t253) && u_is_con(s252, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s252, 0);
    u_value v_r = u_field(s252, 1);
    u_value t257 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e258 = &t257;
    u_value l259 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l259, &n_list_46_Cons)) {
      u_value v_a = u_field(l259, 0);
      if (u_truth(u_mk_bool(1))) {
        e258 = u_append(e258, f_check_46_holes_1(v_a));
      }
      l259 = u_field(l259, 1);
    }
    t253 = f_list_46_contains_2(u_mk_bool(1), t257);
  }
  if ((!t253) && u_is_con(s252, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s252, 0);
    u_value t260 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e261 = &t260;
    u_value l262 = v_ts;
    while (u_is_con(l262, &n_list_46_Cons)) {
      u_value v_a = u_field(l262, 0);
      if (u_truth(u_mk_bool(1))) {
        e261 = u_append(e261, f_check_46_holes_1(v_a));
      }
      l262 = u_field(l262, 1);
    }
    t253 = f_list_46_contains_2(u_mk_bool(1), t260);
  }
  if (!t253) {
    t253 = u_mk_bool(0);
  }
  if (!t253) {
    u_no_case(s252);
  }
  return t253;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s263 = v_t;
  u_value t264 = NULL;
  if ((!t264) && u_is_con(s263, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s263, 0);
    u_value v_args = u_field(s263, 1);
    u_value t265 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e266 = &t265;
    u_value l267 = v_args;
    while (u_is_con(l267, &n_list_46_Cons)) {
      u_value v_a = u_field(l267, 0);
      if (u_truth(u_mk_bool(1))) {
        e266 = u_append(e266, f_check_46_subst_2(v_a, v_m));
      }
      l267 = u_field(l267, 1);
    }
    t264 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t265);
  }
  if ((!t264) && u_is_con(s263, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s263, 0);
    u_value v_r = u_field(s263, 1);
    u_value t268 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e269 = &t268;
    u_value l270 = v_args;
    while (u_is_con(l270, &n_list_46_Cons)) {
      u_value v_a = u_field(l270, 0);
      if (u_truth(u_mk_bool(1))) {
        e269 = u_append(e269, f_check_46_subst_2(v_a, v_m));
      }
      l270 = u_field(l270, 1);
    }
    t264 = u_mk_con(&n_syntax_46_TFun, 2, t268, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t264) && u_is_con(s263, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s263, 0);
    u_value t271 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e272 = &t271;
    u_value l273 = v_ts;
    while (u_is_con(l273, &n_list_46_Cons)) {
      u_value v_a = u_field(l273, 0);
      if (u_truth(u_mk_bool(1))) {
        e272 = u_append(e272, f_check_46_subst_2(v_a, v_m));
      }
      l273 = u_field(l273, 1);
    }
    t264 = u_mk_con(&n_syntax_46_TTuple, 1, t271);
  }
  if (!t264) {
    u_value s274 = f_list_46_lookup_2(v_t, v_m);
    u_value t275 = NULL;
    if ((!t275) && u_is_con(s274, &n_option_46_Some)) {
      u_value v_u = u_field(s274, 0);
      t275 = v_u;
    }
    if ((!t275) && u_is_con(s274, &n_option_46_None)) {
      t275 = v_t;
    }
    if (!t275) {
      u_no_case(s274);
    }
    t264 = t275;
  }
  if (!t264) {
    u_no_case(s263);
  }
  return t264;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s276 = v_t;
  u_value t277 = NULL;
  if ((!t277) && u_is_con(s276, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s276, 0);
    u_value s278 = f_map_46_get_2(v_i, v_su);
    u_value t279 = NULL;
    if ((!t279) && u_is_con(s278, &n_option_46_Some)) {
      u_value v_u = u_field(s278, 0);
      t279 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t279) && u_is_con(s278, &n_option_46_None)) {
      t279 = v_t;
    }
    if (!t279) {
      u_no_case(s278);
    }
    t277 = t279;
  }
  if (!t277) {
    t277 = v_t;
  }
  if (!t277) {
    u_no_case(s276);
  }
  return t277;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s280 = f_check_46_resolve_2(v_su, v_t);
  u_value t281 = NULL;
  if ((!t281) && u_is_con(s280, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s280, 0);
    t281 = u_eq(v_i, v_j);
  }
  if ((!t281) && u_is_con(s280, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s280, 1);
    u_value t282 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e283 = &t282;
    u_value l284 = v_args;
    while (u_is_con(l284, &n_list_46_Cons)) {
      u_value v_a = u_field(l284, 0);
      if (u_truth(u_mk_bool(1))) {
        e283 = u_append(e283, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l284 = u_field(l284, 1);
    }
    t281 = f_list_46_contains_2(u_mk_bool(1), t282);
  }
  if ((!t281) && u_is_con(s280, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s280, 0);
    u_value v_r = u_field(s280, 1);
    u_value t285 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e286 = &t285;
    u_value l287 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l287, &n_list_46_Cons)) {
      u_value v_a = u_field(l287, 0);
      if (u_truth(u_mk_bool(1))) {
        e286 = u_append(e286, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l287 = u_field(l287, 1);
    }
    t281 = f_list_46_contains_2(u_mk_bool(1), t285);
  }
  if ((!t281) && u_is_con(s280, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s280, 0);
    u_value t288 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e289 = &t288;
    u_value l290 = v_ts;
    while (u_is_con(l290, &n_list_46_Cons)) {
      u_value v_a = u_field(l290, 0);
      if (u_truth(u_mk_bool(1))) {
        e289 = u_append(e289, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l290 = u_field(l290, 1);
    }
    t281 = f_list_46_contains_2(u_mk_bool(1), t288);
  }
  if (!t281) {
    t281 = u_mk_bool(0);
  }
  if (!t281) {
    u_no_case(s280);
  }
  return t281;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s291 = v_k;
  u_value t292 = NULL;
  if (!t292) {
    u_value v_ps = u_field(s291, 0);
    u_value v_ks = u_field(s291, 1);
    u_value v_r = u_field(s291, 2);
    u_value t293;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t293 = v_r;
    } else {
      t293 = f_check_46_fun_2(v_ks, v_r);
    }
    t292 = u_mk_tuple(2, v_ps, t293);
  }
  if (!t292) {
    u_no_case(s291);
  }
  return t292;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s294 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t295 = NULL;
  if ((!t295) && u_is_con(s294, &n_option_46_Some)) {
    u_value v_kc = u_field(s294, 0);
    t295 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t295) && u_is_con(s294, &n_option_46_None)) {
    t295 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t295) {
    u_no_case(s294);
  }
  return t295;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s296 = f_list_46_lookup_2(v_n, v_env);
  u_value t297 = NULL;
  if ((!t297) && u_is_con(s296, &n_option_46_Some)) {
    u_value v_sc = u_field(s296, 0);
    t297 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t297) && u_is_con(s296, &n_option_46_None)) {
    u_value t298 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e299 = &t298;
    u_value l300 = f_check_46_visible_names_3(v_s, v_n, f_check_46_funs_named_2(f_check_46_ctx_of_1(v_s), v_n));
    while (u_is_con(l300, &n_list_46_Cons)) {
      u_value v_f = u_field(l300, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t301 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e302 = &t301;
        u_value l303 = f_ctx_46_schemes_of_2(f_check_46_ctx_of_1(v_s), v_f);
        while (u_is_con(l303, &n_list_46_Cons)) {
          u_value v_sc = u_field(l303, 0);
          if (u_truth(u_mk_bool(1))) {
            e302 = u_append(e302, u_mk_tuple(2, v_f, v_sc));
          }
          l303 = u_field(l303, 1);
        }
        e299 = u_append(e299, t301);
      }
      l300 = u_field(l300, 1);
    }
    u_value t304 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e305 = &t304;
    u_value l306 = f_check_46_visible_names_3(v_s, v_n, f_check_46_constrs_named_2(f_check_46_ctx_of_1(v_s), v_n));
    while (u_is_con(l306, &n_list_46_Cons)) {
      u_value v_k = u_field(l306, 0);
      if (u_truth(u_mk_bool(1))) {
        e305 = u_append(e305, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_ctx_of_1(v_s), v_k)));
      }
      l306 = u_field(l306, 1);
    }
    u_value t307 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e308 = &t307;
    u_value l309 = f_check_46_builtin_1(v_n);
    while (u_is_con(l309, &n_list_46_Cons)) {
      u_value v_sc = u_field(l309, 0);
      if (u_truth(u_mk_bool(1))) {
        e308 = u_append(e308, u_mk_tuple(2, v_n, v_sc));
      }
      l309 = u_field(l309, 1);
    }
    t297 = f_list_46__43__43__2(f_list_46_concat_1(t298), f_list_46__43__43__2(t304, t307));
  }
  if (!t297) {
    u_no_case(s296);
  }
  return t297;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s310 = v_cs;
  u_value t311 = NULL;
  if ((!t311) && u_is_con(s310, &n_list_46_Nil)) {
    t311 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t311) && u_is_con(s310, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s310, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s310, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s310, 0), 1), 1);
    u_value v_rest = u_field(s310, 1);
    u_value t312;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t312 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t312 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t311 = t312;
  }
  if (!t311) {
    u_no_case(s310);
  }
  return t311;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s313 = v_scs;
  u_value t314 = NULL;
  if ((!t314) && u_is_con(s313, &n_list_46_Nil)) {
    t314 = u_mk_bool(0);
  }
  if ((!t314) && u_is_con(s313, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s313, 0), 1);
    u_value v_rest = u_field(s313, 1);
    u_value t315 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t315)) {
      t315 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t314 = t315;
  }
  if (!t314) {
    u_no_case(s313);
  }
  return t314;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t316 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e317 = &t316;
  u_value l318 = v_xs;
  while (u_is_con(l318, &n_list_46_Cons)) {
    u_value v_x = u_field(l318, 0);
    if (u_truth(u_mk_bool(1))) {
      e317 = u_append(e317, u_mk_tuple(2, f_syntax_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_typ_1(v_x))));
    }
    l318 = u_field(l318, 1);
  }
  return t316;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s319 = v_env;
  u_value t320 = NULL;
  if ((!t320) && u_is_con(s319, &n_list_46_Nil)) {
    t320 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t320) && u_is_con(s319, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s319, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s319, 0), 1), 1);
    u_value v_rest = u_field(s319, 1);
    t320 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t320) {
    u_no_case(s319);
  }
  return t320;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t326 = f_check_46_fixed_2(v_su, v_env);
  u_value t327 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t325;
  {
    u_value v_fx = t326;
    u_value v_u = t327;
    u_value t321 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e322 = &t321;
    u_value l323 = f_check_46_free_1(v_u);
    while (u_is_con(l323, &n_list_46_Cons)) {
      u_value v_m = u_field(l323, 0);
      u_value t324 = f_check_46_is_meta_1(v_m);
      if (u_truth(t324)) {
        t324 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t324)) {
        e322 = u_append(e322, v_m);
      }
      l323 = u_field(l323, 1);
    }
    t325 = u_mk_tuple(2, t321, v_u);
  }
  return t325;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t328 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e329 = &t328;
  u_value l330 = v_xs;
  while (u_is_con(l330, &n_list_46_Cons)) {
    u_value v_x = u_field(l330, 0);
    if (u_truth(u_mk_bool(1))) {
      e329 = u_append(e329, u_mk_tuple(2, f_syntax_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_typ_1(v_x))));
    }
    l330 = u_field(l330, 1);
  }
  return t328;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s331 = v_b;
  u_value t332 = NULL;
  if ((!t332) && u_is_con(s331, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s331, 0);
    u_value v_g = u_field(s331, 1);
    t332 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t332) {
    u_no_case(s331);
  }
  return t332;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s333 = v_g;
  u_value t334 = NULL;
  if ((!t334) && u_is_con(s333, &n_option_46_Some)) {
    u_value v_e = u_field(s333, 0);
    t334 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t334) && u_is_con(s333, &n_option_46_None)) {
    t334 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t334) {
    u_no_case(s333);
  }
  return t334;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s335 = f_check_46_resolve_2(v_su, v_t);
  u_value t336 = NULL;
  if ((!t336) && u_is_con(s335, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s335, 0);
    u_value v_args = u_field(s335, 1);
    u_value t337 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e338 = &t337;
    u_value l339 = v_args;
    while (u_is_con(l339, &n_list_46_Cons)) {
      u_value v_a = u_field(l339, 0);
      if (u_truth(u_mk_bool(1))) {
        e338 = u_append(e338, f_check_46_zonk_type_2(v_su, v_a));
      }
      l339 = u_field(l339, 1);
    }
    t336 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t337);
  }
  if ((!t336) && u_is_con(s335, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s335, 0);
    u_value v_r = u_field(s335, 1);
    u_value t340 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e341 = &t340;
    u_value l342 = v_args;
    while (u_is_con(l342, &n_list_46_Cons)) {
      u_value v_a = u_field(l342, 0);
      if (u_truth(u_mk_bool(1))) {
        e341 = u_append(e341, f_check_46_zonk_type_2(v_su, v_a));
      }
      l342 = u_field(l342, 1);
    }
    t336 = u_mk_con(&n_syntax_46_TFun, 2, t340, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t336) && u_is_con(s335, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s335, 0);
    u_value t343 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e344 = &t343;
    u_value l345 = v_ts;
    while (u_is_con(l345, &n_list_46_Cons)) {
      u_value v_a = u_field(l345, 0);
      if (u_truth(u_mk_bool(1))) {
        e344 = u_append(e344, f_check_46_zonk_type_2(v_su, v_a));
      }
      l345 = u_field(l345, 1);
    }
    t336 = u_mk_con(&n_syntax_46_TTuple, 1, t343);
  }
  if (!t336) {
    u_value v_u = s335;
    t336 = v_u;
  }
  if (!t336) {
    u_no_case(s335);
  }
  return t336;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  u_value s346 = v_x;
  u_value t347 = NULL;
  if ((!t347) && u_is_con(s346, &n_syntax_46_Var)) {
    u_value v_n = u_field(s346, 0);
    u_value v_t = u_field(s346, 1);
    t347 = u_mk_con(&n_syntax_46_Var, 2, v_n, f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t347) {
    u_no_case(s346);
  }
  return t347;
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t348 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e349 = &t348;
  u_value l350 = v_xs;
  while (u_is_con(l350, &n_list_46_Cons)) {
    u_value v_x = u_field(l350, 0);
    if (u_truth(u_mk_bool(1))) {
      e349 = u_append(e349, f_check_46_zonk_var_2(v_su, v_x));
    }
    l350 = u_field(l350, 1);
  }
  return t348;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s351 = v_e;
  u_value t352 = NULL;
  if ((!t352) && u_is_con(s351, &n_syntax_46_ELit)) {
    t352 = v_e;
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s351, 0);
    t352 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s351, 0);
    u_value v_args = u_field(s351, 1);
    u_value t353 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e354 = &t353;
    u_value l355 = v_args;
    while (u_is_con(l355, &n_list_46_Cons)) {
      u_value v_a = u_field(l355, 0);
      if (u_truth(u_mk_bool(1))) {
        e354 = u_append(e354, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l355 = u_field(l355, 1);
    }
    t352 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t353);
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s351, 0);
    u_value v_b = u_field(s351, 1);
    t352 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s351, 0);
    u_value v_b = u_field(s351, 1);
    u_value t356 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e357 = &t356;
    u_value l358 = v_ps;
    while (u_is_con(l358, &n_list_46_Cons)) {
      u_value v_p = u_field(l358, 0);
      if (u_truth(u_mk_bool(1))) {
        e357 = u_append(e357, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l358 = u_field(l358, 1);
    }
    t352 = u_mk_con(&n_syntax_46_ELet, 2, t356, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s351, 0);
    u_value v_l = u_field(s351, 1);
    u_value v_r = u_field(s351, 2);
    t352 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s351, 0);
    u_value v_bs = u_field(s351, 1);
    u_value t359 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e360 = &t359;
    u_value l361 = v_bs;
    while (u_is_con(l361, &n_list_46_Cons)) {
      u_value v_b = u_field(l361, 0);
      if (u_truth(u_mk_bool(1))) {
        e360 = u_append(e360, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l361 = u_field(l361, 1);
    }
    t352 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t359);
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s351, 0);
    u_value v_xs = u_field(s351, 1);
    u_value v_ls = u_field(s351, 2);
    u_value v_c = u_field(s351, 3);
    u_value t362 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e363 = &t362;
    u_value l364 = v_ls;
    while (u_is_con(l364, &n_list_46_Cons)) {
      u_value v_l = u_field(l364, 0);
      if (u_truth(u_mk_bool(1))) {
        e363 = u_append(e363, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l364 = u_field(l364, 1);
    }
    t352 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t362, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t352) && u_is_con(s351, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s351, 0);
    u_value t365 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e366 = &t365;
    u_value l367 = v_es;
    while (u_is_con(l367, &n_list_46_Cons)) {
      u_value v_e = u_field(l367, 0);
      if (u_truth(u_mk_bool(1))) {
        e366 = u_append(e366, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l367 = u_field(l367, 1);
    }
    t352 = u_mk_con(&n_syntax_46_ETuple, 1, t365);
  }
  if (!t352) {
    u_no_case(s351);
  }
  return t352;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s368 = v_p;
  u_value t369 = NULL;
  if (!t369) {
    u_value v_x = u_field(s368, 0);
    u_value v_e = u_field(s368, 1);
    t369 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t369) {
    u_no_case(s368);
  }
  return t369;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s370 = v_b;
  u_value t371 = NULL;
  if ((!t371) && u_is_con(s370, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s370, 0);
    u_value v_g = u_field(s370, 1);
    u_value v_e = u_field(s370, 2);
    t371 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t371) {
    u_no_case(s370);
  }
  return t371;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s372 = v_p;
  u_value t373 = NULL;
  if ((!t373) && u_is_con(s372, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s372, 0);
    t373 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t373) && u_is_con(s372, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s372, 0);
    u_value v_args = u_field(s372, 1);
    u_value t374 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e375 = &t374;
    u_value l376 = v_args;
    while (u_is_con(l376, &n_list_46_Cons)) {
      u_value v_a = u_field(l376, 0);
      if (u_truth(u_mk_bool(1))) {
        e375 = u_append(e375, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l376 = u_field(l376, 1);
    }
    t373 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t374);
  }
  if ((!t373) && u_is_con(s372, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s372, 0);
    u_value t377 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e378 = &t377;
    u_value l379 = v_ps;
    while (u_is_con(l379, &n_list_46_Cons)) {
      u_value v_q = u_field(l379, 0);
      if (u_truth(u_mk_bool(1))) {
        e378 = u_append(e378, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l379 = u_field(l379, 1);
    }
    t373 = u_mk_con(&n_syntax_46_PTuple, 1, t377);
  }
  if (!t373) {
    t373 = v_p;
  }
  if (!t373) {
    u_no_case(s372);
  }
  return t373;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s380 = v_x;
  u_value t381 = NULL;
  if ((!t381) && u_is_con(s380, &n_syntax_46_SSkip)) {
    t381 = v_x;
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s380, 0);
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
    t381 = u_mk_con(&n_syntax_46_SAssign, 1, t382);
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s380, 0);
    u_value v_ys = u_field(s380, 1);
    u_value t385 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e386 = &t385;
    u_value l387 = v_ys;
    while (u_is_con(l387, &n_list_46_Cons)) {
      u_value v_y = u_field(l387, 0);
      if (u_truth(u_mk_bool(1))) {
        e386 = u_append(e386, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l387 = u_field(l387, 1);
    }
    t381 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t385);
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s380, 0);
    u_value v_l = u_field(s380, 1);
    u_value v_r = u_field(s380, 2);
    t381 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s380, 0);
    u_value v_b = u_field(s380, 1);
    t381 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s380, 0);
    u_value v_ins = u_field(s380, 1);
    u_value v_outs = u_field(s380, 2);
    u_value v_refs = u_field(s380, 3);
    u_value t388 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e389 = &t388;
    u_value l390 = v_ins;
    while (u_is_con(l390, &n_list_46_Cons)) {
      u_value v_e = u_field(l390, 0);
      if (u_truth(u_mk_bool(1))) {
        e389 = u_append(e389, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l390 = u_field(l390, 1);
    }
    t381 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t388, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s380, 0);
    u_value v_bs = u_field(s380, 1);
    u_value t391 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e392 = &t391;
    u_value l393 = v_bs;
    while (u_is_con(l393, &n_list_46_Cons)) {
      u_value v_b = u_field(l393, 0);
      if (u_truth(u_mk_bool(1))) {
        e392 = u_append(e392, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l393 = u_field(l393, 1);
    }
    t381 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t391);
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s380, 0);
    t381 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s380, 0);
    u_value v_t = u_field(s380, 1);
    t381 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t381) && u_is_con(s380, &n_syntax_46_STry)) {
    u_value v_b = u_field(s380, 0);
    u_value v_hs = u_field(s380, 1);
    u_value t394 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e395 = &t394;
    u_value l396 = v_hs;
    while (u_is_con(l396, &n_list_46_Cons)) {
      u_value v_h = u_field(l396, 0);
      if (u_truth(u_mk_bool(1))) {
        e395 = u_append(e395, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l396 = u_field(l396, 1);
    }
    t381 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t394);
  }
  if (!t381) {
    u_no_case(s380);
  }
  return t381;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s397 = v_b;
  u_value t398 = NULL;
  if ((!t398) && u_is_con(s397, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s397, 0);
    u_value v_g = u_field(s397, 1);
    u_value v_y = u_field(s397, 2);
    t398 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t398) {
    u_no_case(s397);
  }
  return t398;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s399 = v_h;
  u_value t400 = NULL;
  if ((!t400) && u_is_con(s399, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s399, 0);
    u_value v_p = u_field(s399, 1);
    u_value v_g = u_field(s399, 2);
    u_value v_b = u_field(s399, 3);
    t400 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t400) {
    u_no_case(s399);
  }
  return t400;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s401 = v_c;
  u_value t402 = NULL;
  if ((!t402) && u_is_con(s401, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s401, 0);
    u_value v_ps = u_field(s401, 1);
    u_value v_xss = u_field(s401, 2);
    u_value v_r = u_field(s401, 3);
    u_value v_b = u_field(s401, 4);
    u_value t403 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e404 = &t403;
    u_value l405 = v_xss;
    while (u_is_con(l405, &n_list_46_Cons)) {
      u_value v_xs = u_field(l405, 0);
      if (u_truth(u_mk_bool(1))) {
        e404 = u_append(e404, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l405 = u_field(l405, 1);
    }
    t402 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t403, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t402) && u_is_con(s401, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s401, 0);
    u_value v_ps = u_field(s401, 1);
    u_value v_ins = u_field(s401, 2);
    u_value v_outs = u_field(s401, 3);
    u_value v_refs = u_field(s401, 4);
    u_value v_b = u_field(s401, 5);
    t402 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t402) && u_is_con(s401, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s401, 0);
    t402 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t402) && u_is_con(s401, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s401, 0);
    u_value v_b = u_field(s401, 1);
    t402 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t402) && u_is_con(s401, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s401, 0);
    u_value v_b = u_field(s401, 1);
    t402 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t402) {
    t402 = v_c;
  }
  if (!t402) {
    u_no_case(s401);
  }
  return t402;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t406;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t406 = u_mk_con(&n_option_46_None, 0);
  } else {
    t406 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t406;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s407 = v_x;
  u_value t408 = NULL;
  if ((!t408) && u_is_con(s407, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s407, 1);
    t408 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SIf)) {
    u_value v_l = u_field(s407, 1);
    u_value v_r = u_field(s407, 2);
    t408 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r));
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SWhile)) {
    u_value v_b = u_field(s407, 1);
    t408 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SMatch)) {
    u_value v_bs = u_field(s407, 1);
    t408 = f_check_46_unknown_in_branches_1(v_bs);
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_SRaise)) {
    u_value v_t = u_field(s407, 1);
    t408 = f_check_46_unknown_type_1(v_t);
  }
  if ((!t408) && u_is_con(s407, &n_syntax_46_STry)) {
    u_value v_b = u_field(s407, 0);
    u_value v_hs = u_field(s407, 1);
    t408 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t408) {
    t408 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t408) {
    u_no_case(s407);
  }
  return t408;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s409 = v_xs;
  u_value t410 = NULL;
  if ((!t410) && u_is_con(s409, &n_list_46_Nil)) {
    t410 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t410) && u_is_con(s409, &n_list_46_Cons)) {
    u_value v_y = u_field(s409, 0);
    u_value v_ys = u_field(s409, 1);
    t410 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t410) {
    u_no_case(s409);
  }
  return t410;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s411 = v_bs;
  u_value t412 = NULL;
  if ((!t412) && u_is_con(s411, &n_list_46_Nil)) {
    t412 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t412) && (u_is_con(s411, &n_list_46_Cons) && u_is_con(u_field(s411, 0), &n_syntax_46_Branch))) {
    u_value v_y = u_field(u_field(s411, 0), 2);
    u_value v_rest = u_field(s411, 1);
    t412 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t412) {
    u_no_case(s411);
  }
  return t412;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s413 = v_hs;
  u_value t414 = NULL;
  if ((!t414) && u_is_con(s413, &n_list_46_Nil)) {
    t414 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t414) && (u_is_con(s413, &n_list_46_Cons) && (u_is_con(u_field(s413, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s413, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s413, 0), 3);
    u_value v_rest = u_field(s413, 1);
    t414 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t414) && (u_is_con(s413, &n_list_46_Cons) && u_is_con(u_field(s413, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s413, 0), 0);
    u_value v_b = u_field(u_field(s413, 0), 3);
    u_value v_rest = u_field(s413, 1);
    t414 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t414) {
    u_no_case(s413);
  }
  return t414;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s415 = v_c;
  u_value t416 = NULL;
  if ((!t416) && u_is_con(s415, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s415, 5);
    t416 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t416) && u_is_con(s415, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s415, 1);
    t416 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t416) && u_is_con(s415, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s415, 1);
    t416 = f_check_46_unknown_in_1(v_b);
  }
  if (!t416) {
    t416 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t416) {
    u_no_case(s415);
  }
  return t416;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s417 = v_p;
  u_value t418 = NULL;
  if ((!t418) && u_is_con(s417, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s417, 0);
    u_value v_args = u_field(s417, 1);
    t418 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t418) && u_is_con(s417, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s417, 0);
    t418 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t418) && u_is_con(s417, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s417, 0);
    t418 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t418) {
    t418 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t418) {
    u_no_case(s417);
  }
  return t418;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s419 = v_rows;
  u_value t420 = NULL;
  if ((!t420) && u_is_con(s419, &n_list_46_Nil)) {
    t420 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t420) && (u_is_con(s419, &n_list_46_Cons) && u_is_con(u_field(s419, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s419, 0), 0);
    u_value v_rest = u_field(s419, 1);
    u_value s421 = f_check_46_head_of_1(v_p);
    u_value t422 = NULL;
    if ((!t422) && u_is_con(s421, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s421, 0), 0);
      t422 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t422) && u_is_con(s421, &n_option_46_None)) {
      t422 = f_check_46_heads_1(v_rest);
    }
    if (!t422) {
      u_no_case(s421);
    }
    t420 = t422;
  }
  if ((!t420) && (u_is_con(s419, &n_list_46_Cons) && u_is_con(u_field(s419, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s419, 1);
    t420 = f_check_46_heads_1(v_rest);
  }
  if (!t420) {
    u_no_case(s419);
  }
  return t420;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s423 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t424 = NULL;
  if ((!t424) && u_is_con(s423, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s423, 0), 0);
    u_value v_ks = u_field(u_field(s423, 0), 1);
    u_value t425 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e426 = &t425;
    u_value l427 = v_ks;
    while (u_is_con(l427, &n_list_46_Cons)) {
      u_value v_a = u_field(l427, 0);
      if (u_truth(u_mk_bool(1))) {
        e426 = u_append(e426, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l427 = u_field(l427, 1);
    }
    t424 = t425;
  }
  if ((!t424) && u_is_con(s423, &n_option_46_None)) {
    t424 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t424) {
    u_no_case(s423);
  }
  return t424;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s428 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t429 = NULL;
  if ((!t429) && (u_is_con(s428, &n_syntax_46_TSort) && (u_is_con(u_field(s428, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s428, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s428, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s428, 1), &n_list_46_Nil)))))) {
    t429 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s428, 0);
    t429 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s428, 0);
    u_value v_args = u_field(s428, 1);
    u_value s430 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t431 = NULL;
    if ((!t431) && u_is_con(s430, &n_option_46_Some)) {
      u_value v_ks = u_field(s430, 0);
      u_value t432 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e433 = &t432;
      u_value l434 = v_ks;
      while (u_is_con(l434, &n_list_46_Cons)) {
        u_value v_k = u_field(l434, 0);
        if (u_truth(u_mk_bool(1))) {
          e433 = u_append(e433, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l434 = u_field(l434, 1);
      }
      t431 = u_mk_con(&n_option_46_Some, 1, t432);
    }
    if ((!t431) && u_is_con(s430, &n_option_46_None)) {
      t431 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t431) {
      u_no_case(s430);
    }
    t429 = t431;
  }
  if (!t429) {
    t429 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t429) {
    u_no_case(s428);
  }
  return t429;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s435 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t436 = NULL;
  if ((!t436) && u_is_con(s435, &n_option_46_Some)) {
    u_value v_ks = u_field(s435, 0);
    u_value s437 = f_list_46_lookup_2(v_h, v_ks);
    u_value t438 = NULL;
    if ((!t438) && u_is_con(s437, &n_option_46_Some)) {
      u_value v_ts = u_field(s437, 0);
      t438 = v_ts;
    }
    if ((!t438) && u_is_con(s437, &n_option_46_None)) {
      t438 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t438) {
      u_no_case(s437);
    }
    t436 = t438;
  }
  if ((!t436) && u_is_con(s435, &n_option_46_None)) {
    t436 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t436) {
    u_no_case(s435);
  }
  return t436;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s439 = v_ks;
  u_value t440 = NULL;
  if ((!t440) && u_is_con(s439, &n_list_46_Nil)) {
    t440 = u_mk_bool(1);
  }
  if ((!t440) && u_is_con(s439, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s439, 0), 0);
    u_value v_rest = u_field(s439, 1);
    u_value t441 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t441)) {
      t441 = f_check_46_covers_2(v_hs, v_rest);
    }
    t440 = t441;
  }
  if (!t440) {
    u_no_case(s439);
  }
  return t440;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s442 = v_rows;
  u_value t443 = NULL;
  if ((!t443) && u_is_con(s442, &n_list_46_Nil)) {
    t443 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t443) && (u_is_con(s442, &n_list_46_Cons) && u_is_con(u_field(s442, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s442, 0), 0);
    u_value v_ps = u_field(u_field(s442, 0), 1);
    u_value v_rest = u_field(s442, 1);
    u_value s444 = f_check_46_head_of_1(v_p);
    u_value t445 = NULL;
    if ((!t445) && u_is_con(s444, &n_option_46_Some)) {
      t445 = f_check_46_defaults_1(v_rest);
    }
    if ((!t445) && u_is_con(s444, &n_option_46_None)) {
      t445 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t445) {
      u_no_case(s444);
    }
    t443 = t445;
  }
  if ((!t443) && (u_is_con(s442, &n_list_46_Cons) && u_is_con(u_field(s442, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s442, 1);
    t443 = f_check_46_defaults_1(v_rest);
  }
  if (!t443) {
    u_no_case(s442);
  }
  return t443;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s446 = v_rows;
  u_value t447 = NULL;
  if ((!t447) && u_is_con(s446, &n_list_46_Nil)) {
    t447 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t447) && (u_is_con(s446, &n_list_46_Cons) && u_is_con(u_field(s446, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s446, 0), 0);
    u_value v_ps = u_field(u_field(s446, 0), 1);
    u_value v_rest = u_field(s446, 1);
    u_value s448 = f_check_46_head_of_1(v_p);
    u_value t449 = NULL;
    if ((!t449) && u_is_con(s448, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s448, 0), 0);
      u_value v_args = u_field(u_field(s448, 0), 1);
      u_value t450;
      if (u_truth(u_eq(v_g, v_h))) {
        t450 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t450 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t449 = t450;
    }
    if ((!t449) && u_is_con(s448, &n_option_46_None)) {
      t449 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t449) {
      u_no_case(s448);
    }
    t447 = t449;
  }
  if ((!t447) && (u_is_con(s446, &n_list_46_Cons) && u_is_con(u_field(s446, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s446, 1);
    t447 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t447) {
    u_no_case(s446);
  }
  return t447;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s451 = u_mk_tuple(2, v_q, v_ts);
  u_value t452 = NULL;
  if ((!t452) && u_is_con(u_field(s451, 0), &n_list_46_Nil)) {
    t452 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t452) && (u_is_con(u_field(s451, 0), &n_list_46_Cons) && u_is_con(u_field(s451, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s451, 0), 0);
    u_value v_ps = u_field(u_field(s451, 0), 1);
    u_value v_t = u_field(u_field(s451, 1), 0);
    u_value v_rest = u_field(u_field(s451, 1), 1);
    u_value s453 = f_check_46_head_of_1(v_p);
    u_value t454 = NULL;
    if ((!t454) && u_is_con(s453, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s453, 0), 0);
      u_value v_args = u_field(u_field(s453, 0), 1);
      t454 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t454) && u_is_con(s453, &n_option_46_None)) {
      u_value s455 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t456 = NULL;
      if ((!t456) && u_is_con(s455, &n_option_46_Some)) {
        u_value v_ks = u_field(s455, 0);
        u_value t457;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t457 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t457 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t456 = t457;
      }
      if ((!t456) && u_is_con(s455, &n_option_46_None)) {
        t456 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t456) {
        u_no_case(s455);
      }
      t454 = t456;
    }
    if (!t454) {
      u_no_case(s453);
    }
    t452 = t454;
  }
  if ((!t452) && (u_is_con(u_field(s451, 0), &n_list_46_Cons) && u_is_con(u_field(s451, 1), &n_list_46_Nil))) {
    t452 = u_mk_bool(0);
  }
  if (!t452) {
    u_no_case(s451);
  }
  return t452;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s458 = v_ks;
  u_value t459 = NULL;
  if ((!t459) && u_is_con(s458, &n_list_46_Nil)) {
    t459 = u_mk_bool(0);
  }
  if ((!t459) && u_is_con(s458, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s458, 0), 0);
    u_value v_args = u_field(u_field(s458, 0), 1);
    u_value v_more = u_field(s458, 1);
    u_value t460 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t460)) {
      t460 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t459 = t460;
  }
  if (!t459) {
    u_no_case(s458);
  }
  return t459;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s461 = v_h;
  u_value t462 = NULL;
  if ((!t462) && u_is_con(s461, &n_check_46_HCon)) {
    u_value v_n = u_field(s461, 0);
    u_value t463;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t463 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t463 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t462 = t463;
  }
  if ((!t462) && u_is_con(s461, &n_check_46_HLit)) {
    u_value v_v = u_field(s461, 0);
    t462 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t462) {
    u_no_case(s461);
  }
  return t462;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s464 = v_ts;
  u_value t465 = NULL;
  if ((!t465) && u_is_con(s464, &n_list_46_Nil)) {
    u_value t466;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t466 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t466 = u_mk_con(&n_option_46_None, 0);
    }
    t465 = t466;
  }
  if ((!t465) && u_is_con(s464, &n_list_46_Cons)) {
    u_value v_t = u_field(s464, 0);
    u_value v_rest = u_field(s464, 1);
    t465 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t465) {
    u_no_case(s464);
  }
  return t465;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s467 = v_cs;
  u_value t468 = NULL;
  if ((!t468) && u_is_con(s467, &n_option_46_Some)) {
    u_value v_ks = u_field(s467, 0);
    u_value t469;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t469 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t469 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t468 = t469;
  }
  if ((!t468) && u_is_con(s467, &n_option_46_None)) {
    t468 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t468) {
    u_no_case(s467);
  }
  return t468;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s470 = v_ks;
  u_value t471 = NULL;
  if ((!t471) && u_is_con(s470, &n_list_46_Nil)) {
    t471 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t471) && u_is_con(s470, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s470, 0), 0);
    u_value v_args = u_field(u_field(s470, 0), 1);
    u_value v_more = u_field(s470, 1);
    u_value s472 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t473 = NULL;
    if ((!t473) && u_is_con(s472, &n_option_46_Some)) {
      u_value v_ws = u_field(s472, 0);
      t473 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t473) && u_is_con(s472, &n_option_46_None)) {
      t473 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t473) {
      u_no_case(s472);
    }
    t471 = t473;
  }
  if (!t471) {
    u_no_case(s470);
  }
  return t471;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s474 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t475 = NULL;
  if ((!t475) && u_is_con(s474, &n_option_46_Some)) {
    u_value v_ws = u_field(s474, 0);
    t475 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t475) && u_is_con(s474, &n_option_46_None)) {
    t475 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t475) {
    u_no_case(s474);
  }
  return t475;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s476 = v_cs;
  u_value t477 = NULL;
  if ((!t477) && u_is_con(s476, &n_option_46_Some)) {
    u_value v_ks = u_field(s476, 0);
    u_value t478;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t478 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t478 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t477 = t478;
  }
  if ((!t477) && u_is_con(s476, &n_option_46_None)) {
    t477 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t477) {
    u_no_case(s476);
  }
  return t477;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s479 = v_ks;
  u_value t480 = NULL;
  if ((!t480) && u_is_con(s479, &n_list_46_Nil)) {
    t480 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t480) && u_is_con(s479, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s479, 0), 0);
    u_value v_args = u_field(u_field(s479, 0), 1);
    u_value v_rest = u_field(s479, 1);
    u_value t481;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t481 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t481 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t480 = t481;
  }
  if (!t480) {
    u_no_case(s479);
  }
  return t480;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value s482 = *r_s;
    int d483 = 0;
    if ((!d483) && u_is_con(s482, &n_check_46_State)) {
      u_value v_c = u_field(s482, 0);
      u_value v_su = u_field(s482, 1);
      u_value v_n = u_field(s482, 2);
      u_value v_m = u_field(s482, 3);
      u_value v_ms = u_field(s482, 4);
      u_value v_vs = u_field(s482, 5);
      d483 = 1;
      {
        u_value t484 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(v_n, u_mk_int(1)));
        u_value t485 = u_mk_con(&n_check_46_State, 6, v_c, v_su, u_add(v_n, u_mk_int(1)), v_m, v_ms, v_vs);
        *r_t = t484;
        *r_s = t485;
      }
    }
    if (!d483) {
      u_no_case(s482);
    }
  }
}

void p_check_46_bind_meta(u_value v_i, u_value v_t, u_value *r_s) {
  {
    if (u_truth(f_check_46_occurs_3(f_check_46_sub_of_1(*r_s), v_i, v_t))) {
      {
        u_raise(u_mk_con(&n_check_46_InfiniteType, 2, u_mk_con(&n_syntax_46_TMeta, 1, v_i), f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t)), "check.TypeError");
      }
    }
    u_value s486 = *r_s;
    int d487 = 0;
    if ((!d487) && u_is_con(s486, &n_check_46_State)) {
      u_value v_c = u_field(s486, 0);
      u_value v_su = u_field(s486, 1);
      u_value v_n = u_field(s486, 2);
      u_value v_m = u_field(s486, 3);
      u_value v_ms = u_field(s486, 4);
      u_value v_vs = u_field(s486, 5);
      d487 = 1;
      {
        u_value t488 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_put_3(v_i, v_t, v_su), v_n, v_m, v_ms, v_vs);
        *r_s = t488;
      }
    }
    if (!d487) {
      u_no_case(s486);
    }
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t489 = f_check_46_resolve_2(f_check_46_sub_of_1(*r_s), v_t1);
    u_value t490 = f_check_46_resolve_2(f_check_46_sub_of_1(*r_s), v_t2);
    v_a = t489;
    v_b = t490;
    u_value s491 = u_mk_tuple(2, v_a, v_b);
    int d492 = 0;
    if ((!d492) && (u_is_con(u_field(s491, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s491, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s491, 0), 0);
      u_value v_j = u_field(u_field(s491, 1), 0);
      d492 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d492) && u_is_con(u_field(s491, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s491, 0), 0);
      d492 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d492) && u_is_con(u_field(s491, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s491, 1), 0);
      d492 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d492) && (u_is_con(u_field(s491, 0), &n_syntax_46_TParam) && u_is_con(u_field(s491, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s491, 0), 0);
      u_value v_y = u_field(u_field(s491, 1), 0);
      d492 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d492) && (u_is_con(u_field(s491, 0), &n_syntax_46_TSort) && u_is_con(u_field(s491, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s491, 0), 0);
      u_value v_xs = u_field(u_field(s491, 0), 1);
      u_value v_g = u_field(u_field(s491, 1), 0);
      u_value v_ys = u_field(u_field(s491, 1), 1);
      d492 = 1;
      {
        u_value t493 = u_eq(v_f, v_g);
        if (u_truth(t493)) {
          t493 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t493)) {
          {
            p_check_46_unifies(v_xs, v_ys, r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if ((!d492) && (u_is_con(u_field(s491, 0), &n_syntax_46_TFun) && u_is_con(u_field(s491, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s491, 0), 0);
      u_value v_r1 = u_field(u_field(s491, 0), 1);
      u_value v_ys = u_field(u_field(s491, 1), 0);
      u_value v_r2 = u_field(u_field(s491, 1), 1);
      d492 = 1;
      {
        if (u_truth(u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)))) {
          {
            p_check_46_unifies(f_list_46__43__43__2(v_xs, u_mk_con(&n_list_46_Cons, 2, v_r1, u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_ys, u_mk_con(&n_list_46_Cons, 2, v_r2, u_mk_con(&n_list_46_Nil, 0))), r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if ((!d492) && (u_is_con(u_field(s491, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s491, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s491, 0), 0);
      u_value v_ys = u_field(u_field(s491, 1), 0);
      d492 = 1;
      {
        if (u_truth(u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)))) {
          {
            p_check_46_unifies(v_xs, v_ys, r_s);
          }
        } else {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t2)), "check.TypeError");
          }
        }
      }
    }
    if (!d492) {
      d492 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d492) {
      u_no_case(s491);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s494 = u_mk_tuple(2, v_xs, v_ys);
    int d495 = 0;
    if ((!d495) && (u_is_con(u_field(s494, 0), &n_list_46_Cons) && u_is_con(u_field(s494, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s494, 0), 0);
      u_value v_xs1 = u_field(u_field(s494, 0), 1);
      u_value v_y = u_field(u_field(s494, 1), 0);
      u_value v_ys1 = u_field(u_field(s494, 1), 1);
      d495 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d495) && (u_is_con(u_field(s494, 0), &n_list_46_Nil) && u_is_con(u_field(s494, 1), &n_list_46_Nil))) {
      d495 = 1;
      {
      }
    }
    if (!d495) {
      d495 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d495) {
      u_no_case(s494);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s496 = v_xs;
    int d497 = 0;
    if ((!d497) && u_is_con(s496, &n_list_46_Nil)) {
      d497 = 1;
      {
        u_value t498 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t498;
      }
    }
    if ((!d497) && u_is_con(s496, &n_list_46_Cons)) {
      u_value v_more = u_field(s496, 1);
      d497 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t499 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t499;
      }
    }
    if (!d497) {
      u_no_case(s496);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s500 = v_ps;
    int d501 = 0;
    if ((!d501) && u_is_con(s500, &n_list_46_Nil)) {
      d501 = 1;
      {
        u_value t502 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t502;
      }
    }
    if ((!d501) && u_is_con(s500, &n_list_46_Cons)) {
      u_value v_p = u_field(s500, 0);
      u_value v_qs = u_field(s500, 1);
      d501 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t503 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t503;
      }
    }
    if (!d501) {
      u_no_case(s500);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s504 = v_sc;
    int d505 = 0;
    if (!d505) {
      u_value v_ps = u_field(s504, 0);
      u_value v_u = u_field(s504, 1);
      d505 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t506 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_u), v_m);
        *r_t = t506;
      }
    }
    if (!d505) {
      u_no_case(s504);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s507 = f_list_46_repeated_1(v_ns);
    int d508 = 0;
    if ((!d508) && u_is_con(s507, &n_option_46_Some)) {
      u_value v_n = u_field(s507, 0);
      d508 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d508) && u_is_con(s507, &n_option_46_None)) {
      d508 = 1;
      {
      }
    }
    if (!d508) {
      u_no_case(s507);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s509 = f_list_46_repeated_1(v_ns);
    int d510 = 0;
    if ((!d510) && u_is_con(s509, &n_option_46_Some)) {
      u_value v_n = u_field(s509, 0);
      d510 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d510) && u_is_con(s509, &n_option_46_None)) {
      d510 = 1;
      {
      }
    }
    if (!d510) {
      u_no_case(s509);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s511 = v_ns;
    int d512 = 0;
    if ((!d512) && (u_is_con(s511, &n_list_46_Cons) && u_is_con(u_field(s511, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s511, 0);
      d512 = 1;
      {
        u_value t513 = v_k;
        *r_m = t513;
      }
    }
    if ((!d512) && u_is_con(s511, &n_list_46_Nil)) {
      d512 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d512) {
      d512 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d512) {
      u_no_case(s511);
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
    u_value s514 = v_t;
    int d515 = 0;
    if ((!d515) && u_is_con(s514, &n_syntax_46_THole)) {
      d515 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d515) && u_is_con(s514, &n_syntax_46_TParam)) {
      d515 = 1;
      {
        u_value t516 = v_t;
        *r_u = t516;
      }
    }
    if ((!d515) && u_is_con(s514, &n_syntax_46_TMeta)) {
      d515 = 1;
      {
        u_value t517 = v_t;
        *r_u = t517;
      }
    }
    if ((!d515) && u_is_con(s514, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s514, 0);
      u_value v_r = u_field(s514, 1);
      d515 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t518 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t518;
      }
    }
    if ((!d515) && u_is_con(s514, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s514, 0);
      d515 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t519 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t519;
      }
    }
    if ((!d515) && u_is_con(s514, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s514, 0);
      u_value v_args = u_field(s514, 1);
      d515 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t520 = v_t;
            *r_u = t520;
          }
        } else {
          {
            u_value s521 = f_check_46_visible_names_3(*r_s, v_n, f_check_46_sorts_named_2(f_check_46_ctx_of_1(*r_s), v_n));
            int d522 = 0;
            if ((!d522) && (u_is_con(s521, &n_list_46_Cons) && u_is_con(u_field(s521, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s521, 0);
              d522 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d522) && u_is_con(s521, &n_list_46_Nil)) {
              d522 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d522) {
              u_value v_ms = s521;
              d522 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d522) {
              u_no_case(s521);
            }
          }
        }
      }
    }
    if (!d515) {
      u_no_case(s514);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s523 = f_ctx_46_data_of_2(f_check_46_ctx_of_1(*r_s), v_n);
    int d524 = 0;
    if ((!d524) && u_is_con(s523, &n_option_46_Some)) {
      u_value v_k = u_field(s523, 0);
      d524 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t525 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t525;
      }
    }
    if ((!d524) && u_is_con(s523, &n_option_46_None)) {
      d524 = 1;
      {
        u_value s526 = f_ctx_46_alias_of_2(f_check_46_ctx_of_1(*r_s), v_n);
        int d527 = 0;
        if ((!d527) && u_is_con(s526, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s526, 0), 0);
          u_value v_rhs = u_field(u_field(s526, 0), 1);
          d527 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t528 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t528;
          }
        }
        if ((!d527) && u_is_con(s526, &n_option_46_None)) {
          d527 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d527) {
          u_no_case(s526);
        }
      }
    }
    if (!d524) {
      u_no_case(s523);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s529 = v_ts;
    int d530 = 0;
    if ((!d530) && u_is_con(s529, &n_list_46_Nil)) {
      d530 = 1;
      {
        u_value t531 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t531;
      }
    }
    if ((!d530) && u_is_con(s529, &n_list_46_Cons)) {
      u_value v_t = u_field(s529, 0);
      u_value v_more = u_field(s529, 1);
      d530 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t532 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t532;
      }
    }
    if (!d530) {
      u_no_case(s529);
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
    u_value s533 = v_ts;
    int d534 = 0;
    if ((!d534) && u_is_con(s533, &n_list_46_Nil)) {
      d534 = 1;
      {
        u_value t535 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t535;
      }
    }
    if ((!d534) && u_is_con(s533, &n_list_46_Cons)) {
      u_value v_t = u_field(s533, 0);
      u_value v_more = u_field(s533, 1);
      d534 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t536 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t536;
      }
    }
    if (!d534) {
      u_no_case(s533);
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
    u_value s537 = v_xs;
    int d538 = 0;
    if ((!d538) && u_is_con(s537, &n_list_46_Nil)) {
      d538 = 1;
      {
        u_value t539 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t539;
      }
    }
    if ((!d538) && (u_is_con(s537, &n_list_46_Cons) && u_is_con(u_field(s537, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s537, 0), 0);
      u_value v_t = u_field(u_field(s537, 0), 1);
      u_value v_more = u_field(s537, 1);
      d538 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t540 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t540;
      }
    }
    if (!d538) {
      u_no_case(s537);
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
    u_value s541 = v_xs;
    int d542 = 0;
    if ((!d542) && u_is_con(s541, &n_list_46_Nil)) {
      d542 = 1;
      {
        u_value t543 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t543;
      }
    }
    if ((!d542) && (u_is_con(s541, &n_list_46_Cons) && u_is_con(u_field(s541, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s541, 0), 0);
      u_value v_t = u_field(u_field(s541, 0), 1);
      u_value v_more = u_field(s541, 1);
      d542 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t544 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t544;
      }
    }
    if (!d542) {
      u_no_case(s541);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s545 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d546 = 0;
    if ((!d546) && u_is_con(s545, &n_list_46_Nil)) {
      d546 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d546) && (u_is_con(s545, &n_list_46_Cons) && u_is_con(u_field(s545, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s545, 0), 0);
      u_value v_one = u_field(u_field(s545, 0), 1);
      d546 = 1;
      {
        u_value t547 = v_m1;
        u_value t548 = v_one;
        *r_m = t547;
        *r_sc = t548;
      }
    }
    if (!d546) {
      u_value v_cs = s545;
      d546 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s549 = f_check_46_with_arity_2(v_cs, v_k);
        int d550 = 0;
        if ((!d550) && (u_is_con(s549, &n_list_46_Cons) && u_is_con(u_field(s549, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s549, 0), 0);
          u_value v_one = u_field(u_field(s549, 0), 1);
          d550 = 1;
          {
            u_value t551 = v_m1;
            u_value t552 = v_one;
            *r_m = t551;
            *r_sc = t552;
          }
        }
        if ((!d550) && u_is_con(s549, &n_list_46_Nil)) {
          d550 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d550) {
          u_value v_more = s549;
          d550 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d550) {
          u_no_case(s549);
        }
      }
    }
    if (!d546) {
      u_no_case(s545);
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
    u_value t553 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t553;
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
    u_value s554 = v_e;
    int d555 = 0;
    if ((!d555) && u_is_con(s554, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s554, 0);
      d555 = 1;
      {
        u_value t556 = v_e;
        u_value t557 = f_check_46_literal_1(v_v);
        *r_e1 = t556;
        *r_t = t557;
      }
    }
    if ((!d555) && (u_is_con(s554, &n_syntax_46_EVar) && u_is_con(u_field(s554, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s554, 0), 0);
      u_value v_a = u_field(u_field(s554, 0), 1);
      d555 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, f_prelude_46__45__1(u_mk_int(1)), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t558 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t559 = f_syntax_46_typ_1(v_x);
        *r_e1 = t558;
        *r_t = t559;
      }
    }
    if ((!d555) && u_is_con(s554, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s554, 0);
      u_value v_args = u_field(s554, 1);
      d555 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d555) && u_is_con(s554, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s554, 0);
      u_value v_body = u_field(s554, 1);
      d555 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t560 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t561 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t560;
        *r_t = t561;
      }
    }
    if ((!d555) && u_is_con(s554, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s554, 0);
      u_value v_body = u_field(s554, 1);
      d555 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_sub_of_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t562 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t563 = v_u;
        *r_e1 = t562;
        *r_t = t563;
      }
    }
    if ((!d555) && u_is_con(s554, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s554, 0);
      u_value v_left = u_field(s554, 1);
      u_value v_right = u_field(s554, 2);
      d555 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t564 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t565 = v_u;
        *r_e1 = t564;
        *r_t = t565;
      }
    }
    if ((!d555) && u_is_con(s554, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s554, 0);
      u_value v_bound = u_field(s554, 1);
      u_value v_lists = u_field(s554, 2);
      u_value v_cond = u_field(s554, 3);
      d555 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t566 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t567 = f_check_46_tlist_1(v_u);
        *r_e1 = t566;
        *r_t = t567;
      }
    }
    if ((!d555) && u_is_con(s554, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s554, 0);
      d555 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t568 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t569 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t568;
        *r_t = t569;
      }
    }
    if ((!d555) && u_is_con(s554, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s554, 0);
      u_value v_cases = u_field(s554, 1);
      d555 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t570 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e571 = &t570;
        u_value l572 = v_bs;
        while (u_is_con(l572, &n_list_46_Cons)) {
          u_value v_br = u_field(l572, 0);
          if (u_truth(u_mk_bool(1))) {
            e571 = u_append(e571, f_check_46_case_of_1(v_br));
          }
          l572 = u_field(l572, 1);
        }
        p_check_46_coverage(*r_s, v_u, t570);
        u_value t573 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t573;
      }
    }
    if (!d555) {
      u_no_case(s554);
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
    u_value s574 = v_f;
    int d575 = 0;
    if ((!d575) && (u_is_con(s574, &n_syntax_46_EVar) && u_is_con(u_field(s574, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s574, 0), 0);
      u_value v_a = u_field(u_field(s574, 0), 1);
      d575 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t576 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t576;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t577 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t577;
          }
        }
      }
    }
    if (!d575) {
      d575 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t578 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t578;
      }
    }
    if (!d575) {
      u_no_case(s574);
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
    u_value s579 = u_mk_tuple(2, v_es, v_ts);
    int d580 = 0;
    if ((!d580) && (u_is_con(u_field(s579, 0), &n_list_46_Cons) && u_is_con(u_field(s579, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s579, 0), 0);
      u_value v_more = u_field(u_field(s579, 0), 1);
      u_value v_t = u_field(u_field(s579, 1), 0);
      u_value v_ts1 = u_field(u_field(s579, 1), 1);
      d580 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t581 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t581;
      }
    }
    if ((!d580) && (u_is_con(u_field(s579, 0), &n_list_46_Nil) && u_is_con(u_field(s579, 1), &n_list_46_Nil))) {
      d580 = 1;
      {
        u_value t582 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t582;
      }
    }
    if (!d580) {
      d580 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d580) {
      u_no_case(s579);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s583 = v_es;
    int d584 = 0;
    if ((!d584) && u_is_con(s583, &n_list_46_Nil)) {
      d584 = 1;
      {
        u_value t585 = u_mk_con(&n_list_46_Nil, 0);
        u_value t586 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t585;
        *r_ts = t586;
      }
    }
    if ((!d584) && u_is_con(s583, &n_list_46_Cons)) {
      u_value v_e = u_field(s583, 0);
      u_value v_more = u_field(s583, 1);
      d584 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t587 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t588 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t587;
        *r_ts = t588;
      }
    }
    if (!d584) {
      u_no_case(s583);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s589 = u_mk_tuple(2, v_xs, v_lists);
    int d590 = 0;
    if ((!d590) && (u_is_con(u_field(s589, 0), &n_list_46_Cons) && u_is_con(u_field(s589, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s589, 0), 0);
      u_value v_ys = u_field(u_field(s589, 0), 1);
      u_value v_e = u_field(u_field(s589, 1), 0);
      u_value v_es = u_field(u_field(s589, 1), 1);
      d590 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t591 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t591;
      }
    }
    if (!d590) {
      d590 = 1;
      {
        u_value t592 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t592;
      }
    }
    if (!d590) {
      u_no_case(s589);
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
    u_value s593 = v_cases;
    int d594 = 0;
    if ((!d594) && u_is_con(s593, &n_list_46_Nil)) {
      d594 = 1;
      {
        u_value t595 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t595;
      }
    }
    if ((!d594) && (u_is_con(s593, &n_list_46_Cons) && u_is_con(u_field(s593, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s593, 0), 0);
      u_value v_g = u_field(u_field(s593, 0), 1);
      u_value v_b = u_field(u_field(s593, 0), 2);
      u_value v_more = u_field(s593, 1);
      d594 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t596 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t596;
      }
    }
    if (!d594) {
      u_no_case(s593);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s597 = v_g;
    int d598 = 0;
    if ((!d598) && u_is_con(s597, &n_option_46_Some)) {
      u_value v_e = u_field(s597, 0);
      d598 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t599 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t599;
      }
    }
    if ((!d598) && u_is_con(s597, &n_option_46_None)) {
      d598 = 1;
      {
        u_value t600 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t600;
      }
    }
    if (!d598) {
      u_no_case(s597);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t601 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e602 = &t601;
    u_value l603 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l603, &n_list_46_Cons)) {
      u_value v_p = u_field(l603, 0);
      if (u_truth(u_mk_bool(1))) {
        e602 = u_append(e602, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l603 = u_field(l603, 1);
    }
    u_value s604 = f_check_46_missing_4(f_check_46_ctx_of_1(v_s), f_check_46_sub_of_1(v_s), t601, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d605 = 0;
    if ((!d605) && (u_is_con(s604, &n_option_46_Some) && u_is_con(u_field(s604, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s604, 0), 0);
      d605 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d605) {
      d605 = 1;
      {
      }
    }
    if (!d605) {
      u_no_case(s604);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s606 = v_cs;
  u_value t607 = NULL;
  if ((!t607) && u_is_con(s606, &n_list_46_Nil)) {
    t607 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t607) && (u_is_con(s606, &n_list_46_Cons) && u_equals(u_field(u_field(s606, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s606, 0), 0);
    u_value v_rest = u_field(s606, 1);
    t607 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t607) && (u_is_con(s606, &n_list_46_Cons) && u_equals(u_field(u_field(s606, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s606, 1);
    t607 = f_check_46_unguarded_1(v_rest);
  }
  if (!t607) {
    u_no_case(s606);
  }
  return t607;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s608 = v_cs;
    int d609 = 0;
    if ((!d609) && u_is_con(s608, &n_list_46_Nil)) {
      d609 = 1;
      {
      }
    }
    if ((!d609) && u_is_con(s608, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s608, 0), 0);
      u_value v_open = u_field(u_field(s608, 0), 1);
      u_value v_rest = u_field(s608, 1);
      d609 = 1;
      {
        u_value t610 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e611 = &t610;
        u_value l612 = v_before;
        while (u_is_con(l612, &n_list_46_Cons)) {
          u_value v_q = u_field(l612, 0);
          if (u_truth(u_mk_bool(1))) {
            e611 = u_append(e611, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l612 = u_field(l612, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_ctx_of_1(v_s), f_check_46_sub_of_1(v_s), t610, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d609) {
      u_no_case(s608);
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
    u_value s613 = v_p;
    int d614 = 0;
    if ((!d614) && u_is_con(s613, &n_syntax_46_PWild)) {
      d614 = 1;
      {
        u_value t615 = v_p;
        u_value t616 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t615;
        *r_xs = t616;
      }
    }
    if ((!d614) && u_is_con(s613, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s613, 0);
      d614 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t617 = v_p;
        u_value t618 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t617;
        *r_xs = t618;
      }
    }
    if ((!d614) && (u_is_con(s613, &n_syntax_46_PVar) && u_is_con(u_field(s613, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s613, 0), 0);
      u_value v_a = u_field(u_field(s613, 0), 1);
      d614 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_ctx_of_1(*r_s), v_n)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t619 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t620 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t619;
            *r_xs = t620;
          }
        }
      }
    }
    if ((!d614) && u_is_con(s613, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s613, 0);
      u_value v_args = u_field(s613, 1);
      d614 = 1;
      {
        u_value s621 = f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_ctx_of_1(*r_s), v_n));
        int d622 = 0;
        if ((!d622) && (u_is_con(s621, &n_list_46_Cons) && u_is_con(u_field(s621, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s621, 0);
          d622 = 1;
          {
            u_value t623 = v_k1;
            v_k = t623;
          }
        }
        if ((!d622) && u_is_con(s621, &n_list_46_Nil)) {
          d622 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d622) {
          u_value v_ks = s621;
          d622 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d622) {
          u_no_case(s621);
        }
        u_value s624 = f_ctx_46_constr_of_2(f_check_46_ctx_of_1(*r_s), v_k);
        int d625 = 0;
        if ((!d625) && u_is_con(s624, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s624, 0), 0);
          u_value v_ks = u_field(u_field(s624, 0), 1);
          u_value v_r = u_field(u_field(s624, 0), 2);
          d625 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t626 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e627 = &t626;
            u_value l628 = v_ks;
            while (u_is_con(l628, &n_list_46_Cons)) {
              u_value v_kt = u_field(l628, 0);
              if (u_truth(u_mk_bool(1))) {
                e627 = u_append(e627, f_check_46_subst_2(v_kt, v_m));
              }
              l628 = u_field(l628, 1);
            }
            p_check_46_binder_list(v_args, t626, &v_qs, r_xs, r_s);
            u_value t629 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t629;
          }
        }
        if ((!d625) && u_is_con(s624, &n_option_46_None)) {
          d625 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d625) {
          u_no_case(s624);
        }
      }
    }
    if ((!d614) && u_is_con(s613, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s613, 0);
      d614 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t630 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t630;
      }
    }
    if (!d614) {
      u_no_case(s613);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s631 = u_mk_tuple(2, v_ps, v_ts);
    int d632 = 0;
    if ((!d632) && (u_is_con(u_field(s631, 0), &n_list_46_Cons) && u_is_con(u_field(s631, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s631, 0), 0);
      u_value v_more = u_field(u_field(s631, 0), 1);
      u_value v_t = u_field(u_field(s631, 1), 0);
      u_value v_ts1 = u_field(u_field(s631, 1), 1);
      d632 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t633 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t634 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t633;
        *r_xs = t634;
      }
    }
    if ((!d632) && (u_is_con(u_field(s631, 0), &n_list_46_Nil) && u_is_con(u_field(s631, 1), &n_list_46_Nil))) {
      d632 = 1;
      {
        u_value t635 = u_mk_con(&n_list_46_Nil, 0);
        u_value t636 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t635;
        *r_xs = t636;
      }
    }
    if (!d632) {
      d632 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d632) {
      u_no_case(s631);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s637 = v_x;
    int d638 = 0;
    if ((!d638) && u_is_con(s637, &n_syntax_46_Var)) {
      u_value v_n = u_field(s637, 0);
      u_value v_a = u_field(s637, 1);
      d638 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s639 = f_list_46_lookup_2(v_n, v_env);
        int d640 = 0;
        if ((!d640) && u_is_con(s639, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s639, 0), 1);
          d640 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t641 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t641;
          }
        }
        if ((!d640) && u_is_con(s639, &n_option_46_None)) {
          d640 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d640) {
          u_no_case(s639);
        }
      }
    }
    if (!d638) {
      u_no_case(s637);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s642 = v_xs;
    int d643 = 0;
    if ((!d643) && u_is_con(s642, &n_list_46_Nil)) {
      d643 = 1;
      {
        u_value t644 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t644;
      }
    }
    if ((!d643) && u_is_con(s642, &n_list_46_Cons)) {
      u_value v_x = u_field(s642, 0);
      u_value v_more = u_field(s642, 1);
      d643 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t645 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t645;
      }
    }
    if (!d643) {
      u_no_case(s642);
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
    u_value s646 = v_x;
    int d647 = 0;
    if ((!d647) && u_is_con(s646, &n_syntax_46_SSkip)) {
      d647 = 1;
      {
        u_value t648 = v_x;
        *r_y = t648;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s646, 0);
      d647 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t649 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t649;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s646, 0);
      u_value v_body = u_field(s646, 1);
      d647 = 1;
      {
        p_check_46_signature(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t650 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t650;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s646, 0);
      u_value v_left = u_field(s646, 1);
      u_value v_right = u_field(s646, 2);
      d647 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t651 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t651;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s646, 0);
      u_value v_body = u_field(s646, 1);
      d647 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t652 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t652;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s646, 0);
      u_value v_ins = u_field(s646, 1);
      u_value v_outs = u_field(s646, 2);
      u_value v_refs = u_field(s646, 3);
      d647 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s646, 0);
      u_value v_cases = u_field(s646, 1);
      d647 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t653 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e654 = &t653;
        u_value l655 = v_bs;
        while (u_is_con(l655, &n_list_46_Cons)) {
          u_value v_br = u_field(l655, 0);
          if (u_truth(u_mk_bool(1))) {
            e654 = u_append(e654, f_check_46_case_of_1(v_br));
          }
          l655 = u_field(l655, 1);
        }
        p_check_46_coverage(*r_s, v_t, t653);
        u_value t656 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t656;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s646, 0);
      d647 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t657 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t657;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s646, 0);
      u_value v_a = u_field(s646, 1);
      d647 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t658 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t658;
      }
    }
    if ((!d647) && u_is_con(s646, &n_syntax_46_STry)) {
      u_value v_body = u_field(s646, 0);
      u_value v_handlers = u_field(s646, 1);
      d647 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t659 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t659;
      }
    }
    if (!d647) {
      u_no_case(s646);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s660 = v_xs;
    int d661 = 0;
    if ((!d661) && u_is_con(s660, &n_list_46_Nil)) {
      d661 = 1;
      {
        u_value t662 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t662;
      }
    }
    if ((!d661) && u_is_con(s660, &n_list_46_Cons)) {
      u_value v_x = u_field(s660, 0);
      u_value v_more = u_field(s660, 1);
      d661 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t663 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t663;
      }
    }
    if (!d661) {
      u_no_case(s660);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t668;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_664 = U_LIT("os_args");
    static struct u_object l_os_read_665 = U_LIT("os_read");
    static struct u_object l_os_write_666 = U_LIT("os_write");
    static struct u_object l_os_library_667 = U_LIT("os_library");
    t668 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_664), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_665), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_666), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_667), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t668 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t668;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t669 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e670 = &t669;
  static struct u_object l_Program_671 = U_LIT("Program");
  u_value l672 = f_ctx_46_named_2(v_c, &l_Program_671);
  while (u_is_con(l672, &n_list_46_Cons)) {
    u_value v_d = u_field(l672, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e670 = u_append(e670, v_d);
    }
    l672 = u_field(l672, 1);
  }
  u_value s673 = t669;
  u_value t674 = NULL;
  if ((!t674) && u_is_con(s673, &n_list_46_Cons)) {
    u_value v_d = u_field(s673, 0);
    static struct u_object l_read_675 = U_LIT("read");
    static struct u_object l_library_676 = U_LIT("library");
    t674 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_675), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_676), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t674) {
    t674 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t674) {
    u_no_case(s673);
  }
  return t674;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s677 = f_check_46_visible_names_3(*r_s, v_p, f_check_46_procs_named_2(f_check_46_ctx_of_1(*r_s), v_p));
    int d678 = 0;
    if ((!d678) && (u_is_con(s677, &n_list_46_Cons) && u_is_con(u_field(s677, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s677, 0);
      d678 = 1;
      {
        u_value t679 = v_q1;
        v_q = t679;
      }
    }
    if ((!d678) && u_is_con(s677, &n_list_46_Nil)) {
      d678 = 1;
      {
        u_value t680 = v_p;
        v_q = t680;
      }
    }
    if (!d678) {
      u_value v_qs = s677;
      d678 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d678) {
      u_no_case(s677);
    }
    u_value s681 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_ctx_of_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_ctx_of_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_ctx_of_1(*r_s)))));
    int d682 = 0;
    if ((!d682) && u_is_con(s681, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s681, 0), 0);
      u_value v_is = u_field(u_field(s681, 0), 1);
      u_value v_os = u_field(u_field(s681, 0), 2);
      u_value v_rs = u_field(u_field(s681, 0), 3);
      d682 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t683 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e684 = &t683;
        u_value l685 = v_is;
        while (u_is_con(l685, &n_list_46_Cons)) {
          u_value v_t = u_field(l685, 0);
          if (u_truth(u_mk_bool(1))) {
            e684 = u_append(e684, f_check_46_subst_2(v_t, v_m));
          }
          l685 = u_field(l685, 1);
        }
        p_check_46_checks(v_env, v_ins, t683, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t686 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e687 = &t686;
        u_value l688 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l688, &n_list_46_Cons)) {
          u_value v_t = u_field(l688, 0);
          if (u_truth(u_mk_bool(1))) {
            e687 = u_append(e687, f_check_46_subst_2(v_t, v_m));
          }
          l688 = u_field(l688, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t686, r_s);
        u_value t689 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t689;
      }
    }
    if ((!d682) && u_is_con(s681, &n_option_46_None)) {
      d682 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d682) {
      u_no_case(s681);
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
    u_value s690 = v_cases;
    int d691 = 0;
    if ((!d691) && u_is_con(s690, &n_list_46_Nil)) {
      d691 = 1;
      {
        u_value t692 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t692;
      }
    }
    if ((!d691) && (u_is_con(s690, &n_list_46_Cons) && u_is_con(u_field(s690, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s690, 0), 0);
      u_value v_g = u_field(u_field(s690, 0), 1);
      u_value v_b = u_field(u_field(s690, 0), 2);
      u_value v_more = u_field(s690, 1);
      d691 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t693 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t693;
      }
    }
    if (!d691) {
      u_no_case(s690);
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
    u_value s694 = v_hs;
    int d695 = 0;
    if ((!d695) && u_is_con(s694, &n_list_46_Nil)) {
      d695 = 1;
      {
        u_value t696 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t696;
      }
    }
    if ((!d695) && (u_is_con(s694, &n_list_46_Cons) && u_is_con(u_field(s694, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s694, 0), 0);
      u_value v_p = u_field(u_field(s694, 0), 1);
      u_value v_g = u_field(u_field(s694, 0), 2);
      u_value v_b = u_field(u_field(s694, 0), 3);
      u_value v_more = u_field(s694, 1);
      d695 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t697 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t697;
      }
    }
    if (!d695) {
      u_no_case(s694);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t698 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t698)) {
      t698 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t699 = t698;
    if (!u_truth(t699)) {
      t699 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t700 = t699;
    if (!u_truth(t700)) {
      t700 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t700)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t701 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t701)) {
      t701 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t702 = t701;
    if (!u_truth(t702)) {
      t702 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t702)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_703 = U_LIT("Bool");
    static struct u_object l_Int_704 = U_LIT("Int");
    static struct u_object l_Str_705 = U_LIT("Str");
    static struct u_object l_Char_706 = U_LIT("Char");
    static struct u_object l__45__62__707 = U_LIT("->");
    u_value t708 = f_list_46_contains_2(f_syntax_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_703, u_mk_con(&n_list_46_Cons, 2, &l_Int_704, u_mk_con(&n_list_46_Cons, 2, &l_Str_705, u_mk_con(&n_list_46_Cons, 2, &l_Char_706, u_mk_con(&n_list_46_Cons, 2, &l__45__62__707, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t708)) {
      t708 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t709 = t708;
    if (!u_truth(t709)) {
      t709 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t709)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t710 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e711 = &t710;
    u_value l712 = v_ts;
    while (u_is_con(l712, &n_list_46_Cons)) {
      u_value v_t = u_field(l712, 0);
      if (u_truth(u_mk_bool(1))) {
        e711 = u_append(e711, f_check_46_free_1(v_t));
      }
      l712 = u_field(l712, 1);
    }
    u_value t713 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t710)), f_check_46_params_1(v_ps));
    v_undeclared = t713;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s714 = v_cs;
    int d715 = 0;
    if ((!d715) && u_is_con(s714, &n_list_46_Nil)) {
      d715 = 1;
      {
      }
    }
    if ((!d715) && (u_is_con(s714, &n_list_46_Cons) && u_is_con(u_field(u_field(s714, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s714, 0), 0);
      u_value v_n = u_field(u_field(u_field(s714, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s714, 0), 1), 1);
      u_value v_rest = u_field(s714, 1);
      d715 = 1;
      {
        u_value t716 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t716;
        p_check_46_declare_sort(f_check_46_ctx_of_1(*r_s), v_n);
        u_value t717 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_ctx_of_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t717;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d715) && u_is_con(s714, &n_list_46_Cons)) {
      u_value v_rest = u_field(s714, 1);
      d715 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d715) {
      u_no_case(s714);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s718 = v_cs;
    int d719 = 0;
    if ((!d719) && u_is_con(s718, &n_list_46_Nil)) {
      d719 = 1;
      {
      }
    }
    if ((!d719) && (u_is_con(s718, &n_list_46_Cons) && u_is_con(u_field(u_field(s718, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s718, 0), 0);
      u_value v_n = u_field(u_field(u_field(s718, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s718, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s718, 0), 1), 2);
      u_value v_rest = u_field(s718, 1);
      d719 = 1;
      {
        u_value t720 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t720;
        p_check_46_declare_sort(f_check_46_ctx_of_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t721 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_ctx_of_1(*r_s), v_n, v_ps, v_u));
        *r_s = t721;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d719) && u_is_con(s718, &n_list_46_Cons)) {
      u_value v_rest = u_field(s718, 1);
      d719 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d719) {
      u_no_case(s718);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s722 = v_cs;
    int d723 = 0;
    if ((!d723) && u_is_con(s722, &n_list_46_Nil)) {
      d723 = 1;
      {
      }
    }
    if ((!d723) && (u_is_con(s722, &n_list_46_Cons) && u_is_con(u_field(u_field(s722, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s722, 0), 0);
      u_value v_n = u_field(u_field(u_field(s722, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s722, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s722, 0), 1), 2);
      u_value v_rest = u_field(s722, 1);
      d723 = 1;
      {
        u_value t724 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t724;
        p_check_46_unique_ids(v_ps);
        u_value t725 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e726 = &t725;
        u_value l727 = v_ks;
        while (u_is_con(l727, &n_list_46_Cons)) {
          u_value v_k = u_field(l727, 0);
          if (u_truth(u_mk_bool(1))) {
            e726 = u_append(e726, f_check_46_constr_name_1(v_k));
          }
          l727 = u_field(l727, 1);
        }
        u_value t728 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_ctx_of_1(*r_s), v_n, t725));
        *r_s = t728;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d723) && u_is_con(s722, &n_list_46_Cons)) {
      u_value v_rest = u_field(s722, 1);
      d723 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d723) {
      u_no_case(s722);
    }
  }
}

u_value f_check_46_constr_name_1(u_value v_k) {
  u_value s729 = v_k;
  u_value t730 = NULL;
  if ((!t730) && u_is_con(s729, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s729, 0);
    t730 = v_n;
  }
  if (!t730) {
    u_no_case(s729);
  }
  return t730;
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s731 = v_ks;
    int d732 = 0;
    if ((!d732) && u_is_con(s731, &n_list_46_Nil)) {
      d732 = 1;
      {
      }
    }
    if ((!d732) && (u_is_con(s731, &n_list_46_Cons) && u_is_con(u_field(s731, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s731, 0), 0);
      u_value v_args = u_field(u_field(s731, 0), 1);
      u_value v_rest = u_field(s731, 1);
      d732 = 1;
      {
        p_check_46_declare_name(f_check_46_ctx_of_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t733 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_ctx_of_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t733;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d732) {
      u_no_case(s731);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s734 = v_xss;
  u_value t735 = NULL;
  if ((!t735) && u_is_con(s734, &n_list_46_Nil)) {
    t735 = v_r;
  }
  if ((!t735) && u_is_con(s734, &n_list_46_Cons)) {
    u_value v_xs = u_field(s734, 0);
    u_value v_rest = u_field(s734, 1);
    t735 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t735) {
    u_no_case(s734);
  }
  return t735;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s736 = v_xss;
  u_value t737 = NULL;
  if ((!t737) && u_is_con(s736, &n_list_46_Nil)) {
    t737 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t737) && u_is_con(s736, &n_list_46_Cons)) {
    u_value v_xs = u_field(s736, 0);
    t737 = v_xs;
  }
  if (!t737) {
    u_no_case(s736);
  }
  return t737;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s738 = v_xss;
    int d739 = 0;
    if ((!d739) && u_is_con(s738, &n_list_46_Nil)) {
      d739 = 1;
      {
        u_value t740 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t740;
      }
    }
    if ((!d739) && u_is_con(s738, &n_list_46_Cons)) {
      u_value v_xs = u_field(s738, 0);
      u_value v_more = u_field(s738, 1);
      d739 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t741 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t741;
      }
    }
    if (!d739) {
      u_no_case(s738);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s742 = v_cs;
    int d743 = 0;
    if ((!d743) && u_is_con(s742, &n_list_46_Nil)) {
      d743 = 1;
      {
      }
    }
    if ((!d743) && (u_is_con(s742, &n_list_46_Cons) && u_is_con(u_field(u_field(s742, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s742, 0), 0);
      u_value v_n = u_field(u_field(u_field(s742, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s742, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s742, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s742, 0), 1), 3);
      u_value v_rest = u_field(s742, 1);
      d743 = 1;
      {
        u_value t744 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t744;
        p_check_46_overload(f_check_46_ctx_of_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t745 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_ctx_of_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t745;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d743) && u_is_con(s742, &n_list_46_Cons)) {
      u_value v_rest = u_field(s742, 1);
      d743 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d743) {
      u_no_case(s742);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s746 = v_cs;
    int d747 = 0;
    if ((!d747) && u_is_con(s746, &n_list_46_Nil)) {
      d747 = 1;
      {
      }
    }
    if ((!d747) && (u_is_con(s746, &n_list_46_Cons) && u_is_con(u_field(u_field(s746, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s746, 0), 0);
      u_value v_n = u_field(u_field(u_field(s746, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s746, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s746, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s746, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s746, 0), 1), 4);
      u_value v_rest = u_field(s746, 1);
      d747 = 1;
      {
        u_value t748 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t748;
        p_check_46_declare_name(f_check_46_ctx_of_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t749 = f_syntax_46_types_1(v_xs);
        v_ts = t749;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t750 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_ctx_of_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t750;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d747) && u_is_con(s746, &n_list_46_Cons)) {
      u_value v_rest = u_field(s746, 1);
      d747 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d747) {
      u_no_case(s746);
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
    u_value s751 = v_c;
    int d752 = 0;
    if ((!d752) && u_is_con(s751, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s751, 0);
      u_value v_ps = u_field(s751, 1);
      u_value v_bound = u_field(s751, 2);
      u_value v_res = u_field(s751, 3);
      u_value v_body = u_field(s751, 4);
      d752 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t753 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t753;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s751, 0);
      u_value v_ps = u_field(s751, 1);
      u_value v_ins = u_field(s751, 2);
      u_value v_outs = u_field(s751, 3);
      u_value v_refs = u_field(s751, 4);
      u_value v_body = u_field(s751, 5);
      d752 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t754 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t754;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s751, 0);
      d752 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t755 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t755;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s751, 0);
      u_value v_body = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t756 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t756;
      }
    }
    if ((!d752) && u_is_con(s751, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s751, 0);
      u_value v_body = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t757 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t757;
      }
    }
    if (!d752) {
      d752 = 1;
      {
        u_value t758 = v_c;
        *r_c1 = t758;
      }
    }
    if (!d752) {
      u_no_case(s751);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s759 = v_cs;
    int d760 = 0;
    if ((!d760) && u_is_con(s759, &n_list_46_Nil)) {
      d760 = 1;
      {
        u_value t761 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t761;
      }
    }
    if ((!d760) && u_is_con(s759, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s759, 0), 0);
      u_value v_c = u_field(u_field(s759, 0), 1);
      u_value v_more = u_field(s759, 1);
      d760 = 1;
      {
        u_value t762 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t762;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t763 = f_check_46_zonk_cmd_2(f_check_46_sub_of_1(*r_s), v_c1);
        v_c2 = t763;
        u_value s764 = f_check_46_unknown_in_cmd_1(v_c2);
        int d765 = 0;
        if ((!d765) && u_is_con(s764, &n_option_46_Some)) {
          u_value v_t = u_field(s764, 0);
          d765 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d765) && u_is_con(s764, &n_option_46_None)) {
          d765 = 1;
          {
          }
        }
        if (!d765) {
          u_no_case(s764);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t766 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t766;
      }
    }
    if (!d760) {
      u_no_case(s759);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s767 = v_cs;
  u_value t768 = NULL;
  if ((!t768) && u_is_con(s767, &n_list_46_Nil)) {
    t768 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t768) && (u_is_con(s767, &n_list_46_Cons) && u_is_con(u_field(u_field(s767, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s767, 0), 1), 0);
    u_value v_more = u_field(s767, 1);
    t768 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t768) && (u_is_con(s767, &n_list_46_Cons) && u_is_con(u_field(u_field(s767, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s767, 0), 1), 0);
    u_value v_more = u_field(s767, 1);
    t768 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t768) && u_is_con(s767, &n_list_46_Cons)) {
    u_value v_more = u_field(s767, 1);
    t768 = f_check_46_block_names_1(v_more);
  }
  if (!t768) {
    u_no_case(s767);
  }
  return t768;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s769 = v_p;
    int d770 = 0;
    if ((!d770) && u_is_con(s769, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s769, 0);
      d770 = 1;
      {
        u_value t771 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t771;
        u_value t772 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t772;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t773 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t773;
      }
    }
    if (!d770) {
      u_no_case(s769);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_774 = U_LIT("main");
    u_value t775 = f_check_46_start_2(&l_main_774, u_mk_con(&n_list_46_Nil, 0));
    v_s = t775;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t776 = f_check_46_zonk_type_2(f_check_46_sub_of_1(v_s), v_u);
    *r_t = t776;
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
  static struct u_object l_List_777 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_777), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_778 = U_LIT("list");
  static struct u_object l_List_779 = U_LIT("List");
  static struct u_object l_A_780 = U_LIT("A");
  static struct u_object l_Nil_781 = U_LIT("Nil");
  static struct u_object l_Cons_782 = U_LIT("Cons");
  static struct u_object l_A_783 = U_LIT("A");
  static struct u_object l_A_784 = U_LIT("A");
  static struct u_object l_list_785 = U_LIT("list");
  static struct u_object l_len_786 = U_LIT("len");
  static struct u_object l_A_787 = U_LIT("A");
  static struct u_object l_xs_788 = U_LIT("xs");
  static struct u_object l_A_789 = U_LIT("A");
  static struct u_object l_xs_790 = U_LIT("xs");
  static struct u_object l_Nil_791 = U_LIT("Nil");
  static struct u_object l_Cons_792 = U_LIT("Cons");
  static struct u_object l_ys_793 = U_LIT("ys");
  static struct u_object l__43__794 = U_LIT("+");
  static struct u_object l_len_795 = U_LIT("len");
  static struct u_object l_ys_796 = U_LIT("ys");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_778, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_779), u_mk_con(&n_list_46_Cons, 2, &l_A_780, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_781), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_782), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_783), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_784)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_785), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_786), u_mk_con(&n_list_46_Cons, 2, &l_A_787, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_788), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_789))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_790), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_791), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_792), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_793), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__794, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_795, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_796), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s797 = v_t;
  u_value t798 = NULL;
  if ((!t798) && u_is_con(s797, &n_c_46_CtVoid)) {
    static struct u_object l_void_799 = U_LIT("void");
    t798 = &l_void_799;
  }
  if ((!t798) && u_is_con(s797, &n_c_46_CtInt)) {
    static struct u_object l_int_800 = U_LIT("int");
    t798 = &l_int_800;
  }
  if ((!t798) && u_is_con(s797, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s797, 0);
    t798 = v_n;
  }
  if ((!t798) && (u_is_con(s797, &n_c_46_CtPtr) && u_is_con(u_field(s797, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s797, 0), 0);
    static struct u_object l__42__801 = U_LIT("*");
    t798 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__801);
  }
  if ((!t798) && u_is_con(s797, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s797, 0);
    static struct u_object l__32__42__802 = U_LIT(" *");
    t798 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__802);
  }
  if (!t798) {
    u_no_case(s797);
  }
  return t798;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s803 = v_t;
  u_value t804 = NULL;
  if ((!t804) && u_is_con(s803, &n_c_46_CtPtr)) {
    t804 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t804) {
    static struct u_object l__32__805 = U_LIT(" ");
    t804 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__805, v_x));
  }
  if (!t804) {
    u_no_case(s803);
  }
  return t804;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s806 = v_e;
  u_value t807 = NULL;
  if ((!t807) && u_is_con(s806, &n_c_46_CeVar)) {
    u_value v_x = u_field(s806, 0);
    t807 = v_x;
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeNum)) {
    u_value v_n = u_field(s806, 0);
    t807 = u_str_show(v_n);
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeStr)) {
    u_value v_s = u_field(s806, 0);
    t807 = u_str_quote(v_s);
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeCall)) {
    u_value v_f = u_field(s806, 0);
    u_value v_args = u_field(s806, 1);
    static struct u_object l__40__808 = U_LIT("(");
    static struct u_object l__44__32__809 = U_LIT(", ");
    u_value t810 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e811 = &t810;
    u_value l812 = v_args;
    while (u_is_con(l812, &n_list_46_Cons)) {
      u_value v_a = u_field(l812, 0);
      if (u_truth(u_mk_bool(1))) {
        e811 = u_append(e811, f_c_46_c_expr_1(v_a));
      }
      l812 = u_field(l812, 1);
    }
    static struct u_object l__41__813 = U_LIT(")");
    t807 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__808, u_concat(u_str_join(&l__44__32__809, t810), &l__41__813)));
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s806, 0);
    u_value v_a = u_field(s806, 1);
    t807 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s806, 0);
    u_value v_a = u_field(s806, 1);
    u_value v_b = u_field(s806, 2);
    static struct u_object l__32__814 = U_LIT(" ");
    static struct u_object l__32__815 = U_LIT(" ");
    t807 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__814, u_concat(v_o, u_concat(&l__32__815, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s806, 0);
    u_value v_i = u_field(s806, 1);
    static struct u_object l__91__816 = U_LIT("[");
    static struct u_object l__93__817 = U_LIT("]");
    t807 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__816, u_concat(f_c_46_c_expr_1(v_i), &l__93__817)));
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s806, 0);
    u_value v_f = u_field(s806, 1);
    static struct u_object l__45__62__818 = U_LIT("->");
    t807 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__818, v_f));
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeCast)) {
    u_value v_t = u_field(s806, 0);
    u_value v_a = u_field(s806, 1);
    static struct u_object l__40__819 = U_LIT("(");
    static struct u_object l__41__820 = U_LIT(")");
    t807 = u_concat(&l__40__819, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__820, f_c_46_c_operand_1(v_a))));
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeCond)) {
    u_value v_c = u_field(s806, 0);
    u_value v_a = u_field(s806, 1);
    u_value v_b = u_field(s806, 2);
    static struct u_object l__32__63__32__821 = U_LIT(" ? ");
    static struct u_object l__32__58__32__822 = U_LIT(" : ");
    t807 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__821, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__822, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t807) && u_is_con(s806, &n_c_46_CeInit)) {
    u_value v_es = u_field(s806, 0);
    static struct u_object l__123__823 = U_LIT("{");
    static struct u_object l__44__32__824 = U_LIT(", ");
    u_value t825 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e826 = &t825;
    u_value l827 = v_es;
    while (u_is_con(l827, &n_list_46_Cons)) {
      u_value v_a = u_field(l827, 0);
      if (u_truth(u_mk_bool(1))) {
        e826 = u_append(e826, f_c_46_c_expr_1(v_a));
      }
      l827 = u_field(l827, 1);
    }
    static struct u_object l__125__828 = U_LIT("}");
    t807 = u_concat(&l__123__823, u_concat(u_str_join(&l__44__32__824, t825), &l__125__828));
  }
  if (!t807) {
    u_no_case(s806);
  }
  return t807;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s829 = v_e;
  u_value t830 = NULL;
  if ((!t830) && u_is_con(s829, &n_c_46_CeVar)) {
    t830 = f_c_46_c_expr_1(v_e);
  }
  if ((!t830) && u_is_con(s829, &n_c_46_CeNum)) {
    u_value v_n = u_field(s829, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t830 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t830) && u_is_con(s829, &n_c_46_CeStr)) {
    t830 = f_c_46_c_expr_1(v_e);
  }
  if ((!t830) && u_is_con(s829, &n_c_46_CeCall)) {
    t830 = f_c_46_c_expr_1(v_e);
  }
  if ((!t830) && u_is_con(s829, &n_c_46_CeIndex)) {
    t830 = f_c_46_c_expr_1(v_e);
  }
  if ((!t830) && u_is_con(s829, &n_c_46_CeArrow)) {
    t830 = f_c_46_c_expr_1(v_e);
  }
  if (!t830) {
    static struct u_object l__40__831 = U_LIT("(");
    static struct u_object l__41__832 = U_LIT(")");
    t830 = u_concat(&l__40__831, u_concat(f_c_46_c_expr_1(v_e), &l__41__832));
  }
  if (!t830) {
    u_no_case(s829);
  }
  return t830;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s833 = v_s;
  u_value t834 = NULL;
  if ((!t834) && u_is_con(s833, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s833, 0);
    static struct u_object l__59__835 = U_LIT(";");
    t834 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__835)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t834) && (u_is_con(s833, &n_c_46_CsDecl) && u_is_con(u_field(s833, 2), &n_option_46_None))) {
    u_value v_t = u_field(s833, 0);
    u_value v_x = u_field(s833, 1);
    static struct u_object l__59__836 = U_LIT(";");
    t834 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__836)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t834) && (u_is_con(s833, &n_c_46_CsDecl) && u_is_con(u_field(s833, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s833, 0);
    u_value v_x = u_field(s833, 1);
    u_value v_e = u_field(u_field(s833, 2), 0);
    static struct u_object l__32__61__32__837 = U_LIT(" = ");
    static struct u_object l__59__838 = U_LIT(";");
    t834 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__837, u_concat(f_c_46_c_expr_1(v_e), &l__59__838)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t834) && u_is_con(s833, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s833, 0);
    u_value v_e = u_field(s833, 1);
    static struct u_object l__32__61__32__839 = U_LIT(" = ");
    static struct u_object l__59__840 = U_LIT(";");
    t834 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__839, u_concat(f_c_46_c_expr_1(v_e), &l__59__840)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t834) && (u_is_con(s833, &n_c_46_CsIf) && u_is_con(u_field(s833, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s833, 0);
    u_value v_l = u_field(s833, 1);
    static struct u_object l_if_32__40__841 = U_LIT("if (");
    static struct u_object l__41__32__123__842 = U_LIT(") {");
    static struct u_object l__32__32__843 = U_LIT("  ");
    static struct u_object l__125__844 = U_LIT("}");
    t834 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__841, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__842))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__843), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__844), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t834) && u_is_con(s833, &n_c_46_CsIf)) {
    u_value v_c = u_field(s833, 0);
    u_value v_l = u_field(s833, 1);
    u_value v_r = u_field(s833, 2);
    static struct u_object l_if_32__40__845 = U_LIT("if (");
    static struct u_object l__41__32__123__846 = U_LIT(") {");
    static struct u_object l__32__32__847 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__848 = U_LIT("} else {");
    static struct u_object l__32__32__849 = U_LIT("  ");
    static struct u_object l__125__850 = U_LIT("}");
    t834 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__845, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__846))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__847), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__848), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__849), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__850), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t834) && u_is_con(s833, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s833, 0);
    u_value v_b = u_field(s833, 1);
    static struct u_object l_while_32__40__851 = U_LIT("while (");
    static struct u_object l__41__32__123__852 = U_LIT(") {");
    static struct u_object l__32__32__853 = U_LIT("  ");
    static struct u_object l__125__854 = U_LIT("}");
    t834 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__851, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__852))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__853), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__854), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t834) && (u_is_con(s833, &n_c_46_CsReturn) && u_is_con(u_field(s833, 0), &n_option_46_None))) {
    static struct u_object l_return_59__855 = U_LIT("return;");
    t834 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__855), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t834) && (u_is_con(s833, &n_c_46_CsReturn) && u_is_con(u_field(s833, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s833, 0), 0);
    static struct u_object l_return_32__856 = U_LIT("return ");
    static struct u_object l__59__857 = U_LIT(";");
    t834 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__856, u_concat(f_c_46_c_expr_1(v_e), &l__59__857))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t834) && u_is_con(s833, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s833, 0);
    static struct u_object l__123__858 = U_LIT("{");
    static struct u_object l__32__32__859 = U_LIT("  ");
    static struct u_object l__125__860 = U_LIT("}");
    t834 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__858), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__859), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__860), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t834) {
    u_no_case(s833);
  }
  return t834;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t861 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e862 = &t861;
  u_value l863 = v_ss;
  while (u_is_con(l863, &n_list_46_Cons)) {
    u_value v_s = u_field(l863, 0);
    if (u_truth(u_mk_bool(1))) {
      e862 = u_append(e862, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l863 = u_field(l863, 1);
  }
  return f_list_46_concat_1(t861);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s864 = v_ps;
  u_value t865 = NULL;
  if ((!t865) && u_is_con(s864, &n_list_46_Nil)) {
    static struct u_object l_void_866 = U_LIT("void");
    t865 = &l_void_866;
  }
  if (!t865) {
    static struct u_object l__44__32__867 = U_LIT(", ");
    u_value t868 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e869 = &t868;
    u_value l870 = v_ps;
    while (u_is_con(l870, &n_list_46_Cons)) {
      u_value v_p = u_field(l870, 0);
      if (u_truth(u_mk_bool(1))) {
        e869 = u_append(e869, f_c_46_c_param_1(v_p));
      }
      l870 = u_field(l870, 1);
    }
    t865 = u_str_join(&l__44__32__867, t868);
  }
  if (!t865) {
    u_no_case(s864);
  }
  return t865;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s871 = v_p;
  u_value t872 = NULL;
  if (!t872) {
    u_value v_t = u_field(s871, 0);
    u_value v_x = u_field(s871, 1);
    t872 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t872) {
    u_no_case(s871);
  }
  return t872;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s873 = v_d;
  u_value t874 = NULL;
  if ((!t874) && u_is_con(s873, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s873, 0);
    static struct u_object l__35_include_32__875 = U_LIT("#include ");
    t874 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__875, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t874) && u_is_con(s873, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s873, 0);
    u_value v_f = u_field(s873, 1);
    u_value v_ps = u_field(s873, 2);
    static struct u_object l__40__876 = U_LIT("(");
    static struct u_object l__41__59__877 = U_LIT(");");
    t874 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__876, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__877))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t874) && u_is_con(s873, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s873, 0);
    u_value v_f = u_field(s873, 1);
    u_value v_ps = u_field(s873, 2);
    u_value v_b = u_field(s873, 3);
    static struct u_object l__40__878 = U_LIT("(");
    static struct u_object l__41__32__123__879 = U_LIT(") {");
    static struct u_object l__32__32__880 = U_LIT("  ");
    static struct u_object l__125__881 = U_LIT("}");
    t874 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__878, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__879))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__880, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__881, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t874) && (u_is_con(s873, &n_c_46_CdGlobal) && u_is_con(u_field(s873, 2), &n_option_46_None))) {
    u_value v_t = u_field(s873, 0);
    u_value v_x = u_field(s873, 1);
    static struct u_object l__59__882 = U_LIT(";");
    t874 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__882), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t874) && (u_is_con(s873, &n_c_46_CdGlobal) && u_is_con(u_field(s873, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s873, 0);
    u_value v_x = u_field(s873, 1);
    u_value v_e = u_field(u_field(s873, 2), 0);
    static struct u_object l__32__61__32__883 = U_LIT(" = ");
    static struct u_object l__59__884 = U_LIT(";");
    t874 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__883, u_concat(f_c_46_c_expr_1(v_e), &l__59__884))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t874) {
    u_no_case(s873);
  }
  return t874;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s885 = v_f;
  u_value t886 = NULL;
  if ((!t886) && u_is_con(s885, &n_c_46_CFile)) {
    u_value v_ds = u_field(s885, 0);
    static struct u_object l__10__10__887 = U_LIT("\n\n");
    u_value t888 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e889 = &t888;
    u_value l890 = v_ds;
    while (u_is_con(l890, &n_list_46_Cons)) {
      u_value v_d = u_field(l890, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__891 = U_LIT("\n");
        e889 = u_append(e889, u_str_join(&l__10__891, f_c_46_c_decl_1(v_d)));
      }
      l890 = u_field(l890, 1);
    }
    static struct u_object l__10__892 = U_LIT("\n");
    t886 = u_concat(u_str_join(&l__10__10__887, t888), &l__10__892);
  }
  if (!t886) {
    u_no_case(s885);
  }
  return t886;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__893 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__893, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__894 = U_LIT("()[],;");
  return u_str_contains(&l__40__41__91__93__44__59__894, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t895 = u_is_letter(v_c);
  if (!u_truth(t895)) {
    t895 = u_is_digit(v_c);
  }
  u_value t896 = t895;
  if (!u_truth(t896)) {
    t896 = u_eq(v_c, u_mk_char(95));
  }
  return t896;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t897;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t897 = u_str_get(v_s, v_i);
  } else {
    t897 = u_chr(u_mk_int(0));
  }
  return t897;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s898 = v_c;
  u_value t899 = NULL;
  if ((!t899) && u_equals(s898, u_mk_char(34))) {
    t899 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t899) && u_equals(s898, u_mk_char(39))) {
    t899 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t899) && u_equals(s898, u_mk_char(92))) {
    t899 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t899) && u_equals(s898, u_mk_char(110))) {
    t899 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t899) && u_equals(s898, u_mk_char(116))) {
    t899 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t899) && u_equals(s898, u_mk_char(114))) {
    t899 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t899) {
    t899 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t899) {
    u_no_case(s898);
  }
  return t899;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t900;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t900 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t900 = v_i;
  }
  return t900;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t901 = u_mk_int(0);
    u_value t902 = u_mk_int(1);
    u_value t903 = u_mk_int(1);
    v_j = t901;
    *r_line = t902;
    *r_column = t903;
    u_value t908 = u_lt(v_j, v_i);
    while (u_truth(t908)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t904 = u_add(*r_line, u_mk_int(1));
            u_value t905 = u_mk_int(1);
            *r_line = t904;
            *r_column = t905;
          }
        } else {
          {
            u_value t906 = u_add(*r_column, u_mk_int(1));
            *r_column = t906;
          }
        }
        u_value t907 = u_add(v_j, u_mk_int(1));
        v_j = t907;
      }
      t908 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t909 = *r_i;
    u_value t910 = u_add(*r_i, u_mk_int(1));
    u_value t911 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t909;
    *r_i = t910;
    v_parts = t911;
    u_value t912 = *r_i;
    v_from = t912;
    u_value t923 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t923)) {
      {
        u_value t913 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t913;
        u_value t914 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t914)) {
          t914 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t914)) {
          {
            static struct u_object l_unterminated_32_lit_915 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_915, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s916 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d917 = 0;
            if ((!d917) && u_is_con(s916, &n_option_46_Some)) {
              u_value v_d = u_field(s916, 0);
              d917 = 1;
              {
                u_value t918 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t919 = u_add(*r_i, u_mk_int(2));
                v_parts = t918;
                *r_i = t919;
                u_value t920 = *r_i;
                v_from = t920;
              }
            }
            if ((!d917) && u_is_con(s916, &n_option_46_None)) {
              d917 = 1;
              {
                static struct u_object l_invalid_32_escape_921 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_921, *r_i), "lex.LexError");
              }
            }
            if (!d917) {
              u_no_case(s916);
            }
          }
        } else {
          {
            u_value t922 = u_add(*r_i, u_mk_int(1));
            *r_i = t922;
          }
        }
      }
      t923 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__924 = U_LIT("");
    u_value t925 = u_str_join(&l__924, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t926 = u_add(*r_i, u_mk_int(1));
    *r_text = t925;
    *r_i = t926;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t927 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t927;
    u_value t928 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t929 = v_j;
    *r_t = t928;
    *r_i = t929;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t930 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t930;
    u_value s931 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d932 = 0;
    if ((!d932) && u_is_con(s931, &n_option_46_Some)) {
      u_value v_n = u_field(s931, 0);
      d932 = 1;
      {
        u_value t933 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t934 = v_j;
        *r_t = t933;
        *r_i = t934;
      }
    }
    if ((!d932) && u_is_con(s931, &n_option_46_None)) {
      d932 = 1;
      {
        static struct u_object l_invalid_32_number_935 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_935, *r_i), "lex.LexError");
      }
    }
    if (!d932) {
      u_no_case(s931);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t936 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t936;
    u_value t937 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t938 = v_j;
    *r_t = t937;
    *r_i = t938;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t939 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t939;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t940 = *r_i;
    v_start = t940;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_941 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_941, v_start), "lex.LexError");
      }
    }
    u_value t942 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t942;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t943 = u_str_get(v_s, *r_i);
    v_c = t943;
    u_value s944 = v_c;
    int d945 = 0;
    if (!d945) {
      u_value t946 = u_is_letter(v_c);
      if (!u_truth(t946)) {
        t946 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t946)) {
        d945 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d945) {
      if (u_truth(u_is_digit(v_c))) {
        d945 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d945) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d945 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d945) && u_equals(s944, u_mk_char(34))) {
      d945 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d945) && u_equals(s944, u_mk_char(39))) {
      d945 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d945) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d945 = 1;
        {
          u_value t947 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t948 = u_add(*r_i, u_mk_int(1));
          *r_t = t947;
          *r_i = t948;
        }
      }
    }
    if (!d945) {
      d945 = 1;
      {
        static struct u_object l_unexpected_32_chara_949 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_949, *r_i), "lex.LexError");
      }
    }
    if (!d945) {
      u_no_case(s944);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value v_rev = NULL;
    u_value t950 = u_mk_int(0);
    u_value t951 = u_mk_con(&n_list_46_Nil, 0);
    v_i = t950;
    v_rev = t951;
    u_value t958 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t958)) {
      {
        u_value t952 = v_i;
        v_start = t952;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t953 = u_add(v_i, u_mk_int(1));
            v_i = t953;
          }
        } else {
          u_value t954 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t954)) {
            t954 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t954)) {
            {
              u_value t956 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam955, 0));
              v_i = t956;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t957 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t957;
            }
          }
        }
      }
      t958 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t959 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t959;
  }
}

u_value f_lex_46_tokens_1(u_value v_ts) {
  return f_list_46_firsts_1(v_ts);
}

u_value f_parse_46_scope_params_1(u_value v_sc) {
  u_value s960 = v_sc;
  u_value t961 = NULL;
  if ((!t961) && u_is_con(s960, &n_parse_46_Scope)) {
    u_value v_ps = u_field(s960, 0);
    t961 = v_ps;
  }
  if (!t961) {
    u_no_case(s960);
  }
  return t961;
}

u_value f_parse_46_scope_notations_1(u_value v_sc) {
  u_value s962 = v_sc;
  u_value t963 = NULL;
  if ((!t963) && u_is_con(s962, &n_parse_46_Scope)) {
    u_value v_ns = u_field(s962, 1);
    t963 = v_ns;
  }
  if (!t963) {
    u_no_case(s962);
  }
  return t963;
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
  static struct u_object l___964 = U_LIT("_");
  static struct u_object l_assert_965 = U_LIT("assert");
  static struct u_object l_assign_966 = U_LIT("assign");
  static struct u_object l_call_967 = U_LIT("call");
  static struct u_object l_case_968 = U_LIT("case");
  static struct u_object l_catch_969 = U_LIT("catch");
  static struct u_object l_data_970 = U_LIT("data");
  static struct u_object l_else_971 = U_LIT("else");
  static struct u_object l_end_972 = U_LIT("end");
  static struct u_object l_eval_973 = U_LIT("eval");
  static struct u_object l_false_974 = U_LIT("false");
  static struct u_object l_for_975 = U_LIT("for");
  static struct u_object l_function_976 = U_LIT("function");
  static struct u_object l_if_977 = U_LIT("if");
  static struct u_object l_import_978 = U_LIT("import");
  static struct u_object l_in_979 = U_LIT("in");
  static struct u_object l_infix_980 = U_LIT("infix");
  static struct u_object l_lambda_981 = U_LIT("lambda");
  static struct u_object l_let_982 = U_LIT("let");
  static struct u_object l_local_983 = U_LIT("local");
  static struct u_object l_match_984 = U_LIT("match");
  static struct u_object l_out_985 = U_LIT("out");
  static struct u_object l_postfix_986 = U_LIT("postfix");
  static struct u_object l_prefix_987 = U_LIT("prefix");
  static struct u_object l_procedure_988 = U_LIT("procedure");
  static struct u_object l_raise_989 = U_LIT("raise");
  static struct u_object l_ref_990 = U_LIT("ref");
  static struct u_object l_run_991 = U_LIT("run");
  static struct u_object l_skip_992 = U_LIT("skip");
  static struct u_object l_test_993 = U_LIT("test");
  static struct u_object l_then_994 = U_LIT("then");
  static struct u_object l_true_995 = U_LIT("true");
  static struct u_object l_type_996 = U_LIT("type");
  static struct u_object l_try_997 = U_LIT("try");
  static struct u_object l_while_998 = U_LIT("while");
  cache = u_mk_con(&n_list_46_Cons, 2, &l___964, u_mk_con(&n_list_46_Cons, 2, &l_assert_965, u_mk_con(&n_list_46_Cons, 2, &l_assign_966, u_mk_con(&n_list_46_Cons, 2, &l_call_967, u_mk_con(&n_list_46_Cons, 2, &l_case_968, u_mk_con(&n_list_46_Cons, 2, &l_catch_969, u_mk_con(&n_list_46_Cons, 2, &l_data_970, u_mk_con(&n_list_46_Cons, 2, &l_else_971, u_mk_con(&n_list_46_Cons, 2, &l_end_972, u_mk_con(&n_list_46_Cons, 2, &l_eval_973, u_mk_con(&n_list_46_Cons, 2, &l_false_974, u_mk_con(&n_list_46_Cons, 2, &l_for_975, u_mk_con(&n_list_46_Cons, 2, &l_function_976, u_mk_con(&n_list_46_Cons, 2, &l_if_977, u_mk_con(&n_list_46_Cons, 2, &l_import_978, u_mk_con(&n_list_46_Cons, 2, &l_in_979, u_mk_con(&n_list_46_Cons, 2, &l_infix_980, u_mk_con(&n_list_46_Cons, 2, &l_lambda_981, u_mk_con(&n_list_46_Cons, 2, &l_let_982, u_mk_con(&n_list_46_Cons, 2, &l_local_983, u_mk_con(&n_list_46_Cons, 2, &l_match_984, u_mk_con(&n_list_46_Cons, 2, &l_out_985, u_mk_con(&n_list_46_Cons, 2, &l_postfix_986, u_mk_con(&n_list_46_Cons, 2, &l_prefix_987, u_mk_con(&n_list_46_Cons, 2, &l_procedure_988, u_mk_con(&n_list_46_Cons, 2, &l_raise_989, u_mk_con(&n_list_46_Cons, 2, &l_ref_990, u_mk_con(&n_list_46_Cons, 2, &l_run_991, u_mk_con(&n_list_46_Cons, 2, &l_skip_992, u_mk_con(&n_list_46_Cons, 2, &l_test_993, u_mk_con(&n_list_46_Cons, 2, &l_then_994, u_mk_con(&n_list_46_Cons, 2, &l_true_995, u_mk_con(&n_list_46_Cons, 2, &l_type_996, u_mk_con(&n_list_46_Cons, 2, &l_try_997, u_mk_con(&n_list_46_Cons, 2, &l_while_998, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__999 = U_LIT("=");
  static struct u_object l__61__62__1000 = U_LIT("=>");
  static struct u_object l__58__61__1001 = U_LIT(":=");
  static struct u_object l__58__1002 = U_LIT(":");
  static struct u_object l__124__1003 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__999, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1000, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1001, u_mk_con(&n_list_46_Cons, 2, &l__58__1002, u_mk_con(&n_list_46_Cons, 2, &l__124__1003, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1004 = v_ns;
  u_value t1005 = NULL;
  if ((!t1005) && u_is_con(s1004, &n_list_46_Nil)) {
    t1005 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1005) && (u_is_con(s1004, &n_list_46_Cons) && (u_is_con(u_field(s1004, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1004, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1004, 0), 1);
    u_value v_ops = u_field(u_field(s1004, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1005 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1005) && u_is_con(s1004, &n_list_46_Cons)) {
    u_value v_more = u_field(s1004, 1);
    t1005 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1005) {
    u_no_case(s1004);
  }
  return t1005;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1006 = v_ns;
  u_value t1007 = NULL;
  if ((!t1007) && u_is_con(s1006, &n_list_46_Nil)) {
    t1007 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1007) && (u_is_con(s1006, &n_list_46_Cons) && (u_is_con(u_field(s1006, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1006, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1006, 0), 1);
    u_value v_ops = u_field(u_field(s1006, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1007 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1007) && u_is_con(s1006, &n_list_46_Cons)) {
    u_value v_more = u_field(s1006, 1);
    t1007 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1007) {
    u_no_case(s1006);
  }
  return t1007;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1008 = v_ns;
  u_value t1009 = NULL;
  if ((!t1009) && u_is_con(s1008, &n_list_46_Nil)) {
    t1009 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1009) && (u_is_con(s1008, &n_list_46_Cons) && u_is_con(u_field(s1008, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1008, 0), 0);
    u_value v_p = u_field(u_field(s1008, 0), 1);
    u_value v_ops = u_field(u_field(s1008, 0), 2);
    u_value t1010 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1010)) {
      t1010 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1011 = t1010;
    if (u_truth(t1011)) {
      t1011 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1011)) {
      t1009 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1009) && u_is_con(s1008, &n_list_46_Cons)) {
    u_value v_more = u_field(s1008, 1);
    t1009 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1009) {
    u_no_case(s1008);
  }
  return t1009;
}

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op) {
  u_value s1012 = v_ns;
  u_value t1013 = NULL;
  if ((!t1013) && u_is_con(s1012, &n_list_46_Nil)) {
    t1013 = u_mk_bool(0);
  }
  if ((!t1013) && (u_is_con(s1012, &n_list_46_Cons) && u_is_con(u_field(s1012, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1012, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1013 = u_mk_bool(1);
    }
  }
  if ((!t1013) && u_is_con(s1012, &n_list_46_Cons)) {
    u_value v_more = u_field(s1012, 1);
    t1013 = f_parse_46_declared_2(v_more, v_op);
  }
  if (!t1013) {
    u_no_case(s1012);
  }
  return t1013;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1014 = v_ts;
  u_value t1015 = NULL;
  if ((!t1015) && u_is_con(s1014, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1014, 0), 0);
    t1015 = v_t;
  }
  if (!t1015) {
    t1015 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1015) {
    u_no_case(s1014);
  }
  return t1015;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1016 = v_ts;
  u_value t1017 = NULL;
  if ((!t1017) && (u_is_con(s1016, &n_list_46_Cons) && u_is_con(u_field(s1016, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1016, 1), 0), 0);
    t1017 = v_t;
  }
  if (!t1017) {
    t1017 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1017) {
    u_no_case(s1016);
  }
  return t1017;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1018 = v_ts;
  u_value t1019 = NULL;
  if ((!t1019) && u_is_con(s1018, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1018, 0), 1);
    t1019 = v_i;
  }
  if (!t1019) {
    t1019 = u_mk_int(0);
  }
  if (!t1019) {
    u_no_case(s1018);
  }
  return t1019;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1020 = v_t;
  u_value t1021 = NULL;
  if ((!t1021) && u_is_con(s1020, &n_lex_46_TName)) {
    u_value v_n = u_field(s1020, 0);
    t1021 = v_n;
  }
  if ((!t1021) && u_is_con(s1020, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1020, 0);
    t1021 = v_s;
  }
  if ((!t1021) && u_is_con(s1020, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1020, 0);
    t1021 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1021) && u_is_con(s1020, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1020, 0);
    t1021 = u_str_show(v_n);
  }
  if ((!t1021) && u_is_con(s1020, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1020, 0);
    t1021 = u_str_quote(v_s);
  }
  if ((!t1021) && u_is_con(s1020, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1020, 0);
    static struct u_object l__39__1022 = U_LIT("'");
    static struct u_object l__39__1023 = U_LIT("'");
    t1021 = u_concat(&l__39__1022, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1023));
  }
  if ((!t1021) && u_is_con(s1020, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1024 = U_LIT("end of input");
    t1021 = &l_end_32_of_32_input_1024;
  }
  if (!t1021) {
    u_no_case(s1020);
  }
  return t1021;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1025 = f_parse_46_head_token_1(v_ts);
  u_value t1026 = NULL;
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1025, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1026 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TName)) {
    u_value v_n = u_field(s1025, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1026 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1026) {
    t1026 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1026) {
    u_no_case(s1025);
  }
  return t1026;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1027 = *r_ts;
    int d1028 = 0;
    if ((!d1028) && u_is_con(s1027, &n_list_46_Cons)) {
      u_value v_more = u_field(s1027, 1);
      d1028 = 1;
      {
        u_value t1029 = v_more;
        *r_ts = t1029;
      }
    }
    if (!d1028) {
      d1028 = 1;
      {
      }
    }
    if (!d1028) {
      u_no_case(s1027);
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
        static struct u_object l__32_expected_32_but_32__1030 = U_LIT(" expected but ");
        static struct u_object l__32_found_1031 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1030, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1031))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1032 = f_parse_46_head_token_1(*r_ts);
    int d1033 = 0;
    if ((!d1033) && u_is_con(s1032, &n_lex_46_TName)) {
      u_value v_m = u_field(s1032, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1033 = 1;
        {
          u_value t1034 = v_m;
          *r_n = t1034;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1033) {
      d1033 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1035 = U_LIT("name expected but ");
        static struct u_object l__32_found_1036 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1035, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1036)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1033) {
      u_no_case(s1032);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1037 = f_parse_46_head_token_1(*r_ts);
    int d1038 = 0;
    if ((!d1038) && u_is_con(s1037, &n_lex_46_TName)) {
      u_value v_m = u_field(s1037, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_scope_notations_1(v_sc), v_m))) {
        d1038 = 1;
        {
          static struct u_object l_operator_32__1039 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1040 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1039, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1040)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1038) {
      d1038 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1038) {
      u_no_case(s1037);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1041 = f_parse_46_head_token_1(*r_ts);
    int d1042 = 0;
    if ((!d1042) && u_is_con(s1041, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1041, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1042 = 1;
        {
          u_value t1043 = v_s;
          *r_n = t1043;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1042) {
      d1042 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1042) {
      u_no_case(s1041);
    }
  }
}

u_value f_parse_46_ops_notations_1(u_value v_o) {
  u_value s1044 = v_o;
  u_value t1045 = NULL;
  if ((!t1045) && u_is_con(s1044, &n_parse_46_Ops)) {
    u_value v_ns = u_field(s1044, 0);
    t1045 = v_ns;
  }
  if (!t1045) {
    u_no_case(s1044);
  }
  return t1045;
}

u_value f_parse_46_apply_prefix_3(u_value v_o, u_value v_op, u_value v_a) {
  u_value s1046 = v_o;
  u_value t1047 = NULL;
  if ((!t1047) && u_is_con(s1046, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1046, 1);
    t1047 = u_apply(v_f, 2, v_op, v_a);
  }
  if (!t1047) {
    u_no_case(s1046);
  }
  return t1047;
}

u_value f_parse_46_apply_postfix_3(u_value v_o, u_value v_op, u_value v_a) {
  u_value s1048 = v_o;
  u_value t1049 = NULL;
  if ((!t1049) && u_is_con(s1048, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1048, 2);
    t1049 = u_apply(v_f, 2, v_op, v_a);
  }
  if (!t1049) {
    u_no_case(s1048);
  }
  return t1049;
}

u_value f_parse_46_apply_infix_4(u_value v_o, u_value v_op, u_value v_a, u_value v_b) {
  u_value s1050 = v_o;
  u_value t1051 = NULL;
  if ((!t1051) && u_is_con(s1050, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1050, 3);
    t1051 = u_apply(v_f, 3, v_op, v_a, v_b);
  }
  if (!t1051) {
    u_no_case(s1050);
  }
  return t1051;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1052 = v_rest;
    int d1053 = 0;
    if ((!d1053) && u_is_con(s1052, &n_list_46_Nil)) {
      d1053 = 1;
      {
      }
    }
    if ((!d1053) && (u_is_con(s1052, &n_list_46_Cons) && u_is_con(u_field(s1052, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1052, 0), 0);
      u_value v_i = u_field(u_field(s1052, 0), 1);
      d1053 = 1;
      {
        static struct u_object l_unexpected_32_opera_1054 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1054, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1053) {
      d1053 = 1;
      {
        static struct u_object l_missing_32_operator_1055 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1055, v_at), "parse.ParseError");
      }
    }
    if (!d1053) {
      u_no_case(s1052);
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
    u_value s1056 = v_items;
    int d1057 = 0;
    if ((!d1057) && (u_is_con(s1056, &n_list_46_Cons) && u_is_con(u_field(s1056, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1056, 0), 0);
      u_value v_more = u_field(s1056, 1);
      d1057 = 1;
      {
        u_value t1058 = v_x;
        u_value t1059 = v_more;
        *r_a = t1058;
        *r_rest = t1059;
      }
    }
    if ((!d1057) && (u_is_con(s1056, &n_list_46_Cons) && u_is_con(u_field(s1056, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1056, 0), 0);
      u_value v_i = u_field(u_field(s1056, 0), 1);
      u_value v_more = u_field(s1056, 1);
      d1057 = 1;
      {
        u_value s1060 = f_parse_46_prefix_prec_2(f_parse_46_ops_notations_1(v_o), v_op);
        int d1061 = 0;
        if ((!d1061) && u_is_con(s1060, &n_option_46_Some)) {
          u_value v_p = u_field(s1060, 0);
          d1061 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1062 = f_parse_46_apply_prefix_3(v_o, v_op, v_b);
            *r_a = t1062;
          }
        }
        if ((!d1061) && u_is_con(s1060, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_ops_notations_1(v_o), v_op))) {
            d1061 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1063 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1063), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1061) && u_is_con(s1060, &n_option_46_None)) {
          d1061 = 1;
          {
            static struct u_object l_undeclared_32_opera_1064 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1064, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1061) {
          u_no_case(s1060);
        }
      }
    }
    if ((!d1057) && u_is_con(s1056, &n_list_46_Nil)) {
      d1057 = 1;
      {
        static struct u_object l_missing_32_operand_1065 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1065, v_at), "parse.ParseError");
      }
    }
    if (!d1057) {
      u_no_case(s1056);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1066 = v_items;
    int d1067 = 0;
    if ((!d1067) && (u_is_con(s1066, &n_list_46_Cons) && u_is_con(u_field(s1066, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1066, 0), 0);
      u_value v_i = u_field(u_field(s1066, 0), 1);
      u_value v_tail = u_field(s1066, 1);
      d1067 = 1;
      {
        u_value s1068 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_ops_notations_1(v_o), v_op), f_parse_46_infix_of_2(f_parse_46_ops_notations_1(v_o), v_op));
        int d1069 = 0;
        if ((!d1069) && u_is_con(u_field(s1068, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1068, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1069 = 1;
            {
              p_parse_46_climb_loop(f_parse_46_apply_postfix_3(v_o, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1069) && u_is_con(u_field(s1068, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1068, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1068, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1069 = 1;
            {
              u_value t1070;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1070 = v_p;
              } else {
                t1070 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1070, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_ops_notations_1(v_o));
              p_parse_46_climb_loop(f_parse_46_apply_infix_4(v_o, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1069) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_ops_notations_1(v_o), v_op)))) {
            d1069 = 1;
            {
              static struct u_object l_undeclared_32_opera_1071 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1071, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1069) {
          d1069 = 1;
          {
            u_value t1072 = v_lhs;
            u_value t1073 = v_items;
            *r_a = t1072;
            *r_rest = t1073;
          }
        }
        if (!d1069) {
          u_no_case(s1068);
        }
      }
    }
    if (!d1067) {
      d1067 = 1;
      {
        u_value t1074 = v_lhs;
        u_value t1075 = v_items;
        *r_a = t1074;
        *r_rest = t1075;
      }
    }
    if (!d1067) {
      u_no_case(s1066);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1076 = v_rest;
    int d1077 = 0;
    if ((!d1077) && (u_is_con(s1076, &n_list_46_Cons) && u_is_con(u_field(s1076, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1076, 0), 0);
      u_value v_i = u_field(u_field(s1076, 0), 1);
      u_value t1080 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1080)) {
        t1080 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1080)) {
        d1077 = 1;
        {
          static struct u_object l_operator_32__1078 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1079 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1078, u_concat(v_op, &l__32_is_32_not_32_associat_1079)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1077) {
      d1077 = 1;
      {
      }
    }
    if (!d1077) {
      u_no_case(s1076);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1081 = v_fp;
  u_value t1082 = NULL;
  if ((!t1082) && u_is_con(s1081, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1081, 0), 1);
    t1082 = u_eq(v_q, v_p);
  }
  if (!t1082) {
    t1082 = u_mk_bool(0);
  }
  if (!t1082) {
    u_no_case(s1081);
  }
  return t1082;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1083 = v_ts;
  u_value t1084 = NULL;
  if ((!t1084) && (u_is_con(s1083, &n_list_46_Cons) && (u_is_con(u_field(s1083, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1083, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1083, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1083, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1083, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1083, 0), 1);
    t1084 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1084) && (u_is_con(s1083, &n_list_46_Cons) && u_is_con(u_field(s1083, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1083, 0);
    t1084 = v_t;
  }
  if ((!t1084) && u_is_con(s1083, &n_list_46_Nil)) {
    t1084 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1084) {
    static struct u_object l__40_invalid_41__1085 = U_LIT("(invalid)");
    t1084 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1085), v_ts);
  }
  if (!t1084) {
    u_no_case(s1083);
  }
  return t1084;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1086 = v_ts;
  u_value t1087 = NULL;
  if ((!t1087) && (u_is_con(s1086, &n_list_46_Cons) && (u_is_con(u_field(s1086, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1086, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1086, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1086, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1086, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1086, 0), 1);
    t1087 = v_cs;
  }
  if (!t1087) {
    t1087 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1087) {
    u_no_case(s1086);
  }
  return t1087;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1088 = U_LIT("->");
  u_value t1095;
  if (u_truth(u_eq(v_op, &l__45__62__1088))) {
    u_value t1089 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1090 = &t1089;
    u_value l1091 = v_l;
    while (u_is_con(l1091, &n_list_46_Cons)) {
      u_value v_t = u_field(l1091, 0);
      if (u_truth(u_mk_bool(1))) {
        e1090 = u_append(e1090, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1091 = u_field(l1091, 1);
    }
    t1095 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1089, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1092 = U_LIT("*");
    u_value t1094;
    if (u_truth(u_eq(v_op, &l__42__1092))) {
      static struct u_object l__40_product_41__1093 = U_LIT("(product)");
      t1094 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1093), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1094 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1095 = t1094;
  }
  return t1095;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1096 = v_t;
  u_value t1097 = NULL;
  if ((!t1097) && (u_is_con(s1096, &n_syntax_46_TSort) && (u_is_con(u_field(s1096, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1096, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1096, 0), 1), u_mk_lit("(invalid)")))))) {
    t1097 = u_mk_bool(1);
  }
  if ((!t1097) && u_is_con(s1096, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1096, 1);
    t1097 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1097) && u_is_con(s1096, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1096, 0);
    u_value v_r = u_field(s1096, 1);
    u_value t1098 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1098)) {
      t1098 = f_parse_46_has_invalid_1(v_r);
    }
    t1097 = t1098;
  }
  if ((!t1097) && u_is_con(s1096, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1096, 0);
    t1097 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1097) {
    t1097 = u_mk_bool(0);
  }
  if (!t1097) {
    u_no_case(s1096);
  }
  return t1097;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1099 = v_ts;
  u_value t1100 = NULL;
  if ((!t1100) && u_is_con(s1099, &n_list_46_Nil)) {
    t1100 = u_mk_bool(0);
  }
  if ((!t1100) && u_is_con(s1099, &n_list_46_Cons)) {
    u_value v_t = u_field(s1099, 0);
    u_value v_more = u_field(s1099, 1);
    u_value t1101 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1101)) {
      t1101 = f_parse_46_any_invalid_1(v_more);
    }
    t1100 = t1101;
  }
  if (!t1100) {
    u_no_case(s1099);
  }
  return t1100;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1102 = v_t;
  u_value t1103 = NULL;
  if ((!t1103) && u_is_con(s1102, &n_lex_46_TName)) {
    u_value v_n = u_field(s1102, 0);
    u_value t1104 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1104)) {
      t1104 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1103 = t1104;
  }
  if ((!t1103) && (u_is_con(s1102, &n_lex_46_TPunct) && u_equals(u_field(s1102, 0), u_mk_char(40)))) {
    t1103 = u_mk_bool(1);
  }
  if (!t1103) {
    t1103 = u_mk_bool(0);
  }
  if (!t1103) {
    u_no_case(s1102);
  }
  return t1103;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1105 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1105;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_scope_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1106 = f_parse_46_type_one_1(v_r);
    *r_t = t1106;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1107 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1107, v_start), "parse.ParseError");
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
    u_value t1108 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1109 = u_mk_bool(1);
    u_value t1110 = u_mk_bool(1);
    v_rev = t1108;
    v_next = t1109;
    v_go = t1110;
    u_value t1119 = v_go;
    while (u_truth(t1119)) {
      {
        u_value s1111 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1112 = 0;
        if ((!d1112) && u_is_con(s1111, &n_option_46_Some)) {
          u_value v_op = u_field(s1111, 0);
          d1112 = 1;
          {
            u_value t1113 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1114 = u_mk_bool(1);
            v_rev = t1113;
            v_next = t1114;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1112) && u_is_con(s1111, &n_option_46_None)) {
          u_value t1117 = v_next;
          if (u_truth(t1117)) {
            t1117 = f_parse_46_starts_type_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1117)) {
            d1112 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1115 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1116 = u_mk_bool(0);
              v_rev = t1115;
              v_next = t1116;
            }
          }
        }
        if (!d1112) {
          d1112 = 1;
          {
            u_value t1118 = u_mk_bool(0);
            v_go = t1118;
          }
        }
        if (!d1112) {
          u_no_case(s1111);
        }
      }
      t1119 = v_go;
    }
    u_value s1120 = v_rev;
    int d1121 = 0;
    if ((!d1121) && u_is_con(s1120, &n_list_46_Nil)) {
      d1121 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1122 = U_LIT("type expected but ");
        static struct u_object l__32_found_1123 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1122, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1123)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1121) {
      d1121 = 1;
      {
        u_value t1124 = f_list_46_reverse_1(v_rev);
        *r_items = t1124;
      }
    }
    if (!d1121) {
      u_no_case(s1120);
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
            u_value t1125 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1125;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_scope_params_1(v_sc)))) {
            {
              u_value t1126 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1126;
            }
          } else {
            {
              u_value t1127 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1127;
            }
          }
        }
      }
    }
  }
}

void p_parse_46_type_list(u_value v_sc, u_value v_close, u_value *r_r, u_value *r_ts) {
  {
    u_value v_t = NULL;
    u_value v_rev = NULL;
    u_value t1128 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1128;
    u_value t1129 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1129)) {
      t1129 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1129)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1130 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1130;
        u_value t1132 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1132)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1131 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1131;
          }
          t1132 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1133 = f_list_46_reverse_1(v_rev);
    *r_r = t1133;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1134 = v_es;
  u_value t1135 = NULL;
  if ((!t1135) && u_is_con(s1134, &n_list_46_Nil)) {
    t1135 = v_rest;
  }
  if ((!t1135) && u_is_con(s1134, &n_list_46_Cons)) {
    u_value v_e = u_field(s1134, 0);
    u_value v_more = u_field(s1134, 1);
    static struct u_object l_Cons_1136 = U_LIT("Cons");
    t1135 = f_parse_46_expr_op2_3(&l_Cons_1136, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1135) {
    u_no_case(s1134);
  }
  return t1135;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1137 = v_t;
  u_value t1138 = NULL;
  if ((!t1138) && u_is_con(s1137, &n_lex_46_TName)) {
    u_value v_n = u_field(s1137, 0);
    static struct u_object l_let_1139 = U_LIT("let");
    static struct u_object l_if_1140 = U_LIT("if");
    static struct u_object l_lambda_1141 = U_LIT("lambda");
    static struct u_object l_match_1142 = U_LIT("match");
    static struct u_object l_true_1143 = U_LIT("true");
    static struct u_object l_false_1144 = U_LIT("false");
    u_value t1146 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1139, u_mk_con(&n_list_46_Cons, 2, &l_if_1140, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1141, u_mk_con(&n_list_46_Cons, 2, &l_match_1142, u_mk_con(&n_list_46_Cons, 2, &l_true_1143, u_mk_con(&n_list_46_Cons, 2, &l_false_1144, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1146)) {
      u_value t1145 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1145)) {
        t1145 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1146 = t1145;
    }
    t1138 = t1146;
  }
  if ((!t1138) && u_is_con(s1137, &n_lex_46_TNum)) {
    t1138 = u_mk_bool(1);
  }
  if ((!t1138) && u_is_con(s1137, &n_lex_46_TStr)) {
    t1138 = u_mk_bool(1);
  }
  if ((!t1138) && u_is_con(s1137, &n_lex_46_TChar)) {
    t1138 = u_mk_bool(1);
  }
  if ((!t1138) && u_is_con(s1137, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1137, 0);
    u_value t1147 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1147)) {
      t1147 = u_eq(v_c, u_mk_char(91));
    }
    t1138 = t1147;
  }
  if (!t1138) {
    t1138 = u_mk_bool(0);
  }
  if (!t1138) {
    u_no_case(s1137);
  }
  return t1138;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_items = NULL;
    p_parse_46_expr_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_scope_notations_1(v_sc), u_mk_closure(w_f_parse_46_expr_op1_2, 0), u_mk_closure(w_f_parse_46_expr_op1_2, 0), u_mk_closure(w_f_parse_46_expr_op2_3, 0)), f_parse_46_head_offset_1(*r_ts), r_e);
  }
}

void p_parse_46_expr_items(u_value v_sc, u_value *r_items, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_next = NULL;
    u_value v_go = NULL;
    u_value v_e = NULL;
    u_value t1148 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1149 = u_mk_bool(1);
    u_value t1150 = u_mk_bool(1);
    v_rev = t1148;
    v_next = t1149;
    v_go = t1150;
    u_value t1159 = v_go;
    while (u_truth(t1159)) {
      {
        u_value s1151 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1152 = 0;
        if ((!d1152) && u_is_con(s1151, &n_option_46_Some)) {
          u_value v_op = u_field(s1151, 0);
          d1152 = 1;
          {
            u_value t1153 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1154 = u_mk_bool(1);
            v_rev = t1153;
            v_next = t1154;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1152) && u_is_con(s1151, &n_option_46_None)) {
          u_value t1157 = v_next;
          if (u_truth(t1157)) {
            t1157 = f_parse_46_starts_expr_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1157)) {
            d1152 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1155 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1156 = u_mk_bool(0);
              v_rev = t1155;
              v_next = t1156;
            }
          }
        }
        if (!d1152) {
          d1152 = 1;
          {
            u_value t1158 = u_mk_bool(0);
            v_go = t1158;
          }
        }
        if (!d1152) {
          u_no_case(s1151);
        }
      }
      t1159 = v_go;
    }
    u_value s1160 = v_rev;
    int d1161 = 0;
    if ((!d1161) && u_is_con(s1160, &n_list_46_Nil)) {
      d1161 = 1;
      {
        static struct u_object l_expression_32_expec_1162 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1163 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1162, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1163)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1161) {
      d1161 = 1;
      {
        u_value t1164 = f_list_46_reverse_1(v_rev);
        *r_items = t1164;
      }
    }
    if (!d1161) {
      u_no_case(s1160);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1165 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1165;
    u_value t1167 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1167)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1166 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1166;
      }
      t1167 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1168 = f_list_46_reverse_1(v_rev);
    *r_es = t1168;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1169 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1169;
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
    p_parse_46_parse_atom(v_sc, r_e, r_ts);
    u_value t1171 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1171)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
        u_value t1170 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
        *r_e = t1170;
      }
      t1171 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
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
    u_value s1172 = f_parse_46_head_token_1(*r_ts);
    int d1173 = 0;
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TName) && u_equals(u_field(s1172, 0), u_mk_lit("let")))) {
      d1173 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TName) && u_equals(u_field(s1172, 0), u_mk_lit("if")))) {
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1174 = U_LIT("then");
        p_parse_46_keyword(&l_then_1174, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1175 = U_LIT("else");
        p_parse_46_keyword(&l_else_1175, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1176 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1176;
      }
    }
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TName) && u_equals(u_field(s1172, 0), u_mk_lit("lambda")))) {
      d1173 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TName) && u_equals(u_field(s1172, 0), u_mk_lit("match")))) {
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1177 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1177;
      }
    }
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TName) && u_equals(u_field(s1172, 0), u_mk_lit("true")))) {
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1178 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1178;
      }
    }
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TName) && u_equals(u_field(s1172, 0), u_mk_lit("false")))) {
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1179 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1179;
      }
    }
    if ((!d1173) && u_is_con(s1172, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1172, 0);
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1180 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1180;
      }
    }
    if ((!d1173) && u_is_con(s1172, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1172, 0);
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1181 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1181;
      }
    }
    if ((!d1173) && u_is_con(s1172, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1172, 0);
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1182 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1182;
      }
    }
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TPunct) && u_equals(u_field(s1172, 0), u_mk_char(40)))) {
      d1173 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1183 = v_es;
        int d1184 = 0;
        if ((!d1184) && (u_is_con(s1183, &n_list_46_Cons) && u_is_con(u_field(s1183, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1183, 0);
          d1184 = 1;
          {
            u_value t1185 = v_x;
            *r_e = t1185;
          }
        }
        if (!d1184) {
          d1184 = 1;
          {
            u_value t1186 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1186;
          }
        }
        if (!d1184) {
          u_no_case(s1183);
        }
      }
    }
    if ((!d1173) && (u_is_con(s1172, &n_lex_46_TPunct) && u_equals(u_field(s1172, 0), u_mk_char(91)))) {
      d1173 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1173) && u_is_con(s1172, &n_lex_46_TName)) {
      u_value v_n = u_field(s1172, 0);
      u_value t1188 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1188)) {
        t1188 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_scope_notations_1(v_sc), v_n));
      }
      if (u_truth(t1188)) {
        d1173 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1187 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1187;
        }
      }
    }
    if (!d1173) {
      d1173 = 1;
      {
        static struct u_object l_expression_32_expec_1189 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1190 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1189, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1190)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1173) {
      u_no_case(s1172);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1191 = v_ps;
  u_value t1192 = NULL;
  if ((!t1192) && u_is_con(s1191, &n_list_46_Nil)) {
    t1192 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1192) && (u_is_con(s1191, &n_list_46_Cons) && u_is_con(u_field(s1191, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1191, 0), 0);
    u_value v_more = u_field(s1191, 1);
    u_value s1193 = f_parse_46_pattern_vars_1(v_more);
    u_value t1194 = NULL;
    if ((!t1194) && u_is_con(s1193, &n_option_46_Some)) {
      u_value v_xs = u_field(s1193, 0);
      t1194 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1194) && u_is_con(s1193, &n_option_46_None)) {
      t1194 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1194) {
      u_no_case(s1193);
    }
    t1192 = t1194;
  }
  if (!t1192) {
    t1192 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1192) {
    u_no_case(s1191);
  }
  return t1192;
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
    static struct u_object l__58__61__1195 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1195), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1196 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1197 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1196;
    v_es = t1197;
    u_value t1201 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1201)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1198 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1198), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1199 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1200 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1199;
        v_es = t1200;
      }
      t1201 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1202 = U_LIT("in");
    p_parse_46_keyword(&l_in_1202, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1203 = f_list_46_reverse_1(v_ps);
    u_value t1204 = f_list_46_reverse_1(v_es);
    v_ps = t1203;
    v_es = t1204;
    u_value s1205 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1206 = 0;
    if ((!d1206) && u_is_con(u_field(s1205, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1205, 0), 0);
      d1206 = 1;
      {
        u_value t1207 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1207;
      }
    }
    if ((!d1206) && (u_is_con(u_field(s1205, 0), &n_option_46_None) && (u_is_con(u_field(s1205, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1205, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1205, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1205, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1205, 1), 0);
      u_value v_c = u_field(u_field(s1205, 2), 0);
      d1206 = 1;
      {
        u_value t1208 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1208;
      }
    }
    if (!d1206) {
      d1206 = 1;
      {
        u_value t1209 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1209;
      }
    }
    if (!d1206) {
      u_no_case(s1205);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1210 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1210;
    static struct u_object l__61__62__1211 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1211)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1212 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1212;
        u_value t1214 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1214)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1213 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1213;
          }
          t1214 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1215 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1215), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1216 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1216;
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
        static struct u_object l_Nil_1217 = U_LIT("Nil");
        u_value t1218 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1217), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1218;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1219 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1219))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1220 = U_LIT("in");
            p_parse_46_keyword(&l_in_1220, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1221 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1222 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1221;
            v_lists = t1222;
            u_value t1226 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1226)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1223 = U_LIT("in");
                p_parse_46_keyword(&l_in_1223, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1224 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1225 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1224;
                v_lists = t1225;
              }
              t1226 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1227 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1227;
            static struct u_object l_if_1228 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1228))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1229 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1229;
          }
        } else {
          {
            u_value t1230 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1230;
            u_value t1232 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1232)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1231 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1231;
              }
              t1232 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1233 = U_LIT("Nil");
            u_value t1234 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1233), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1234;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1235 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1235;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1236 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1236))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1237 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1237;
      }
    } else {
      {
        u_value t1238 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1238;
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
    u_value t1239 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1239;
    static struct u_object l_case_1240 = U_LIT("case");
    u_value t1243 = f_parse_46_at_keyword_2(*r_ts, &l_case_1240);
    while (u_truth(t1243)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1241 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1241), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1242 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1242;
      }
      static struct u_object l_case_1244 = U_LIT("case");
      t1243 = f_parse_46_at_keyword_2(*r_ts, &l_case_1244);
    }
    static struct u_object l_else_1245 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1245))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1246 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1246;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1247 = U_LIT("end");
    p_parse_46_keyword(&l_end_1247, r_ts);
    u_value t1248 = f_list_46_reverse_1(v_rev);
    *r_cs = t1248;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1249 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1249))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1250 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1250, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1251 = v_ps;
  u_value t1252 = NULL;
  if ((!t1252) && u_is_con(s1251, &n_list_46_Nil)) {
    t1252 = v_rest;
  }
  if ((!t1252) && u_is_con(s1251, &n_list_46_Cons)) {
    u_value v_p = u_field(s1251, 0);
    u_value v_more = u_field(s1251, 1);
    static struct u_object l_Cons_1253 = U_LIT("Cons");
    t1252 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1253), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1252) {
    u_no_case(s1251);
  }
  return t1252;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1254 = v_t;
  u_value t1255 = NULL;
  if ((!t1255) && u_is_con(s1254, &n_lex_46_TName)) {
    u_value v_n = u_field(s1254, 0);
    static struct u_object l___1256 = U_LIT("_");
    static struct u_object l_true_1257 = U_LIT("true");
    static struct u_object l_false_1258 = U_LIT("false");
    u_value t1260 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1256, u_mk_con(&n_list_46_Cons, 2, &l_true_1257, u_mk_con(&n_list_46_Cons, 2, &l_false_1258, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1260)) {
      u_value t1259 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1259)) {
        t1259 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1260 = t1259;
    }
    t1255 = t1260;
  }
  if ((!t1255) && u_is_con(s1254, &n_lex_46_TNum)) {
    t1255 = u_mk_bool(1);
  }
  if ((!t1255) && u_is_con(s1254, &n_lex_46_TStr)) {
    t1255 = u_mk_bool(1);
  }
  if ((!t1255) && u_is_con(s1254, &n_lex_46_TChar)) {
    t1255 = u_mk_bool(1);
  }
  if ((!t1255) && u_is_con(s1254, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1254, 0);
    u_value t1261 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1261)) {
      t1261 = u_eq(v_c, u_mk_char(91));
    }
    t1255 = t1261;
  }
  if (!t1255) {
    t1255 = u_mk_bool(0);
  }
  if (!t1255) {
    u_no_case(s1254);
  }
  return t1255;
}

void p_parse_46_parse_pattern(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_items = NULL;
    p_parse_46_pattern_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_scope_notations_1(v_sc), u_mk_closure(w_f_parse_46_pat_op1_2, 0), u_mk_closure(w_f_parse_46_pat_op1_2, 0), u_mk_closure(w_f_parse_46_pat_op2_3, 0)), f_parse_46_head_offset_1(*r_ts), r_p);
  }
}

void p_parse_46_pattern_items(u_value v_sc, u_value *r_items, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_next = NULL;
    u_value v_go = NULL;
    u_value v_p = NULL;
    u_value t1262 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1263 = u_mk_bool(1);
    u_value t1264 = u_mk_bool(1);
    v_rev = t1262;
    v_next = t1263;
    v_go = t1264;
    u_value t1273 = v_go;
    while (u_truth(t1273)) {
      {
        u_value s1265 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1266 = 0;
        if ((!d1266) && u_is_con(s1265, &n_option_46_Some)) {
          u_value v_op = u_field(s1265, 0);
          d1266 = 1;
          {
            u_value t1267 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1268 = u_mk_bool(1);
            v_rev = t1267;
            v_next = t1268;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1266) && u_is_con(s1265, &n_option_46_None)) {
          u_value t1271 = v_next;
          if (u_truth(t1271)) {
            t1271 = f_parse_46_starts_pattern_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1271)) {
            d1266 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1269 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1270 = u_mk_bool(0);
              v_rev = t1269;
              v_next = t1270;
            }
          }
        }
        if (!d1266) {
          d1266 = 1;
          {
            u_value t1272 = u_mk_bool(0);
            v_go = t1272;
          }
        }
        if (!d1266) {
          u_no_case(s1265);
        }
      }
      t1273 = v_go;
    }
    u_value s1274 = v_rev;
    int d1275 = 0;
    if ((!d1275) && u_is_con(s1274, &n_list_46_Nil)) {
      d1275 = 1;
      {
        static struct u_object l_pattern_32_expected_1276 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1277 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1276, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1277)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1275) {
      d1275 = 1;
      {
        u_value t1278 = f_list_46_reverse_1(v_rev);
        *r_items = t1278;
      }
    }
    if (!d1275) {
      u_no_case(s1274);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_p = NULL;
    u_value v_rev = NULL;
    u_value t1279 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1279;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1280 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1280;
        u_value t1282 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1282)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1281 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1281;
          }
          t1282 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1283 = f_list_46_reverse_1(v_rev);
    *r_ps = t1283;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1284 = f_parse_46_head_token_1(*r_ts);
    int d1285 = 0;
    if ((!d1285) && (u_is_con(s1284, &n_lex_46_TName) && u_equals(u_field(s1284, 0), u_mk_lit("_")))) {
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1286 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1286;
      }
    }
    if ((!d1285) && (u_is_con(s1284, &n_lex_46_TName) && u_equals(u_field(s1284, 0), u_mk_lit("true")))) {
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1287 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1287;
      }
    }
    if ((!d1285) && (u_is_con(s1284, &n_lex_46_TName) && u_equals(u_field(s1284, 0), u_mk_lit("false")))) {
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1288 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1288;
      }
    }
    if ((!d1285) && u_is_con(s1284, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1284, 0);
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1289 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1289;
      }
    }
    if ((!d1285) && u_is_con(s1284, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1284, 0);
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1290 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1290;
      }
    }
    if ((!d1285) && u_is_con(s1284, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1284, 0);
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1291 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1291;
      }
    }
    if ((!d1285) && (u_is_con(s1284, &n_lex_46_TPunct) && u_equals(u_field(s1284, 0), u_mk_char(40)))) {
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1292 = v_ps;
        int d1293 = 0;
        if ((!d1293) && (u_is_con(s1292, &n_list_46_Cons) && u_is_con(u_field(s1292, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1292, 0);
          d1293 = 1;
          {
            u_value t1294 = v_q;
            *r_p = t1294;
          }
        }
        if (!d1293) {
          d1293 = 1;
          {
            u_value t1295 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1295;
          }
        }
        if (!d1293) {
          u_no_case(s1292);
        }
      }
    }
    if ((!d1285) && (u_is_con(s1284, &n_lex_46_TPunct) && u_equals(u_field(s1284, 0), u_mk_char(91)))) {
      d1285 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1296 = U_LIT("Nil");
            u_value t1297 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1296), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1297;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1298 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1298;
            u_value t1300 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1300)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1299 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1299;
              }
              t1300 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1301 = U_LIT("Nil");
            u_value t1302 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1301), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1302;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1303 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1303;
          }
        }
      }
    }
    if (!d1285) {
      d1285 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1304 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1304;
          }
        } else {
          {
            u_value t1305 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1305;
          }
        }
      }
    }
    if (!d1285) {
      u_no_case(s1284);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1306 = v_t;
  u_value t1307 = NULL;
  if ((!t1307) && u_is_con(s1306, &n_lex_46_TName)) {
    u_value v_n = u_field(s1306, 0);
    static struct u_object l_skip_1308 = U_LIT("skip");
    static struct u_object l_if_1309 = U_LIT("if");
    static struct u_object l_while_1310 = U_LIT("while");
    static struct u_object l_call_1311 = U_LIT("call");
    static struct u_object l_match_1312 = U_LIT("match");
    static struct u_object l_assert_1313 = U_LIT("assert");
    static struct u_object l_assign_1314 = U_LIT("assign");
    static struct u_object l_raise_1315 = U_LIT("raise");
    static struct u_object l_try_1316 = U_LIT("try");
    t1307 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1308, u_mk_con(&n_list_46_Cons, 2, &l_if_1309, u_mk_con(&n_list_46_Cons, 2, &l_while_1310, u_mk_con(&n_list_46_Cons, 2, &l_call_1311, u_mk_con(&n_list_46_Cons, 2, &l_match_1312, u_mk_con(&n_list_46_Cons, 2, &l_assert_1313, u_mk_con(&n_list_46_Cons, 2, &l_assign_1314, u_mk_con(&n_list_46_Cons, 2, &l_raise_1315, u_mk_con(&n_list_46_Cons, 2, &l_try_1316, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1307) {
    t1307 = u_mk_bool(0);
  }
  if (!t1307) {
    u_no_case(s1306);
  }
  return t1307;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1317 = v_cs;
  u_value t1318 = NULL;
  if ((!t1318) && u_is_con(s1317, &n_list_46_Nil)) {
    t1318 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1318) && (u_is_con(s1317, &n_list_46_Cons) && u_is_con(u_field(s1317, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1317, 0), 0);
    u_value v_g = u_field(u_field(s1317, 0), 1);
    u_value v_b = u_field(u_field(s1317, 0), 2);
    u_value v_more = u_field(s1317, 1);
    t1318 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1318) {
    u_no_case(s1317);
  }
  return t1318;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_more = NULL;
    u_value v_s = NULL;
    u_value v_rev = NULL;
    u_value t1319 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1320 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1319;
    v_rev = t1320;
    static struct u_object l_local_1321 = U_LIT("local");
    u_value t1323 = f_parse_46_at_keyword_2(*r_ts, &l_local_1321);
    while (u_truth(t1323)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(v_sc, &v_more, r_ts);
        u_value t1322 = f_list_46__43__43__2(v_xs, v_more);
        v_xs = t1322;
      }
      static struct u_object l_local_1324 = U_LIT("local");
      t1323 = f_parse_46_at_keyword_2(*r_ts, &l_local_1324);
    }
    u_value t1326 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1326)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1325 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1325;
      }
      t1326 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1327 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46_reverse_1(v_rev));
    *r_b = t1327;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1328 = f_parse_46_head_token_1(*r_ts);
    int d1329 = 0;
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("skip")))) {
      d1329 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1330 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1330;
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("if")))) {
      d1329 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("while")))) {
      d1329 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1331 = U_LIT("end");
        p_parse_46_keyword(&l_end_1331, r_ts);
        u_value t1332 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1332;
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("call")))) {
      d1329 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("match")))) {
      d1329 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1333 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1333;
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("assert")))) {
      d1329 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1334 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1334;
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("assign")))) {
      d1329 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("raise")))) {
      d1329 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1335 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1335;
      }
    }
    if ((!d1329) && (u_is_con(s1328, &n_lex_46_TName) && u_equals(u_field(s1328, 0), u_mk_lit("try")))) {
      d1329 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1336 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1336, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1337 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1337;
      }
    }
    if (!d1329) {
      d1329 = 1;
      {
        static struct u_object l_statement_32_expect_1338 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1339 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1338, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1339)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1329) {
      u_no_case(s1328);
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
    static struct u_object l_else_1340 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1340)))) {
      {
        static struct u_object l_end_1341 = U_LIT("end");
        p_parse_46_keyword(&l_end_1341, r_ts);
        u_value t1342 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1342;
      }
    } else {
      static struct u_object l_if_1343 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1343)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1344 = U_LIT("end");
          p_parse_46_keyword(&l_end_1344, r_ts);
        }
      }
    }
    u_value t1345 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1345;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1346 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1346), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1347 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1347;
    u_value t1350 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1350)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1348 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1348), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1349 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1349;
      }
      t1350 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1351 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1351;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1352 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1352;
    static struct u_object l_case_1353 = U_LIT("case");
    u_value t1356 = f_parse_46_at_keyword_2(*r_ts, &l_case_1353);
    while (u_truth(t1356)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1354 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1354), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1355 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1355;
      }
      static struct u_object l_case_1357 = U_LIT("case");
      t1356 = f_parse_46_at_keyword_2(*r_ts, &l_case_1357);
    }
    static struct u_object l_else_1358 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1358))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1359 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1359;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1360 = U_LIT("end");
    p_parse_46_keyword(&l_end_1360, r_ts);
    u_value t1361 = f_list_46_reverse_1(v_rev);
    *r_cs = t1361;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1363;
  if (u_truth(v_first)) {
    t1363 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1362 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1362)) {
      t1362 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1363 = t1362;
  }
  return t1363;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1365 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1365)) {
    static struct u_object l_out_1364 = U_LIT("out");
    t1365 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1364));
  }
  u_value t1367 = t1365;
  if (u_truth(t1367)) {
    static struct u_object l_ref_1366 = U_LIT("ref");
    t1367 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1366));
  }
  return t1367;
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
    u_value t1368 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1369 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1370 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1371 = u_mk_bool(1);
    v_ins = t1368;
    v_outs = t1369;
    v_refs = t1370;
    v_first = t1371;
    static struct u_object l_in_1372 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1372))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1373 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1374 = u_mk_bool(0);
        v_ins = t1373;
        v_first = t1374;
        u_value t1376 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1376)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1375 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1375;
          }
          t1376 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1377 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1377, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1378 = u_mk_bool(0);
        v_first = t1378;
      }
    }
    static struct u_object l_ref_1379 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1379, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1380 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1380;
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
    u_value t1381 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1381;
    u_value t1383 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1383)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1382 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1382;
      }
      t1383 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1384 = f_list_46_reverse_1(v_rev);
    *r_xs = t1384;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1385 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1385;
    static struct u_object l__58__1386 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1386)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1387 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1387;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1388 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1388;
    u_value t1390 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1390)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1389 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1389;
      }
      t1390 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1391 = f_list_46_reverse_1(v_rev);
    *r_xs = t1391;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1392 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1392;
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
  u_value s1393 = v_cs;
  u_value t1394 = NULL;
  if ((!t1394) && u_is_con(s1393, &n_list_46_Nil)) {
    t1394 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1394) && (u_is_con(s1393, &n_list_46_Cons) && u_is_con(u_field(s1393, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1393, 1);
    t1394 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1394) && (u_is_con(s1393, &n_list_46_Cons) && u_is_con(u_field(s1393, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1393, 1);
    t1394 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1394) && (u_is_con(s1393, &n_list_46_Cons) && u_is_con(u_field(s1393, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1393, 1);
    t1394 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1394) && u_is_con(s1393, &n_list_46_Cons)) {
    u_value v_c = u_field(s1393, 0);
    u_value v_more = u_field(s1393, 1);
    t1394 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1394) {
    u_no_case(s1393);
  }
  return t1394;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1395 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1396 = &t1395;
  u_value l1397 = v_cs;
  while (u_is_con(l1397, &n_list_46_Cons)) {
    u_value v_c = u_field(l1397, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1396 = u_append(e1396, v_c);
    }
    l1397 = u_field(l1397, 1);
  }
  return t1395;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1398 = v_c;
  u_value t1399 = NULL;
  if ((!t1399) && u_is_con(s1398, &n_syntax_46_CNotation)) {
    t1399 = u_mk_bool(1);
  }
  if (!t1399) {
    t1399 = u_mk_bool(0);
  }
  if (!t1399) {
    u_no_case(s1398);
  }
  return t1399;
}

u_value f_parse_46_loaded_modules_1(u_value v_ld) {
  u_value s1400 = v_ld;
  u_value t1401 = NULL;
  if ((!t1401) && u_is_con(s1400, &n_parse_46_Loader)) {
    u_value v_ms = u_field(s1400, 0);
    t1401 = v_ms;
  }
  if (!t1401) {
    u_no_case(s1400);
  }
  return t1401;
}

u_value f_parse_46_loading_1(u_value v_ld) {
  u_value s1402 = v_ld;
  u_value t1403 = NULL;
  if ((!t1403) && u_is_con(s1402, &n_parse_46_Loader)) {
    u_value v_ls = u_field(s1402, 1);
    t1403 = v_ls;
  }
  if (!t1403) {
    u_no_case(s1402);
  }
  return t1403;
}

u_value f_parse_46_base_notations_1(u_value v_ld) {
  u_value s1404 = v_ld;
  u_value t1405 = NULL;
  if ((!t1405) && u_is_con(s1404, &n_parse_46_Loader)) {
    u_value v_b = u_field(s1404, 2);
    t1405 = v_b;
  }
  if (!t1405) {
    u_no_case(s1404);
  }
  return t1405;
}

u_value f_parse_46_directory_1(u_value v_ld) {
  u_value s1406 = v_ld;
  u_value t1407 = NULL;
  if ((!t1407) && u_is_con(s1406, &n_parse_46_Loader)) {
    u_value v_d = u_field(s1406, 3);
    t1407 = v_d;
  }
  if (!t1407) {
    u_no_case(s1406);
  }
  return t1407;
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1408 = v_c;
  u_value t1409 = NULL;
  if ((!t1409) && u_is_con(s1408, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1408, 0);
    u_value v_cs = u_field(s1408, 1);
    u_value t1410 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1411 = &t1410;
    u_value l1412 = v_cs;
    while (u_is_con(l1412, &n_list_46_Cons)) {
      u_value v_d = u_field(l1412, 0);
      if (u_truth(u_mk_bool(1))) {
        e1411 = u_append(e1411, f_parse_46_modules_of_1(v_d));
      }
      l1412 = u_field(l1412, 1);
    }
    t1409 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1410));
  }
  if (!t1409) {
    t1409 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1409) {
    u_no_case(s1408);
  }
  return t1409;
}

u_value f_parse_46_own_notations_2(u_value v_ld, u_value v_n) {
  u_value s1413 = f_list_46_lookup_2(v_n, f_parse_46_loaded_modules_1(v_ld));
  u_value t1414 = NULL;
  if ((!t1414) && u_is_con(s1413, &n_option_46_Some)) {
    u_value v_ns = u_field(s1413, 0);
    t1414 = v_ns;
  }
  if ((!t1414) && u_is_con(s1413, &n_option_46_None)) {
    t1414 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1414) {
    u_no_case(s1413);
  }
  return t1414;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_rev = NULL;
    u_value t1415 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1415;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1416 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1416;
        u_value t1418 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1418)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1417 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1417;
          }
          t1418 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1419 = f_list_46_reverse_1(v_rev);
    *r_ps = t1419;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1420 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1420;
    u_value s1421 = f_parse_46_head_token_1(*r_ts);
    int d1422 = 0;
    if ((!d1422) && (u_is_con(s1421, &n_lex_46_TName) && u_equals(u_field(s1421, 0), u_mk_lit("prefix")))) {
      d1422 = 1;
      {
        u_value t1423 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1423;
      }
    }
    if ((!d1422) && (u_is_con(s1421, &n_lex_46_TName) && u_equals(u_field(s1421, 0), u_mk_lit("postfix")))) {
      d1422 = 1;
      {
        u_value t1424 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1424;
      }
    }
    if (!d1422) {
      d1422 = 1;
      {
        u_value s1425 = f_parse_46_second_token_1(*r_ts);
        int d1426 = 0;
        if ((!d1426) && (u_is_con(s1425, &n_lex_46_TName) && u_equals(u_field(s1425, 0), u_mk_lit("left")))) {
          d1426 = 1;
          {
            u_value t1427 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1427;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1426) && (u_is_con(s1425, &n_lex_46_TName) && u_equals(u_field(s1425, 0), u_mk_lit("right")))) {
          d1426 = 1;
          {
            u_value t1428 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1428;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1426) {
          d1426 = 1;
          {
          }
        }
        if (!d1426) {
          u_no_case(s1425);
        }
      }
    }
    if (!d1422) {
      u_no_case(s1421);
    }
    p_parse_46_advance(r_ts);
    u_value s1429 = f_parse_46_head_token_1(*r_ts);
    int d1430 = 0;
    if ((!d1430) && u_is_con(s1429, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1429, 0);
      d1430 = 1;
      {
        u_value t1431 = v_n;
        v_p = t1431;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1430) {
      d1430 = 1;
      {
        static struct u_object l_precedence_32_expec_1432 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1433 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1432, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1433)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1430) {
      u_no_case(s1429);
    }
    u_value t1434 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1435 = u_mk_bool(1);
    v_ops = t1434;
    v_go = t1435;
    u_value t1441 = v_go;
    while (u_truth(t1441)) {
      {
        u_value s1436 = f_parse_46_head_token_1(*r_ts);
        int d1437 = 0;
        if ((!d1437) && u_is_con(s1436, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1436, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1437 = 1;
            {
              u_value t1438 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1438;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1437) && u_is_con(s1436, &n_lex_46_TName)) {
          u_value v_n = u_field(s1436, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1437 = 1;
            {
              u_value t1439 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1439;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1437) {
          d1437 = 1;
          {
            u_value t1440 = u_mk_bool(0);
            v_go = t1440;
          }
        }
        if (!d1437) {
          u_no_case(s1436);
        }
      }
      t1441 = v_go;
    }
    u_value s1442 = v_ops;
    int d1443 = 0;
    if ((!d1443) && u_is_con(s1442, &n_list_46_Nil)) {
      d1443 = 1;
      {
        static struct u_object l_operator_32_expecte_1444 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1445 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1444, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1445)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1443) {
      d1443 = 1;
      {
        u_value t1446 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1446;
      }
    }
    if (!d1443) {
      u_no_case(s1442);
    }
  }
}

void p_parse_46_constructor(u_value v_sc, u_value *r_k, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_args = NULL;
    p_parse_46_parse_name_or_symbol(&v_n, r_ts);
    u_value t1447 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1447;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_type_list(v_sc, u_mk_char(41), &v_args, r_ts);
      }
    }
    u_value t1448 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), v_args);
    *r_k = t1448;
  }
}

void p_parse_46_parse_data(u_value v_ns, u_value *r_c, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_k = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_name(&v_n, r_ts);
    p_parse_46_type_params(&v_ps, r_ts);
    static struct u_object l__58__61__1449 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1449), r_ts);
    static struct u_object l__124__1450 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1450)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, r_ts);
    u_value t1451 = u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1451;
    static struct u_object l__124__1452 = U_LIT("|");
    u_value t1454 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1452));
    while (u_truth(t1454)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, r_ts);
        u_value t1453 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
        v_rev = t1453;
      }
      static struct u_object l__124__1455 = U_LIT("|");
      t1454 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1455));
    }
    u_value t1456 = u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_rev));
    *r_c = t1456;
  }
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
    u_value t1457 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1457;
    u_value t1459 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1459)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1458 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1458;
      }
      t1459 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1460 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1460), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1461 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1461), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1462 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1462;
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
    u_value t1463 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1464 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1465 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1466 = u_mk_bool(1);
    v_ins = t1463;
    v_outs = t1464;
    v_refs = t1465;
    v_first = t1466;
    static struct u_object l_in_1467 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1467))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1468 = u_mk_bool(0);
        v_first = t1468;
      }
    }
    static struct u_object l_out_1469 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1469, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1470 = u_mk_bool(0);
        v_first = t1470;
      }
    }
    static struct u_object l_ref_1471 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1471, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1472 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1472), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1473 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1473;
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
    u_value s1474 = f_parse_46_head_token_1(*r_ts);
    int d1475 = 0;
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("import")))) {
      d1475 = 1;
      {
        u_value t1476 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1476;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_loaded_modules_1(v_ld))))) {
          {
            u_value t1477 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1477;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_ns, v_ld, v_at, r_cs);
            u_value t1478 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1478;
          }
        }
      }
    }
    if ((!d1475) && u_is_con(s1474, &n_lex_46_TName)) {
      u_value v_w = u_field(s1474, 0);
      static struct u_object l_prefix_1480 = U_LIT("prefix");
      static struct u_object l_postfix_1481 = U_LIT("postfix");
      static struct u_object l_infix_1482 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1480, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1481, u_mk_con(&n_list_46_Cons, 2, &l_infix_1482, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1475 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1479 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1479;
        }
      }
    }
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("type")))) {
      d1475 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1483 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1483), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1484 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1484;
      }
    }
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("data")))) {
      d1475 = 1;
      {
        p_parse_46_parse_data(v_ns, &v_c, r_ts);
        u_value t1485 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1485;
      }
    }
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("function")))) {
      d1475 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1486 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1486;
      }
    }
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("procedure")))) {
      d1475 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1487 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1487;
      }
    }
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("eval")))) {
      d1475 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1488 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1488;
      }
    }
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("run")))) {
      d1475 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1489 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1489), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1490 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1490;
      }
    }
    if ((!d1475) && (u_is_con(s1474, &n_lex_46_TName) && u_equals(u_field(s1474, 0), u_mk_lit("test")))) {
      d1475 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1491 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1491), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1492 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1492;
      }
    }
    if (!d1475) {
      d1475 = 1;
      {
        static struct u_object l_declaration_32_expe_1493 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1494 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1493, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1494)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1475) {
      u_no_case(s1474);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_ns, u_value *r_ld) {
  {
    u_value s1495 = v_cs;
    int d1496 = 0;
    if ((!d1496) && u_is_con(s1495, &n_list_46_Nil)) {
      d1496 = 1;
      {
      }
    }
    if ((!d1496) && (u_is_con(s1495, &n_list_46_Cons) && u_is_con(u_field(s1495, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1495, 0), 0);
      u_value v_p = u_field(u_field(s1495, 0), 1);
      u_value v_ops = u_field(u_field(s1495, 0), 2);
      u_value v_more = u_field(s1495, 1);
      d1496 = 1;
      {
        u_value t1497 = f_list_46__43__43__2(*r_ns, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_ns = t1497;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1496) && (u_is_con(s1495, &n_list_46_Cons) && u_is_con(u_field(s1495, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1495, 0), 0);
      u_value v_ds = u_field(u_field(s1495, 0), 1);
      u_value v_more = u_field(s1495, 1);
      d1496 = 1;
      {
        u_value t1498 = u_mk_con(&n_parse_46_Loader, 4, f_list_46__43__43__2(f_parse_46_modules_of_1(u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds)), f_parse_46_loaded_modules_1(*r_ld)), f_parse_46_loading_1(*r_ld), f_parse_46_base_notations_1(*r_ld), f_parse_46_directory_1(*r_ld));
        *r_ld = t1498;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1496) && (u_is_con(s1495, &n_list_46_Cons) && u_is_con(u_field(s1495, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1495, 0), 0);
      u_value v_more = u_field(s1495, 1);
      d1496 = 1;
      {
        u_value t1499 = f_list_46__43__43__2(*r_ns, f_parse_46_own_notations_2(*r_ld, v_n));
        *r_ns = t1499;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1496) && u_is_con(s1495, &n_list_46_Cons)) {
      u_value v_more = u_field(s1495, 1);
      d1496 = 1;
      {
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if (!d1496) {
      u_no_case(s1495);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_ns, u_value *r_ld, u_value *r_ts) {
  {
    u_value v_more = NULL;
    u_value v_rev = NULL;
    u_value t1500 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1500;
    u_value t1502 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1502)) {
      {
        p_parse_46_parse_command(*r_ns, *r_ld, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_ns, r_ld);
        u_value t1501 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1501;
      }
      t1502 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1503 = f_list_46_reverse_1(v_rev);
    *r_cs = t1503;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1504 = v_dir;
    int d1505 = 0;
    if ((!d1505) && u_is_con(s1504, &n_option_46_Some)) {
      u_value v_d = u_field(s1504, 0);
      d1505 = 1;
      {
        {
          struct u_handler *h1508 = u_push();
          if (setjmp(h1508->env) == 0) {
            {
              static struct u_object l__47__1506 = U_LIT("/");
              static struct u_object l__46_u_1507 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1506, u_concat(v_n, &l__46_u_1507))), r_text);
            }
            u_pop(h1508);
          } else {
            u_pop(h1508);
            int d1509 = 0;
            if ((!d1509) && (u_has_type(h1508, "os.OsError") && u_is_con(u_caught(h1508), &n_os_46_OsError))) {
              d1509 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1509) {
              u_reraise(h1508);
            }
          }
        }
      }
    }
    if ((!d1505) && u_is_con(s1504, &n_option_46_None)) {
      d1505 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1505) {
      u_no_case(s1504);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1510 = u_push();
      if (setjmp(h1510->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1510);
      } else {
        u_pop(h1510);
        int d1511 = 0;
        if ((!d1511) && (u_has_type(h1510, "os.OsError") && u_is_con(u_caught(h1510), &n_os_46_OsError))) {
          d1511 = 1;
          {
            static struct u_object l_unknown_32_module_32__1512 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1512, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1511) {
          u_reraise(h1510);
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
    if (u_truth(f_list_46_contains_2(v_n, f_parse_46_loading_1(v_ld)))) {
      {
        static struct u_object l_cyclic_32_import_32_of_1513 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1513, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_directory_1(v_ld), v_at, &v_text);
    u_value t1514 = u_mk_con(&n_parse_46_Loader, 4, f_parse_46_loaded_modules_1(v_ld), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_loading_1(v_ld)), f_parse_46_base_notations_1(v_ld), f_parse_46_directory_1(v_ld));
    v_mld = t1514;
    u_value s1515 = f_parse_46_base_notations_1(v_ld);
    int d1516 = 0;
    if ((!d1516) && u_is_con(s1515, &n_option_46_Some)) {
      u_value v_b = u_field(s1515, 0);
      d1516 = 1;
      {
        u_value t1517 = v_b;
        v_mns = t1517;
      }
    }
    if ((!d1516) && u_is_con(s1515, &n_option_46_None)) {
      d1516 = 1;
      {
        u_value t1518 = v_ns;
        v_mns = t1518;
      }
    }
    if (!d1516) {
      u_no_case(s1515);
    }
    {
      struct u_handler *h1519 = u_push();
      if (setjmp(h1519->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mns, &v_mld, &v_mts);
        }
        u_pop(h1519);
      } else {
        u_pop(h1519);
        int d1520 = 0;
        if ((!d1520) && (u_has_type(h1519, "lex.LexError") && u_is_con(u_caught(h1519), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1519), 0);
          u_value v_i = u_field(u_caught(h1519), 1);
          d1520 = 1;
          {
            static struct u_object l__32_in_32__1521 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1521, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1520) && (u_has_type(h1519, "parse.ParseError") && u_is_con(u_caught(h1519), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1519), 0);
          u_value v_i = u_field(u_caught(h1519), 1);
          d1520 = 1;
          {
            static struct u_object l__32_in_32__1522 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1522, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1520) {
          u_reraise(h1519);
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
      struct u_handler *h1523 = u_push();
      if (setjmp(h1523->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1523);
      } else {
        u_pop(h1523);
        int d1524 = 0;
        if ((!d1524) && (u_has_type(h1523, "lex.LexError") && u_is_con(u_caught(h1523), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1523), 0);
          u_value v_i = u_field(u_caught(h1523), 1);
          d1524 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1524) {
          u_reraise(h1523);
        }
      }
    }
    u_value t1525 = v_ns;
    u_value t1526 = v_ld;
    v_ns1 = t1525;
    v_ld1 = t1526;
    p_parse_46_parse_commands(&v_cs, &v_ns1, &v_ld1, &v_ts);
    u_value t1527 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1527;
  }
}

void p_parse_46_prelude_scope(u_value v_pre, u_value *r_ns, u_value *r_ms) {
  {
    u_value v_ld = NULL;
    u_value t1528 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1529 = u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    *r_ns = t1528;
    v_ld = t1529;
    p_parse_46_declare_all(f_parse_46_commands_of_1(v_pre), r_ns, &v_ld);
    u_value t1530 = f_parse_46_loaded_modules_1(v_ld);
    *r_ms = t1530;
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
    static struct u_object l_prelude_1531 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1531))) {
      {
        static struct u_object l_prelude_1532 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1532, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1533 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1533, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1534 = v_xs;
  u_value t1535 = NULL;
  if ((!t1535) && (u_is_con(s1534, &n_list_46_Cons) && u_is_con(u_field(s1534, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1534, 0);
    t1535 = v_x;
  }
  if ((!t1535) && u_is_con(s1534, &n_list_46_Cons)) {
    u_value v_more = u_field(s1534, 1);
    t1535 = f_parse_46_last_part_1(v_more);
  }
  if (!t1535) {
    static struct u_object l__1536 = U_LIT("");
    t1535 = &l__1536;
  }
  if (!t1535) {
    u_no_case(s1534);
  }
  return t1535;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1541 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1540;
  {
    u_value v_f = t1541;
    u_value t1538 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1538)) {
      static struct u_object l__46_u_1537 = U_LIT(".u");
      t1538 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1537);
    }
    u_value t1539;
    if (u_truth(t1538)) {
      t1539 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1539 = v_f;
    }
    t1540 = t1539;
  }
  return t1540;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1546 = u_str_split(v_path, u_mk_char(47));
  u_value t1545;
  {
    u_value v_parts = t1546;
    u_value t1544;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1542 = U_LIT(".");
      t1544 = &l__46__1542;
    } else {
      static struct u_object l__47__1543 = U_LIT("/");
      t1544 = u_str_join(&l__47__1543, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1545 = t1544;
  }
  return t1545;
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
    static struct u_object l_prelude_1547 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1547, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

u_value f_parse_46_commands_of_1(u_value v_p) {
  u_value s1548 = v_p;
  u_value t1549 = NULL;
  if ((!t1549) && u_is_con(s1548, &n_syntax_46_Program)) {
    u_value v_cs = u_field(s1548, 0);
    t1549 = v_cs;
  }
  if (!t1549) {
    u_no_case(s1548);
  }
  return t1549;
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1550 = u_mk_tuple(2, v_cs, v_ds);
    int d1551 = 0;
    if ((!d1551) && (u_is_con(u_field(s1550, 0), &n_list_46_Cons) && u_is_con(u_field(s1550, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1550, 0), 0);
      u_value v_more = u_field(u_field(s1550, 0), 1);
      u_value v_d = u_field(u_field(s1550, 1), 0);
      u_value v_rest = u_field(u_field(s1550, 1), 1);
      d1551 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1551) {
      d1551 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1551) {
      u_no_case(s1550);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1552 = v_paths;
    int d1553 = 0;
    if ((!d1553) && u_is_con(s1552, &n_list_46_Nil)) {
      d1553 = 1;
      {
      }
    }
    if ((!d1553) && u_is_con(s1552, &n_list_46_Cons)) {
      u_value v_path = u_field(s1552, 0);
      u_value v_more = u_field(s1552, 1);
      d1553 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_parse_46_commands_of_1(v_p), f_parse_46_commands_of_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1553) {
      u_no_case(s1552);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1554 = v_paths;
    int d1555 = 0;
    if ((!d1555) && u_is_con(s1554, &n_list_46_Nil)) {
      d1555 = 1;
      {
      }
    }
    if ((!d1555) && u_is_con(s1554, &n_list_46_Cons)) {
      u_value v_path = u_field(s1554, 0);
      u_value v_more = u_field(s1554, 1);
      d1555 = 1;
      {
        u_value t1556 = u_mk_bool(0);
        v_failed = t1556;
        {
          struct u_handler *h1557 = u_push();
          if (setjmp(h1557->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1557);
          } else {
            u_pop(h1557);
            int d1558 = 0;
            if ((!d1558) && (u_has_type(h1557, "parse.ParseError") && u_is_con(u_caught(h1557), &n_parse_46_ParseError))) {
              d1558 = 1;
              {
                u_value t1559 = u_mk_bool(1);
                v_failed = t1559;
              }
            }
            if (!d1558) {
              u_reraise(h1557);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1555) {
      u_no_case(s1554);
    }
  }
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
  static struct u_object l_u_value_1560 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1560);
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
  static struct u_object l_u_value_1561 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1561));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1562 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1563 = &t1562;
  u_value l1564 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1564, &n_list_46_Cons)) {
    u_value v_c = u_field(l1564, 0);
    if (u_truth(u_mk_bool(1))) {
      e1563 = u_append(e1563, f_compile_46_mangle_char_1(v_c));
    }
    l1564 = u_field(l1564, 1);
  }
  return u_string(f_list_46_concat_1(t1562));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1565 = u_is_letter(v_c);
  if (!u_truth(t1565)) {
    t1565 = u_is_digit(v_c);
  }
  u_value t1566 = t1565;
  if (!u_truth(t1566)) {
    t1566 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1569;
  if (u_truth(t1566)) {
    t1569 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1567 = U_LIT("_");
    static struct u_object l___1568 = U_LIT("_");
    t1569 = u_chars(u_concat(&l___1567, u_concat(u_str_show(u_ord(v_c)), &l___1568)));
  }
  return t1569;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1570 = U_LIT("f_");
  static struct u_object l___1571 = U_LIT("_");
  return u_concat(&l_f__1570, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1571, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1572 = U_LIT("p_");
  return u_concat(&l_p__1572, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1573 = U_LIT("v_");
  return u_concat(&l_v__1573, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1574 = U_LIT("r_");
  return u_concat(&l_r__1574, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1575 = U_LIT("+");
  static struct u_object l_u_add_1576 = U_LIT("u_add");
  static struct u_object l__45__1577 = U_LIT("-");
  static struct u_object l_u_sub_1578 = U_LIT("u_sub");
  static struct u_object l__42__1579 = U_LIT("*");
  static struct u_object l_u_mul_1580 = U_LIT("u_mul");
  static struct u_object l__47__1581 = U_LIT("/");
  static struct u_object l_u_div_1582 = U_LIT("u_div");
  static struct u_object l__37__1583 = U_LIT("%");
  static struct u_object l_u_mod_1584 = U_LIT("u_mod");
  static struct u_object l__60__1585 = U_LIT("<");
  static struct u_object l_u_lt_1586 = U_LIT("u_lt");
  static struct u_object l__61__61__1587 = U_LIT("==");
  static struct u_object l_u_eq_1588 = U_LIT("u_eq");
  static struct u_object l_compare_1589 = U_LIT("compare");
  static struct u_object l_u_compare_1590 = U_LIT("u_compare");
  static struct u_object l_chars_1591 = U_LIT("chars");
  static struct u_object l_u_chars_1592 = U_LIT("u_chars");
  static struct u_object l_string_1593 = U_LIT("string");
  static struct u_object l_u_string_1594 = U_LIT("u_string");
  static struct u_object l_ord_1595 = U_LIT("ord");
  static struct u_object l_u_ord_1596 = U_LIT("u_ord");
  static struct u_object l_chr_1597 = U_LIT("chr");
  static struct u_object l_u_chr_1598 = U_LIT("u_chr");
  static struct u_object l__94__1599 = U_LIT("^");
  static struct u_object l_u_concat_1600 = U_LIT("u_concat");
  static struct u_object l_str_size_1601 = U_LIT("str_size");
  static struct u_object l_u_str_size_1602 = U_LIT("u_str_size");
  static struct u_object l_str_join_1603 = U_LIT("str_join");
  static struct u_object l_u_str_join_1604 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1605 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1606 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1607 = U_LIT("str_split");
  static struct u_object l_u_str_split_1608 = U_LIT("u_str_split");
  static struct u_object l_str_get_1609 = U_LIT("str_get");
  static struct u_object l_u_str_get_1610 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1611 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1612 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1613 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1614 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1615 = U_LIT("str_show");
  static struct u_object l_u_str_show_1616 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1617 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1618 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1619 = U_LIT("str_number");
  static struct u_object l_u_str_number_1620 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1621 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1622 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1623 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1624 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1625 = U_LIT("is_space");
  static struct u_object l_u_is_space_1626 = U_LIT("u_is_space");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1575), u_mk_tuple(2, &l_u_add_1576, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1577), u_mk_tuple(2, &l_u_sub_1578, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1579), u_mk_tuple(2, &l_u_mul_1580, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1581), u_mk_tuple(2, &l_u_div_1582, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1583), u_mk_tuple(2, &l_u_mod_1584, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1585), u_mk_tuple(2, &l_u_lt_1586, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1587), u_mk_tuple(2, &l_u_eq_1588, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1589), u_mk_tuple(2, &l_u_compare_1590, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1591), u_mk_tuple(2, &l_u_chars_1592, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1593), u_mk_tuple(2, &l_u_string_1594, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1595), u_mk_tuple(2, &l_u_ord_1596, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1597), u_mk_tuple(2, &l_u_chr_1598, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1599), u_mk_tuple(2, &l_u_concat_1600, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1601), u_mk_tuple(2, &l_u_str_size_1602, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1603), u_mk_tuple(2, &l_u_str_join_1604, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1605), u_mk_tuple(2, &l_u_str_slice_1606, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1607), u_mk_tuple(2, &l_u_str_split_1608, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1609), u_mk_tuple(2, &l_u_str_get_1610, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1611), u_mk_tuple(2, &l_u_str_contains_1612, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1613), u_mk_tuple(2, &l_u_str_compare_1614, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1615), u_mk_tuple(2, &l_u_str_show_1616, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1617), u_mk_tuple(2, &l_u_str_quote_1618, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1619), u_mk_tuple(2, &l_u_str_number_1620, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1621), u_mk_tuple(2, &l_u_is_digit_1622, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1623), u_mk_tuple(2, &l_u_is_letter_1624, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1625), u_mk_tuple(2, &l_u_is_space_1626, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_funs_of_1(u_value v_gl) {
  u_value s1627 = v_gl;
  u_value t1628 = NULL;
  if ((!t1628) && u_is_con(s1627, &n_compile_46_Globals)) {
    u_value v_fs = u_field(s1627, 0);
    t1628 = v_fs;
  }
  if (!t1628) {
    u_no_case(s1627);
  }
  return t1628;
}

u_value f_compile_46_constrs_of_1(u_value v_gl) {
  u_value s1629 = v_gl;
  u_value t1630 = NULL;
  if ((!t1630) && u_is_con(s1629, &n_compile_46_Globals)) {
    u_value v_ks = u_field(s1629, 1);
    t1630 = v_ks;
  }
  if (!t1630) {
    u_no_case(s1629);
  }
  return t1630;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1631 = U_LIT("&");
  static struct u_object l_n__1632 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1631, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1632, f_compile_46_mangle_1(v_n))));
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
  u_value s1633 = v_n;
  u_value t1634 = NULL;
  if ((!t1634) && (u_is_con(s1633, &n_syntax_46_Name) && u_is_con(u_field(s1633, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1633, 0), 0);
    u_value v_b = u_field(s1633, 1);
    static struct u_object l_const_32_struct_32_u_n_1635 = U_LIT("const struct u_name");
    static struct u_object l_n__1636 = U_LIT("n_");
    t1634 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1635), u_concat(&l_n__1636, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1634) && (u_is_con(s1633, &n_syntax_46_Name) && u_is_con(u_field(s1633, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1633, 1);
    static struct u_object l_const_32_struct_32_u_n_1637 = U_LIT("const struct u_name");
    static struct u_object l_n__1638 = U_LIT("n_");
    static struct u_object l_NULL_1639 = U_LIT("NULL");
    t1634 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1637), u_concat(&l_n__1638, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1639), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1634) {
    u_no_case(s1633);
  }
  return t1634;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1640 = v_cs;
  u_value t1641 = NULL;
  if ((!t1641) && u_is_con(s1640, &n_list_46_Nil)) {
    t1641 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1641) && (u_is_con(s1640, &n_list_46_Cons) && u_is_con(u_field(s1640, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1640, 0), 2);
    u_value v_rest = u_field(s1640, 1);
    u_value t1642 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1643 = &t1642;
    u_value l1644 = v_ks;
    while (u_is_con(l1644, &n_list_46_Cons)) {
      u_value v_k = u_field(l1644, 0);
      if (u_truth(u_mk_bool(1))) {
        e1643 = u_append(e1643, f_check_46_constr_name_1(v_k));
      }
      l1644 = u_field(l1644, 1);
    }
    t1641 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1642, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1641) && u_is_con(s1640, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1640, 1);
    t1641 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1641) {
    u_no_case(s1640);
  }
  return t1641;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1645 = v_ks;
  u_value t1646 = NULL;
  if ((!t1646) && u_is_con(s1645, &n_list_46_Nil)) {
    t1646 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1646) && u_is_con(s1645, &n_list_46_Cons)) {
    u_value v_k = u_field(s1645, 0);
    u_value v_rest = u_field(s1645, 1);
    u_value t1647;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1647 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1647 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1646 = t1647;
  }
  if (!t1646) {
    u_no_case(s1645);
  }
  return t1646;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1648 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1649 = &t1648;
  u_value l1650 = v_xs;
  while (u_is_con(l1650, &n_list_46_Cons)) {
    u_value v_x = u_field(l1650, 0);
    if (u_truth(u_mk_bool(1))) {
      e1649 = u_append(e1649, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1650 = u_field(l1650, 1);
  }
  return t1648;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1651 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1651, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1652 = f_list_46_lookup_2(v_x, v_env);
  u_value t1653 = NULL;
  if ((!t1653) && u_is_con(s1652, &n_option_46_Some)) {
    u_value v_c = u_field(s1652, 0);
    t1653 = v_c;
  }
  if (!t1653) {
    t1653 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1653) {
    u_no_case(s1652);
  }
  return t1653;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1654 = v_c;
  u_value t1655 = NULL;
  if ((!t1655) && (u_is_con(s1654, &n_c_46_CeUnary) && u_equals(u_field(s1654, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1654, 1);
    t1655 = v_p;
  }
  if (!t1655) {
    static struct u_object l__38__1656 = U_LIT("&");
    t1655 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1656, v_c);
  }
  if (!t1655) {
    u_no_case(s1654);
  }
  return t1655;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1657 = v_t;
  u_value t1658 = NULL;
  if ((!t1658) && (u_is_con(s1657, &n_syntax_46_TSort) && u_is_con(u_field(s1657, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1657, 0);
    t1658 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1658) && u_is_con(s1657, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1657, 0);
    u_value v_args = u_field(s1657, 1);
    static struct u_object l__91__1659 = U_LIT("[");
    static struct u_object l__44__32__1660 = U_LIT(", ");
    u_value t1661 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1662 = &t1661;
    u_value l1663 = v_args;
    while (u_is_con(l1663, &n_list_46_Cons)) {
      u_value v_a = u_field(l1663, 0);
      if (u_truth(u_mk_bool(1))) {
        e1662 = u_append(e1662, f_compile_46_type_key_1(v_a));
      }
      l1663 = u_field(l1663, 1);
    }
    static struct u_object l__93__1664 = U_LIT("]");
    t1658 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1659, u_concat(u_str_join(&l__44__32__1660, t1661), &l__93__1664)));
  }
  if ((!t1658) && u_is_con(s1657, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1657, 0);
    u_value v_r = u_field(s1657, 1);
    static struct u_object l__40__1665 = U_LIT("(");
    static struct u_object l__44__32__1666 = U_LIT(", ");
    u_value t1667 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1668 = &t1667;
    u_value l1669 = v_args;
    while (u_is_con(l1669, &n_list_46_Cons)) {
      u_value v_a = u_field(l1669, 0);
      if (u_truth(u_mk_bool(1))) {
        e1668 = u_append(e1668, f_compile_46_type_key_1(v_a));
      }
      l1669 = u_field(l1669, 1);
    }
    static struct u_object l__41__32__45__62__32__1670 = U_LIT(") -> ");
    t1658 = u_concat(&l__40__1665, u_concat(u_str_join(&l__44__32__1666, t1667), u_concat(&l__41__32__45__62__32__1670, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1658) && u_is_con(s1657, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1657, 0);
    static struct u_object l__40__1671 = U_LIT("(");
    static struct u_object l__32__42__32__1672 = U_LIT(" * ");
    u_value t1673 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1674 = &t1673;
    u_value l1675 = v_ts;
    while (u_is_con(l1675, &n_list_46_Cons)) {
      u_value v_a = u_field(l1675, 0);
      if (u_truth(u_mk_bool(1))) {
        e1674 = u_append(e1674, f_compile_46_type_key_1(v_a));
      }
      l1675 = u_field(l1675, 1);
    }
    static struct u_object l__41__1676 = U_LIT(")");
    t1658 = u_concat(&l__40__1671, u_concat(u_str_join(&l__32__42__32__1672, t1673), &l__41__1676));
  }
  if (!t1658) {
    static struct u_object l__63__1677 = U_LIT("?");
    t1658 = &l__63__1677;
  }
  if (!t1658) {
    u_no_case(s1657);
  }
  return t1658;
}

u_value f_compile_46_program_cmds_1(u_value v_p) {
  u_value s1678 = v_p;
  u_value t1679 = NULL;
  if ((!t1679) && u_is_con(s1678, &n_syntax_46_Program)) {
    u_value v_cs = u_field(s1678, 0);
    t1679 = v_cs;
  }
  if (!t1679) {
    u_no_case(s1678);
  }
  return t1679;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1680 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1681 = &t1680;
  u_value l1682 = v_cs;
  while (u_is_con(l1682, &n_list_46_Cons)) {
    u_value v_c = u_field(l1682, 0);
    if (u_truth(u_mk_bool(1))) {
      e1681 = u_append(e1681, f_compile_46_fun_arities_1(v_c));
    }
    l1682 = u_field(l1682, 1);
  }
  u_value t1683 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1684 = &t1683;
  u_value l1685 = v_cs;
  while (u_is_con(l1685, &n_list_46_Cons)) {
    u_value v_c = u_field(l1685, 0);
    if (u_truth(u_mk_bool(1))) {
      e1684 = u_append(e1684, f_compile_46_constr_arities_1(v_c));
    }
    l1685 = u_field(l1685, 1);
  }
  u_value t1687 = f_list_46_concat_1(t1680);
  u_value t1688 = f_list_46_concat_1(t1683);
  u_value t1686;
  {
    u_value v_fs = t1687;
    u_value v_ks = t1688;
    t1686 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1686;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1689 = v_fs;
  u_value t1690 = NULL;
  if ((!t1690) && u_is_con(s1689, &n_list_46_Nil)) {
    t1690 = v_m;
  }
  if ((!t1690) && u_is_con(s1689, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1689, 0), 0);
    u_value v_k = u_field(u_field(s1689, 0), 1);
    u_value v_rest = u_field(s1689, 1);
    u_value s1691 = f_map_46_get_2(v_f, v_m);
    u_value t1692 = NULL;
    if ((!t1692) && u_is_con(s1691, &n_option_46_Some)) {
      u_value v_ks = u_field(s1691, 0);
      t1692 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1692) && u_is_con(s1691, &n_option_46_None)) {
      t1692 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1692) {
      u_no_case(s1691);
    }
    t1690 = t1692;
  }
  if (!t1690) {
    u_no_case(s1689);
  }
  return t1690;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1693 = v_gl;
  u_value t1694 = NULL;
  if ((!t1694) && u_is_con(s1693, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1693, 2);
    u_value s1695 = f_map_46_get_2(v_n, v_fm);
    u_value t1696 = NULL;
    if ((!t1696) && u_is_con(s1695, &n_option_46_Some)) {
      u_value v_ks = u_field(s1695, 0);
      t1696 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1696) && u_is_con(s1695, &n_option_46_None)) {
      t1696 = u_mk_bool(0);
    }
    if (!t1696) {
      u_no_case(s1695);
    }
    t1694 = t1696;
  }
  if (!t1694) {
    u_no_case(s1693);
  }
  return t1694;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1697 = v_gl;
  u_value t1698 = NULL;
  if ((!t1698) && u_is_con(s1697, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1697, 3);
    t1698 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1698) {
    u_no_case(s1697);
  }
  return t1698;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1699 = v_c;
  u_value t1700 = NULL;
  if ((!t1700) && u_is_con(s1699, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1699, 0);
    u_value v_xss = u_field(s1699, 2);
    t1700 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1700) {
    t1700 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1700) {
    u_no_case(s1699);
  }
  return t1700;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1701 = v_c;
  u_value t1702 = NULL;
  if ((!t1702) && u_is_con(s1701, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1701, 2);
    u_value t1703 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1704 = &t1703;
    u_value l1705 = v_ks;
    while (u_is_con(l1705, &n_list_46_Cons)) {
      u_value v_k = u_field(l1705, 0);
      if (u_truth(u_mk_bool(1))) {
        e1704 = u_append(e1704, f_compile_46_constr_arity_1(v_k));
      }
      l1705 = u_field(l1705, 1);
    }
    t1702 = t1703;
  }
  if (!t1702) {
    t1702 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1702) {
    u_no_case(s1701);
  }
  return t1702;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1706 = v_k;
  u_value t1707 = NULL;
  if ((!t1707) && u_is_con(s1706, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1706, 0);
    u_value v_ts = u_field(s1706, 1);
    t1707 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1707) {
    u_no_case(s1706);
  }
  return t1707;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1708 = v_xs;
  u_value t1709 = NULL;
  if ((!t1709) && u_is_con(s1708, &n_list_46_Nil)) {
    t1709 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1709) && u_is_con(s1708, &n_list_46_Cons)) {
    u_value v_x = u_field(s1708, 0);
    u_value v_rest = u_field(s1708, 1);
    t1709 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1709) {
    u_no_case(s1708);
  }
  return t1709;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1710 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1710, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1711 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1711, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1712 = v_cs;
  u_value t1713 = NULL;
  if ((!t1713) && u_is_con(s1712, &n_list_46_Nil)) {
    t1713 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1713) && (u_is_con(s1712, &n_list_46_Cons) && u_is_con(u_field(s1712, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1712, 0);
    t1713 = v_d;
  }
  if ((!t1713) && u_is_con(s1712, &n_list_46_Cons)) {
    u_value v_d = u_field(s1712, 0);
    u_value v_rest = u_field(s1712, 1);
    static struct u_object l__38__38__1714 = U_LIT("&&");
    t1713 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1714, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1713) {
    u_no_case(s1712);
  }
  return t1713;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1715 = v_l;
  u_value t1716 = NULL;
  if ((!t1716) && u_is_con(s1715, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1717 = U_LIT("u_mk_tuple");
    t1716 = f_c_46_c_call_2(&l_u_mk_tuple_1717, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1716) && u_is_con(s1715, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1715, 0);
    static struct u_object l_u_mk_bool_1718 = U_LIT("u_mk_bool");
    u_value t1719;
    if (u_truth(v_b)) {
      t1719 = u_mk_int(1);
    } else {
      t1719 = u_mk_int(0);
    }
    t1716 = f_c_46_c_call_2(&l_u_mk_bool_1718, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1719), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1716) && u_is_con(s1715, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1715, 0);
    u_value t1720 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1720)) {
      t1720 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1723;
    if (u_truth(t1720)) {
      static struct u_object l_u_mk_int_1721 = U_LIT("u_mk_int");
      t1723 = f_c_46_c_call_2(&l_u_mk_int_1721, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1722 = U_LIT("u_mk_int_str");
      t1723 = f_c_46_c_call_2(&l_u_mk_int_str_1722, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1716 = t1723;
  }
  if ((!t1716) && u_is_con(s1715, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1715, 0);
    static struct u_object l_u_mk_lit_1724 = U_LIT("u_mk_lit");
    t1716 = f_c_46_c_call_2(&l_u_mk_lit_1724, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1716) && u_is_con(s1715, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1715, 0);
    static struct u_object l_u_mk_char_1725 = U_LIT("u_mk_char");
    t1716 = f_c_46_c_call_2(&l_u_mk_char_1725, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1716) {
    u_no_case(s1715);
  }
  return t1716;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1726 = v_p;
  u_value t1727 = NULL;
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1726, 0);
    static struct u_object l_u_equals_1728 = U_LIT("u_equals");
    t1727 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1728, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1726, 0);
    u_value v_ps = u_field(s1726, 1);
    static struct u_object l_u_is_con_1729 = U_LIT("u_is_con");
    t1727 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1729, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1726, 0);
    t1727 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1727) {
    t1727 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1727) {
    u_no_case(s1726);
  }
  return t1727;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1730 = v_ps;
  u_value t1731 = NULL;
  if ((!t1731) && u_is_con(s1730, &n_list_46_Nil)) {
    t1731 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1731) && u_is_con(s1730, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1730, 0), 0);
    u_value v_i = u_field(u_field(s1730, 0), 1);
    u_value v_rest = u_field(s1730, 1);
    t1731 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1731) {
    u_no_case(s1730);
  }
  return t1731;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1732 = v_p;
  u_value t1733 = NULL;
  if ((!t1733) && (u_is_con(s1732, &n_syntax_46_PVar) && u_is_con(u_field(s1732, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1732, 0), 0);
    t1733 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1733) && u_is_con(s1732, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1732, 1);
    t1733 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1733) && u_is_con(s1732, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1732, 0);
    t1733 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1733) {
    t1733 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1733) {
    u_no_case(s1732);
  }
  return t1733;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1734 = v_ps;
  u_value t1735 = NULL;
  if ((!t1735) && u_is_con(s1734, &n_list_46_Nil)) {
    t1735 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1735) && u_is_con(s1734, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1734, 0), 0);
    u_value v_i = u_field(u_field(s1734, 0), 1);
    u_value v_rest = u_field(s1734, 1);
    t1735 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1735) {
    u_no_case(s1734);
  }
  return t1735;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1736 = v_p;
  u_value t1737 = NULL;
  if ((!t1737) && (u_is_con(s1736, &n_syntax_46_PVar) && u_is_con(u_field(s1736, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1736, 0), 0);
    t1737 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1737) && u_is_con(s1736, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1736, 1);
    u_value t1738 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1739 = &t1738;
    u_value l1740 = v_ps;
    while (u_is_con(l1740, &n_list_46_Cons)) {
      u_value v_q = u_field(l1740, 0);
      if (u_truth(u_mk_bool(1))) {
        e1739 = u_append(e1739, f_compile_46_pattern_names_1(v_q));
      }
      l1740 = u_field(l1740, 1);
    }
    t1737 = f_list_46_concat_1(t1738);
  }
  if ((!t1737) && u_is_con(s1736, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1736, 0);
    u_value t1741 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1742 = &t1741;
    u_value l1743 = v_ps;
    while (u_is_con(l1743, &n_list_46_Cons)) {
      u_value v_q = u_field(l1743, 0);
      if (u_truth(u_mk_bool(1))) {
        e1742 = u_append(e1742, f_compile_46_pattern_names_1(v_q));
      }
      l1743 = u_field(l1743, 1);
    }
    t1737 = f_list_46_concat_1(t1741);
  }
  if (!t1737) {
    t1737 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1737) {
    u_no_case(s1736);
  }
  return t1737;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1744 = v_e;
  u_value t1745 = NULL;
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_ELit)) {
    t1745 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1745) && (u_is_con(s1744, &n_syntax_46_EVar) && u_is_con(u_field(s1744, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1744, 0), 0);
    u_value t1746;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1746 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1746 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1745 = t1746;
  }
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1744, 0);
    u_value v_args = u_field(s1744, 1);
    u_value t1747 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1748 = &t1747;
    u_value l1749 = v_args;
    while (u_is_con(l1749, &n_list_46_Cons)) {
      u_value v_a = u_field(l1749, 0);
      if (u_truth(u_mk_bool(1))) {
        e1748 = u_append(e1748, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1749 = u_field(l1749, 1);
    }
    t1745 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1747));
  }
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1744, 0);
    u_value v_b = u_field(s1744, 1);
    t1745 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1744, 0);
    u_value v_b = u_field(s1744, 1);
    u_value t1750 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1751 = &t1750;
    u_value l1752 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1752, &n_list_46_Cons)) {
      u_value v_a = u_field(l1752, 0);
      if (u_truth(u_mk_bool(1))) {
        e1751 = u_append(e1751, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1752 = u_field(l1752, 1);
    }
    t1745 = f_list_46__43__43__2(f_list_46_concat_1(t1750), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1744, 0);
    u_value v_l = u_field(s1744, 1);
    u_value v_r = u_field(s1744, 2);
    t1745 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1744, 0);
    u_value v_cs = u_field(s1744, 1);
    u_value t1753 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1754 = &t1753;
    u_value l1755 = v_cs;
    while (u_is_con(l1755, &n_list_46_Cons)) {
      u_value v_b = u_field(l1755, 0);
      if (u_truth(u_mk_bool(1))) {
        e1754 = u_append(e1754, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1755 = u_field(l1755, 1);
    }
    t1745 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1753));
  }
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1744, 0);
    u_value v_xs = u_field(s1744, 1);
    u_value v_ls = u_field(s1744, 2);
    u_value v_c = u_field(s1744, 3);
    u_value t1756 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1757 = &t1756;
    u_value l1758 = v_ls;
    while (u_is_con(l1758, &n_list_46_Cons)) {
      u_value v_l = u_field(l1758, 0);
      if (u_truth(u_mk_bool(1))) {
        e1757 = u_append(e1757, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1758 = u_field(l1758, 1);
    }
    t1745 = f_list_46__43__43__2(f_list_46_concat_1(t1756), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1745) && u_is_con(s1744, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1744, 0);
    u_value t1759 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1760 = &t1759;
    u_value l1761 = v_es;
    while (u_is_con(l1761, &n_list_46_Cons)) {
      u_value v_a = u_field(l1761, 0);
      if (u_truth(u_mk_bool(1))) {
        e1760 = u_append(e1760, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1761 = u_field(l1761, 1);
    }
    t1745 = f_list_46_concat_1(t1759);
  }
  if (!t1745) {
    u_no_case(s1744);
  }
  return t1745;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1762 = v_b;
  u_value t1763 = NULL;
  if ((!t1763) && (u_is_con(s1762, &n_syntax_46_Branch) && u_is_con(u_field(s1762, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1762, 0);
    u_value v_g = u_field(u_field(s1762, 1), 0);
    u_value v_e = u_field(s1762, 2);
    t1763 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1763) && (u_is_con(s1762, &n_syntax_46_Branch) && u_is_con(u_field(s1762, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1762, 0);
    u_value v_e = u_field(s1762, 2);
    t1763 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1763) {
    u_no_case(s1762);
  }
  return t1763;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1764 = v_xss;
  u_value t1765 = NULL;
  if ((!t1765) && u_is_con(s1764, &n_list_46_Nil)) {
    t1765 = v_b;
  }
  if ((!t1765) && u_is_con(s1764, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1764, 0);
    u_value v_rest = u_field(s1764, 1);
    t1765 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1765) {
    u_no_case(s1764);
  }
  return t1765;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1766 = *r_g;
    int d1767 = 0;
    if ((!d1767) && u_is_con(s1766, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1766, 0);
      u_value v_ds = u_field(s1766, 1);
      d1767 = 1;
      {
        u_value t1768 = u_concat(v_stem, u_str_show(v_n));
        u_value t1769 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1768;
        *r_g = t1769;
      }
    }
    if (!d1767) {
      u_no_case(s1766);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1770 = *r_g;
    int d1771 = 0;
    if ((!d1771) && u_is_con(s1770, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1770, 0);
      u_value v_ds = u_field(s1770, 1);
      d1771 = 1;
      {
        u_value t1772 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1772;
      }
    }
    if (!d1771) {
      u_no_case(s1770);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1773 = U_LIT("u_mk_closure");
        static struct u_object l_w__1774 = U_LIT("w_");
        u_value t1775 = f_c_46_c_call_2(&l_u_mk_closure_1773, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1774, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1775;
      }
    } else {
      {
        u_value s1776 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1777 = 0;
        if ((!d1777) && (u_is_con(s1776, &n_option_46_Some) && u_equals(u_field(s1776, 0), u_mk_int(0)))) {
          d1777 = 1;
          {
            static struct u_object l_u_mk_con_1778 = U_LIT("u_mk_con");
            u_value t1779 = f_c_46_c_call_2(&l_u_mk_con_1778, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1779;
          }
        }
        if ((!d1777) && u_is_con(s1776, &n_option_46_Some)) {
          d1777 = 1;
          {
            static struct u_object l_u_mk_closure_1780 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1781 = U_LIT("wc_");
            u_value t1782 = f_c_46_c_call_2(&l_u_mk_closure_1780, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1781, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1782;
          }
        }
        if ((!d1777) && u_is_con(s1776, &n_option_46_None)) {
          d1777 = 1;
          {
            u_value s1783 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1784 = 0;
            if ((!d1784) && u_is_con(s1783, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1783, 0), 0);
              d1784 = 1;
              {
                static struct u_object l_u_mk_closure_1785 = U_LIT("u_mk_closure");
                static struct u_object l_w_1786 = U_LIT("w");
                u_value t1787 = f_c_46_c_call_2(&l_u_mk_closure_1785, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1786, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1787;
              }
            }
            if ((!d1784) && u_is_con(s1783, &n_option_46_None)) {
              d1784 = 1;
              {
                static struct u_object l_the_32_variable_32__1788 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1788, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1784) {
              u_no_case(s1783);
            }
          }
        }
        if (!d1777) {
          u_no_case(s1776);
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
    u_value s1789 = v_e;
    int d1790 = 0;
    if ((!d1790) && (u_is_con(s1789, &n_syntax_46_ELit) && u_is_con(u_field(s1789, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1789, 0), 0);
      d1790 = 1;
      {
        static struct u_object l_l__1791 = U_LIT("l_");
        static struct u_object l___1792 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1791, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), u_mk_int(16)))), &l___1792)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1793 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1794 = U_LIT("U_LIT");
        static struct u_object l__38__1795 = U_LIT("&");
        u_value t1796 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1793), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1794, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1797 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1795, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1796;
        *r_r = t1797;
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1789, 0);
      d1790 = 1;
      {
        u_value t1798 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1799 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1798;
        *r_r = t1799;
      }
    }
    if ((!d1790) && (u_is_con(s1789, &n_syntax_46_EVar) && u_is_con(u_field(s1789, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1789, 0), 0);
      u_value v_ty = u_field(u_field(s1789, 0), 1);
      d1790 = 1;
      {
        u_value s1800 = f_list_46_lookup_2(v_n, v_env);
        int d1801 = 0;
        if ((!d1801) && u_is_con(s1800, &n_option_46_Some)) {
          u_value v_c = u_field(s1800, 0);
          d1801 = 1;
          {
            u_value t1802 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1803 = v_c;
            *r_ss = t1802;
            *r_r = t1803;
          }
        }
        if ((!d1801) && u_is_con(s1800, &n_option_46_None)) {
          d1801 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1804 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1804;
          }
        }
        if (!d1801) {
          u_no_case(s1800);
        }
      }
    }
    if ((!d1790) && (u_is_con(s1789, &n_syntax_46_EApp) && (u_is_con(u_field(s1789, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1789, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1789, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1789, 0), 0), 1);
      u_value v_args = u_field(s1789, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1790 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1789, 0);
      u_value v_args = u_field(s1789, 1);
      d1790 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1805 = U_LIT("u_apply");
        u_value t1806 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1807 = f_c_46_c_call_2(&l_u_apply_1805, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1806;
        *r_r = t1807;
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1789, 0);
      u_value v_b = u_field(s1789, 1);
      d1790 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1789, 0);
      u_value v_b = u_field(s1789, 1);
      d1790 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1808 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1808, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1809 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1809;
        u_value t1810 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1810;
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1789, 0);
      u_value v_l = u_field(s1789, 1);
      u_value v_rr = u_field(s1789, 2);
      d1790 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1811 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1811, &v_t, r_g);
        u_value t1812 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1812;
        u_value t1813 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1813;
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1789, 0);
      u_value v_cases = u_field(s1789, 1);
      d1790 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1814 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1814, &v_d, r_g);
        static struct u_object l_t_1815 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1815, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1816 = U_LIT("NULL");
        u_value t1817 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1816))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1817;
        static struct u_object l__33__1818 = U_LIT("!");
        static struct u_object l_u_no_case_1819 = U_LIT("u_no_case");
        u_value t1820 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1818, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1819, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1820;
        u_value t1821 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1821;
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1789, 0);
      u_value v_xs = u_field(s1789, 1);
      u_value v_lists = u_field(s1789, 2);
      u_value v_c = u_field(s1789, 3);
      d1790 = 1;
      {
        static struct u_object l_t_1822 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1822, &v_t, r_g);
        static struct u_object l_e_1823 = U_LIT("e");
        p_compile_46_gen_name(&l_e_1823, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_1824 = U_LIT("u_mk_con");
        static struct u_object l__38__1825 = U_LIT("&");
        u_value t1826 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_1824, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__1825, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t1826;
        u_value t1827 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1827;
      }
    }
    if ((!d1790) && u_is_con(s1789, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1789, 0);
      d1790 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_1828 = U_LIT("u_mk_tuple");
        u_value t1829 = f_c_46_c_call_2(&l_u_mk_tuple_1828, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t1829;
      }
    }
    if (!d1790) {
      u_no_case(s1789);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s1830 = v_es;
    int d1831 = 0;
    if ((!d1831) && u_is_con(s1830, &n_list_46_Nil)) {
      d1831 = 1;
      {
        u_value t1832 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1833 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1832;
        *r_rs = t1833;
      }
    }
    if ((!d1831) && u_is_con(s1830, &n_list_46_Cons)) {
      u_value v_e = u_field(s1830, 0);
      u_value v_rest = u_field(s1830, 1);
      d1831 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t1834 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1835 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t1834;
        *r_rs = t1835;
      }
    }
    if (!d1831) {
      u_no_case(s1830);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s1836 = v_rs;
    int d1837 = 0;
    if ((!d1837) && u_is_con(s1836, &n_list_46_Nil)) {
      d1837 = 1;
      {
        u_value t1838 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1838;
      }
    }
    if ((!d1837) && u_is_con(s1836, &n_list_46_Cons)) {
      u_value v_r = u_field(s1836, 0);
      u_value v_rest = u_field(s1836, 1);
      d1837 = 1;
      {
        static struct u_object l_t_1839 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1839, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t1840 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t1840;
      }
    }
    if (!d1837) {
      u_no_case(s1836);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s1841 = u_mk_tuple(2, v_xs, v_ts);
  u_value t1842 = NULL;
  if ((!t1842) && (u_is_con(u_field(s1841, 0), &n_list_46_Cons) && (u_is_con(u_field(s1841, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1841, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s1841, 0), 0);
    u_value v_more = u_field(u_field(s1841, 0), 1);
    u_value v_t = u_field(u_field(u_field(s1841, 1), 0), 1);
    u_value v_rest = u_field(u_field(s1841, 1), 1);
    t1842 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t1842) {
    t1842 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1842) {
    u_no_case(s1841);
  }
  return t1842;
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
    u_value s1843 = v_args;
    int d1844 = 0;
    if ((!d1844) && (u_is_con(s1843, &n_list_46_Cons) && (u_is_con(u_field(s1843, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1843, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s1843, 0);
      u_value v_b = u_field(u_field(s1843, 1), 0);
      static struct u_object l_and_1851 = U_LIT("and");
      u_value t1853 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1851));
      if (!u_truth(t1853)) {
        static struct u_object l_or_1852 = U_LIT("or");
        t1853 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_1852));
      }
      if (u_truth(t1853)) {
        d1844 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_1845 = U_LIT("t");
          p_compile_46_gen_name(&l_t_1845, &v_t, r_g);
          static struct u_object l_and_1846 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1846)))) {
            {
              u_value t1847 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1847;
            }
          } else {
            {
              static struct u_object l__33__1848 = U_LIT("!");
              u_value t1849 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1848, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1849;
            }
          }
          u_value t1850 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t1850;
        }
      }
    }
    if (!d1844) {
      d1844 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t1854 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t1854)) {
          t1854 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t1854)) {
          {
            u_value t1855 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t1855;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_1856 = U_LIT("u_mk_con");
              u_value t1857 = f_c_46_c_call_2(&l_u_mk_con_1856, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t1857;
            }
          } else {
            {
              u_value s1858 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d1859 = 0;
              if ((!d1859) && u_is_con(s1858, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s1858, 0), 0);
                u_value v_k = u_field(u_field(s1858, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d1859 = 1;
                  {
                    u_value t1860 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t1860;
                  }
                }
              }
              if (!d1859) {
                d1859 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_1861 = U_LIT("u_apply");
                  u_value t1862 = f_c_46_c_call_2(&l_u_apply_1861, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t1862;
                }
              }
              if (!d1859) {
                u_no_case(s1858);
              }
            }
          }
        }
      }
    }
    if (!d1844) {
      u_no_case(s1843);
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
    u_value t1863 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1864 = &t1863;
    u_value l1865 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l1865, &n_list_46_Cons)) {
      u_value v_x = u_field(l1865, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e1864 = u_append(e1864, v_x);
      }
      l1865 = u_field(l1865, 1);
    }
    u_value t1866 = t1863;
    v_caps = t1866;
    static struct u_object l_lam_1867 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_1867, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_1868 = U_LIT("env");
    static struct u_object l_args_1869 = U_LIT("args");
    u_value t1870 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_1868), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_1869));
    v_decls = t1870;
    static struct u_object l_env_1871 = U_LIT("env");
    static struct u_object l_args_1872 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_1871), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_1872), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_1873 = U_LIT("u_mk_closure");
    u_value t1874 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1875 = &t1874;
    u_value l1876 = v_caps;
    while (u_is_con(l1876, &n_list_46_Cons)) {
      u_value v_x = u_field(l1876, 0);
      if (u_truth(u_mk_bool(1))) {
        e1875 = u_append(e1875, f_compile_46_var_expr_2(v_env, v_x));
      }
      l1876 = u_field(l1876, 1);
    }
    u_value t1877 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1878 = f_c_46_c_call_2(&l_u_mk_closure_1873, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t1874));
    *r_ss = t1877;
    *r_r = t1878;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s1879 = v_xs;
  u_value t1880 = NULL;
  if ((!t1880) && u_is_con(s1879, &n_list_46_Nil)) {
    t1880 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1880) && u_is_con(s1879, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s1879, 0), 0);
    u_value v_i = u_field(u_field(s1879, 0), 1);
    u_value v_rest = u_field(s1879, 1);
    t1880 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t1880) {
    u_no_case(s1879);
  }
  return t1880;
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
    u_value s1881 = v_cases;
    int d1882 = 0;
    if ((!d1882) && u_is_con(s1881, &n_list_46_Nil)) {
      d1882 = 1;
      {
        u_value t1883 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1883;
      }
    }
    if ((!d1882) && (u_is_con(s1881, &n_list_46_Cons) && u_is_con(u_field(s1881, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1881, 0), 0);
      u_value v_guard = u_field(u_field(s1881, 0), 1);
      u_value v_e = u_field(u_field(s1881, 0), 2);
      u_value v_more = u_field(s1881, 1);
      d1882 = 1;
      {
        u_value t1884 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t1884;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t1885 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t1885;
        u_value s1886 = v_guard;
        int d1887 = 0;
        if ((!d1887) && u_is_con(s1886, &n_option_46_Some)) {
          u_value v_c = u_field(s1886, 0);
          d1887 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t1888 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t1888;
          }
        }
        if ((!d1887) && u_is_con(s1886, &n_option_46_None)) {
          d1887 = 1;
          {
          }
        }
        if (!d1887) {
          u_no_case(s1886);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__1889 = U_LIT("!");
        u_value t1890 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1889, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t1890;
      }
    }
    if (!d1882) {
      u_no_case(s1881);
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
    u_value s1891 = u_mk_tuple(2, v_xs, v_lists);
    int d1892 = 0;
    if ((!d1892) && (u_is_con(u_field(s1891, 0), &n_list_46_Cons) && u_is_con(u_field(s1891, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s1891, 0), 0);
      u_value v_more = u_field(u_field(s1891, 0), 1);
      u_value v_list = u_field(u_field(s1891, 1), 0);
      u_value v_others = u_field(u_field(s1891, 1), 1);
      d1892 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_1893 = U_LIT("l");
        p_compile_46_gen_name(&l_l_1893, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t1894 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1894;
        static struct u_object l_u_is_con_1895 = U_LIT("u_is_con");
        u_value t1896 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_1895, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1896;
      }
    }
    if (!d1892) {
      d1892 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_1897 = U_LIT("u_append");
        u_value t1898 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_1897, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1898;
      }
    }
    if (!d1892) {
      u_no_case(s1891);
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
    u_value s1899 = v_s;
    int d1900 = 0;
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SSkip)) {
      d1900 = 1;
      {
        u_value t1901 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1901;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s1899, 0);
      d1900 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1902 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t1902;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s1899, 0);
      u_value v_body = u_field(s1899, 1);
      d1900 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t1903 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1904 = &t1903;
        u_value l1905 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l1905, &n_list_46_Cons)) {
          u_value v_x = u_field(l1905, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_1906 = U_LIT("NULL");
            e1904 = u_append(e1904, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1906))));
          }
          l1905 = u_field(l1905, 1);
        }
        u_value t1907 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t1903, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t1907;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s1899, 0);
      u_value v_l = u_field(s1899, 1);
      u_value v_r = u_field(s1899, 2);
      d1900 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t1908 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1908;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s1899, 0);
      u_value v_b = u_field(s1899, 1);
      d1900 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_1909 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1909, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t1910 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1910;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s1899, 0);
      u_value v_ins = u_field(s1899, 1);
      u_value v_outs = u_field(s1899, 2);
      u_value v_refs = u_field(s1899, 3);
      d1900 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t1911 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1912 = &t1911;
        u_value l1913 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l1913, &n_list_46_Cons)) {
          u_value v_x = u_field(l1913, 0);
          if (u_truth(u_mk_bool(1))) {
            e1912 = u_append(e1912, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l1913 = u_field(l1913, 1);
        }
        u_value t1914 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t1911))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1914;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s1899, 0);
      u_value v_cases = u_field(s1899, 1);
      d1900 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1915 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1915, &v_d, r_g);
        static struct u_object l_d_1916 = U_LIT("d");
        p_compile_46_gen_name(&l_d_1916, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t1917 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1917;
        static struct u_object l__33__1918 = U_LIT("!");
        static struct u_object l_u_no_case_1919 = U_LIT("u_no_case");
        u_value t1920 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1918, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1919, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1920;
      }
    }
    if ((!d1900) && (u_is_con(s1899, &n_syntax_46_SAssert) && (u_is_con(u_field(s1899, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s1899, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s1899, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s1899, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s1899, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s1899, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s1899, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1899, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s1899, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s1899, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s1899, 0), 1), 1), 0);
      d1900 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_1921 = U_LIT("u_assert_equal");
        u_value t1922 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_1921, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1922;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s1899, 0);
      d1900 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_1923 = U_LIT("u_assert_true");
        u_value t1924 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_1923, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1924;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s1899, 0);
      u_value v_ty = u_field(s1899, 1);
      d1900 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_1925 = U_LIT("u_raise");
        u_value t1926 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_1925, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1926;
      }
    }
    if ((!d1900) && u_is_con(s1899, &n_syntax_46_STry)) {
      u_value v_body = u_field(s1899, 0);
      u_value v_hs = u_field(s1899, 1);
      d1900 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_1927 = U_LIT("h");
        p_compile_46_gen_name(&l_h_1927, &v_d, r_g);
        static struct u_object l_d_1928 = U_LIT("d");
        p_compile_46_gen_name(&l_d_1928, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_1929 = U_LIT("u_pop");
        static struct u_object l__33__1930 = U_LIT("!");
        static struct u_object l_u_reraise_1931 = U_LIT("u_reraise");
        u_value t1932 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_1929, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1930, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_1931, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t1932;
        static struct u_object l_struct_32_u_handler_1933 = U_LIT("struct u_handler");
        static struct u_object l_u_push_1934 = U_LIT("u_push");
        static struct u_object l__61__61__1935 = U_LIT("==");
        static struct u_object l_setjmp_1936 = U_LIT("setjmp");
        static struct u_object l_env_1937 = U_LIT("env");
        static struct u_object l_u_pop_1938 = U_LIT("u_pop");
        u_value t1939 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_1933)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_1934, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__1935, f_c_46_c_call_2(&l_setjmp_1936, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_1937), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_1938, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t1939;
      }
    }
    if (!d1900) {
      u_no_case(s1899);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s1940 = v_xs;
    int d1941 = 0;
    if ((!d1941) && u_is_con(s1940, &n_list_46_Nil)) {
      d1941 = 1;
      {
        u_value t1942 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1942;
      }
    }
    if ((!d1941) && u_is_con(s1940, &n_list_46_Cons)) {
      u_value v_x = u_field(s1940, 0);
      u_value v_rest = u_field(s1940, 1);
      d1941 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t1943 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t1943;
      }
    }
    if (!d1941) {
      u_no_case(s1940);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s1944 = u_mk_tuple(2, v_xs, v_ts);
  u_value t1945 = NULL;
  if ((!t1945) && (u_is_con(u_field(s1944, 0), &n_list_46_Cons) && (u_is_con(u_field(s1944, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1944, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s1944, 0), 0);
    u_value v_more = u_field(u_field(s1944, 0), 1);
    u_value v_t = u_field(u_field(u_field(s1944, 1), 0), 1);
    u_value v_rest = u_field(u_field(s1944, 1), 1);
    t1945 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t1945) {
    t1945 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1945) {
    u_no_case(s1944);
  }
  return t1945;
}

void p_compile_46_handler_cases(u_value v_gl, u_value v_env, u_value v_hs, u_value v_h, u_value v_d, u_value *r_ss, u_value *r_g) {
  {
    u_value v_bs = NULL;
    u_value v_gs = NULL;
    u_value v_rg = NULL;
    u_value v_body = NULL;
    u_value v_rest = NULL;
    u_value v_env1 = NULL;
    u_value v_v = NULL;
    u_value v_tests = NULL;
    static struct u_object l_u_caught_1946 = U_LIT("u_caught");
    u_value t1947 = f_c_46_c_call_2(&l_u_caught_1946, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t1947;
    u_value s1948 = v_hs;
    int d1949 = 0;
    if ((!d1949) && u_is_con(s1948, &n_list_46_Nil)) {
      d1949 = 1;
      {
        u_value t1950 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1950;
      }
    }
    if ((!d1949) && (u_is_con(s1948, &n_list_46_Cons) && u_is_con(u_field(s1948, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s1948, 0), 0);
      u_value v_p = u_field(u_field(s1948, 0), 1);
      u_value v_guard = u_field(u_field(s1948, 0), 2);
      u_value v_s = u_field(u_field(s1948, 0), 3);
      u_value v_more = u_field(s1948, 1);
      d1949 = 1;
      {
        u_value t1951 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t1951;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t1952 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t1952;
        u_value s1953 = v_guard;
        int d1954 = 0;
        if ((!d1954) && u_is_con(s1953, &n_option_46_Some)) {
          u_value v_c = u_field(s1953, 0);
          d1954 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t1955 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t1955;
          }
        }
        if ((!d1954) && u_is_con(s1953, &n_option_46_None)) {
          d1954 = 1;
          {
          }
        }
        if (!d1954) {
          u_no_case(s1953);
        }
        u_value s1956 = v_p;
        int d1957 = 0;
        if ((!d1957) && u_is_con(s1956, &n_syntax_46_PWild)) {
          d1957 = 1;
          {
            u_value t1958 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t1958;
          }
        }
        if (!d1957) {
          d1957 = 1;
          {
            static struct u_object l_u_has_type_1959 = U_LIT("u_has_type");
            u_value t1960 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_1959, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t1960;
          }
        }
        if (!d1957) {
          u_no_case(s1956);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__1961 = U_LIT("!");
        u_value t1962 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1961, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t1962;
      }
    }
    if (!d1949) {
      u_no_case(s1948);
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
    u_value s1963 = v_cases;
    int d1964 = 0;
    if ((!d1964) && u_is_con(s1963, &n_list_46_Nil)) {
      d1964 = 1;
      {
        u_value t1965 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1965;
      }
    }
    if ((!d1964) && (u_is_con(s1963, &n_list_46_Cons) && u_is_con(u_field(s1963, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1963, 0), 0);
      u_value v_guard = u_field(u_field(s1963, 0), 1);
      u_value v_s = u_field(u_field(s1963, 0), 2);
      u_value v_more = u_field(s1963, 1);
      d1964 = 1;
      {
        u_value t1966 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t1966;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t1967 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t1967;
        u_value s1968 = v_guard;
        int d1969 = 0;
        if ((!d1969) && u_is_con(s1968, &n_option_46_Some)) {
          u_value v_c = u_field(s1968, 0);
          d1969 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t1970 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t1970;
          }
        }
        if ((!d1969) && u_is_con(s1968, &n_option_46_None)) {
          d1969 = 1;
          {
          }
        }
        if (!d1969) {
          u_no_case(s1968);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__1971 = U_LIT("!");
        u_value t1972 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1971, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t1972;
      }
    }
    if (!d1964) {
      u_no_case(s1963);
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
    u_value s1973 = v_c;
    int d1974 = 0;
    if ((!d1974) && u_is_con(s1973, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s1973, 0);
      u_value v_xss = u_field(s1973, 2);
      u_value v_b = u_field(s1973, 4);
      d1974 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t1975 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1976 = &t1975;
        u_value l1977 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l1977, &n_list_46_Cons)) {
          u_value v_x = u_field(l1977, 0);
          if (u_truth(u_mk_bool(1))) {
            e1976 = u_append(e1976, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l1977 = u_field(l1977, 1);
        }
        u_value t1978 = t1975;
        v_params = t1978;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t1979 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t1979;
          }
        } else {
          {
            u_value t1980 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t1980;
          }
        }
        u_value t1981 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t1982 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t1981;
        *r_ms = t1982;
      }
    }
    if ((!d1974) && u_is_con(s1973, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s1973, 0);
      u_value v_ins = u_field(s1973, 2);
      u_value v_outs = u_field(s1973, 3);
      u_value v_refs = u_field(s1973, 4);
      u_value v_body = u_field(s1973, 5);
      d1974 = 1;
      {
        u_value t1983 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1984 = &t1983;
        u_value l1985 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l1985, &n_list_46_Cons)) {
          u_value v_x = u_field(l1985, 0);
          if (u_truth(u_mk_bool(1))) {
            e1984 = u_append(e1984, f_compile_46_pointer_1(v_x));
          }
          l1985 = u_field(l1985, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t1983), v_body, &v_bs, r_g);
        u_value t1986 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1987 = &t1986;
        u_value l1988 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l1988, &n_list_46_Cons)) {
          u_value v_x = u_field(l1988, 0);
          if (u_truth(u_mk_bool(1))) {
            e1987 = u_append(e1987, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l1988 = u_field(l1988, 1);
        }
        u_value t1989 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1990 = &t1989;
        u_value l1991 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l1991, &n_list_46_Cons)) {
          u_value v_x = u_field(l1991, 0);
          if (u_truth(u_mk_bool(1))) {
            e1990 = u_append(e1990, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l1991 = u_field(l1991, 1);
        }
        u_value t1992 = f_list_46__43__43__2(t1986, t1989);
        v_params = t1992;
        u_value t1993 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t1994 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t1993;
        *r_ms = t1994;
      }
    }
    if ((!d1974) && u_is_con(s1973, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s1973, 0);
      d1974 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_1995 = U_LIT("u_print_line");
        u_value t1996 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1997 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_1995, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t1996;
        *r_ms = t1997;
      }
    }
    if ((!d1974) && (u_is_con(s1973, &n_syntax_46_CRun) && u_is_con(u_field(s1973, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s1973, 1), 0);
      u_value v_body = u_field(u_field(s1973, 1), 1);
      d1974 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t1998 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1999 = &t1998;
        u_value l2000 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2000, &n_list_46_Cons)) {
          u_value v_x = u_field(l2000, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2001 = U_LIT("NULL");
            e1999 = u_append(e1999, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2001))));
          }
          l2000 = u_field(l2000, 1);
        }
        u_value t2002 = t1998;
        v_copy = t2002;
        u_value t2003 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2004 = &t2003;
        u_value l2005 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2005, &n_list_46_Cons)) {
          u_value v_x = u_field(l2005, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2006 = U_LIT("u_print_binding");
            e2004 = u_append(e2004, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2006, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2005 = u_field(l2005, 1);
        }
        u_value t2007 = t2003;
        v_back = t2007;
        u_value t2008 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2009 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2008;
        *r_ms = t2009;
      }
    }
    if ((!d1974) && u_is_con(s1973, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s1973, 1);
      d1974 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2010 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2011 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2010;
        *r_ms = t2011;
      }
    }
    if ((!d1974) && u_is_con(s1973, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s1973, 1);
      d1974 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2012 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2013 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2012;
        *r_ms = t2013;
      }
    }
    if (!d1974) {
      d1974 = 1;
      {
        u_value t2014 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2015 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2014;
        *r_ms = t2015;
      }
    }
    if (!d1974) {
      u_no_case(s1973);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2016 = v_cs;
    int d2017 = 0;
    if ((!d2017) && u_is_con(s2016, &n_list_46_Nil)) {
      d2017 = 1;
      {
        u_value t2018 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2019 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2018;
        *r_ms = t2019;
      }
    }
    if ((!d2017) && u_is_con(s2016, &n_list_46_Cons)) {
      u_value v_c = u_field(s2016, 0);
      u_value v_rest = u_field(s2016, 1);
      d2017 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2020 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2021 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2020;
        *r_ms = t2021;
      }
    }
    if (!d2017) {
      u_no_case(s2016);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_static_32_u_value_2022 = U_LIT("static u_value");
  static struct u_object l_cache_2023 = U_LIT("cache");
  static struct u_object l_NULL_2024 = U_LIT("NULL");
  static struct u_object l__61__61__2025 = U_LIT("==");
  static struct u_object l_cache_2026 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2027 = U_LIT("U_COMPUTING");
  static struct u_object l_u_fail_2028 = U_LIT("u_fail");
  static struct u_object l_recursive_32_initia_2029 = U_LIT("recursive initialization of ");
  static struct u_object l_cache_2030 = U_LIT("cache");
  static struct u_object l_cache_2031 = U_LIT("cache");
  static struct u_object l_cache_2032 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2033 = U_LIT("U_COMPUTING");
  static struct u_object l_cache_2034 = U_LIT("cache");
  static struct u_object l_cache_2035 = U_LIT("cache");
  return f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2022), &l_cache_2023, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2024))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2025, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2026), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2027)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2028, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2029, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2030), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2031))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2032), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2033)), u_mk_con(&n_list_46_Nil, 0))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2034), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2035))), u_mk_con(&n_list_46_Nil, 0)))));
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2036 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2037 = &t2036;
  u_value l2038 = f_compile_46_funs_of_1(v_gl);
  while (u_is_con(l2038, &n_list_46_Cons)) {
    u_value v_p = u_field(l2038, 0);
    if (u_truth(u_mk_bool(1))) {
      e2037 = u_append(e2037, f_compile_46_fun_wrapper_1(v_p));
    }
    l2038 = u_field(l2038, 1);
  }
  u_value t2039 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2040 = &t2039;
  u_value l2041 = f_compile_46_constrs_of_1(v_gl);
  while (u_is_con(l2041, &n_list_46_Cons)) {
    u_value v_p = u_field(l2041, 0);
    if (u_truth(u_mk_bool(1))) {
      e2040 = u_append(e2040, f_compile_46_constr_wrapper_1(v_p));
    }
    l2041 = u_field(l2041, 1);
  }
  u_value t2042 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2043 = &t2042;
  u_value l2044 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2044, &n_list_46_Cons)) {
    u_value v_p = u_field(l2044, 0);
    if (u_truth(u_mk_bool(1))) {
      e2043 = u_append(e2043, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2044 = u_field(l2044, 1);
  }
  return f_list_46__43__43__2(t2036, f_list_46__43__43__2(f_list_46_concat_1(t2039), t2042));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2045 = v_p;
  u_value t2046 = NULL;
  if (!t2046) {
    u_value v_n = u_field(s2045, 0);
    u_value v_k = u_field(s2045, 1);
    static struct u_object l_w__2047 = U_LIT("w_");
    t2046 = f_compile_46_wrapper_3(u_concat(&l_w__2047, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2046) {
    u_no_case(s2045);
  }
  return t2046;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2048 = v_p;
  u_value t2049 = NULL;
  if (!t2049) {
    u_value v_f = u_field(s2048, 0);
    u_value v_k = u_field(s2048, 1);
    static struct u_object l_w_2050 = U_LIT("w");
    t2049 = f_compile_46_wrapper_3(u_concat(&l_w_2050, v_f), v_f, v_k);
  }
  if (!t2049) {
    u_no_case(s2048);
  }
  return t2049;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2051 = U_LIT("env");
  static struct u_object l_args_2052 = U_LIT("args");
  u_value t2053 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2054 = &t2053;
  u_value l2055 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2055, &n_list_46_Cons)) {
    u_value v_i = u_field(l2055, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2056 = U_LIT("args");
      e2054 = u_append(e2054, f_compile_46_index_2(&l_args_2056, v_i));
    }
    l2055 = u_field(l2055, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2051), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2052), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2053))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2057 = v_p;
  u_value t2058 = NULL;
  if ((!t2058) && u_equals(u_field(s2057, 1), u_mk_int(0))) {
    t2058 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2058) {
    u_value v_n = u_field(s2057, 0);
    u_value v_k = u_field(s2057, 1);
    static struct u_object l_wc__2059 = U_LIT("wc_");
    static struct u_object l_env_2060 = U_LIT("env");
    static struct u_object l_args_2061 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2062 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2063 = U_LIT("args");
    t2058 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2059, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2060), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2061), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2062, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2063), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2058) {
    u_no_case(s2057);
  }
  return t2058;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2064;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2064 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2064 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2064;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2065 = v_d;
  u_value t2066 = NULL;
  if ((!t2066) && u_is_con(s2065, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2065, 0);
    u_value v_f = u_field(s2065, 1);
    u_value v_ps = u_field(s2065, 2);
    t2066 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2066) {
    t2066 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2066) {
    u_no_case(s2065);
  }
  return t2066;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2067 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    u_value t2068 = f_compile_46_globals_1(f_compile_46_program_cmds_1(v_q));
    v_g = t2067;
    v_gl = t2068;
    p_compile_46_compile_cmds(v_gl, f_compile_46_program_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2069 = v_g;
    int d2070 = 0;
    if ((!d2070) && u_is_con(s2069, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2069, 1);
      d2070 = 1;
      {
        u_value t2071 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2071;
      }
    }
    if (!d2070) {
      u_no_case(s2069);
    }
    static struct u_object l_ulang_46_h_2072 = U_LIT("ulang.h");
    u_value t2073 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2074 = &t2073;
    u_value l2075 = v_fs;
    while (u_is_con(l2075, &n_list_46_Cons)) {
      u_value v_d = u_field(l2075, 0);
      if (u_truth(u_mk_bool(1))) {
        e2074 = u_append(e2074, f_compile_46_prototype_1(v_d));
      }
      l2075 = u_field(l2075, 1);
    }
    static struct u_object l_main_2076 = U_LIT("main");
    static struct u_object l_argc_2077 = U_LIT("argc");
    static struct u_object l_char_2078 = U_LIT("char");
    static struct u_object l_argv_2079 = U_LIT("argv");
    static struct u_object l_u_args_2080 = U_LIT("u_args");
    static struct u_object l_argc_2081 = U_LIT("argc");
    static struct u_object l_argv_2082 = U_LIT("argv");
    u_value t2083 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2072), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_compile_46_program_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2073), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2076, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2077), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2078))), &l_argv_2079), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2080, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2081), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2082), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2083;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2084 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2084, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2085 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2085, f_compile_46_program_cmds_1(v_pre)), f_compile_46_program_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam955(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2086 = v_args;
    int d2087 = 0;
    if ((!d2087) && u_is_con(s2086, &n_list_46_Nil)) {
      d2087 = 1;
      {
      }
    }
    if ((!d2087) && (u_is_con(s2086, &n_list_46_Cons) && (u_is_con(u_field(s2086, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2086, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2086, 0);
      u_value v_target = u_field(u_field(s2086, 1), 0);
      d2087 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2087) {
      d2087 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2088 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2088), "os.OsError");
      }
    }
    if (!d2087) {
      u_no_case(s2086);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
