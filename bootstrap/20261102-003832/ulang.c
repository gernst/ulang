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

u_value w_f_check_46_initialized_1(u_value *env, u_value *args);

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

u_value lam960(u_value *env, u_value *args);

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

u_value w_f_check_46_initialized_1(u_value *env, u_value *args) {
  return f_check_46_initialized_1(args[0]);
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
    v_a = t489;
    u_value t490 = f_check_46_resolve_2(f_check_46_sub_of_1(*r_s), v_t2);
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
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
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

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s660 = v_xs;
  u_value t661 = NULL;
  if ((!t661) && (u_is_con(s660, &n_list_46_Cons) && (u_is_con(u_field(s660, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s660, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s660, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s660, 0), 0), 0), 0);
    u_value v_rest = u_field(s660, 1);
    t661 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t661) {
    t661 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t661) {
    u_no_case(s660);
  }
  return t661;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s662 = v_xs;
    int d663 = 0;
    if ((!d663) && u_is_con(s662, &n_list_46_Nil)) {
      d663 = 1;
      {
      }
    }
    if ((!d663) && (u_is_con(s662, &n_list_46_Cons) && u_is_con(u_field(s662, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s662, 0), 0);
      u_value v_t = u_field(u_field(s662, 0), 1);
      u_value v_rest = u_field(s662, 1);
      d663 = 1;
      {
        u_value t664 = f_check_46_holes_1(v_t);
        if (u_truth(t664)) {
          t664 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t664)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d663) {
      u_no_case(s662);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s665 = v_xs;
    int d666 = 0;
    if ((!d666) && u_is_con(s665, &n_list_46_Nil)) {
      d666 = 1;
      {
        u_value t667 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t667;
      }
    }
    if ((!d666) && u_is_con(s665, &n_list_46_Cons)) {
      u_value v_x = u_field(s665, 0);
      u_value v_more = u_field(s665, 1);
      d666 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t668 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t668;
      }
    }
    if (!d666) {
      u_no_case(s665);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t673;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_669 = U_LIT("os_args");
    static struct u_object l_os_read_670 = U_LIT("os_read");
    static struct u_object l_os_write_671 = U_LIT("os_write");
    static struct u_object l_os_library_672 = U_LIT("os_library");
    t673 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_669), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_670), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_671), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_672), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t673 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t673;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t674 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e675 = &t674;
  static struct u_object l_Program_676 = U_LIT("Program");
  u_value l677 = f_ctx_46_named_2(v_c, &l_Program_676);
  while (u_is_con(l677, &n_list_46_Cons)) {
    u_value v_d = u_field(l677, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e675 = u_append(e675, v_d);
    }
    l677 = u_field(l677, 1);
  }
  u_value s678 = t674;
  u_value t679 = NULL;
  if ((!t679) && u_is_con(s678, &n_list_46_Cons)) {
    u_value v_d = u_field(s678, 0);
    static struct u_object l_read_680 = U_LIT("read");
    static struct u_object l_library_681 = U_LIT("library");
    t679 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_680), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_681), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t679) {
    t679 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t679) {
    u_no_case(s678);
  }
  return t679;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s682 = f_check_46_visible_names_3(*r_s, v_p, f_check_46_procs_named_2(f_check_46_ctx_of_1(*r_s), v_p));
    int d683 = 0;
    if ((!d683) && (u_is_con(s682, &n_list_46_Cons) && u_is_con(u_field(s682, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s682, 0);
      d683 = 1;
      {
        u_value t684 = v_q1;
        v_q = t684;
      }
    }
    if ((!d683) && u_is_con(s682, &n_list_46_Nil)) {
      d683 = 1;
      {
        u_value t685 = v_p;
        v_q = t685;
      }
    }
    if (!d683) {
      u_value v_qs = s682;
      d683 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d683) {
      u_no_case(s682);
    }
    u_value s686 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_ctx_of_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_ctx_of_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_ctx_of_1(*r_s)))));
    int d687 = 0;
    if ((!d687) && u_is_con(s686, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s686, 0), 0);
      u_value v_is = u_field(u_field(s686, 0), 1);
      u_value v_os = u_field(u_field(s686, 0), 2);
      u_value v_rs = u_field(u_field(s686, 0), 3);
      d687 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t688 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e689 = &t688;
        u_value l690 = v_is;
        while (u_is_con(l690, &n_list_46_Cons)) {
          u_value v_t = u_field(l690, 0);
          if (u_truth(u_mk_bool(1))) {
            e689 = u_append(e689, f_check_46_subst_2(v_t, v_m));
          }
          l690 = u_field(l690, 1);
        }
        p_check_46_checks(v_env, v_ins, t688, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t691 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e692 = &t691;
        u_value l693 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l693, &n_list_46_Cons)) {
          u_value v_t = u_field(l693, 0);
          if (u_truth(u_mk_bool(1))) {
            e692 = u_append(e692, f_check_46_subst_2(v_t, v_m));
          }
          l693 = u_field(l693, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t691, r_s);
        u_value t694 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t694;
      }
    }
    if ((!d687) && u_is_con(s686, &n_option_46_None)) {
      d687 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d687) {
      u_no_case(s686);
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
    u_value s695 = v_cases;
    int d696 = 0;
    if ((!d696) && u_is_con(s695, &n_list_46_Nil)) {
      d696 = 1;
      {
        u_value t697 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t697;
      }
    }
    if ((!d696) && (u_is_con(s695, &n_list_46_Cons) && u_is_con(u_field(s695, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s695, 0), 0);
      u_value v_g = u_field(u_field(s695, 0), 1);
      u_value v_b = u_field(u_field(s695, 0), 2);
      u_value v_more = u_field(s695, 1);
      d696 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t698 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t698;
      }
    }
    if (!d696) {
      u_no_case(s695);
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
    u_value s699 = v_hs;
    int d700 = 0;
    if ((!d700) && u_is_con(s699, &n_list_46_Nil)) {
      d700 = 1;
      {
        u_value t701 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t701;
      }
    }
    if ((!d700) && (u_is_con(s699, &n_list_46_Cons) && u_is_con(u_field(s699, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s699, 0), 0);
      u_value v_p = u_field(u_field(s699, 0), 1);
      u_value v_g = u_field(u_field(s699, 0), 2);
      u_value v_b = u_field(u_field(s699, 0), 3);
      u_value v_more = u_field(s699, 1);
      d700 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t702 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t702;
      }
    }
    if (!d700) {
      u_no_case(s699);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t703 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t703)) {
      t703 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t704 = t703;
    if (!u_truth(t704)) {
      t704 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t705 = t704;
    if (!u_truth(t705)) {
      t705 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t705)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t706 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t706)) {
      t706 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t707 = t706;
    if (!u_truth(t707)) {
      t707 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t707)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_708 = U_LIT("Bool");
    static struct u_object l_Int_709 = U_LIT("Int");
    static struct u_object l_Str_710 = U_LIT("Str");
    static struct u_object l_Char_711 = U_LIT("Char");
    static struct u_object l__45__62__712 = U_LIT("->");
    u_value t713 = f_list_46_contains_2(f_syntax_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_708, u_mk_con(&n_list_46_Cons, 2, &l_Int_709, u_mk_con(&n_list_46_Cons, 2, &l_Str_710, u_mk_con(&n_list_46_Cons, 2, &l_Char_711, u_mk_con(&n_list_46_Cons, 2, &l__45__62__712, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t713)) {
      t713 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t714 = t713;
    if (!u_truth(t714)) {
      t714 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t714)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t715 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e716 = &t715;
    u_value l717 = v_ts;
    while (u_is_con(l717, &n_list_46_Cons)) {
      u_value v_t = u_field(l717, 0);
      if (u_truth(u_mk_bool(1))) {
        e716 = u_append(e716, f_check_46_free_1(v_t));
      }
      l717 = u_field(l717, 1);
    }
    u_value t718 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t715)), f_check_46_params_1(v_ps));
    v_undeclared = t718;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s719 = v_cs;
    int d720 = 0;
    if ((!d720) && u_is_con(s719, &n_list_46_Nil)) {
      d720 = 1;
      {
      }
    }
    if ((!d720) && (u_is_con(s719, &n_list_46_Cons) && u_is_con(u_field(u_field(s719, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s719, 0), 0);
      u_value v_n = u_field(u_field(u_field(s719, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s719, 0), 1), 1);
      u_value v_rest = u_field(s719, 1);
      d720 = 1;
      {
        u_value t721 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t721;
        p_check_46_declare_sort(f_check_46_ctx_of_1(*r_s), v_n);
        u_value t722 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_ctx_of_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t722;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d720) && u_is_con(s719, &n_list_46_Cons)) {
      u_value v_rest = u_field(s719, 1);
      d720 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d720) {
      u_no_case(s719);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s723 = v_cs;
    int d724 = 0;
    if ((!d724) && u_is_con(s723, &n_list_46_Nil)) {
      d724 = 1;
      {
      }
    }
    if ((!d724) && (u_is_con(s723, &n_list_46_Cons) && u_is_con(u_field(u_field(s723, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s723, 0), 0);
      u_value v_n = u_field(u_field(u_field(s723, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s723, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s723, 0), 1), 2);
      u_value v_rest = u_field(s723, 1);
      d724 = 1;
      {
        u_value t725 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t725;
        p_check_46_declare_sort(f_check_46_ctx_of_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t726 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_ctx_of_1(*r_s), v_n, v_ps, v_u));
        *r_s = t726;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d724) && u_is_con(s723, &n_list_46_Cons)) {
      u_value v_rest = u_field(s723, 1);
      d724 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d724) {
      u_no_case(s723);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s727 = v_cs;
    int d728 = 0;
    if ((!d728) && u_is_con(s727, &n_list_46_Nil)) {
      d728 = 1;
      {
      }
    }
    if ((!d728) && (u_is_con(s727, &n_list_46_Cons) && u_is_con(u_field(u_field(s727, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s727, 0), 0);
      u_value v_n = u_field(u_field(u_field(s727, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s727, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s727, 0), 1), 2);
      u_value v_rest = u_field(s727, 1);
      d728 = 1;
      {
        u_value t729 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t729;
        p_check_46_unique_ids(v_ps);
        u_value t730 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e731 = &t730;
        u_value l732 = v_ks;
        while (u_is_con(l732, &n_list_46_Cons)) {
          u_value v_k = u_field(l732, 0);
          if (u_truth(u_mk_bool(1))) {
            e731 = u_append(e731, f_check_46_constr_name_1(v_k));
          }
          l732 = u_field(l732, 1);
        }
        u_value t733 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_ctx_of_1(*r_s), v_n, t730));
        *r_s = t733;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d728) && u_is_con(s727, &n_list_46_Cons)) {
      u_value v_rest = u_field(s727, 1);
      d728 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d728) {
      u_no_case(s727);
    }
  }
}

u_value f_check_46_constr_name_1(u_value v_k) {
  u_value s734 = v_k;
  u_value t735 = NULL;
  if ((!t735) && u_is_con(s734, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s734, 0);
    t735 = v_n;
  }
  if (!t735) {
    u_no_case(s734);
  }
  return t735;
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s736 = v_ks;
    int d737 = 0;
    if ((!d737) && u_is_con(s736, &n_list_46_Nil)) {
      d737 = 1;
      {
      }
    }
    if ((!d737) && (u_is_con(s736, &n_list_46_Cons) && u_is_con(u_field(s736, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s736, 0), 0);
      u_value v_args = u_field(u_field(s736, 0), 1);
      u_value v_rest = u_field(s736, 1);
      d737 = 1;
      {
        p_check_46_declare_name(f_check_46_ctx_of_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t738 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_ctx_of_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t738;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d737) {
      u_no_case(s736);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s739 = v_xss;
  u_value t740 = NULL;
  if ((!t740) && u_is_con(s739, &n_list_46_Nil)) {
    t740 = v_r;
  }
  if ((!t740) && u_is_con(s739, &n_list_46_Cons)) {
    u_value v_xs = u_field(s739, 0);
    u_value v_rest = u_field(s739, 1);
    t740 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t740) {
    u_no_case(s739);
  }
  return t740;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s741 = v_xss;
  u_value t742 = NULL;
  if ((!t742) && u_is_con(s741, &n_list_46_Nil)) {
    t742 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t742) && u_is_con(s741, &n_list_46_Cons)) {
    u_value v_xs = u_field(s741, 0);
    t742 = v_xs;
  }
  if (!t742) {
    u_no_case(s741);
  }
  return t742;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s743 = v_xss;
    int d744 = 0;
    if ((!d744) && u_is_con(s743, &n_list_46_Nil)) {
      d744 = 1;
      {
        u_value t745 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t745;
      }
    }
    if ((!d744) && u_is_con(s743, &n_list_46_Cons)) {
      u_value v_xs = u_field(s743, 0);
      u_value v_more = u_field(s743, 1);
      d744 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t746 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t746;
      }
    }
    if (!d744) {
      u_no_case(s743);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s747 = v_cs;
    int d748 = 0;
    if ((!d748) && u_is_con(s747, &n_list_46_Nil)) {
      d748 = 1;
      {
      }
    }
    if ((!d748) && (u_is_con(s747, &n_list_46_Cons) && u_is_con(u_field(u_field(s747, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s747, 0), 0);
      u_value v_n = u_field(u_field(u_field(s747, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s747, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s747, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s747, 0), 1), 3);
      u_value v_rest = u_field(s747, 1);
      d748 = 1;
      {
        u_value t749 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t749;
        p_check_46_overload(f_check_46_ctx_of_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t750 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_ctx_of_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t750;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d748) && u_is_con(s747, &n_list_46_Cons)) {
      u_value v_rest = u_field(s747, 1);
      d748 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d748) {
      u_no_case(s747);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s751 = v_cs;
    int d752 = 0;
    if ((!d752) && u_is_con(s751, &n_list_46_Nil)) {
      d752 = 1;
      {
      }
    }
    if ((!d752) && (u_is_con(s751, &n_list_46_Cons) && u_is_con(u_field(u_field(s751, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s751, 0), 0);
      u_value v_n = u_field(u_field(u_field(s751, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s751, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s751, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s751, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s751, 0), 1), 4);
      u_value v_rest = u_field(s751, 1);
      d752 = 1;
      {
        u_value t753 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t753;
        p_check_46_declare_name(f_check_46_ctx_of_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t754 = f_syntax_46_types_1(v_xs);
        v_ts = t754;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t755 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_ctx_of_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t755;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d752) && u_is_con(s751, &n_list_46_Cons)) {
      u_value v_rest = u_field(s751, 1);
      d752 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d752) {
      u_no_case(s751);
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
    u_value s756 = v_c;
    int d757 = 0;
    if ((!d757) && u_is_con(s756, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s756, 0);
      u_value v_ps = u_field(s756, 1);
      u_value v_bound = u_field(s756, 2);
      u_value v_res = u_field(s756, 3);
      u_value v_body = u_field(s756, 4);
      d757 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t758 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t758;
      }
    }
    if ((!d757) && u_is_con(s756, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s756, 0);
      u_value v_ps = u_field(s756, 1);
      u_value v_ins = u_field(s756, 2);
      u_value v_outs = u_field(s756, 3);
      u_value v_refs = u_field(s756, 4);
      u_value v_body = u_field(s756, 5);
      d757 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t759 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t759;
      }
    }
    if ((!d757) && u_is_con(s756, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s756, 0);
      d757 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t760 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t760;
      }
    }
    if ((!d757) && u_is_con(s756, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s756, 0);
      u_value v_body = u_field(s756, 1);
      d757 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t761 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t761;
      }
    }
    if ((!d757) && u_is_con(s756, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s756, 0);
      u_value v_body = u_field(s756, 1);
      d757 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t762 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t762;
      }
    }
    if (!d757) {
      d757 = 1;
      {
        u_value t763 = v_c;
        *r_c1 = t763;
      }
    }
    if (!d757) {
      u_no_case(s756);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s764 = v_cs;
    int d765 = 0;
    if ((!d765) && u_is_con(s764, &n_list_46_Nil)) {
      d765 = 1;
      {
        u_value t766 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t766;
      }
    }
    if ((!d765) && u_is_con(s764, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s764, 0), 0);
      u_value v_c = u_field(u_field(s764, 0), 1);
      u_value v_more = u_field(s764, 1);
      d765 = 1;
      {
        u_value t767 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t767;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t768 = f_check_46_zonk_cmd_2(f_check_46_sub_of_1(*r_s), v_c1);
        v_c2 = t768;
        u_value s769 = f_check_46_unknown_in_cmd_1(v_c2);
        int d770 = 0;
        if ((!d770) && u_is_con(s769, &n_option_46_Some)) {
          u_value v_t = u_field(s769, 0);
          d770 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d770) && u_is_con(s769, &n_option_46_None)) {
          d770 = 1;
          {
          }
        }
        if (!d770) {
          u_no_case(s769);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t771 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t771;
      }
    }
    if (!d765) {
      u_no_case(s764);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s772 = v_cs;
  u_value t773 = NULL;
  if ((!t773) && u_is_con(s772, &n_list_46_Nil)) {
    t773 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t773) && (u_is_con(s772, &n_list_46_Cons) && u_is_con(u_field(u_field(s772, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s772, 0), 1), 0);
    u_value v_more = u_field(s772, 1);
    t773 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t773) && (u_is_con(s772, &n_list_46_Cons) && u_is_con(u_field(u_field(s772, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s772, 0), 1), 0);
    u_value v_more = u_field(s772, 1);
    t773 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t773) && u_is_con(s772, &n_list_46_Cons)) {
    u_value v_more = u_field(s772, 1);
    t773 = f_check_46_block_names_1(v_more);
  }
  if (!t773) {
    u_no_case(s772);
  }
  return t773;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s774 = v_p;
    int d775 = 0;
    if ((!d775) && u_is_con(s774, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s774, 0);
      d775 = 1;
      {
        u_value t776 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t776;
        u_value t777 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t777;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t778 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t778;
      }
    }
    if (!d775) {
      u_no_case(s774);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_779 = U_LIT("main");
    u_value t780 = f_check_46_start_2(&l_main_779, u_mk_con(&n_list_46_Nil, 0));
    v_s = t780;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t781 = f_check_46_zonk_type_2(f_check_46_sub_of_1(v_s), v_u);
    *r_t = t781;
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
  static struct u_object l_List_782 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_782), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_783 = U_LIT("list");
  static struct u_object l_List_784 = U_LIT("List");
  static struct u_object l_A_785 = U_LIT("A");
  static struct u_object l_Nil_786 = U_LIT("Nil");
  static struct u_object l_Cons_787 = U_LIT("Cons");
  static struct u_object l_A_788 = U_LIT("A");
  static struct u_object l_A_789 = U_LIT("A");
  static struct u_object l_list_790 = U_LIT("list");
  static struct u_object l_len_791 = U_LIT("len");
  static struct u_object l_A_792 = U_LIT("A");
  static struct u_object l_xs_793 = U_LIT("xs");
  static struct u_object l_A_794 = U_LIT("A");
  static struct u_object l_xs_795 = U_LIT("xs");
  static struct u_object l_Nil_796 = U_LIT("Nil");
  static struct u_object l_Cons_797 = U_LIT("Cons");
  static struct u_object l_ys_798 = U_LIT("ys");
  static struct u_object l__43__799 = U_LIT("+");
  static struct u_object l_len_800 = U_LIT("len");
  static struct u_object l_ys_801 = U_LIT("ys");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_783, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_784), u_mk_con(&n_list_46_Cons, 2, &l_A_785, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_786), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_787), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_788), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_789)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_790), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_791), u_mk_con(&n_list_46_Cons, 2, &l_A_792, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_793), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_794))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_795), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_796), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_797), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_798), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__799, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_800, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_801), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s802 = v_t;
  u_value t803 = NULL;
  if ((!t803) && u_is_con(s802, &n_c_46_CtVoid)) {
    static struct u_object l_void_804 = U_LIT("void");
    t803 = &l_void_804;
  }
  if ((!t803) && u_is_con(s802, &n_c_46_CtInt)) {
    static struct u_object l_int_805 = U_LIT("int");
    t803 = &l_int_805;
  }
  if ((!t803) && u_is_con(s802, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s802, 0);
    t803 = v_n;
  }
  if ((!t803) && (u_is_con(s802, &n_c_46_CtPtr) && u_is_con(u_field(s802, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s802, 0), 0);
    static struct u_object l__42__806 = U_LIT("*");
    t803 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__806);
  }
  if ((!t803) && u_is_con(s802, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s802, 0);
    static struct u_object l__32__42__807 = U_LIT(" *");
    t803 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__807);
  }
  if (!t803) {
    u_no_case(s802);
  }
  return t803;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s808 = v_t;
  u_value t809 = NULL;
  if ((!t809) && u_is_con(s808, &n_c_46_CtPtr)) {
    t809 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t809) {
    static struct u_object l__32__810 = U_LIT(" ");
    t809 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__810, v_x));
  }
  if (!t809) {
    u_no_case(s808);
  }
  return t809;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s811 = v_e;
  u_value t812 = NULL;
  if ((!t812) && u_is_con(s811, &n_c_46_CeVar)) {
    u_value v_x = u_field(s811, 0);
    t812 = v_x;
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeNum)) {
    u_value v_n = u_field(s811, 0);
    t812 = u_str_show(v_n);
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeStr)) {
    u_value v_s = u_field(s811, 0);
    t812 = u_str_quote(v_s);
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeCall)) {
    u_value v_f = u_field(s811, 0);
    u_value v_args = u_field(s811, 1);
    static struct u_object l__40__813 = U_LIT("(");
    static struct u_object l__44__32__814 = U_LIT(", ");
    u_value t815 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e816 = &t815;
    u_value l817 = v_args;
    while (u_is_con(l817, &n_list_46_Cons)) {
      u_value v_a = u_field(l817, 0);
      if (u_truth(u_mk_bool(1))) {
        e816 = u_append(e816, f_c_46_c_expr_1(v_a));
      }
      l817 = u_field(l817, 1);
    }
    static struct u_object l__41__818 = U_LIT(")");
    t812 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__813, u_concat(u_str_join(&l__44__32__814, t815), &l__41__818)));
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s811, 0);
    u_value v_a = u_field(s811, 1);
    t812 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s811, 0);
    u_value v_a = u_field(s811, 1);
    u_value v_b = u_field(s811, 2);
    static struct u_object l__32__819 = U_LIT(" ");
    static struct u_object l__32__820 = U_LIT(" ");
    t812 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__819, u_concat(v_o, u_concat(&l__32__820, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s811, 0);
    u_value v_i = u_field(s811, 1);
    static struct u_object l__91__821 = U_LIT("[");
    static struct u_object l__93__822 = U_LIT("]");
    t812 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__821, u_concat(f_c_46_c_expr_1(v_i), &l__93__822)));
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s811, 0);
    u_value v_f = u_field(s811, 1);
    static struct u_object l__45__62__823 = U_LIT("->");
    t812 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__823, v_f));
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeCast)) {
    u_value v_t = u_field(s811, 0);
    u_value v_a = u_field(s811, 1);
    static struct u_object l__40__824 = U_LIT("(");
    static struct u_object l__41__825 = U_LIT(")");
    t812 = u_concat(&l__40__824, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__825, f_c_46_c_operand_1(v_a))));
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeCond)) {
    u_value v_c = u_field(s811, 0);
    u_value v_a = u_field(s811, 1);
    u_value v_b = u_field(s811, 2);
    static struct u_object l__32__63__32__826 = U_LIT(" ? ");
    static struct u_object l__32__58__32__827 = U_LIT(" : ");
    t812 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__826, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__827, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CeInit)) {
    u_value v_es = u_field(s811, 0);
    static struct u_object l__123__828 = U_LIT("{");
    static struct u_object l__44__32__829 = U_LIT(", ");
    u_value t830 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e831 = &t830;
    u_value l832 = v_es;
    while (u_is_con(l832, &n_list_46_Cons)) {
      u_value v_a = u_field(l832, 0);
      if (u_truth(u_mk_bool(1))) {
        e831 = u_append(e831, f_c_46_c_expr_1(v_a));
      }
      l832 = u_field(l832, 1);
    }
    static struct u_object l__125__833 = U_LIT("}");
    t812 = u_concat(&l__123__828, u_concat(u_str_join(&l__44__32__829, t830), &l__125__833));
  }
  if (!t812) {
    u_no_case(s811);
  }
  return t812;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s834 = v_e;
  u_value t835 = NULL;
  if ((!t835) && u_is_con(s834, &n_c_46_CeVar)) {
    t835 = f_c_46_c_expr_1(v_e);
  }
  if ((!t835) && u_is_con(s834, &n_c_46_CeNum)) {
    u_value v_n = u_field(s834, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t835 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t835) && u_is_con(s834, &n_c_46_CeStr)) {
    t835 = f_c_46_c_expr_1(v_e);
  }
  if ((!t835) && u_is_con(s834, &n_c_46_CeCall)) {
    t835 = f_c_46_c_expr_1(v_e);
  }
  if ((!t835) && u_is_con(s834, &n_c_46_CeIndex)) {
    t835 = f_c_46_c_expr_1(v_e);
  }
  if ((!t835) && u_is_con(s834, &n_c_46_CeArrow)) {
    t835 = f_c_46_c_expr_1(v_e);
  }
  if (!t835) {
    static struct u_object l__40__836 = U_LIT("(");
    static struct u_object l__41__837 = U_LIT(")");
    t835 = u_concat(&l__40__836, u_concat(f_c_46_c_expr_1(v_e), &l__41__837));
  }
  if (!t835) {
    u_no_case(s834);
  }
  return t835;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s838 = v_s;
  u_value t839 = NULL;
  if ((!t839) && u_is_con(s838, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s838, 0);
    static struct u_object l__59__840 = U_LIT(";");
    t839 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__840)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t839) && (u_is_con(s838, &n_c_46_CsDecl) && u_is_con(u_field(s838, 2), &n_option_46_None))) {
    u_value v_t = u_field(s838, 0);
    u_value v_x = u_field(s838, 1);
    static struct u_object l__59__841 = U_LIT(";");
    t839 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__841)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t839) && (u_is_con(s838, &n_c_46_CsDecl) && u_is_con(u_field(s838, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s838, 0);
    u_value v_x = u_field(s838, 1);
    u_value v_e = u_field(u_field(s838, 2), 0);
    static struct u_object l__32__61__32__842 = U_LIT(" = ");
    static struct u_object l__59__843 = U_LIT(";");
    t839 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__842, u_concat(f_c_46_c_expr_1(v_e), &l__59__843)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t839) && u_is_con(s838, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s838, 0);
    u_value v_e = u_field(s838, 1);
    static struct u_object l__32__61__32__844 = U_LIT(" = ");
    static struct u_object l__59__845 = U_LIT(";");
    t839 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__844, u_concat(f_c_46_c_expr_1(v_e), &l__59__845)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t839) && (u_is_con(s838, &n_c_46_CsIf) && u_is_con(u_field(s838, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s838, 0);
    u_value v_l = u_field(s838, 1);
    static struct u_object l_if_32__40__846 = U_LIT("if (");
    static struct u_object l__41__32__123__847 = U_LIT(") {");
    static struct u_object l__32__32__848 = U_LIT("  ");
    static struct u_object l__125__849 = U_LIT("}");
    t839 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__846, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__847))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__848), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__849), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t839) && u_is_con(s838, &n_c_46_CsIf)) {
    u_value v_c = u_field(s838, 0);
    u_value v_l = u_field(s838, 1);
    u_value v_r = u_field(s838, 2);
    static struct u_object l_if_32__40__850 = U_LIT("if (");
    static struct u_object l__41__32__123__851 = U_LIT(") {");
    static struct u_object l__32__32__852 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__853 = U_LIT("} else {");
    static struct u_object l__32__32__854 = U_LIT("  ");
    static struct u_object l__125__855 = U_LIT("}");
    t839 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__850, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__851))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__852), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__853), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__854), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__855), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t839) && u_is_con(s838, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s838, 0);
    u_value v_b = u_field(s838, 1);
    static struct u_object l_while_32__40__856 = U_LIT("while (");
    static struct u_object l__41__32__123__857 = U_LIT(") {");
    static struct u_object l__32__32__858 = U_LIT("  ");
    static struct u_object l__125__859 = U_LIT("}");
    t839 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__856, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__857))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__858), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__859), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t839) && (u_is_con(s838, &n_c_46_CsReturn) && u_is_con(u_field(s838, 0), &n_option_46_None))) {
    static struct u_object l_return_59__860 = U_LIT("return;");
    t839 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__860), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t839) && (u_is_con(s838, &n_c_46_CsReturn) && u_is_con(u_field(s838, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s838, 0), 0);
    static struct u_object l_return_32__861 = U_LIT("return ");
    static struct u_object l__59__862 = U_LIT(";");
    t839 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__861, u_concat(f_c_46_c_expr_1(v_e), &l__59__862))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t839) && u_is_con(s838, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s838, 0);
    static struct u_object l__123__863 = U_LIT("{");
    static struct u_object l__32__32__864 = U_LIT("  ");
    static struct u_object l__125__865 = U_LIT("}");
    t839 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__863), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__864), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__865), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t839) {
    u_no_case(s838);
  }
  return t839;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t866 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e867 = &t866;
  u_value l868 = v_ss;
  while (u_is_con(l868, &n_list_46_Cons)) {
    u_value v_s = u_field(l868, 0);
    if (u_truth(u_mk_bool(1))) {
      e867 = u_append(e867, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l868 = u_field(l868, 1);
  }
  return f_list_46_concat_1(t866);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s869 = v_ps;
  u_value t870 = NULL;
  if ((!t870) && u_is_con(s869, &n_list_46_Nil)) {
    static struct u_object l_void_871 = U_LIT("void");
    t870 = &l_void_871;
  }
  if (!t870) {
    static struct u_object l__44__32__872 = U_LIT(", ");
    u_value t873 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e874 = &t873;
    u_value l875 = v_ps;
    while (u_is_con(l875, &n_list_46_Cons)) {
      u_value v_p = u_field(l875, 0);
      if (u_truth(u_mk_bool(1))) {
        e874 = u_append(e874, f_c_46_c_param_1(v_p));
      }
      l875 = u_field(l875, 1);
    }
    t870 = u_str_join(&l__44__32__872, t873);
  }
  if (!t870) {
    u_no_case(s869);
  }
  return t870;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s876 = v_p;
  u_value t877 = NULL;
  if (!t877) {
    u_value v_t = u_field(s876, 0);
    u_value v_x = u_field(s876, 1);
    t877 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t877) {
    u_no_case(s876);
  }
  return t877;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s878 = v_d;
  u_value t879 = NULL;
  if ((!t879) && u_is_con(s878, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s878, 0);
    static struct u_object l__35_include_32__880 = U_LIT("#include ");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__880, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && u_is_con(s878, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s878, 0);
    u_value v_f = u_field(s878, 1);
    u_value v_ps = u_field(s878, 2);
    static struct u_object l__40__881 = U_LIT("(");
    static struct u_object l__41__59__882 = U_LIT(");");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__881, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__882))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && u_is_con(s878, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s878, 0);
    u_value v_f = u_field(s878, 1);
    u_value v_ps = u_field(s878, 2);
    u_value v_b = u_field(s878, 3);
    static struct u_object l__40__883 = U_LIT("(");
    static struct u_object l__41__32__123__884 = U_LIT(") {");
    static struct u_object l__32__32__885 = U_LIT("  ");
    static struct u_object l__125__886 = U_LIT("}");
    t879 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__883, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__884))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__885, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__886, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t879) && (u_is_con(s878, &n_c_46_CdGlobal) && u_is_con(u_field(s878, 2), &n_option_46_None))) {
    u_value v_t = u_field(s878, 0);
    u_value v_x = u_field(s878, 1);
    static struct u_object l__59__887 = U_LIT(";");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__887), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && (u_is_con(s878, &n_c_46_CdGlobal) && u_is_con(u_field(s878, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s878, 0);
    u_value v_x = u_field(s878, 1);
    u_value v_e = u_field(u_field(s878, 2), 0);
    static struct u_object l__32__61__32__888 = U_LIT(" = ");
    static struct u_object l__59__889 = U_LIT(";");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__888, u_concat(f_c_46_c_expr_1(v_e), &l__59__889))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t879) {
    u_no_case(s878);
  }
  return t879;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s890 = v_f;
  u_value t891 = NULL;
  if ((!t891) && u_is_con(s890, &n_c_46_CFile)) {
    u_value v_ds = u_field(s890, 0);
    static struct u_object l__10__10__892 = U_LIT("\n\n");
    u_value t893 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e894 = &t893;
    u_value l895 = v_ds;
    while (u_is_con(l895, &n_list_46_Cons)) {
      u_value v_d = u_field(l895, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__896 = U_LIT("\n");
        e894 = u_append(e894, u_str_join(&l__10__896, f_c_46_c_decl_1(v_d)));
      }
      l895 = u_field(l895, 1);
    }
    static struct u_object l__10__897 = U_LIT("\n");
    t891 = u_concat(u_str_join(&l__10__10__892, t893), &l__10__897);
  }
  if (!t891) {
    u_no_case(s890);
  }
  return t891;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__898 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__898, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__899 = U_LIT("()[],;");
  return u_str_contains(&l__40__41__91__93__44__59__899, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t900 = u_is_letter(v_c);
  if (!u_truth(t900)) {
    t900 = u_is_digit(v_c);
  }
  u_value t901 = t900;
  if (!u_truth(t901)) {
    t901 = u_eq(v_c, u_mk_char(95));
  }
  return t901;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t902;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t902 = u_str_get(v_s, v_i);
  } else {
    t902 = u_chr(u_mk_int(0));
  }
  return t902;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s903 = v_c;
  u_value t904 = NULL;
  if ((!t904) && u_equals(s903, u_mk_char(34))) {
    t904 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t904) && u_equals(s903, u_mk_char(39))) {
    t904 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t904) && u_equals(s903, u_mk_char(92))) {
    t904 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t904) && u_equals(s903, u_mk_char(110))) {
    t904 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t904) && u_equals(s903, u_mk_char(116))) {
    t904 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t904) && u_equals(s903, u_mk_char(114))) {
    t904 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t904) {
    t904 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t904) {
    u_no_case(s903);
  }
  return t904;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t905;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t905 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t905 = v_i;
  }
  return t905;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t906 = u_mk_int(0);
    u_value t907 = u_mk_int(1);
    u_value t908 = u_mk_int(1);
    v_j = t906;
    *r_line = t907;
    *r_column = t908;
    u_value t913 = u_lt(v_j, v_i);
    while (u_truth(t913)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t909 = u_add(*r_line, u_mk_int(1));
            u_value t910 = u_mk_int(1);
            *r_line = t909;
            *r_column = t910;
          }
        } else {
          {
            u_value t911 = u_add(*r_column, u_mk_int(1));
            *r_column = t911;
          }
        }
        u_value t912 = u_add(v_j, u_mk_int(1));
        v_j = t912;
      }
      t913 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t914 = *r_i;
    u_value t915 = u_add(*r_i, u_mk_int(1));
    u_value t916 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t914;
    *r_i = t915;
    v_parts = t916;
    u_value t917 = *r_i;
    v_from = t917;
    u_value t928 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t928)) {
      {
        u_value t918 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t918;
        u_value t919 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t919)) {
          t919 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t919)) {
          {
            static struct u_object l_unterminated_32_lit_920 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_920, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s921 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d922 = 0;
            if ((!d922) && u_is_con(s921, &n_option_46_Some)) {
              u_value v_d = u_field(s921, 0);
              d922 = 1;
              {
                u_value t923 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t924 = u_add(*r_i, u_mk_int(2));
                v_parts = t923;
                *r_i = t924;
                u_value t925 = *r_i;
                v_from = t925;
              }
            }
            if ((!d922) && u_is_con(s921, &n_option_46_None)) {
              d922 = 1;
              {
                static struct u_object l_invalid_32_escape_926 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_926, *r_i), "lex.LexError");
              }
            }
            if (!d922) {
              u_no_case(s921);
            }
          }
        } else {
          {
            u_value t927 = u_add(*r_i, u_mk_int(1));
            *r_i = t927;
          }
        }
      }
      t928 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__929 = U_LIT("");
    u_value t930 = u_str_join(&l__929, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t931 = u_add(*r_i, u_mk_int(1));
    *r_text = t930;
    *r_i = t931;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t932 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t932;
    u_value t933 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t934 = v_j;
    *r_t = t933;
    *r_i = t934;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t935 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t935;
    u_value s936 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d937 = 0;
    if ((!d937) && u_is_con(s936, &n_option_46_Some)) {
      u_value v_n = u_field(s936, 0);
      d937 = 1;
      {
        u_value t938 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t939 = v_j;
        *r_t = t938;
        *r_i = t939;
      }
    }
    if ((!d937) && u_is_con(s936, &n_option_46_None)) {
      d937 = 1;
      {
        static struct u_object l_invalid_32_number_940 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_940, *r_i), "lex.LexError");
      }
    }
    if (!d937) {
      u_no_case(s936);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t941 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t941;
    u_value t942 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t943 = v_j;
    *r_t = t942;
    *r_i = t943;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t944 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t944;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t945 = *r_i;
    v_start = t945;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_946 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_946, v_start), "lex.LexError");
      }
    }
    u_value t947 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t947;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t948 = u_str_get(v_s, *r_i);
    v_c = t948;
    u_value s949 = v_c;
    int d950 = 0;
    if (!d950) {
      u_value t951 = u_is_letter(v_c);
      if (!u_truth(t951)) {
        t951 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t951)) {
        d950 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d950) {
      if (u_truth(u_is_digit(v_c))) {
        d950 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d950) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d950 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d950) && u_equals(s949, u_mk_char(34))) {
      d950 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d950) && u_equals(s949, u_mk_char(39))) {
      d950 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d950) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d950 = 1;
        {
          u_value t952 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t953 = u_add(*r_i, u_mk_int(1));
          *r_t = t952;
          *r_i = t953;
        }
      }
    }
    if (!d950) {
      d950 = 1;
      {
        static struct u_object l_unexpected_32_chara_954 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_954, *r_i), "lex.LexError");
      }
    }
    if (!d950) {
      u_no_case(s949);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t955 = u_mk_int(0);
    v_i = t955;
    u_value t956 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t956;
    u_value t963 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t963)) {
      {
        u_value t957 = v_i;
        v_start = t957;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t958 = u_add(v_i, u_mk_int(1));
            v_i = t958;
          }
        } else {
          u_value t959 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t959)) {
            t959 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t959)) {
            {
              u_value t961 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam960, 0));
              v_i = t961;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t962 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t962;
            }
          }
        }
      }
      t963 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t964 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t964;
  }
}

u_value f_lex_46_tokens_1(u_value v_ts) {
  return f_list_46_firsts_1(v_ts);
}

u_value f_parse_46_scope_params_1(u_value v_sc) {
  u_value s965 = v_sc;
  u_value t966 = NULL;
  if ((!t966) && u_is_con(s965, &n_parse_46_Scope)) {
    u_value v_ps = u_field(s965, 0);
    t966 = v_ps;
  }
  if (!t966) {
    u_no_case(s965);
  }
  return t966;
}

u_value f_parse_46_scope_notations_1(u_value v_sc) {
  u_value s967 = v_sc;
  u_value t968 = NULL;
  if ((!t968) && u_is_con(s967, &n_parse_46_Scope)) {
    u_value v_ns = u_field(s967, 1);
    t968 = v_ns;
  }
  if (!t968) {
    u_no_case(s967);
  }
  return t968;
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
  static struct u_object l___969 = U_LIT("_");
  static struct u_object l_assert_970 = U_LIT("assert");
  static struct u_object l_assign_971 = U_LIT("assign");
  static struct u_object l_call_972 = U_LIT("call");
  static struct u_object l_case_973 = U_LIT("case");
  static struct u_object l_catch_974 = U_LIT("catch");
  static struct u_object l_data_975 = U_LIT("data");
  static struct u_object l_else_976 = U_LIT("else");
  static struct u_object l_end_977 = U_LIT("end");
  static struct u_object l_eval_978 = U_LIT("eval");
  static struct u_object l_false_979 = U_LIT("false");
  static struct u_object l_for_980 = U_LIT("for");
  static struct u_object l_function_981 = U_LIT("function");
  static struct u_object l_if_982 = U_LIT("if");
  static struct u_object l_import_983 = U_LIT("import");
  static struct u_object l_in_984 = U_LIT("in");
  static struct u_object l_infix_985 = U_LIT("infix");
  static struct u_object l_lambda_986 = U_LIT("lambda");
  static struct u_object l_let_987 = U_LIT("let");
  static struct u_object l_local_988 = U_LIT("local");
  static struct u_object l_match_989 = U_LIT("match");
  static struct u_object l_out_990 = U_LIT("out");
  static struct u_object l_postfix_991 = U_LIT("postfix");
  static struct u_object l_prefix_992 = U_LIT("prefix");
  static struct u_object l_procedure_993 = U_LIT("procedure");
  static struct u_object l_raise_994 = U_LIT("raise");
  static struct u_object l_ref_995 = U_LIT("ref");
  static struct u_object l_run_996 = U_LIT("run");
  static struct u_object l_skip_997 = U_LIT("skip");
  static struct u_object l_test_998 = U_LIT("test");
  static struct u_object l_then_999 = U_LIT("then");
  static struct u_object l_true_1000 = U_LIT("true");
  static struct u_object l_type_1001 = U_LIT("type");
  static struct u_object l_try_1002 = U_LIT("try");
  static struct u_object l_while_1003 = U_LIT("while");
  cache = u_mk_con(&n_list_46_Cons, 2, &l___969, u_mk_con(&n_list_46_Cons, 2, &l_assert_970, u_mk_con(&n_list_46_Cons, 2, &l_assign_971, u_mk_con(&n_list_46_Cons, 2, &l_call_972, u_mk_con(&n_list_46_Cons, 2, &l_case_973, u_mk_con(&n_list_46_Cons, 2, &l_catch_974, u_mk_con(&n_list_46_Cons, 2, &l_data_975, u_mk_con(&n_list_46_Cons, 2, &l_else_976, u_mk_con(&n_list_46_Cons, 2, &l_end_977, u_mk_con(&n_list_46_Cons, 2, &l_eval_978, u_mk_con(&n_list_46_Cons, 2, &l_false_979, u_mk_con(&n_list_46_Cons, 2, &l_for_980, u_mk_con(&n_list_46_Cons, 2, &l_function_981, u_mk_con(&n_list_46_Cons, 2, &l_if_982, u_mk_con(&n_list_46_Cons, 2, &l_import_983, u_mk_con(&n_list_46_Cons, 2, &l_in_984, u_mk_con(&n_list_46_Cons, 2, &l_infix_985, u_mk_con(&n_list_46_Cons, 2, &l_lambda_986, u_mk_con(&n_list_46_Cons, 2, &l_let_987, u_mk_con(&n_list_46_Cons, 2, &l_local_988, u_mk_con(&n_list_46_Cons, 2, &l_match_989, u_mk_con(&n_list_46_Cons, 2, &l_out_990, u_mk_con(&n_list_46_Cons, 2, &l_postfix_991, u_mk_con(&n_list_46_Cons, 2, &l_prefix_992, u_mk_con(&n_list_46_Cons, 2, &l_procedure_993, u_mk_con(&n_list_46_Cons, 2, &l_raise_994, u_mk_con(&n_list_46_Cons, 2, &l_ref_995, u_mk_con(&n_list_46_Cons, 2, &l_run_996, u_mk_con(&n_list_46_Cons, 2, &l_skip_997, u_mk_con(&n_list_46_Cons, 2, &l_test_998, u_mk_con(&n_list_46_Cons, 2, &l_then_999, u_mk_con(&n_list_46_Cons, 2, &l_true_1000, u_mk_con(&n_list_46_Cons, 2, &l_type_1001, u_mk_con(&n_list_46_Cons, 2, &l_try_1002, u_mk_con(&n_list_46_Cons, 2, &l_while_1003, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1004 = U_LIT("=");
  static struct u_object l__61__62__1005 = U_LIT("=>");
  static struct u_object l__58__61__1006 = U_LIT(":=");
  static struct u_object l__58__1007 = U_LIT(":");
  static struct u_object l__124__1008 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1004, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1005, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1006, u_mk_con(&n_list_46_Cons, 2, &l__58__1007, u_mk_con(&n_list_46_Cons, 2, &l__124__1008, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1009 = v_ns;
  u_value t1010 = NULL;
  if ((!t1010) && u_is_con(s1009, &n_list_46_Nil)) {
    t1010 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1010) && (u_is_con(s1009, &n_list_46_Cons) && (u_is_con(u_field(s1009, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1009, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1009, 0), 1);
    u_value v_ops = u_field(u_field(s1009, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1010 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1010) && u_is_con(s1009, &n_list_46_Cons)) {
    u_value v_more = u_field(s1009, 1);
    t1010 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1010) {
    u_no_case(s1009);
  }
  return t1010;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1011 = v_ns;
  u_value t1012 = NULL;
  if ((!t1012) && u_is_con(s1011, &n_list_46_Nil)) {
    t1012 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1012) && (u_is_con(s1011, &n_list_46_Cons) && (u_is_con(u_field(s1011, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1011, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1011, 0), 1);
    u_value v_ops = u_field(u_field(s1011, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1012 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1012) && u_is_con(s1011, &n_list_46_Cons)) {
    u_value v_more = u_field(s1011, 1);
    t1012 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1012) {
    u_no_case(s1011);
  }
  return t1012;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1013 = v_ns;
  u_value t1014 = NULL;
  if ((!t1014) && u_is_con(s1013, &n_list_46_Nil)) {
    t1014 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1014) && (u_is_con(s1013, &n_list_46_Cons) && u_is_con(u_field(s1013, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1013, 0), 0);
    u_value v_p = u_field(u_field(s1013, 0), 1);
    u_value v_ops = u_field(u_field(s1013, 0), 2);
    u_value t1015 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1015)) {
      t1015 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1016 = t1015;
    if (u_truth(t1016)) {
      t1016 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1016)) {
      t1014 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1014) && u_is_con(s1013, &n_list_46_Cons)) {
    u_value v_more = u_field(s1013, 1);
    t1014 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1014) {
    u_no_case(s1013);
  }
  return t1014;
}

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op) {
  u_value s1017 = v_ns;
  u_value t1018 = NULL;
  if ((!t1018) && u_is_con(s1017, &n_list_46_Nil)) {
    t1018 = u_mk_bool(0);
  }
  if ((!t1018) && (u_is_con(s1017, &n_list_46_Cons) && u_is_con(u_field(s1017, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1017, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1018 = u_mk_bool(1);
    }
  }
  if ((!t1018) && u_is_con(s1017, &n_list_46_Cons)) {
    u_value v_more = u_field(s1017, 1);
    t1018 = f_parse_46_declared_2(v_more, v_op);
  }
  if (!t1018) {
    u_no_case(s1017);
  }
  return t1018;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1019 = v_ts;
  u_value t1020 = NULL;
  if ((!t1020) && u_is_con(s1019, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1019, 0), 0);
    t1020 = v_t;
  }
  if (!t1020) {
    t1020 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1020) {
    u_no_case(s1019);
  }
  return t1020;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1021 = v_ts;
  u_value t1022 = NULL;
  if ((!t1022) && (u_is_con(s1021, &n_list_46_Cons) && u_is_con(u_field(s1021, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1021, 1), 0), 0);
    t1022 = v_t;
  }
  if (!t1022) {
    t1022 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1022) {
    u_no_case(s1021);
  }
  return t1022;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1023 = v_ts;
  u_value t1024 = NULL;
  if ((!t1024) && u_is_con(s1023, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1023, 0), 1);
    t1024 = v_i;
  }
  if (!t1024) {
    t1024 = u_mk_int(0);
  }
  if (!t1024) {
    u_no_case(s1023);
  }
  return t1024;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1025 = v_t;
  u_value t1026 = NULL;
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TName)) {
    u_value v_n = u_field(s1025, 0);
    t1026 = v_n;
  }
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1025, 0);
    t1026 = v_s;
  }
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1025, 0);
    t1026 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1025, 0);
    t1026 = u_str_show(v_n);
  }
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1025, 0);
    t1026 = u_str_quote(v_s);
  }
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1025, 0);
    static struct u_object l__39__1027 = U_LIT("'");
    static struct u_object l__39__1028 = U_LIT("'");
    t1026 = u_concat(&l__39__1027, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1028));
  }
  if ((!t1026) && u_is_con(s1025, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1029 = U_LIT("end of input");
    t1026 = &l_end_32_of_32_input_1029;
  }
  if (!t1026) {
    u_no_case(s1025);
  }
  return t1026;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1030 = f_parse_46_head_token_1(v_ts);
  u_value t1031 = NULL;
  if ((!t1031) && u_is_con(s1030, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1030, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1031 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1031) && u_is_con(s1030, &n_lex_46_TName)) {
    u_value v_n = u_field(s1030, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1031 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1031) {
    t1031 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1031) {
    u_no_case(s1030);
  }
  return t1031;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1032 = *r_ts;
    int d1033 = 0;
    if ((!d1033) && u_is_con(s1032, &n_list_46_Cons)) {
      u_value v_more = u_field(s1032, 1);
      d1033 = 1;
      {
        u_value t1034 = v_more;
        *r_ts = t1034;
      }
    }
    if (!d1033) {
      d1033 = 1;
      {
      }
    }
    if (!d1033) {
      u_no_case(s1032);
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
        static struct u_object l__32_expected_32_but_32__1035 = U_LIT(" expected but ");
        static struct u_object l__32_found_1036 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1035, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1036))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1037 = f_parse_46_head_token_1(*r_ts);
    int d1038 = 0;
    if ((!d1038) && u_is_con(s1037, &n_lex_46_TName)) {
      u_value v_m = u_field(s1037, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1038 = 1;
        {
          u_value t1039 = v_m;
          *r_n = t1039;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1038) {
      d1038 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1040 = U_LIT("name expected but ");
        static struct u_object l__32_found_1041 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1040, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1041)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1038) {
      u_no_case(s1037);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1042 = f_parse_46_head_token_1(*r_ts);
    int d1043 = 0;
    if ((!d1043) && u_is_con(s1042, &n_lex_46_TName)) {
      u_value v_m = u_field(s1042, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_scope_notations_1(v_sc), v_m))) {
        d1043 = 1;
        {
          static struct u_object l_operator_32__1044 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1045 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1044, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1045)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1043) {
      d1043 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1043) {
      u_no_case(s1042);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1046 = f_parse_46_head_token_1(*r_ts);
    int d1047 = 0;
    if ((!d1047) && u_is_con(s1046, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1046, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1047 = 1;
        {
          u_value t1048 = v_s;
          *r_n = t1048;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1047) {
      d1047 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1047) {
      u_no_case(s1046);
    }
  }
}

u_value f_parse_46_ops_notations_1(u_value v_o) {
  u_value s1049 = v_o;
  u_value t1050 = NULL;
  if ((!t1050) && u_is_con(s1049, &n_parse_46_Ops)) {
    u_value v_ns = u_field(s1049, 0);
    t1050 = v_ns;
  }
  if (!t1050) {
    u_no_case(s1049);
  }
  return t1050;
}

u_value f_parse_46_apply_prefix_3(u_value v_o, u_value v_op, u_value v_a) {
  u_value s1051 = v_o;
  u_value t1052 = NULL;
  if ((!t1052) && u_is_con(s1051, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1051, 1);
    t1052 = u_apply(v_f, 2, v_op, v_a);
  }
  if (!t1052) {
    u_no_case(s1051);
  }
  return t1052;
}

u_value f_parse_46_apply_postfix_3(u_value v_o, u_value v_op, u_value v_a) {
  u_value s1053 = v_o;
  u_value t1054 = NULL;
  if ((!t1054) && u_is_con(s1053, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1053, 2);
    t1054 = u_apply(v_f, 2, v_op, v_a);
  }
  if (!t1054) {
    u_no_case(s1053);
  }
  return t1054;
}

u_value f_parse_46_apply_infix_4(u_value v_o, u_value v_op, u_value v_a, u_value v_b) {
  u_value s1055 = v_o;
  u_value t1056 = NULL;
  if ((!t1056) && u_is_con(s1055, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1055, 3);
    t1056 = u_apply(v_f, 3, v_op, v_a, v_b);
  }
  if (!t1056) {
    u_no_case(s1055);
  }
  return t1056;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1057 = v_rest;
    int d1058 = 0;
    if ((!d1058) && u_is_con(s1057, &n_list_46_Nil)) {
      d1058 = 1;
      {
      }
    }
    if ((!d1058) && (u_is_con(s1057, &n_list_46_Cons) && u_is_con(u_field(s1057, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1057, 0), 0);
      u_value v_i = u_field(u_field(s1057, 0), 1);
      d1058 = 1;
      {
        static struct u_object l_unexpected_32_opera_1059 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1059, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1058) {
      d1058 = 1;
      {
        static struct u_object l_missing_32_operator_1060 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1060, v_at), "parse.ParseError");
      }
    }
    if (!d1058) {
      u_no_case(s1057);
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
    u_value s1061 = v_items;
    int d1062 = 0;
    if ((!d1062) && (u_is_con(s1061, &n_list_46_Cons) && u_is_con(u_field(s1061, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1061, 0), 0);
      u_value v_more = u_field(s1061, 1);
      d1062 = 1;
      {
        u_value t1063 = v_x;
        u_value t1064 = v_more;
        *r_a = t1063;
        *r_rest = t1064;
      }
    }
    if ((!d1062) && (u_is_con(s1061, &n_list_46_Cons) && u_is_con(u_field(s1061, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1061, 0), 0);
      u_value v_i = u_field(u_field(s1061, 0), 1);
      u_value v_more = u_field(s1061, 1);
      d1062 = 1;
      {
        u_value s1065 = f_parse_46_prefix_prec_2(f_parse_46_ops_notations_1(v_o), v_op);
        int d1066 = 0;
        if ((!d1066) && u_is_con(s1065, &n_option_46_Some)) {
          u_value v_p = u_field(s1065, 0);
          d1066 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1067 = f_parse_46_apply_prefix_3(v_o, v_op, v_b);
            *r_a = t1067;
          }
        }
        if ((!d1066) && u_is_con(s1065, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_ops_notations_1(v_o), v_op))) {
            d1066 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1068 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1068), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1066) && u_is_con(s1065, &n_option_46_None)) {
          d1066 = 1;
          {
            static struct u_object l_undeclared_32_opera_1069 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1069, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1066) {
          u_no_case(s1065);
        }
      }
    }
    if ((!d1062) && u_is_con(s1061, &n_list_46_Nil)) {
      d1062 = 1;
      {
        static struct u_object l_missing_32_operand_1070 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1070, v_at), "parse.ParseError");
      }
    }
    if (!d1062) {
      u_no_case(s1061);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1071 = v_items;
    int d1072 = 0;
    if ((!d1072) && (u_is_con(s1071, &n_list_46_Cons) && u_is_con(u_field(s1071, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1071, 0), 0);
      u_value v_i = u_field(u_field(s1071, 0), 1);
      u_value v_tail = u_field(s1071, 1);
      d1072 = 1;
      {
        u_value s1073 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_ops_notations_1(v_o), v_op), f_parse_46_infix_of_2(f_parse_46_ops_notations_1(v_o), v_op));
        int d1074 = 0;
        if ((!d1074) && u_is_con(u_field(s1073, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1073, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1074 = 1;
            {
              p_parse_46_climb_loop(f_parse_46_apply_postfix_3(v_o, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1074) && u_is_con(u_field(s1073, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1073, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1073, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1074 = 1;
            {
              u_value t1075;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1075 = v_p;
              } else {
                t1075 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1075, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_ops_notations_1(v_o));
              p_parse_46_climb_loop(f_parse_46_apply_infix_4(v_o, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1074) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_ops_notations_1(v_o), v_op)))) {
            d1074 = 1;
            {
              static struct u_object l_undeclared_32_opera_1076 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1076, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1074) {
          d1074 = 1;
          {
            u_value t1077 = v_lhs;
            u_value t1078 = v_items;
            *r_a = t1077;
            *r_rest = t1078;
          }
        }
        if (!d1074) {
          u_no_case(s1073);
        }
      }
    }
    if (!d1072) {
      d1072 = 1;
      {
        u_value t1079 = v_lhs;
        u_value t1080 = v_items;
        *r_a = t1079;
        *r_rest = t1080;
      }
    }
    if (!d1072) {
      u_no_case(s1071);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1081 = v_rest;
    int d1082 = 0;
    if ((!d1082) && (u_is_con(s1081, &n_list_46_Cons) && u_is_con(u_field(s1081, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1081, 0), 0);
      u_value v_i = u_field(u_field(s1081, 0), 1);
      u_value t1085 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1085)) {
        t1085 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1085)) {
        d1082 = 1;
        {
          static struct u_object l_operator_32__1083 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1084 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1083, u_concat(v_op, &l__32_is_32_not_32_associat_1084)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1082) {
      d1082 = 1;
      {
      }
    }
    if (!d1082) {
      u_no_case(s1081);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1086 = v_fp;
  u_value t1087 = NULL;
  if ((!t1087) && u_is_con(s1086, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1086, 0), 1);
    t1087 = u_eq(v_q, v_p);
  }
  if (!t1087) {
    t1087 = u_mk_bool(0);
  }
  if (!t1087) {
    u_no_case(s1086);
  }
  return t1087;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1088 = v_ts;
  u_value t1089 = NULL;
  if ((!t1089) && (u_is_con(s1088, &n_list_46_Cons) && (u_is_con(u_field(s1088, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1088, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1088, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1088, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1088, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1088, 0), 1);
    t1089 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1089) && (u_is_con(s1088, &n_list_46_Cons) && u_is_con(u_field(s1088, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1088, 0);
    t1089 = v_t;
  }
  if ((!t1089) && u_is_con(s1088, &n_list_46_Nil)) {
    t1089 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1089) {
    static struct u_object l__40_invalid_41__1090 = U_LIT("(invalid)");
    t1089 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1090), v_ts);
  }
  if (!t1089) {
    u_no_case(s1088);
  }
  return t1089;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1091 = v_ts;
  u_value t1092 = NULL;
  if ((!t1092) && (u_is_con(s1091, &n_list_46_Cons) && (u_is_con(u_field(s1091, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1091, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1091, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1091, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1091, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1091, 0), 1);
    t1092 = v_cs;
  }
  if (!t1092) {
    t1092 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1092) {
    u_no_case(s1091);
  }
  return t1092;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1093 = U_LIT("->");
  u_value t1100;
  if (u_truth(u_eq(v_op, &l__45__62__1093))) {
    u_value t1094 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1095 = &t1094;
    u_value l1096 = v_l;
    while (u_is_con(l1096, &n_list_46_Cons)) {
      u_value v_t = u_field(l1096, 0);
      if (u_truth(u_mk_bool(1))) {
        e1095 = u_append(e1095, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1096 = u_field(l1096, 1);
    }
    t1100 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1094, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1097 = U_LIT("*");
    u_value t1099;
    if (u_truth(u_eq(v_op, &l__42__1097))) {
      static struct u_object l__40_product_41__1098 = U_LIT("(product)");
      t1099 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1098), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1099 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1100 = t1099;
  }
  return t1100;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1101 = v_t;
  u_value t1102 = NULL;
  if ((!t1102) && (u_is_con(s1101, &n_syntax_46_TSort) && (u_is_con(u_field(s1101, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1101, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1101, 0), 1), u_mk_lit("(invalid)")))))) {
    t1102 = u_mk_bool(1);
  }
  if ((!t1102) && u_is_con(s1101, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1101, 1);
    t1102 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1102) && u_is_con(s1101, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1101, 0);
    u_value v_r = u_field(s1101, 1);
    u_value t1103 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1103)) {
      t1103 = f_parse_46_has_invalid_1(v_r);
    }
    t1102 = t1103;
  }
  if ((!t1102) && u_is_con(s1101, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1101, 0);
    t1102 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1102) {
    t1102 = u_mk_bool(0);
  }
  if (!t1102) {
    u_no_case(s1101);
  }
  return t1102;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1104 = v_ts;
  u_value t1105 = NULL;
  if ((!t1105) && u_is_con(s1104, &n_list_46_Nil)) {
    t1105 = u_mk_bool(0);
  }
  if ((!t1105) && u_is_con(s1104, &n_list_46_Cons)) {
    u_value v_t = u_field(s1104, 0);
    u_value v_more = u_field(s1104, 1);
    u_value t1106 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1106)) {
      t1106 = f_parse_46_any_invalid_1(v_more);
    }
    t1105 = t1106;
  }
  if (!t1105) {
    u_no_case(s1104);
  }
  return t1105;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1107 = v_t;
  u_value t1108 = NULL;
  if ((!t1108) && u_is_con(s1107, &n_lex_46_TName)) {
    u_value v_n = u_field(s1107, 0);
    u_value t1109 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1109)) {
      t1109 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1108 = t1109;
  }
  if ((!t1108) && (u_is_con(s1107, &n_lex_46_TPunct) && u_equals(u_field(s1107, 0), u_mk_char(40)))) {
    t1108 = u_mk_bool(1);
  }
  if (!t1108) {
    t1108 = u_mk_bool(0);
  }
  if (!t1108) {
    u_no_case(s1107);
  }
  return t1108;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1110 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1110;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_scope_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1111 = f_parse_46_type_one_1(v_r);
    *r_t = t1111;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1112 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1112, v_start), "parse.ParseError");
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
    u_value t1113 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1113;
    u_value t1114 = u_mk_bool(1);
    v_next = t1114;
    u_value t1115 = u_mk_bool(1);
    v_go = t1115;
    u_value t1124 = v_go;
    while (u_truth(t1124)) {
      {
        u_value s1116 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1117 = 0;
        if ((!d1117) && u_is_con(s1116, &n_option_46_Some)) {
          u_value v_op = u_field(s1116, 0);
          d1117 = 1;
          {
            u_value t1118 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1119 = u_mk_bool(1);
            v_rev = t1118;
            v_next = t1119;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1117) && u_is_con(s1116, &n_option_46_None)) {
          u_value t1122 = v_next;
          if (u_truth(t1122)) {
            t1122 = f_parse_46_starts_type_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1122)) {
            d1117 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1120 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1121 = u_mk_bool(0);
              v_rev = t1120;
              v_next = t1121;
            }
          }
        }
        if (!d1117) {
          d1117 = 1;
          {
            u_value t1123 = u_mk_bool(0);
            v_go = t1123;
          }
        }
        if (!d1117) {
          u_no_case(s1116);
        }
      }
      t1124 = v_go;
    }
    u_value s1125 = v_rev;
    int d1126 = 0;
    if ((!d1126) && u_is_con(s1125, &n_list_46_Nil)) {
      d1126 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1127 = U_LIT("type expected but ");
        static struct u_object l__32_found_1128 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1127, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1128)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1126) {
      d1126 = 1;
      {
        u_value t1129 = f_list_46_reverse_1(v_rev);
        *r_items = t1129;
      }
    }
    if (!d1126) {
      u_no_case(s1125);
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
            u_value t1130 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1130;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_scope_params_1(v_sc)))) {
            {
              u_value t1131 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1131;
            }
          } else {
            {
              u_value t1132 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1132;
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
    u_value t1133 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1133;
    u_value t1134 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1134)) {
      t1134 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1134)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1135 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1135;
        u_value t1137 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1137)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1136 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1136;
          }
          t1137 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1138 = f_list_46_reverse_1(v_rev);
    *r_r = t1138;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1139 = v_es;
  u_value t1140 = NULL;
  if ((!t1140) && u_is_con(s1139, &n_list_46_Nil)) {
    t1140 = v_rest;
  }
  if ((!t1140) && u_is_con(s1139, &n_list_46_Cons)) {
    u_value v_e = u_field(s1139, 0);
    u_value v_more = u_field(s1139, 1);
    static struct u_object l_Cons_1141 = U_LIT("Cons");
    t1140 = f_parse_46_expr_op2_3(&l_Cons_1141, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1140) {
    u_no_case(s1139);
  }
  return t1140;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1142 = v_t;
  u_value t1143 = NULL;
  if ((!t1143) && u_is_con(s1142, &n_lex_46_TName)) {
    u_value v_n = u_field(s1142, 0);
    static struct u_object l_let_1144 = U_LIT("let");
    static struct u_object l_if_1145 = U_LIT("if");
    static struct u_object l_lambda_1146 = U_LIT("lambda");
    static struct u_object l_match_1147 = U_LIT("match");
    static struct u_object l_true_1148 = U_LIT("true");
    static struct u_object l_false_1149 = U_LIT("false");
    u_value t1151 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1144, u_mk_con(&n_list_46_Cons, 2, &l_if_1145, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1146, u_mk_con(&n_list_46_Cons, 2, &l_match_1147, u_mk_con(&n_list_46_Cons, 2, &l_true_1148, u_mk_con(&n_list_46_Cons, 2, &l_false_1149, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1151)) {
      u_value t1150 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1150)) {
        t1150 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1151 = t1150;
    }
    t1143 = t1151;
  }
  if ((!t1143) && u_is_con(s1142, &n_lex_46_TNum)) {
    t1143 = u_mk_bool(1);
  }
  if ((!t1143) && u_is_con(s1142, &n_lex_46_TStr)) {
    t1143 = u_mk_bool(1);
  }
  if ((!t1143) && u_is_con(s1142, &n_lex_46_TChar)) {
    t1143 = u_mk_bool(1);
  }
  if ((!t1143) && u_is_con(s1142, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1142, 0);
    u_value t1152 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1152)) {
      t1152 = u_eq(v_c, u_mk_char(91));
    }
    t1143 = t1152;
  }
  if (!t1143) {
    t1143 = u_mk_bool(0);
  }
  if (!t1143) {
    u_no_case(s1142);
  }
  return t1143;
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
    u_value t1153 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1153;
    u_value t1154 = u_mk_bool(1);
    v_next = t1154;
    u_value t1155 = u_mk_bool(1);
    v_go = t1155;
    u_value t1164 = v_go;
    while (u_truth(t1164)) {
      {
        u_value s1156 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1157 = 0;
        if ((!d1157) && u_is_con(s1156, &n_option_46_Some)) {
          u_value v_op = u_field(s1156, 0);
          d1157 = 1;
          {
            u_value t1158 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1159 = u_mk_bool(1);
            v_rev = t1158;
            v_next = t1159;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1157) && u_is_con(s1156, &n_option_46_None)) {
          u_value t1162 = v_next;
          if (u_truth(t1162)) {
            t1162 = f_parse_46_starts_expr_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1162)) {
            d1157 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1160 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1161 = u_mk_bool(0);
              v_rev = t1160;
              v_next = t1161;
            }
          }
        }
        if (!d1157) {
          d1157 = 1;
          {
            u_value t1163 = u_mk_bool(0);
            v_go = t1163;
          }
        }
        if (!d1157) {
          u_no_case(s1156);
        }
      }
      t1164 = v_go;
    }
    u_value s1165 = v_rev;
    int d1166 = 0;
    if ((!d1166) && u_is_con(s1165, &n_list_46_Nil)) {
      d1166 = 1;
      {
        static struct u_object l_expression_32_expec_1167 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1168 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1167, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1168)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1166) {
      d1166 = 1;
      {
        u_value t1169 = f_list_46_reverse_1(v_rev);
        *r_items = t1169;
      }
    }
    if (!d1166) {
      u_no_case(s1165);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1170 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1170;
    u_value t1172 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1172)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1171 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1171;
      }
      t1172 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1173 = f_list_46_reverse_1(v_rev);
    *r_es = t1173;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1174 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1174;
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
    u_value t1176 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1176)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
        u_value t1175 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
        *r_e = t1175;
      }
      t1176 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
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
    u_value s1177 = f_parse_46_head_token_1(*r_ts);
    int d1178 = 0;
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TName) && u_equals(u_field(s1177, 0), u_mk_lit("let")))) {
      d1178 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TName) && u_equals(u_field(s1177, 0), u_mk_lit("if")))) {
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1179 = U_LIT("then");
        p_parse_46_keyword(&l_then_1179, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1180 = U_LIT("else");
        p_parse_46_keyword(&l_else_1180, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1181 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1181;
      }
    }
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TName) && u_equals(u_field(s1177, 0), u_mk_lit("lambda")))) {
      d1178 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TName) && u_equals(u_field(s1177, 0), u_mk_lit("match")))) {
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1182 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1182;
      }
    }
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TName) && u_equals(u_field(s1177, 0), u_mk_lit("true")))) {
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1183 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1183;
      }
    }
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TName) && u_equals(u_field(s1177, 0), u_mk_lit("false")))) {
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1184 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1184;
      }
    }
    if ((!d1178) && u_is_con(s1177, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1177, 0);
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1185 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1185;
      }
    }
    if ((!d1178) && u_is_con(s1177, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1177, 0);
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1186 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1186;
      }
    }
    if ((!d1178) && u_is_con(s1177, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1177, 0);
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1187 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1187;
      }
    }
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TPunct) && u_equals(u_field(s1177, 0), u_mk_char(40)))) {
      d1178 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1188 = v_es;
        int d1189 = 0;
        if ((!d1189) && (u_is_con(s1188, &n_list_46_Cons) && u_is_con(u_field(s1188, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1188, 0);
          d1189 = 1;
          {
            u_value t1190 = v_x;
            *r_e = t1190;
          }
        }
        if (!d1189) {
          d1189 = 1;
          {
            u_value t1191 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1191;
          }
        }
        if (!d1189) {
          u_no_case(s1188);
        }
      }
    }
    if ((!d1178) && (u_is_con(s1177, &n_lex_46_TPunct) && u_equals(u_field(s1177, 0), u_mk_char(91)))) {
      d1178 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1178) && u_is_con(s1177, &n_lex_46_TName)) {
      u_value v_n = u_field(s1177, 0);
      u_value t1193 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1193)) {
        t1193 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_scope_notations_1(v_sc), v_n));
      }
      if (u_truth(t1193)) {
        d1178 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1192 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1192;
        }
      }
    }
    if (!d1178) {
      d1178 = 1;
      {
        static struct u_object l_expression_32_expec_1194 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1195 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1194, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1195)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1178) {
      u_no_case(s1177);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1196 = v_ps;
  u_value t1197 = NULL;
  if ((!t1197) && u_is_con(s1196, &n_list_46_Nil)) {
    t1197 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1197) && (u_is_con(s1196, &n_list_46_Cons) && u_is_con(u_field(s1196, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1196, 0), 0);
    u_value v_more = u_field(s1196, 1);
    u_value s1198 = f_parse_46_pattern_vars_1(v_more);
    u_value t1199 = NULL;
    if ((!t1199) && u_is_con(s1198, &n_option_46_Some)) {
      u_value v_xs = u_field(s1198, 0);
      t1199 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1199) && u_is_con(s1198, &n_option_46_None)) {
      t1199 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1199) {
      u_no_case(s1198);
    }
    t1197 = t1199;
  }
  if (!t1197) {
    t1197 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1197) {
    u_no_case(s1196);
  }
  return t1197;
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
    static struct u_object l__58__61__1200 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1200), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1201 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1202 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1201;
    v_es = t1202;
    u_value t1206 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1206)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1203 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1203), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1204 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1205 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1204;
        v_es = t1205;
      }
      t1206 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1207 = U_LIT("in");
    p_parse_46_keyword(&l_in_1207, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1208 = f_list_46_reverse_1(v_ps);
    u_value t1209 = f_list_46_reverse_1(v_es);
    v_ps = t1208;
    v_es = t1209;
    u_value s1210 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1211 = 0;
    if ((!d1211) && u_is_con(u_field(s1210, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1210, 0), 0);
      d1211 = 1;
      {
        u_value t1212 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1212;
      }
    }
    if ((!d1211) && (u_is_con(u_field(s1210, 0), &n_option_46_None) && (u_is_con(u_field(s1210, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1210, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1210, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1210, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1210, 1), 0);
      u_value v_c = u_field(u_field(s1210, 2), 0);
      d1211 = 1;
      {
        u_value t1213 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1213;
      }
    }
    if (!d1211) {
      d1211 = 1;
      {
        u_value t1214 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1214;
      }
    }
    if (!d1211) {
      u_no_case(s1210);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1215 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1215;
    static struct u_object l__61__62__1216 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1216)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1217 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1217;
        u_value t1219 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1219)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1218 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1218;
          }
          t1219 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1220 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1220), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1221 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1221;
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
        static struct u_object l_Nil_1222 = U_LIT("Nil");
        u_value t1223 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1222), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1223;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1224 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1224))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1225 = U_LIT("in");
            p_parse_46_keyword(&l_in_1225, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1226 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1227 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1226;
            v_lists = t1227;
            u_value t1231 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1231)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1228 = U_LIT("in");
                p_parse_46_keyword(&l_in_1228, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1229 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1230 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1229;
                v_lists = t1230;
              }
              t1231 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1232 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1232;
            static struct u_object l_if_1233 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1233))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1234 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1234;
          }
        } else {
          {
            u_value t1235 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1235;
            u_value t1237 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1237)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1236 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1236;
              }
              t1237 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1238 = U_LIT("Nil");
            u_value t1239 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1238), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1239;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1240 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1240;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1241 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1241))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1242 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1242;
      }
    } else {
      {
        u_value t1243 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1243;
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
    u_value t1244 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1244;
    static struct u_object l_case_1245 = U_LIT("case");
    u_value t1248 = f_parse_46_at_keyword_2(*r_ts, &l_case_1245);
    while (u_truth(t1248)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1246 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1246), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1247 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1247;
      }
      static struct u_object l_case_1249 = U_LIT("case");
      t1248 = f_parse_46_at_keyword_2(*r_ts, &l_case_1249);
    }
    static struct u_object l_else_1250 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1250))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1251 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1251;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1252 = U_LIT("end");
    p_parse_46_keyword(&l_end_1252, r_ts);
    u_value t1253 = f_list_46_reverse_1(v_rev);
    *r_cs = t1253;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1254 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1254))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1255 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1255, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1256 = v_ps;
  u_value t1257 = NULL;
  if ((!t1257) && u_is_con(s1256, &n_list_46_Nil)) {
    t1257 = v_rest;
  }
  if ((!t1257) && u_is_con(s1256, &n_list_46_Cons)) {
    u_value v_p = u_field(s1256, 0);
    u_value v_more = u_field(s1256, 1);
    static struct u_object l_Cons_1258 = U_LIT("Cons");
    t1257 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1258), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1257) {
    u_no_case(s1256);
  }
  return t1257;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1259 = v_t;
  u_value t1260 = NULL;
  if ((!t1260) && u_is_con(s1259, &n_lex_46_TName)) {
    u_value v_n = u_field(s1259, 0);
    static struct u_object l___1261 = U_LIT("_");
    static struct u_object l_true_1262 = U_LIT("true");
    static struct u_object l_false_1263 = U_LIT("false");
    u_value t1265 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1261, u_mk_con(&n_list_46_Cons, 2, &l_true_1262, u_mk_con(&n_list_46_Cons, 2, &l_false_1263, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1265)) {
      u_value t1264 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1264)) {
        t1264 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1265 = t1264;
    }
    t1260 = t1265;
  }
  if ((!t1260) && u_is_con(s1259, &n_lex_46_TNum)) {
    t1260 = u_mk_bool(1);
  }
  if ((!t1260) && u_is_con(s1259, &n_lex_46_TStr)) {
    t1260 = u_mk_bool(1);
  }
  if ((!t1260) && u_is_con(s1259, &n_lex_46_TChar)) {
    t1260 = u_mk_bool(1);
  }
  if ((!t1260) && u_is_con(s1259, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1259, 0);
    u_value t1266 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1266)) {
      t1266 = u_eq(v_c, u_mk_char(91));
    }
    t1260 = t1266;
  }
  if (!t1260) {
    t1260 = u_mk_bool(0);
  }
  if (!t1260) {
    u_no_case(s1259);
  }
  return t1260;
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
    u_value t1267 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1267;
    u_value t1268 = u_mk_bool(1);
    v_next = t1268;
    u_value t1269 = u_mk_bool(1);
    v_go = t1269;
    u_value t1278 = v_go;
    while (u_truth(t1278)) {
      {
        u_value s1270 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1271 = 0;
        if ((!d1271) && u_is_con(s1270, &n_option_46_Some)) {
          u_value v_op = u_field(s1270, 0);
          d1271 = 1;
          {
            u_value t1272 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1273 = u_mk_bool(1);
            v_rev = t1272;
            v_next = t1273;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1271) && u_is_con(s1270, &n_option_46_None)) {
          u_value t1276 = v_next;
          if (u_truth(t1276)) {
            t1276 = f_parse_46_starts_pattern_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1276)) {
            d1271 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1274 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1275 = u_mk_bool(0);
              v_rev = t1274;
              v_next = t1275;
            }
          }
        }
        if (!d1271) {
          d1271 = 1;
          {
            u_value t1277 = u_mk_bool(0);
            v_go = t1277;
          }
        }
        if (!d1271) {
          u_no_case(s1270);
        }
      }
      t1278 = v_go;
    }
    u_value s1279 = v_rev;
    int d1280 = 0;
    if ((!d1280) && u_is_con(s1279, &n_list_46_Nil)) {
      d1280 = 1;
      {
        static struct u_object l_pattern_32_expected_1281 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1282 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1281, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1282)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1280) {
      d1280 = 1;
      {
        u_value t1283 = f_list_46_reverse_1(v_rev);
        *r_items = t1283;
      }
    }
    if (!d1280) {
      u_no_case(s1279);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1284 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1284;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1285 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1285;
        u_value t1287 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1287)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1286 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1286;
          }
          t1287 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1288 = f_list_46_reverse_1(v_rev);
    *r_ps = t1288;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1289 = f_parse_46_head_token_1(*r_ts);
    int d1290 = 0;
    if ((!d1290) && (u_is_con(s1289, &n_lex_46_TName) && u_equals(u_field(s1289, 0), u_mk_lit("_")))) {
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1291 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1291;
      }
    }
    if ((!d1290) && (u_is_con(s1289, &n_lex_46_TName) && u_equals(u_field(s1289, 0), u_mk_lit("true")))) {
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1292 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1292;
      }
    }
    if ((!d1290) && (u_is_con(s1289, &n_lex_46_TName) && u_equals(u_field(s1289, 0), u_mk_lit("false")))) {
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1293 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1293;
      }
    }
    if ((!d1290) && u_is_con(s1289, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1289, 0);
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1294 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1294;
      }
    }
    if ((!d1290) && u_is_con(s1289, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1289, 0);
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1295 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1295;
      }
    }
    if ((!d1290) && u_is_con(s1289, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1289, 0);
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1296 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1296;
      }
    }
    if ((!d1290) && (u_is_con(s1289, &n_lex_46_TPunct) && u_equals(u_field(s1289, 0), u_mk_char(40)))) {
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1297 = v_ps;
        int d1298 = 0;
        if ((!d1298) && (u_is_con(s1297, &n_list_46_Cons) && u_is_con(u_field(s1297, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1297, 0);
          d1298 = 1;
          {
            u_value t1299 = v_q;
            *r_p = t1299;
          }
        }
        if (!d1298) {
          d1298 = 1;
          {
            u_value t1300 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1300;
          }
        }
        if (!d1298) {
          u_no_case(s1297);
        }
      }
    }
    if ((!d1290) && (u_is_con(s1289, &n_lex_46_TPunct) && u_equals(u_field(s1289, 0), u_mk_char(91)))) {
      d1290 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1301 = U_LIT("Nil");
            u_value t1302 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1301), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1302;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1303 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1303;
            u_value t1305 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1305)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1304 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1304;
              }
              t1305 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1306 = U_LIT("Nil");
            u_value t1307 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1306), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1307;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1308 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1308;
          }
        }
      }
    }
    if (!d1290) {
      d1290 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1309 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1309;
          }
        } else {
          {
            u_value t1310 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1310;
          }
        }
      }
    }
    if (!d1290) {
      u_no_case(s1289);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1311 = v_t;
  u_value t1312 = NULL;
  if ((!t1312) && u_is_con(s1311, &n_lex_46_TName)) {
    u_value v_n = u_field(s1311, 0);
    static struct u_object l_skip_1313 = U_LIT("skip");
    static struct u_object l_if_1314 = U_LIT("if");
    static struct u_object l_while_1315 = U_LIT("while");
    static struct u_object l_call_1316 = U_LIT("call");
    static struct u_object l_match_1317 = U_LIT("match");
    static struct u_object l_assert_1318 = U_LIT("assert");
    static struct u_object l_assign_1319 = U_LIT("assign");
    static struct u_object l_raise_1320 = U_LIT("raise");
    static struct u_object l_try_1321 = U_LIT("try");
    t1312 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1313, u_mk_con(&n_list_46_Cons, 2, &l_if_1314, u_mk_con(&n_list_46_Cons, 2, &l_while_1315, u_mk_con(&n_list_46_Cons, 2, &l_call_1316, u_mk_con(&n_list_46_Cons, 2, &l_match_1317, u_mk_con(&n_list_46_Cons, 2, &l_assert_1318, u_mk_con(&n_list_46_Cons, 2, &l_assign_1319, u_mk_con(&n_list_46_Cons, 2, &l_raise_1320, u_mk_con(&n_list_46_Cons, 2, &l_try_1321, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1312) {
    t1312 = u_mk_bool(0);
  }
  if (!t1312) {
    u_no_case(s1311);
  }
  return t1312;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1322 = v_cs;
  u_value t1323 = NULL;
  if ((!t1323) && u_is_con(s1322, &n_list_46_Nil)) {
    t1323 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1323) && (u_is_con(s1322, &n_list_46_Cons) && u_is_con(u_field(s1322, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1322, 0), 0);
    u_value v_g = u_field(u_field(s1322, 0), 1);
    u_value v_b = u_field(u_field(s1322, 0), 2);
    u_value v_more = u_field(s1322, 1);
    t1323 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1323) {
    u_no_case(s1322);
  }
  return t1323;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1324 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1324;
    u_value t1325 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1325;
    u_value t1326 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1326;
    static struct u_object l_local_1327 = U_LIT("local");
    u_value t1329 = f_parse_46_at_keyword_2(*r_ts, &l_local_1327);
    while (u_truth(t1329)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1328 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1328)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1328 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1330 = U_LIT("local");
      t1329 = f_parse_46_at_keyword_2(*r_ts, &l_local_1330);
    }
    u_value t1332 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1332)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1331 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1331;
      }
      t1332 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1333 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1333;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1334 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1334;
    static struct u_object l__58__1335 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1335)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1336 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1336)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1337 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1337;
      }
    }
    u_value t1338 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1338;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1339 = f_parse_46_head_token_1(*r_ts);
    int d1340 = 0;
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("skip")))) {
      d1340 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1341 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1341;
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("if")))) {
      d1340 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("while")))) {
      d1340 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1342 = U_LIT("end");
        p_parse_46_keyword(&l_end_1342, r_ts);
        u_value t1343 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1343;
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("call")))) {
      d1340 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("match")))) {
      d1340 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1344 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1344;
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("assert")))) {
      d1340 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1345 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1345;
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("assign")))) {
      d1340 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("raise")))) {
      d1340 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1346 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1346;
      }
    }
    if ((!d1340) && (u_is_con(s1339, &n_lex_46_TName) && u_equals(u_field(s1339, 0), u_mk_lit("try")))) {
      d1340 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1347 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1347, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1348 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1348;
      }
    }
    if (!d1340) {
      d1340 = 1;
      {
        static struct u_object l_statement_32_expect_1349 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1350 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1349, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1350)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1340) {
      u_no_case(s1339);
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
    static struct u_object l_else_1351 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1351)))) {
      {
        static struct u_object l_end_1352 = U_LIT("end");
        p_parse_46_keyword(&l_end_1352, r_ts);
        u_value t1353 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1353;
      }
    } else {
      static struct u_object l_if_1354 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1354)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1355 = U_LIT("end");
          p_parse_46_keyword(&l_end_1355, r_ts);
        }
      }
    }
    u_value t1356 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1356;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1357 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1357), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1358 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1358;
    u_value t1361 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1361)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1359 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1359), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1360 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1360;
      }
      t1361 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1362 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1362;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1363 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1363;
    static struct u_object l_case_1364 = U_LIT("case");
    u_value t1367 = f_parse_46_at_keyword_2(*r_ts, &l_case_1364);
    while (u_truth(t1367)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1365 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1365), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1366 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1366;
      }
      static struct u_object l_case_1368 = U_LIT("case");
      t1367 = f_parse_46_at_keyword_2(*r_ts, &l_case_1368);
    }
    static struct u_object l_else_1369 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1369))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1370 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1370;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1371 = U_LIT("end");
    p_parse_46_keyword(&l_end_1371, r_ts);
    u_value t1372 = f_list_46_reverse_1(v_rev);
    *r_cs = t1372;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1374;
  if (u_truth(v_first)) {
    t1374 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1373 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1373)) {
      t1373 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1374 = t1373;
  }
  return t1374;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1376 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1376)) {
    static struct u_object l_out_1375 = U_LIT("out");
    t1376 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1375));
  }
  u_value t1378 = t1376;
  if (u_truth(t1378)) {
    static struct u_object l_ref_1377 = U_LIT("ref");
    t1378 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1377));
  }
  return t1378;
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
    u_value t1379 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1380 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1381 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1382 = u_mk_bool(1);
    v_ins = t1379;
    v_outs = t1380;
    v_refs = t1381;
    v_first = t1382;
    static struct u_object l_in_1383 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1383))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1384 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1385 = u_mk_bool(0);
        v_ins = t1384;
        v_first = t1385;
        u_value t1387 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1387)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1386 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1386;
          }
          t1387 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1388 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1388, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1389 = u_mk_bool(0);
        v_first = t1389;
      }
    }
    static struct u_object l_ref_1390 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1390, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1391 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1391;
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
    u_value t1392 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1392;
    u_value t1394 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1394)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1393 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1393;
      }
      t1394 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1395 = f_list_46_reverse_1(v_rev);
    *r_xs = t1395;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1396 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1396;
    static struct u_object l__58__1397 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1397)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1398 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1398;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1399 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1399;
    u_value t1401 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1401)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1400 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1400;
      }
      t1401 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1402 = f_list_46_reverse_1(v_rev);
    *r_xs = t1402;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1403 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1403;
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
  u_value s1404 = v_cs;
  u_value t1405 = NULL;
  if ((!t1405) && u_is_con(s1404, &n_list_46_Nil)) {
    t1405 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1405) && (u_is_con(s1404, &n_list_46_Cons) && u_is_con(u_field(s1404, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1404, 1);
    t1405 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1405) && (u_is_con(s1404, &n_list_46_Cons) && u_is_con(u_field(s1404, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1404, 1);
    t1405 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1405) && (u_is_con(s1404, &n_list_46_Cons) && u_is_con(u_field(s1404, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1404, 1);
    t1405 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1405) && u_is_con(s1404, &n_list_46_Cons)) {
    u_value v_c = u_field(s1404, 0);
    u_value v_more = u_field(s1404, 1);
    t1405 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1405) {
    u_no_case(s1404);
  }
  return t1405;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1406 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1407 = &t1406;
  u_value l1408 = v_cs;
  while (u_is_con(l1408, &n_list_46_Cons)) {
    u_value v_c = u_field(l1408, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1407 = u_append(e1407, v_c);
    }
    l1408 = u_field(l1408, 1);
  }
  return t1406;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1409 = v_c;
  u_value t1410 = NULL;
  if ((!t1410) && u_is_con(s1409, &n_syntax_46_CNotation)) {
    t1410 = u_mk_bool(1);
  }
  if (!t1410) {
    t1410 = u_mk_bool(0);
  }
  if (!t1410) {
    u_no_case(s1409);
  }
  return t1410;
}

u_value f_parse_46_loaded_modules_1(u_value v_ld) {
  u_value s1411 = v_ld;
  u_value t1412 = NULL;
  if ((!t1412) && u_is_con(s1411, &n_parse_46_Loader)) {
    u_value v_ms = u_field(s1411, 0);
    t1412 = v_ms;
  }
  if (!t1412) {
    u_no_case(s1411);
  }
  return t1412;
}

u_value f_parse_46_loading_1(u_value v_ld) {
  u_value s1413 = v_ld;
  u_value t1414 = NULL;
  if ((!t1414) && u_is_con(s1413, &n_parse_46_Loader)) {
    u_value v_ls = u_field(s1413, 1);
    t1414 = v_ls;
  }
  if (!t1414) {
    u_no_case(s1413);
  }
  return t1414;
}

u_value f_parse_46_base_notations_1(u_value v_ld) {
  u_value s1415 = v_ld;
  u_value t1416 = NULL;
  if ((!t1416) && u_is_con(s1415, &n_parse_46_Loader)) {
    u_value v_b = u_field(s1415, 2);
    t1416 = v_b;
  }
  if (!t1416) {
    u_no_case(s1415);
  }
  return t1416;
}

u_value f_parse_46_directory_1(u_value v_ld) {
  u_value s1417 = v_ld;
  u_value t1418 = NULL;
  if ((!t1418) && u_is_con(s1417, &n_parse_46_Loader)) {
    u_value v_d = u_field(s1417, 3);
    t1418 = v_d;
  }
  if (!t1418) {
    u_no_case(s1417);
  }
  return t1418;
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1419 = v_c;
  u_value t1420 = NULL;
  if ((!t1420) && u_is_con(s1419, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1419, 0);
    u_value v_cs = u_field(s1419, 1);
    u_value t1421 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1422 = &t1421;
    u_value l1423 = v_cs;
    while (u_is_con(l1423, &n_list_46_Cons)) {
      u_value v_d = u_field(l1423, 0);
      if (u_truth(u_mk_bool(1))) {
        e1422 = u_append(e1422, f_parse_46_modules_of_1(v_d));
      }
      l1423 = u_field(l1423, 1);
    }
    t1420 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1421));
  }
  if (!t1420) {
    t1420 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1420) {
    u_no_case(s1419);
  }
  return t1420;
}

u_value f_parse_46_own_notations_2(u_value v_ld, u_value v_n) {
  u_value s1424 = f_list_46_lookup_2(v_n, f_parse_46_loaded_modules_1(v_ld));
  u_value t1425 = NULL;
  if ((!t1425) && u_is_con(s1424, &n_option_46_Some)) {
    u_value v_ns = u_field(s1424, 0);
    t1425 = v_ns;
  }
  if ((!t1425) && u_is_con(s1424, &n_option_46_None)) {
    t1425 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1425) {
    u_no_case(s1424);
  }
  return t1425;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1426 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1426;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1427 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1427;
        u_value t1429 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1429)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1428 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1428;
          }
          t1429 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1430 = f_list_46_reverse_1(v_rev);
    *r_ps = t1430;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1431 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1431;
    u_value s1432 = f_parse_46_head_token_1(*r_ts);
    int d1433 = 0;
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("prefix")))) {
      d1433 = 1;
      {
        u_value t1434 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1434;
      }
    }
    if ((!d1433) && (u_is_con(s1432, &n_lex_46_TName) && u_equals(u_field(s1432, 0), u_mk_lit("postfix")))) {
      d1433 = 1;
      {
        u_value t1435 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1435;
      }
    }
    if (!d1433) {
      d1433 = 1;
      {
        u_value s1436 = f_parse_46_second_token_1(*r_ts);
        int d1437 = 0;
        if ((!d1437) && (u_is_con(s1436, &n_lex_46_TName) && u_equals(u_field(s1436, 0), u_mk_lit("left")))) {
          d1437 = 1;
          {
            u_value t1438 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1438;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1437) && (u_is_con(s1436, &n_lex_46_TName) && u_equals(u_field(s1436, 0), u_mk_lit("right")))) {
          d1437 = 1;
          {
            u_value t1439 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1439;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1437) {
          d1437 = 1;
          {
          }
        }
        if (!d1437) {
          u_no_case(s1436);
        }
      }
    }
    if (!d1433) {
      u_no_case(s1432);
    }
    p_parse_46_advance(r_ts);
    u_value s1440 = f_parse_46_head_token_1(*r_ts);
    int d1441 = 0;
    if ((!d1441) && u_is_con(s1440, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1440, 0);
      d1441 = 1;
      {
        u_value t1442 = v_n;
        v_p = t1442;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1441) {
      d1441 = 1;
      {
        static struct u_object l_precedence_32_expec_1443 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1444 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1443, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1444)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1441) {
      u_no_case(s1440);
    }
    u_value t1445 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1446 = u_mk_bool(1);
    v_ops = t1445;
    v_go = t1446;
    u_value t1452 = v_go;
    while (u_truth(t1452)) {
      {
        u_value s1447 = f_parse_46_head_token_1(*r_ts);
        int d1448 = 0;
        if ((!d1448) && u_is_con(s1447, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1447, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1448 = 1;
            {
              u_value t1449 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1449;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1448) && u_is_con(s1447, &n_lex_46_TName)) {
          u_value v_n = u_field(s1447, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1448 = 1;
            {
              u_value t1450 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1450;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1448) {
          d1448 = 1;
          {
            u_value t1451 = u_mk_bool(0);
            v_go = t1451;
          }
        }
        if (!d1448) {
          u_no_case(s1447);
        }
      }
      t1452 = v_go;
    }
    u_value s1453 = v_ops;
    int d1454 = 0;
    if ((!d1454) && u_is_con(s1453, &n_list_46_Nil)) {
      d1454 = 1;
      {
        static struct u_object l_operator_32_expecte_1455 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1456 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1455, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1456)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1454) {
      d1454 = 1;
      {
        u_value t1457 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1457;
      }
    }
    if (!d1454) {
      u_no_case(s1453);
    }
  }
}

void p_parse_46_constructor(u_value v_sc, u_value *r_k, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_args = NULL;
    p_parse_46_parse_name_or_symbol(&v_n, r_ts);
    u_value t1458 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1458;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_type_list(v_sc, u_mk_char(41), &v_args, r_ts);
      }
    }
    u_value t1459 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), v_args);
    *r_k = t1459;
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
    static struct u_object l__58__61__1460 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1460), r_ts);
    static struct u_object l__124__1461 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1461)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, r_ts);
    u_value t1462 = u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1462;
    static struct u_object l__124__1463 = U_LIT("|");
    u_value t1465 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1463));
    while (u_truth(t1465)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, r_ts);
        u_value t1464 = u_mk_con(&n_list_46_Cons, 2, v_k, v_rev);
        v_rev = t1464;
      }
      static struct u_object l__124__1466 = U_LIT("|");
      t1465 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1466));
    }
    u_value t1467 = u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_rev));
    *r_c = t1467;
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
    u_value t1468 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1468;
    u_value t1470 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1470)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1469 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1469;
      }
      t1470 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1471 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1471), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1472 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1472), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1473 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1473;
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
    u_value t1474 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1475 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1476 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1477 = u_mk_bool(1);
    v_ins = t1474;
    v_outs = t1475;
    v_refs = t1476;
    v_first = t1477;
    static struct u_object l_in_1478 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1478))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1479 = u_mk_bool(0);
        v_first = t1479;
      }
    }
    static struct u_object l_out_1480 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1480, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1481 = u_mk_bool(0);
        v_first = t1481;
      }
    }
    static struct u_object l_ref_1482 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1482, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1483 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1483), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1484 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1484;
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
    u_value s1485 = f_parse_46_head_token_1(*r_ts);
    int d1486 = 0;
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("import")))) {
      d1486 = 1;
      {
        u_value t1487 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1487;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_loaded_modules_1(v_ld))))) {
          {
            u_value t1488 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1488;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_ns, v_ld, v_at, r_cs);
            u_value t1489 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1489;
          }
        }
      }
    }
    if ((!d1486) && u_is_con(s1485, &n_lex_46_TName)) {
      u_value v_w = u_field(s1485, 0);
      static struct u_object l_prefix_1491 = U_LIT("prefix");
      static struct u_object l_postfix_1492 = U_LIT("postfix");
      static struct u_object l_infix_1493 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1491, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1492, u_mk_con(&n_list_46_Cons, 2, &l_infix_1493, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1486 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1490 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1490;
        }
      }
    }
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("type")))) {
      d1486 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1494 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1494), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1495 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1495;
      }
    }
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("data")))) {
      d1486 = 1;
      {
        p_parse_46_parse_data(v_ns, &v_c, r_ts);
        u_value t1496 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1496;
      }
    }
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("function")))) {
      d1486 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1497 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1497;
      }
    }
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("procedure")))) {
      d1486 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1498 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1498;
      }
    }
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("eval")))) {
      d1486 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1499 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1499;
      }
    }
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("run")))) {
      d1486 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1500 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1500), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1501 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1501;
      }
    }
    if ((!d1486) && (u_is_con(s1485, &n_lex_46_TName) && u_equals(u_field(s1485, 0), u_mk_lit("test")))) {
      d1486 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1502 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1502), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1503 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1503;
      }
    }
    if (!d1486) {
      d1486 = 1;
      {
        static struct u_object l_declaration_32_expe_1504 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1505 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1504, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1505)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1486) {
      u_no_case(s1485);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_ns, u_value *r_ld) {
  {
    u_value s1506 = v_cs;
    int d1507 = 0;
    if ((!d1507) && u_is_con(s1506, &n_list_46_Nil)) {
      d1507 = 1;
      {
      }
    }
    if ((!d1507) && (u_is_con(s1506, &n_list_46_Cons) && u_is_con(u_field(s1506, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1506, 0), 0);
      u_value v_p = u_field(u_field(s1506, 0), 1);
      u_value v_ops = u_field(u_field(s1506, 0), 2);
      u_value v_more = u_field(s1506, 1);
      d1507 = 1;
      {
        u_value t1508 = f_list_46__43__43__2(*r_ns, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_ns = t1508;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1507) && (u_is_con(s1506, &n_list_46_Cons) && u_is_con(u_field(s1506, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1506, 0), 0);
      u_value v_ds = u_field(u_field(s1506, 0), 1);
      u_value v_more = u_field(s1506, 1);
      d1507 = 1;
      {
        u_value t1509 = u_mk_con(&n_parse_46_Loader, 4, f_list_46__43__43__2(f_parse_46_modules_of_1(u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds)), f_parse_46_loaded_modules_1(*r_ld)), f_parse_46_loading_1(*r_ld), f_parse_46_base_notations_1(*r_ld), f_parse_46_directory_1(*r_ld));
        *r_ld = t1509;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1507) && (u_is_con(s1506, &n_list_46_Cons) && u_is_con(u_field(s1506, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1506, 0), 0);
      u_value v_more = u_field(s1506, 1);
      d1507 = 1;
      {
        u_value t1510 = f_list_46__43__43__2(*r_ns, f_parse_46_own_notations_2(*r_ld, v_n));
        *r_ns = t1510;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1507) && u_is_con(s1506, &n_list_46_Cons)) {
      u_value v_more = u_field(s1506, 1);
      d1507 = 1;
      {
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if (!d1507) {
      u_no_case(s1506);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_ns, u_value *r_ld, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1511 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1511;
    u_value t1513 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1513)) {
      {
        p_parse_46_parse_command(*r_ns, *r_ld, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_ns, r_ld);
        u_value t1512 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1512;
      }
      t1513 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1514 = f_list_46_reverse_1(v_rev);
    *r_cs = t1514;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1515 = v_dir;
    int d1516 = 0;
    if ((!d1516) && u_is_con(s1515, &n_option_46_Some)) {
      u_value v_d = u_field(s1515, 0);
      d1516 = 1;
      {
        {
          struct u_handler *h1519 = u_push();
          if (setjmp(h1519->env) == 0) {
            {
              static struct u_object l__47__1517 = U_LIT("/");
              static struct u_object l__46_u_1518 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1517, u_concat(v_n, &l__46_u_1518))), r_text);
            }
            u_pop(h1519);
          } else {
            u_pop(h1519);
            int d1520 = 0;
            if ((!d1520) && (u_has_type(h1519, "os.OsError") && u_is_con(u_caught(h1519), &n_os_46_OsError))) {
              d1520 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1520) {
              u_reraise(h1519);
            }
          }
        }
      }
    }
    if ((!d1516) && u_is_con(s1515, &n_option_46_None)) {
      d1516 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1516) {
      u_no_case(s1515);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1521 = u_push();
      if (setjmp(h1521->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1521);
      } else {
        u_pop(h1521);
        int d1522 = 0;
        if ((!d1522) && (u_has_type(h1521, "os.OsError") && u_is_con(u_caught(h1521), &n_os_46_OsError))) {
          d1522 = 1;
          {
            static struct u_object l_unknown_32_module_32__1523 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1523, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1522) {
          u_reraise(h1521);
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
        static struct u_object l_cyclic_32_import_32_of_1524 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1524, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_directory_1(v_ld), v_at, &v_text);
    u_value t1525 = u_mk_con(&n_parse_46_Loader, 4, f_parse_46_loaded_modules_1(v_ld), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_loading_1(v_ld)), f_parse_46_base_notations_1(v_ld), f_parse_46_directory_1(v_ld));
    v_mld = t1525;
    u_value s1526 = f_parse_46_base_notations_1(v_ld);
    int d1527 = 0;
    if ((!d1527) && u_is_con(s1526, &n_option_46_Some)) {
      u_value v_b = u_field(s1526, 0);
      d1527 = 1;
      {
        u_value t1528 = v_b;
        v_mns = t1528;
      }
    }
    if ((!d1527) && u_is_con(s1526, &n_option_46_None)) {
      d1527 = 1;
      {
        u_value t1529 = v_ns;
        v_mns = t1529;
      }
    }
    if (!d1527) {
      u_no_case(s1526);
    }
    {
      struct u_handler *h1530 = u_push();
      if (setjmp(h1530->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mns, &v_mld, &v_mts);
        }
        u_pop(h1530);
      } else {
        u_pop(h1530);
        int d1531 = 0;
        if ((!d1531) && (u_has_type(h1530, "lex.LexError") && u_is_con(u_caught(h1530), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1530), 0);
          u_value v_i = u_field(u_caught(h1530), 1);
          d1531 = 1;
          {
            static struct u_object l__32_in_32__1532 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1532, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1531) && (u_has_type(h1530, "parse.ParseError") && u_is_con(u_caught(h1530), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1530), 0);
          u_value v_i = u_field(u_caught(h1530), 1);
          d1531 = 1;
          {
            static struct u_object l__32_in_32__1533 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1533, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1531) {
          u_reraise(h1530);
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
      struct u_handler *h1534 = u_push();
      if (setjmp(h1534->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1534);
      } else {
        u_pop(h1534);
        int d1535 = 0;
        if ((!d1535) && (u_has_type(h1534, "lex.LexError") && u_is_con(u_caught(h1534), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1534), 0);
          u_value v_i = u_field(u_caught(h1534), 1);
          d1535 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1535) {
          u_reraise(h1534);
        }
      }
    }
    u_value t1536 = v_ns;
    u_value t1537 = v_ld;
    v_ns1 = t1536;
    v_ld1 = t1537;
    p_parse_46_parse_commands(&v_cs, &v_ns1, &v_ld1, &v_ts);
    u_value t1538 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1538;
  }
}

void p_parse_46_prelude_scope(u_value v_pre, u_value *r_ns, u_value *r_ms) {
  {
    u_value v_ld = NULL;
    u_value t1539 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1540 = u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    *r_ns = t1539;
    v_ld = t1540;
    p_parse_46_declare_all(f_parse_46_commands_of_1(v_pre), r_ns, &v_ld);
    u_value t1541 = f_parse_46_loaded_modules_1(v_ld);
    *r_ms = t1541;
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
    static struct u_object l_prelude_1542 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1542))) {
      {
        static struct u_object l_prelude_1543 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1543, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1544 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1544, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1545 = v_xs;
  u_value t1546 = NULL;
  if ((!t1546) && (u_is_con(s1545, &n_list_46_Cons) && u_is_con(u_field(s1545, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1545, 0);
    t1546 = v_x;
  }
  if ((!t1546) && u_is_con(s1545, &n_list_46_Cons)) {
    u_value v_more = u_field(s1545, 1);
    t1546 = f_parse_46_last_part_1(v_more);
  }
  if (!t1546) {
    static struct u_object l__1547 = U_LIT("");
    t1546 = &l__1547;
  }
  if (!t1546) {
    u_no_case(s1545);
  }
  return t1546;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1552 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1551;
  {
    u_value v_f = t1552;
    u_value t1549 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1549)) {
      static struct u_object l__46_u_1548 = U_LIT(".u");
      t1549 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1548);
    }
    u_value t1550;
    if (u_truth(t1549)) {
      t1550 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1550 = v_f;
    }
    t1551 = t1550;
  }
  return t1551;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1557 = u_str_split(v_path, u_mk_char(47));
  u_value t1556;
  {
    u_value v_parts = t1557;
    u_value t1555;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1553 = U_LIT(".");
      t1555 = &l__46__1553;
    } else {
      static struct u_object l__47__1554 = U_LIT("/");
      t1555 = u_str_join(&l__47__1554, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1556 = t1555;
  }
  return t1556;
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
    static struct u_object l_prelude_1558 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1558, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

u_value f_parse_46_commands_of_1(u_value v_p) {
  u_value s1559 = v_p;
  u_value t1560 = NULL;
  if ((!t1560) && u_is_con(s1559, &n_syntax_46_Program)) {
    u_value v_cs = u_field(s1559, 0);
    t1560 = v_cs;
  }
  if (!t1560) {
    u_no_case(s1559);
  }
  return t1560;
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1561 = u_mk_tuple(2, v_cs, v_ds);
    int d1562 = 0;
    if ((!d1562) && (u_is_con(u_field(s1561, 0), &n_list_46_Cons) && u_is_con(u_field(s1561, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1561, 0), 0);
      u_value v_more = u_field(u_field(s1561, 0), 1);
      u_value v_d = u_field(u_field(s1561, 1), 0);
      u_value v_rest = u_field(u_field(s1561, 1), 1);
      d1562 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1562) {
      d1562 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1562) {
      u_no_case(s1561);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1563 = v_paths;
    int d1564 = 0;
    if ((!d1564) && u_is_con(s1563, &n_list_46_Nil)) {
      d1564 = 1;
      {
      }
    }
    if ((!d1564) && u_is_con(s1563, &n_list_46_Cons)) {
      u_value v_path = u_field(s1563, 0);
      u_value v_more = u_field(s1563, 1);
      d1564 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_parse_46_commands_of_1(v_p), f_parse_46_commands_of_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1564) {
      u_no_case(s1563);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1565 = v_paths;
    int d1566 = 0;
    if ((!d1566) && u_is_con(s1565, &n_list_46_Nil)) {
      d1566 = 1;
      {
      }
    }
    if ((!d1566) && u_is_con(s1565, &n_list_46_Cons)) {
      u_value v_path = u_field(s1565, 0);
      u_value v_more = u_field(s1565, 1);
      d1566 = 1;
      {
        u_value t1567 = u_mk_bool(0);
        v_failed = t1567;
        {
          struct u_handler *h1568 = u_push();
          if (setjmp(h1568->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1568);
          } else {
            u_pop(h1568);
            int d1569 = 0;
            if ((!d1569) && (u_has_type(h1568, "parse.ParseError") && u_is_con(u_caught(h1568), &n_parse_46_ParseError))) {
              d1569 = 1;
              {
                u_value t1570 = u_mk_bool(1);
                v_failed = t1570;
              }
            }
            if (!d1569) {
              u_reraise(h1568);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1566) {
      u_no_case(s1565);
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
  static struct u_object l_u_value_1571 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1571);
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
  static struct u_object l_u_value_1572 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1572));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1573 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1574 = &t1573;
  u_value l1575 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1575, &n_list_46_Cons)) {
    u_value v_c = u_field(l1575, 0);
    if (u_truth(u_mk_bool(1))) {
      e1574 = u_append(e1574, f_compile_46_mangle_char_1(v_c));
    }
    l1575 = u_field(l1575, 1);
  }
  return u_string(f_list_46_concat_1(t1573));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1576 = u_is_letter(v_c);
  if (!u_truth(t1576)) {
    t1576 = u_is_digit(v_c);
  }
  u_value t1577 = t1576;
  if (!u_truth(t1577)) {
    t1577 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1580;
  if (u_truth(t1577)) {
    t1580 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1578 = U_LIT("_");
    static struct u_object l___1579 = U_LIT("_");
    t1580 = u_chars(u_concat(&l___1578, u_concat(u_str_show(u_ord(v_c)), &l___1579)));
  }
  return t1580;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1581 = U_LIT("f_");
  static struct u_object l___1582 = U_LIT("_");
  return u_concat(&l_f__1581, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1582, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1583 = U_LIT("p_");
  return u_concat(&l_p__1583, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1584 = U_LIT("v_");
  return u_concat(&l_v__1584, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1585 = U_LIT("r_");
  return u_concat(&l_r__1585, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1586 = U_LIT("+");
  static struct u_object l_u_add_1587 = U_LIT("u_add");
  static struct u_object l__45__1588 = U_LIT("-");
  static struct u_object l_u_sub_1589 = U_LIT("u_sub");
  static struct u_object l__42__1590 = U_LIT("*");
  static struct u_object l_u_mul_1591 = U_LIT("u_mul");
  static struct u_object l__47__1592 = U_LIT("/");
  static struct u_object l_u_div_1593 = U_LIT("u_div");
  static struct u_object l__37__1594 = U_LIT("%");
  static struct u_object l_u_mod_1595 = U_LIT("u_mod");
  static struct u_object l__60__1596 = U_LIT("<");
  static struct u_object l_u_lt_1597 = U_LIT("u_lt");
  static struct u_object l__61__61__1598 = U_LIT("==");
  static struct u_object l_u_eq_1599 = U_LIT("u_eq");
  static struct u_object l_compare_1600 = U_LIT("compare");
  static struct u_object l_u_compare_1601 = U_LIT("u_compare");
  static struct u_object l_chars_1602 = U_LIT("chars");
  static struct u_object l_u_chars_1603 = U_LIT("u_chars");
  static struct u_object l_string_1604 = U_LIT("string");
  static struct u_object l_u_string_1605 = U_LIT("u_string");
  static struct u_object l_ord_1606 = U_LIT("ord");
  static struct u_object l_u_ord_1607 = U_LIT("u_ord");
  static struct u_object l_chr_1608 = U_LIT("chr");
  static struct u_object l_u_chr_1609 = U_LIT("u_chr");
  static struct u_object l__94__1610 = U_LIT("^");
  static struct u_object l_u_concat_1611 = U_LIT("u_concat");
  static struct u_object l_str_size_1612 = U_LIT("str_size");
  static struct u_object l_u_str_size_1613 = U_LIT("u_str_size");
  static struct u_object l_str_join_1614 = U_LIT("str_join");
  static struct u_object l_u_str_join_1615 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1616 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1617 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1618 = U_LIT("str_split");
  static struct u_object l_u_str_split_1619 = U_LIT("u_str_split");
  static struct u_object l_str_get_1620 = U_LIT("str_get");
  static struct u_object l_u_str_get_1621 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1622 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1623 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1624 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1625 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1626 = U_LIT("str_show");
  static struct u_object l_u_str_show_1627 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1628 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1629 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1630 = U_LIT("str_number");
  static struct u_object l_u_str_number_1631 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1632 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1633 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1634 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1635 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1636 = U_LIT("is_space");
  static struct u_object l_u_is_space_1637 = U_LIT("u_is_space");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1586), u_mk_tuple(2, &l_u_add_1587, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1588), u_mk_tuple(2, &l_u_sub_1589, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1590), u_mk_tuple(2, &l_u_mul_1591, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1592), u_mk_tuple(2, &l_u_div_1593, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1594), u_mk_tuple(2, &l_u_mod_1595, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1596), u_mk_tuple(2, &l_u_lt_1597, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1598), u_mk_tuple(2, &l_u_eq_1599, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1600), u_mk_tuple(2, &l_u_compare_1601, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1602), u_mk_tuple(2, &l_u_chars_1603, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1604), u_mk_tuple(2, &l_u_string_1605, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1606), u_mk_tuple(2, &l_u_ord_1607, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1608), u_mk_tuple(2, &l_u_chr_1609, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1610), u_mk_tuple(2, &l_u_concat_1611, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1612), u_mk_tuple(2, &l_u_str_size_1613, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1614), u_mk_tuple(2, &l_u_str_join_1615, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1616), u_mk_tuple(2, &l_u_str_slice_1617, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1618), u_mk_tuple(2, &l_u_str_split_1619, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1620), u_mk_tuple(2, &l_u_str_get_1621, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1622), u_mk_tuple(2, &l_u_str_contains_1623, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1624), u_mk_tuple(2, &l_u_str_compare_1625, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1626), u_mk_tuple(2, &l_u_str_show_1627, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1628), u_mk_tuple(2, &l_u_str_quote_1629, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1630), u_mk_tuple(2, &l_u_str_number_1631, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1632), u_mk_tuple(2, &l_u_is_digit_1633, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1634), u_mk_tuple(2, &l_u_is_letter_1635, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1636), u_mk_tuple(2, &l_u_is_space_1637, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_funs_of_1(u_value v_gl) {
  u_value s1638 = v_gl;
  u_value t1639 = NULL;
  if ((!t1639) && u_is_con(s1638, &n_compile_46_Globals)) {
    u_value v_fs = u_field(s1638, 0);
    t1639 = v_fs;
  }
  if (!t1639) {
    u_no_case(s1638);
  }
  return t1639;
}

u_value f_compile_46_constrs_of_1(u_value v_gl) {
  u_value s1640 = v_gl;
  u_value t1641 = NULL;
  if ((!t1641) && u_is_con(s1640, &n_compile_46_Globals)) {
    u_value v_ks = u_field(s1640, 1);
    t1641 = v_ks;
  }
  if (!t1641) {
    u_no_case(s1640);
  }
  return t1641;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1642 = U_LIT("&");
  static struct u_object l_n__1643 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1642, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1643, f_compile_46_mangle_1(v_n))));
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
  u_value s1644 = v_n;
  u_value t1645 = NULL;
  if ((!t1645) && (u_is_con(s1644, &n_syntax_46_Name) && u_is_con(u_field(s1644, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1644, 0), 0);
    u_value v_b = u_field(s1644, 1);
    static struct u_object l_const_32_struct_32_u_n_1646 = U_LIT("const struct u_name");
    static struct u_object l_n__1647 = U_LIT("n_");
    t1645 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1646), u_concat(&l_n__1647, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1645) && (u_is_con(s1644, &n_syntax_46_Name) && u_is_con(u_field(s1644, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1644, 1);
    static struct u_object l_const_32_struct_32_u_n_1648 = U_LIT("const struct u_name");
    static struct u_object l_n__1649 = U_LIT("n_");
    static struct u_object l_NULL_1650 = U_LIT("NULL");
    t1645 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1648), u_concat(&l_n__1649, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1650), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1645) {
    u_no_case(s1644);
  }
  return t1645;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1651 = v_cs;
  u_value t1652 = NULL;
  if ((!t1652) && u_is_con(s1651, &n_list_46_Nil)) {
    t1652 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1652) && (u_is_con(s1651, &n_list_46_Cons) && u_is_con(u_field(s1651, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1651, 0), 2);
    u_value v_rest = u_field(s1651, 1);
    u_value t1653 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1654 = &t1653;
    u_value l1655 = v_ks;
    while (u_is_con(l1655, &n_list_46_Cons)) {
      u_value v_k = u_field(l1655, 0);
      if (u_truth(u_mk_bool(1))) {
        e1654 = u_append(e1654, f_check_46_constr_name_1(v_k));
      }
      l1655 = u_field(l1655, 1);
    }
    t1652 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1653, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1652) && u_is_con(s1651, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1651, 1);
    t1652 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1652) {
    u_no_case(s1651);
  }
  return t1652;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1656 = v_ks;
  u_value t1657 = NULL;
  if ((!t1657) && u_is_con(s1656, &n_list_46_Nil)) {
    t1657 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1657) && u_is_con(s1656, &n_list_46_Cons)) {
    u_value v_k = u_field(s1656, 0);
    u_value v_rest = u_field(s1656, 1);
    u_value t1658;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1658 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1658 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1657 = t1658;
  }
  if (!t1657) {
    u_no_case(s1656);
  }
  return t1657;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1659 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1660 = &t1659;
  u_value l1661 = v_xs;
  while (u_is_con(l1661, &n_list_46_Cons)) {
    u_value v_x = u_field(l1661, 0);
    if (u_truth(u_mk_bool(1))) {
      e1660 = u_append(e1660, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1661 = u_field(l1661, 1);
  }
  return t1659;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1662 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1662, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1663 = f_list_46_lookup_2(v_x, v_env);
  u_value t1664 = NULL;
  if ((!t1664) && u_is_con(s1663, &n_option_46_Some)) {
    u_value v_c = u_field(s1663, 0);
    t1664 = v_c;
  }
  if (!t1664) {
    t1664 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1664) {
    u_no_case(s1663);
  }
  return t1664;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1665 = v_c;
  u_value t1666 = NULL;
  if ((!t1666) && (u_is_con(s1665, &n_c_46_CeUnary) && u_equals(u_field(s1665, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1665, 1);
    t1666 = v_p;
  }
  if (!t1666) {
    static struct u_object l__38__1667 = U_LIT("&");
    t1666 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1667, v_c);
  }
  if (!t1666) {
    u_no_case(s1665);
  }
  return t1666;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1668 = v_t;
  u_value t1669 = NULL;
  if ((!t1669) && (u_is_con(s1668, &n_syntax_46_TSort) && u_is_con(u_field(s1668, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1668, 0);
    t1669 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1669) && u_is_con(s1668, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1668, 0);
    u_value v_args = u_field(s1668, 1);
    static struct u_object l__91__1670 = U_LIT("[");
    static struct u_object l__44__32__1671 = U_LIT(", ");
    u_value t1672 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1673 = &t1672;
    u_value l1674 = v_args;
    while (u_is_con(l1674, &n_list_46_Cons)) {
      u_value v_a = u_field(l1674, 0);
      if (u_truth(u_mk_bool(1))) {
        e1673 = u_append(e1673, f_compile_46_type_key_1(v_a));
      }
      l1674 = u_field(l1674, 1);
    }
    static struct u_object l__93__1675 = U_LIT("]");
    t1669 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1670, u_concat(u_str_join(&l__44__32__1671, t1672), &l__93__1675)));
  }
  if ((!t1669) && u_is_con(s1668, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1668, 0);
    u_value v_r = u_field(s1668, 1);
    static struct u_object l__40__1676 = U_LIT("(");
    static struct u_object l__44__32__1677 = U_LIT(", ");
    u_value t1678 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1679 = &t1678;
    u_value l1680 = v_args;
    while (u_is_con(l1680, &n_list_46_Cons)) {
      u_value v_a = u_field(l1680, 0);
      if (u_truth(u_mk_bool(1))) {
        e1679 = u_append(e1679, f_compile_46_type_key_1(v_a));
      }
      l1680 = u_field(l1680, 1);
    }
    static struct u_object l__41__32__45__62__32__1681 = U_LIT(") -> ");
    t1669 = u_concat(&l__40__1676, u_concat(u_str_join(&l__44__32__1677, t1678), u_concat(&l__41__32__45__62__32__1681, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1669) && u_is_con(s1668, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1668, 0);
    static struct u_object l__40__1682 = U_LIT("(");
    static struct u_object l__32__42__32__1683 = U_LIT(" * ");
    u_value t1684 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1685 = &t1684;
    u_value l1686 = v_ts;
    while (u_is_con(l1686, &n_list_46_Cons)) {
      u_value v_a = u_field(l1686, 0);
      if (u_truth(u_mk_bool(1))) {
        e1685 = u_append(e1685, f_compile_46_type_key_1(v_a));
      }
      l1686 = u_field(l1686, 1);
    }
    static struct u_object l__41__1687 = U_LIT(")");
    t1669 = u_concat(&l__40__1682, u_concat(u_str_join(&l__32__42__32__1683, t1684), &l__41__1687));
  }
  if (!t1669) {
    static struct u_object l__63__1688 = U_LIT("?");
    t1669 = &l__63__1688;
  }
  if (!t1669) {
    u_no_case(s1668);
  }
  return t1669;
}

u_value f_compile_46_program_cmds_1(u_value v_p) {
  u_value s1689 = v_p;
  u_value t1690 = NULL;
  if ((!t1690) && u_is_con(s1689, &n_syntax_46_Program)) {
    u_value v_cs = u_field(s1689, 0);
    t1690 = v_cs;
  }
  if (!t1690) {
    u_no_case(s1689);
  }
  return t1690;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1691 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1692 = &t1691;
  u_value l1693 = v_cs;
  while (u_is_con(l1693, &n_list_46_Cons)) {
    u_value v_c = u_field(l1693, 0);
    if (u_truth(u_mk_bool(1))) {
      e1692 = u_append(e1692, f_compile_46_fun_arities_1(v_c));
    }
    l1693 = u_field(l1693, 1);
  }
  u_value t1694 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1695 = &t1694;
  u_value l1696 = v_cs;
  while (u_is_con(l1696, &n_list_46_Cons)) {
    u_value v_c = u_field(l1696, 0);
    if (u_truth(u_mk_bool(1))) {
      e1695 = u_append(e1695, f_compile_46_constr_arities_1(v_c));
    }
    l1696 = u_field(l1696, 1);
  }
  u_value t1698 = f_list_46_concat_1(t1691);
  u_value t1699 = f_list_46_concat_1(t1694);
  u_value t1697;
  {
    u_value v_fs = t1698;
    u_value v_ks = t1699;
    t1697 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1697;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1700 = v_fs;
  u_value t1701 = NULL;
  if ((!t1701) && u_is_con(s1700, &n_list_46_Nil)) {
    t1701 = v_m;
  }
  if ((!t1701) && u_is_con(s1700, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1700, 0), 0);
    u_value v_k = u_field(u_field(s1700, 0), 1);
    u_value v_rest = u_field(s1700, 1);
    u_value s1702 = f_map_46_get_2(v_f, v_m);
    u_value t1703 = NULL;
    if ((!t1703) && u_is_con(s1702, &n_option_46_Some)) {
      u_value v_ks = u_field(s1702, 0);
      t1703 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1703) && u_is_con(s1702, &n_option_46_None)) {
      t1703 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1703) {
      u_no_case(s1702);
    }
    t1701 = t1703;
  }
  if (!t1701) {
    u_no_case(s1700);
  }
  return t1701;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1704 = v_gl;
  u_value t1705 = NULL;
  if ((!t1705) && u_is_con(s1704, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1704, 2);
    u_value s1706 = f_map_46_get_2(v_n, v_fm);
    u_value t1707 = NULL;
    if ((!t1707) && u_is_con(s1706, &n_option_46_Some)) {
      u_value v_ks = u_field(s1706, 0);
      t1707 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1707) && u_is_con(s1706, &n_option_46_None)) {
      t1707 = u_mk_bool(0);
    }
    if (!t1707) {
      u_no_case(s1706);
    }
    t1705 = t1707;
  }
  if (!t1705) {
    u_no_case(s1704);
  }
  return t1705;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1708 = v_gl;
  u_value t1709 = NULL;
  if ((!t1709) && u_is_con(s1708, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1708, 3);
    t1709 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1709) {
    u_no_case(s1708);
  }
  return t1709;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1710 = v_c;
  u_value t1711 = NULL;
  if ((!t1711) && u_is_con(s1710, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1710, 0);
    u_value v_xss = u_field(s1710, 2);
    t1711 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1711) {
    t1711 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1711) {
    u_no_case(s1710);
  }
  return t1711;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1712 = v_c;
  u_value t1713 = NULL;
  if ((!t1713) && u_is_con(s1712, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1712, 2);
    u_value t1714 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1715 = &t1714;
    u_value l1716 = v_ks;
    while (u_is_con(l1716, &n_list_46_Cons)) {
      u_value v_k = u_field(l1716, 0);
      if (u_truth(u_mk_bool(1))) {
        e1715 = u_append(e1715, f_compile_46_constr_arity_1(v_k));
      }
      l1716 = u_field(l1716, 1);
    }
    t1713 = t1714;
  }
  if (!t1713) {
    t1713 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1713) {
    u_no_case(s1712);
  }
  return t1713;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1717 = v_k;
  u_value t1718 = NULL;
  if ((!t1718) && u_is_con(s1717, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1717, 0);
    u_value v_ts = u_field(s1717, 1);
    t1718 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1718) {
    u_no_case(s1717);
  }
  return t1718;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1719 = v_xs;
  u_value t1720 = NULL;
  if ((!t1720) && u_is_con(s1719, &n_list_46_Nil)) {
    t1720 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1720) && u_is_con(s1719, &n_list_46_Cons)) {
    u_value v_x = u_field(s1719, 0);
    u_value v_rest = u_field(s1719, 1);
    t1720 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1720) {
    u_no_case(s1719);
  }
  return t1720;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1721 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1721, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1722 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1722, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1723 = v_cs;
  u_value t1724 = NULL;
  if ((!t1724) && u_is_con(s1723, &n_list_46_Nil)) {
    t1724 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1724) && (u_is_con(s1723, &n_list_46_Cons) && u_is_con(u_field(s1723, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1723, 0);
    t1724 = v_d;
  }
  if ((!t1724) && u_is_con(s1723, &n_list_46_Cons)) {
    u_value v_d = u_field(s1723, 0);
    u_value v_rest = u_field(s1723, 1);
    static struct u_object l__38__38__1725 = U_LIT("&&");
    t1724 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1725, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1724) {
    u_no_case(s1723);
  }
  return t1724;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1726 = v_l;
  u_value t1727 = NULL;
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1728 = U_LIT("u_mk_tuple");
    t1727 = f_c_46_c_call_2(&l_u_mk_tuple_1728, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1726, 0);
    static struct u_object l_u_mk_bool_1729 = U_LIT("u_mk_bool");
    u_value t1730;
    if (u_truth(v_b)) {
      t1730 = u_mk_int(1);
    } else {
      t1730 = u_mk_int(0);
    }
    t1727 = f_c_46_c_call_2(&l_u_mk_bool_1729, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1730), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1726, 0);
    u_value t1731 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1731)) {
      t1731 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1734;
    if (u_truth(t1731)) {
      static struct u_object l_u_mk_int_1732 = U_LIT("u_mk_int");
      t1734 = f_c_46_c_call_2(&l_u_mk_int_1732, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1733 = U_LIT("u_mk_int_str");
      t1734 = f_c_46_c_call_2(&l_u_mk_int_str_1733, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1727 = t1734;
  }
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1726, 0);
    static struct u_object l_u_mk_lit_1735 = U_LIT("u_mk_lit");
    t1727 = f_c_46_c_call_2(&l_u_mk_lit_1735, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1727) && u_is_con(s1726, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1726, 0);
    static struct u_object l_u_mk_char_1736 = U_LIT("u_mk_char");
    t1727 = f_c_46_c_call_2(&l_u_mk_char_1736, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1727) {
    u_no_case(s1726);
  }
  return t1727;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1737 = v_p;
  u_value t1738 = NULL;
  if ((!t1738) && u_is_con(s1737, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1737, 0);
    static struct u_object l_u_equals_1739 = U_LIT("u_equals");
    t1738 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1739, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1738) && u_is_con(s1737, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1737, 0);
    u_value v_ps = u_field(s1737, 1);
    static struct u_object l_u_is_con_1740 = U_LIT("u_is_con");
    t1738 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1740, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1738) && u_is_con(s1737, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1737, 0);
    t1738 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1738) {
    t1738 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1738) {
    u_no_case(s1737);
  }
  return t1738;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1741 = v_ps;
  u_value t1742 = NULL;
  if ((!t1742) && u_is_con(s1741, &n_list_46_Nil)) {
    t1742 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1742) && u_is_con(s1741, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1741, 0), 0);
    u_value v_i = u_field(u_field(s1741, 0), 1);
    u_value v_rest = u_field(s1741, 1);
    t1742 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1742) {
    u_no_case(s1741);
  }
  return t1742;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1743 = v_p;
  u_value t1744 = NULL;
  if ((!t1744) && (u_is_con(s1743, &n_syntax_46_PVar) && u_is_con(u_field(s1743, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1743, 0), 0);
    t1744 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1744) && u_is_con(s1743, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1743, 1);
    t1744 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1744) && u_is_con(s1743, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1743, 0);
    t1744 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1744) {
    t1744 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1744) {
    u_no_case(s1743);
  }
  return t1744;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1745 = v_ps;
  u_value t1746 = NULL;
  if ((!t1746) && u_is_con(s1745, &n_list_46_Nil)) {
    t1746 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1746) && u_is_con(s1745, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1745, 0), 0);
    u_value v_i = u_field(u_field(s1745, 0), 1);
    u_value v_rest = u_field(s1745, 1);
    t1746 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1746) {
    u_no_case(s1745);
  }
  return t1746;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1747 = v_p;
  u_value t1748 = NULL;
  if ((!t1748) && (u_is_con(s1747, &n_syntax_46_PVar) && u_is_con(u_field(s1747, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1747, 0), 0);
    t1748 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1748) && u_is_con(s1747, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1747, 1);
    u_value t1749 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1750 = &t1749;
    u_value l1751 = v_ps;
    while (u_is_con(l1751, &n_list_46_Cons)) {
      u_value v_q = u_field(l1751, 0);
      if (u_truth(u_mk_bool(1))) {
        e1750 = u_append(e1750, f_compile_46_pattern_names_1(v_q));
      }
      l1751 = u_field(l1751, 1);
    }
    t1748 = f_list_46_concat_1(t1749);
  }
  if ((!t1748) && u_is_con(s1747, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1747, 0);
    u_value t1752 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1753 = &t1752;
    u_value l1754 = v_ps;
    while (u_is_con(l1754, &n_list_46_Cons)) {
      u_value v_q = u_field(l1754, 0);
      if (u_truth(u_mk_bool(1))) {
        e1753 = u_append(e1753, f_compile_46_pattern_names_1(v_q));
      }
      l1754 = u_field(l1754, 1);
    }
    t1748 = f_list_46_concat_1(t1752);
  }
  if (!t1748) {
    t1748 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1748) {
    u_no_case(s1747);
  }
  return t1748;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1755 = v_e;
  u_value t1756 = NULL;
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_ELit)) {
    t1756 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1756) && (u_is_con(s1755, &n_syntax_46_EVar) && u_is_con(u_field(s1755, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1755, 0), 0);
    u_value t1757;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1757 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1757 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1756 = t1757;
  }
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1755, 0);
    u_value v_args = u_field(s1755, 1);
    u_value t1758 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1759 = &t1758;
    u_value l1760 = v_args;
    while (u_is_con(l1760, &n_list_46_Cons)) {
      u_value v_a = u_field(l1760, 0);
      if (u_truth(u_mk_bool(1))) {
        e1759 = u_append(e1759, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1760 = u_field(l1760, 1);
    }
    t1756 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1758));
  }
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1755, 0);
    u_value v_b = u_field(s1755, 1);
    t1756 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1755, 0);
    u_value v_b = u_field(s1755, 1);
    u_value t1761 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1762 = &t1761;
    u_value l1763 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1763, &n_list_46_Cons)) {
      u_value v_a = u_field(l1763, 0);
      if (u_truth(u_mk_bool(1))) {
        e1762 = u_append(e1762, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1763 = u_field(l1763, 1);
    }
    t1756 = f_list_46__43__43__2(f_list_46_concat_1(t1761), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1755, 0);
    u_value v_l = u_field(s1755, 1);
    u_value v_r = u_field(s1755, 2);
    t1756 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1755, 0);
    u_value v_cs = u_field(s1755, 1);
    u_value t1764 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1765 = &t1764;
    u_value l1766 = v_cs;
    while (u_is_con(l1766, &n_list_46_Cons)) {
      u_value v_b = u_field(l1766, 0);
      if (u_truth(u_mk_bool(1))) {
        e1765 = u_append(e1765, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1766 = u_field(l1766, 1);
    }
    t1756 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1764));
  }
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1755, 0);
    u_value v_xs = u_field(s1755, 1);
    u_value v_ls = u_field(s1755, 2);
    u_value v_c = u_field(s1755, 3);
    u_value t1767 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1768 = &t1767;
    u_value l1769 = v_ls;
    while (u_is_con(l1769, &n_list_46_Cons)) {
      u_value v_l = u_field(l1769, 0);
      if (u_truth(u_mk_bool(1))) {
        e1768 = u_append(e1768, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1769 = u_field(l1769, 1);
    }
    t1756 = f_list_46__43__43__2(f_list_46_concat_1(t1767), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1756) && u_is_con(s1755, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1755, 0);
    u_value t1770 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1771 = &t1770;
    u_value l1772 = v_es;
    while (u_is_con(l1772, &n_list_46_Cons)) {
      u_value v_a = u_field(l1772, 0);
      if (u_truth(u_mk_bool(1))) {
        e1771 = u_append(e1771, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1772 = u_field(l1772, 1);
    }
    t1756 = f_list_46_concat_1(t1770);
  }
  if (!t1756) {
    u_no_case(s1755);
  }
  return t1756;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1773 = v_b;
  u_value t1774 = NULL;
  if ((!t1774) && (u_is_con(s1773, &n_syntax_46_Branch) && u_is_con(u_field(s1773, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1773, 0);
    u_value v_g = u_field(u_field(s1773, 1), 0);
    u_value v_e = u_field(s1773, 2);
    t1774 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1774) && (u_is_con(s1773, &n_syntax_46_Branch) && u_is_con(u_field(s1773, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1773, 0);
    u_value v_e = u_field(s1773, 2);
    t1774 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1774) {
    u_no_case(s1773);
  }
  return t1774;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1775 = v_xss;
  u_value t1776 = NULL;
  if ((!t1776) && u_is_con(s1775, &n_list_46_Nil)) {
    t1776 = v_b;
  }
  if ((!t1776) && u_is_con(s1775, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1775, 0);
    u_value v_rest = u_field(s1775, 1);
    t1776 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1776) {
    u_no_case(s1775);
  }
  return t1776;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1777 = *r_g;
    int d1778 = 0;
    if ((!d1778) && u_is_con(s1777, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1777, 0);
      u_value v_ds = u_field(s1777, 1);
      d1778 = 1;
      {
        u_value t1779 = u_concat(v_stem, u_str_show(v_n));
        u_value t1780 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1779;
        *r_g = t1780;
      }
    }
    if (!d1778) {
      u_no_case(s1777);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1781 = *r_g;
    int d1782 = 0;
    if ((!d1782) && u_is_con(s1781, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1781, 0);
      u_value v_ds = u_field(s1781, 1);
      d1782 = 1;
      {
        u_value t1783 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1783;
      }
    }
    if (!d1782) {
      u_no_case(s1781);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1784 = U_LIT("u_mk_closure");
        static struct u_object l_w__1785 = U_LIT("w_");
        u_value t1786 = f_c_46_c_call_2(&l_u_mk_closure_1784, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1785, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1786;
      }
    } else {
      {
        u_value s1787 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1788 = 0;
        if ((!d1788) && (u_is_con(s1787, &n_option_46_Some) && u_equals(u_field(s1787, 0), u_mk_int(0)))) {
          d1788 = 1;
          {
            static struct u_object l_u_mk_con_1789 = U_LIT("u_mk_con");
            u_value t1790 = f_c_46_c_call_2(&l_u_mk_con_1789, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1790;
          }
        }
        if ((!d1788) && u_is_con(s1787, &n_option_46_Some)) {
          d1788 = 1;
          {
            static struct u_object l_u_mk_closure_1791 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1792 = U_LIT("wc_");
            u_value t1793 = f_c_46_c_call_2(&l_u_mk_closure_1791, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1792, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1793;
          }
        }
        if ((!d1788) && u_is_con(s1787, &n_option_46_None)) {
          d1788 = 1;
          {
            u_value s1794 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1795 = 0;
            if ((!d1795) && u_is_con(s1794, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1794, 0), 0);
              d1795 = 1;
              {
                static struct u_object l_u_mk_closure_1796 = U_LIT("u_mk_closure");
                static struct u_object l_w_1797 = U_LIT("w");
                u_value t1798 = f_c_46_c_call_2(&l_u_mk_closure_1796, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1797, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1798;
              }
            }
            if ((!d1795) && u_is_con(s1794, &n_option_46_None)) {
              d1795 = 1;
              {
                static struct u_object l_the_32_variable_32__1799 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1799, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1795) {
              u_no_case(s1794);
            }
          }
        }
        if (!d1788) {
          u_no_case(s1787);
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
    u_value s1800 = v_e;
    int d1801 = 0;
    if ((!d1801) && (u_is_con(s1800, &n_syntax_46_ELit) && u_is_con(u_field(s1800, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1800, 0), 0);
      d1801 = 1;
      {
        static struct u_object l_l__1802 = U_LIT("l_");
        static struct u_object l___1803 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1802, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), u_mk_int(16)))), &l___1803)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1804 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1805 = U_LIT("U_LIT");
        static struct u_object l__38__1806 = U_LIT("&");
        u_value t1807 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1804), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1805, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1808 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1806, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1807;
        *r_r = t1808;
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1800, 0);
      d1801 = 1;
      {
        u_value t1809 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1810 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1809;
        *r_r = t1810;
      }
    }
    if ((!d1801) && (u_is_con(s1800, &n_syntax_46_EVar) && u_is_con(u_field(s1800, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1800, 0), 0);
      u_value v_ty = u_field(u_field(s1800, 0), 1);
      d1801 = 1;
      {
        u_value s1811 = f_list_46_lookup_2(v_n, v_env);
        int d1812 = 0;
        if ((!d1812) && u_is_con(s1811, &n_option_46_Some)) {
          u_value v_c = u_field(s1811, 0);
          d1812 = 1;
          {
            u_value t1813 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1814 = v_c;
            *r_ss = t1813;
            *r_r = t1814;
          }
        }
        if ((!d1812) && u_is_con(s1811, &n_option_46_None)) {
          d1812 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1815 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1815;
          }
        }
        if (!d1812) {
          u_no_case(s1811);
        }
      }
    }
    if ((!d1801) && (u_is_con(s1800, &n_syntax_46_EApp) && (u_is_con(u_field(s1800, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1800, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1800, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1800, 0), 0), 1);
      u_value v_args = u_field(s1800, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1801 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1800, 0);
      u_value v_args = u_field(s1800, 1);
      d1801 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1816 = U_LIT("u_apply");
        u_value t1817 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1818 = f_c_46_c_call_2(&l_u_apply_1816, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1817;
        *r_r = t1818;
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1800, 0);
      u_value v_b = u_field(s1800, 1);
      d1801 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1800, 0);
      u_value v_b = u_field(s1800, 1);
      d1801 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1819 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1819, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1820 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1820;
        u_value t1821 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1821;
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1800, 0);
      u_value v_l = u_field(s1800, 1);
      u_value v_rr = u_field(s1800, 2);
      d1801 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1822 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1822, &v_t, r_g);
        u_value t1823 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1823;
        u_value t1824 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1824;
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1800, 0);
      u_value v_cases = u_field(s1800, 1);
      d1801 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1825 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1825, &v_d, r_g);
        static struct u_object l_t_1826 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1826, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1827 = U_LIT("NULL");
        u_value t1828 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1827))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1828;
        static struct u_object l__33__1829 = U_LIT("!");
        static struct u_object l_u_no_case_1830 = U_LIT("u_no_case");
        u_value t1831 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1829, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1830, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1831;
        u_value t1832 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1832;
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1800, 0);
      u_value v_xs = u_field(s1800, 1);
      u_value v_lists = u_field(s1800, 2);
      u_value v_c = u_field(s1800, 3);
      d1801 = 1;
      {
        static struct u_object l_t_1833 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1833, &v_t, r_g);
        static struct u_object l_e_1834 = U_LIT("e");
        p_compile_46_gen_name(&l_e_1834, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_1835 = U_LIT("u_mk_con");
        static struct u_object l__38__1836 = U_LIT("&");
        u_value t1837 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_1835, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__1836, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t1837;
        u_value t1838 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1838;
      }
    }
    if ((!d1801) && u_is_con(s1800, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1800, 0);
      d1801 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_1839 = U_LIT("u_mk_tuple");
        u_value t1840 = f_c_46_c_call_2(&l_u_mk_tuple_1839, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t1840;
      }
    }
    if (!d1801) {
      u_no_case(s1800);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s1841 = v_es;
    int d1842 = 0;
    if ((!d1842) && u_is_con(s1841, &n_list_46_Nil)) {
      d1842 = 1;
      {
        u_value t1843 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1844 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1843;
        *r_rs = t1844;
      }
    }
    if ((!d1842) && u_is_con(s1841, &n_list_46_Cons)) {
      u_value v_e = u_field(s1841, 0);
      u_value v_rest = u_field(s1841, 1);
      d1842 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t1845 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1846 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t1845;
        *r_rs = t1846;
      }
    }
    if (!d1842) {
      u_no_case(s1841);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s1847 = v_rs;
    int d1848 = 0;
    if ((!d1848) && u_is_con(s1847, &n_list_46_Nil)) {
      d1848 = 1;
      {
        u_value t1849 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1849;
      }
    }
    if ((!d1848) && u_is_con(s1847, &n_list_46_Cons)) {
      u_value v_r = u_field(s1847, 0);
      u_value v_rest = u_field(s1847, 1);
      d1848 = 1;
      {
        static struct u_object l_t_1850 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1850, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t1851 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t1851;
      }
    }
    if (!d1848) {
      u_no_case(s1847);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s1852 = u_mk_tuple(2, v_xs, v_ts);
  u_value t1853 = NULL;
  if ((!t1853) && (u_is_con(u_field(s1852, 0), &n_list_46_Cons) && (u_is_con(u_field(s1852, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1852, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s1852, 0), 0);
    u_value v_more = u_field(u_field(s1852, 0), 1);
    u_value v_t = u_field(u_field(u_field(s1852, 1), 0), 1);
    u_value v_rest = u_field(u_field(s1852, 1), 1);
    t1853 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t1853) {
    t1853 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1853) {
    u_no_case(s1852);
  }
  return t1853;
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
    u_value s1854 = v_args;
    int d1855 = 0;
    if ((!d1855) && (u_is_con(s1854, &n_list_46_Cons) && (u_is_con(u_field(s1854, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1854, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s1854, 0);
      u_value v_b = u_field(u_field(s1854, 1), 0);
      static struct u_object l_and_1862 = U_LIT("and");
      u_value t1864 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1862));
      if (!u_truth(t1864)) {
        static struct u_object l_or_1863 = U_LIT("or");
        t1864 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_1863));
      }
      if (u_truth(t1864)) {
        d1855 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_1856 = U_LIT("t");
          p_compile_46_gen_name(&l_t_1856, &v_t, r_g);
          static struct u_object l_and_1857 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1857)))) {
            {
              u_value t1858 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1858;
            }
          } else {
            {
              static struct u_object l__33__1859 = U_LIT("!");
              u_value t1860 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1859, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1860;
            }
          }
          u_value t1861 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t1861;
        }
      }
    }
    if (!d1855) {
      d1855 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t1865 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t1865)) {
          t1865 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t1865)) {
          {
            u_value t1866 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t1866;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_1867 = U_LIT("u_mk_con");
              u_value t1868 = f_c_46_c_call_2(&l_u_mk_con_1867, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t1868;
            }
          } else {
            {
              u_value s1869 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d1870 = 0;
              if ((!d1870) && u_is_con(s1869, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s1869, 0), 0);
                u_value v_k = u_field(u_field(s1869, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d1870 = 1;
                  {
                    u_value t1871 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t1871;
                  }
                }
              }
              if (!d1870) {
                d1870 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_1872 = U_LIT("u_apply");
                  u_value t1873 = f_c_46_c_call_2(&l_u_apply_1872, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t1873;
                }
              }
              if (!d1870) {
                u_no_case(s1869);
              }
            }
          }
        }
      }
    }
    if (!d1855) {
      u_no_case(s1854);
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
    u_value t1874 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1875 = &t1874;
    u_value l1876 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l1876, &n_list_46_Cons)) {
      u_value v_x = u_field(l1876, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e1875 = u_append(e1875, v_x);
      }
      l1876 = u_field(l1876, 1);
    }
    u_value t1877 = t1874;
    v_caps = t1877;
    static struct u_object l_lam_1878 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_1878, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_1879 = U_LIT("env");
    static struct u_object l_args_1880 = U_LIT("args");
    u_value t1881 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_1879), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_1880));
    v_decls = t1881;
    static struct u_object l_env_1882 = U_LIT("env");
    static struct u_object l_args_1883 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_1882), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_1883), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_1884 = U_LIT("u_mk_closure");
    u_value t1885 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1886 = &t1885;
    u_value l1887 = v_caps;
    while (u_is_con(l1887, &n_list_46_Cons)) {
      u_value v_x = u_field(l1887, 0);
      if (u_truth(u_mk_bool(1))) {
        e1886 = u_append(e1886, f_compile_46_var_expr_2(v_env, v_x));
      }
      l1887 = u_field(l1887, 1);
    }
    u_value t1888 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1889 = f_c_46_c_call_2(&l_u_mk_closure_1884, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t1885));
    *r_ss = t1888;
    *r_r = t1889;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s1890 = v_xs;
  u_value t1891 = NULL;
  if ((!t1891) && u_is_con(s1890, &n_list_46_Nil)) {
    t1891 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1891) && u_is_con(s1890, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s1890, 0), 0);
    u_value v_i = u_field(u_field(s1890, 0), 1);
    u_value v_rest = u_field(s1890, 1);
    t1891 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t1891) {
    u_no_case(s1890);
  }
  return t1891;
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
    u_value s1892 = v_cases;
    int d1893 = 0;
    if ((!d1893) && u_is_con(s1892, &n_list_46_Nil)) {
      d1893 = 1;
      {
        u_value t1894 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1894;
      }
    }
    if ((!d1893) && (u_is_con(s1892, &n_list_46_Cons) && u_is_con(u_field(s1892, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1892, 0), 0);
      u_value v_guard = u_field(u_field(s1892, 0), 1);
      u_value v_e = u_field(u_field(s1892, 0), 2);
      u_value v_more = u_field(s1892, 1);
      d1893 = 1;
      {
        u_value t1895 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t1895;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t1896 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t1896;
        u_value s1897 = v_guard;
        int d1898 = 0;
        if ((!d1898) && u_is_con(s1897, &n_option_46_Some)) {
          u_value v_c = u_field(s1897, 0);
          d1898 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t1899 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t1899;
          }
        }
        if ((!d1898) && u_is_con(s1897, &n_option_46_None)) {
          d1898 = 1;
          {
          }
        }
        if (!d1898) {
          u_no_case(s1897);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__1900 = U_LIT("!");
        u_value t1901 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1900, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t1901;
      }
    }
    if (!d1893) {
      u_no_case(s1892);
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
    u_value s1902 = u_mk_tuple(2, v_xs, v_lists);
    int d1903 = 0;
    if ((!d1903) && (u_is_con(u_field(s1902, 0), &n_list_46_Cons) && u_is_con(u_field(s1902, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s1902, 0), 0);
      u_value v_more = u_field(u_field(s1902, 0), 1);
      u_value v_list = u_field(u_field(s1902, 1), 0);
      u_value v_others = u_field(u_field(s1902, 1), 1);
      d1903 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_1904 = U_LIT("l");
        p_compile_46_gen_name(&l_l_1904, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t1905 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1905;
        static struct u_object l_u_is_con_1906 = U_LIT("u_is_con");
        u_value t1907 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_1906, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1907;
      }
    }
    if (!d1903) {
      d1903 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_1908 = U_LIT("u_append");
        u_value t1909 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_1908, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1909;
      }
    }
    if (!d1903) {
      u_no_case(s1902);
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
    u_value s1910 = v_s;
    int d1911 = 0;
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SSkip)) {
      d1911 = 1;
      {
        u_value t1912 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1912;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s1910, 0);
      d1911 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1913 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t1913;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s1910, 0);
      u_value v_body = u_field(s1910, 1);
      d1911 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t1914 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1915 = &t1914;
        u_value l1916 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l1916, &n_list_46_Cons)) {
          u_value v_x = u_field(l1916, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_1917 = U_LIT("NULL");
            e1915 = u_append(e1915, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1917))));
          }
          l1916 = u_field(l1916, 1);
        }
        u_value t1918 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t1914, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t1918;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s1910, 0);
      u_value v_l = u_field(s1910, 1);
      u_value v_r = u_field(s1910, 2);
      d1911 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t1919 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1919;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s1910, 0);
      u_value v_b = u_field(s1910, 1);
      d1911 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_1920 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1920, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t1921 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1921;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s1910, 0);
      u_value v_ins = u_field(s1910, 1);
      u_value v_outs = u_field(s1910, 2);
      u_value v_refs = u_field(s1910, 3);
      d1911 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t1922 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1923 = &t1922;
        u_value l1924 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l1924, &n_list_46_Cons)) {
          u_value v_x = u_field(l1924, 0);
          if (u_truth(u_mk_bool(1))) {
            e1923 = u_append(e1923, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l1924 = u_field(l1924, 1);
        }
        u_value t1925 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t1922))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1925;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s1910, 0);
      u_value v_cases = u_field(s1910, 1);
      d1911 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1926 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1926, &v_d, r_g);
        static struct u_object l_d_1927 = U_LIT("d");
        p_compile_46_gen_name(&l_d_1927, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t1928 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1928;
        static struct u_object l__33__1929 = U_LIT("!");
        static struct u_object l_u_no_case_1930 = U_LIT("u_no_case");
        u_value t1931 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1929, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1930, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1931;
      }
    }
    if ((!d1911) && (u_is_con(s1910, &n_syntax_46_SAssert) && (u_is_con(u_field(s1910, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s1910, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s1910, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s1910, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s1910, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s1910, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s1910, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1910, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s1910, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s1910, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s1910, 0), 1), 1), 0);
      d1911 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_1932 = U_LIT("u_assert_equal");
        u_value t1933 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_1932, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1933;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s1910, 0);
      d1911 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_1934 = U_LIT("u_assert_true");
        u_value t1935 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_1934, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1935;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s1910, 0);
      u_value v_ty = u_field(s1910, 1);
      d1911 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_1936 = U_LIT("u_raise");
        u_value t1937 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_1936, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1937;
      }
    }
    if ((!d1911) && u_is_con(s1910, &n_syntax_46_STry)) {
      u_value v_body = u_field(s1910, 0);
      u_value v_hs = u_field(s1910, 1);
      d1911 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_1938 = U_LIT("h");
        p_compile_46_gen_name(&l_h_1938, &v_d, r_g);
        static struct u_object l_d_1939 = U_LIT("d");
        p_compile_46_gen_name(&l_d_1939, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_1940 = U_LIT("u_pop");
        static struct u_object l__33__1941 = U_LIT("!");
        static struct u_object l_u_reraise_1942 = U_LIT("u_reraise");
        u_value t1943 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_1940, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1941, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_1942, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t1943;
        static struct u_object l_struct_32_u_handler_1944 = U_LIT("struct u_handler");
        static struct u_object l_u_push_1945 = U_LIT("u_push");
        static struct u_object l__61__61__1946 = U_LIT("==");
        static struct u_object l_setjmp_1947 = U_LIT("setjmp");
        static struct u_object l_env_1948 = U_LIT("env");
        static struct u_object l_u_pop_1949 = U_LIT("u_pop");
        u_value t1950 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_1944)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_1945, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__1946, f_c_46_c_call_2(&l_setjmp_1947, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_1948), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_1949, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t1950;
      }
    }
    if (!d1911) {
      u_no_case(s1910);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s1951 = v_xs;
    int d1952 = 0;
    if ((!d1952) && u_is_con(s1951, &n_list_46_Nil)) {
      d1952 = 1;
      {
        u_value t1953 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1953;
      }
    }
    if ((!d1952) && u_is_con(s1951, &n_list_46_Cons)) {
      u_value v_x = u_field(s1951, 0);
      u_value v_rest = u_field(s1951, 1);
      d1952 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t1954 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t1954;
      }
    }
    if (!d1952) {
      u_no_case(s1951);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s1955 = u_mk_tuple(2, v_xs, v_ts);
  u_value t1956 = NULL;
  if ((!t1956) && (u_is_con(u_field(s1955, 0), &n_list_46_Cons) && (u_is_con(u_field(s1955, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1955, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s1955, 0), 0);
    u_value v_more = u_field(u_field(s1955, 0), 1);
    u_value v_t = u_field(u_field(u_field(s1955, 1), 0), 1);
    u_value v_rest = u_field(u_field(s1955, 1), 1);
    t1956 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t1956) {
    t1956 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1956) {
    u_no_case(s1955);
  }
  return t1956;
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
    static struct u_object l_u_caught_1957 = U_LIT("u_caught");
    u_value t1958 = f_c_46_c_call_2(&l_u_caught_1957, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t1958;
    u_value s1959 = v_hs;
    int d1960 = 0;
    if ((!d1960) && u_is_con(s1959, &n_list_46_Nil)) {
      d1960 = 1;
      {
        u_value t1961 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1961;
      }
    }
    if ((!d1960) && (u_is_con(s1959, &n_list_46_Cons) && u_is_con(u_field(s1959, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s1959, 0), 0);
      u_value v_p = u_field(u_field(s1959, 0), 1);
      u_value v_guard = u_field(u_field(s1959, 0), 2);
      u_value v_s = u_field(u_field(s1959, 0), 3);
      u_value v_more = u_field(s1959, 1);
      d1960 = 1;
      {
        u_value t1962 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t1962;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t1963 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t1963;
        u_value s1964 = v_guard;
        int d1965 = 0;
        if ((!d1965) && u_is_con(s1964, &n_option_46_Some)) {
          u_value v_c = u_field(s1964, 0);
          d1965 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t1966 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t1966;
          }
        }
        if ((!d1965) && u_is_con(s1964, &n_option_46_None)) {
          d1965 = 1;
          {
          }
        }
        if (!d1965) {
          u_no_case(s1964);
        }
        u_value s1967 = v_p;
        int d1968 = 0;
        if ((!d1968) && u_is_con(s1967, &n_syntax_46_PWild)) {
          d1968 = 1;
          {
            u_value t1969 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t1969;
          }
        }
        if (!d1968) {
          d1968 = 1;
          {
            static struct u_object l_u_has_type_1970 = U_LIT("u_has_type");
            u_value t1971 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_1970, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t1971;
          }
        }
        if (!d1968) {
          u_no_case(s1967);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__1972 = U_LIT("!");
        u_value t1973 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1972, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t1973;
      }
    }
    if (!d1960) {
      u_no_case(s1959);
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
    u_value s1974 = v_cases;
    int d1975 = 0;
    if ((!d1975) && u_is_con(s1974, &n_list_46_Nil)) {
      d1975 = 1;
      {
        u_value t1976 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1976;
      }
    }
    if ((!d1975) && (u_is_con(s1974, &n_list_46_Cons) && u_is_con(u_field(s1974, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1974, 0), 0);
      u_value v_guard = u_field(u_field(s1974, 0), 1);
      u_value v_s = u_field(u_field(s1974, 0), 2);
      u_value v_more = u_field(s1974, 1);
      d1975 = 1;
      {
        u_value t1977 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t1977;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t1978 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t1978;
        u_value s1979 = v_guard;
        int d1980 = 0;
        if ((!d1980) && u_is_con(s1979, &n_option_46_Some)) {
          u_value v_c = u_field(s1979, 0);
          d1980 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t1981 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t1981;
          }
        }
        if ((!d1980) && u_is_con(s1979, &n_option_46_None)) {
          d1980 = 1;
          {
          }
        }
        if (!d1980) {
          u_no_case(s1979);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__1982 = U_LIT("!");
        u_value t1983 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1982, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t1983;
      }
    }
    if (!d1975) {
      u_no_case(s1974);
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
    u_value s1984 = v_c;
    int d1985 = 0;
    if ((!d1985) && u_is_con(s1984, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s1984, 0);
      u_value v_xss = u_field(s1984, 2);
      u_value v_b = u_field(s1984, 4);
      d1985 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t1986 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1987 = &t1986;
        u_value l1988 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l1988, &n_list_46_Cons)) {
          u_value v_x = u_field(l1988, 0);
          if (u_truth(u_mk_bool(1))) {
            e1987 = u_append(e1987, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l1988 = u_field(l1988, 1);
        }
        u_value t1989 = t1986;
        v_params = t1989;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t1990 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t1990;
          }
        } else {
          {
            u_value t1991 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t1991;
          }
        }
        u_value t1992 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t1993 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t1992;
        *r_ms = t1993;
      }
    }
    if ((!d1985) && u_is_con(s1984, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s1984, 0);
      u_value v_ins = u_field(s1984, 2);
      u_value v_outs = u_field(s1984, 3);
      u_value v_refs = u_field(s1984, 4);
      u_value v_body = u_field(s1984, 5);
      d1985 = 1;
      {
        u_value t1994 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1995 = &t1994;
        u_value l1996 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l1996, &n_list_46_Cons)) {
          u_value v_x = u_field(l1996, 0);
          if (u_truth(u_mk_bool(1))) {
            e1995 = u_append(e1995, f_compile_46_pointer_1(v_x));
          }
          l1996 = u_field(l1996, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t1994), v_body, &v_bs, r_g);
        u_value t1997 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1998 = &t1997;
        u_value l1999 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l1999, &n_list_46_Cons)) {
          u_value v_x = u_field(l1999, 0);
          if (u_truth(u_mk_bool(1))) {
            e1998 = u_append(e1998, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l1999 = u_field(l1999, 1);
        }
        u_value t2000 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2001 = &t2000;
        u_value l2002 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2002, &n_list_46_Cons)) {
          u_value v_x = u_field(l2002, 0);
          if (u_truth(u_mk_bool(1))) {
            e2001 = u_append(e2001, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2002 = u_field(l2002, 1);
        }
        u_value t2003 = f_list_46__43__43__2(t1997, t2000);
        v_params = t2003;
        u_value t2004 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2005 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2004;
        *r_ms = t2005;
      }
    }
    if ((!d1985) && u_is_con(s1984, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s1984, 0);
      d1985 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2006 = U_LIT("u_print_line");
        u_value t2007 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2008 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2006, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2007;
        *r_ms = t2008;
      }
    }
    if ((!d1985) && (u_is_con(s1984, &n_syntax_46_CRun) && u_is_con(u_field(s1984, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s1984, 1), 0);
      u_value v_body = u_field(u_field(s1984, 1), 1);
      d1985 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2009 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2010 = &t2009;
        u_value l2011 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2011, &n_list_46_Cons)) {
          u_value v_x = u_field(l2011, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2012 = U_LIT("NULL");
            e2010 = u_append(e2010, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2012))));
          }
          l2011 = u_field(l2011, 1);
        }
        u_value t2013 = t2009;
        v_copy = t2013;
        u_value t2014 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2015 = &t2014;
        u_value l2016 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2016, &n_list_46_Cons)) {
          u_value v_x = u_field(l2016, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2017 = U_LIT("u_print_binding");
            e2015 = u_append(e2015, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2017, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2016 = u_field(l2016, 1);
        }
        u_value t2018 = t2014;
        v_back = t2018;
        u_value t2019 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2020 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2019;
        *r_ms = t2020;
      }
    }
    if ((!d1985) && u_is_con(s1984, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s1984, 1);
      d1985 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2021 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2022 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2021;
        *r_ms = t2022;
      }
    }
    if ((!d1985) && u_is_con(s1984, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s1984, 1);
      d1985 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2023 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2024 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2023;
        *r_ms = t2024;
      }
    }
    if (!d1985) {
      d1985 = 1;
      {
        u_value t2025 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2026 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2025;
        *r_ms = t2026;
      }
    }
    if (!d1985) {
      u_no_case(s1984);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2027 = v_cs;
    int d2028 = 0;
    if ((!d2028) && u_is_con(s2027, &n_list_46_Nil)) {
      d2028 = 1;
      {
        u_value t2029 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2030 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2029;
        *r_ms = t2030;
      }
    }
    if ((!d2028) && u_is_con(s2027, &n_list_46_Cons)) {
      u_value v_c = u_field(s2027, 0);
      u_value v_rest = u_field(s2027, 1);
      d2028 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2031 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2032 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2031;
        *r_ms = t2032;
      }
    }
    if (!d2028) {
      u_no_case(s2027);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_static_32_u_value_2033 = U_LIT("static u_value");
  static struct u_object l_cache_2034 = U_LIT("cache");
  static struct u_object l_NULL_2035 = U_LIT("NULL");
  static struct u_object l__61__61__2036 = U_LIT("==");
  static struct u_object l_cache_2037 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2038 = U_LIT("U_COMPUTING");
  static struct u_object l_u_fail_2039 = U_LIT("u_fail");
  static struct u_object l_recursive_32_initia_2040 = U_LIT("recursive initialization of ");
  static struct u_object l_cache_2041 = U_LIT("cache");
  static struct u_object l_cache_2042 = U_LIT("cache");
  static struct u_object l_cache_2043 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2044 = U_LIT("U_COMPUTING");
  static struct u_object l_cache_2045 = U_LIT("cache");
  static struct u_object l_cache_2046 = U_LIT("cache");
  return f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2033), &l_cache_2034, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2035))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2036, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2037), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2038)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2039, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2040, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2041), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2042))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2043), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2044)), u_mk_con(&n_list_46_Nil, 0))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2045), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2046))), u_mk_con(&n_list_46_Nil, 0)))));
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2047 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2048 = &t2047;
  u_value l2049 = f_compile_46_funs_of_1(v_gl);
  while (u_is_con(l2049, &n_list_46_Cons)) {
    u_value v_p = u_field(l2049, 0);
    if (u_truth(u_mk_bool(1))) {
      e2048 = u_append(e2048, f_compile_46_fun_wrapper_1(v_p));
    }
    l2049 = u_field(l2049, 1);
  }
  u_value t2050 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2051 = &t2050;
  u_value l2052 = f_compile_46_constrs_of_1(v_gl);
  while (u_is_con(l2052, &n_list_46_Cons)) {
    u_value v_p = u_field(l2052, 0);
    if (u_truth(u_mk_bool(1))) {
      e2051 = u_append(e2051, f_compile_46_constr_wrapper_1(v_p));
    }
    l2052 = u_field(l2052, 1);
  }
  u_value t2053 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2054 = &t2053;
  u_value l2055 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2055, &n_list_46_Cons)) {
    u_value v_p = u_field(l2055, 0);
    if (u_truth(u_mk_bool(1))) {
      e2054 = u_append(e2054, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2055 = u_field(l2055, 1);
  }
  return f_list_46__43__43__2(t2047, f_list_46__43__43__2(f_list_46_concat_1(t2050), t2053));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2056 = v_p;
  u_value t2057 = NULL;
  if (!t2057) {
    u_value v_n = u_field(s2056, 0);
    u_value v_k = u_field(s2056, 1);
    static struct u_object l_w__2058 = U_LIT("w_");
    t2057 = f_compile_46_wrapper_3(u_concat(&l_w__2058, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2057) {
    u_no_case(s2056);
  }
  return t2057;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2059 = v_p;
  u_value t2060 = NULL;
  if (!t2060) {
    u_value v_f = u_field(s2059, 0);
    u_value v_k = u_field(s2059, 1);
    static struct u_object l_w_2061 = U_LIT("w");
    t2060 = f_compile_46_wrapper_3(u_concat(&l_w_2061, v_f), v_f, v_k);
  }
  if (!t2060) {
    u_no_case(s2059);
  }
  return t2060;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2062 = U_LIT("env");
  static struct u_object l_args_2063 = U_LIT("args");
  u_value t2064 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2065 = &t2064;
  u_value l2066 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2066, &n_list_46_Cons)) {
    u_value v_i = u_field(l2066, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2067 = U_LIT("args");
      e2065 = u_append(e2065, f_compile_46_index_2(&l_args_2067, v_i));
    }
    l2066 = u_field(l2066, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2062), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2063), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2064))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2068 = v_p;
  u_value t2069 = NULL;
  if ((!t2069) && u_equals(u_field(s2068, 1), u_mk_int(0))) {
    t2069 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2069) {
    u_value v_n = u_field(s2068, 0);
    u_value v_k = u_field(s2068, 1);
    static struct u_object l_wc__2070 = U_LIT("wc_");
    static struct u_object l_env_2071 = U_LIT("env");
    static struct u_object l_args_2072 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2073 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2074 = U_LIT("args");
    t2069 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2070, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2071), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2072), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2073, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2074), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2069) {
    u_no_case(s2068);
  }
  return t2069;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2075;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2075 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2075 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2075;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2076 = v_d;
  u_value t2077 = NULL;
  if ((!t2077) && u_is_con(s2076, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2076, 0);
    u_value v_f = u_field(s2076, 1);
    u_value v_ps = u_field(s2076, 2);
    t2077 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2077) {
    t2077 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2077) {
    u_no_case(s2076);
  }
  return t2077;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2078 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2078;
    u_value t2079 = f_compile_46_globals_1(f_compile_46_program_cmds_1(v_q));
    v_gl = t2079;
    p_compile_46_compile_cmds(v_gl, f_compile_46_program_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2080 = v_g;
    int d2081 = 0;
    if ((!d2081) && u_is_con(s2080, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2080, 1);
      d2081 = 1;
      {
        u_value t2082 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2082;
      }
    }
    if (!d2081) {
      u_no_case(s2080);
    }
    static struct u_object l_ulang_46_h_2083 = U_LIT("ulang.h");
    u_value t2084 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2085 = &t2084;
    u_value l2086 = v_fs;
    while (u_is_con(l2086, &n_list_46_Cons)) {
      u_value v_d = u_field(l2086, 0);
      if (u_truth(u_mk_bool(1))) {
        e2085 = u_append(e2085, f_compile_46_prototype_1(v_d));
      }
      l2086 = u_field(l2086, 1);
    }
    static struct u_object l_main_2087 = U_LIT("main");
    static struct u_object l_argc_2088 = U_LIT("argc");
    static struct u_object l_char_2089 = U_LIT("char");
    static struct u_object l_argv_2090 = U_LIT("argv");
    static struct u_object l_u_args_2091 = U_LIT("u_args");
    static struct u_object l_argc_2092 = U_LIT("argc");
    static struct u_object l_argv_2093 = U_LIT("argv");
    u_value t2094 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2083), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_compile_46_program_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2084), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2087, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2088), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2089))), &l_argv_2090), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2091, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2092), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2093), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2094;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2095 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2095, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2096 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2096, f_compile_46_program_cmds_1(v_pre)), f_compile_46_program_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam960(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2097 = v_args;
    int d2098 = 0;
    if ((!d2098) && u_is_con(s2097, &n_list_46_Nil)) {
      d2098 = 1;
      {
      }
    }
    if ((!d2098) && (u_is_con(s2097, &n_list_46_Cons) && (u_is_con(u_field(s2097, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2097, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2097, 0);
      u_value v_target = u_field(u_field(s2097, 1), 0);
      d2098 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2098) {
      d2098 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2099 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2099), "os.OsError");
      }
    }
    if (!d2098) {
      u_no_case(s2097);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
