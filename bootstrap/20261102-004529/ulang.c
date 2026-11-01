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

u_value w_f_parse_46_fields_of_1(u_value *env, u_value *args);

u_value w_f_parse_46_named_fields_4(u_value *env, u_value *args);

u_value w_f_parse_46_accessors_3(u_value *env, u_value *args);

u_value w_f_parse_46_first_of_1(u_value *env, u_value *args);

u_value w_f_parse_46_accessor_5(u_value *env, u_value *args);

u_value w_f_parse_46_field_type_1(u_value *env, u_value *args);

u_value w_f_parse_46_field_case_1(u_value *env, u_value *args);

u_value w_f_parse_46_missing_field_2(u_value *env, u_value *args);

u_value w_f_parse_46_positions_1(u_value *env, u_value *args);

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

void p_parse_46_constructor(u_value v_sc, u_value *r_k, u_value *r_fs, u_value *r_ts);

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts);

void p_parse_46_parse_data(u_value v_ns, u_value *r_cs, u_value *r_ts);

u_value f_parse_46_fields_of_1(u_value v_ks);

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i);

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks);

u_value f_parse_46_first_of_1(u_value v_p);

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n);

u_value f_parse_46_field_type_1(u_value v_es);

u_value f_parse_46_field_case_1(u_value v_e);

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f);

u_value f_parse_46_positions_1(u_value v_n);

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

u_value lam969(u_value *env, u_value *args);

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

u_value w_f_parse_46_fields_of_1(u_value *env, u_value *args) {
  return f_parse_46_fields_of_1(args[0]);
}

u_value w_f_parse_46_named_fields_4(u_value *env, u_value *args) {
  return f_parse_46_named_fields_4(args[0], args[1], args[2], args[3]);
}

u_value w_f_parse_46_accessors_3(u_value *env, u_value *args) {
  return f_parse_46_accessors_3(args[0], args[1], args[2]);
}

u_value w_f_parse_46_first_of_1(u_value *env, u_value *args) {
  return f_parse_46_first_of_1(args[0]);
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
    if ((!t236) && u_equals(s235, u_mk_lit("error"))) {
      t236 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), v_a)), u_mk_con(&n_list_46_Nil, 0));
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
    if ((!d575) && (u_is_con(s574, &n_syntax_46_EVar) && (u_is_con(u_field(s574, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s574, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s574, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s574, 0), 0), 1);
      u_value v_a = u_field(u_field(s574, 0), 1);
      u_value t576 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t576)) {
        t576 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t576)) {
        d575 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
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
            u_value t577 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t577;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t578 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t578;
          }
        }
      }
    }
    if (!d575) {
      d575 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t579 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t579;
      }
    }
    if (!d575) {
      u_no_case(s574);
    }
  }
}

void p_check_46_field_access(u_value v_env, u_value v_f, u_value v_a, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_arg1 = NULL;
    u_value v_u = NULL;
    u_value v_acc = NULL;
    u_value v_x = NULL;
    u_value s580 = v_args;
    int d581 = 0;
    if ((!d581) && (u_is_con(s580, &n_list_46_Cons) && u_is_con(u_field(s580, 1), &n_list_46_Nil))) {
      u_value v_arg = u_field(s580, 0);
      d581 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s582 = f_check_46_zonk_type_2(f_check_46_sub_of_1(*r_s), v_u);
        int d583 = 0;
        if ((!d583) && (u_is_con(s582, &n_syntax_46_TSort) && (u_is_con(u_field(s582, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s582, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s582, 0), 0), 0);
          u_value v_tn = u_field(u_field(s582, 0), 1);
          d583 = 1;
          {
            u_value t584 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t584;
          }
        }
        if (!d583) {
          u_value v_v = s582;
          d583 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d583) {
          u_no_case(s582);
        }
        u_value s585 = f_ctx_46_schemes_of_2(f_check_46_ctx_of_1(*r_s), v_acc);
        int d586 = 0;
        if ((!d586) && (u_is_con(s585, &n_list_46_Cons) && u_is_con(u_field(s585, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s585, 0);
          d586 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d586) {
          d586 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d586) {
          u_no_case(s585);
        }
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)), *r_t), r_s);
        u_value t587 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, u_mk_con(&n_list_46_Nil, 0)));
        *r_e1 = t587;
      }
    }
    if (!d581) {
      d581 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d581) {
      u_no_case(s580);
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
    u_value s588 = u_mk_tuple(2, v_es, v_ts);
    int d589 = 0;
    if ((!d589) && (u_is_con(u_field(s588, 0), &n_list_46_Cons) && u_is_con(u_field(s588, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s588, 0), 0);
      u_value v_more = u_field(u_field(s588, 0), 1);
      u_value v_t = u_field(u_field(s588, 1), 0);
      u_value v_ts1 = u_field(u_field(s588, 1), 1);
      d589 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t590 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t590;
      }
    }
    if ((!d589) && (u_is_con(u_field(s588, 0), &n_list_46_Nil) && u_is_con(u_field(s588, 1), &n_list_46_Nil))) {
      d589 = 1;
      {
        u_value t591 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t591;
      }
    }
    if (!d589) {
      d589 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d589) {
      u_no_case(s588);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s592 = v_es;
    int d593 = 0;
    if ((!d593) && u_is_con(s592, &n_list_46_Nil)) {
      d593 = 1;
      {
        u_value t594 = u_mk_con(&n_list_46_Nil, 0);
        u_value t595 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t594;
        *r_ts = t595;
      }
    }
    if ((!d593) && u_is_con(s592, &n_list_46_Cons)) {
      u_value v_e = u_field(s592, 0);
      u_value v_more = u_field(s592, 1);
      d593 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t596 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t597 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t596;
        *r_ts = t597;
      }
    }
    if (!d593) {
      u_no_case(s592);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s598 = u_mk_tuple(2, v_xs, v_lists);
    int d599 = 0;
    if ((!d599) && (u_is_con(u_field(s598, 0), &n_list_46_Cons) && u_is_con(u_field(s598, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s598, 0), 0);
      u_value v_ys = u_field(u_field(s598, 0), 1);
      u_value v_e = u_field(u_field(s598, 1), 0);
      u_value v_es = u_field(u_field(s598, 1), 1);
      d599 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t600 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t600;
      }
    }
    if (!d599) {
      d599 = 1;
      {
        u_value t601 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t601;
      }
    }
    if (!d599) {
      u_no_case(s598);
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
    u_value s602 = v_cases;
    int d603 = 0;
    if ((!d603) && u_is_con(s602, &n_list_46_Nil)) {
      d603 = 1;
      {
        u_value t604 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t604;
      }
    }
    if ((!d603) && (u_is_con(s602, &n_list_46_Cons) && u_is_con(u_field(s602, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s602, 0), 0);
      u_value v_g = u_field(u_field(s602, 0), 1);
      u_value v_b = u_field(u_field(s602, 0), 2);
      u_value v_more = u_field(s602, 1);
      d603 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t605 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t605;
      }
    }
    if (!d603) {
      u_no_case(s602);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s606 = v_g;
    int d607 = 0;
    if ((!d607) && u_is_con(s606, &n_option_46_Some)) {
      u_value v_e = u_field(s606, 0);
      d607 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t608 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t608;
      }
    }
    if ((!d607) && u_is_con(s606, &n_option_46_None)) {
      d607 = 1;
      {
        u_value t609 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t609;
      }
    }
    if (!d607) {
      u_no_case(s606);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t610 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e611 = &t610;
    u_value l612 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l612, &n_list_46_Cons)) {
      u_value v_p = u_field(l612, 0);
      if (u_truth(u_mk_bool(1))) {
        e611 = u_append(e611, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l612 = u_field(l612, 1);
    }
    u_value s613 = f_check_46_missing_4(f_check_46_ctx_of_1(v_s), f_check_46_sub_of_1(v_s), t610, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d614 = 0;
    if ((!d614) && (u_is_con(s613, &n_option_46_Some) && u_is_con(u_field(s613, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s613, 0), 0);
      d614 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d614) {
      d614 = 1;
      {
      }
    }
    if (!d614) {
      u_no_case(s613);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s615 = v_cs;
  u_value t616 = NULL;
  if ((!t616) && u_is_con(s615, &n_list_46_Nil)) {
    t616 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t616) && (u_is_con(s615, &n_list_46_Cons) && u_equals(u_field(u_field(s615, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s615, 0), 0);
    u_value v_rest = u_field(s615, 1);
    t616 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t616) && (u_is_con(s615, &n_list_46_Cons) && u_equals(u_field(u_field(s615, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s615, 1);
    t616 = f_check_46_unguarded_1(v_rest);
  }
  if (!t616) {
    u_no_case(s615);
  }
  return t616;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s617 = v_cs;
    int d618 = 0;
    if ((!d618) && u_is_con(s617, &n_list_46_Nil)) {
      d618 = 1;
      {
      }
    }
    if ((!d618) && u_is_con(s617, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s617, 0), 0);
      u_value v_open = u_field(u_field(s617, 0), 1);
      u_value v_rest = u_field(s617, 1);
      d618 = 1;
      {
        u_value t619 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e620 = &t619;
        u_value l621 = v_before;
        while (u_is_con(l621, &n_list_46_Cons)) {
          u_value v_q = u_field(l621, 0);
          if (u_truth(u_mk_bool(1))) {
            e620 = u_append(e620, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l621 = u_field(l621, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_ctx_of_1(v_s), f_check_46_sub_of_1(v_s), t619, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d618) {
      u_no_case(s617);
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
    u_value s622 = v_p;
    int d623 = 0;
    if ((!d623) && u_is_con(s622, &n_syntax_46_PWild)) {
      d623 = 1;
      {
        u_value t624 = v_p;
        u_value t625 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t624;
        *r_xs = t625;
      }
    }
    if ((!d623) && u_is_con(s622, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s622, 0);
      d623 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t626 = v_p;
        u_value t627 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t626;
        *r_xs = t627;
      }
    }
    if ((!d623) && (u_is_con(s622, &n_syntax_46_PVar) && u_is_con(u_field(s622, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s622, 0), 0);
      u_value v_a = u_field(u_field(s622, 0), 1);
      d623 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_ctx_of_1(*r_s), v_n)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t628 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t629 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t628;
            *r_xs = t629;
          }
        }
      }
    }
    if ((!d623) && u_is_con(s622, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s622, 0);
      u_value v_args = u_field(s622, 1);
      d623 = 1;
      {
        u_value s630 = f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_ctx_of_1(*r_s), v_n));
        int d631 = 0;
        if ((!d631) && (u_is_con(s630, &n_list_46_Cons) && u_is_con(u_field(s630, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s630, 0);
          d631 = 1;
          {
            u_value t632 = v_k1;
            v_k = t632;
          }
        }
        if ((!d631) && u_is_con(s630, &n_list_46_Nil)) {
          d631 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d631) {
          u_value v_ks = s630;
          d631 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d631) {
          u_no_case(s630);
        }
        u_value s633 = f_ctx_46_constr_of_2(f_check_46_ctx_of_1(*r_s), v_k);
        int d634 = 0;
        if ((!d634) && u_is_con(s633, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s633, 0), 0);
          u_value v_ks = u_field(u_field(s633, 0), 1);
          u_value v_r = u_field(u_field(s633, 0), 2);
          d634 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t635 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e636 = &t635;
            u_value l637 = v_ks;
            while (u_is_con(l637, &n_list_46_Cons)) {
              u_value v_kt = u_field(l637, 0);
              if (u_truth(u_mk_bool(1))) {
                e636 = u_append(e636, f_check_46_subst_2(v_kt, v_m));
              }
              l637 = u_field(l637, 1);
            }
            p_check_46_binder_list(v_args, t635, &v_qs, r_xs, r_s);
            u_value t638 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t638;
          }
        }
        if ((!d634) && u_is_con(s633, &n_option_46_None)) {
          d634 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d634) {
          u_no_case(s633);
        }
      }
    }
    if ((!d623) && u_is_con(s622, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s622, 0);
      d623 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t639 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t639;
      }
    }
    if (!d623) {
      u_no_case(s622);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s640 = u_mk_tuple(2, v_ps, v_ts);
    int d641 = 0;
    if ((!d641) && (u_is_con(u_field(s640, 0), &n_list_46_Cons) && u_is_con(u_field(s640, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s640, 0), 0);
      u_value v_more = u_field(u_field(s640, 0), 1);
      u_value v_t = u_field(u_field(s640, 1), 0);
      u_value v_ts1 = u_field(u_field(s640, 1), 1);
      d641 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t642 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t643 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t642;
        *r_xs = t643;
      }
    }
    if ((!d641) && (u_is_con(u_field(s640, 0), &n_list_46_Nil) && u_is_con(u_field(s640, 1), &n_list_46_Nil))) {
      d641 = 1;
      {
        u_value t644 = u_mk_con(&n_list_46_Nil, 0);
        u_value t645 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t644;
        *r_xs = t645;
      }
    }
    if (!d641) {
      d641 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d641) {
      u_no_case(s640);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s646 = v_x;
    int d647 = 0;
    if ((!d647) && u_is_con(s646, &n_syntax_46_Var)) {
      u_value v_n = u_field(s646, 0);
      u_value v_a = u_field(s646, 1);
      d647 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s648 = f_list_46_lookup_2(v_n, v_env);
        int d649 = 0;
        if ((!d649) && u_is_con(s648, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s648, 0), 1);
          d649 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t650 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t650;
          }
        }
        if ((!d649) && u_is_con(s648, &n_option_46_None)) {
          d649 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d649) {
          u_no_case(s648);
        }
      }
    }
    if (!d647) {
      u_no_case(s646);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s651 = v_xs;
    int d652 = 0;
    if ((!d652) && u_is_con(s651, &n_list_46_Nil)) {
      d652 = 1;
      {
        u_value t653 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t653;
      }
    }
    if ((!d652) && u_is_con(s651, &n_list_46_Cons)) {
      u_value v_x = u_field(s651, 0);
      u_value v_more = u_field(s651, 1);
      d652 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t654 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t654;
      }
    }
    if (!d652) {
      u_no_case(s651);
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
    u_value s655 = v_x;
    int d656 = 0;
    if ((!d656) && u_is_con(s655, &n_syntax_46_SSkip)) {
      d656 = 1;
      {
        u_value t657 = v_x;
        *r_y = t657;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s655, 0);
      d656 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t658 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t658;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s655, 0);
      u_value v_body = u_field(s655, 1);
      d656 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t659 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t659;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s655, 0);
      u_value v_left = u_field(s655, 1);
      u_value v_right = u_field(s655, 2);
      d656 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t660 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t660;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s655, 0);
      u_value v_body = u_field(s655, 1);
      d656 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t661 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t661;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s655, 0);
      u_value v_ins = u_field(s655, 1);
      u_value v_outs = u_field(s655, 2);
      u_value v_refs = u_field(s655, 3);
      d656 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s655, 0);
      u_value v_cases = u_field(s655, 1);
      d656 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t662 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e663 = &t662;
        u_value l664 = v_bs;
        while (u_is_con(l664, &n_list_46_Cons)) {
          u_value v_br = u_field(l664, 0);
          if (u_truth(u_mk_bool(1))) {
            e663 = u_append(e663, f_check_46_case_of_1(v_br));
          }
          l664 = u_field(l664, 1);
        }
        p_check_46_coverage(*r_s, v_t, t662);
        u_value t665 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t665;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s655, 0);
      d656 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t666 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t666;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s655, 0);
      u_value v_a = u_field(s655, 1);
      d656 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t667 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t667;
      }
    }
    if ((!d656) && u_is_con(s655, &n_syntax_46_STry)) {
      u_value v_body = u_field(s655, 0);
      u_value v_handlers = u_field(s655, 1);
      d656 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t668 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t668;
      }
    }
    if (!d656) {
      u_no_case(s655);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s669 = v_xs;
  u_value t670 = NULL;
  if ((!t670) && (u_is_con(s669, &n_list_46_Cons) && (u_is_con(u_field(s669, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s669, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s669, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s669, 0), 0), 0), 0);
    u_value v_rest = u_field(s669, 1);
    t670 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t670) {
    t670 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t670) {
    u_no_case(s669);
  }
  return t670;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s671 = v_xs;
    int d672 = 0;
    if ((!d672) && u_is_con(s671, &n_list_46_Nil)) {
      d672 = 1;
      {
      }
    }
    if ((!d672) && (u_is_con(s671, &n_list_46_Cons) && u_is_con(u_field(s671, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s671, 0), 0);
      u_value v_t = u_field(u_field(s671, 0), 1);
      u_value v_rest = u_field(s671, 1);
      d672 = 1;
      {
        u_value t673 = f_check_46_holes_1(v_t);
        if (u_truth(t673)) {
          t673 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t673)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d672) {
      u_no_case(s671);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s674 = v_xs;
    int d675 = 0;
    if ((!d675) && u_is_con(s674, &n_list_46_Nil)) {
      d675 = 1;
      {
        u_value t676 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t676;
      }
    }
    if ((!d675) && u_is_con(s674, &n_list_46_Cons)) {
      u_value v_x = u_field(s674, 0);
      u_value v_more = u_field(s674, 1);
      d675 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t677 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t677;
      }
    }
    if (!d675) {
      u_no_case(s674);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t682;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_678 = U_LIT("os_args");
    static struct u_object l_os_read_679 = U_LIT("os_read");
    static struct u_object l_os_write_680 = U_LIT("os_write");
    static struct u_object l_os_library_681 = U_LIT("os_library");
    t682 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_678), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_679), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_680), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_681), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t682 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t682;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t683 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e684 = &t683;
  static struct u_object l_Program_685 = U_LIT("Program");
  u_value l686 = f_ctx_46_named_2(v_c, &l_Program_685);
  while (u_is_con(l686, &n_list_46_Cons)) {
    u_value v_d = u_field(l686, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e684 = u_append(e684, v_d);
    }
    l686 = u_field(l686, 1);
  }
  u_value s687 = t683;
  u_value t688 = NULL;
  if ((!t688) && u_is_con(s687, &n_list_46_Cons)) {
    u_value v_d = u_field(s687, 0);
    static struct u_object l_read_689 = U_LIT("read");
    static struct u_object l_library_690 = U_LIT("library");
    t688 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_689), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_690), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t688) {
    t688 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t688) {
    u_no_case(s687);
  }
  return t688;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s691 = f_check_46_visible_names_3(*r_s, v_p, f_check_46_procs_named_2(f_check_46_ctx_of_1(*r_s), v_p));
    int d692 = 0;
    if ((!d692) && (u_is_con(s691, &n_list_46_Cons) && u_is_con(u_field(s691, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s691, 0);
      d692 = 1;
      {
        u_value t693 = v_q1;
        v_q = t693;
      }
    }
    if ((!d692) && u_is_con(s691, &n_list_46_Nil)) {
      d692 = 1;
      {
        u_value t694 = v_p;
        v_q = t694;
      }
    }
    if (!d692) {
      u_value v_qs = s691;
      d692 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d692) {
      u_no_case(s691);
    }
    u_value s695 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_ctx_of_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_ctx_of_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_ctx_of_1(*r_s)))));
    int d696 = 0;
    if ((!d696) && u_is_con(s695, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s695, 0), 0);
      u_value v_is = u_field(u_field(s695, 0), 1);
      u_value v_os = u_field(u_field(s695, 0), 2);
      u_value v_rs = u_field(u_field(s695, 0), 3);
      d696 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t697 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e698 = &t697;
        u_value l699 = v_is;
        while (u_is_con(l699, &n_list_46_Cons)) {
          u_value v_t = u_field(l699, 0);
          if (u_truth(u_mk_bool(1))) {
            e698 = u_append(e698, f_check_46_subst_2(v_t, v_m));
          }
          l699 = u_field(l699, 1);
        }
        p_check_46_checks(v_env, v_ins, t697, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t700 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e701 = &t700;
        u_value l702 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l702, &n_list_46_Cons)) {
          u_value v_t = u_field(l702, 0);
          if (u_truth(u_mk_bool(1))) {
            e701 = u_append(e701, f_check_46_subst_2(v_t, v_m));
          }
          l702 = u_field(l702, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t700, r_s);
        u_value t703 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t703;
      }
    }
    if ((!d696) && u_is_con(s695, &n_option_46_None)) {
      d696 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d696) {
      u_no_case(s695);
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
    u_value s704 = v_cases;
    int d705 = 0;
    if ((!d705) && u_is_con(s704, &n_list_46_Nil)) {
      d705 = 1;
      {
        u_value t706 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t706;
      }
    }
    if ((!d705) && (u_is_con(s704, &n_list_46_Cons) && u_is_con(u_field(s704, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s704, 0), 0);
      u_value v_g = u_field(u_field(s704, 0), 1);
      u_value v_b = u_field(u_field(s704, 0), 2);
      u_value v_more = u_field(s704, 1);
      d705 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t707 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t707;
      }
    }
    if (!d705) {
      u_no_case(s704);
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
    u_value s708 = v_hs;
    int d709 = 0;
    if ((!d709) && u_is_con(s708, &n_list_46_Nil)) {
      d709 = 1;
      {
        u_value t710 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t710;
      }
    }
    if ((!d709) && (u_is_con(s708, &n_list_46_Cons) && u_is_con(u_field(s708, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s708, 0), 0);
      u_value v_p = u_field(u_field(s708, 0), 1);
      u_value v_g = u_field(u_field(s708, 0), 2);
      u_value v_b = u_field(u_field(s708, 0), 3);
      u_value v_more = u_field(s708, 1);
      d709 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t711 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t711;
      }
    }
    if (!d709) {
      u_no_case(s708);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t712 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t712)) {
      t712 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t713 = t712;
    if (!u_truth(t713)) {
      t713 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t714 = t713;
    if (!u_truth(t714)) {
      t714 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t714)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t715 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t715)) {
      t715 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t716 = t715;
    if (!u_truth(t716)) {
      t716 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t716)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_717 = U_LIT("Bool");
    static struct u_object l_Int_718 = U_LIT("Int");
    static struct u_object l_Str_719 = U_LIT("Str");
    static struct u_object l_Char_720 = U_LIT("Char");
    static struct u_object l__45__62__721 = U_LIT("->");
    u_value t722 = f_list_46_contains_2(f_syntax_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_717, u_mk_con(&n_list_46_Cons, 2, &l_Int_718, u_mk_con(&n_list_46_Cons, 2, &l_Str_719, u_mk_con(&n_list_46_Cons, 2, &l_Char_720, u_mk_con(&n_list_46_Cons, 2, &l__45__62__721, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t722)) {
      t722 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t723 = t722;
    if (!u_truth(t723)) {
      t723 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t723)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t724 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e725 = &t724;
    u_value l726 = v_ts;
    while (u_is_con(l726, &n_list_46_Cons)) {
      u_value v_t = u_field(l726, 0);
      if (u_truth(u_mk_bool(1))) {
        e725 = u_append(e725, f_check_46_free_1(v_t));
      }
      l726 = u_field(l726, 1);
    }
    u_value t727 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t724)), f_check_46_params_1(v_ps));
    v_undeclared = t727;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s728 = v_cs;
    int d729 = 0;
    if ((!d729) && u_is_con(s728, &n_list_46_Nil)) {
      d729 = 1;
      {
      }
    }
    if ((!d729) && (u_is_con(s728, &n_list_46_Cons) && u_is_con(u_field(u_field(s728, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s728, 0), 0);
      u_value v_n = u_field(u_field(u_field(s728, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s728, 0), 1), 1);
      u_value v_rest = u_field(s728, 1);
      d729 = 1;
      {
        u_value t730 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t730;
        p_check_46_declare_sort(f_check_46_ctx_of_1(*r_s), v_n);
        u_value t731 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_ctx_of_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t731;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d729) && u_is_con(s728, &n_list_46_Cons)) {
      u_value v_rest = u_field(s728, 1);
      d729 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d729) {
      u_no_case(s728);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s732 = v_cs;
    int d733 = 0;
    if ((!d733) && u_is_con(s732, &n_list_46_Nil)) {
      d733 = 1;
      {
      }
    }
    if ((!d733) && (u_is_con(s732, &n_list_46_Cons) && u_is_con(u_field(u_field(s732, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s732, 0), 0);
      u_value v_n = u_field(u_field(u_field(s732, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s732, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s732, 0), 1), 2);
      u_value v_rest = u_field(s732, 1);
      d733 = 1;
      {
        u_value t734 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t734;
        p_check_46_declare_sort(f_check_46_ctx_of_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t735 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_ctx_of_1(*r_s), v_n, v_ps, v_u));
        *r_s = t735;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d733) && u_is_con(s732, &n_list_46_Cons)) {
      u_value v_rest = u_field(s732, 1);
      d733 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d733) {
      u_no_case(s732);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s736 = v_cs;
    int d737 = 0;
    if ((!d737) && u_is_con(s736, &n_list_46_Nil)) {
      d737 = 1;
      {
      }
    }
    if ((!d737) && (u_is_con(s736, &n_list_46_Cons) && u_is_con(u_field(u_field(s736, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s736, 0), 0);
      u_value v_n = u_field(u_field(u_field(s736, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s736, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s736, 0), 1), 2);
      u_value v_rest = u_field(s736, 1);
      d737 = 1;
      {
        u_value t738 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t738;
        p_check_46_unique_ids(v_ps);
        u_value t739 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e740 = &t739;
        u_value l741 = v_ks;
        while (u_is_con(l741, &n_list_46_Cons)) {
          u_value v_k = u_field(l741, 0);
          if (u_truth(u_mk_bool(1))) {
            e740 = u_append(e740, f_check_46_constr_name_1(v_k));
          }
          l741 = u_field(l741, 1);
        }
        u_value t742 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_ctx_of_1(*r_s), v_n, t739));
        *r_s = t742;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d737) && u_is_con(s736, &n_list_46_Cons)) {
      u_value v_rest = u_field(s736, 1);
      d737 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d737) {
      u_no_case(s736);
    }
  }
}

u_value f_check_46_constr_name_1(u_value v_k) {
  u_value s743 = v_k;
  u_value t744 = NULL;
  if ((!t744) && u_is_con(s743, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s743, 0);
    t744 = v_n;
  }
  if (!t744) {
    u_no_case(s743);
  }
  return t744;
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s745 = v_ks;
    int d746 = 0;
    if ((!d746) && u_is_con(s745, &n_list_46_Nil)) {
      d746 = 1;
      {
      }
    }
    if ((!d746) && (u_is_con(s745, &n_list_46_Cons) && u_is_con(u_field(s745, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s745, 0), 0);
      u_value v_args = u_field(u_field(s745, 0), 1);
      u_value v_rest = u_field(s745, 1);
      d746 = 1;
      {
        p_check_46_declare_name(f_check_46_ctx_of_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t747 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_ctx_of_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t747;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d746) {
      u_no_case(s745);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s748 = v_xss;
  u_value t749 = NULL;
  if ((!t749) && u_is_con(s748, &n_list_46_Nil)) {
    t749 = v_r;
  }
  if ((!t749) && u_is_con(s748, &n_list_46_Cons)) {
    u_value v_xs = u_field(s748, 0);
    u_value v_rest = u_field(s748, 1);
    t749 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t749) {
    u_no_case(s748);
  }
  return t749;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s750 = v_xss;
  u_value t751 = NULL;
  if ((!t751) && u_is_con(s750, &n_list_46_Nil)) {
    t751 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t751) && u_is_con(s750, &n_list_46_Cons)) {
    u_value v_xs = u_field(s750, 0);
    t751 = v_xs;
  }
  if (!t751) {
    u_no_case(s750);
  }
  return t751;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s752 = v_xss;
    int d753 = 0;
    if ((!d753) && u_is_con(s752, &n_list_46_Nil)) {
      d753 = 1;
      {
        u_value t754 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t754;
      }
    }
    if ((!d753) && u_is_con(s752, &n_list_46_Cons)) {
      u_value v_xs = u_field(s752, 0);
      u_value v_more = u_field(s752, 1);
      d753 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t755 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t755;
      }
    }
    if (!d753) {
      u_no_case(s752);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s756 = v_cs;
    int d757 = 0;
    if ((!d757) && u_is_con(s756, &n_list_46_Nil)) {
      d757 = 1;
      {
      }
    }
    if ((!d757) && (u_is_con(s756, &n_list_46_Cons) && u_is_con(u_field(u_field(s756, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s756, 0), 0);
      u_value v_n = u_field(u_field(u_field(s756, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s756, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s756, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s756, 0), 1), 3);
      u_value v_rest = u_field(s756, 1);
      d757 = 1;
      {
        u_value t758 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t758;
        p_check_46_overload(f_check_46_ctx_of_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t759 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_ctx_of_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t759;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d757) && u_is_con(s756, &n_list_46_Cons)) {
      u_value v_rest = u_field(s756, 1);
      d757 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d757) {
      u_no_case(s756);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s760 = v_cs;
    int d761 = 0;
    if ((!d761) && u_is_con(s760, &n_list_46_Nil)) {
      d761 = 1;
      {
      }
    }
    if ((!d761) && (u_is_con(s760, &n_list_46_Cons) && u_is_con(u_field(u_field(s760, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s760, 0), 0);
      u_value v_n = u_field(u_field(u_field(s760, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s760, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s760, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s760, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s760, 0), 1), 4);
      u_value v_rest = u_field(s760, 1);
      d761 = 1;
      {
        u_value t762 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t762;
        p_check_46_declare_name(f_check_46_ctx_of_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t763 = f_syntax_46_types_1(v_xs);
        v_ts = t763;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t764 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_ctx_of_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t764;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d761) && u_is_con(s760, &n_list_46_Cons)) {
      u_value v_rest = u_field(s760, 1);
      d761 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d761) {
      u_no_case(s760);
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
    u_value s765 = v_c;
    int d766 = 0;
    if ((!d766) && u_is_con(s765, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s765, 0);
      u_value v_ps = u_field(s765, 1);
      u_value v_bound = u_field(s765, 2);
      u_value v_res = u_field(s765, 3);
      u_value v_body = u_field(s765, 4);
      d766 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t767 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t767;
      }
    }
    if ((!d766) && u_is_con(s765, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s765, 0);
      u_value v_ps = u_field(s765, 1);
      u_value v_ins = u_field(s765, 2);
      u_value v_outs = u_field(s765, 3);
      u_value v_refs = u_field(s765, 4);
      u_value v_body = u_field(s765, 5);
      d766 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t768 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t768;
      }
    }
    if ((!d766) && u_is_con(s765, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s765, 0);
      d766 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t769 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t769;
      }
    }
    if ((!d766) && u_is_con(s765, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s765, 0);
      u_value v_body = u_field(s765, 1);
      d766 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t770 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t770;
      }
    }
    if ((!d766) && u_is_con(s765, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s765, 0);
      u_value v_body = u_field(s765, 1);
      d766 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t771 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t771;
      }
    }
    if (!d766) {
      d766 = 1;
      {
        u_value t772 = v_c;
        *r_c1 = t772;
      }
    }
    if (!d766) {
      u_no_case(s765);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s773 = v_cs;
    int d774 = 0;
    if ((!d774) && u_is_con(s773, &n_list_46_Nil)) {
      d774 = 1;
      {
        u_value t775 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t775;
      }
    }
    if ((!d774) && u_is_con(s773, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s773, 0), 0);
      u_value v_c = u_field(u_field(s773, 0), 1);
      u_value v_more = u_field(s773, 1);
      d774 = 1;
      {
        u_value t776 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t776;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t777 = f_check_46_zonk_cmd_2(f_check_46_sub_of_1(*r_s), v_c1);
        v_c2 = t777;
        u_value s778 = f_check_46_unknown_in_cmd_1(v_c2);
        int d779 = 0;
        if ((!d779) && u_is_con(s778, &n_option_46_Some)) {
          u_value v_t = u_field(s778, 0);
          d779 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d779) && u_is_con(s778, &n_option_46_None)) {
          d779 = 1;
          {
          }
        }
        if (!d779) {
          u_no_case(s778);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t780 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t780;
      }
    }
    if (!d774) {
      u_no_case(s773);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s781 = v_cs;
  u_value t782 = NULL;
  if ((!t782) && u_is_con(s781, &n_list_46_Nil)) {
    t782 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t782) && (u_is_con(s781, &n_list_46_Cons) && u_is_con(u_field(u_field(s781, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s781, 0), 1), 0);
    u_value v_more = u_field(s781, 1);
    t782 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t782) && (u_is_con(s781, &n_list_46_Cons) && u_is_con(u_field(u_field(s781, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s781, 0), 1), 0);
    u_value v_more = u_field(s781, 1);
    t782 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t782) && u_is_con(s781, &n_list_46_Cons)) {
    u_value v_more = u_field(s781, 1);
    t782 = f_check_46_block_names_1(v_more);
  }
  if (!t782) {
    u_no_case(s781);
  }
  return t782;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s783 = v_p;
    int d784 = 0;
    if ((!d784) && u_is_con(s783, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s783, 0);
      d784 = 1;
      {
        u_value t785 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t785;
        u_value t786 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t786;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t787 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t787;
      }
    }
    if (!d784) {
      u_no_case(s783);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_788 = U_LIT("main");
    u_value t789 = f_check_46_start_2(&l_main_788, u_mk_con(&n_list_46_Nil, 0));
    v_s = t789;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t790 = f_check_46_zonk_type_2(f_check_46_sub_of_1(v_s), v_u);
    *r_t = t790;
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
  static struct u_object l_List_791 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_791), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_792 = U_LIT("list");
  static struct u_object l_List_793 = U_LIT("List");
  static struct u_object l_A_794 = U_LIT("A");
  static struct u_object l_Nil_795 = U_LIT("Nil");
  static struct u_object l_Cons_796 = U_LIT("Cons");
  static struct u_object l_A_797 = U_LIT("A");
  static struct u_object l_A_798 = U_LIT("A");
  static struct u_object l_list_799 = U_LIT("list");
  static struct u_object l_len_800 = U_LIT("len");
  static struct u_object l_A_801 = U_LIT("A");
  static struct u_object l_xs_802 = U_LIT("xs");
  static struct u_object l_A_803 = U_LIT("A");
  static struct u_object l_xs_804 = U_LIT("xs");
  static struct u_object l_Nil_805 = U_LIT("Nil");
  static struct u_object l_Cons_806 = U_LIT("Cons");
  static struct u_object l_ys_807 = U_LIT("ys");
  static struct u_object l__43__808 = U_LIT("+");
  static struct u_object l_len_809 = U_LIT("len");
  static struct u_object l_ys_810 = U_LIT("ys");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_792, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_793), u_mk_con(&n_list_46_Cons, 2, &l_A_794, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_795), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_796), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_797), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_798)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_799), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_800), u_mk_con(&n_list_46_Cons, 2, &l_A_801, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_802), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_803))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_804), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_805), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_806), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_807), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__808, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_809, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_810), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s811 = v_t;
  u_value t812 = NULL;
  if ((!t812) && u_is_con(s811, &n_c_46_CtVoid)) {
    static struct u_object l_void_813 = U_LIT("void");
    t812 = &l_void_813;
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CtInt)) {
    static struct u_object l_int_814 = U_LIT("int");
    t812 = &l_int_814;
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s811, 0);
    t812 = v_n;
  }
  if ((!t812) && (u_is_con(s811, &n_c_46_CtPtr) && u_is_con(u_field(s811, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s811, 0), 0);
    static struct u_object l__42__815 = U_LIT("*");
    t812 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__815);
  }
  if ((!t812) && u_is_con(s811, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s811, 0);
    static struct u_object l__32__42__816 = U_LIT(" *");
    t812 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__816);
  }
  if (!t812) {
    u_no_case(s811);
  }
  return t812;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s817 = v_t;
  u_value t818 = NULL;
  if ((!t818) && u_is_con(s817, &n_c_46_CtPtr)) {
    t818 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t818) {
    static struct u_object l__32__819 = U_LIT(" ");
    t818 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__819, v_x));
  }
  if (!t818) {
    u_no_case(s817);
  }
  return t818;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s820 = v_e;
  u_value t821 = NULL;
  if ((!t821) && u_is_con(s820, &n_c_46_CeVar)) {
    u_value v_x = u_field(s820, 0);
    t821 = v_x;
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeNum)) {
    u_value v_n = u_field(s820, 0);
    t821 = u_str_show(v_n);
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeStr)) {
    u_value v_s = u_field(s820, 0);
    t821 = u_str_quote(v_s);
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeCall)) {
    u_value v_f = u_field(s820, 0);
    u_value v_args = u_field(s820, 1);
    static struct u_object l__40__822 = U_LIT("(");
    static struct u_object l__44__32__823 = U_LIT(", ");
    u_value t824 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e825 = &t824;
    u_value l826 = v_args;
    while (u_is_con(l826, &n_list_46_Cons)) {
      u_value v_a = u_field(l826, 0);
      if (u_truth(u_mk_bool(1))) {
        e825 = u_append(e825, f_c_46_c_expr_1(v_a));
      }
      l826 = u_field(l826, 1);
    }
    static struct u_object l__41__827 = U_LIT(")");
    t821 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__822, u_concat(u_str_join(&l__44__32__823, t824), &l__41__827)));
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s820, 0);
    u_value v_a = u_field(s820, 1);
    t821 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s820, 0);
    u_value v_a = u_field(s820, 1);
    u_value v_b = u_field(s820, 2);
    static struct u_object l__32__828 = U_LIT(" ");
    static struct u_object l__32__829 = U_LIT(" ");
    t821 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__828, u_concat(v_o, u_concat(&l__32__829, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s820, 0);
    u_value v_i = u_field(s820, 1);
    static struct u_object l__91__830 = U_LIT("[");
    static struct u_object l__93__831 = U_LIT("]");
    t821 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__830, u_concat(f_c_46_c_expr_1(v_i), &l__93__831)));
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s820, 0);
    u_value v_f = u_field(s820, 1);
    static struct u_object l__45__62__832 = U_LIT("->");
    t821 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__832, v_f));
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeCast)) {
    u_value v_t = u_field(s820, 0);
    u_value v_a = u_field(s820, 1);
    static struct u_object l__40__833 = U_LIT("(");
    static struct u_object l__41__834 = U_LIT(")");
    t821 = u_concat(&l__40__833, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__834, f_c_46_c_operand_1(v_a))));
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeCond)) {
    u_value v_c = u_field(s820, 0);
    u_value v_a = u_field(s820, 1);
    u_value v_b = u_field(s820, 2);
    static struct u_object l__32__63__32__835 = U_LIT(" ? ");
    static struct u_object l__32__58__32__836 = U_LIT(" : ");
    t821 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__835, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__836, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t821) && u_is_con(s820, &n_c_46_CeInit)) {
    u_value v_es = u_field(s820, 0);
    static struct u_object l__123__837 = U_LIT("{");
    static struct u_object l__44__32__838 = U_LIT(", ");
    u_value t839 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e840 = &t839;
    u_value l841 = v_es;
    while (u_is_con(l841, &n_list_46_Cons)) {
      u_value v_a = u_field(l841, 0);
      if (u_truth(u_mk_bool(1))) {
        e840 = u_append(e840, f_c_46_c_expr_1(v_a));
      }
      l841 = u_field(l841, 1);
    }
    static struct u_object l__125__842 = U_LIT("}");
    t821 = u_concat(&l__123__837, u_concat(u_str_join(&l__44__32__838, t839), &l__125__842));
  }
  if (!t821) {
    u_no_case(s820);
  }
  return t821;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s843 = v_e;
  u_value t844 = NULL;
  if ((!t844) && u_is_con(s843, &n_c_46_CeVar)) {
    t844 = f_c_46_c_expr_1(v_e);
  }
  if ((!t844) && u_is_con(s843, &n_c_46_CeNum)) {
    u_value v_n = u_field(s843, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t844 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t844) && u_is_con(s843, &n_c_46_CeStr)) {
    t844 = f_c_46_c_expr_1(v_e);
  }
  if ((!t844) && u_is_con(s843, &n_c_46_CeCall)) {
    t844 = f_c_46_c_expr_1(v_e);
  }
  if ((!t844) && u_is_con(s843, &n_c_46_CeIndex)) {
    t844 = f_c_46_c_expr_1(v_e);
  }
  if ((!t844) && u_is_con(s843, &n_c_46_CeArrow)) {
    t844 = f_c_46_c_expr_1(v_e);
  }
  if (!t844) {
    static struct u_object l__40__845 = U_LIT("(");
    static struct u_object l__41__846 = U_LIT(")");
    t844 = u_concat(&l__40__845, u_concat(f_c_46_c_expr_1(v_e), &l__41__846));
  }
  if (!t844) {
    u_no_case(s843);
  }
  return t844;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s847 = v_s;
  u_value t848 = NULL;
  if ((!t848) && u_is_con(s847, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s847, 0);
    static struct u_object l__59__849 = U_LIT(";");
    t848 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__849)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t848) && (u_is_con(s847, &n_c_46_CsDecl) && u_is_con(u_field(s847, 2), &n_option_46_None))) {
    u_value v_t = u_field(s847, 0);
    u_value v_x = u_field(s847, 1);
    static struct u_object l__59__850 = U_LIT(";");
    t848 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__850)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t848) && (u_is_con(s847, &n_c_46_CsDecl) && u_is_con(u_field(s847, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s847, 0);
    u_value v_x = u_field(s847, 1);
    u_value v_e = u_field(u_field(s847, 2), 0);
    static struct u_object l__32__61__32__851 = U_LIT(" = ");
    static struct u_object l__59__852 = U_LIT(";");
    t848 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__851, u_concat(f_c_46_c_expr_1(v_e), &l__59__852)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t848) && u_is_con(s847, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s847, 0);
    u_value v_e = u_field(s847, 1);
    static struct u_object l__32__61__32__853 = U_LIT(" = ");
    static struct u_object l__59__854 = U_LIT(";");
    t848 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__853, u_concat(f_c_46_c_expr_1(v_e), &l__59__854)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t848) && (u_is_con(s847, &n_c_46_CsIf) && u_is_con(u_field(s847, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s847, 0);
    u_value v_l = u_field(s847, 1);
    static struct u_object l_if_32__40__855 = U_LIT("if (");
    static struct u_object l__41__32__123__856 = U_LIT(") {");
    static struct u_object l__32__32__857 = U_LIT("  ");
    static struct u_object l__125__858 = U_LIT("}");
    t848 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__855, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__856))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__857), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__858), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t848) && u_is_con(s847, &n_c_46_CsIf)) {
    u_value v_c = u_field(s847, 0);
    u_value v_l = u_field(s847, 1);
    u_value v_r = u_field(s847, 2);
    static struct u_object l_if_32__40__859 = U_LIT("if (");
    static struct u_object l__41__32__123__860 = U_LIT(") {");
    static struct u_object l__32__32__861 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__862 = U_LIT("} else {");
    static struct u_object l__32__32__863 = U_LIT("  ");
    static struct u_object l__125__864 = U_LIT("}");
    t848 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__859, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__860))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__861), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__862), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__863), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__864), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t848) && u_is_con(s847, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s847, 0);
    u_value v_b = u_field(s847, 1);
    static struct u_object l_while_32__40__865 = U_LIT("while (");
    static struct u_object l__41__32__123__866 = U_LIT(") {");
    static struct u_object l__32__32__867 = U_LIT("  ");
    static struct u_object l__125__868 = U_LIT("}");
    t848 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__865, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__866))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__867), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__868), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t848) && (u_is_con(s847, &n_c_46_CsReturn) && u_is_con(u_field(s847, 0), &n_option_46_None))) {
    static struct u_object l_return_59__869 = U_LIT("return;");
    t848 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__869), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t848) && (u_is_con(s847, &n_c_46_CsReturn) && u_is_con(u_field(s847, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s847, 0), 0);
    static struct u_object l_return_32__870 = U_LIT("return ");
    static struct u_object l__59__871 = U_LIT(";");
    t848 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__870, u_concat(f_c_46_c_expr_1(v_e), &l__59__871))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t848) && u_is_con(s847, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s847, 0);
    static struct u_object l__123__872 = U_LIT("{");
    static struct u_object l__32__32__873 = U_LIT("  ");
    static struct u_object l__125__874 = U_LIT("}");
    t848 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__872), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__873), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__874), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t848) {
    u_no_case(s847);
  }
  return t848;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t875 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e876 = &t875;
  u_value l877 = v_ss;
  while (u_is_con(l877, &n_list_46_Cons)) {
    u_value v_s = u_field(l877, 0);
    if (u_truth(u_mk_bool(1))) {
      e876 = u_append(e876, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l877 = u_field(l877, 1);
  }
  return f_list_46_concat_1(t875);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s878 = v_ps;
  u_value t879 = NULL;
  if ((!t879) && u_is_con(s878, &n_list_46_Nil)) {
    static struct u_object l_void_880 = U_LIT("void");
    t879 = &l_void_880;
  }
  if (!t879) {
    static struct u_object l__44__32__881 = U_LIT(", ");
    u_value t882 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e883 = &t882;
    u_value l884 = v_ps;
    while (u_is_con(l884, &n_list_46_Cons)) {
      u_value v_p = u_field(l884, 0);
      if (u_truth(u_mk_bool(1))) {
        e883 = u_append(e883, f_c_46_c_param_1(v_p));
      }
      l884 = u_field(l884, 1);
    }
    t879 = u_str_join(&l__44__32__881, t882);
  }
  if (!t879) {
    u_no_case(s878);
  }
  return t879;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s885 = v_p;
  u_value t886 = NULL;
  if (!t886) {
    u_value v_t = u_field(s885, 0);
    u_value v_x = u_field(s885, 1);
    t886 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t886) {
    u_no_case(s885);
  }
  return t886;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s887 = v_d;
  u_value t888 = NULL;
  if ((!t888) && u_is_con(s887, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s887, 0);
    static struct u_object l__35_include_32__889 = U_LIT("#include ");
    t888 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__889, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t888) && u_is_con(s887, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s887, 0);
    u_value v_f = u_field(s887, 1);
    u_value v_ps = u_field(s887, 2);
    static struct u_object l__40__890 = U_LIT("(");
    static struct u_object l__41__59__891 = U_LIT(");");
    t888 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__890, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__891))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t888) && u_is_con(s887, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s887, 0);
    u_value v_f = u_field(s887, 1);
    u_value v_ps = u_field(s887, 2);
    u_value v_b = u_field(s887, 3);
    static struct u_object l__40__892 = U_LIT("(");
    static struct u_object l__41__32__123__893 = U_LIT(") {");
    static struct u_object l__32__32__894 = U_LIT("  ");
    static struct u_object l__125__895 = U_LIT("}");
    t888 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__892, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__893))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__894, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__895, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t888) && (u_is_con(s887, &n_c_46_CdGlobal) && u_is_con(u_field(s887, 2), &n_option_46_None))) {
    u_value v_t = u_field(s887, 0);
    u_value v_x = u_field(s887, 1);
    static struct u_object l__59__896 = U_LIT(";");
    t888 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__896), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t888) && (u_is_con(s887, &n_c_46_CdGlobal) && u_is_con(u_field(s887, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s887, 0);
    u_value v_x = u_field(s887, 1);
    u_value v_e = u_field(u_field(s887, 2), 0);
    static struct u_object l__32__61__32__897 = U_LIT(" = ");
    static struct u_object l__59__898 = U_LIT(";");
    t888 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__897, u_concat(f_c_46_c_expr_1(v_e), &l__59__898))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t888) {
    u_no_case(s887);
  }
  return t888;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s899 = v_f;
  u_value t900 = NULL;
  if ((!t900) && u_is_con(s899, &n_c_46_CFile)) {
    u_value v_ds = u_field(s899, 0);
    static struct u_object l__10__10__901 = U_LIT("\n\n");
    u_value t902 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e903 = &t902;
    u_value l904 = v_ds;
    while (u_is_con(l904, &n_list_46_Cons)) {
      u_value v_d = u_field(l904, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__905 = U_LIT("\n");
        e903 = u_append(e903, u_str_join(&l__10__905, f_c_46_c_decl_1(v_d)));
      }
      l904 = u_field(l904, 1);
    }
    static struct u_object l__10__906 = U_LIT("\n");
    t900 = u_concat(u_str_join(&l__10__10__901, t902), &l__10__906);
  }
  if (!t900) {
    u_no_case(s899);
  }
  return t900;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__907 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__907, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__46__908 = U_LIT("()[],;.");
  return u_str_contains(&l__40__41__91__93__44__59__46__908, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t909 = u_is_letter(v_c);
  if (!u_truth(t909)) {
    t909 = u_is_digit(v_c);
  }
  u_value t910 = t909;
  if (!u_truth(t910)) {
    t910 = u_eq(v_c, u_mk_char(95));
  }
  return t910;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t911;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t911 = u_str_get(v_s, v_i);
  } else {
    t911 = u_chr(u_mk_int(0));
  }
  return t911;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s912 = v_c;
  u_value t913 = NULL;
  if ((!t913) && u_equals(s912, u_mk_char(34))) {
    t913 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t913) && u_equals(s912, u_mk_char(39))) {
    t913 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t913) && u_equals(s912, u_mk_char(92))) {
    t913 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t913) && u_equals(s912, u_mk_char(110))) {
    t913 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t913) && u_equals(s912, u_mk_char(116))) {
    t913 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t913) && u_equals(s912, u_mk_char(114))) {
    t913 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t913) {
    t913 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t913) {
    u_no_case(s912);
  }
  return t913;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t914;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t914 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t914 = v_i;
  }
  return t914;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t915 = u_mk_int(0);
    u_value t916 = u_mk_int(1);
    u_value t917 = u_mk_int(1);
    v_j = t915;
    *r_line = t916;
    *r_column = t917;
    u_value t922 = u_lt(v_j, v_i);
    while (u_truth(t922)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t918 = u_add(*r_line, u_mk_int(1));
            u_value t919 = u_mk_int(1);
            *r_line = t918;
            *r_column = t919;
          }
        } else {
          {
            u_value t920 = u_add(*r_column, u_mk_int(1));
            *r_column = t920;
          }
        }
        u_value t921 = u_add(v_j, u_mk_int(1));
        v_j = t921;
      }
      t922 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t923 = *r_i;
    u_value t924 = u_add(*r_i, u_mk_int(1));
    u_value t925 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t923;
    *r_i = t924;
    v_parts = t925;
    u_value t926 = *r_i;
    v_from = t926;
    u_value t937 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t937)) {
      {
        u_value t927 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t927;
        u_value t928 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t928)) {
          t928 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t928)) {
          {
            static struct u_object l_unterminated_32_lit_929 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_929, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s930 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d931 = 0;
            if ((!d931) && u_is_con(s930, &n_option_46_Some)) {
              u_value v_d = u_field(s930, 0);
              d931 = 1;
              {
                u_value t932 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t933 = u_add(*r_i, u_mk_int(2));
                v_parts = t932;
                *r_i = t933;
                u_value t934 = *r_i;
                v_from = t934;
              }
            }
            if ((!d931) && u_is_con(s930, &n_option_46_None)) {
              d931 = 1;
              {
                static struct u_object l_invalid_32_escape_935 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_935, *r_i), "lex.LexError");
              }
            }
            if (!d931) {
              u_no_case(s930);
            }
          }
        } else {
          {
            u_value t936 = u_add(*r_i, u_mk_int(1));
            *r_i = t936;
          }
        }
      }
      t937 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__938 = U_LIT("");
    u_value t939 = u_str_join(&l__938, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t940 = u_add(*r_i, u_mk_int(1));
    *r_text = t939;
    *r_i = t940;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t941 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t941;
    u_value t942 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t943 = v_j;
    *r_t = t942;
    *r_i = t943;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t944 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t944;
    u_value s945 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d946 = 0;
    if ((!d946) && u_is_con(s945, &n_option_46_Some)) {
      u_value v_n = u_field(s945, 0);
      d946 = 1;
      {
        u_value t947 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t948 = v_j;
        *r_t = t947;
        *r_i = t948;
      }
    }
    if ((!d946) && u_is_con(s945, &n_option_46_None)) {
      d946 = 1;
      {
        static struct u_object l_invalid_32_number_949 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_949, *r_i), "lex.LexError");
      }
    }
    if (!d946) {
      u_no_case(s945);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t950 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t950;
    u_value t951 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t952 = v_j;
    *r_t = t951;
    *r_i = t952;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t953 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t953;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t954 = *r_i;
    v_start = t954;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_955 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_955, v_start), "lex.LexError");
      }
    }
    u_value t956 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t956;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t957 = u_str_get(v_s, *r_i);
    v_c = t957;
    u_value s958 = v_c;
    int d959 = 0;
    if (!d959) {
      u_value t960 = u_is_letter(v_c);
      if (!u_truth(t960)) {
        t960 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t960)) {
        d959 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d959) {
      if (u_truth(u_is_digit(v_c))) {
        d959 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d959) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d959 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d959) && u_equals(s958, u_mk_char(34))) {
      d959 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d959) && u_equals(s958, u_mk_char(39))) {
      d959 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d959) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d959 = 1;
        {
          u_value t961 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t962 = u_add(*r_i, u_mk_int(1));
          *r_t = t961;
          *r_i = t962;
        }
      }
    }
    if (!d959) {
      d959 = 1;
      {
        static struct u_object l_unexpected_32_chara_963 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_963, *r_i), "lex.LexError");
      }
    }
    if (!d959) {
      u_no_case(s958);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t964 = u_mk_int(0);
    v_i = t964;
    u_value t965 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t965;
    u_value t972 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t972)) {
      {
        u_value t966 = v_i;
        v_start = t966;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t967 = u_add(v_i, u_mk_int(1));
            v_i = t967;
          }
        } else {
          u_value t968 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t968)) {
            t968 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t968)) {
            {
              u_value t970 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam969, 0));
              v_i = t970;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t971 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t971;
            }
          }
        }
      }
      t972 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t973 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t973;
  }
}

u_value f_lex_46_tokens_1(u_value v_ts) {
  return f_list_46_firsts_1(v_ts);
}

u_value f_parse_46_scope_params_1(u_value v_sc) {
  u_value s974 = v_sc;
  u_value t975 = NULL;
  if ((!t975) && u_is_con(s974, &n_parse_46_Scope)) {
    u_value v_ps = u_field(s974, 0);
    t975 = v_ps;
  }
  if (!t975) {
    u_no_case(s974);
  }
  return t975;
}

u_value f_parse_46_scope_notations_1(u_value v_sc) {
  u_value s976 = v_sc;
  u_value t977 = NULL;
  if ((!t977) && u_is_con(s976, &n_parse_46_Scope)) {
    u_value v_ns = u_field(s976, 1);
    t977 = v_ns;
  }
  if (!t977) {
    u_no_case(s976);
  }
  return t977;
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
  static struct u_object l___978 = U_LIT("_");
  static struct u_object l_assert_979 = U_LIT("assert");
  static struct u_object l_assign_980 = U_LIT("assign");
  static struct u_object l_call_981 = U_LIT("call");
  static struct u_object l_case_982 = U_LIT("case");
  static struct u_object l_catch_983 = U_LIT("catch");
  static struct u_object l_data_984 = U_LIT("data");
  static struct u_object l_else_985 = U_LIT("else");
  static struct u_object l_end_986 = U_LIT("end");
  static struct u_object l_eval_987 = U_LIT("eval");
  static struct u_object l_false_988 = U_LIT("false");
  static struct u_object l_for_989 = U_LIT("for");
  static struct u_object l_function_990 = U_LIT("function");
  static struct u_object l_if_991 = U_LIT("if");
  static struct u_object l_import_992 = U_LIT("import");
  static struct u_object l_in_993 = U_LIT("in");
  static struct u_object l_infix_994 = U_LIT("infix");
  static struct u_object l_lambda_995 = U_LIT("lambda");
  static struct u_object l_let_996 = U_LIT("let");
  static struct u_object l_local_997 = U_LIT("local");
  static struct u_object l_match_998 = U_LIT("match");
  static struct u_object l_out_999 = U_LIT("out");
  static struct u_object l_postfix_1000 = U_LIT("postfix");
  static struct u_object l_prefix_1001 = U_LIT("prefix");
  static struct u_object l_procedure_1002 = U_LIT("procedure");
  static struct u_object l_raise_1003 = U_LIT("raise");
  static struct u_object l_ref_1004 = U_LIT("ref");
  static struct u_object l_run_1005 = U_LIT("run");
  static struct u_object l_skip_1006 = U_LIT("skip");
  static struct u_object l_test_1007 = U_LIT("test");
  static struct u_object l_then_1008 = U_LIT("then");
  static struct u_object l_true_1009 = U_LIT("true");
  static struct u_object l_type_1010 = U_LIT("type");
  static struct u_object l_try_1011 = U_LIT("try");
  static struct u_object l_while_1012 = U_LIT("while");
  cache = u_mk_con(&n_list_46_Cons, 2, &l___978, u_mk_con(&n_list_46_Cons, 2, &l_assert_979, u_mk_con(&n_list_46_Cons, 2, &l_assign_980, u_mk_con(&n_list_46_Cons, 2, &l_call_981, u_mk_con(&n_list_46_Cons, 2, &l_case_982, u_mk_con(&n_list_46_Cons, 2, &l_catch_983, u_mk_con(&n_list_46_Cons, 2, &l_data_984, u_mk_con(&n_list_46_Cons, 2, &l_else_985, u_mk_con(&n_list_46_Cons, 2, &l_end_986, u_mk_con(&n_list_46_Cons, 2, &l_eval_987, u_mk_con(&n_list_46_Cons, 2, &l_false_988, u_mk_con(&n_list_46_Cons, 2, &l_for_989, u_mk_con(&n_list_46_Cons, 2, &l_function_990, u_mk_con(&n_list_46_Cons, 2, &l_if_991, u_mk_con(&n_list_46_Cons, 2, &l_import_992, u_mk_con(&n_list_46_Cons, 2, &l_in_993, u_mk_con(&n_list_46_Cons, 2, &l_infix_994, u_mk_con(&n_list_46_Cons, 2, &l_lambda_995, u_mk_con(&n_list_46_Cons, 2, &l_let_996, u_mk_con(&n_list_46_Cons, 2, &l_local_997, u_mk_con(&n_list_46_Cons, 2, &l_match_998, u_mk_con(&n_list_46_Cons, 2, &l_out_999, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1000, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1001, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1002, u_mk_con(&n_list_46_Cons, 2, &l_raise_1003, u_mk_con(&n_list_46_Cons, 2, &l_ref_1004, u_mk_con(&n_list_46_Cons, 2, &l_run_1005, u_mk_con(&n_list_46_Cons, 2, &l_skip_1006, u_mk_con(&n_list_46_Cons, 2, &l_test_1007, u_mk_con(&n_list_46_Cons, 2, &l_then_1008, u_mk_con(&n_list_46_Cons, 2, &l_true_1009, u_mk_con(&n_list_46_Cons, 2, &l_type_1010, u_mk_con(&n_list_46_Cons, 2, &l_try_1011, u_mk_con(&n_list_46_Cons, 2, &l_while_1012, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1013 = U_LIT("=");
  static struct u_object l__61__62__1014 = U_LIT("=>");
  static struct u_object l__58__61__1015 = U_LIT(":=");
  static struct u_object l__58__1016 = U_LIT(":");
  static struct u_object l__124__1017 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1013, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1014, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1015, u_mk_con(&n_list_46_Cons, 2, &l__58__1016, u_mk_con(&n_list_46_Cons, 2, &l__124__1017, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1018 = v_ns;
  u_value t1019 = NULL;
  if ((!t1019) && u_is_con(s1018, &n_list_46_Nil)) {
    t1019 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1019) && (u_is_con(s1018, &n_list_46_Cons) && (u_is_con(u_field(s1018, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1018, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1018, 0), 1);
    u_value v_ops = u_field(u_field(s1018, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1019 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1019) && u_is_con(s1018, &n_list_46_Cons)) {
    u_value v_more = u_field(s1018, 1);
    t1019 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1019) {
    u_no_case(s1018);
  }
  return t1019;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1020 = v_ns;
  u_value t1021 = NULL;
  if ((!t1021) && u_is_con(s1020, &n_list_46_Nil)) {
    t1021 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1021) && (u_is_con(s1020, &n_list_46_Cons) && (u_is_con(u_field(s1020, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1020, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1020, 0), 1);
    u_value v_ops = u_field(u_field(s1020, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1021 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1021) && u_is_con(s1020, &n_list_46_Cons)) {
    u_value v_more = u_field(s1020, 1);
    t1021 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1021) {
    u_no_case(s1020);
  }
  return t1021;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1022 = v_ns;
  u_value t1023 = NULL;
  if ((!t1023) && u_is_con(s1022, &n_list_46_Nil)) {
    t1023 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1023) && (u_is_con(s1022, &n_list_46_Cons) && u_is_con(u_field(s1022, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1022, 0), 0);
    u_value v_p = u_field(u_field(s1022, 0), 1);
    u_value v_ops = u_field(u_field(s1022, 0), 2);
    u_value t1024 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1024)) {
      t1024 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1025 = t1024;
    if (u_truth(t1025)) {
      t1025 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1025)) {
      t1023 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1023) && u_is_con(s1022, &n_list_46_Cons)) {
    u_value v_more = u_field(s1022, 1);
    t1023 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1023) {
    u_no_case(s1022);
  }
  return t1023;
}

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op) {
  u_value s1026 = v_ns;
  u_value t1027 = NULL;
  if ((!t1027) && u_is_con(s1026, &n_list_46_Nil)) {
    t1027 = u_mk_bool(0);
  }
  if ((!t1027) && (u_is_con(s1026, &n_list_46_Cons) && u_is_con(u_field(s1026, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1026, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1027 = u_mk_bool(1);
    }
  }
  if ((!t1027) && u_is_con(s1026, &n_list_46_Cons)) {
    u_value v_more = u_field(s1026, 1);
    t1027 = f_parse_46_declared_2(v_more, v_op);
  }
  if (!t1027) {
    u_no_case(s1026);
  }
  return t1027;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1028 = v_ts;
  u_value t1029 = NULL;
  if ((!t1029) && u_is_con(s1028, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1028, 0), 0);
    t1029 = v_t;
  }
  if (!t1029) {
    t1029 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1029) {
    u_no_case(s1028);
  }
  return t1029;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1030 = v_ts;
  u_value t1031 = NULL;
  if ((!t1031) && (u_is_con(s1030, &n_list_46_Cons) && u_is_con(u_field(s1030, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1030, 1), 0), 0);
    t1031 = v_t;
  }
  if (!t1031) {
    t1031 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1031) {
    u_no_case(s1030);
  }
  return t1031;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1032 = v_ts;
  u_value t1033 = NULL;
  if ((!t1033) && u_is_con(s1032, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1032, 0), 1);
    t1033 = v_i;
  }
  if (!t1033) {
    t1033 = u_mk_int(0);
  }
  if (!t1033) {
    u_no_case(s1032);
  }
  return t1033;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1034 = v_t;
  u_value t1035 = NULL;
  if ((!t1035) && u_is_con(s1034, &n_lex_46_TName)) {
    u_value v_n = u_field(s1034, 0);
    t1035 = v_n;
  }
  if ((!t1035) && u_is_con(s1034, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1034, 0);
    t1035 = v_s;
  }
  if ((!t1035) && u_is_con(s1034, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1034, 0);
    t1035 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1035) && u_is_con(s1034, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1034, 0);
    t1035 = u_str_show(v_n);
  }
  if ((!t1035) && u_is_con(s1034, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1034, 0);
    t1035 = u_str_quote(v_s);
  }
  if ((!t1035) && u_is_con(s1034, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1034, 0);
    static struct u_object l__39__1036 = U_LIT("'");
    static struct u_object l__39__1037 = U_LIT("'");
    t1035 = u_concat(&l__39__1036, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1037));
  }
  if ((!t1035) && u_is_con(s1034, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1038 = U_LIT("end of input");
    t1035 = &l_end_32_of_32_input_1038;
  }
  if (!t1035) {
    u_no_case(s1034);
  }
  return t1035;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1039 = f_parse_46_head_token_1(v_ts);
  u_value t1040 = NULL;
  if ((!t1040) && u_is_con(s1039, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1039, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1040 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1040) && u_is_con(s1039, &n_lex_46_TName)) {
    u_value v_n = u_field(s1039, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1040 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1040) {
    t1040 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1040) {
    u_no_case(s1039);
  }
  return t1040;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1041 = *r_ts;
    int d1042 = 0;
    if ((!d1042) && u_is_con(s1041, &n_list_46_Cons)) {
      u_value v_more = u_field(s1041, 1);
      d1042 = 1;
      {
        u_value t1043 = v_more;
        *r_ts = t1043;
      }
    }
    if (!d1042) {
      d1042 = 1;
      {
      }
    }
    if (!d1042) {
      u_no_case(s1041);
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
        static struct u_object l__32_expected_32_but_32__1044 = U_LIT(" expected but ");
        static struct u_object l__32_found_1045 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1044, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1045))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1046 = f_parse_46_head_token_1(*r_ts);
    int d1047 = 0;
    if ((!d1047) && u_is_con(s1046, &n_lex_46_TName)) {
      u_value v_m = u_field(s1046, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1047 = 1;
        {
          u_value t1048 = v_m;
          *r_n = t1048;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1047) {
      d1047 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1049 = U_LIT("name expected but ");
        static struct u_object l__32_found_1050 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1049, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1050)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1047) {
      u_no_case(s1046);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1051 = f_parse_46_head_token_1(*r_ts);
    int d1052 = 0;
    if ((!d1052) && u_is_con(s1051, &n_lex_46_TName)) {
      u_value v_m = u_field(s1051, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_scope_notations_1(v_sc), v_m))) {
        d1052 = 1;
        {
          static struct u_object l_operator_32__1053 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1054 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1053, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1054)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1052) {
      d1052 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1052) {
      u_no_case(s1051);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1055 = f_parse_46_head_token_1(*r_ts);
    int d1056 = 0;
    if ((!d1056) && u_is_con(s1055, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1055, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1056 = 1;
        {
          u_value t1057 = v_s;
          *r_n = t1057;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1056) {
      d1056 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1056) {
      u_no_case(s1055);
    }
  }
}

u_value f_parse_46_ops_notations_1(u_value v_o) {
  u_value s1058 = v_o;
  u_value t1059 = NULL;
  if ((!t1059) && u_is_con(s1058, &n_parse_46_Ops)) {
    u_value v_ns = u_field(s1058, 0);
    t1059 = v_ns;
  }
  if (!t1059) {
    u_no_case(s1058);
  }
  return t1059;
}

u_value f_parse_46_apply_prefix_3(u_value v_o, u_value v_op, u_value v_a) {
  u_value s1060 = v_o;
  u_value t1061 = NULL;
  if ((!t1061) && u_is_con(s1060, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1060, 1);
    t1061 = u_apply(v_f, 2, v_op, v_a);
  }
  if (!t1061) {
    u_no_case(s1060);
  }
  return t1061;
}

u_value f_parse_46_apply_postfix_3(u_value v_o, u_value v_op, u_value v_a) {
  u_value s1062 = v_o;
  u_value t1063 = NULL;
  if ((!t1063) && u_is_con(s1062, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1062, 2);
    t1063 = u_apply(v_f, 2, v_op, v_a);
  }
  if (!t1063) {
    u_no_case(s1062);
  }
  return t1063;
}

u_value f_parse_46_apply_infix_4(u_value v_o, u_value v_op, u_value v_a, u_value v_b) {
  u_value s1064 = v_o;
  u_value t1065 = NULL;
  if ((!t1065) && u_is_con(s1064, &n_parse_46_Ops)) {
    u_value v_f = u_field(s1064, 3);
    t1065 = u_apply(v_f, 3, v_op, v_a, v_b);
  }
  if (!t1065) {
    u_no_case(s1064);
  }
  return t1065;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1066 = v_rest;
    int d1067 = 0;
    if ((!d1067) && u_is_con(s1066, &n_list_46_Nil)) {
      d1067 = 1;
      {
      }
    }
    if ((!d1067) && (u_is_con(s1066, &n_list_46_Cons) && u_is_con(u_field(s1066, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1066, 0), 0);
      u_value v_i = u_field(u_field(s1066, 0), 1);
      d1067 = 1;
      {
        static struct u_object l_unexpected_32_opera_1068 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1068, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1067) {
      d1067 = 1;
      {
        static struct u_object l_missing_32_operator_1069 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1069, v_at), "parse.ParseError");
      }
    }
    if (!d1067) {
      u_no_case(s1066);
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
    u_value s1070 = v_items;
    int d1071 = 0;
    if ((!d1071) && (u_is_con(s1070, &n_list_46_Cons) && u_is_con(u_field(s1070, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1070, 0), 0);
      u_value v_more = u_field(s1070, 1);
      d1071 = 1;
      {
        u_value t1072 = v_x;
        u_value t1073 = v_more;
        *r_a = t1072;
        *r_rest = t1073;
      }
    }
    if ((!d1071) && (u_is_con(s1070, &n_list_46_Cons) && u_is_con(u_field(s1070, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1070, 0), 0);
      u_value v_i = u_field(u_field(s1070, 0), 1);
      u_value v_more = u_field(s1070, 1);
      d1071 = 1;
      {
        u_value s1074 = f_parse_46_prefix_prec_2(f_parse_46_ops_notations_1(v_o), v_op);
        int d1075 = 0;
        if ((!d1075) && u_is_con(s1074, &n_option_46_Some)) {
          u_value v_p = u_field(s1074, 0);
          d1075 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1076 = f_parse_46_apply_prefix_3(v_o, v_op, v_b);
            *r_a = t1076;
          }
        }
        if ((!d1075) && u_is_con(s1074, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_ops_notations_1(v_o), v_op))) {
            d1075 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1077 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1077), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1075) && u_is_con(s1074, &n_option_46_None)) {
          d1075 = 1;
          {
            static struct u_object l_undeclared_32_opera_1078 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1078, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1075) {
          u_no_case(s1074);
        }
      }
    }
    if ((!d1071) && u_is_con(s1070, &n_list_46_Nil)) {
      d1071 = 1;
      {
        static struct u_object l_missing_32_operand_1079 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1079, v_at), "parse.ParseError");
      }
    }
    if (!d1071) {
      u_no_case(s1070);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1080 = v_items;
    int d1081 = 0;
    if ((!d1081) && (u_is_con(s1080, &n_list_46_Cons) && u_is_con(u_field(s1080, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1080, 0), 0);
      u_value v_i = u_field(u_field(s1080, 0), 1);
      u_value v_tail = u_field(s1080, 1);
      d1081 = 1;
      {
        u_value s1082 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_ops_notations_1(v_o), v_op), f_parse_46_infix_of_2(f_parse_46_ops_notations_1(v_o), v_op));
        int d1083 = 0;
        if ((!d1083) && u_is_con(u_field(s1082, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1082, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1083 = 1;
            {
              p_parse_46_climb_loop(f_parse_46_apply_postfix_3(v_o, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1083) && u_is_con(u_field(s1082, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1082, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1082, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1083 = 1;
            {
              u_value t1084;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1084 = v_p;
              } else {
                t1084 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1084, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_ops_notations_1(v_o));
              p_parse_46_climb_loop(f_parse_46_apply_infix_4(v_o, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1083) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_ops_notations_1(v_o), v_op)))) {
            d1083 = 1;
            {
              static struct u_object l_undeclared_32_opera_1085 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1085, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1083) {
          d1083 = 1;
          {
            u_value t1086 = v_lhs;
            u_value t1087 = v_items;
            *r_a = t1086;
            *r_rest = t1087;
          }
        }
        if (!d1083) {
          u_no_case(s1082);
        }
      }
    }
    if (!d1081) {
      d1081 = 1;
      {
        u_value t1088 = v_lhs;
        u_value t1089 = v_items;
        *r_a = t1088;
        *r_rest = t1089;
      }
    }
    if (!d1081) {
      u_no_case(s1080);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1090 = v_rest;
    int d1091 = 0;
    if ((!d1091) && (u_is_con(s1090, &n_list_46_Cons) && u_is_con(u_field(s1090, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1090, 0), 0);
      u_value v_i = u_field(u_field(s1090, 0), 1);
      u_value t1094 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1094)) {
        t1094 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1094)) {
        d1091 = 1;
        {
          static struct u_object l_operator_32__1092 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1093 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1092, u_concat(v_op, &l__32_is_32_not_32_associat_1093)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1091) {
      d1091 = 1;
      {
      }
    }
    if (!d1091) {
      u_no_case(s1090);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1095 = v_fp;
  u_value t1096 = NULL;
  if ((!t1096) && u_is_con(s1095, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1095, 0), 1);
    t1096 = u_eq(v_q, v_p);
  }
  if (!t1096) {
    t1096 = u_mk_bool(0);
  }
  if (!t1096) {
    u_no_case(s1095);
  }
  return t1096;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1097 = v_ts;
  u_value t1098 = NULL;
  if ((!t1098) && (u_is_con(s1097, &n_list_46_Cons) && (u_is_con(u_field(s1097, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1097, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1097, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1097, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1097, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1097, 0), 1);
    t1098 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1098) && (u_is_con(s1097, &n_list_46_Cons) && u_is_con(u_field(s1097, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1097, 0);
    t1098 = v_t;
  }
  if ((!t1098) && u_is_con(s1097, &n_list_46_Nil)) {
    t1098 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1098) {
    static struct u_object l__40_invalid_41__1099 = U_LIT("(invalid)");
    t1098 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1099), v_ts);
  }
  if (!t1098) {
    u_no_case(s1097);
  }
  return t1098;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1100 = v_ts;
  u_value t1101 = NULL;
  if ((!t1101) && (u_is_con(s1100, &n_list_46_Cons) && (u_is_con(u_field(s1100, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1100, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1100, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1100, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1100, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1100, 0), 1);
    t1101 = v_cs;
  }
  if (!t1101) {
    t1101 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1101) {
    u_no_case(s1100);
  }
  return t1101;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1102 = U_LIT("->");
  u_value t1109;
  if (u_truth(u_eq(v_op, &l__45__62__1102))) {
    u_value t1103 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1104 = &t1103;
    u_value l1105 = v_l;
    while (u_is_con(l1105, &n_list_46_Cons)) {
      u_value v_t = u_field(l1105, 0);
      if (u_truth(u_mk_bool(1))) {
        e1104 = u_append(e1104, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1105 = u_field(l1105, 1);
    }
    t1109 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1103, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1106 = U_LIT("*");
    u_value t1108;
    if (u_truth(u_eq(v_op, &l__42__1106))) {
      static struct u_object l__40_product_41__1107 = U_LIT("(product)");
      t1108 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1107), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1108 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1109 = t1108;
  }
  return t1109;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1110 = v_t;
  u_value t1111 = NULL;
  if ((!t1111) && (u_is_con(s1110, &n_syntax_46_TSort) && (u_is_con(u_field(s1110, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1110, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1110, 0), 1), u_mk_lit("(invalid)")))))) {
    t1111 = u_mk_bool(1);
  }
  if ((!t1111) && u_is_con(s1110, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1110, 1);
    t1111 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1111) && u_is_con(s1110, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1110, 0);
    u_value v_r = u_field(s1110, 1);
    u_value t1112 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1112)) {
      t1112 = f_parse_46_has_invalid_1(v_r);
    }
    t1111 = t1112;
  }
  if ((!t1111) && u_is_con(s1110, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1110, 0);
    t1111 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1111) {
    t1111 = u_mk_bool(0);
  }
  if (!t1111) {
    u_no_case(s1110);
  }
  return t1111;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1113 = v_ts;
  u_value t1114 = NULL;
  if ((!t1114) && u_is_con(s1113, &n_list_46_Nil)) {
    t1114 = u_mk_bool(0);
  }
  if ((!t1114) && u_is_con(s1113, &n_list_46_Cons)) {
    u_value v_t = u_field(s1113, 0);
    u_value v_more = u_field(s1113, 1);
    u_value t1115 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1115)) {
      t1115 = f_parse_46_any_invalid_1(v_more);
    }
    t1114 = t1115;
  }
  if (!t1114) {
    u_no_case(s1113);
  }
  return t1114;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1116 = v_t;
  u_value t1117 = NULL;
  if ((!t1117) && u_is_con(s1116, &n_lex_46_TName)) {
    u_value v_n = u_field(s1116, 0);
    u_value t1118 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1118)) {
      t1118 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1117 = t1118;
  }
  if ((!t1117) && (u_is_con(s1116, &n_lex_46_TPunct) && u_equals(u_field(s1116, 0), u_mk_char(40)))) {
    t1117 = u_mk_bool(1);
  }
  if (!t1117) {
    t1117 = u_mk_bool(0);
  }
  if (!t1117) {
    u_no_case(s1116);
  }
  return t1117;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1119 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1119;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_scope_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1120 = f_parse_46_type_one_1(v_r);
    *r_t = t1120;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1121 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1121, v_start), "parse.ParseError");
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
    u_value t1122 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1122;
    u_value t1123 = u_mk_bool(1);
    v_next = t1123;
    u_value t1124 = u_mk_bool(1);
    v_go = t1124;
    u_value t1133 = v_go;
    while (u_truth(t1133)) {
      {
        u_value s1125 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1126 = 0;
        if ((!d1126) && u_is_con(s1125, &n_option_46_Some)) {
          u_value v_op = u_field(s1125, 0);
          d1126 = 1;
          {
            u_value t1127 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1128 = u_mk_bool(1);
            v_rev = t1127;
            v_next = t1128;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1126) && u_is_con(s1125, &n_option_46_None)) {
          u_value t1131 = v_next;
          if (u_truth(t1131)) {
            t1131 = f_parse_46_starts_type_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1131)) {
            d1126 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1129 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1130 = u_mk_bool(0);
              v_rev = t1129;
              v_next = t1130;
            }
          }
        }
        if (!d1126) {
          d1126 = 1;
          {
            u_value t1132 = u_mk_bool(0);
            v_go = t1132;
          }
        }
        if (!d1126) {
          u_no_case(s1125);
        }
      }
      t1133 = v_go;
    }
    u_value s1134 = v_rev;
    int d1135 = 0;
    if ((!d1135) && u_is_con(s1134, &n_list_46_Nil)) {
      d1135 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1136 = U_LIT("type expected but ");
        static struct u_object l__32_found_1137 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1136, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1137)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1135) {
      d1135 = 1;
      {
        u_value t1138 = f_list_46_reverse_1(v_rev);
        *r_items = t1138;
      }
    }
    if (!d1135) {
      u_no_case(s1134);
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
            u_value t1139 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1139;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_scope_params_1(v_sc)))) {
            {
              u_value t1140 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1140;
            }
          } else {
            {
              u_value t1141 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1141;
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
    u_value t1142 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1142;
    u_value t1143 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1143)) {
      t1143 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1143)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1144 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1144;
        u_value t1146 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1146)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1145 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1145;
          }
          t1146 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1147 = f_list_46_reverse_1(v_rev);
    *r_r = t1147;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1148 = v_es;
  u_value t1149 = NULL;
  if ((!t1149) && u_is_con(s1148, &n_list_46_Nil)) {
    t1149 = v_rest;
  }
  if ((!t1149) && u_is_con(s1148, &n_list_46_Cons)) {
    u_value v_e = u_field(s1148, 0);
    u_value v_more = u_field(s1148, 1);
    static struct u_object l_Cons_1150 = U_LIT("Cons");
    t1149 = f_parse_46_expr_op2_3(&l_Cons_1150, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1149) {
    u_no_case(s1148);
  }
  return t1149;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1151 = v_t;
  u_value t1152 = NULL;
  if ((!t1152) && u_is_con(s1151, &n_lex_46_TName)) {
    u_value v_n = u_field(s1151, 0);
    static struct u_object l_let_1153 = U_LIT("let");
    static struct u_object l_if_1154 = U_LIT("if");
    static struct u_object l_lambda_1155 = U_LIT("lambda");
    static struct u_object l_match_1156 = U_LIT("match");
    static struct u_object l_true_1157 = U_LIT("true");
    static struct u_object l_false_1158 = U_LIT("false");
    u_value t1160 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1153, u_mk_con(&n_list_46_Cons, 2, &l_if_1154, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1155, u_mk_con(&n_list_46_Cons, 2, &l_match_1156, u_mk_con(&n_list_46_Cons, 2, &l_true_1157, u_mk_con(&n_list_46_Cons, 2, &l_false_1158, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1160)) {
      u_value t1159 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1159)) {
        t1159 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1160 = t1159;
    }
    t1152 = t1160;
  }
  if ((!t1152) && u_is_con(s1151, &n_lex_46_TNum)) {
    t1152 = u_mk_bool(1);
  }
  if ((!t1152) && u_is_con(s1151, &n_lex_46_TStr)) {
    t1152 = u_mk_bool(1);
  }
  if ((!t1152) && u_is_con(s1151, &n_lex_46_TChar)) {
    t1152 = u_mk_bool(1);
  }
  if ((!t1152) && u_is_con(s1151, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1151, 0);
    u_value t1161 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1161)) {
      t1161 = u_eq(v_c, u_mk_char(91));
    }
    t1152 = t1161;
  }
  if (!t1152) {
    t1152 = u_mk_bool(0);
  }
  if (!t1152) {
    u_no_case(s1151);
  }
  return t1152;
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
    u_value t1162 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1162;
    u_value t1163 = u_mk_bool(1);
    v_next = t1163;
    u_value t1164 = u_mk_bool(1);
    v_go = t1164;
    u_value t1173 = v_go;
    while (u_truth(t1173)) {
      {
        u_value s1165 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
        int d1166 = 0;
        if ((!d1166) && u_is_con(s1165, &n_option_46_Some)) {
          u_value v_op = u_field(s1165, 0);
          d1166 = 1;
          {
            u_value t1167 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1168 = u_mk_bool(1);
            v_rev = t1167;
            v_next = t1168;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1166) && u_is_con(s1165, &n_option_46_None)) {
          u_value t1171 = v_next;
          if (u_truth(t1171)) {
            t1171 = f_parse_46_starts_expr_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1171)) {
            d1166 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1169 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1170 = u_mk_bool(0);
              v_rev = t1169;
              v_next = t1170;
            }
          }
        }
        if (!d1166) {
          d1166 = 1;
          {
            u_value t1172 = u_mk_bool(0);
            v_go = t1172;
          }
        }
        if (!d1166) {
          u_no_case(s1165);
        }
      }
      t1173 = v_go;
    }
    u_value s1174 = v_rev;
    int d1175 = 0;
    if ((!d1175) && u_is_con(s1174, &n_list_46_Nil)) {
      d1175 = 1;
      {
        static struct u_object l_expression_32_expec_1176 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1177 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1176, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1177)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1175) {
      d1175 = 1;
      {
        u_value t1178 = f_list_46_reverse_1(v_rev);
        *r_items = t1178;
      }
    }
    if (!d1175) {
      u_no_case(s1174);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1179 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1179;
    u_value t1181 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1181)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1180 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1180;
      }
      t1181 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1182 = f_list_46_reverse_1(v_rev);
    *r_es = t1182;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1183 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1183;
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
    u_value t1184 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1184)) {
      t1184 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1188 = t1184;
    while (u_truth(t1188)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1185 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1185;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1186 = U_LIT(".");
            u_value t1187 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1186, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1187;
          }
        }
      }
      u_value t1189 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1189)) {
        t1189 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1188 = t1189;
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
    u_value s1190 = f_parse_46_head_token_1(*r_ts);
    int d1191 = 0;
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TName) && u_equals(u_field(s1190, 0), u_mk_lit("let")))) {
      d1191 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TName) && u_equals(u_field(s1190, 0), u_mk_lit("if")))) {
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1192 = U_LIT("then");
        p_parse_46_keyword(&l_then_1192, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1193 = U_LIT("else");
        p_parse_46_keyword(&l_else_1193, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1194 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1194;
      }
    }
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TName) && u_equals(u_field(s1190, 0), u_mk_lit("lambda")))) {
      d1191 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TName) && u_equals(u_field(s1190, 0), u_mk_lit("match")))) {
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1195 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1195;
      }
    }
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TName) && u_equals(u_field(s1190, 0), u_mk_lit("true")))) {
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1196 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1196;
      }
    }
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TName) && u_equals(u_field(s1190, 0), u_mk_lit("false")))) {
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1197 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1197;
      }
    }
    if ((!d1191) && u_is_con(s1190, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1190, 0);
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1198 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1198;
      }
    }
    if ((!d1191) && u_is_con(s1190, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1190, 0);
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1199 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1199;
      }
    }
    if ((!d1191) && u_is_con(s1190, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1190, 0);
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1200 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1200;
      }
    }
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TPunct) && u_equals(u_field(s1190, 0), u_mk_char(40)))) {
      d1191 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1201 = v_es;
        int d1202 = 0;
        if ((!d1202) && (u_is_con(s1201, &n_list_46_Cons) && u_is_con(u_field(s1201, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1201, 0);
          d1202 = 1;
          {
            u_value t1203 = v_x;
            *r_e = t1203;
          }
        }
        if (!d1202) {
          d1202 = 1;
          {
            u_value t1204 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1204;
          }
        }
        if (!d1202) {
          u_no_case(s1201);
        }
      }
    }
    if ((!d1191) && (u_is_con(s1190, &n_lex_46_TPunct) && u_equals(u_field(s1190, 0), u_mk_char(91)))) {
      d1191 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1191) && u_is_con(s1190, &n_lex_46_TName)) {
      u_value v_n = u_field(s1190, 0);
      u_value t1206 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1206)) {
        t1206 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_scope_notations_1(v_sc), v_n));
      }
      if (u_truth(t1206)) {
        d1191 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1205 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1205;
        }
      }
    }
    if (!d1191) {
      d1191 = 1;
      {
        static struct u_object l_expression_32_expec_1207 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1208 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1207, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1208)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1191) {
      u_no_case(s1190);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1209 = v_ps;
  u_value t1210 = NULL;
  if ((!t1210) && u_is_con(s1209, &n_list_46_Nil)) {
    t1210 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1210) && (u_is_con(s1209, &n_list_46_Cons) && u_is_con(u_field(s1209, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1209, 0), 0);
    u_value v_more = u_field(s1209, 1);
    u_value s1211 = f_parse_46_pattern_vars_1(v_more);
    u_value t1212 = NULL;
    if ((!t1212) && u_is_con(s1211, &n_option_46_Some)) {
      u_value v_xs = u_field(s1211, 0);
      t1212 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1212) && u_is_con(s1211, &n_option_46_None)) {
      t1212 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1212) {
      u_no_case(s1211);
    }
    t1210 = t1212;
  }
  if (!t1210) {
    t1210 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1210) {
    u_no_case(s1209);
  }
  return t1210;
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
    static struct u_object l__58__61__1213 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1213), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1214 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1215 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1214;
    v_es = t1215;
    u_value t1219 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1219)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1216 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1216), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1217 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1218 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1217;
        v_es = t1218;
      }
      t1219 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1220 = U_LIT("in");
    p_parse_46_keyword(&l_in_1220, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1221 = f_list_46_reverse_1(v_ps);
    u_value t1222 = f_list_46_reverse_1(v_es);
    v_ps = t1221;
    v_es = t1222;
    u_value s1223 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1224 = 0;
    if ((!d1224) && u_is_con(u_field(s1223, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1223, 0), 0);
      d1224 = 1;
      {
        u_value t1225 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1225;
      }
    }
    if ((!d1224) && (u_is_con(u_field(s1223, 0), &n_option_46_None) && (u_is_con(u_field(s1223, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1223, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1223, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1223, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1223, 1), 0);
      u_value v_c = u_field(u_field(s1223, 2), 0);
      d1224 = 1;
      {
        u_value t1226 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1226;
      }
    }
    if (!d1224) {
      d1224 = 1;
      {
        u_value t1227 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1227;
      }
    }
    if (!d1224) {
      u_no_case(s1223);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1228 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1228;
    static struct u_object l__61__62__1229 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1229)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1230 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1230;
        u_value t1232 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1232)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1231 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1231;
          }
          t1232 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1233 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1233), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1234 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1234;
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
        static struct u_object l_Nil_1235 = U_LIT("Nil");
        u_value t1236 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1235), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1236;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1237 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1237))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1238 = U_LIT("in");
            p_parse_46_keyword(&l_in_1238, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1239 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1240 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1239;
            v_lists = t1240;
            u_value t1244 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1244)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1241 = U_LIT("in");
                p_parse_46_keyword(&l_in_1241, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1242 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1243 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1242;
                v_lists = t1243;
              }
              t1244 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1245 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1245;
            static struct u_object l_if_1246 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1246))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1247 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1247;
          }
        } else {
          {
            u_value t1248 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1248;
            u_value t1250 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1250)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1249 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1249;
              }
              t1250 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1251 = U_LIT("Nil");
            u_value t1252 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1251), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1252;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1253 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1253;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1254 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1254))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1255 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1255;
      }
    } else {
      {
        u_value t1256 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1256;
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
    u_value t1257 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1257;
    static struct u_object l_case_1258 = U_LIT("case");
    u_value t1261 = f_parse_46_at_keyword_2(*r_ts, &l_case_1258);
    while (u_truth(t1261)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1259 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1259), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1260 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1260;
      }
      static struct u_object l_case_1262 = U_LIT("case");
      t1261 = f_parse_46_at_keyword_2(*r_ts, &l_case_1262);
    }
    static struct u_object l_else_1263 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1263))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1264 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1264;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1265 = U_LIT("end");
    p_parse_46_keyword(&l_end_1265, r_ts);
    u_value t1266 = f_list_46_reverse_1(v_rev);
    *r_cs = t1266;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1267 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1267))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1268 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1268, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1269 = v_ps;
  u_value t1270 = NULL;
  if ((!t1270) && u_is_con(s1269, &n_list_46_Nil)) {
    t1270 = v_rest;
  }
  if ((!t1270) && u_is_con(s1269, &n_list_46_Cons)) {
    u_value v_p = u_field(s1269, 0);
    u_value v_more = u_field(s1269, 1);
    static struct u_object l_Cons_1271 = U_LIT("Cons");
    t1270 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1271), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1270) {
    u_no_case(s1269);
  }
  return t1270;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1272 = v_t;
  u_value t1273 = NULL;
  if ((!t1273) && u_is_con(s1272, &n_lex_46_TName)) {
    u_value v_n = u_field(s1272, 0);
    static struct u_object l___1274 = U_LIT("_");
    static struct u_object l_true_1275 = U_LIT("true");
    static struct u_object l_false_1276 = U_LIT("false");
    u_value t1278 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1274, u_mk_con(&n_list_46_Cons, 2, &l_true_1275, u_mk_con(&n_list_46_Cons, 2, &l_false_1276, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1278)) {
      u_value t1277 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1277)) {
        t1277 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1278 = t1277;
    }
    t1273 = t1278;
  }
  if ((!t1273) && u_is_con(s1272, &n_lex_46_TNum)) {
    t1273 = u_mk_bool(1);
  }
  if ((!t1273) && u_is_con(s1272, &n_lex_46_TStr)) {
    t1273 = u_mk_bool(1);
  }
  if ((!t1273) && u_is_con(s1272, &n_lex_46_TChar)) {
    t1273 = u_mk_bool(1);
  }
  if ((!t1273) && u_is_con(s1272, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1272, 0);
    u_value t1279 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1279)) {
      t1279 = u_eq(v_c, u_mk_char(91));
    }
    t1273 = t1279;
  }
  if (!t1273) {
    t1273 = u_mk_bool(0);
  }
  if (!t1273) {
    u_no_case(s1272);
  }
  return t1273;
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
    u_value t1280 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1280;
    u_value t1281 = u_mk_bool(1);
    v_next = t1281;
    u_value t1282 = u_mk_bool(1);
    v_go = t1282;
    u_value t1291 = v_go;
    while (u_truth(t1291)) {
      {
        u_value s1283 = f_parse_46_operator_at_2(f_parse_46_scope_notations_1(v_sc), *r_ts);
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
            t1289 = f_parse_46_starts_pattern_2(f_parse_46_scope_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1289)) {
            d1284 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1287 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
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
        static struct u_object l_pattern_32_expected_1294 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1295 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1294, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1295)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1297 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1297;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1298 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1298;
        u_value t1300 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1300)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1299 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1299;
          }
          t1300 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1301 = f_list_46_reverse_1(v_rev);
    *r_ps = t1301;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1302 = f_parse_46_head_token_1(*r_ts);
    int d1303 = 0;
    if ((!d1303) && (u_is_con(s1302, &n_lex_46_TName) && u_equals(u_field(s1302, 0), u_mk_lit("_")))) {
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1304 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1304;
      }
    }
    if ((!d1303) && (u_is_con(s1302, &n_lex_46_TName) && u_equals(u_field(s1302, 0), u_mk_lit("true")))) {
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1305 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1305;
      }
    }
    if ((!d1303) && (u_is_con(s1302, &n_lex_46_TName) && u_equals(u_field(s1302, 0), u_mk_lit("false")))) {
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1306 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1306;
      }
    }
    if ((!d1303) && u_is_con(s1302, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1302, 0);
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1307 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1307;
      }
    }
    if ((!d1303) && u_is_con(s1302, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1302, 0);
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1308 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1308;
      }
    }
    if ((!d1303) && u_is_con(s1302, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1302, 0);
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1309 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1309;
      }
    }
    if ((!d1303) && (u_is_con(s1302, &n_lex_46_TPunct) && u_equals(u_field(s1302, 0), u_mk_char(40)))) {
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1310 = v_ps;
        int d1311 = 0;
        if ((!d1311) && (u_is_con(s1310, &n_list_46_Cons) && u_is_con(u_field(s1310, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1310, 0);
          d1311 = 1;
          {
            u_value t1312 = v_q;
            *r_p = t1312;
          }
        }
        if (!d1311) {
          d1311 = 1;
          {
            u_value t1313 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1313;
          }
        }
        if (!d1311) {
          u_no_case(s1310);
        }
      }
    }
    if ((!d1303) && (u_is_con(s1302, &n_lex_46_TPunct) && u_equals(u_field(s1302, 0), u_mk_char(91)))) {
      d1303 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1314 = U_LIT("Nil");
            u_value t1315 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1314), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1315;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1316 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1316;
            u_value t1318 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1318)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1317 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1317;
              }
              t1318 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1319 = U_LIT("Nil");
            u_value t1320 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1319), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1320;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1321 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1321;
          }
        }
      }
    }
    if (!d1303) {
      d1303 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1322 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1322;
          }
        } else {
          {
            u_value t1323 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1323;
          }
        }
      }
    }
    if (!d1303) {
      u_no_case(s1302);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1324 = v_t;
  u_value t1325 = NULL;
  if ((!t1325) && u_is_con(s1324, &n_lex_46_TName)) {
    u_value v_n = u_field(s1324, 0);
    static struct u_object l_skip_1326 = U_LIT("skip");
    static struct u_object l_if_1327 = U_LIT("if");
    static struct u_object l_while_1328 = U_LIT("while");
    static struct u_object l_call_1329 = U_LIT("call");
    static struct u_object l_match_1330 = U_LIT("match");
    static struct u_object l_assert_1331 = U_LIT("assert");
    static struct u_object l_assign_1332 = U_LIT("assign");
    static struct u_object l_raise_1333 = U_LIT("raise");
    static struct u_object l_try_1334 = U_LIT("try");
    t1325 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1326, u_mk_con(&n_list_46_Cons, 2, &l_if_1327, u_mk_con(&n_list_46_Cons, 2, &l_while_1328, u_mk_con(&n_list_46_Cons, 2, &l_call_1329, u_mk_con(&n_list_46_Cons, 2, &l_match_1330, u_mk_con(&n_list_46_Cons, 2, &l_assert_1331, u_mk_con(&n_list_46_Cons, 2, &l_assign_1332, u_mk_con(&n_list_46_Cons, 2, &l_raise_1333, u_mk_con(&n_list_46_Cons, 2, &l_try_1334, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1325) {
    t1325 = u_mk_bool(0);
  }
  if (!t1325) {
    u_no_case(s1324);
  }
  return t1325;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1335 = v_cs;
  u_value t1336 = NULL;
  if ((!t1336) && u_is_con(s1335, &n_list_46_Nil)) {
    t1336 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1336) && (u_is_con(s1335, &n_list_46_Cons) && u_is_con(u_field(s1335, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1335, 0), 0);
    u_value v_g = u_field(u_field(s1335, 0), 1);
    u_value v_b = u_field(u_field(s1335, 0), 2);
    u_value v_more = u_field(s1335, 1);
    t1336 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1336) {
    u_no_case(s1335);
  }
  return t1336;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1337 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1337;
    u_value t1338 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1338;
    u_value t1339 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1339;
    static struct u_object l_local_1340 = U_LIT("local");
    u_value t1342 = f_parse_46_at_keyword_2(*r_ts, &l_local_1340);
    while (u_truth(t1342)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1341 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1341)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1341 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1343 = U_LIT("local");
      t1342 = f_parse_46_at_keyword_2(*r_ts, &l_local_1343);
    }
    u_value t1345 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1345)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1344 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1344;
      }
      t1345 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1346 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1346;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1347 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1347;
    static struct u_object l__58__1348 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1348)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1349 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1349)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1350 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1350;
      }
    }
    u_value t1351 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1351;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1352 = f_parse_46_head_token_1(*r_ts);
    int d1353 = 0;
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("skip")))) {
      d1353 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1354 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1354;
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("if")))) {
      d1353 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("while")))) {
      d1353 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1355 = U_LIT("end");
        p_parse_46_keyword(&l_end_1355, r_ts);
        u_value t1356 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1356;
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("call")))) {
      d1353 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("match")))) {
      d1353 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1357 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1357;
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("assert")))) {
      d1353 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1358 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1358;
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("assign")))) {
      d1353 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("raise")))) {
      d1353 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1359 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1359;
      }
    }
    if ((!d1353) && (u_is_con(s1352, &n_lex_46_TName) && u_equals(u_field(s1352, 0), u_mk_lit("try")))) {
      d1353 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1360 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1360, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1361 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1361;
      }
    }
    if (!d1353) {
      d1353 = 1;
      {
        static struct u_object l_statement_32_expect_1362 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1363 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1362, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1363)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1353) {
      u_no_case(s1352);
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
    static struct u_object l_else_1364 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1364)))) {
      {
        static struct u_object l_end_1365 = U_LIT("end");
        p_parse_46_keyword(&l_end_1365, r_ts);
        u_value t1366 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1366;
      }
    } else {
      static struct u_object l_if_1367 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1367)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1368 = U_LIT("end");
          p_parse_46_keyword(&l_end_1368, r_ts);
        }
      }
    }
    u_value t1369 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1369;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1370 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1370), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1371 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1371;
    u_value t1374 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1374)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1372 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1372), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1373 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1373;
      }
      t1374 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1375 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1375;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1376 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1376;
    static struct u_object l_case_1377 = U_LIT("case");
    u_value t1380 = f_parse_46_at_keyword_2(*r_ts, &l_case_1377);
    while (u_truth(t1380)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1378 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1378), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1379 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1379;
      }
      static struct u_object l_case_1381 = U_LIT("case");
      t1380 = f_parse_46_at_keyword_2(*r_ts, &l_case_1381);
    }
    static struct u_object l_else_1382 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1382))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1383 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1383;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1384 = U_LIT("end");
    p_parse_46_keyword(&l_end_1384, r_ts);
    u_value t1385 = f_list_46_reverse_1(v_rev);
    *r_cs = t1385;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1387;
  if (u_truth(v_first)) {
    t1387 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1386 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1386)) {
      t1386 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1387 = t1386;
  }
  return t1387;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1389 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1389)) {
    static struct u_object l_out_1388 = U_LIT("out");
    t1389 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1388));
  }
  u_value t1391 = t1389;
  if (u_truth(t1391)) {
    static struct u_object l_ref_1390 = U_LIT("ref");
    t1391 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1390));
  }
  return t1391;
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
    u_value t1392 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1393 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1394 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1395 = u_mk_bool(1);
    v_ins = t1392;
    v_outs = t1393;
    v_refs = t1394;
    v_first = t1395;
    static struct u_object l_in_1396 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1396))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1397 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1398 = u_mk_bool(0);
        v_ins = t1397;
        v_first = t1398;
        u_value t1400 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1400)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1399 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1399;
          }
          t1400 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1401 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1401, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1402 = u_mk_bool(0);
        v_first = t1402;
      }
    }
    static struct u_object l_ref_1403 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1403, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1404 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1404;
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
    u_value t1405 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1405;
    u_value t1407 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1407)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1406 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1406;
      }
      t1407 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1408 = f_list_46_reverse_1(v_rev);
    *r_xs = t1408;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1409 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1409;
    static struct u_object l__58__1410 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1410)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1411 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1411;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1412 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1412;
    u_value t1414 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1414)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1413 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1413;
      }
      t1414 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1415 = f_list_46_reverse_1(v_rev);
    *r_xs = t1415;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1416 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1416;
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
  u_value s1417 = v_cs;
  u_value t1418 = NULL;
  if ((!t1418) && u_is_con(s1417, &n_list_46_Nil)) {
    t1418 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1418) && (u_is_con(s1417, &n_list_46_Cons) && u_is_con(u_field(s1417, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1417, 1);
    t1418 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1418) && (u_is_con(s1417, &n_list_46_Cons) && u_is_con(u_field(s1417, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1417, 1);
    t1418 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1418) && (u_is_con(s1417, &n_list_46_Cons) && u_is_con(u_field(s1417, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1417, 1);
    t1418 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1418) && u_is_con(s1417, &n_list_46_Cons)) {
    u_value v_c = u_field(s1417, 0);
    u_value v_more = u_field(s1417, 1);
    t1418 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1418) {
    u_no_case(s1417);
  }
  return t1418;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1419 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1420 = &t1419;
  u_value l1421 = v_cs;
  while (u_is_con(l1421, &n_list_46_Cons)) {
    u_value v_c = u_field(l1421, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1420 = u_append(e1420, v_c);
    }
    l1421 = u_field(l1421, 1);
  }
  return t1419;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1422 = v_c;
  u_value t1423 = NULL;
  if ((!t1423) && u_is_con(s1422, &n_syntax_46_CNotation)) {
    t1423 = u_mk_bool(1);
  }
  if (!t1423) {
    t1423 = u_mk_bool(0);
  }
  if (!t1423) {
    u_no_case(s1422);
  }
  return t1423;
}

u_value f_parse_46_loaded_modules_1(u_value v_ld) {
  u_value s1424 = v_ld;
  u_value t1425 = NULL;
  if ((!t1425) && u_is_con(s1424, &n_parse_46_Loader)) {
    u_value v_ms = u_field(s1424, 0);
    t1425 = v_ms;
  }
  if (!t1425) {
    u_no_case(s1424);
  }
  return t1425;
}

u_value f_parse_46_loading_1(u_value v_ld) {
  u_value s1426 = v_ld;
  u_value t1427 = NULL;
  if ((!t1427) && u_is_con(s1426, &n_parse_46_Loader)) {
    u_value v_ls = u_field(s1426, 1);
    t1427 = v_ls;
  }
  if (!t1427) {
    u_no_case(s1426);
  }
  return t1427;
}

u_value f_parse_46_base_notations_1(u_value v_ld) {
  u_value s1428 = v_ld;
  u_value t1429 = NULL;
  if ((!t1429) && u_is_con(s1428, &n_parse_46_Loader)) {
    u_value v_b = u_field(s1428, 2);
    t1429 = v_b;
  }
  if (!t1429) {
    u_no_case(s1428);
  }
  return t1429;
}

u_value f_parse_46_directory_1(u_value v_ld) {
  u_value s1430 = v_ld;
  u_value t1431 = NULL;
  if ((!t1431) && u_is_con(s1430, &n_parse_46_Loader)) {
    u_value v_d = u_field(s1430, 3);
    t1431 = v_d;
  }
  if (!t1431) {
    u_no_case(s1430);
  }
  return t1431;
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1432 = v_c;
  u_value t1433 = NULL;
  if ((!t1433) && u_is_con(s1432, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1432, 0);
    u_value v_cs = u_field(s1432, 1);
    u_value t1434 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1435 = &t1434;
    u_value l1436 = v_cs;
    while (u_is_con(l1436, &n_list_46_Cons)) {
      u_value v_d = u_field(l1436, 0);
      if (u_truth(u_mk_bool(1))) {
        e1435 = u_append(e1435, f_parse_46_modules_of_1(v_d));
      }
      l1436 = u_field(l1436, 1);
    }
    t1433 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1434));
  }
  if (!t1433) {
    t1433 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1433) {
    u_no_case(s1432);
  }
  return t1433;
}

u_value f_parse_46_own_notations_2(u_value v_ld, u_value v_n) {
  u_value s1437 = f_list_46_lookup_2(v_n, f_parse_46_loaded_modules_1(v_ld));
  u_value t1438 = NULL;
  if ((!t1438) && u_is_con(s1437, &n_option_46_Some)) {
    u_value v_ns = u_field(s1437, 0);
    t1438 = v_ns;
  }
  if ((!t1438) && u_is_con(s1437, &n_option_46_None)) {
    t1438 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1438) {
    u_no_case(s1437);
  }
  return t1438;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1439 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1439;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1440 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1440;
        u_value t1442 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1442)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1441 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1441;
          }
          t1442 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1443 = f_list_46_reverse_1(v_rev);
    *r_ps = t1443;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1444 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1444;
    u_value s1445 = f_parse_46_head_token_1(*r_ts);
    int d1446 = 0;
    if ((!d1446) && (u_is_con(s1445, &n_lex_46_TName) && u_equals(u_field(s1445, 0), u_mk_lit("prefix")))) {
      d1446 = 1;
      {
        u_value t1447 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1447;
      }
    }
    if ((!d1446) && (u_is_con(s1445, &n_lex_46_TName) && u_equals(u_field(s1445, 0), u_mk_lit("postfix")))) {
      d1446 = 1;
      {
        u_value t1448 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1448;
      }
    }
    if (!d1446) {
      d1446 = 1;
      {
        u_value s1449 = f_parse_46_second_token_1(*r_ts);
        int d1450 = 0;
        if ((!d1450) && (u_is_con(s1449, &n_lex_46_TName) && u_equals(u_field(s1449, 0), u_mk_lit("left")))) {
          d1450 = 1;
          {
            u_value t1451 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1451;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1450) && (u_is_con(s1449, &n_lex_46_TName) && u_equals(u_field(s1449, 0), u_mk_lit("right")))) {
          d1450 = 1;
          {
            u_value t1452 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1452;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1450) {
          d1450 = 1;
          {
          }
        }
        if (!d1450) {
          u_no_case(s1449);
        }
      }
    }
    if (!d1446) {
      u_no_case(s1445);
    }
    p_parse_46_advance(r_ts);
    u_value s1453 = f_parse_46_head_token_1(*r_ts);
    int d1454 = 0;
    if ((!d1454) && u_is_con(s1453, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1453, 0);
      d1454 = 1;
      {
        u_value t1455 = v_n;
        v_p = t1455;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1454) {
      d1454 = 1;
      {
        static struct u_object l_precedence_32_expec_1456 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1457 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1456, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1457)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1454) {
      u_no_case(s1453);
    }
    u_value t1458 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1459 = u_mk_bool(1);
    v_ops = t1458;
    v_go = t1459;
    u_value t1465 = v_go;
    while (u_truth(t1465)) {
      {
        u_value s1460 = f_parse_46_head_token_1(*r_ts);
        int d1461 = 0;
        if ((!d1461) && u_is_con(s1460, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1460, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1461 = 1;
            {
              u_value t1462 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1462;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1461) && u_is_con(s1460, &n_lex_46_TName)) {
          u_value v_n = u_field(s1460, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1461 = 1;
            {
              u_value t1463 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1463;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1461) {
          d1461 = 1;
          {
            u_value t1464 = u_mk_bool(0);
            v_go = t1464;
          }
        }
        if (!d1461) {
          u_no_case(s1460);
        }
      }
      t1465 = v_go;
    }
    u_value s1466 = v_ops;
    int d1467 = 0;
    if ((!d1467) && u_is_con(s1466, &n_list_46_Nil)) {
      d1467 = 1;
      {
        static struct u_object l_operator_32_expecte_1468 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1469 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1468, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1469)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1467) {
      d1467 = 1;
      {
        u_value t1470 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1470;
      }
    }
    if (!d1467) {
      u_no_case(s1466);
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
    u_value t1471 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1472 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1471;
    *r_fs = t1472;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
        u_value t1473 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        u_value t1474 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
        v_args = t1473;
        *r_fs = t1474;
        u_value t1477 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1477)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1475 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
            u_value t1476 = f_list_46__43__43__2(*r_fs, u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0)));
            v_args = t1475;
            *r_fs = t1476;
          }
          t1477 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1478 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), v_args);
    *r_k = t1478;
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1479 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1480 = 0;
    if ((!d1480) && (u_is_con(u_field(s1479, 0), &n_lex_46_TName) && (u_is_con(u_field(s1479, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1479, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1479, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1480 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1481 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1481;
        }
      }
    }
    if (!d1480) {
      d1480 = 1;
      {
        u_value t1482 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1482;
      }
    }
    if (!d1480) {
      u_no_case(s1479);
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
    static struct u_object l__58__61__1483 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1483), r_ts);
    static struct u_object l__124__1484 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1484)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1485 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1485;
    static struct u_object l__124__1486 = U_LIT("|");
    u_value t1488 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1486));
    while (u_truth(t1488)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1487 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1487;
      }
      static struct u_object l__124__1489 = U_LIT("|");
      t1488 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1489));
    }
    u_value t1490 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)));
    *r_cs = t1490;
  }
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1491 = v_ks;
  u_value t1492 = NULL;
  if ((!t1492) && u_is_con(s1491, &n_list_46_Nil)) {
    t1492 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1492) && (u_is_con(s1491, &n_list_46_Cons) && u_is_con(u_field(u_field(s1491, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1491, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1491, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1491, 0), 1);
    u_value v_rest = u_field(s1491, 1);
    t1492 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1492) {
    u_no_case(s1491);
  }
  return t1492;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1493 = v_fs;
  u_value t1494 = NULL;
  if ((!t1494) && u_is_con(s1493, &n_list_46_Nil)) {
    t1494 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1494) && (u_is_con(s1493, &n_list_46_Cons) && u_is_con(u_field(u_field(s1493, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1493, 0), 0), 0);
    u_value v_t = u_field(u_field(s1493, 0), 1);
    u_value v_rest = u_field(s1493, 1);
    t1494 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1494) && (u_is_con(s1493, &n_list_46_Cons) && u_is_con(u_field(u_field(s1493, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1493, 1);
    t1494 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1494) {
    u_no_case(s1493);
  }
  return t1494;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1502 = f_parse_46_fields_of_1(v_ks);
  u_value t1501;
  {
    u_value v_fs = t1502;
    u_value t1495 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1496 = &t1495;
    u_value l1497 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1497, &n_list_46_Cons)) {
      u_value v_f = u_field(l1497, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1498 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1499 = &t1498;
        u_value l1500 = v_fs;
        while (u_is_con(l1500, &n_list_46_Cons)) {
          u_value v_e = u_field(l1500, 0);
          if (u_truth(u_eq(f_parse_46_first_of_1(v_e), v_f))) {
            e1499 = u_append(e1499, v_e);
          }
          l1500 = u_field(l1500, 1);
        }
        e1496 = u_append(e1496, f_parse_46_accessor_5(v_t, v_ps, v_f, t1498, f_list_46_length_1(v_ks)));
      }
      l1497 = u_field(l1497, 1);
    }
    t1501 = t1495;
  }
  return t1501;
}

u_value f_parse_46_first_of_1(u_value v_p) {
  u_value s1503 = v_p;
  u_value t1504 = NULL;
  if (!t1504) {
    u_value v_a = u_field(s1503, 0);
    t1504 = v_a;
  }
  if (!t1504) {
    u_no_case(s1503);
  }
  return t1504;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1505 = U_LIT("x");
  u_value t1506 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1507 = &t1506;
  u_value l1508 = v_ps;
  while (u_is_con(l1508, &n_list_46_Cons)) {
    u_value v_p = u_field(l1508, 0);
    if (u_truth(u_mk_bool(1))) {
      e1507 = u_append(e1507, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1508 = u_field(l1508, 1);
  }
  u_value t1516 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1505), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1517 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1506);
  u_value t1515;
  {
    u_value v_x = t1516;
    u_value v_owner = t1517;
    static struct u_object l__46__1509 = U_LIT(".");
    static struct u_object l_x_1510 = U_LIT("x");
    u_value t1511 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1512 = &t1511;
    u_value l1513 = v_es;
    while (u_is_con(l1513, &n_list_46_Cons)) {
      u_value v_e = u_field(l1513, 0);
      if (u_truth(u_mk_bool(1))) {
        e1512 = u_append(e1512, f_parse_46_field_case_1(v_e));
      }
      l1513 = u_field(l1513, 1);
    }
    u_value t1514;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1514 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1514 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1515 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1509, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1510), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1511, t1514)));
  }
  return t1515;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1518 = v_es;
  u_value t1519 = NULL;
  if ((!t1519) && u_is_con(s1518, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1518, 0), 1), 3);
    t1519 = v_t;
  }
  if (!t1519) {
    t1519 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1519) {
    u_no_case(s1518);
  }
  return t1519;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1520 = v_e;
  u_value t1521 = NULL;
  if (!t1521) {
    u_value v_k = u_field(u_field(s1520, 1), 0);
    u_value v_i = u_field(u_field(s1520, 1), 1);
    u_value v_n = u_field(u_field(s1520, 1), 2);
    u_value t1522 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1523 = &t1522;
    u_value l1524 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1524, &n_list_46_Cons)) {
      u_value v_j = u_field(l1524, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1526;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1525 = U_LIT("y");
          t1526 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1525), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1526 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1523 = u_append(e1523, t1526);
      }
      l1524 = u_field(l1524, 1);
    }
    static struct u_object l_y_1527 = U_LIT("y");
    t1521 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1522), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1527), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1521) {
    u_no_case(s1520);
  }
  return t1521;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_error_1528 = U_LIT("error");
  static struct u_object l_no_32_field_32__1529 = U_LIT("no field ");
  static struct u_object l__32_of_32__1530 = U_LIT(" of ");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_error_1528), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(&l_no_32_field_32__1529, u_concat(v_f, u_concat(&l__32_of_32__1530, v_t))))), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_positions_1(u_value v_n) {
  u_value t1531;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1531 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1531 = f_list_46__43__43__2(f_parse_46_positions_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_n, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1531;
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
    u_value t1532 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1532;
    u_value t1534 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1534)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1533 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1533;
      }
      t1534 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1535 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1535), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1536 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1536), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1537 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1537;
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
    u_value t1538 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1539 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1540 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1541 = u_mk_bool(1);
    v_ins = t1538;
    v_outs = t1539;
    v_refs = t1540;
    v_first = t1541;
    static struct u_object l_in_1542 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1542))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1543 = u_mk_bool(0);
        v_first = t1543;
      }
    }
    static struct u_object l_out_1544 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1544, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1545 = u_mk_bool(0);
        v_first = t1545;
      }
    }
    static struct u_object l_ref_1546 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1546, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1547 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1547), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1548 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1548;
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
    u_value s1549 = f_parse_46_head_token_1(*r_ts);
    int d1550 = 0;
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("import")))) {
      d1550 = 1;
      {
        u_value t1551 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1551;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_loaded_modules_1(v_ld))))) {
          {
            u_value t1552 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1552;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_ns, v_ld, v_at, r_cs);
            u_value t1553 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1553;
          }
        }
      }
    }
    if ((!d1550) && u_is_con(s1549, &n_lex_46_TName)) {
      u_value v_w = u_field(s1549, 0);
      static struct u_object l_prefix_1555 = U_LIT("prefix");
      static struct u_object l_postfix_1556 = U_LIT("postfix");
      static struct u_object l_infix_1557 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1555, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1556, u_mk_con(&n_list_46_Cons, 2, &l_infix_1557, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1550 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1554 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1554;
        }
      }
    }
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("type")))) {
      d1550 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1558 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1558), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1559 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1559;
      }
    }
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("data")))) {
      d1550 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("function")))) {
      d1550 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1560 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1560;
      }
    }
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("procedure")))) {
      d1550 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1561 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1561;
      }
    }
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("eval")))) {
      d1550 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1562 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1562;
      }
    }
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("run")))) {
      d1550 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1563 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1563), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1564 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1564;
      }
    }
    if ((!d1550) && (u_is_con(s1549, &n_lex_46_TName) && u_equals(u_field(s1549, 0), u_mk_lit("test")))) {
      d1550 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1565 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1565), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1566 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1566;
      }
    }
    if (!d1550) {
      d1550 = 1;
      {
        static struct u_object l_declaration_32_expe_1567 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1568 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1567, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1568)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1550) {
      u_no_case(s1549);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_ns, u_value *r_ld) {
  {
    u_value s1569 = v_cs;
    int d1570 = 0;
    if ((!d1570) && u_is_con(s1569, &n_list_46_Nil)) {
      d1570 = 1;
      {
      }
    }
    if ((!d1570) && (u_is_con(s1569, &n_list_46_Cons) && u_is_con(u_field(s1569, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1569, 0), 0);
      u_value v_p = u_field(u_field(s1569, 0), 1);
      u_value v_ops = u_field(u_field(s1569, 0), 2);
      u_value v_more = u_field(s1569, 1);
      d1570 = 1;
      {
        u_value t1571 = f_list_46__43__43__2(*r_ns, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_ns = t1571;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1570) && (u_is_con(s1569, &n_list_46_Cons) && u_is_con(u_field(s1569, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1569, 0), 0);
      u_value v_ds = u_field(u_field(s1569, 0), 1);
      u_value v_more = u_field(s1569, 1);
      d1570 = 1;
      {
        u_value t1572 = u_mk_con(&n_parse_46_Loader, 4, f_list_46__43__43__2(f_parse_46_modules_of_1(u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds)), f_parse_46_loaded_modules_1(*r_ld)), f_parse_46_loading_1(*r_ld), f_parse_46_base_notations_1(*r_ld), f_parse_46_directory_1(*r_ld));
        *r_ld = t1572;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1570) && (u_is_con(s1569, &n_list_46_Cons) && u_is_con(u_field(s1569, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1569, 0), 0);
      u_value v_more = u_field(s1569, 1);
      d1570 = 1;
      {
        u_value t1573 = f_list_46__43__43__2(*r_ns, f_parse_46_own_notations_2(*r_ld, v_n));
        *r_ns = t1573;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1570) && u_is_con(s1569, &n_list_46_Cons)) {
      u_value v_more = u_field(s1569, 1);
      d1570 = 1;
      {
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if (!d1570) {
      u_no_case(s1569);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_ns, u_value *r_ld, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1574 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1574;
    u_value t1576 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1576)) {
      {
        p_parse_46_parse_command(*r_ns, *r_ld, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_ns, r_ld);
        u_value t1575 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1575;
      }
      t1576 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1577 = f_list_46_reverse_1(v_rev);
    *r_cs = t1577;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1578 = v_dir;
    int d1579 = 0;
    if ((!d1579) && u_is_con(s1578, &n_option_46_Some)) {
      u_value v_d = u_field(s1578, 0);
      d1579 = 1;
      {
        {
          struct u_handler *h1582 = u_push();
          if (setjmp(h1582->env) == 0) {
            {
              static struct u_object l__47__1580 = U_LIT("/");
              static struct u_object l__46_u_1581 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1580, u_concat(v_n, &l__46_u_1581))), r_text);
            }
            u_pop(h1582);
          } else {
            u_pop(h1582);
            int d1583 = 0;
            if ((!d1583) && (u_has_type(h1582, "os.OsError") && u_is_con(u_caught(h1582), &n_os_46_OsError))) {
              d1583 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1583) {
              u_reraise(h1582);
            }
          }
        }
      }
    }
    if ((!d1579) && u_is_con(s1578, &n_option_46_None)) {
      d1579 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1579) {
      u_no_case(s1578);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1584 = u_push();
      if (setjmp(h1584->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1584);
      } else {
        u_pop(h1584);
        int d1585 = 0;
        if ((!d1585) && (u_has_type(h1584, "os.OsError") && u_is_con(u_caught(h1584), &n_os_46_OsError))) {
          d1585 = 1;
          {
            static struct u_object l_unknown_32_module_32__1586 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1586, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1585) {
          u_reraise(h1584);
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
        static struct u_object l_cyclic_32_import_32_of_1587 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1587, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_directory_1(v_ld), v_at, &v_text);
    u_value t1588 = u_mk_con(&n_parse_46_Loader, 4, f_parse_46_loaded_modules_1(v_ld), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_loading_1(v_ld)), f_parse_46_base_notations_1(v_ld), f_parse_46_directory_1(v_ld));
    v_mld = t1588;
    u_value s1589 = f_parse_46_base_notations_1(v_ld);
    int d1590 = 0;
    if ((!d1590) && u_is_con(s1589, &n_option_46_Some)) {
      u_value v_b = u_field(s1589, 0);
      d1590 = 1;
      {
        u_value t1591 = v_b;
        v_mns = t1591;
      }
    }
    if ((!d1590) && u_is_con(s1589, &n_option_46_None)) {
      d1590 = 1;
      {
        u_value t1592 = v_ns;
        v_mns = t1592;
      }
    }
    if (!d1590) {
      u_no_case(s1589);
    }
    {
      struct u_handler *h1593 = u_push();
      if (setjmp(h1593->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mns, &v_mld, &v_mts);
        }
        u_pop(h1593);
      } else {
        u_pop(h1593);
        int d1594 = 0;
        if ((!d1594) && (u_has_type(h1593, "lex.LexError") && u_is_con(u_caught(h1593), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1593), 0);
          u_value v_i = u_field(u_caught(h1593), 1);
          d1594 = 1;
          {
            static struct u_object l__32_in_32__1595 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1595, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1594) && (u_has_type(h1593, "parse.ParseError") && u_is_con(u_caught(h1593), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1593), 0);
          u_value v_i = u_field(u_caught(h1593), 1);
          d1594 = 1;
          {
            static struct u_object l__32_in_32__1596 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1596, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1594) {
          u_reraise(h1593);
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
      struct u_handler *h1597 = u_push();
      if (setjmp(h1597->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1597);
      } else {
        u_pop(h1597);
        int d1598 = 0;
        if ((!d1598) && (u_has_type(h1597, "lex.LexError") && u_is_con(u_caught(h1597), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1597), 0);
          u_value v_i = u_field(u_caught(h1597), 1);
          d1598 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1598) {
          u_reraise(h1597);
        }
      }
    }
    u_value t1599 = v_ns;
    u_value t1600 = v_ld;
    v_ns1 = t1599;
    v_ld1 = t1600;
    p_parse_46_parse_commands(&v_cs, &v_ns1, &v_ld1, &v_ts);
    u_value t1601 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1601;
  }
}

void p_parse_46_prelude_scope(u_value v_pre, u_value *r_ns, u_value *r_ms) {
  {
    u_value v_ld = NULL;
    u_value t1602 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1603 = u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    *r_ns = t1602;
    v_ld = t1603;
    p_parse_46_declare_all(f_parse_46_commands_of_1(v_pre), r_ns, &v_ld);
    u_value t1604 = f_parse_46_loaded_modules_1(v_ld);
    *r_ms = t1604;
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
    static struct u_object l_prelude_1605 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1605))) {
      {
        static struct u_object l_prelude_1606 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1606, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1607 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1607, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1608 = v_xs;
  u_value t1609 = NULL;
  if ((!t1609) && (u_is_con(s1608, &n_list_46_Cons) && u_is_con(u_field(s1608, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1608, 0);
    t1609 = v_x;
  }
  if ((!t1609) && u_is_con(s1608, &n_list_46_Cons)) {
    u_value v_more = u_field(s1608, 1);
    t1609 = f_parse_46_last_part_1(v_more);
  }
  if (!t1609) {
    static struct u_object l__1610 = U_LIT("");
    t1609 = &l__1610;
  }
  if (!t1609) {
    u_no_case(s1608);
  }
  return t1609;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1615 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1614;
  {
    u_value v_f = t1615;
    u_value t1612 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1612)) {
      static struct u_object l__46_u_1611 = U_LIT(".u");
      t1612 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1611);
    }
    u_value t1613;
    if (u_truth(t1612)) {
      t1613 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1613 = v_f;
    }
    t1614 = t1613;
  }
  return t1614;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1620 = u_str_split(v_path, u_mk_char(47));
  u_value t1619;
  {
    u_value v_parts = t1620;
    u_value t1618;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1616 = U_LIT(".");
      t1618 = &l__46__1616;
    } else {
      static struct u_object l__47__1617 = U_LIT("/");
      t1618 = u_str_join(&l__47__1617, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1619 = t1618;
  }
  return t1619;
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
    static struct u_object l_prelude_1621 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1621, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

u_value f_parse_46_commands_of_1(u_value v_p) {
  u_value s1622 = v_p;
  u_value t1623 = NULL;
  if ((!t1623) && u_is_con(s1622, &n_syntax_46_Program)) {
    u_value v_cs = u_field(s1622, 0);
    t1623 = v_cs;
  }
  if (!t1623) {
    u_no_case(s1622);
  }
  return t1623;
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1624 = u_mk_tuple(2, v_cs, v_ds);
    int d1625 = 0;
    if ((!d1625) && (u_is_con(u_field(s1624, 0), &n_list_46_Cons) && u_is_con(u_field(s1624, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1624, 0), 0);
      u_value v_more = u_field(u_field(s1624, 0), 1);
      u_value v_d = u_field(u_field(s1624, 1), 0);
      u_value v_rest = u_field(u_field(s1624, 1), 1);
      d1625 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1625) {
      d1625 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1625) {
      u_no_case(s1624);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1626 = v_paths;
    int d1627 = 0;
    if ((!d1627) && u_is_con(s1626, &n_list_46_Nil)) {
      d1627 = 1;
      {
      }
    }
    if ((!d1627) && u_is_con(s1626, &n_list_46_Cons)) {
      u_value v_path = u_field(s1626, 0);
      u_value v_more = u_field(s1626, 1);
      d1627 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_parse_46_commands_of_1(v_p), f_parse_46_commands_of_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1627) {
      u_no_case(s1626);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1628 = v_paths;
    int d1629 = 0;
    if ((!d1629) && u_is_con(s1628, &n_list_46_Nil)) {
      d1629 = 1;
      {
      }
    }
    if ((!d1629) && u_is_con(s1628, &n_list_46_Cons)) {
      u_value v_path = u_field(s1628, 0);
      u_value v_more = u_field(s1628, 1);
      d1629 = 1;
      {
        u_value t1630 = u_mk_bool(0);
        v_failed = t1630;
        {
          struct u_handler *h1631 = u_push();
          if (setjmp(h1631->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1631);
          } else {
            u_pop(h1631);
            int d1632 = 0;
            if ((!d1632) && (u_has_type(h1631, "parse.ParseError") && u_is_con(u_caught(h1631), &n_parse_46_ParseError))) {
              d1632 = 1;
              {
                u_value t1633 = u_mk_bool(1);
                v_failed = t1633;
              }
            }
            if (!d1632) {
              u_reraise(h1631);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1629) {
      u_no_case(s1628);
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
  static struct u_object l_u_value_1634 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1634);
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
  static struct u_object l_u_value_1635 = U_LIT("u_value");
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1635));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1636 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1637 = &t1636;
  u_value l1638 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1638, &n_list_46_Cons)) {
    u_value v_c = u_field(l1638, 0);
    if (u_truth(u_mk_bool(1))) {
      e1637 = u_append(e1637, f_compile_46_mangle_char_1(v_c));
    }
    l1638 = u_field(l1638, 1);
  }
  return u_string(f_list_46_concat_1(t1636));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1639 = u_is_letter(v_c);
  if (!u_truth(t1639)) {
    t1639 = u_is_digit(v_c);
  }
  u_value t1640 = t1639;
  if (!u_truth(t1640)) {
    t1640 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1643;
  if (u_truth(t1640)) {
    t1643 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1641 = U_LIT("_");
    static struct u_object l___1642 = U_LIT("_");
    t1643 = u_chars(u_concat(&l___1641, u_concat(u_str_show(u_ord(v_c)), &l___1642)));
  }
  return t1643;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1644 = U_LIT("f_");
  static struct u_object l___1645 = U_LIT("_");
  return u_concat(&l_f__1644, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1645, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1646 = U_LIT("p_");
  return u_concat(&l_p__1646, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1647 = U_LIT("v_");
  return u_concat(&l_v__1647, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1648 = U_LIT("r_");
  return u_concat(&l_r__1648, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1649 = U_LIT("+");
  static struct u_object l_u_add_1650 = U_LIT("u_add");
  static struct u_object l__45__1651 = U_LIT("-");
  static struct u_object l_u_sub_1652 = U_LIT("u_sub");
  static struct u_object l__42__1653 = U_LIT("*");
  static struct u_object l_u_mul_1654 = U_LIT("u_mul");
  static struct u_object l__47__1655 = U_LIT("/");
  static struct u_object l_u_div_1656 = U_LIT("u_div");
  static struct u_object l__37__1657 = U_LIT("%");
  static struct u_object l_u_mod_1658 = U_LIT("u_mod");
  static struct u_object l__60__1659 = U_LIT("<");
  static struct u_object l_u_lt_1660 = U_LIT("u_lt");
  static struct u_object l__61__61__1661 = U_LIT("==");
  static struct u_object l_u_eq_1662 = U_LIT("u_eq");
  static struct u_object l_compare_1663 = U_LIT("compare");
  static struct u_object l_u_compare_1664 = U_LIT("u_compare");
  static struct u_object l_error_1665 = U_LIT("error");
  static struct u_object l_u_error_1666 = U_LIT("u_error");
  static struct u_object l_chars_1667 = U_LIT("chars");
  static struct u_object l_u_chars_1668 = U_LIT("u_chars");
  static struct u_object l_string_1669 = U_LIT("string");
  static struct u_object l_u_string_1670 = U_LIT("u_string");
  static struct u_object l_ord_1671 = U_LIT("ord");
  static struct u_object l_u_ord_1672 = U_LIT("u_ord");
  static struct u_object l_chr_1673 = U_LIT("chr");
  static struct u_object l_u_chr_1674 = U_LIT("u_chr");
  static struct u_object l__94__1675 = U_LIT("^");
  static struct u_object l_u_concat_1676 = U_LIT("u_concat");
  static struct u_object l_str_size_1677 = U_LIT("str_size");
  static struct u_object l_u_str_size_1678 = U_LIT("u_str_size");
  static struct u_object l_str_join_1679 = U_LIT("str_join");
  static struct u_object l_u_str_join_1680 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1681 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1682 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1683 = U_LIT("str_split");
  static struct u_object l_u_str_split_1684 = U_LIT("u_str_split");
  static struct u_object l_str_get_1685 = U_LIT("str_get");
  static struct u_object l_u_str_get_1686 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1687 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1688 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1689 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1690 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1691 = U_LIT("str_show");
  static struct u_object l_u_str_show_1692 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1693 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1694 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1695 = U_LIT("str_number");
  static struct u_object l_u_str_number_1696 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1697 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1698 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1699 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1700 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1701 = U_LIT("is_space");
  static struct u_object l_u_is_space_1702 = U_LIT("u_is_space");
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1649), u_mk_tuple(2, &l_u_add_1650, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1651), u_mk_tuple(2, &l_u_sub_1652, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1653), u_mk_tuple(2, &l_u_mul_1654, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1655), u_mk_tuple(2, &l_u_div_1656, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1657), u_mk_tuple(2, &l_u_mod_1658, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1659), u_mk_tuple(2, &l_u_lt_1660, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1661), u_mk_tuple(2, &l_u_eq_1662, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1663), u_mk_tuple(2, &l_u_compare_1664, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_error_1665), u_mk_tuple(2, &l_u_error_1666, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1667), u_mk_tuple(2, &l_u_chars_1668, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1669), u_mk_tuple(2, &l_u_string_1670, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1671), u_mk_tuple(2, &l_u_ord_1672, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1673), u_mk_tuple(2, &l_u_chr_1674, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1675), u_mk_tuple(2, &l_u_concat_1676, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1677), u_mk_tuple(2, &l_u_str_size_1678, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1679), u_mk_tuple(2, &l_u_str_join_1680, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1681), u_mk_tuple(2, &l_u_str_slice_1682, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1683), u_mk_tuple(2, &l_u_str_split_1684, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1685), u_mk_tuple(2, &l_u_str_get_1686, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1687), u_mk_tuple(2, &l_u_str_contains_1688, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1689), u_mk_tuple(2, &l_u_str_compare_1690, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1691), u_mk_tuple(2, &l_u_str_show_1692, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1693), u_mk_tuple(2, &l_u_str_quote_1694, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1695), u_mk_tuple(2, &l_u_str_number_1696, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1697), u_mk_tuple(2, &l_u_is_digit_1698, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1699), u_mk_tuple(2, &l_u_is_letter_1700, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1701), u_mk_tuple(2, &l_u_is_space_1702, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_funs_of_1(u_value v_gl) {
  u_value s1703 = v_gl;
  u_value t1704 = NULL;
  if ((!t1704) && u_is_con(s1703, &n_compile_46_Globals)) {
    u_value v_fs = u_field(s1703, 0);
    t1704 = v_fs;
  }
  if (!t1704) {
    u_no_case(s1703);
  }
  return t1704;
}

u_value f_compile_46_constrs_of_1(u_value v_gl) {
  u_value s1705 = v_gl;
  u_value t1706 = NULL;
  if ((!t1706) && u_is_con(s1705, &n_compile_46_Globals)) {
    u_value v_ks = u_field(s1705, 1);
    t1706 = v_ks;
  }
  if (!t1706) {
    u_no_case(s1705);
  }
  return t1706;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1707 = U_LIT("&");
  static struct u_object l_n__1708 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1707, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1708, f_compile_46_mangle_1(v_n))));
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
  u_value s1709 = v_n;
  u_value t1710 = NULL;
  if ((!t1710) && (u_is_con(s1709, &n_syntax_46_Name) && u_is_con(u_field(s1709, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1709, 0), 0);
    u_value v_b = u_field(s1709, 1);
    static struct u_object l_const_32_struct_32_u_n_1711 = U_LIT("const struct u_name");
    static struct u_object l_n__1712 = U_LIT("n_");
    t1710 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1711), u_concat(&l_n__1712, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1710) && (u_is_con(s1709, &n_syntax_46_Name) && u_is_con(u_field(s1709, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1709, 1);
    static struct u_object l_const_32_struct_32_u_n_1713 = U_LIT("const struct u_name");
    static struct u_object l_n__1714 = U_LIT("n_");
    static struct u_object l_NULL_1715 = U_LIT("NULL");
    t1710 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1713), u_concat(&l_n__1714, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1715), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1710) {
    u_no_case(s1709);
  }
  return t1710;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1716 = v_cs;
  u_value t1717 = NULL;
  if ((!t1717) && u_is_con(s1716, &n_list_46_Nil)) {
    t1717 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1717) && (u_is_con(s1716, &n_list_46_Cons) && u_is_con(u_field(s1716, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1716, 0), 2);
    u_value v_rest = u_field(s1716, 1);
    u_value t1718 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1719 = &t1718;
    u_value l1720 = v_ks;
    while (u_is_con(l1720, &n_list_46_Cons)) {
      u_value v_k = u_field(l1720, 0);
      if (u_truth(u_mk_bool(1))) {
        e1719 = u_append(e1719, f_check_46_constr_name_1(v_k));
      }
      l1720 = u_field(l1720, 1);
    }
    t1717 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1718, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1717) && u_is_con(s1716, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1716, 1);
    t1717 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1717) {
    u_no_case(s1716);
  }
  return t1717;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1721 = v_ks;
  u_value t1722 = NULL;
  if ((!t1722) && u_is_con(s1721, &n_list_46_Nil)) {
    t1722 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1722) && u_is_con(s1721, &n_list_46_Cons)) {
    u_value v_k = u_field(s1721, 0);
    u_value v_rest = u_field(s1721, 1);
    u_value t1723;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1723 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1723 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1722 = t1723;
  }
  if (!t1722) {
    u_no_case(s1721);
  }
  return t1722;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1724 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1725 = &t1724;
  u_value l1726 = v_xs;
  while (u_is_con(l1726, &n_list_46_Cons)) {
    u_value v_x = u_field(l1726, 0);
    if (u_truth(u_mk_bool(1))) {
      e1725 = u_append(e1725, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1726 = u_field(l1726, 1);
  }
  return t1724;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1727 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1727, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1728 = f_list_46_lookup_2(v_x, v_env);
  u_value t1729 = NULL;
  if ((!t1729) && u_is_con(s1728, &n_option_46_Some)) {
    u_value v_c = u_field(s1728, 0);
    t1729 = v_c;
  }
  if (!t1729) {
    t1729 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1729) {
    u_no_case(s1728);
  }
  return t1729;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1730 = v_c;
  u_value t1731 = NULL;
  if ((!t1731) && (u_is_con(s1730, &n_c_46_CeUnary) && u_equals(u_field(s1730, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1730, 1);
    t1731 = v_p;
  }
  if (!t1731) {
    static struct u_object l__38__1732 = U_LIT("&");
    t1731 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1732, v_c);
  }
  if (!t1731) {
    u_no_case(s1730);
  }
  return t1731;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1733 = v_t;
  u_value t1734 = NULL;
  if ((!t1734) && (u_is_con(s1733, &n_syntax_46_TSort) && u_is_con(u_field(s1733, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1733, 0);
    t1734 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1734) && u_is_con(s1733, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1733, 0);
    u_value v_args = u_field(s1733, 1);
    static struct u_object l__91__1735 = U_LIT("[");
    static struct u_object l__44__32__1736 = U_LIT(", ");
    u_value t1737 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1738 = &t1737;
    u_value l1739 = v_args;
    while (u_is_con(l1739, &n_list_46_Cons)) {
      u_value v_a = u_field(l1739, 0);
      if (u_truth(u_mk_bool(1))) {
        e1738 = u_append(e1738, f_compile_46_type_key_1(v_a));
      }
      l1739 = u_field(l1739, 1);
    }
    static struct u_object l__93__1740 = U_LIT("]");
    t1734 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1735, u_concat(u_str_join(&l__44__32__1736, t1737), &l__93__1740)));
  }
  if ((!t1734) && u_is_con(s1733, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1733, 0);
    u_value v_r = u_field(s1733, 1);
    static struct u_object l__40__1741 = U_LIT("(");
    static struct u_object l__44__32__1742 = U_LIT(", ");
    u_value t1743 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1744 = &t1743;
    u_value l1745 = v_args;
    while (u_is_con(l1745, &n_list_46_Cons)) {
      u_value v_a = u_field(l1745, 0);
      if (u_truth(u_mk_bool(1))) {
        e1744 = u_append(e1744, f_compile_46_type_key_1(v_a));
      }
      l1745 = u_field(l1745, 1);
    }
    static struct u_object l__41__32__45__62__32__1746 = U_LIT(") -> ");
    t1734 = u_concat(&l__40__1741, u_concat(u_str_join(&l__44__32__1742, t1743), u_concat(&l__41__32__45__62__32__1746, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1734) && u_is_con(s1733, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1733, 0);
    static struct u_object l__40__1747 = U_LIT("(");
    static struct u_object l__32__42__32__1748 = U_LIT(" * ");
    u_value t1749 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1750 = &t1749;
    u_value l1751 = v_ts;
    while (u_is_con(l1751, &n_list_46_Cons)) {
      u_value v_a = u_field(l1751, 0);
      if (u_truth(u_mk_bool(1))) {
        e1750 = u_append(e1750, f_compile_46_type_key_1(v_a));
      }
      l1751 = u_field(l1751, 1);
    }
    static struct u_object l__41__1752 = U_LIT(")");
    t1734 = u_concat(&l__40__1747, u_concat(u_str_join(&l__32__42__32__1748, t1749), &l__41__1752));
  }
  if (!t1734) {
    static struct u_object l__63__1753 = U_LIT("?");
    t1734 = &l__63__1753;
  }
  if (!t1734) {
    u_no_case(s1733);
  }
  return t1734;
}

u_value f_compile_46_program_cmds_1(u_value v_p) {
  u_value s1754 = v_p;
  u_value t1755 = NULL;
  if ((!t1755) && u_is_con(s1754, &n_syntax_46_Program)) {
    u_value v_cs = u_field(s1754, 0);
    t1755 = v_cs;
  }
  if (!t1755) {
    u_no_case(s1754);
  }
  return t1755;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1756 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1757 = &t1756;
  u_value l1758 = v_cs;
  while (u_is_con(l1758, &n_list_46_Cons)) {
    u_value v_c = u_field(l1758, 0);
    if (u_truth(u_mk_bool(1))) {
      e1757 = u_append(e1757, f_compile_46_fun_arities_1(v_c));
    }
    l1758 = u_field(l1758, 1);
  }
  u_value t1759 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1760 = &t1759;
  u_value l1761 = v_cs;
  while (u_is_con(l1761, &n_list_46_Cons)) {
    u_value v_c = u_field(l1761, 0);
    if (u_truth(u_mk_bool(1))) {
      e1760 = u_append(e1760, f_compile_46_constr_arities_1(v_c));
    }
    l1761 = u_field(l1761, 1);
  }
  u_value t1763 = f_list_46_concat_1(t1756);
  u_value t1764 = f_list_46_concat_1(t1759);
  u_value t1762;
  {
    u_value v_fs = t1763;
    u_value v_ks = t1764;
    t1762 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1762;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1765 = v_fs;
  u_value t1766 = NULL;
  if ((!t1766) && u_is_con(s1765, &n_list_46_Nil)) {
    t1766 = v_m;
  }
  if ((!t1766) && u_is_con(s1765, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1765, 0), 0);
    u_value v_k = u_field(u_field(s1765, 0), 1);
    u_value v_rest = u_field(s1765, 1);
    u_value s1767 = f_map_46_get_2(v_f, v_m);
    u_value t1768 = NULL;
    if ((!t1768) && u_is_con(s1767, &n_option_46_Some)) {
      u_value v_ks = u_field(s1767, 0);
      t1768 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1768) && u_is_con(s1767, &n_option_46_None)) {
      t1768 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1768) {
      u_no_case(s1767);
    }
    t1766 = t1768;
  }
  if (!t1766) {
    u_no_case(s1765);
  }
  return t1766;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1769 = v_gl;
  u_value t1770 = NULL;
  if ((!t1770) && u_is_con(s1769, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1769, 2);
    u_value s1771 = f_map_46_get_2(v_n, v_fm);
    u_value t1772 = NULL;
    if ((!t1772) && u_is_con(s1771, &n_option_46_Some)) {
      u_value v_ks = u_field(s1771, 0);
      t1772 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1772) && u_is_con(s1771, &n_option_46_None)) {
      t1772 = u_mk_bool(0);
    }
    if (!t1772) {
      u_no_case(s1771);
    }
    t1770 = t1772;
  }
  if (!t1770) {
    u_no_case(s1769);
  }
  return t1770;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1773 = v_gl;
  u_value t1774 = NULL;
  if ((!t1774) && u_is_con(s1773, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1773, 3);
    t1774 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1774) {
    u_no_case(s1773);
  }
  return t1774;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1775 = v_c;
  u_value t1776 = NULL;
  if ((!t1776) && u_is_con(s1775, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1775, 0);
    u_value v_xss = u_field(s1775, 2);
    t1776 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1776) {
    t1776 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1776) {
    u_no_case(s1775);
  }
  return t1776;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1777 = v_c;
  u_value t1778 = NULL;
  if ((!t1778) && u_is_con(s1777, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1777, 2);
    u_value t1779 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1780 = &t1779;
    u_value l1781 = v_ks;
    while (u_is_con(l1781, &n_list_46_Cons)) {
      u_value v_k = u_field(l1781, 0);
      if (u_truth(u_mk_bool(1))) {
        e1780 = u_append(e1780, f_compile_46_constr_arity_1(v_k));
      }
      l1781 = u_field(l1781, 1);
    }
    t1778 = t1779;
  }
  if (!t1778) {
    t1778 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1778) {
    u_no_case(s1777);
  }
  return t1778;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1782 = v_k;
  u_value t1783 = NULL;
  if ((!t1783) && u_is_con(s1782, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1782, 0);
    u_value v_ts = u_field(s1782, 1);
    t1783 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1783) {
    u_no_case(s1782);
  }
  return t1783;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1784 = v_xs;
  u_value t1785 = NULL;
  if ((!t1785) && u_is_con(s1784, &n_list_46_Nil)) {
    t1785 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1785) && u_is_con(s1784, &n_list_46_Cons)) {
    u_value v_x = u_field(s1784, 0);
    u_value v_rest = u_field(s1784, 1);
    t1785 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1785) {
    u_no_case(s1784);
  }
  return t1785;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1786 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1786, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1787 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1787, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1788 = v_cs;
  u_value t1789 = NULL;
  if ((!t1789) && u_is_con(s1788, &n_list_46_Nil)) {
    t1789 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1789) && (u_is_con(s1788, &n_list_46_Cons) && u_is_con(u_field(s1788, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1788, 0);
    t1789 = v_d;
  }
  if ((!t1789) && u_is_con(s1788, &n_list_46_Cons)) {
    u_value v_d = u_field(s1788, 0);
    u_value v_rest = u_field(s1788, 1);
    static struct u_object l__38__38__1790 = U_LIT("&&");
    t1789 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1790, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1789) {
    u_no_case(s1788);
  }
  return t1789;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1791 = v_l;
  u_value t1792 = NULL;
  if ((!t1792) && u_is_con(s1791, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1793 = U_LIT("u_mk_tuple");
    t1792 = f_c_46_c_call_2(&l_u_mk_tuple_1793, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1792) && u_is_con(s1791, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1791, 0);
    static struct u_object l_u_mk_bool_1794 = U_LIT("u_mk_bool");
    u_value t1795;
    if (u_truth(v_b)) {
      t1795 = u_mk_int(1);
    } else {
      t1795 = u_mk_int(0);
    }
    t1792 = f_c_46_c_call_2(&l_u_mk_bool_1794, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1795), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1792) && u_is_con(s1791, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1791, 0);
    u_value t1796 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1796)) {
      t1796 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1799;
    if (u_truth(t1796)) {
      static struct u_object l_u_mk_int_1797 = U_LIT("u_mk_int");
      t1799 = f_c_46_c_call_2(&l_u_mk_int_1797, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1798 = U_LIT("u_mk_int_str");
      t1799 = f_c_46_c_call_2(&l_u_mk_int_str_1798, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1792 = t1799;
  }
  if ((!t1792) && u_is_con(s1791, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1791, 0);
    static struct u_object l_u_mk_lit_1800 = U_LIT("u_mk_lit");
    t1792 = f_c_46_c_call_2(&l_u_mk_lit_1800, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1792) && u_is_con(s1791, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1791, 0);
    static struct u_object l_u_mk_char_1801 = U_LIT("u_mk_char");
    t1792 = f_c_46_c_call_2(&l_u_mk_char_1801, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1792) {
    u_no_case(s1791);
  }
  return t1792;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1802 = v_p;
  u_value t1803 = NULL;
  if ((!t1803) && u_is_con(s1802, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1802, 0);
    static struct u_object l_u_equals_1804 = U_LIT("u_equals");
    t1803 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1804, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1803) && u_is_con(s1802, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1802, 0);
    u_value v_ps = u_field(s1802, 1);
    static struct u_object l_u_is_con_1805 = U_LIT("u_is_con");
    t1803 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1805, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1803) && u_is_con(s1802, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1802, 0);
    t1803 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1803) {
    t1803 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1803) {
    u_no_case(s1802);
  }
  return t1803;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1806 = v_ps;
  u_value t1807 = NULL;
  if ((!t1807) && u_is_con(s1806, &n_list_46_Nil)) {
    t1807 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1807) && u_is_con(s1806, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1806, 0), 0);
    u_value v_i = u_field(u_field(s1806, 0), 1);
    u_value v_rest = u_field(s1806, 1);
    t1807 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1807) {
    u_no_case(s1806);
  }
  return t1807;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1808 = v_p;
  u_value t1809 = NULL;
  if ((!t1809) && (u_is_con(s1808, &n_syntax_46_PVar) && u_is_con(u_field(s1808, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1808, 0), 0);
    t1809 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1809) && u_is_con(s1808, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1808, 1);
    t1809 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1809) && u_is_con(s1808, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1808, 0);
    t1809 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1809) {
    t1809 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1809) {
    u_no_case(s1808);
  }
  return t1809;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1810 = v_ps;
  u_value t1811 = NULL;
  if ((!t1811) && u_is_con(s1810, &n_list_46_Nil)) {
    t1811 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1811) && u_is_con(s1810, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1810, 0), 0);
    u_value v_i = u_field(u_field(s1810, 0), 1);
    u_value v_rest = u_field(s1810, 1);
    t1811 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1811) {
    u_no_case(s1810);
  }
  return t1811;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1812 = v_p;
  u_value t1813 = NULL;
  if ((!t1813) && (u_is_con(s1812, &n_syntax_46_PVar) && u_is_con(u_field(s1812, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1812, 0), 0);
    t1813 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1813) && u_is_con(s1812, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1812, 1);
    u_value t1814 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1815 = &t1814;
    u_value l1816 = v_ps;
    while (u_is_con(l1816, &n_list_46_Cons)) {
      u_value v_q = u_field(l1816, 0);
      if (u_truth(u_mk_bool(1))) {
        e1815 = u_append(e1815, f_compile_46_pattern_names_1(v_q));
      }
      l1816 = u_field(l1816, 1);
    }
    t1813 = f_list_46_concat_1(t1814);
  }
  if ((!t1813) && u_is_con(s1812, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1812, 0);
    u_value t1817 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1818 = &t1817;
    u_value l1819 = v_ps;
    while (u_is_con(l1819, &n_list_46_Cons)) {
      u_value v_q = u_field(l1819, 0);
      if (u_truth(u_mk_bool(1))) {
        e1818 = u_append(e1818, f_compile_46_pattern_names_1(v_q));
      }
      l1819 = u_field(l1819, 1);
    }
    t1813 = f_list_46_concat_1(t1817);
  }
  if (!t1813) {
    t1813 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1813) {
    u_no_case(s1812);
  }
  return t1813;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1820 = v_e;
  u_value t1821 = NULL;
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_ELit)) {
    t1821 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1821) && (u_is_con(s1820, &n_syntax_46_EVar) && u_is_con(u_field(s1820, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1820, 0), 0);
    u_value t1822;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1822 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1822 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1821 = t1822;
  }
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1820, 0);
    u_value v_args = u_field(s1820, 1);
    u_value t1823 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1824 = &t1823;
    u_value l1825 = v_args;
    while (u_is_con(l1825, &n_list_46_Cons)) {
      u_value v_a = u_field(l1825, 0);
      if (u_truth(u_mk_bool(1))) {
        e1824 = u_append(e1824, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1825 = u_field(l1825, 1);
    }
    t1821 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1823));
  }
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1820, 0);
    u_value v_b = u_field(s1820, 1);
    t1821 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1820, 0);
    u_value v_b = u_field(s1820, 1);
    u_value t1826 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1827 = &t1826;
    u_value l1828 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1828, &n_list_46_Cons)) {
      u_value v_a = u_field(l1828, 0);
      if (u_truth(u_mk_bool(1))) {
        e1827 = u_append(e1827, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1828 = u_field(l1828, 1);
    }
    t1821 = f_list_46__43__43__2(f_list_46_concat_1(t1826), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1820, 0);
    u_value v_l = u_field(s1820, 1);
    u_value v_r = u_field(s1820, 2);
    t1821 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1820, 0);
    u_value v_cs = u_field(s1820, 1);
    u_value t1829 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1830 = &t1829;
    u_value l1831 = v_cs;
    while (u_is_con(l1831, &n_list_46_Cons)) {
      u_value v_b = u_field(l1831, 0);
      if (u_truth(u_mk_bool(1))) {
        e1830 = u_append(e1830, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1831 = u_field(l1831, 1);
    }
    t1821 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1829));
  }
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1820, 0);
    u_value v_xs = u_field(s1820, 1);
    u_value v_ls = u_field(s1820, 2);
    u_value v_c = u_field(s1820, 3);
    u_value t1832 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1833 = &t1832;
    u_value l1834 = v_ls;
    while (u_is_con(l1834, &n_list_46_Cons)) {
      u_value v_l = u_field(l1834, 0);
      if (u_truth(u_mk_bool(1))) {
        e1833 = u_append(e1833, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1834 = u_field(l1834, 1);
    }
    t1821 = f_list_46__43__43__2(f_list_46_concat_1(t1832), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1821) && u_is_con(s1820, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1820, 0);
    u_value t1835 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1836 = &t1835;
    u_value l1837 = v_es;
    while (u_is_con(l1837, &n_list_46_Cons)) {
      u_value v_a = u_field(l1837, 0);
      if (u_truth(u_mk_bool(1))) {
        e1836 = u_append(e1836, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1837 = u_field(l1837, 1);
    }
    t1821 = f_list_46_concat_1(t1835);
  }
  if (!t1821) {
    u_no_case(s1820);
  }
  return t1821;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1838 = v_b;
  u_value t1839 = NULL;
  if ((!t1839) && (u_is_con(s1838, &n_syntax_46_Branch) && u_is_con(u_field(s1838, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1838, 0);
    u_value v_g = u_field(u_field(s1838, 1), 0);
    u_value v_e = u_field(s1838, 2);
    t1839 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1839) && (u_is_con(s1838, &n_syntax_46_Branch) && u_is_con(u_field(s1838, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1838, 0);
    u_value v_e = u_field(s1838, 2);
    t1839 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1839) {
    u_no_case(s1838);
  }
  return t1839;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1840 = v_xss;
  u_value t1841 = NULL;
  if ((!t1841) && u_is_con(s1840, &n_list_46_Nil)) {
    t1841 = v_b;
  }
  if ((!t1841) && u_is_con(s1840, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1840, 0);
    u_value v_rest = u_field(s1840, 1);
    t1841 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1841) {
    u_no_case(s1840);
  }
  return t1841;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1842 = *r_g;
    int d1843 = 0;
    if ((!d1843) && u_is_con(s1842, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1842, 0);
      u_value v_ds = u_field(s1842, 1);
      d1843 = 1;
      {
        u_value t1844 = u_concat(v_stem, u_str_show(v_n));
        u_value t1845 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1844;
        *r_g = t1845;
      }
    }
    if (!d1843) {
      u_no_case(s1842);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1846 = *r_g;
    int d1847 = 0;
    if ((!d1847) && u_is_con(s1846, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1846, 0);
      u_value v_ds = u_field(s1846, 1);
      d1847 = 1;
      {
        u_value t1848 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1848;
      }
    }
    if (!d1847) {
      u_no_case(s1846);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1849 = U_LIT("u_mk_closure");
        static struct u_object l_w__1850 = U_LIT("w_");
        u_value t1851 = f_c_46_c_call_2(&l_u_mk_closure_1849, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1850, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1851;
      }
    } else {
      {
        u_value s1852 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1853 = 0;
        if ((!d1853) && (u_is_con(s1852, &n_option_46_Some) && u_equals(u_field(s1852, 0), u_mk_int(0)))) {
          d1853 = 1;
          {
            static struct u_object l_u_mk_con_1854 = U_LIT("u_mk_con");
            u_value t1855 = f_c_46_c_call_2(&l_u_mk_con_1854, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1855;
          }
        }
        if ((!d1853) && u_is_con(s1852, &n_option_46_Some)) {
          d1853 = 1;
          {
            static struct u_object l_u_mk_closure_1856 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1857 = U_LIT("wc_");
            u_value t1858 = f_c_46_c_call_2(&l_u_mk_closure_1856, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1857, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1858;
          }
        }
        if ((!d1853) && u_is_con(s1852, &n_option_46_None)) {
          d1853 = 1;
          {
            u_value s1859 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1860 = 0;
            if ((!d1860) && u_is_con(s1859, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1859, 0), 0);
              d1860 = 1;
              {
                static struct u_object l_u_mk_closure_1861 = U_LIT("u_mk_closure");
                static struct u_object l_w_1862 = U_LIT("w");
                u_value t1863 = f_c_46_c_call_2(&l_u_mk_closure_1861, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1862, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1863;
              }
            }
            if ((!d1860) && u_is_con(s1859, &n_option_46_None)) {
              d1860 = 1;
              {
                static struct u_object l_the_32_variable_32__1864 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1864, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1860) {
              u_no_case(s1859);
            }
          }
        }
        if (!d1853) {
          u_no_case(s1852);
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
    u_value s1865 = v_e;
    int d1866 = 0;
    if ((!d1866) && (u_is_con(s1865, &n_syntax_46_ELit) && u_is_con(u_field(s1865, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1865, 0), 0);
      d1866 = 1;
      {
        static struct u_object l_l__1867 = U_LIT("l_");
        static struct u_object l___1868 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1867, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), u_mk_int(16)))), &l___1868)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1869 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1870 = U_LIT("U_LIT");
        static struct u_object l__38__1871 = U_LIT("&");
        u_value t1872 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1869), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1870, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1873 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1871, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1872;
        *r_r = t1873;
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1865, 0);
      d1866 = 1;
      {
        u_value t1874 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1875 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1874;
        *r_r = t1875;
      }
    }
    if ((!d1866) && (u_is_con(s1865, &n_syntax_46_EVar) && u_is_con(u_field(s1865, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1865, 0), 0);
      u_value v_ty = u_field(u_field(s1865, 0), 1);
      d1866 = 1;
      {
        u_value s1876 = f_list_46_lookup_2(v_n, v_env);
        int d1877 = 0;
        if ((!d1877) && u_is_con(s1876, &n_option_46_Some)) {
          u_value v_c = u_field(s1876, 0);
          d1877 = 1;
          {
            u_value t1878 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1879 = v_c;
            *r_ss = t1878;
            *r_r = t1879;
          }
        }
        if ((!d1877) && u_is_con(s1876, &n_option_46_None)) {
          d1877 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1880 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1880;
          }
        }
        if (!d1877) {
          u_no_case(s1876);
        }
      }
    }
    if ((!d1866) && (u_is_con(s1865, &n_syntax_46_EApp) && (u_is_con(u_field(s1865, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1865, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1865, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1865, 0), 0), 1);
      u_value v_args = u_field(s1865, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1866 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1865, 0);
      u_value v_args = u_field(s1865, 1);
      d1866 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1881 = U_LIT("u_apply");
        u_value t1882 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1883 = f_c_46_c_call_2(&l_u_apply_1881, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1882;
        *r_r = t1883;
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1865, 0);
      u_value v_b = u_field(s1865, 1);
      d1866 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1865, 0);
      u_value v_b = u_field(s1865, 1);
      d1866 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1884 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1884, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1885 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1885;
        u_value t1886 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1886;
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1865, 0);
      u_value v_l = u_field(s1865, 1);
      u_value v_rr = u_field(s1865, 2);
      d1866 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1887 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1887, &v_t, r_g);
        u_value t1888 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1888;
        u_value t1889 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1889;
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1865, 0);
      u_value v_cases = u_field(s1865, 1);
      d1866 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1890 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1890, &v_d, r_g);
        static struct u_object l_t_1891 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1891, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1892 = U_LIT("NULL");
        u_value t1893 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1892))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1893;
        static struct u_object l__33__1894 = U_LIT("!");
        static struct u_object l_u_no_case_1895 = U_LIT("u_no_case");
        u_value t1896 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1894, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1895, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1896;
        u_value t1897 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1897;
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1865, 0);
      u_value v_xs = u_field(s1865, 1);
      u_value v_lists = u_field(s1865, 2);
      u_value v_c = u_field(s1865, 3);
      d1866 = 1;
      {
        static struct u_object l_t_1898 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1898, &v_t, r_g);
        static struct u_object l_e_1899 = U_LIT("e");
        p_compile_46_gen_name(&l_e_1899, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_1900 = U_LIT("u_mk_con");
        static struct u_object l__38__1901 = U_LIT("&");
        u_value t1902 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_1900, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__1901, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t1902;
        u_value t1903 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1903;
      }
    }
    if ((!d1866) && u_is_con(s1865, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1865, 0);
      d1866 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_1904 = U_LIT("u_mk_tuple");
        u_value t1905 = f_c_46_c_call_2(&l_u_mk_tuple_1904, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t1905;
      }
    }
    if (!d1866) {
      u_no_case(s1865);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s1906 = v_es;
    int d1907 = 0;
    if ((!d1907) && u_is_con(s1906, &n_list_46_Nil)) {
      d1907 = 1;
      {
        u_value t1908 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1909 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1908;
        *r_rs = t1909;
      }
    }
    if ((!d1907) && u_is_con(s1906, &n_list_46_Cons)) {
      u_value v_e = u_field(s1906, 0);
      u_value v_rest = u_field(s1906, 1);
      d1907 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t1910 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1911 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t1910;
        *r_rs = t1911;
      }
    }
    if (!d1907) {
      u_no_case(s1906);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s1912 = v_rs;
    int d1913 = 0;
    if ((!d1913) && u_is_con(s1912, &n_list_46_Nil)) {
      d1913 = 1;
      {
        u_value t1914 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1914;
      }
    }
    if ((!d1913) && u_is_con(s1912, &n_list_46_Cons)) {
      u_value v_r = u_field(s1912, 0);
      u_value v_rest = u_field(s1912, 1);
      d1913 = 1;
      {
        static struct u_object l_t_1915 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1915, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t1916 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t1916;
      }
    }
    if (!d1913) {
      u_no_case(s1912);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s1917 = u_mk_tuple(2, v_xs, v_ts);
  u_value t1918 = NULL;
  if ((!t1918) && (u_is_con(u_field(s1917, 0), &n_list_46_Cons) && (u_is_con(u_field(s1917, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1917, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s1917, 0), 0);
    u_value v_more = u_field(u_field(s1917, 0), 1);
    u_value v_t = u_field(u_field(u_field(s1917, 1), 0), 1);
    u_value v_rest = u_field(u_field(s1917, 1), 1);
    t1918 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t1918) {
    t1918 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1918) {
    u_no_case(s1917);
  }
  return t1918;
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
    u_value s1919 = v_args;
    int d1920 = 0;
    if ((!d1920) && (u_is_con(s1919, &n_list_46_Cons) && (u_is_con(u_field(s1919, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1919, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s1919, 0);
      u_value v_b = u_field(u_field(s1919, 1), 0);
      static struct u_object l_and_1927 = U_LIT("and");
      u_value t1929 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1927));
      if (!u_truth(t1929)) {
        static struct u_object l_or_1928 = U_LIT("or");
        t1929 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_1928));
      }
      if (u_truth(t1929)) {
        d1920 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_1921 = U_LIT("t");
          p_compile_46_gen_name(&l_t_1921, &v_t, r_g);
          static struct u_object l_and_1922 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1922)))) {
            {
              u_value t1923 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1923;
            }
          } else {
            {
              static struct u_object l__33__1924 = U_LIT("!");
              u_value t1925 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1924, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1925;
            }
          }
          u_value t1926 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t1926;
        }
      }
    }
    if (!d1920) {
      d1920 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t1930 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t1930)) {
          t1930 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t1930)) {
          {
            u_value t1931 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t1931;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_1932 = U_LIT("u_mk_con");
              u_value t1933 = f_c_46_c_call_2(&l_u_mk_con_1932, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t1933;
            }
          } else {
            {
              u_value s1934 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d1935 = 0;
              if ((!d1935) && u_is_con(s1934, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s1934, 0), 0);
                u_value v_k = u_field(u_field(s1934, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d1935 = 1;
                  {
                    u_value t1936 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t1936;
                  }
                }
              }
              if (!d1935) {
                d1935 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_1937 = U_LIT("u_apply");
                  u_value t1938 = f_c_46_c_call_2(&l_u_apply_1937, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t1938;
                }
              }
              if (!d1935) {
                u_no_case(s1934);
              }
            }
          }
        }
      }
    }
    if (!d1920) {
      u_no_case(s1919);
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
    u_value t1939 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1940 = &t1939;
    u_value l1941 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l1941, &n_list_46_Cons)) {
      u_value v_x = u_field(l1941, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e1940 = u_append(e1940, v_x);
      }
      l1941 = u_field(l1941, 1);
    }
    u_value t1942 = t1939;
    v_caps = t1942;
    static struct u_object l_lam_1943 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_1943, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_1944 = U_LIT("env");
    static struct u_object l_args_1945 = U_LIT("args");
    u_value t1946 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_1944), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_1945));
    v_decls = t1946;
    static struct u_object l_env_1947 = U_LIT("env");
    static struct u_object l_args_1948 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_1947), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_1948), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_1949 = U_LIT("u_mk_closure");
    u_value t1950 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1951 = &t1950;
    u_value l1952 = v_caps;
    while (u_is_con(l1952, &n_list_46_Cons)) {
      u_value v_x = u_field(l1952, 0);
      if (u_truth(u_mk_bool(1))) {
        e1951 = u_append(e1951, f_compile_46_var_expr_2(v_env, v_x));
      }
      l1952 = u_field(l1952, 1);
    }
    u_value t1953 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1954 = f_c_46_c_call_2(&l_u_mk_closure_1949, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t1950));
    *r_ss = t1953;
    *r_r = t1954;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s1955 = v_xs;
  u_value t1956 = NULL;
  if ((!t1956) && u_is_con(s1955, &n_list_46_Nil)) {
    t1956 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1956) && u_is_con(s1955, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s1955, 0), 0);
    u_value v_i = u_field(u_field(s1955, 0), 1);
    u_value v_rest = u_field(s1955, 1);
    t1956 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t1956) {
    u_no_case(s1955);
  }
  return t1956;
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
    u_value s1957 = v_cases;
    int d1958 = 0;
    if ((!d1958) && u_is_con(s1957, &n_list_46_Nil)) {
      d1958 = 1;
      {
        u_value t1959 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1959;
      }
    }
    if ((!d1958) && (u_is_con(s1957, &n_list_46_Cons) && u_is_con(u_field(s1957, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s1957, 0), 0);
      u_value v_guard = u_field(u_field(s1957, 0), 1);
      u_value v_e = u_field(u_field(s1957, 0), 2);
      u_value v_more = u_field(s1957, 1);
      d1958 = 1;
      {
        u_value t1960 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t1960;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t1961 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t1961;
        u_value s1962 = v_guard;
        int d1963 = 0;
        if ((!d1963) && u_is_con(s1962, &n_option_46_Some)) {
          u_value v_c = u_field(s1962, 0);
          d1963 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t1964 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t1964;
          }
        }
        if ((!d1963) && u_is_con(s1962, &n_option_46_None)) {
          d1963 = 1;
          {
          }
        }
        if (!d1963) {
          u_no_case(s1962);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__1965 = U_LIT("!");
        u_value t1966 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1965, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t1966;
      }
    }
    if (!d1958) {
      u_no_case(s1957);
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
    u_value s1967 = u_mk_tuple(2, v_xs, v_lists);
    int d1968 = 0;
    if ((!d1968) && (u_is_con(u_field(s1967, 0), &n_list_46_Cons) && u_is_con(u_field(s1967, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s1967, 0), 0);
      u_value v_more = u_field(u_field(s1967, 0), 1);
      u_value v_list = u_field(u_field(s1967, 1), 0);
      u_value v_others = u_field(u_field(s1967, 1), 1);
      d1968 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_1969 = U_LIT("l");
        p_compile_46_gen_name(&l_l_1969, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t1970 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1970;
        static struct u_object l_u_is_con_1971 = U_LIT("u_is_con");
        u_value t1972 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_1971, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1972;
      }
    }
    if (!d1968) {
      d1968 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_1973 = U_LIT("u_append");
        u_value t1974 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_1973, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1974;
      }
    }
    if (!d1968) {
      u_no_case(s1967);
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
    u_value s1975 = v_s;
    int d1976 = 0;
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SSkip)) {
      d1976 = 1;
      {
        u_value t1977 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1977;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s1975, 0);
      d1976 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1978 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t1978;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s1975, 0);
      u_value v_body = u_field(s1975, 1);
      d1976 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t1979 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1980 = &t1979;
        u_value l1981 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l1981, &n_list_46_Cons)) {
          u_value v_x = u_field(l1981, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_1982 = U_LIT("NULL");
            e1980 = u_append(e1980, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1982))));
          }
          l1981 = u_field(l1981, 1);
        }
        u_value t1983 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t1979, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t1983;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s1975, 0);
      u_value v_l = u_field(s1975, 1);
      u_value v_r = u_field(s1975, 2);
      d1976 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t1984 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1984;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s1975, 0);
      u_value v_b = u_field(s1975, 1);
      d1976 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_1985 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1985, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t1986 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1986;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s1975, 0);
      u_value v_ins = u_field(s1975, 1);
      u_value v_outs = u_field(s1975, 2);
      u_value v_refs = u_field(s1975, 3);
      d1976 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t1987 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e1988 = &t1987;
        u_value l1989 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l1989, &n_list_46_Cons)) {
          u_value v_x = u_field(l1989, 0);
          if (u_truth(u_mk_bool(1))) {
            e1988 = u_append(e1988, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l1989 = u_field(l1989, 1);
        }
        u_value t1990 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t1987))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1990;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s1975, 0);
      u_value v_cases = u_field(s1975, 1);
      d1976 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1991 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1991, &v_d, r_g);
        static struct u_object l_d_1992 = U_LIT("d");
        p_compile_46_gen_name(&l_d_1992, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t1993 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1993;
        static struct u_object l__33__1994 = U_LIT("!");
        static struct u_object l_u_no_case_1995 = U_LIT("u_no_case");
        u_value t1996 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1994, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1995, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1996;
      }
    }
    if ((!d1976) && (u_is_con(s1975, &n_syntax_46_SAssert) && (u_is_con(u_field(s1975, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s1975, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s1975, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s1975, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s1975, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s1975, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s1975, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s1975, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s1975, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s1975, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s1975, 0), 1), 1), 0);
      d1976 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_1997 = U_LIT("u_assert_equal");
        u_value t1998 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_1997, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1998;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s1975, 0);
      d1976 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_1999 = U_LIT("u_assert_true");
        u_value t2000 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_1999, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2000;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s1975, 0);
      u_value v_ty = u_field(s1975, 1);
      d1976 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2001 = U_LIT("u_raise");
        u_value t2002 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2001, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2002;
      }
    }
    if ((!d1976) && u_is_con(s1975, &n_syntax_46_STry)) {
      u_value v_body = u_field(s1975, 0);
      u_value v_hs = u_field(s1975, 1);
      d1976 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2003 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2003, &v_d, r_g);
        static struct u_object l_d_2004 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2004, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2005 = U_LIT("u_pop");
        static struct u_object l__33__2006 = U_LIT("!");
        static struct u_object l_u_reraise_2007 = U_LIT("u_reraise");
        u_value t2008 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2005, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2006, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2007, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2008;
        static struct u_object l_struct_32_u_handler_2009 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2010 = U_LIT("u_push");
        static struct u_object l__61__61__2011 = U_LIT("==");
        static struct u_object l_setjmp_2012 = U_LIT("setjmp");
        static struct u_object l_env_2013 = U_LIT("env");
        static struct u_object l_u_pop_2014 = U_LIT("u_pop");
        u_value t2015 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2009)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2010, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2011, f_c_46_c_call_2(&l_setjmp_2012, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2013), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2014, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2015;
      }
    }
    if (!d1976) {
      u_no_case(s1975);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2016 = v_xs;
    int d2017 = 0;
    if ((!d2017) && u_is_con(s2016, &n_list_46_Nil)) {
      d2017 = 1;
      {
        u_value t2018 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2018;
      }
    }
    if ((!d2017) && u_is_con(s2016, &n_list_46_Cons)) {
      u_value v_x = u_field(s2016, 0);
      u_value v_rest = u_field(s2016, 1);
      d2017 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2019 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2019;
      }
    }
    if (!d2017) {
      u_no_case(s2016);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2020 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2021 = NULL;
  if ((!t2021) && (u_is_con(u_field(s2020, 0), &n_list_46_Cons) && (u_is_con(u_field(s2020, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2020, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2020, 0), 0);
    u_value v_more = u_field(u_field(s2020, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2020, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2020, 1), 1);
    t2021 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2021) {
    t2021 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2021) {
    u_no_case(s2020);
  }
  return t2021;
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
    static struct u_object l_u_caught_2022 = U_LIT("u_caught");
    u_value t2023 = f_c_46_c_call_2(&l_u_caught_2022, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2023;
    u_value s2024 = v_hs;
    int d2025 = 0;
    if ((!d2025) && u_is_con(s2024, &n_list_46_Nil)) {
      d2025 = 1;
      {
        u_value t2026 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2026;
      }
    }
    if ((!d2025) && (u_is_con(s2024, &n_list_46_Cons) && u_is_con(u_field(s2024, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2024, 0), 0);
      u_value v_p = u_field(u_field(s2024, 0), 1);
      u_value v_guard = u_field(u_field(s2024, 0), 2);
      u_value v_s = u_field(u_field(s2024, 0), 3);
      u_value v_more = u_field(s2024, 1);
      d2025 = 1;
      {
        u_value t2027 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2027;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2028 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2028;
        u_value s2029 = v_guard;
        int d2030 = 0;
        if ((!d2030) && u_is_con(s2029, &n_option_46_Some)) {
          u_value v_c = u_field(s2029, 0);
          d2030 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2031 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2031;
          }
        }
        if ((!d2030) && u_is_con(s2029, &n_option_46_None)) {
          d2030 = 1;
          {
          }
        }
        if (!d2030) {
          u_no_case(s2029);
        }
        u_value s2032 = v_p;
        int d2033 = 0;
        if ((!d2033) && u_is_con(s2032, &n_syntax_46_PWild)) {
          d2033 = 1;
          {
            u_value t2034 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2034;
          }
        }
        if (!d2033) {
          d2033 = 1;
          {
            static struct u_object l_u_has_type_2035 = U_LIT("u_has_type");
            u_value t2036 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2035, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2036;
          }
        }
        if (!d2033) {
          u_no_case(s2032);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2037 = U_LIT("!");
        u_value t2038 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2037, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2038;
      }
    }
    if (!d2025) {
      u_no_case(s2024);
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
    u_value s2039 = v_cases;
    int d2040 = 0;
    if ((!d2040) && u_is_con(s2039, &n_list_46_Nil)) {
      d2040 = 1;
      {
        u_value t2041 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2041;
      }
    }
    if ((!d2040) && (u_is_con(s2039, &n_list_46_Cons) && u_is_con(u_field(s2039, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2039, 0), 0);
      u_value v_guard = u_field(u_field(s2039, 0), 1);
      u_value v_s = u_field(u_field(s2039, 0), 2);
      u_value v_more = u_field(s2039, 1);
      d2040 = 1;
      {
        u_value t2042 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2042;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2043 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2043;
        u_value s2044 = v_guard;
        int d2045 = 0;
        if ((!d2045) && u_is_con(s2044, &n_option_46_Some)) {
          u_value v_c = u_field(s2044, 0);
          d2045 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2046 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2046;
          }
        }
        if ((!d2045) && u_is_con(s2044, &n_option_46_None)) {
          d2045 = 1;
          {
          }
        }
        if (!d2045) {
          u_no_case(s2044);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2047 = U_LIT("!");
        u_value t2048 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2047, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2048;
      }
    }
    if (!d2040) {
      u_no_case(s2039);
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
    u_value s2049 = v_c;
    int d2050 = 0;
    if ((!d2050) && u_is_con(s2049, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2049, 0);
      u_value v_xss = u_field(s2049, 2);
      u_value v_b = u_field(s2049, 4);
      d2050 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2051 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2052 = &t2051;
        u_value l2053 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2053, &n_list_46_Cons)) {
          u_value v_x = u_field(l2053, 0);
          if (u_truth(u_mk_bool(1))) {
            e2052 = u_append(e2052, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2053 = u_field(l2053, 1);
        }
        u_value t2054 = t2051;
        v_params = t2054;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2055 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2055;
          }
        } else {
          {
            u_value t2056 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2056;
          }
        }
        u_value t2057 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2058 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2057;
        *r_ms = t2058;
      }
    }
    if ((!d2050) && u_is_con(s2049, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2049, 0);
      u_value v_ins = u_field(s2049, 2);
      u_value v_outs = u_field(s2049, 3);
      u_value v_refs = u_field(s2049, 4);
      u_value v_body = u_field(s2049, 5);
      d2050 = 1;
      {
        u_value t2059 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2060 = &t2059;
        u_value l2061 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2061, &n_list_46_Cons)) {
          u_value v_x = u_field(l2061, 0);
          if (u_truth(u_mk_bool(1))) {
            e2060 = u_append(e2060, f_compile_46_pointer_1(v_x));
          }
          l2061 = u_field(l2061, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2059), v_body, &v_bs, r_g);
        u_value t2062 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2063 = &t2062;
        u_value l2064 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2064, &n_list_46_Cons)) {
          u_value v_x = u_field(l2064, 0);
          if (u_truth(u_mk_bool(1))) {
            e2063 = u_append(e2063, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2064 = u_field(l2064, 1);
        }
        u_value t2065 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2066 = &t2065;
        u_value l2067 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2067, &n_list_46_Cons)) {
          u_value v_x = u_field(l2067, 0);
          if (u_truth(u_mk_bool(1))) {
            e2066 = u_append(e2066, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2067 = u_field(l2067, 1);
        }
        u_value t2068 = f_list_46__43__43__2(t2062, t2065);
        v_params = t2068;
        u_value t2069 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2070 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2069;
        *r_ms = t2070;
      }
    }
    if ((!d2050) && u_is_con(s2049, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2049, 0);
      d2050 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2071 = U_LIT("u_print_line");
        u_value t2072 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2073 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2071, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2072;
        *r_ms = t2073;
      }
    }
    if ((!d2050) && (u_is_con(s2049, &n_syntax_46_CRun) && u_is_con(u_field(s2049, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2049, 1), 0);
      u_value v_body = u_field(u_field(s2049, 1), 1);
      d2050 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2074 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2075 = &t2074;
        u_value l2076 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2076, &n_list_46_Cons)) {
          u_value v_x = u_field(l2076, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2077 = U_LIT("NULL");
            e2075 = u_append(e2075, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2077))));
          }
          l2076 = u_field(l2076, 1);
        }
        u_value t2078 = t2074;
        v_copy = t2078;
        u_value t2079 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2080 = &t2079;
        u_value l2081 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2081, &n_list_46_Cons)) {
          u_value v_x = u_field(l2081, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2082 = U_LIT("u_print_binding");
            e2080 = u_append(e2080, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2082, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2081 = u_field(l2081, 1);
        }
        u_value t2083 = t2079;
        v_back = t2083;
        u_value t2084 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2085 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2084;
        *r_ms = t2085;
      }
    }
    if ((!d2050) && u_is_con(s2049, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2049, 1);
      d2050 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2086 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2087 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2086;
        *r_ms = t2087;
      }
    }
    if ((!d2050) && u_is_con(s2049, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2049, 1);
      d2050 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2088 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2089 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2088;
        *r_ms = t2089;
      }
    }
    if (!d2050) {
      d2050 = 1;
      {
        u_value t2090 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2091 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2090;
        *r_ms = t2091;
      }
    }
    if (!d2050) {
      u_no_case(s2049);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2092 = v_cs;
    int d2093 = 0;
    if ((!d2093) && u_is_con(s2092, &n_list_46_Nil)) {
      d2093 = 1;
      {
        u_value t2094 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2095 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2094;
        *r_ms = t2095;
      }
    }
    if ((!d2093) && u_is_con(s2092, &n_list_46_Cons)) {
      u_value v_c = u_field(s2092, 0);
      u_value v_rest = u_field(s2092, 1);
      d2093 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2096 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2097 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2096;
        *r_ms = t2097;
      }
    }
    if (!d2093) {
      u_no_case(s2092);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_static_32_u_value_2098 = U_LIT("static u_value");
  static struct u_object l_cache_2099 = U_LIT("cache");
  static struct u_object l_NULL_2100 = U_LIT("NULL");
  static struct u_object l__61__61__2101 = U_LIT("==");
  static struct u_object l_cache_2102 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2103 = U_LIT("U_COMPUTING");
  static struct u_object l_u_fail_2104 = U_LIT("u_fail");
  static struct u_object l_recursive_32_initia_2105 = U_LIT("recursive initialization of ");
  static struct u_object l_cache_2106 = U_LIT("cache");
  static struct u_object l_cache_2107 = U_LIT("cache");
  static struct u_object l_cache_2108 = U_LIT("cache");
  static struct u_object l_U_COMPUTING_2109 = U_LIT("U_COMPUTING");
  static struct u_object l_cache_2110 = U_LIT("cache");
  static struct u_object l_cache_2111 = U_LIT("cache");
  return f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2098), &l_cache_2099, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2100))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2101, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2102), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2103)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2104, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2105, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2106), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2107))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2108), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2109)), u_mk_con(&n_list_46_Nil, 0))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2110), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2111))), u_mk_con(&n_list_46_Nil, 0)))));
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2112 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2113 = &t2112;
  u_value l2114 = f_compile_46_funs_of_1(v_gl);
  while (u_is_con(l2114, &n_list_46_Cons)) {
    u_value v_p = u_field(l2114, 0);
    if (u_truth(u_mk_bool(1))) {
      e2113 = u_append(e2113, f_compile_46_fun_wrapper_1(v_p));
    }
    l2114 = u_field(l2114, 1);
  }
  u_value t2115 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2116 = &t2115;
  u_value l2117 = f_compile_46_constrs_of_1(v_gl);
  while (u_is_con(l2117, &n_list_46_Cons)) {
    u_value v_p = u_field(l2117, 0);
    if (u_truth(u_mk_bool(1))) {
      e2116 = u_append(e2116, f_compile_46_constr_wrapper_1(v_p));
    }
    l2117 = u_field(l2117, 1);
  }
  u_value t2118 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2119 = &t2118;
  u_value l2120 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2120, &n_list_46_Cons)) {
    u_value v_p = u_field(l2120, 0);
    if (u_truth(u_mk_bool(1))) {
      e2119 = u_append(e2119, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2120 = u_field(l2120, 1);
  }
  return f_list_46__43__43__2(t2112, f_list_46__43__43__2(f_list_46_concat_1(t2115), t2118));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2121 = v_p;
  u_value t2122 = NULL;
  if (!t2122) {
    u_value v_n = u_field(s2121, 0);
    u_value v_k = u_field(s2121, 1);
    static struct u_object l_w__2123 = U_LIT("w_");
    t2122 = f_compile_46_wrapper_3(u_concat(&l_w__2123, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2122) {
    u_no_case(s2121);
  }
  return t2122;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2124 = v_p;
  u_value t2125 = NULL;
  if (!t2125) {
    u_value v_f = u_field(s2124, 0);
    u_value v_k = u_field(s2124, 1);
    static struct u_object l_w_2126 = U_LIT("w");
    t2125 = f_compile_46_wrapper_3(u_concat(&l_w_2126, v_f), v_f, v_k);
  }
  if (!t2125) {
    u_no_case(s2124);
  }
  return t2125;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2127 = U_LIT("env");
  static struct u_object l_args_2128 = U_LIT("args");
  u_value t2129 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2130 = &t2129;
  u_value l2131 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2131, &n_list_46_Cons)) {
    u_value v_i = u_field(l2131, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2132 = U_LIT("args");
      e2130 = u_append(e2130, f_compile_46_index_2(&l_args_2132, v_i));
    }
    l2131 = u_field(l2131, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2127), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2128), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2129))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2133 = v_p;
  u_value t2134 = NULL;
  if ((!t2134) && u_equals(u_field(s2133, 1), u_mk_int(0))) {
    t2134 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2134) {
    u_value v_n = u_field(s2133, 0);
    u_value v_k = u_field(s2133, 1);
    static struct u_object l_wc__2135 = U_LIT("wc_");
    static struct u_object l_env_2136 = U_LIT("env");
    static struct u_object l_args_2137 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2138 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2139 = U_LIT("args");
    t2134 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2135, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2136), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2137), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2138, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2139), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2134) {
    u_no_case(s2133);
  }
  return t2134;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2140;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2140 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2140 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2140;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2141 = v_d;
  u_value t2142 = NULL;
  if ((!t2142) && u_is_con(s2141, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2141, 0);
    u_value v_f = u_field(s2141, 1);
    u_value v_ps = u_field(s2141, 2);
    t2142 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2142) {
    t2142 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2142) {
    u_no_case(s2141);
  }
  return t2142;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2143 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2143;
    u_value t2144 = f_compile_46_globals_1(f_compile_46_program_cmds_1(v_q));
    v_gl = t2144;
    p_compile_46_compile_cmds(v_gl, f_compile_46_program_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2145 = v_g;
    int d2146 = 0;
    if ((!d2146) && u_is_con(s2145, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2145, 1);
      d2146 = 1;
      {
        u_value t2147 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2147;
      }
    }
    if (!d2146) {
      u_no_case(s2145);
    }
    static struct u_object l_ulang_46_h_2148 = U_LIT("ulang.h");
    u_value t2149 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2150 = &t2149;
    u_value l2151 = v_fs;
    while (u_is_con(l2151, &n_list_46_Cons)) {
      u_value v_d = u_field(l2151, 0);
      if (u_truth(u_mk_bool(1))) {
        e2150 = u_append(e2150, f_compile_46_prototype_1(v_d));
      }
      l2151 = u_field(l2151, 1);
    }
    static struct u_object l_main_2152 = U_LIT("main");
    static struct u_object l_argc_2153 = U_LIT("argc");
    static struct u_object l_char_2154 = U_LIT("char");
    static struct u_object l_argv_2155 = U_LIT("argv");
    static struct u_object l_u_args_2156 = U_LIT("u_args");
    static struct u_object l_argc_2157 = U_LIT("argc");
    static struct u_object l_argv_2158 = U_LIT("argv");
    u_value t2159 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2148), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_compile_46_program_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2149), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2152, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2153), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2154))), &l_argv_2155), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2156, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2157), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2158), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2159;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2160 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2160, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2161 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2161, f_compile_46_program_cmds_1(v_pre)), f_compile_46_program_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam969(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2162 = v_args;
    int d2163 = 0;
    if ((!d2163) && u_is_con(s2162, &n_list_46_Nil)) {
      d2163 = 1;
      {
      }
    }
    if ((!d2163) && (u_is_con(s2162, &n_list_46_Cons) && (u_is_con(u_field(s2162, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2162, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2162, 0);
      u_value v_target = u_field(u_field(s2162, 1), 0);
      d2163 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2163) {
      d2163 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2164 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2164), "os.OsError");
      }
    }
    if (!d2163) {
      u_no_case(s2162);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
