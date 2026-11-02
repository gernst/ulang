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

u_value lam1011(u_value *env, u_value *args);

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
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_list_88 = U_LIT("list");
  static struct u_object l_List_89 = U_LIT("List");
  u_pop(handler);
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
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_list_90 = U_LIT("list");
  static struct u_object l_Nil_91 = U_LIT("Nil");
  u_pop(handler);
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
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_list_92 = U_LIT("list");
  static struct u_object l_Cons_93 = U_LIT("Cons");
  u_pop(handler);
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
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_option_94 = U_LIT("option");
  static struct u_object l_Option_95 = U_LIT("Option");
  u_pop(handler);
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
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_option_96 = U_LIT("option");
  static struct u_object l_None_97 = U_LIT("None");
  u_pop(handler);
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
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_option_98 = U_LIT("option");
  static struct u_object l_Some_99 = U_LIT("Some");
  u_pop(handler);
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
  struct u_handler *handler = u_push();
  if (setjmp(handler->env) != 0) {
    u_pop(handler);
    cache = NULL;
    u_reraise(handler);
  }
  static struct u_object l_os_100 = U_LIT("os");
  static struct u_object l_OsError_101 = U_LIT("OsError");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_os_100), &l_OsError_101);
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
  static struct u_object l_prelude_102 = U_LIT("prelude");
  static struct u_object l_DivisionByZero_103 = U_LIT("DivisionByZero");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_102), &l_DivisionByZero_103);
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
  static struct u_object l_prelude_104 = U_LIT("prelude");
  static struct u_object l_NoIndex_105 = U_LIT("NoIndex");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_104), &l_NoIndex_105);
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
  static struct u_object l_prelude_106 = U_LIT("prelude");
  static struct u_object l_NoCharacter_107 = U_LIT("NoCharacter");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_106), &l_NoCharacter_107);
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
  static struct u_object l_prelude_108 = U_LIT("prelude");
  static struct u_object l_MissingField_109 = U_LIT("MissingField");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, &l_prelude_108), &l_MissingField_109);
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
  static struct u_object l__44__110 = U_LIT(",");
  u_pop(handler);
  cache = f_syntax_46_unqualified_1(&l__44__110);
  return cache;
}

u_value f_syntax_46_names_1(u_value v_xs) {
  u_value t111 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e112 = &t111;
  u_value l113 = v_xs;
  while (u_is_con(l113, &n_list_46_Cons)) {
    u_value v_x = u_field(l113, 0);
    if (u_truth(u_mk_bool(1))) {
      e112 = u_append(e112, f_syntax_46_Var_46_name_1(v_x));
    }
    l113 = u_field(l113, 1);
  }
  return t111;
}

u_value f_syntax_46_types_1(u_value v_xs) {
  u_value t114 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e115 = &t114;
  u_value l116 = v_xs;
  while (u_is_con(l116, &n_list_46_Cons)) {
    u_value v_x = u_field(l116, 0);
    if (u_truth(u_mk_bool(1))) {
      e115 = u_append(e115, f_syntax_46_Var_46_typ_1(v_x));
    }
    l116 = u_field(l116, 1);
  }
  return t114;
}

u_value f_syntax_46_arity_1(u_value v_t) {
  u_value s117 = v_t;
  u_value t118 = NULL;
  if ((!t118) && u_is_con(s117, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s117, 0);
    t118 = f_list_46_length_1(v_args);
  }
  if (!t118) {
    t118 = f_prelude_46__45__1(u_mk_int(1));
  }
  if (!t118) {
    u_no_case(s117);
  }
  return t118;
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
  u_value s119 = v_m;
  u_value t120 = NULL;
  if ((!t120) && u_is_con(s119, &n_map_46_MLeaf)) {
    t120 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t120) && u_is_con(s119, &n_map_46_MNode)) {
    u_value v_l = u_field(s119, 0);
    u_value v_j = u_field(s119, 1);
    u_value v_v = u_field(s119, 2);
    u_value v_r = u_field(s119, 3);
    u_value t124 = u_compare(v_k, v_j);
    u_value t123;
    {
      u_value v_c = t124;
      u_value t122;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t122 = f_map_46_get_2(v_k, v_l);
      } else {
        u_value t121;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t121 = f_map_46_get_2(v_k, v_r);
        } else {
          t121 = u_mk_con(&n_option_46_Some, 1, v_v);
        }
        t122 = t121;
      }
      t123 = t122;
    }
    t120 = t123;
  }
  if (!t120) {
    u_no_case(s119);
  }
  return t120;
}

u_value f_map_46_has_2(u_value v_k, u_value v_m) {
  return f_prelude_46__33__61__2(f_map_46_get_2(v_k, v_m), u_mk_con(&n_option_46_None, 0));
}

u_value f_map_46_put_3(u_value v_k, u_value v_v, u_value v_m) {
  u_value s125 = v_m;
  u_value t126 = NULL;
  if ((!t126) && u_is_con(s125, &n_map_46_MLeaf)) {
    t126 = u_mk_con(&n_map_46_MNode, 5, u_mk_con(&n_map_46_MLeaf, 0), v_k, v_v, u_mk_con(&n_map_46_MLeaf, 0), u_mk_int(1));
  }
  if ((!t126) && u_is_con(s125, &n_map_46_MNode)) {
    u_value v_l = u_field(s125, 0);
    u_value v_j = u_field(s125, 1);
    u_value v_w = u_field(s125, 2);
    u_value v_r = u_field(s125, 3);
    u_value v_h = u_field(s125, 4);
    u_value t130 = u_compare(v_k, v_j);
    u_value t129;
    {
      u_value v_c = t130;
      u_value t128;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t128 = f_map_46_balance_4(f_map_46_put_3(v_k, v_v, v_l), v_j, v_w, v_r);
      } else {
        u_value t127;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t127 = f_map_46_balance_4(v_l, v_j, v_w, f_map_46_put_3(v_k, v_v, v_r));
        } else {
          t127 = u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, v_h);
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

u_value f_map_46_entries_1(u_value v_m) {
  return f_map_46_entries_onto_2(v_m, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_map_46_entries_onto_2(u_value v_m, u_value v_rest) {
  u_value s131 = v_m;
  u_value t132 = NULL;
  if ((!t132) && u_is_con(s131, &n_map_46_MLeaf)) {
    t132 = v_rest;
  }
  if ((!t132) && u_is_con(s131, &n_map_46_MNode)) {
    u_value v_l = u_field(s131, 0);
    u_value v_k = u_field(s131, 1);
    u_value v_v = u_field(s131, 2);
    u_value v_r = u_field(s131, 3);
    t132 = f_map_46_entries_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_v), f_map_46_entries_onto_2(v_r, v_rest)));
  }
  if (!t132) {
    u_no_case(s131);
  }
  return t132;
}

u_value f_map_46_size_1(u_value v_m) {
  u_value s133 = v_m;
  u_value t134 = NULL;
  if ((!t134) && u_is_con(s133, &n_map_46_MLeaf)) {
    t134 = u_mk_int(0);
  }
  if ((!t134) && u_is_con(s133, &n_map_46_MNode)) {
    u_value v_l = u_field(s133, 0);
    u_value v_r = u_field(s133, 3);
    t134 = u_add(u_add(f_map_46_size_1(v_l), u_mk_int(1)), f_map_46_size_1(v_r));
  }
  if (!t134) {
    u_no_case(s133);
  }
  return t134;
}

u_value f_map_46_height_1(u_value v_m) {
  u_value s135 = v_m;
  u_value t136 = NULL;
  if ((!t136) && u_is_con(s135, &n_map_46_MLeaf)) {
    t136 = u_mk_int(0);
  }
  if ((!t136) && u_is_con(s135, &n_map_46_MNode)) {
    u_value v_h = u_field(s135, 4);
    t136 = v_h;
  }
  if (!t136) {
    u_no_case(s135);
  }
  return t136;
}

u_value f_map_46_mnode_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  return u_mk_con(&n_map_46_MNode, 5, v_l, v_k, v_v, v_r, f_map_46_mnode_height_2(v_l, v_r));
}

u_value f_map_46_rotate_left_1(u_value v_m) {
  u_value s137 = v_m;
  u_value t138 = NULL;
  if ((!t138) && (u_is_con(s137, &n_map_46_MNode) && u_is_con(u_field(s137, 3), &n_map_46_MNode))) {
    u_value v_a = u_field(s137, 0);
    u_value v_j = u_field(s137, 1);
    u_value v_v = u_field(s137, 2);
    u_value v_b = u_field(u_field(s137, 3), 0);
    u_value v_k = u_field(u_field(s137, 3), 1);
    u_value v_w = u_field(u_field(s137, 3), 2);
    u_value v_c = u_field(u_field(s137, 3), 3);
    t138 = f_map_46_mnode_4(f_map_46_mnode_4(v_a, v_j, v_v, v_b), v_k, v_w, v_c);
  }
  if (!t138) {
    t138 = v_m;
  }
  if (!t138) {
    u_no_case(s137);
  }
  return t138;
}

u_value f_map_46_rotate_right_1(u_value v_m) {
  u_value s139 = v_m;
  u_value t140 = NULL;
  if ((!t140) && (u_is_con(s139, &n_map_46_MNode) && u_is_con(u_field(s139, 0), &n_map_46_MNode))) {
    u_value v_a = u_field(u_field(s139, 0), 0);
    u_value v_j = u_field(u_field(s139, 0), 1);
    u_value v_v = u_field(u_field(s139, 0), 2);
    u_value v_b = u_field(u_field(s139, 0), 3);
    u_value v_k = u_field(s139, 1);
    u_value v_w = u_field(s139, 2);
    u_value v_c = u_field(s139, 3);
    t140 = f_map_46_mnode_4(v_a, v_j, v_v, f_map_46_mnode_4(v_b, v_k, v_w, v_c));
  }
  if (!t140) {
    t140 = v_m;
  }
  if (!t140) {
    u_no_case(s139);
  }
  return t140;
}

u_value f_map_46_left_of_1(u_value v_m) {
  u_value s141 = v_m;
  u_value t142 = NULL;
  if ((!t142) && u_is_con(s141, &n_map_46_MNode)) {
    u_value v_l = u_field(s141, 0);
    t142 = v_l;
  }
  if (!t142) {
    t142 = v_m;
  }
  if (!t142) {
    u_no_case(s141);
  }
  return t142;
}

u_value f_map_46_right_of_1(u_value v_m) {
  u_value s143 = v_m;
  u_value t144 = NULL;
  if ((!t144) && u_is_con(s143, &n_map_46_MNode)) {
    u_value v_r = u_field(s143, 3);
    t144 = v_r;
  }
  if (!t144) {
    t144 = v_m;
  }
  if (!t144) {
    u_no_case(s143);
  }
  return t144;
}

u_value f_map_46_balance_4(u_value v_l, u_value v_k, u_value v_v, u_value v_r) {
  u_value t148;
  if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1))))) {
    u_value t145;
    if (u_truth(u_lt(f_map_46_height_1(f_map_46_left_of_1(v_l)), f_map_46_height_1(f_map_46_right_of_1(v_l))))) {
      t145 = f_map_46_rotate_left_1(v_l);
    } else {
      t145 = v_l;
    }
    t148 = f_map_46_rotate_right_1(f_map_46_mnode_4(t145, v_k, v_v, v_r));
  } else {
    u_value t147;
    if (u_truth(f_prelude_46__62__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1))))) {
      u_value t146;
      if (u_truth(u_lt(f_map_46_height_1(f_map_46_right_of_1(v_r)), f_map_46_height_1(f_map_46_left_of_1(v_r))))) {
        t146 = f_map_46_rotate_right_1(v_r);
      } else {
        t146 = v_r;
      }
      t147 = f_map_46_rotate_left_1(f_map_46_mnode_4(v_l, v_k, v_v, t146));
    } else {
      t147 = f_map_46_mnode_4(v_l, v_k, v_v, v_r);
    }
    t148 = t147;
  }
  return t148;
}

u_value f_map_46_balanced_1(u_value v_m) {
  u_value s149 = v_m;
  u_value t150 = NULL;
  if ((!t150) && u_is_con(s149, &n_map_46_MLeaf)) {
    t150 = u_mk_bool(1);
  }
  if ((!t150) && u_is_con(s149, &n_map_46_MNode)) {
    u_value v_l = u_field(s149, 0);
    u_value v_r = u_field(s149, 3);
    u_value v_h = u_field(s149, 4);
    u_value t151 = f_map_46_balanced_1(v_l);
    if (u_truth(t151)) {
      t151 = f_map_46_balanced_1(v_r);
    }
    u_value t152 = t151;
    if (u_truth(t152)) {
      t152 = u_eq(v_h, f_map_46_mnode_height_2(v_l, v_r));
    }
    u_value t153 = t152;
    if (u_truth(t153)) {
      t153 = f_prelude_46__60__61__2(f_map_46_height_1(v_l), u_add(f_map_46_height_1(v_r), u_mk_int(1)));
    }
    u_value t154 = t153;
    if (u_truth(t154)) {
      t154 = f_prelude_46__60__61__2(f_map_46_height_1(v_r), u_add(f_map_46_height_1(v_l), u_mk_int(1)));
    }
    t150 = t154;
  }
  if (!t150) {
    u_no_case(s149);
  }
  return t150;
}

u_value f_map_46_mnode_height_2(u_value v_l, u_value v_r) {
  u_value t155;
  if (u_truth(u_lt(f_map_46_height_1(v_l), f_map_46_height_1(v_r)))) {
    t155 = f_map_46_height_1(v_r);
  } else {
    t155 = f_map_46_height_1(v_l);
  }
  return u_add(u_mk_int(1), t155);
}

u_value f_map_46_put_all_2(u_value v_ps, u_value v_m) {
  u_value s156 = v_ps;
  u_value t157 = NULL;
  if ((!t157) && u_is_con(s156, &n_list_46_Nil)) {
    t157 = v_m;
  }
  if ((!t157) && u_is_con(s156, &n_list_46_Cons)) {
    u_value v_k = u_field(u_field(s156, 0), 0);
    u_value v_v = u_field(u_field(s156, 0), 1);
    u_value v_rest = u_field(s156, 1);
    t157 = f_map_46_put_all_2(v_rest, f_map_46_put_3(v_k, v_v, v_m));
  }
  if (!t157) {
    u_no_case(s156);
  }
  return t157;
}

u_value f_map_46_numbers_1(u_value v_n) {
  u_value t158;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t158 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t158 = f_list_46__43__43__2(f_map_46_numbers_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t158;
}

u_value f_ctx_46_Ctx_46_aliases_1(u_value v_x) {
  u_value s159 = v_x;
  u_value t160 = NULL;
  if ((!t160) && u_is_con(s159, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s159, 0);
    t160 = v_y;
  }
  if (!t160) {
    u_no_case(s159);
  }
  return t160;
}

u_value f_ctx_46_Ctx_46_funs_1(u_value v_x) {
  u_value s161 = v_x;
  u_value t162 = NULL;
  if ((!t162) && u_is_con(s161, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s161, 1);
    t162 = v_y;
  }
  if (!t162) {
    u_no_case(s161);
  }
  return t162;
}

u_value f_ctx_46_Ctx_46_procs_1(u_value v_x) {
  u_value s163 = v_x;
  u_value t164 = NULL;
  if ((!t164) && u_is_con(s163, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s163, 2);
    t164 = v_y;
  }
  if (!t164) {
    u_no_case(s163);
  }
  return t164;
}

u_value f_ctx_46_Ctx_46_datas_1(u_value v_x) {
  u_value s165 = v_x;
  u_value t166 = NULL;
  if ((!t166) && u_is_con(s165, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s165, 3);
    t166 = v_y;
  }
  if (!t166) {
    u_no_case(s165);
  }
  return t166;
}

u_value f_ctx_46_Ctx_46_members_1(u_value v_x) {
  u_value s167 = v_x;
  u_value t168 = NULL;
  if ((!t168) && u_is_con(s167, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s167, 4);
    t168 = v_y;
  }
  if (!t168) {
    u_no_case(s167);
  }
  return t168;
}

u_value f_ctx_46_Ctx_46_constrs_1(u_value v_x) {
  u_value s169 = v_x;
  u_value t170 = NULL;
  if ((!t170) && u_is_con(s169, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s169, 5);
    t170 = v_y;
  }
  if (!t170) {
    u_no_case(s169);
  }
  return t170;
}

u_value f_ctx_46_Ctx_46_named_1(u_value v_x) {
  u_value s171 = v_x;
  u_value t172 = NULL;
  if ((!t172) && u_is_con(s171, &n_ctx_46_Ctx)) {
    u_value v_y = u_field(s171, 6);
    t172 = v_y;
  }
  if (!t172) {
    u_no_case(s171);
  }
  return t172;
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
  u_value s173 = f_map_46_get_2(v_n, f_ctx_46_Ctx_46_funs_1(v_c));
  u_value t174 = NULL;
  if ((!t174) && u_is_con(s173, &n_option_46_Some)) {
    u_value v_ss = u_field(s173, 0);
    t174 = v_ss;
  }
  if ((!t174) && u_is_con(s173, &n_option_46_None)) {
    t174 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t174) {
    u_no_case(s173);
  }
  return t174;
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
  u_value s175 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t176 = NULL;
  if ((!t176) && u_is_con(s175, &n_option_46_Some)) {
    u_value v_ns = u_field(s175, 0);
    t176 = v_ns;
  }
  if ((!t176) && u_is_con(s175, &n_option_46_None)) {
    t176 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t176) {
    u_no_case(s175);
  }
  return t176;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s177 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t178 = NULL;
  if ((!t178) && u_is_con(s177, &n_option_46_Some)) {
    u_value v_ns = u_field(s177, 0);
    u_value t179;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t179 = v_bs;
    } else {
      t179 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, v_ns), v_bs);
    }
    t178 = t179;
  }
  if ((!t178) && u_is_con(s177, &n_option_46_None)) {
    t178 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t178) {
    u_no_case(s177);
  }
  return t178;
}

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t) {
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
    t181 = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), v_a), v_f, v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t181) {
    u_no_case(s180);
  }
  return t181;
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
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
    t183 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, u_mk_con(&n_list_46_Nil, 0))), v_f), v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t183) {
    u_no_case(s182);
  }
  return t183;
}

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g) {
  u_value s184 = v_c;
  u_value t185 = NULL;
  if ((!t185) && u_is_con(s184, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s184, 0);
    u_value v_f = u_field(s184, 1);
    u_value v_p = u_field(s184, 2);
    u_value v_d = u_field(s184, 3);
    u_value v_m = u_field(s184, 4);
    u_value v_k = u_field(s184, 5);
    u_value v_bs = u_field(s184, 6);
    t185 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, f_map_46_put_3(v_n, v_g, v_p), v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t185) {
    u_no_case(s184);
  }
  return t185;
}

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity) {
  u_value s186 = v_c;
  u_value t187 = NULL;
  if ((!t187) && u_is_con(s186, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s186, 0);
    u_value v_f = u_field(s186, 1);
    u_value v_p = u_field(s186, 2);
    u_value v_d = u_field(s186, 3);
    u_value v_m = u_field(s186, 4);
    u_value v_k = u_field(s186, 5);
    u_value v_bs = u_field(s186, 6);
    t187 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, f_map_46_put_3(v_n, v_arity, v_d), v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t187) {
    u_no_case(s186);
  }
  return t187;
}

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs) {
  u_value s188 = v_c;
  u_value t189 = NULL;
  if ((!t189) && u_is_con(s188, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s188, 0);
    u_value v_f = u_field(s188, 1);
    u_value v_p = u_field(s188, 2);
    u_value v_d = u_field(s188, 3);
    u_value v_m = u_field(s188, 4);
    u_value v_k = u_field(s188, 5);
    u_value v_bs = u_field(s188, 6);
    t189 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, f_map_46_put_3(v_n, v_cs, v_m), v_k, v_bs);
  }
  if (!t189) {
    u_no_case(s188);
  }
  return t189;
}

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc) {
  u_value s190 = v_c;
  u_value t191 = NULL;
  if ((!t191) && u_is_con(s190, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s190, 0);
    u_value v_f = u_field(s190, 1);
    u_value v_p = u_field(s190, 2);
    u_value v_d = u_field(s190, 3);
    u_value v_m = u_field(s190, 4);
    u_value v_k = u_field(s190, 5);
    u_value v_bs = u_field(s190, 6);
    t191 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, v_m, f_map_46_put_3(v_n, v_kc, v_k), f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t191) {
    u_no_case(s190);
  }
  return t191;
}

u_value f_check_46_State_46_ctx_1(u_value v_x) {
  u_value s192 = v_x;
  u_value t193 = NULL;
  if ((!t193) && u_is_con(s192, &n_check_46_State)) {
    u_value v_y = u_field(s192, 0);
    t193 = v_y;
  }
  if (!t193) {
    u_no_case(s192);
  }
  return t193;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s194 = v_x;
  u_value t195 = NULL;
  if ((!t195) && u_is_con(s194, &n_check_46_State)) {
    u_value v_y = u_field(s194, 1);
    t195 = v_y;
  }
  if (!t195) {
    u_no_case(s194);
  }
  return t195;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s196 = v_x;
  u_value t197 = NULL;
  if ((!t197) && u_is_con(s196, &n_check_46_State)) {
    u_value v_y = u_field(s196, 2);
    t197 = v_y;
  }
  if (!t197) {
    u_no_case(s196);
  }
  return t197;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s198 = v_x;
  u_value t199 = NULL;
  if ((!t199) && u_is_con(s198, &n_check_46_State)) {
    u_value v_y = u_field(s198, 3);
    t199 = v_y;
  }
  if (!t199) {
    u_no_case(s198);
  }
  return t199;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s200 = v_x;
  u_value t201 = NULL;
  if ((!t201) && u_is_con(s200, &n_check_46_State)) {
    u_value v_y = u_field(s200, 4);
    t201 = v_y;
  }
  if (!t201) {
    u_no_case(s200);
  }
  return t201;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s202 = v_x;
  u_value t203 = NULL;
  if ((!t203) && u_is_con(s202, &n_check_46_State)) {
    u_value v_y = u_field(s202, 5);
    t203 = v_y;
  }
  if (!t203) {
    u_no_case(s202);
  }
  return t203;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  u_value s204 = v_s;
  u_value t205 = NULL;
  if ((!t205) && u_is_con(s204, &n_check_46_State)) {
    u_value v_su = u_field(s204, 1);
    u_value v_n = u_field(s204, 2);
    u_value v_m = u_field(s204, 3);
    u_value v_ms = u_field(s204, 4);
    u_value v_vs = u_field(s204, 5);
    t205 = u_mk_con(&n_check_46_State, 6, v_c, v_su, v_n, v_m, v_ms, v_vs);
  }
  if (!t205) {
    u_no_case(s204);
  }
  return t205;
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  u_value s206 = v_s;
  u_value t207 = NULL;
  if ((!t207) && u_is_con(s206, &n_check_46_State)) {
    u_value v_c = u_field(s206, 0);
    u_value v_vs = u_field(s206, 5);
    t207 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_empty_0(), u_mk_int(0), v_m, f_check_46_sees_2(v_m, v_vs), v_vs);
  }
  if (!t207) {
    u_no_case(s206);
  }
  return t207;
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s208 = f_list_46_lookup_2(v_m, v_vs);
  u_value t209 = NULL;
  if ((!t209) && u_is_con(s208, &n_option_46_Some)) {
    u_value v_ms = u_field(s208, 0);
    t209 = v_ms;
  }
  if ((!t209) && u_is_con(s208, &n_option_46_None)) {
    t209 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t209) {
    u_no_case(s208);
  }
  return t209;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s210 = v_cs;
  u_value t211 = NULL;
  if ((!t211) && u_is_con(s210, &n_list_46_Nil)) {
    t211 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t211) && (u_is_con(s210, &n_list_46_Cons) && u_is_con(u_field(s210, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s210, 0), 0);
    u_value v_ds = u_field(u_field(s210, 0), 1);
    u_value v_rest = u_field(s210, 1);
    t211 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t211) && u_is_con(s210, &n_list_46_Cons)) {
    u_value v_c = u_field(s210, 0);
    u_value v_rest = u_field(s210, 1);
    t211 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t211) {
    u_no_case(s210);
  }
  return t211;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s212 = v_c;
  u_value t213 = NULL;
  if ((!t213) && u_is_con(s212, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s212, 0);
    u_value v_ps = u_field(s212, 1);
    u_value v_t = u_field(s212, 2);
    t213 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t213) && u_is_con(s212, &n_syntax_46_CData)) {
    u_value v_n = u_field(s212, 0);
    u_value v_ps = u_field(s212, 1);
    u_value v_ks = u_field(s212, 2);
    u_value t214 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e215 = &t214;
    u_value l216 = v_ks;
    while (u_is_con(l216, &n_list_46_Cons)) {
      u_value v_k = u_field(l216, 0);
      if (u_truth(u_mk_bool(1))) {
        e215 = u_append(e215, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l216 = u_field(l216, 1);
    }
    t213 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t214);
  }
  if ((!t213) && u_is_con(s212, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s212, 0);
    u_value v_ps = u_field(s212, 1);
    u_value v_xss = u_field(s212, 2);
    u_value v_r = u_field(s212, 3);
    u_value v_b = u_field(s212, 4);
    t213 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t213) && u_is_con(s212, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s212, 0);
    u_value v_ps = u_field(s212, 1);
    u_value v_ins = u_field(s212, 2);
    u_value v_outs = u_field(s212, 3);
    u_value v_refs = u_field(s212, 4);
    u_value v_b = u_field(s212, 5);
    t213 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t213) {
    t213 = v_c;
  }
  if (!t213) {
    u_no_case(s212);
  }
  return t213;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  u_value s217 = v_k;
  u_value t218 = NULL;
  if ((!t218) && u_is_con(s217, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s217, 0);
    u_value v_ts = u_field(s217, 1);
    t218 = u_mk_con(&n_syntax_46_Constr, 2, f_check_46_in_module_2(v_m, v_n), v_ts);
  }
  if (!t218) {
    u_no_case(s217);
  }
  return t218;
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s219 = v_ds;
  u_value t220 = NULL;
  if ((!t220) && u_is_con(s219, &n_list_46_Nil)) {
    t220 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t220) && (u_is_con(s219, &n_list_46_Cons) && u_is_con(u_field(u_field(s219, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s219, 0), 0);
    u_value v_n = u_field(u_field(u_field(s219, 0), 1), 0);
    u_value v_rest = u_field(s219, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t220 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t220) && u_is_con(s219, &n_list_46_Cons)) {
    u_value v_rest = u_field(s219, 1);
    t220 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t220) {
    u_no_case(s219);
  }
  return t220;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t221 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e222 = &t221;
  u_value l223 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l223, &n_list_46_Cons)) {
    u_value v_m = u_field(l223, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_224 = U_LIT("prelude");
      static struct u_object l_prelude_225 = U_LIT("prelude");
      e222 = u_append(e222, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_224, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_225)))))));
    }
    l223 = u_field(l223, 1);
  }
  return t221;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value s226 = v_n;
  u_value t227 = NULL;
  if ((!t227) && (u_is_con(s226, &n_syntax_46_Name) && u_is_con(u_field(s226, 0), &n_option_46_Some))) {
    u_value t228 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e229 = &t228;
    u_value l230 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    while (u_is_con(l230, &n_list_46_Cons)) {
      u_value v_d = u_field(l230, 0);
      if (u_truth(f_list_46_contains_2(v_d, v_declared))) {
        e229 = u_append(e229, v_d);
      }
      l230 = u_field(l230, 1);
    }
    t227 = t228;
  }
  if ((!t227) && (u_is_con(s226, &n_syntax_46_Name) && u_is_con(u_field(s226, 0), &n_option_46_None))) {
    u_value v_b = u_field(s226, 1);
    u_value t237;
    if (u_truth(f_list_46_contains_2(u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), v_declared))) {
      t237 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
    } else {
      u_value t231 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e232 = &t231;
      u_value t233 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e234 = &t233;
      u_value l235 = f_list_46_drop_2(u_mk_int(1), f_check_46_State_46_visible_1(v_s));
      while (u_is_con(l235, &n_list_46_Cons)) {
        u_value v_m = u_field(l235, 0);
        if (u_truth(u_mk_bool(1))) {
          e234 = u_append(e234, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_b));
        }
        l235 = u_field(l235, 1);
      }
      u_value l236 = t233;
      while (u_is_con(l236, &n_list_46_Cons)) {
        u_value v_d = u_field(l236, 0);
        if (u_truth(f_list_46_contains_2(v_d, v_declared))) {
          e232 = u_append(e232, v_d);
        }
        l236 = u_field(l236, 1);
      }
      t237 = t231;
    }
    t227 = t237;
  }
  if (!t227) {
    u_no_case(s226);
  }
  return t227;
}

u_value f_check_46_funs_named_2(u_value v_c, u_value v_n) {
  u_value t238 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e239 = &t238;
  u_value l240 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l240, &n_list_46_Cons)) {
    u_value v_d = u_field(l240, 0);
    if (u_truth(f_ctx_46_is_fun_2(v_c, v_d))) {
      e239 = u_append(e239, v_d);
    }
    l240 = u_field(l240, 1);
  }
  return t238;
}

u_value f_check_46_constrs_named_2(u_value v_c, u_value v_n) {
  u_value t241 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e242 = &t241;
  u_value l243 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l243, &n_list_46_Cons)) {
    u_value v_d = u_field(l243, 0);
    if (u_truth(f_ctx_46_is_constr_2(v_c, v_d))) {
      e242 = u_append(e242, v_d);
    }
    l243 = u_field(l243, 1);
  }
  return t241;
}

u_value f_check_46_procs_named_2(u_value v_c, u_value v_n) {
  u_value t244 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e245 = &t244;
  u_value l246 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l246, &n_list_46_Cons)) {
    u_value v_d = u_field(l246, 0);
    if (u_truth(f_ctx_46_is_proc_2(v_c, v_d))) {
      e245 = u_append(e245, v_d);
    }
    l246 = u_field(l246, 1);
  }
  return t244;
}

u_value f_check_46_sorts_named_2(u_value v_c, u_value v_n) {
  u_value t247 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e248 = &t247;
  u_value l249 = f_ctx_46_names_of_2(v_c, f_syntax_46_Name_46_base_1(v_n));
  while (u_is_con(l249, &n_list_46_Cons)) {
    u_value v_d = u_field(l249, 0);
    u_value t250 = f_ctx_46_is_data_2(v_c, v_d);
    if (!u_truth(t250)) {
      t250 = f_ctx_46_is_alias_2(v_c, v_d);
    }
    if (u_truth(t250)) {
      e248 = u_append(e248, v_d);
    }
    l249 = u_field(l249, 1);
  }
  return t247;
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
  static struct u_object l_Bool_251 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_251), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_252 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_252), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_253 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_253), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_254 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_254), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t255 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e256 = &t255;
  u_value l257 = v_ps;
  while (u_is_con(l257, &n_list_46_Cons)) {
    u_value v_p = u_field(l257, 0);
    if (u_truth(u_mk_bool(1))) {
      e256 = u_append(e256, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l257 = u_field(l257, 1);
  }
  return t255;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s258 = v_n;
  u_value t259 = NULL;
  if ((!t259) && (u_is_con(s258, &n_syntax_46_Name) && u_is_con(u_field(s258, 0), &n_option_46_None))) {
    u_value v_b = u_field(s258, 1);
    static struct u_object l_Bool_260 = U_LIT("Bool");
    static struct u_object l_Int_261 = U_LIT("Int");
    static struct u_object l_Str_262 = U_LIT("Str");
    static struct u_object l_Char_263 = U_LIT("Char");
    t259 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_260, u_mk_con(&n_list_46_Cons, 2, &l_Int_261, u_mk_con(&n_list_46_Cons, 2, &l_Str_262, u_mk_con(&n_list_46_Cons, 2, &l_Char_263, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t259) {
    t259 = u_mk_bool(0);
  }
  if (!t259) {
    u_no_case(s258);
  }
  return t259;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s264 = v_v;
  u_value t265 = NULL;
  if ((!t265) && u_is_con(s264, &n_syntax_46_VUnit)) {
    t265 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t265) && u_is_con(s264, &n_syntax_46_VBool)) {
    t265 = f_check_46_tbool_0();
  }
  if ((!t265) && u_is_con(s264, &n_syntax_46_VInt)) {
    t265 = f_check_46_tint_0();
  }
  if ((!t265) && u_is_con(s264, &n_syntax_46_VStr)) {
    t265 = f_check_46_tstr_0();
  }
  if ((!t265) && u_is_con(s264, &n_syntax_46_VChar)) {
    t265 = f_check_46_tchar_0();
  }
  if (!t265) {
    u_no_case(s264);
  }
  return t265;
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
  u_value s266 = v_n;
  u_value t267 = NULL;
  if ((!t267) && (u_is_con(s266, &n_syntax_46_Name) && u_is_con(u_field(s266, 0), &n_option_46_None))) {
    u_value v_b = u_field(s266, 1);
    t267 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t267) {
    t267 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t267) {
    u_no_case(s266);
  }
  return t267;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_268 = U_LIT("A");
  u_value t272 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_268);
  u_value t271;
  {
    u_value v_a = t272;
    u_value s269 = v_b;
    u_value t270 = NULL;
    if ((!t270) && u_equals(s269, u_mk_lit("+"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("-"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("*"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("/"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("%"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("<"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("=="))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("compare"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("and"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("or"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("chars"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("string"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("ord"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("chr"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("^"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_size"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_join"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_slice"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_split"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_get"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_contains"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_compare"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_show"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_quote"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("str_number"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("is_digit"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("is_letter"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t270) && u_equals(s269, u_mk_lit("is_space"))) {
      t270 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t270) {
      t270 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t270) {
      u_no_case(s269);
    }
    t271 = t270;
  }
  return t271;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s273 = v_t;
  u_value t274 = NULL;
  if ((!t274) && u_is_con(s273, &n_syntax_46_TMeta)) {
    t274 = u_mk_bool(1);
  }
  if (!t274) {
    t274 = u_mk_bool(0);
  }
  if (!t274) {
    u_no_case(s273);
  }
  return t274;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s275 = v_t;
  u_value t276 = NULL;
  if ((!t276) && u_is_con(s275, &n_syntax_46_TParam)) {
    t276 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t276) && u_is_con(s275, &n_syntax_46_TMeta)) {
    t276 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t276) && u_is_con(s275, &n_syntax_46_THole)) {
    t276 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t276) && u_is_con(s275, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s275, 1);
    u_value t277 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e278 = &t277;
    u_value l279 = v_args;
    while (u_is_con(l279, &n_list_46_Cons)) {
      u_value v_a = u_field(l279, 0);
      if (u_truth(u_mk_bool(1))) {
        e278 = u_append(e278, f_check_46_free_1(v_a));
      }
      l279 = u_field(l279, 1);
    }
    t276 = f_list_46_distinct_1(f_list_46_concat_1(t277));
  }
  if ((!t276) && u_is_con(s275, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s275, 0);
    u_value v_r = u_field(s275, 1);
    u_value t280 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e281 = &t280;
    u_value l282 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l282, &n_list_46_Cons)) {
      u_value v_a = u_field(l282, 0);
      if (u_truth(u_mk_bool(1))) {
        e281 = u_append(e281, f_check_46_free_1(v_a));
      }
      l282 = u_field(l282, 1);
    }
    t276 = f_list_46_distinct_1(f_list_46_concat_1(t280));
  }
  if ((!t276) && u_is_con(s275, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s275, 0);
    u_value t283 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e284 = &t283;
    u_value l285 = v_ts;
    while (u_is_con(l285, &n_list_46_Cons)) {
      u_value v_a = u_field(l285, 0);
      if (u_truth(u_mk_bool(1))) {
        e284 = u_append(e284, f_check_46_free_1(v_a));
      }
      l285 = u_field(l285, 1);
    }
    t276 = f_list_46_distinct_1(f_list_46_concat_1(t283));
  }
  if (!t276) {
    u_no_case(s275);
  }
  return t276;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s286 = v_t;
  u_value t287 = NULL;
  if ((!t287) && u_is_con(s286, &n_syntax_46_THole)) {
    t287 = u_mk_bool(1);
  }
  if ((!t287) && u_is_con(s286, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s286, 1);
    u_value t288 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e289 = &t288;
    u_value l290 = v_args;
    while (u_is_con(l290, &n_list_46_Cons)) {
      u_value v_a = u_field(l290, 0);
      if (u_truth(u_mk_bool(1))) {
        e289 = u_append(e289, f_check_46_holes_1(v_a));
      }
      l290 = u_field(l290, 1);
    }
    t287 = f_list_46_contains_2(u_mk_bool(1), t288);
  }
  if ((!t287) && u_is_con(s286, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s286, 0);
    u_value v_r = u_field(s286, 1);
    u_value t291 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e292 = &t291;
    u_value l293 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l293, &n_list_46_Cons)) {
      u_value v_a = u_field(l293, 0);
      if (u_truth(u_mk_bool(1))) {
        e292 = u_append(e292, f_check_46_holes_1(v_a));
      }
      l293 = u_field(l293, 1);
    }
    t287 = f_list_46_contains_2(u_mk_bool(1), t291);
  }
  if ((!t287) && u_is_con(s286, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s286, 0);
    u_value t294 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e295 = &t294;
    u_value l296 = v_ts;
    while (u_is_con(l296, &n_list_46_Cons)) {
      u_value v_a = u_field(l296, 0);
      if (u_truth(u_mk_bool(1))) {
        e295 = u_append(e295, f_check_46_holes_1(v_a));
      }
      l296 = u_field(l296, 1);
    }
    t287 = f_list_46_contains_2(u_mk_bool(1), t294);
  }
  if (!t287) {
    t287 = u_mk_bool(0);
  }
  if (!t287) {
    u_no_case(s286);
  }
  return t287;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s297 = v_t;
  u_value t298 = NULL;
  if ((!t298) && u_is_con(s297, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s297, 0);
    u_value v_args = u_field(s297, 1);
    u_value t299 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e300 = &t299;
    u_value l301 = v_args;
    while (u_is_con(l301, &n_list_46_Cons)) {
      u_value v_a = u_field(l301, 0);
      if (u_truth(u_mk_bool(1))) {
        e300 = u_append(e300, f_check_46_subst_2(v_a, v_m));
      }
      l301 = u_field(l301, 1);
    }
    t298 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t299);
  }
  if ((!t298) && u_is_con(s297, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s297, 0);
    u_value v_r = u_field(s297, 1);
    u_value t302 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e303 = &t302;
    u_value l304 = v_args;
    while (u_is_con(l304, &n_list_46_Cons)) {
      u_value v_a = u_field(l304, 0);
      if (u_truth(u_mk_bool(1))) {
        e303 = u_append(e303, f_check_46_subst_2(v_a, v_m));
      }
      l304 = u_field(l304, 1);
    }
    t298 = u_mk_con(&n_syntax_46_TFun, 2, t302, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t298) && u_is_con(s297, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s297, 0);
    u_value t305 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e306 = &t305;
    u_value l307 = v_ts;
    while (u_is_con(l307, &n_list_46_Cons)) {
      u_value v_a = u_field(l307, 0);
      if (u_truth(u_mk_bool(1))) {
        e306 = u_append(e306, f_check_46_subst_2(v_a, v_m));
      }
      l307 = u_field(l307, 1);
    }
    t298 = u_mk_con(&n_syntax_46_TTuple, 1, t305);
  }
  if (!t298) {
    u_value s308 = f_list_46_lookup_2(v_t, v_m);
    u_value t309 = NULL;
    if ((!t309) && u_is_con(s308, &n_option_46_Some)) {
      u_value v_u = u_field(s308, 0);
      t309 = v_u;
    }
    if ((!t309) && u_is_con(s308, &n_option_46_None)) {
      t309 = v_t;
    }
    if (!t309) {
      u_no_case(s308);
    }
    t298 = t309;
  }
  if (!t298) {
    u_no_case(s297);
  }
  return t298;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s310 = v_t;
  u_value t311 = NULL;
  if ((!t311) && u_is_con(s310, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s310, 0);
    u_value s312 = f_map_46_get_2(v_i, v_su);
    u_value t313 = NULL;
    if ((!t313) && u_is_con(s312, &n_option_46_Some)) {
      u_value v_u = u_field(s312, 0);
      t313 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t313) && u_is_con(s312, &n_option_46_None)) {
      t313 = v_t;
    }
    if (!t313) {
      u_no_case(s312);
    }
    t311 = t313;
  }
  if (!t311) {
    t311 = v_t;
  }
  if (!t311) {
    u_no_case(s310);
  }
  return t311;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s314 = f_check_46_resolve_2(v_su, v_t);
  u_value t315 = NULL;
  if ((!t315) && u_is_con(s314, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s314, 0);
    t315 = u_eq(v_i, v_j);
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s314, 1);
    u_value t316 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e317 = &t316;
    u_value l318 = v_args;
    while (u_is_con(l318, &n_list_46_Cons)) {
      u_value v_a = u_field(l318, 0);
      if (u_truth(u_mk_bool(1))) {
        e317 = u_append(e317, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l318 = u_field(l318, 1);
    }
    t315 = f_list_46_contains_2(u_mk_bool(1), t316);
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s314, 0);
    u_value v_r = u_field(s314, 1);
    u_value t319 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e320 = &t319;
    u_value l321 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l321, &n_list_46_Cons)) {
      u_value v_a = u_field(l321, 0);
      if (u_truth(u_mk_bool(1))) {
        e320 = u_append(e320, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l321 = u_field(l321, 1);
    }
    t315 = f_list_46_contains_2(u_mk_bool(1), t319);
  }
  if ((!t315) && u_is_con(s314, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s314, 0);
    u_value t322 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e323 = &t322;
    u_value l324 = v_ts;
    while (u_is_con(l324, &n_list_46_Cons)) {
      u_value v_a = u_field(l324, 0);
      if (u_truth(u_mk_bool(1))) {
        e323 = u_append(e323, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l324 = u_field(l324, 1);
    }
    t315 = f_list_46_contains_2(u_mk_bool(1), t322);
  }
  if (!t315) {
    t315 = u_mk_bool(0);
  }
  if (!t315) {
    u_no_case(s314);
  }
  return t315;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s325 = v_k;
  u_value t326 = NULL;
  if (!t326) {
    u_value v_ps = u_field(s325, 0);
    u_value v_ks = u_field(s325, 1);
    u_value v_r = u_field(s325, 2);
    u_value t327;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t327 = v_r;
    } else {
      t327 = f_check_46_fun_2(v_ks, v_r);
    }
    t326 = u_mk_tuple(2, v_ps, t327);
  }
  if (!t326) {
    u_no_case(s325);
  }
  return t326;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s328 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t329 = NULL;
  if ((!t329) && u_is_con(s328, &n_option_46_Some)) {
    u_value v_kc = u_field(s328, 0);
    t329 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t329) && u_is_con(s328, &n_option_46_None)) {
    t329 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t329) {
    u_no_case(s328);
  }
  return t329;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s330 = f_list_46_lookup_2(v_n, v_env);
  u_value t331 = NULL;
  if ((!t331) && u_is_con(s330, &n_option_46_Some)) {
    u_value v_sc = u_field(s330, 0);
    t331 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t331) && u_is_con(s330, &n_option_46_None)) {
    u_value t332 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e333 = &t332;
    u_value l334 = f_check_46_visible_names_3(v_s, v_n, f_check_46_funs_named_2(f_check_46_State_46_ctx_1(v_s), v_n));
    while (u_is_con(l334, &n_list_46_Cons)) {
      u_value v_f = u_field(l334, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t335 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e336 = &t335;
        u_value l337 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l337, &n_list_46_Cons)) {
          u_value v_sc = u_field(l337, 0);
          if (u_truth(u_mk_bool(1))) {
            e336 = u_append(e336, u_mk_tuple(2, v_f, v_sc));
          }
          l337 = u_field(l337, 1);
        }
        e333 = u_append(e333, t335);
      }
      l334 = u_field(l334, 1);
    }
    u_value t338 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e339 = &t338;
    u_value l340 = f_check_46_visible_names_3(v_s, v_n, f_check_46_constrs_named_2(f_check_46_State_46_ctx_1(v_s), v_n));
    while (u_is_con(l340, &n_list_46_Cons)) {
      u_value v_k = u_field(l340, 0);
      if (u_truth(u_mk_bool(1))) {
        e339 = u_append(e339, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l340 = u_field(l340, 1);
    }
    u_value t341 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e342 = &t341;
    u_value l343 = f_check_46_builtin_1(v_n);
    while (u_is_con(l343, &n_list_46_Cons)) {
      u_value v_sc = u_field(l343, 0);
      if (u_truth(u_mk_bool(1))) {
        e342 = u_append(e342, u_mk_tuple(2, v_n, v_sc));
      }
      l343 = u_field(l343, 1);
    }
    t331 = f_list_46__43__43__2(f_list_46_concat_1(t332), f_list_46__43__43__2(t338, t341));
  }
  if (!t331) {
    u_no_case(s330);
  }
  return t331;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s344 = v_cs;
  u_value t345 = NULL;
  if ((!t345) && u_is_con(s344, &n_list_46_Nil)) {
    t345 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t345) && u_is_con(s344, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s344, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s344, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s344, 0), 1), 1);
    u_value v_rest = u_field(s344, 1);
    u_value t346;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t346 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t346 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t345 = t346;
  }
  if (!t345) {
    u_no_case(s344);
  }
  return t345;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s347 = v_scs;
  u_value t348 = NULL;
  if ((!t348) && u_is_con(s347, &n_list_46_Nil)) {
    t348 = u_mk_bool(0);
  }
  if ((!t348) && u_is_con(s347, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s347, 0), 1);
    u_value v_rest = u_field(s347, 1);
    u_value t349 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t349)) {
      t349 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t348 = t349;
  }
  if (!t348) {
    u_no_case(s347);
  }
  return t348;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t350 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e351 = &t350;
  u_value l352 = v_xs;
  while (u_is_con(l352, &n_list_46_Cons)) {
    u_value v_x = u_field(l352, 0);
    if (u_truth(u_mk_bool(1))) {
      e351 = u_append(e351, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l352 = u_field(l352, 1);
  }
  return t350;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s353 = v_env;
  u_value t354 = NULL;
  if ((!t354) && u_is_con(s353, &n_list_46_Nil)) {
    t354 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t354) && u_is_con(s353, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s353, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s353, 0), 1), 1);
    u_value v_rest = u_field(s353, 1);
    t354 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t354) {
    u_no_case(s353);
  }
  return t354;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t360 = f_check_46_fixed_2(v_su, v_env);
  u_value t361 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t359;
  {
    u_value v_fx = t360;
    u_value v_u = t361;
    u_value t355 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e356 = &t355;
    u_value l357 = f_check_46_free_1(v_u);
    while (u_is_con(l357, &n_list_46_Cons)) {
      u_value v_m = u_field(l357, 0);
      u_value t358 = f_check_46_is_meta_1(v_m);
      if (u_truth(t358)) {
        t358 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t358)) {
        e356 = u_append(e356, v_m);
      }
      l357 = u_field(l357, 1);
    }
    t359 = u_mk_tuple(2, t355, v_u);
  }
  return t359;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t362 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e363 = &t362;
  u_value l364 = v_xs;
  while (u_is_con(l364, &n_list_46_Cons)) {
    u_value v_x = u_field(l364, 0);
    if (u_truth(u_mk_bool(1))) {
      e363 = u_append(e363, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l364 = u_field(l364, 1);
  }
  return t362;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s365 = v_b;
  u_value t366 = NULL;
  if ((!t366) && u_is_con(s365, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s365, 0);
    u_value v_g = u_field(s365, 1);
    t366 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t366) {
    u_no_case(s365);
  }
  return t366;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s367 = v_g;
  u_value t368 = NULL;
  if ((!t368) && u_is_con(s367, &n_option_46_Some)) {
    u_value v_e = u_field(s367, 0);
    t368 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t368) && u_is_con(s367, &n_option_46_None)) {
    t368 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t368) {
    u_no_case(s367);
  }
  return t368;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s369 = f_check_46_resolve_2(v_su, v_t);
  u_value t370 = NULL;
  if ((!t370) && u_is_con(s369, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s369, 0);
    u_value v_args = u_field(s369, 1);
    u_value t371 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e372 = &t371;
    u_value l373 = v_args;
    while (u_is_con(l373, &n_list_46_Cons)) {
      u_value v_a = u_field(l373, 0);
      if (u_truth(u_mk_bool(1))) {
        e372 = u_append(e372, f_check_46_zonk_type_2(v_su, v_a));
      }
      l373 = u_field(l373, 1);
    }
    t370 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t371);
  }
  if ((!t370) && u_is_con(s369, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s369, 0);
    u_value v_r = u_field(s369, 1);
    u_value t374 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e375 = &t374;
    u_value l376 = v_args;
    while (u_is_con(l376, &n_list_46_Cons)) {
      u_value v_a = u_field(l376, 0);
      if (u_truth(u_mk_bool(1))) {
        e375 = u_append(e375, f_check_46_zonk_type_2(v_su, v_a));
      }
      l376 = u_field(l376, 1);
    }
    t370 = u_mk_con(&n_syntax_46_TFun, 2, t374, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t370) && u_is_con(s369, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s369, 0);
    u_value t377 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e378 = &t377;
    u_value l379 = v_ts;
    while (u_is_con(l379, &n_list_46_Cons)) {
      u_value v_a = u_field(l379, 0);
      if (u_truth(u_mk_bool(1))) {
        e378 = u_append(e378, f_check_46_zonk_type_2(v_su, v_a));
      }
      l379 = u_field(l379, 1);
    }
    t370 = u_mk_con(&n_syntax_46_TTuple, 1, t377);
  }
  if (!t370) {
    u_value v_u = s369;
    t370 = v_u;
  }
  if (!t370) {
    u_no_case(s369);
  }
  return t370;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  u_value s380 = v_x;
  u_value t381 = NULL;
  if ((!t381) && u_is_con(s380, &n_syntax_46_Var)) {
    u_value v_n = u_field(s380, 0);
    u_value v_t = u_field(s380, 1);
    t381 = u_mk_con(&n_syntax_46_Var, 2, v_n, f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t381) {
    u_no_case(s380);
  }
  return t381;
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t382 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e383 = &t382;
  u_value l384 = v_xs;
  while (u_is_con(l384, &n_list_46_Cons)) {
    u_value v_x = u_field(l384, 0);
    if (u_truth(u_mk_bool(1))) {
      e383 = u_append(e383, f_check_46_zonk_var_2(v_su, v_x));
    }
    l384 = u_field(l384, 1);
  }
  return t382;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s385 = v_e;
  u_value t386 = NULL;
  if ((!t386) && u_is_con(s385, &n_syntax_46_ELit)) {
    t386 = v_e;
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s385, 0);
    t386 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s385, 0);
    u_value v_args = u_field(s385, 1);
    u_value t387 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e388 = &t387;
    u_value l389 = v_args;
    while (u_is_con(l389, &n_list_46_Cons)) {
      u_value v_a = u_field(l389, 0);
      if (u_truth(u_mk_bool(1))) {
        e388 = u_append(e388, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l389 = u_field(l389, 1);
    }
    t386 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t387);
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s385, 0);
    u_value v_b = u_field(s385, 1);
    t386 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s385, 0);
    u_value v_b = u_field(s385, 1);
    u_value t390 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e391 = &t390;
    u_value l392 = v_ps;
    while (u_is_con(l392, &n_list_46_Cons)) {
      u_value v_p = u_field(l392, 0);
      if (u_truth(u_mk_bool(1))) {
        e391 = u_append(e391, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l392 = u_field(l392, 1);
    }
    t386 = u_mk_con(&n_syntax_46_ELet, 2, t390, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s385, 0);
    u_value v_l = u_field(s385, 1);
    u_value v_r = u_field(s385, 2);
    t386 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s385, 0);
    u_value v_bs = u_field(s385, 1);
    u_value t393 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e394 = &t393;
    u_value l395 = v_bs;
    while (u_is_con(l395, &n_list_46_Cons)) {
      u_value v_b = u_field(l395, 0);
      if (u_truth(u_mk_bool(1))) {
        e394 = u_append(e394, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l395 = u_field(l395, 1);
    }
    t386 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t393);
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s385, 0);
    u_value v_xs = u_field(s385, 1);
    u_value v_ls = u_field(s385, 2);
    u_value v_c = u_field(s385, 3);
    u_value t396 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e397 = &t396;
    u_value l398 = v_ls;
    while (u_is_con(l398, &n_list_46_Cons)) {
      u_value v_l = u_field(l398, 0);
      if (u_truth(u_mk_bool(1))) {
        e397 = u_append(e397, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l398 = u_field(l398, 1);
    }
    t386 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t396, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s385, 0);
    u_value t399 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e400 = &t399;
    u_value l401 = v_es;
    while (u_is_con(l401, &n_list_46_Cons)) {
      u_value v_e = u_field(l401, 0);
      if (u_truth(u_mk_bool(1))) {
        e400 = u_append(e400, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l401 = u_field(l401, 1);
    }
    t386 = u_mk_con(&n_syntax_46_ETuple, 1, t399);
  }
  if ((!t386) && u_is_con(s385, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s385, 0);
    u_value v_t = u_field(s385, 1);
    t386 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t386) {
    u_no_case(s385);
  }
  return t386;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s402 = v_p;
  u_value t403 = NULL;
  if (!t403) {
    u_value v_x = u_field(s402, 0);
    u_value v_e = u_field(s402, 1);
    t403 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t403) {
    u_no_case(s402);
  }
  return t403;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s404 = v_b;
  u_value t405 = NULL;
  if ((!t405) && u_is_con(s404, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s404, 0);
    u_value v_g = u_field(s404, 1);
    u_value v_e = u_field(s404, 2);
    t405 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t405) {
    u_no_case(s404);
  }
  return t405;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s406 = v_p;
  u_value t407 = NULL;
  if ((!t407) && u_is_con(s406, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s406, 0);
    t407 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s406, 0);
    u_value v_args = u_field(s406, 1);
    u_value t408 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e409 = &t408;
    u_value l410 = v_args;
    while (u_is_con(l410, &n_list_46_Cons)) {
      u_value v_a = u_field(l410, 0);
      if (u_truth(u_mk_bool(1))) {
        e409 = u_append(e409, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l410 = u_field(l410, 1);
    }
    t407 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t408);
  }
  if ((!t407) && u_is_con(s406, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s406, 0);
    u_value t411 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e412 = &t411;
    u_value l413 = v_ps;
    while (u_is_con(l413, &n_list_46_Cons)) {
      u_value v_q = u_field(l413, 0);
      if (u_truth(u_mk_bool(1))) {
        e412 = u_append(e412, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l413 = u_field(l413, 1);
    }
    t407 = u_mk_con(&n_syntax_46_PTuple, 1, t411);
  }
  if (!t407) {
    t407 = v_p;
  }
  if (!t407) {
    u_no_case(s406);
  }
  return t407;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s414 = v_x;
  u_value t415 = NULL;
  if ((!t415) && u_is_con(s414, &n_syntax_46_SSkip)) {
    t415 = v_x;
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s414, 0);
    u_value t416 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e417 = &t416;
    u_value l418 = v_ps;
    while (u_is_con(l418, &n_list_46_Cons)) {
      u_value v_p = u_field(l418, 0);
      if (u_truth(u_mk_bool(1))) {
        e417 = u_append(e417, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l418 = u_field(l418, 1);
    }
    t415 = u_mk_con(&n_syntax_46_SAssign, 1, t416);
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s414, 0);
    u_value v_ys = u_field(s414, 1);
    u_value t419 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e420 = &t419;
    u_value l421 = v_ys;
    while (u_is_con(l421, &n_list_46_Cons)) {
      u_value v_y = u_field(l421, 0);
      if (u_truth(u_mk_bool(1))) {
        e420 = u_append(e420, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l421 = u_field(l421, 1);
    }
    t415 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t419);
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s414, 0);
    u_value v_l = u_field(s414, 1);
    u_value v_r = u_field(s414, 2);
    t415 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s414, 0);
    u_value v_b = u_field(s414, 1);
    t415 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s414, 0);
    u_value v_ins = u_field(s414, 1);
    u_value v_outs = u_field(s414, 2);
    u_value v_refs = u_field(s414, 3);
    u_value t422 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e423 = &t422;
    u_value l424 = v_ins;
    while (u_is_con(l424, &n_list_46_Cons)) {
      u_value v_e = u_field(l424, 0);
      if (u_truth(u_mk_bool(1))) {
        e423 = u_append(e423, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l424 = u_field(l424, 1);
    }
    t415 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t422, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s414, 0);
    u_value v_bs = u_field(s414, 1);
    u_value t425 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e426 = &t425;
    u_value l427 = v_bs;
    while (u_is_con(l427, &n_list_46_Cons)) {
      u_value v_b = u_field(l427, 0);
      if (u_truth(u_mk_bool(1))) {
        e426 = u_append(e426, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l427 = u_field(l427, 1);
    }
    t415 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t425);
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s414, 0);
    t415 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s414, 0);
    u_value v_t = u_field(s414, 1);
    t415 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t415) && u_is_con(s414, &n_syntax_46_STry)) {
    u_value v_b = u_field(s414, 0);
    u_value v_hs = u_field(s414, 1);
    u_value t428 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e429 = &t428;
    u_value l430 = v_hs;
    while (u_is_con(l430, &n_list_46_Cons)) {
      u_value v_h = u_field(l430, 0);
      if (u_truth(u_mk_bool(1))) {
        e429 = u_append(e429, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l430 = u_field(l430, 1);
    }
    t415 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t428);
  }
  if (!t415) {
    u_no_case(s414);
  }
  return t415;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s431 = v_b;
  u_value t432 = NULL;
  if ((!t432) && u_is_con(s431, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s431, 0);
    u_value v_g = u_field(s431, 1);
    u_value v_y = u_field(s431, 2);
    t432 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t432) {
    u_no_case(s431);
  }
  return t432;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s433 = v_h;
  u_value t434 = NULL;
  if ((!t434) && u_is_con(s433, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s433, 0);
    u_value v_p = u_field(s433, 1);
    u_value v_g = u_field(s433, 2);
    u_value v_b = u_field(s433, 3);
    t434 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t434) {
    u_no_case(s433);
  }
  return t434;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s435 = v_c;
  u_value t436 = NULL;
  if ((!t436) && u_is_con(s435, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s435, 0);
    u_value v_ps = u_field(s435, 1);
    u_value v_xss = u_field(s435, 2);
    u_value v_r = u_field(s435, 3);
    u_value v_b = u_field(s435, 4);
    u_value t437 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e438 = &t437;
    u_value l439 = v_xss;
    while (u_is_con(l439, &n_list_46_Cons)) {
      u_value v_xs = u_field(l439, 0);
      if (u_truth(u_mk_bool(1))) {
        e438 = u_append(e438, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l439 = u_field(l439, 1);
    }
    t436 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t437, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t436) && u_is_con(s435, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s435, 0);
    u_value v_ps = u_field(s435, 1);
    u_value v_ins = u_field(s435, 2);
    u_value v_outs = u_field(s435, 3);
    u_value v_refs = u_field(s435, 4);
    u_value v_b = u_field(s435, 5);
    t436 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t436) && u_is_con(s435, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s435, 0);
    t436 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t436) && u_is_con(s435, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s435, 0);
    u_value v_b = u_field(s435, 1);
    t436 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t436) && u_is_con(s435, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s435, 0);
    u_value v_b = u_field(s435, 1);
    t436 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t436) {
    t436 = v_c;
  }
  if (!t436) {
    u_no_case(s435);
  }
  return t436;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t440;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t440 = u_mk_con(&n_option_46_None, 0);
  } else {
    t440 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t440;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s441 = v_x;
  u_value t442 = NULL;
  if ((!t442) && u_is_con(s441, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s441, 0);
    t442 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s441, 1);
    t442 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s441, 0);
    u_value v_l = u_field(s441, 1);
    u_value v_r = u_field(s441, 2);
    t442 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s441, 0);
    u_value v_b = u_field(s441, 1);
    t442 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s441, 1);
    t442 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s441, 0);
    u_value v_bs = u_field(s441, 1);
    t442 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s441, 0);
    t442 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s441, 0);
    u_value v_t = u_field(s441, 1);
    t442 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t442) && u_is_con(s441, &n_syntax_46_STry)) {
    u_value v_b = u_field(s441, 0);
    u_value v_hs = u_field(s441, 1);
    t442 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t442) {
    t442 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t442) {
    u_no_case(s441);
  }
  return t442;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s443 = v_e;
  u_value t444 = NULL;
  if ((!t444) && u_is_con(s443, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s443, 0);
    u_value v_t = u_field(s443, 1);
    t444 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s443, 0);
    u_value v_args = u_field(s443, 1);
    t444 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s443, 1);
    t444 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s443, 0);
    u_value v_b = u_field(s443, 1);
    t444 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s443, 0);
    u_value v_l = u_field(s443, 1);
    u_value v_r = u_field(s443, 2);
    t444 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s443, 0);
    u_value v_bs = u_field(s443, 1);
    u_value t445 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e446 = &t445;
    u_value l447 = v_bs;
    while (u_is_con(l447, &n_list_46_Cons)) {
      u_value v_b = u_field(l447, 0);
      if (u_truth(u_mk_bool(1))) {
        e446 = u_append(e446, f_check_46_branch_exprs_1(v_b));
      }
      l447 = u_field(l447, 1);
    }
    t444 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t445)));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s443, 0);
    u_value v_ls = u_field(s443, 2);
    u_value v_c = u_field(s443, 3);
    t444 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s443, 0);
    t444 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t444) {
    t444 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t444) {
    u_no_case(s443);
  }
  return t444;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s448 = v_es;
  u_value t449 = NULL;
  if ((!t449) && u_is_con(s448, &n_list_46_Nil)) {
    t449 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t449) && u_is_con(s448, &n_list_46_Cons)) {
    u_value v_e = u_field(s448, 0);
    u_value v_rest = u_field(s448, 1);
    t449 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t449) {
    u_no_case(s448);
  }
  return t449;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s450 = v_b;
  u_value t451 = NULL;
  if ((!t451) && (u_is_con(s450, &n_syntax_46_Branch) && u_is_con(u_field(s450, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s450, 1), 0);
    u_value v_e = u_field(s450, 2);
    t451 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t451) && (u_is_con(s450, &n_syntax_46_Branch) && u_is_con(u_field(s450, 1), &n_option_46_None))) {
    u_value v_e = u_field(s450, 2);
    t451 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t451) {
    u_no_case(s450);
  }
  return t451;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s452 = v_xs;
  u_value t453 = NULL;
  if ((!t453) && u_is_con(s452, &n_list_46_Nil)) {
    t453 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t453) && u_is_con(s452, &n_list_46_Cons)) {
    u_value v_y = u_field(s452, 0);
    u_value v_ys = u_field(s452, 1);
    t453 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t453) {
    u_no_case(s452);
  }
  return t453;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s454 = v_bs;
  u_value t455 = NULL;
  if ((!t455) && u_is_con(s454, &n_list_46_Nil)) {
    t455 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t455) && (u_is_con(s454, &n_list_46_Cons) && (u_is_con(u_field(s454, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s454, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s454, 0), 1), 0);
    u_value v_y = u_field(u_field(s454, 0), 2);
    u_value v_rest = u_field(s454, 1);
    t455 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t455) && (u_is_con(s454, &n_list_46_Cons) && (u_is_con(u_field(s454, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s454, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s454, 0), 2);
    u_value v_rest = u_field(s454, 1);
    t455 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t455) {
    u_no_case(s454);
  }
  return t455;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s456 = v_hs;
  u_value t457 = NULL;
  if ((!t457) && u_is_con(s456, &n_list_46_Nil)) {
    t457 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t457) && (u_is_con(s456, &n_list_46_Cons) && (u_is_con(u_field(s456, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s456, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s456, 0), 3);
    u_value v_rest = u_field(s456, 1);
    t457 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t457) && (u_is_con(s456, &n_list_46_Cons) && u_is_con(u_field(s456, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s456, 0), 0);
    u_value v_b = u_field(u_field(s456, 0), 3);
    u_value v_rest = u_field(s456, 1);
    t457 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t457) {
    u_no_case(s456);
  }
  return t457;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s458 = v_c;
  u_value t459 = NULL;
  if ((!t459) && u_is_con(s458, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s458, 4);
    t459 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s458, 0);
    t459 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s458, 5);
    t459 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s458, 1);
    t459 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t459) && u_is_con(s458, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s458, 1);
    t459 = f_check_46_unknown_in_1(v_b);
  }
  if (!t459) {
    t459 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t459) {
    u_no_case(s458);
  }
  return t459;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s460 = v_p;
  u_value t461 = NULL;
  if ((!t461) && u_is_con(s460, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s460, 0);
    u_value v_args = u_field(s460, 1);
    t461 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t461) && u_is_con(s460, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s460, 0);
    t461 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t461) && u_is_con(s460, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s460, 0);
    t461 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t461) {
    t461 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t461) {
    u_no_case(s460);
  }
  return t461;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s462 = v_rows;
  u_value t463 = NULL;
  if ((!t463) && u_is_con(s462, &n_list_46_Nil)) {
    t463 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t463) && (u_is_con(s462, &n_list_46_Cons) && u_is_con(u_field(s462, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s462, 0), 0);
    u_value v_rest = u_field(s462, 1);
    u_value s464 = f_check_46_head_of_1(v_p);
    u_value t465 = NULL;
    if ((!t465) && u_is_con(s464, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s464, 0), 0);
      t465 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t465) && u_is_con(s464, &n_option_46_None)) {
      t465 = f_check_46_heads_1(v_rest);
    }
    if (!t465) {
      u_no_case(s464);
    }
    t463 = t465;
  }
  if ((!t463) && (u_is_con(s462, &n_list_46_Cons) && u_is_con(u_field(s462, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s462, 1);
    t463 = f_check_46_heads_1(v_rest);
  }
  if (!t463) {
    u_no_case(s462);
  }
  return t463;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s466 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t467 = NULL;
  if ((!t467) && u_is_con(s466, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s466, 0), 0);
    u_value v_ks = u_field(u_field(s466, 0), 1);
    u_value t468 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e469 = &t468;
    u_value l470 = v_ks;
    while (u_is_con(l470, &n_list_46_Cons)) {
      u_value v_a = u_field(l470, 0);
      if (u_truth(u_mk_bool(1))) {
        e469 = u_append(e469, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l470 = u_field(l470, 1);
    }
    t467 = t468;
  }
  if ((!t467) && u_is_con(s466, &n_option_46_None)) {
    t467 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t467) {
    u_no_case(s466);
  }
  return t467;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s471 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t472 = NULL;
  if ((!t472) && (u_is_con(s471, &n_syntax_46_TSort) && (u_is_con(u_field(s471, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s471, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s471, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s471, 1), &n_list_46_Nil)))))) {
    t472 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t472) && u_is_con(s471, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s471, 0);
    t472 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t472) && u_is_con(s471, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s471, 0);
    u_value v_args = u_field(s471, 1);
    u_value s473 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t474 = NULL;
    if ((!t474) && u_is_con(s473, &n_option_46_Some)) {
      u_value v_ks = u_field(s473, 0);
      u_value t475 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e476 = &t475;
      u_value l477 = v_ks;
      while (u_is_con(l477, &n_list_46_Cons)) {
        u_value v_k = u_field(l477, 0);
        if (u_truth(u_mk_bool(1))) {
          e476 = u_append(e476, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l477 = u_field(l477, 1);
      }
      t474 = u_mk_con(&n_option_46_Some, 1, t475);
    }
    if ((!t474) && u_is_con(s473, &n_option_46_None)) {
      t474 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t474) {
      u_no_case(s473);
    }
    t472 = t474;
  }
  if (!t472) {
    t472 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t472) {
    u_no_case(s471);
  }
  return t472;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s478 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t479 = NULL;
  if ((!t479) && u_is_con(s478, &n_option_46_Some)) {
    u_value v_ks = u_field(s478, 0);
    u_value s480 = f_list_46_lookup_2(v_h, v_ks);
    u_value t481 = NULL;
    if ((!t481) && u_is_con(s480, &n_option_46_Some)) {
      u_value v_ts = u_field(s480, 0);
      t481 = v_ts;
    }
    if ((!t481) && u_is_con(s480, &n_option_46_None)) {
      t481 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t481) {
      u_no_case(s480);
    }
    t479 = t481;
  }
  if ((!t479) && u_is_con(s478, &n_option_46_None)) {
    t479 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t479) {
    u_no_case(s478);
  }
  return t479;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s482 = v_ks;
  u_value t483 = NULL;
  if ((!t483) && u_is_con(s482, &n_list_46_Nil)) {
    t483 = u_mk_bool(1);
  }
  if ((!t483) && u_is_con(s482, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s482, 0), 0);
    u_value v_rest = u_field(s482, 1);
    u_value t484 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t484)) {
      t484 = f_check_46_covers_2(v_hs, v_rest);
    }
    t483 = t484;
  }
  if (!t483) {
    u_no_case(s482);
  }
  return t483;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s485 = v_rows;
  u_value t486 = NULL;
  if ((!t486) && u_is_con(s485, &n_list_46_Nil)) {
    t486 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t486) && (u_is_con(s485, &n_list_46_Cons) && u_is_con(u_field(s485, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s485, 0), 0);
    u_value v_ps = u_field(u_field(s485, 0), 1);
    u_value v_rest = u_field(s485, 1);
    u_value s487 = f_check_46_head_of_1(v_p);
    u_value t488 = NULL;
    if ((!t488) && u_is_con(s487, &n_option_46_Some)) {
      t488 = f_check_46_defaults_1(v_rest);
    }
    if ((!t488) && u_is_con(s487, &n_option_46_None)) {
      t488 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t488) {
      u_no_case(s487);
    }
    t486 = t488;
  }
  if ((!t486) && (u_is_con(s485, &n_list_46_Cons) && u_is_con(u_field(s485, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s485, 1);
    t486 = f_check_46_defaults_1(v_rest);
  }
  if (!t486) {
    u_no_case(s485);
  }
  return t486;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s489 = v_rows;
  u_value t490 = NULL;
  if ((!t490) && u_is_con(s489, &n_list_46_Nil)) {
    t490 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t490) && (u_is_con(s489, &n_list_46_Cons) && u_is_con(u_field(s489, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s489, 0), 0);
    u_value v_ps = u_field(u_field(s489, 0), 1);
    u_value v_rest = u_field(s489, 1);
    u_value s491 = f_check_46_head_of_1(v_p);
    u_value t492 = NULL;
    if ((!t492) && u_is_con(s491, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s491, 0), 0);
      u_value v_args = u_field(u_field(s491, 0), 1);
      u_value t493;
      if (u_truth(u_eq(v_g, v_h))) {
        t493 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t493 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t492 = t493;
    }
    if ((!t492) && u_is_con(s491, &n_option_46_None)) {
      t492 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t492) {
      u_no_case(s491);
    }
    t490 = t492;
  }
  if ((!t490) && (u_is_con(s489, &n_list_46_Cons) && u_is_con(u_field(s489, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s489, 1);
    t490 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t490) {
    u_no_case(s489);
  }
  return t490;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s494 = u_mk_tuple(2, v_q, v_ts);
  u_value t495 = NULL;
  if ((!t495) && u_is_con(u_field(s494, 0), &n_list_46_Nil)) {
    t495 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t495) && (u_is_con(u_field(s494, 0), &n_list_46_Cons) && u_is_con(u_field(s494, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s494, 0), 0);
    u_value v_ps = u_field(u_field(s494, 0), 1);
    u_value v_t = u_field(u_field(s494, 1), 0);
    u_value v_rest = u_field(u_field(s494, 1), 1);
    u_value s496 = f_check_46_head_of_1(v_p);
    u_value t497 = NULL;
    if ((!t497) && u_is_con(s496, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s496, 0), 0);
      u_value v_args = u_field(u_field(s496, 0), 1);
      t497 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t497) && u_is_con(s496, &n_option_46_None)) {
      u_value s498 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t499 = NULL;
      if ((!t499) && u_is_con(s498, &n_option_46_Some)) {
        u_value v_ks = u_field(s498, 0);
        u_value t500;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t500 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t500 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t499 = t500;
      }
      if ((!t499) && u_is_con(s498, &n_option_46_None)) {
        t499 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t499) {
        u_no_case(s498);
      }
      t497 = t499;
    }
    if (!t497) {
      u_no_case(s496);
    }
    t495 = t497;
  }
  if ((!t495) && (u_is_con(u_field(s494, 0), &n_list_46_Cons) && u_is_con(u_field(s494, 1), &n_list_46_Nil))) {
    t495 = u_mk_bool(0);
  }
  if (!t495) {
    u_no_case(s494);
  }
  return t495;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s501 = v_ks;
  u_value t502 = NULL;
  if ((!t502) && u_is_con(s501, &n_list_46_Nil)) {
    t502 = u_mk_bool(0);
  }
  if ((!t502) && u_is_con(s501, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s501, 0), 0);
    u_value v_args = u_field(u_field(s501, 0), 1);
    u_value v_more = u_field(s501, 1);
    u_value t503 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t503)) {
      t503 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t502 = t503;
  }
  if (!t502) {
    u_no_case(s501);
  }
  return t502;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s504 = v_h;
  u_value t505 = NULL;
  if ((!t505) && u_is_con(s504, &n_check_46_HCon)) {
    u_value v_n = u_field(s504, 0);
    u_value t506;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t506 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t506 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t505 = t506;
  }
  if ((!t505) && u_is_con(s504, &n_check_46_HLit)) {
    u_value v_v = u_field(s504, 0);
    t505 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t505) {
    u_no_case(s504);
  }
  return t505;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s507 = v_ts;
  u_value t508 = NULL;
  if ((!t508) && u_is_con(s507, &n_list_46_Nil)) {
    u_value t509;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t509 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t509 = u_mk_con(&n_option_46_None, 0);
    }
    t508 = t509;
  }
  if ((!t508) && u_is_con(s507, &n_list_46_Cons)) {
    u_value v_t = u_field(s507, 0);
    u_value v_rest = u_field(s507, 1);
    t508 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t508) {
    u_no_case(s507);
  }
  return t508;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s510 = v_cs;
  u_value t511 = NULL;
  if ((!t511) && u_is_con(s510, &n_option_46_Some)) {
    u_value v_ks = u_field(s510, 0);
    u_value t512;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t512 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t512 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t511 = t512;
  }
  if ((!t511) && u_is_con(s510, &n_option_46_None)) {
    t511 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t511) {
    u_no_case(s510);
  }
  return t511;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s513 = v_ks;
  u_value t514 = NULL;
  if ((!t514) && u_is_con(s513, &n_list_46_Nil)) {
    t514 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t514) && u_is_con(s513, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s513, 0), 0);
    u_value v_args = u_field(u_field(s513, 0), 1);
    u_value v_more = u_field(s513, 1);
    u_value s515 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t516 = NULL;
    if ((!t516) && u_is_con(s515, &n_option_46_Some)) {
      u_value v_ws = u_field(s515, 0);
      t516 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t516) && u_is_con(s515, &n_option_46_None)) {
      t516 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t516) {
      u_no_case(s515);
    }
    t514 = t516;
  }
  if (!t514) {
    u_no_case(s513);
  }
  return t514;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s517 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t518 = NULL;
  if ((!t518) && u_is_con(s517, &n_option_46_Some)) {
    u_value v_ws = u_field(s517, 0);
    t518 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t518) && u_is_con(s517, &n_option_46_None)) {
    t518 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t518) {
    u_no_case(s517);
  }
  return t518;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s519 = v_cs;
  u_value t520 = NULL;
  if ((!t520) && u_is_con(s519, &n_option_46_Some)) {
    u_value v_ks = u_field(s519, 0);
    u_value t521;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t521 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t521 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t520 = t521;
  }
  if ((!t520) && u_is_con(s519, &n_option_46_None)) {
    t520 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t520) {
    u_no_case(s519);
  }
  return t520;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s522 = v_ks;
  u_value t523 = NULL;
  if ((!t523) && u_is_con(s522, &n_list_46_Nil)) {
    t523 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t523) && u_is_con(s522, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s522, 0), 0);
    u_value v_args = u_field(u_field(s522, 0), 1);
    u_value v_rest = u_field(s522, 1);
    u_value t524;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t524 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t524 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t523 = t524;
  }
  if (!t523) {
    u_no_case(s522);
  }
  return t523;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value s525 = *r_s;
    int d526 = 0;
    if ((!d526) && u_is_con(s525, &n_check_46_State)) {
      u_value v_c = u_field(s525, 0);
      u_value v_su = u_field(s525, 1);
      u_value v_n = u_field(s525, 2);
      u_value v_m = u_field(s525, 3);
      u_value v_ms = u_field(s525, 4);
      u_value v_vs = u_field(s525, 5);
      d526 = 1;
      {
        u_value t527 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(v_n, u_mk_int(1)));
        u_value t528 = u_mk_con(&n_check_46_State, 6, v_c, v_su, u_add(v_n, u_mk_int(1)), v_m, v_ms, v_vs);
        *r_t = t527;
        *r_s = t528;
      }
    }
    if (!d526) {
      u_no_case(s525);
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
    u_value s529 = *r_s;
    int d530 = 0;
    if ((!d530) && u_is_con(s529, &n_check_46_State)) {
      u_value v_c = u_field(s529, 0);
      u_value v_su = u_field(s529, 1);
      u_value v_n = u_field(s529, 2);
      u_value v_m = u_field(s529, 3);
      u_value v_ms = u_field(s529, 4);
      u_value v_vs = u_field(s529, 5);
      d530 = 1;
      {
        u_value t531 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_put_3(v_i, v_t, v_su), v_n, v_m, v_ms, v_vs);
        *r_s = t531;
      }
    }
    if (!d530) {
      u_no_case(s529);
    }
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t532 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t532;
    u_value t533 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t533;
    u_value s534 = u_mk_tuple(2, v_a, v_b);
    int d535 = 0;
    if ((!d535) && (u_is_con(u_field(s534, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s534, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s534, 0), 0);
      u_value v_j = u_field(u_field(s534, 1), 0);
      d535 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d535) && u_is_con(u_field(s534, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s534, 0), 0);
      d535 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d535) && u_is_con(u_field(s534, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s534, 1), 0);
      d535 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d535) && (u_is_con(u_field(s534, 0), &n_syntax_46_TParam) && u_is_con(u_field(s534, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s534, 0), 0);
      u_value v_y = u_field(u_field(s534, 1), 0);
      d535 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d535) && (u_is_con(u_field(s534, 0), &n_syntax_46_TSort) && u_is_con(u_field(s534, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s534, 0), 0);
      u_value v_xs = u_field(u_field(s534, 0), 1);
      u_value v_g = u_field(u_field(s534, 1), 0);
      u_value v_ys = u_field(u_field(s534, 1), 1);
      d535 = 1;
      {
        u_value t536 = u_eq(v_f, v_g);
        if (u_truth(t536)) {
          t536 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t536)) {
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
    if ((!d535) && (u_is_con(u_field(s534, 0), &n_syntax_46_TFun) && u_is_con(u_field(s534, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s534, 0), 0);
      u_value v_r1 = u_field(u_field(s534, 0), 1);
      u_value v_ys = u_field(u_field(s534, 1), 0);
      u_value v_r2 = u_field(u_field(s534, 1), 1);
      d535 = 1;
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
    if ((!d535) && (u_is_con(u_field(s534, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s534, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s534, 0), 0);
      u_value v_ys = u_field(u_field(s534, 1), 0);
      d535 = 1;
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
    if (!d535) {
      d535 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d535) {
      u_no_case(s534);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s537 = u_mk_tuple(2, v_xs, v_ys);
    int d538 = 0;
    if ((!d538) && (u_is_con(u_field(s537, 0), &n_list_46_Cons) && u_is_con(u_field(s537, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s537, 0), 0);
      u_value v_xs1 = u_field(u_field(s537, 0), 1);
      u_value v_y = u_field(u_field(s537, 1), 0);
      u_value v_ys1 = u_field(u_field(s537, 1), 1);
      d538 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d538) && (u_is_con(u_field(s537, 0), &n_list_46_Nil) && u_is_con(u_field(s537, 1), &n_list_46_Nil))) {
      d538 = 1;
      {
      }
    }
    if (!d538) {
      d538 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d538) {
      u_no_case(s537);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s539 = v_xs;
    int d540 = 0;
    if ((!d540) && u_is_con(s539, &n_list_46_Nil)) {
      d540 = 1;
      {
        u_value t541 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t541;
      }
    }
    if ((!d540) && u_is_con(s539, &n_list_46_Cons)) {
      u_value v_more = u_field(s539, 1);
      d540 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t542 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t542;
      }
    }
    if (!d540) {
      u_no_case(s539);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s543 = v_ps;
    int d544 = 0;
    if ((!d544) && u_is_con(s543, &n_list_46_Nil)) {
      d544 = 1;
      {
        u_value t545 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t545;
      }
    }
    if ((!d544) && u_is_con(s543, &n_list_46_Cons)) {
      u_value v_p = u_field(s543, 0);
      u_value v_qs = u_field(s543, 1);
      d544 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t546 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t546;
      }
    }
    if (!d544) {
      u_no_case(s543);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s547 = v_sc;
    int d548 = 0;
    if (!d548) {
      u_value v_ps = u_field(s547, 0);
      u_value v_u = u_field(s547, 1);
      d548 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t549 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t549;
      }
    }
    if (!d548) {
      u_no_case(s547);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s550 = f_list_46_repeated_1(v_ns);
    int d551 = 0;
    if ((!d551) && u_is_con(s550, &n_option_46_Some)) {
      u_value v_n = u_field(s550, 0);
      d551 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
      }
    }
    if ((!d551) && u_is_con(s550, &n_option_46_None)) {
      d551 = 1;
      {
      }
    }
    if (!d551) {
      u_no_case(s550);
    }
  }
}

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s552 = f_list_46_repeated_1(v_ns);
    int d553 = 0;
    if ((!d553) && u_is_con(s552, &n_option_46_Some)) {
      u_value v_n = u_field(s552, 0);
      d553 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d553) && u_is_con(s552, &n_option_46_None)) {
      d553 = 1;
      {
      }
    }
    if (!d553) {
      u_no_case(s552);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s554 = v_ns;
    int d555 = 0;
    if ((!d555) && (u_is_con(s554, &n_list_46_Cons) && u_is_con(u_field(s554, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s554, 0);
      d555 = 1;
      {
        u_value t556 = v_k;
        *r_m = t556;
      }
    }
    if ((!d555) && u_is_con(s554, &n_list_46_Nil)) {
      d555 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d555) {
      d555 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d555) {
      u_no_case(s554);
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
    u_value s557 = v_t;
    int d558 = 0;
    if ((!d558) && u_is_con(s557, &n_syntax_46_THole)) {
      d558 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d558) && u_is_con(s557, &n_syntax_46_TParam)) {
      d558 = 1;
      {
        u_value t559 = v_t;
        *r_u = t559;
      }
    }
    if ((!d558) && u_is_con(s557, &n_syntax_46_TMeta)) {
      d558 = 1;
      {
        u_value t560 = v_t;
        *r_u = t560;
      }
    }
    if ((!d558) && u_is_con(s557, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s557, 0);
      u_value v_r = u_field(s557, 1);
      d558 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t561 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t561;
      }
    }
    if ((!d558) && u_is_con(s557, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s557, 0);
      d558 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t562 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t562;
      }
    }
    if ((!d558) && u_is_con(s557, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s557, 0);
      u_value v_args = u_field(s557, 1);
      d558 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t563 = v_t;
            *r_u = t563;
          }
        } else {
          {
            u_value s564 = f_check_46_visible_names_3(*r_s, v_n, f_check_46_sorts_named_2(f_check_46_State_46_ctx_1(*r_s), v_n));
            int d565 = 0;
            if ((!d565) && (u_is_con(s564, &n_list_46_Cons) && u_is_con(u_field(s564, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s564, 0);
              d565 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d565) && u_is_con(s564, &n_list_46_Nil)) {
              d565 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d565) {
              u_value v_ms = s564;
              d565 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d565) {
              u_no_case(s564);
            }
          }
        }
      }
    }
    if (!d558) {
      u_no_case(s557);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s566 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d567 = 0;
    if ((!d567) && u_is_con(s566, &n_option_46_Some)) {
      u_value v_k = u_field(s566, 0);
      d567 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t568 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t568;
      }
    }
    if ((!d567) && u_is_con(s566, &n_option_46_None)) {
      d567 = 1;
      {
        u_value s569 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d570 = 0;
        if ((!d570) && u_is_con(s569, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s569, 0), 0);
          u_value v_rhs = u_field(u_field(s569, 0), 1);
          d570 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t571 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t571;
          }
        }
        if ((!d570) && u_is_con(s569, &n_option_46_None)) {
          d570 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d570) {
          u_no_case(s569);
        }
      }
    }
    if (!d567) {
      u_no_case(s566);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s572 = v_ts;
    int d573 = 0;
    if ((!d573) && u_is_con(s572, &n_list_46_Nil)) {
      d573 = 1;
      {
        u_value t574 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t574;
      }
    }
    if ((!d573) && u_is_con(s572, &n_list_46_Cons)) {
      u_value v_t = u_field(s572, 0);
      u_value v_more = u_field(s572, 1);
      d573 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t575 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t575;
      }
    }
    if (!d573) {
      u_no_case(s572);
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
    u_value s576 = v_ts;
    int d577 = 0;
    if ((!d577) && u_is_con(s576, &n_list_46_Nil)) {
      d577 = 1;
      {
        u_value t578 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t578;
      }
    }
    if ((!d577) && u_is_con(s576, &n_list_46_Cons)) {
      u_value v_t = u_field(s576, 0);
      u_value v_more = u_field(s576, 1);
      d577 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t579 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t579;
      }
    }
    if (!d577) {
      u_no_case(s576);
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
    u_value s580 = v_xs;
    int d581 = 0;
    if ((!d581) && u_is_con(s580, &n_list_46_Nil)) {
      d581 = 1;
      {
        u_value t582 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t582;
      }
    }
    if ((!d581) && (u_is_con(s580, &n_list_46_Cons) && u_is_con(u_field(s580, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s580, 0), 0);
      u_value v_t = u_field(u_field(s580, 0), 1);
      u_value v_more = u_field(s580, 1);
      d581 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t583 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t583;
      }
    }
    if (!d581) {
      u_no_case(s580);
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
    u_value s584 = v_xs;
    int d585 = 0;
    if ((!d585) && u_is_con(s584, &n_list_46_Nil)) {
      d585 = 1;
      {
        u_value t586 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t586;
      }
    }
    if ((!d585) && (u_is_con(s584, &n_list_46_Cons) && u_is_con(u_field(s584, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s584, 0), 0);
      u_value v_t = u_field(u_field(s584, 0), 1);
      u_value v_more = u_field(s584, 1);
      d585 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t587 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t587;
      }
    }
    if (!d585) {
      u_no_case(s584);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s588 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d589 = 0;
    if ((!d589) && u_is_con(s588, &n_list_46_Nil)) {
      d589 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d589) && (u_is_con(s588, &n_list_46_Cons) && u_is_con(u_field(s588, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s588, 0), 0);
      u_value v_one = u_field(u_field(s588, 0), 1);
      d589 = 1;
      {
        u_value t590 = v_m1;
        u_value t591 = v_one;
        *r_m = t590;
        *r_sc = t591;
      }
    }
    if (!d589) {
      u_value v_cs = s588;
      d589 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s592 = f_check_46_with_arity_2(v_cs, v_k);
        int d593 = 0;
        if ((!d593) && (u_is_con(s592, &n_list_46_Cons) && u_is_con(u_field(s592, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s592, 0), 0);
          u_value v_one = u_field(u_field(s592, 0), 1);
          d593 = 1;
          {
            u_value t594 = v_m1;
            u_value t595 = v_one;
            *r_m = t594;
            *r_sc = t595;
          }
        }
        if ((!d593) && u_is_con(s592, &n_list_46_Nil)) {
          d593 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d593) {
          u_value v_more = s592;
          d593 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d593) {
          u_no_case(s592);
        }
      }
    }
    if (!d589) {
      u_no_case(s588);
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
    u_value t596 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t596;
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
    u_value s597 = v_e;
    int d598 = 0;
    if ((!d598) && u_is_con(s597, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s597, 0);
      d598 = 1;
      {
        u_value t599 = v_e;
        u_value t600 = f_check_46_literal_1(v_v);
        *r_e1 = t599;
        *r_t = t600;
      }
    }
    if ((!d598) && (u_is_con(s597, &n_syntax_46_EVar) && u_is_con(u_field(s597, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s597, 0), 0);
      u_value v_a = u_field(u_field(s597, 0), 1);
      d598 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, f_prelude_46__45__1(u_mk_int(1)), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t601 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t602 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t601;
        *r_t = t602;
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s597, 0);
      u_value v_args = u_field(s597, 1);
      d598 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s597, 0);
      u_value v_body = u_field(s597, 1);
      d598 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t603 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t604 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t603;
        *r_t = t604;
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s597, 0);
      u_value v_body = u_field(s597, 1);
      d598 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t605 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t606 = v_u;
        *r_e1 = t605;
        *r_t = t606;
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s597, 0);
      u_value v_left = u_field(s597, 1);
      u_value v_right = u_field(s597, 2);
      d598 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t607 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t608 = v_u;
        *r_e1 = t607;
        *r_t = t608;
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s597, 0);
      u_value v_bound = u_field(s597, 1);
      u_value v_lists = u_field(s597, 2);
      u_value v_cond = u_field(s597, 3);
      d598 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t609 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t610 = f_check_46_tlist_1(v_u);
        *r_e1 = t609;
        *r_t = t610;
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s597, 0);
      d598 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t611 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t612 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t611;
        *r_t = t612;
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s597, 0);
      u_value v_a = u_field(s597, 1);
      d598 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t613 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t613;
      }
    }
    if ((!d598) && u_is_con(s597, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s597, 0);
      u_value v_cases = u_field(s597, 1);
      d598 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t614 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e615 = &t614;
        u_value l616 = v_bs;
        while (u_is_con(l616, &n_list_46_Cons)) {
          u_value v_br = u_field(l616, 0);
          if (u_truth(u_mk_bool(1))) {
            e615 = u_append(e615, f_check_46_case_of_1(v_br));
          }
          l616 = u_field(l616, 1);
        }
        p_check_46_coverage(*r_s, v_u, t614);
        u_value t617 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t617;
      }
    }
    if (!d598) {
      u_no_case(s597);
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
    u_value s618 = v_f;
    int d619 = 0;
    if ((!d619) && (u_is_con(s618, &n_syntax_46_EVar) && (u_is_con(u_field(s618, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s618, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s618, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s618, 0), 0), 1);
      u_value v_a = u_field(u_field(s618, 0), 1);
      u_value t620 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t620)) {
        t620 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t620)) {
        d619 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d619) && (u_is_con(s618, &n_syntax_46_EVar) && u_is_con(u_field(s618, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s618, 0), 0);
      u_value v_a = u_field(u_field(s618, 0), 1);
      d619 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t621 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t621;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t622 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t622;
          }
        }
      }
    }
    if (!d619) {
      d619 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t623 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t623;
      }
    }
    if (!d619) {
      u_no_case(s618);
    }
  }
}

void p_check_46_field_access(u_value v_env, u_value v_f, u_value v_a, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_arg1 = NULL;
    u_value v_u = NULL;
    u_value v_acc = NULL;
    u_value v_x = NULL;
    u_value s624 = v_args;
    int d625 = 0;
    if ((!d625) && (u_is_con(s624, &n_list_46_Cons) && u_is_con(u_field(s624, 1), &n_list_46_Nil))) {
      u_value v_arg = u_field(s624, 0);
      d625 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s626 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d627 = 0;
        if ((!d627) && (u_is_con(s626, &n_syntax_46_TSort) && (u_is_con(u_field(s626, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s626, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s626, 0), 0), 0);
          u_value v_tn = u_field(u_field(s626, 0), 1);
          d627 = 1;
          {
            u_value t628 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t628;
          }
        }
        if (!d627) {
          u_value v_v = s626;
          d627 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d627) {
          u_no_case(s626);
        }
        u_value s629 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d630 = 0;
        if ((!d630) && (u_is_con(s629, &n_list_46_Cons) && u_is_con(u_field(s629, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s629, 0);
          d630 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d630) {
          d630 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d630) {
          u_no_case(s629);
        }
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)), *r_t), r_s);
        u_value t631 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, u_mk_con(&n_list_46_Nil, 0)));
        *r_e1 = t631;
      }
    }
    if (!d625) {
      d625 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d625) {
      u_no_case(s624);
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
    u_value s632 = u_mk_tuple(2, v_es, v_ts);
    int d633 = 0;
    if ((!d633) && (u_is_con(u_field(s632, 0), &n_list_46_Cons) && u_is_con(u_field(s632, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s632, 0), 0);
      u_value v_more = u_field(u_field(s632, 0), 1);
      u_value v_t = u_field(u_field(s632, 1), 0);
      u_value v_ts1 = u_field(u_field(s632, 1), 1);
      d633 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t634 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t634;
      }
    }
    if ((!d633) && (u_is_con(u_field(s632, 0), &n_list_46_Nil) && u_is_con(u_field(s632, 1), &n_list_46_Nil))) {
      d633 = 1;
      {
        u_value t635 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t635;
      }
    }
    if (!d633) {
      d633 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d633) {
      u_no_case(s632);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s636 = v_es;
    int d637 = 0;
    if ((!d637) && u_is_con(s636, &n_list_46_Nil)) {
      d637 = 1;
      {
        u_value t638 = u_mk_con(&n_list_46_Nil, 0);
        u_value t639 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t638;
        *r_ts = t639;
      }
    }
    if ((!d637) && u_is_con(s636, &n_list_46_Cons)) {
      u_value v_e = u_field(s636, 0);
      u_value v_more = u_field(s636, 1);
      d637 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t640 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t641 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t640;
        *r_ts = t641;
      }
    }
    if (!d637) {
      u_no_case(s636);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s642 = u_mk_tuple(2, v_xs, v_lists);
    int d643 = 0;
    if ((!d643) && (u_is_con(u_field(s642, 0), &n_list_46_Cons) && u_is_con(u_field(s642, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s642, 0), 0);
      u_value v_ys = u_field(u_field(s642, 0), 1);
      u_value v_e = u_field(u_field(s642, 1), 0);
      u_value v_es = u_field(u_field(s642, 1), 1);
      d643 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t644 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t644;
      }
    }
    if (!d643) {
      d643 = 1;
      {
        u_value t645 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t645;
      }
    }
    if (!d643) {
      u_no_case(s642);
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
    u_value s646 = v_cases;
    int d647 = 0;
    if ((!d647) && u_is_con(s646, &n_list_46_Nil)) {
      d647 = 1;
      {
        u_value t648 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t648;
      }
    }
    if ((!d647) && (u_is_con(s646, &n_list_46_Cons) && u_is_con(u_field(s646, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s646, 0), 0);
      u_value v_g = u_field(u_field(s646, 0), 1);
      u_value v_b = u_field(u_field(s646, 0), 2);
      u_value v_more = u_field(s646, 1);
      d647 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t649 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t649;
      }
    }
    if (!d647) {
      u_no_case(s646);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s650 = v_g;
    int d651 = 0;
    if ((!d651) && u_is_con(s650, &n_option_46_Some)) {
      u_value v_e = u_field(s650, 0);
      d651 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t652 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t652;
      }
    }
    if ((!d651) && u_is_con(s650, &n_option_46_None)) {
      d651 = 1;
      {
        u_value t653 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t653;
      }
    }
    if (!d651) {
      u_no_case(s650);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t654 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e655 = &t654;
    u_value l656 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l656, &n_list_46_Cons)) {
      u_value v_p = u_field(l656, 0);
      if (u_truth(u_mk_bool(1))) {
        e655 = u_append(e655, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l656 = u_field(l656, 1);
    }
    u_value s657 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t654, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d658 = 0;
    if ((!d658) && (u_is_con(s657, &n_option_46_Some) && u_is_con(u_field(s657, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s657, 0), 0);
      d658 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d658) {
      d658 = 1;
      {
      }
    }
    if (!d658) {
      u_no_case(s657);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s659 = v_cs;
  u_value t660 = NULL;
  if ((!t660) && u_is_con(s659, &n_list_46_Nil)) {
    t660 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t660) && (u_is_con(s659, &n_list_46_Cons) && u_equals(u_field(u_field(s659, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s659, 0), 0);
    u_value v_rest = u_field(s659, 1);
    t660 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t660) && (u_is_con(s659, &n_list_46_Cons) && u_equals(u_field(u_field(s659, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s659, 1);
    t660 = f_check_46_unguarded_1(v_rest);
  }
  if (!t660) {
    u_no_case(s659);
  }
  return t660;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s661 = v_cs;
    int d662 = 0;
    if ((!d662) && u_is_con(s661, &n_list_46_Nil)) {
      d662 = 1;
      {
      }
    }
    if ((!d662) && u_is_con(s661, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s661, 0), 0);
      u_value v_open = u_field(u_field(s661, 0), 1);
      u_value v_rest = u_field(s661, 1);
      d662 = 1;
      {
        u_value t663 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e664 = &t663;
        u_value l665 = v_before;
        while (u_is_con(l665, &n_list_46_Cons)) {
          u_value v_q = u_field(l665, 0);
          if (u_truth(u_mk_bool(1))) {
            e664 = u_append(e664, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l665 = u_field(l665, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t663, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d662) {
      u_no_case(s661);
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
    u_value s666 = v_p;
    int d667 = 0;
    if ((!d667) && u_is_con(s666, &n_syntax_46_PWild)) {
      d667 = 1;
      {
        u_value t668 = v_p;
        u_value t669 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t668;
        *r_xs = t669;
      }
    }
    if ((!d667) && u_is_con(s666, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s666, 0);
      d667 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t670 = v_p;
        u_value t671 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t670;
        *r_xs = t671;
      }
    }
    if ((!d667) && (u_is_con(s666, &n_syntax_46_PVar) && u_is_con(u_field(s666, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s666, 0), 0);
      u_value v_a = u_field(u_field(s666, 0), 1);
      d667 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_State_46_ctx_1(*r_s), v_n)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t672 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t673 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t672;
            *r_xs = t673;
          }
        }
      }
    }
    if ((!d667) && u_is_con(s666, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s666, 0);
      u_value v_args = u_field(s666, 1);
      d667 = 1;
      {
        u_value s674 = f_check_46_visible_names_3(*r_s, v_n, f_check_46_constrs_named_2(f_check_46_State_46_ctx_1(*r_s), v_n));
        int d675 = 0;
        if ((!d675) && (u_is_con(s674, &n_list_46_Cons) && u_is_con(u_field(s674, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s674, 0);
          d675 = 1;
          {
            u_value t676 = v_k1;
            v_k = t676;
          }
        }
        if ((!d675) && u_is_con(s674, &n_list_46_Nil)) {
          d675 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d675) {
          u_value v_ks = s674;
          d675 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d675) {
          u_no_case(s674);
        }
        u_value s677 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d678 = 0;
        if ((!d678) && u_is_con(s677, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s677, 0), 0);
          u_value v_ks = u_field(u_field(s677, 0), 1);
          u_value v_r = u_field(u_field(s677, 0), 2);
          d678 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t679 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e680 = &t679;
            u_value l681 = v_ks;
            while (u_is_con(l681, &n_list_46_Cons)) {
              u_value v_kt = u_field(l681, 0);
              if (u_truth(u_mk_bool(1))) {
                e680 = u_append(e680, f_check_46_subst_2(v_kt, v_m));
              }
              l681 = u_field(l681, 1);
            }
            p_check_46_binder_list(v_args, t679, &v_qs, r_xs, r_s);
            u_value t682 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t682;
          }
        }
        if ((!d678) && u_is_con(s677, &n_option_46_None)) {
          d678 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d678) {
          u_no_case(s677);
        }
      }
    }
    if ((!d667) && u_is_con(s666, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s666, 0);
      d667 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t683 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t683;
      }
    }
    if (!d667) {
      u_no_case(s666);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s684 = u_mk_tuple(2, v_ps, v_ts);
    int d685 = 0;
    if ((!d685) && (u_is_con(u_field(s684, 0), &n_list_46_Cons) && u_is_con(u_field(s684, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s684, 0), 0);
      u_value v_more = u_field(u_field(s684, 0), 1);
      u_value v_t = u_field(u_field(s684, 1), 0);
      u_value v_ts1 = u_field(u_field(s684, 1), 1);
      d685 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t686 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t687 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t686;
        *r_xs = t687;
      }
    }
    if ((!d685) && (u_is_con(u_field(s684, 0), &n_list_46_Nil) && u_is_con(u_field(s684, 1), &n_list_46_Nil))) {
      d685 = 1;
      {
        u_value t688 = u_mk_con(&n_list_46_Nil, 0);
        u_value t689 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t688;
        *r_xs = t689;
      }
    }
    if (!d685) {
      d685 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d685) {
      u_no_case(s684);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s690 = v_x;
    int d691 = 0;
    if ((!d691) && u_is_con(s690, &n_syntax_46_Var)) {
      u_value v_n = u_field(s690, 0);
      u_value v_a = u_field(s690, 1);
      d691 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s692 = f_list_46_lookup_2(v_n, v_env);
        int d693 = 0;
        if ((!d693) && u_is_con(s692, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s692, 0), 1);
          d693 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t694 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t694;
          }
        }
        if ((!d693) && u_is_con(s692, &n_option_46_None)) {
          d693 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d693) {
          u_no_case(s692);
        }
      }
    }
    if (!d691) {
      u_no_case(s690);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s695 = v_xs;
    int d696 = 0;
    if ((!d696) && u_is_con(s695, &n_list_46_Nil)) {
      d696 = 1;
      {
        u_value t697 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t697;
      }
    }
    if ((!d696) && u_is_con(s695, &n_list_46_Cons)) {
      u_value v_x = u_field(s695, 0);
      u_value v_more = u_field(s695, 1);
      d696 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t698 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t698;
      }
    }
    if (!d696) {
      u_no_case(s695);
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
    u_value s699 = v_x;
    int d700 = 0;
    if ((!d700) && u_is_con(s699, &n_syntax_46_SSkip)) {
      d700 = 1;
      {
        u_value t701 = v_x;
        *r_y = t701;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s699, 0);
      d700 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t702 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t702;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s699, 0);
      u_value v_body = u_field(s699, 1);
      d700 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t703 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t703;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s699, 0);
      u_value v_left = u_field(s699, 1);
      u_value v_right = u_field(s699, 2);
      d700 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t704 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t704;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s699, 0);
      u_value v_body = u_field(s699, 1);
      d700 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t705 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t705;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s699, 0);
      u_value v_ins = u_field(s699, 1);
      u_value v_outs = u_field(s699, 2);
      u_value v_refs = u_field(s699, 3);
      d700 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s699, 0);
      u_value v_cases = u_field(s699, 1);
      d700 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t706 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e707 = &t706;
        u_value l708 = v_bs;
        while (u_is_con(l708, &n_list_46_Cons)) {
          u_value v_br = u_field(l708, 0);
          if (u_truth(u_mk_bool(1))) {
            e707 = u_append(e707, f_check_46_case_of_1(v_br));
          }
          l708 = u_field(l708, 1);
        }
        p_check_46_coverage(*r_s, v_t, t706);
        u_value t709 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t709;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s699, 0);
      d700 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t710 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t710;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s699, 0);
      u_value v_a = u_field(s699, 1);
      d700 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t711 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t711;
      }
    }
    if ((!d700) && u_is_con(s699, &n_syntax_46_STry)) {
      u_value v_body = u_field(s699, 0);
      u_value v_handlers = u_field(s699, 1);
      d700 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t712 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t712;
      }
    }
    if (!d700) {
      u_no_case(s699);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s713 = v_xs;
  u_value t714 = NULL;
  if ((!t714) && (u_is_con(s713, &n_list_46_Cons) && (u_is_con(u_field(s713, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s713, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s713, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s713, 0), 0), 0), 0);
    u_value v_rest = u_field(s713, 1);
    t714 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t714) {
    t714 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t714) {
    u_no_case(s713);
  }
  return t714;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s715 = v_xs;
    int d716 = 0;
    if ((!d716) && u_is_con(s715, &n_list_46_Nil)) {
      d716 = 1;
      {
      }
    }
    if ((!d716) && (u_is_con(s715, &n_list_46_Cons) && u_is_con(u_field(s715, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s715, 0), 0);
      u_value v_t = u_field(u_field(s715, 0), 1);
      u_value v_rest = u_field(s715, 1);
      d716 = 1;
      {
        u_value t717 = f_check_46_holes_1(v_t);
        if (u_truth(t717)) {
          t717 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t717)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d716) {
      u_no_case(s715);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s718 = v_xs;
    int d719 = 0;
    if ((!d719) && u_is_con(s718, &n_list_46_Nil)) {
      d719 = 1;
      {
        u_value t720 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t720;
      }
    }
    if ((!d719) && u_is_con(s718, &n_list_46_Cons)) {
      u_value v_x = u_field(s718, 0);
      u_value v_more = u_field(s718, 1);
      d719 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t721 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t721;
      }
    }
    if (!d719) {
      u_no_case(s718);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t726;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_722 = U_LIT("os_args");
    static struct u_object l_os_read_723 = U_LIT("os_read");
    static struct u_object l_os_write_724 = U_LIT("os_write");
    static struct u_object l_os_library_725 = U_LIT("os_library");
    t726 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_722), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_723), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_724), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_725), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t726 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t726;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t727 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e728 = &t727;
  static struct u_object l_Program_729 = U_LIT("Program");
  u_value l730 = f_ctx_46_names_of_2(v_c, &l_Program_729);
  while (u_is_con(l730, &n_list_46_Cons)) {
    u_value v_d = u_field(l730, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e728 = u_append(e728, v_d);
    }
    l730 = u_field(l730, 1);
  }
  u_value s731 = t727;
  u_value t732 = NULL;
  if ((!t732) && u_is_con(s731, &n_list_46_Cons)) {
    u_value v_d = u_field(s731, 0);
    static struct u_object l_read_733 = U_LIT("read");
    static struct u_object l_library_734 = U_LIT("library");
    t732 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_733), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_734), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t732) {
    t732 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t732) {
    u_no_case(s731);
  }
  return t732;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s735 = f_check_46_visible_names_3(*r_s, v_p, f_check_46_procs_named_2(f_check_46_State_46_ctx_1(*r_s), v_p));
    int d736 = 0;
    if ((!d736) && (u_is_con(s735, &n_list_46_Cons) && u_is_con(u_field(s735, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s735, 0);
      d736 = 1;
      {
        u_value t737 = v_q1;
        v_q = t737;
      }
    }
    if ((!d736) && u_is_con(s735, &n_list_46_Nil)) {
      d736 = 1;
      {
        u_value t738 = v_p;
        v_q = t738;
      }
    }
    if (!d736) {
      u_value v_qs = s735;
      d736 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d736) {
      u_no_case(s735);
    }
    u_value s739 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d740 = 0;
    if ((!d740) && u_is_con(s739, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s739, 0), 0);
      u_value v_is = u_field(u_field(s739, 0), 1);
      u_value v_os = u_field(u_field(s739, 0), 2);
      u_value v_rs = u_field(u_field(s739, 0), 3);
      d740 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t741 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e742 = &t741;
        u_value l743 = v_is;
        while (u_is_con(l743, &n_list_46_Cons)) {
          u_value v_t = u_field(l743, 0);
          if (u_truth(u_mk_bool(1))) {
            e742 = u_append(e742, f_check_46_subst_2(v_t, v_m));
          }
          l743 = u_field(l743, 1);
        }
        p_check_46_checks(v_env, v_ins, t741, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t744 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e745 = &t744;
        u_value l746 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l746, &n_list_46_Cons)) {
          u_value v_t = u_field(l746, 0);
          if (u_truth(u_mk_bool(1))) {
            e745 = u_append(e745, f_check_46_subst_2(v_t, v_m));
          }
          l746 = u_field(l746, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t744, r_s);
        u_value t747 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t747;
      }
    }
    if ((!d740) && u_is_con(s739, &n_option_46_None)) {
      d740 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d740) {
      u_no_case(s739);
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
    u_value s748 = v_cases;
    int d749 = 0;
    if ((!d749) && u_is_con(s748, &n_list_46_Nil)) {
      d749 = 1;
      {
        u_value t750 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t750;
      }
    }
    if ((!d749) && (u_is_con(s748, &n_list_46_Cons) && u_is_con(u_field(s748, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s748, 0), 0);
      u_value v_g = u_field(u_field(s748, 0), 1);
      u_value v_b = u_field(u_field(s748, 0), 2);
      u_value v_more = u_field(s748, 1);
      d749 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t751 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t751;
      }
    }
    if (!d749) {
      u_no_case(s748);
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
    u_value s752 = v_hs;
    int d753 = 0;
    if ((!d753) && u_is_con(s752, &n_list_46_Nil)) {
      d753 = 1;
      {
        u_value t754 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t754;
      }
    }
    if ((!d753) && (u_is_con(s752, &n_list_46_Cons) && u_is_con(u_field(s752, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s752, 0), 0);
      u_value v_p = u_field(u_field(s752, 0), 1);
      u_value v_g = u_field(u_field(s752, 0), 2);
      u_value v_b = u_field(u_field(s752, 0), 3);
      u_value v_more = u_field(s752, 1);
      d753 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t755 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t755;
      }
    }
    if (!d753) {
      u_no_case(s752);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t756 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t756)) {
      t756 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t757 = t756;
    if (!u_truth(t757)) {
      t757 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t758 = t757;
    if (!u_truth(t758)) {
      t758 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t758)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t759 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t759)) {
      t759 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t760 = t759;
    if (!u_truth(t760)) {
      t760 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t760)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_761 = U_LIT("Bool");
    static struct u_object l_Int_762 = U_LIT("Int");
    static struct u_object l_Str_763 = U_LIT("Str");
    static struct u_object l_Char_764 = U_LIT("Char");
    static struct u_object l__45__62__765 = U_LIT("->");
    u_value t766 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_761, u_mk_con(&n_list_46_Cons, 2, &l_Int_762, u_mk_con(&n_list_46_Cons, 2, &l_Str_763, u_mk_con(&n_list_46_Cons, 2, &l_Char_764, u_mk_con(&n_list_46_Cons, 2, &l__45__62__765, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t766)) {
      t766 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t767 = t766;
    if (!u_truth(t767)) {
      t767 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t767)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t768 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e769 = &t768;
    u_value l770 = v_ts;
    while (u_is_con(l770, &n_list_46_Cons)) {
      u_value v_t = u_field(l770, 0);
      if (u_truth(u_mk_bool(1))) {
        e769 = u_append(e769, f_check_46_free_1(v_t));
      }
      l770 = u_field(l770, 1);
    }
    u_value t771 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t768)), f_check_46_params_1(v_ps));
    v_undeclared = t771;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s772 = v_cs;
    int d773 = 0;
    if ((!d773) && u_is_con(s772, &n_list_46_Nil)) {
      d773 = 1;
      {
      }
    }
    if ((!d773) && (u_is_con(s772, &n_list_46_Cons) && u_is_con(u_field(u_field(s772, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s772, 0), 0);
      u_value v_n = u_field(u_field(u_field(s772, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s772, 0), 1), 1);
      u_value v_rest = u_field(s772, 1);
      d773 = 1;
      {
        u_value t774 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t774;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t775 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t775;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d773) && u_is_con(s772, &n_list_46_Cons)) {
      u_value v_rest = u_field(s772, 1);
      d773 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d773) {
      u_no_case(s772);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s776 = v_cs;
    int d777 = 0;
    if ((!d777) && u_is_con(s776, &n_list_46_Nil)) {
      d777 = 1;
      {
      }
    }
    if ((!d777) && (u_is_con(s776, &n_list_46_Cons) && u_is_con(u_field(u_field(s776, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s776, 0), 0);
      u_value v_n = u_field(u_field(u_field(s776, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s776, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s776, 0), 1), 2);
      u_value v_rest = u_field(s776, 1);
      d777 = 1;
      {
        u_value t778 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t778;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t779 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t779;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d777) && u_is_con(s776, &n_list_46_Cons)) {
      u_value v_rest = u_field(s776, 1);
      d777 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d777) {
      u_no_case(s776);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
  {
    u_value s780 = v_cs;
    int d781 = 0;
    if ((!d781) && u_is_con(s780, &n_list_46_Nil)) {
      d781 = 1;
      {
      }
    }
    if ((!d781) && (u_is_con(s780, &n_list_46_Cons) && u_is_con(u_field(u_field(s780, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s780, 0), 0);
      u_value v_n = u_field(u_field(u_field(s780, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s780, 0), 1), 1);
      u_value v_ks = u_field(u_field(u_field(s780, 0), 1), 2);
      u_value v_rest = u_field(s780, 1);
      d781 = 1;
      {
        u_value t782 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t782;
        p_check_46_unique_ids(v_ps);
        u_value t783 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e784 = &t783;
        u_value l785 = v_ks;
        while (u_is_con(l785, &n_list_46_Cons)) {
          u_value v_k = u_field(l785, 0);
          if (u_truth(u_mk_bool(1))) {
            e784 = u_append(e784, f_syntax_46_Constr_46_name_1(v_k));
          }
          l785 = u_field(l785, 1);
        }
        u_value t786 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t783));
        *r_s = t786;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d781) && u_is_con(s780, &n_list_46_Cons)) {
      u_value v_rest = u_field(s780, 1);
      d781 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d781) {
      u_no_case(s780);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s787 = v_ks;
    int d788 = 0;
    if ((!d788) && u_is_con(s787, &n_list_46_Nil)) {
      d788 = 1;
      {
      }
    }
    if ((!d788) && (u_is_con(s787, &n_list_46_Cons) && u_is_con(u_field(s787, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s787, 0), 0);
      u_value v_args = u_field(u_field(s787, 0), 1);
      u_value v_rest = u_field(s787, 1);
      d788 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t789 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t789;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d788) {
      u_no_case(s787);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s790 = v_xss;
  u_value t791 = NULL;
  if ((!t791) && u_is_con(s790, &n_list_46_Nil)) {
    t791 = v_r;
  }
  if ((!t791) && u_is_con(s790, &n_list_46_Cons)) {
    u_value v_xs = u_field(s790, 0);
    u_value v_rest = u_field(s790, 1);
    t791 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t791) {
    u_no_case(s790);
  }
  return t791;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s792 = v_xss;
  u_value t793 = NULL;
  if ((!t793) && u_is_con(s792, &n_list_46_Nil)) {
    t793 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t793) && u_is_con(s792, &n_list_46_Cons)) {
    u_value v_xs = u_field(s792, 0);
    t793 = v_xs;
  }
  if (!t793) {
    u_no_case(s792);
  }
  return t793;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s794 = v_xss;
    int d795 = 0;
    if ((!d795) && u_is_con(s794, &n_list_46_Nil)) {
      d795 = 1;
      {
        u_value t796 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t796;
      }
    }
    if ((!d795) && u_is_con(s794, &n_list_46_Cons)) {
      u_value v_xs = u_field(s794, 0);
      u_value v_more = u_field(s794, 1);
      d795 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t797 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t797;
      }
    }
    if (!d795) {
      u_no_case(s794);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s798 = v_cs;
    int d799 = 0;
    if ((!d799) && u_is_con(s798, &n_list_46_Nil)) {
      d799 = 1;
      {
      }
    }
    if ((!d799) && (u_is_con(s798, &n_list_46_Cons) && u_is_con(u_field(u_field(s798, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s798, 0), 0);
      u_value v_n = u_field(u_field(u_field(s798, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s798, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s798, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s798, 0), 1), 3);
      u_value v_rest = u_field(s798, 1);
      d799 = 1;
      {
        u_value t800 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t800;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t801 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t801;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d799) && u_is_con(s798, &n_list_46_Cons)) {
      u_value v_rest = u_field(s798, 1);
      d799 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d799) {
      u_no_case(s798);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s802 = v_cs;
    int d803 = 0;
    if ((!d803) && u_is_con(s802, &n_list_46_Nil)) {
      d803 = 1;
      {
      }
    }
    if ((!d803) && (u_is_con(s802, &n_list_46_Cons) && u_is_con(u_field(u_field(s802, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s802, 0), 0);
      u_value v_n = u_field(u_field(u_field(s802, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s802, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s802, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s802, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s802, 0), 1), 4);
      u_value v_rest = u_field(s802, 1);
      d803 = 1;
      {
        u_value t804 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t804;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t805 = f_syntax_46_types_1(v_xs);
        v_ts = t805;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t806 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t806;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d803) && u_is_con(s802, &n_list_46_Cons)) {
      u_value v_rest = u_field(s802, 1);
      d803 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d803) {
      u_no_case(s802);
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
    u_value s807 = v_c;
    int d808 = 0;
    if ((!d808) && u_is_con(s807, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s807, 0);
      u_value v_ps = u_field(s807, 1);
      u_value v_bound = u_field(s807, 2);
      u_value v_res = u_field(s807, 3);
      u_value v_body = u_field(s807, 4);
      d808 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t809 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t809;
      }
    }
    if ((!d808) && u_is_con(s807, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s807, 0);
      u_value v_ps = u_field(s807, 1);
      u_value v_ins = u_field(s807, 2);
      u_value v_outs = u_field(s807, 3);
      u_value v_refs = u_field(s807, 4);
      u_value v_body = u_field(s807, 5);
      d808 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t810 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t810;
      }
    }
    if ((!d808) && u_is_con(s807, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s807, 0);
      d808 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t811 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t811;
      }
    }
    if ((!d808) && u_is_con(s807, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s807, 0);
      u_value v_body = u_field(s807, 1);
      d808 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t812 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t812;
      }
    }
    if ((!d808) && u_is_con(s807, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s807, 0);
      u_value v_body = u_field(s807, 1);
      d808 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t813 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t813;
      }
    }
    if (!d808) {
      d808 = 1;
      {
        u_value t814 = v_c;
        *r_c1 = t814;
      }
    }
    if (!d808) {
      u_no_case(s807);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s815 = v_cs;
    int d816 = 0;
    if ((!d816) && u_is_con(s815, &n_list_46_Nil)) {
      d816 = 1;
      {
        u_value t817 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t817;
      }
    }
    if ((!d816) && u_is_con(s815, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s815, 0), 0);
      u_value v_c = u_field(u_field(s815, 0), 1);
      u_value v_more = u_field(s815, 1);
      d816 = 1;
      {
        u_value t818 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t818;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t819 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t819;
        u_value s820 = f_check_46_unknown_in_cmd_1(v_c2);
        int d821 = 0;
        if ((!d821) && u_is_con(s820, &n_option_46_Some)) {
          u_value v_t = u_field(s820, 0);
          d821 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d821) && u_is_con(s820, &n_option_46_None)) {
          d821 = 1;
          {
          }
        }
        if (!d821) {
          u_no_case(s820);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t822 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t822;
      }
    }
    if (!d816) {
      u_no_case(s815);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s823 = v_cs;
  u_value t824 = NULL;
  if ((!t824) && u_is_con(s823, &n_list_46_Nil)) {
    t824 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t824) && (u_is_con(s823, &n_list_46_Cons) && u_is_con(u_field(u_field(s823, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s823, 0), 1), 0);
    u_value v_more = u_field(s823, 1);
    t824 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t824) && (u_is_con(s823, &n_list_46_Cons) && u_is_con(u_field(u_field(s823, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s823, 0), 1), 0);
    u_value v_more = u_field(s823, 1);
    t824 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t824) && u_is_con(s823, &n_list_46_Cons)) {
    u_value v_more = u_field(s823, 1);
    t824 = f_check_46_block_names_1(v_more);
  }
  if (!t824) {
    u_no_case(s823);
  }
  return t824;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s825 = v_p;
    int d826 = 0;
    if ((!d826) && u_is_con(s825, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s825, 0);
      d826 = 1;
      {
        u_value t827 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t827;
        u_value t828 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t828;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t829 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t829;
      }
    }
    if (!d826) {
      u_no_case(s825);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_830 = U_LIT("main");
    u_value t831 = f_check_46_start_2(&l_main_830, u_mk_con(&n_list_46_Nil, 0));
    v_s = t831;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t832 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t832;
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
  static struct u_object l_List_833 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_833), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_834 = U_LIT("list");
  static struct u_object l_List_835 = U_LIT("List");
  static struct u_object l_A_836 = U_LIT("A");
  static struct u_object l_Nil_837 = U_LIT("Nil");
  static struct u_object l_Cons_838 = U_LIT("Cons");
  static struct u_object l_A_839 = U_LIT("A");
  static struct u_object l_A_840 = U_LIT("A");
  static struct u_object l_list_841 = U_LIT("list");
  static struct u_object l_len_842 = U_LIT("len");
  static struct u_object l_A_843 = U_LIT("A");
  static struct u_object l_xs_844 = U_LIT("xs");
  static struct u_object l_A_845 = U_LIT("A");
  static struct u_object l_xs_846 = U_LIT("xs");
  static struct u_object l_Nil_847 = U_LIT("Nil");
  static struct u_object l_Cons_848 = U_LIT("Cons");
  static struct u_object l_ys_849 = U_LIT("ys");
  static struct u_object l__43__850 = U_LIT("+");
  static struct u_object l_len_851 = U_LIT("len");
  static struct u_object l_ys_852 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_834, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_835), u_mk_con(&n_list_46_Cons, 2, &l_A_836, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_837), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_838), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_839), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_840)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_841), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_842), u_mk_con(&n_list_46_Cons, 2, &l_A_843, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_844), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_845))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_846), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_847), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_848), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_849), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__850, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_851, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_852), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s853 = v_t;
  u_value t854 = NULL;
  if ((!t854) && u_is_con(s853, &n_c_46_CtVoid)) {
    static struct u_object l_void_855 = U_LIT("void");
    t854 = &l_void_855;
  }
  if ((!t854) && u_is_con(s853, &n_c_46_CtInt)) {
    static struct u_object l_int_856 = U_LIT("int");
    t854 = &l_int_856;
  }
  if ((!t854) && u_is_con(s853, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s853, 0);
    t854 = v_n;
  }
  if ((!t854) && (u_is_con(s853, &n_c_46_CtPtr) && u_is_con(u_field(s853, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s853, 0), 0);
    static struct u_object l__42__857 = U_LIT("*");
    t854 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__857);
  }
  if ((!t854) && u_is_con(s853, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s853, 0);
    static struct u_object l__32__42__858 = U_LIT(" *");
    t854 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__858);
  }
  if (!t854) {
    u_no_case(s853);
  }
  return t854;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s859 = v_t;
  u_value t860 = NULL;
  if ((!t860) && u_is_con(s859, &n_c_46_CtPtr)) {
    t860 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t860) {
    static struct u_object l__32__861 = U_LIT(" ");
    t860 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__861, v_x));
  }
  if (!t860) {
    u_no_case(s859);
  }
  return t860;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s862 = v_e;
  u_value t863 = NULL;
  if ((!t863) && u_is_con(s862, &n_c_46_CeVar)) {
    u_value v_x = u_field(s862, 0);
    t863 = v_x;
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeNum)) {
    u_value v_n = u_field(s862, 0);
    t863 = u_str_show(v_n);
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeStr)) {
    u_value v_s = u_field(s862, 0);
    t863 = u_str_quote(v_s);
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeCall)) {
    u_value v_f = u_field(s862, 0);
    u_value v_args = u_field(s862, 1);
    static struct u_object l__40__864 = U_LIT("(");
    static struct u_object l__44__32__865 = U_LIT(", ");
    u_value t866 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e867 = &t866;
    u_value l868 = v_args;
    while (u_is_con(l868, &n_list_46_Cons)) {
      u_value v_a = u_field(l868, 0);
      if (u_truth(u_mk_bool(1))) {
        e867 = u_append(e867, f_c_46_c_expr_1(v_a));
      }
      l868 = u_field(l868, 1);
    }
    static struct u_object l__41__869 = U_LIT(")");
    t863 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__864, u_concat(u_str_join(&l__44__32__865, t866), &l__41__869)));
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s862, 0);
    u_value v_a = u_field(s862, 1);
    t863 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s862, 0);
    u_value v_a = u_field(s862, 1);
    u_value v_b = u_field(s862, 2);
    static struct u_object l__32__870 = U_LIT(" ");
    static struct u_object l__32__871 = U_LIT(" ");
    t863 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__870, u_concat(v_o, u_concat(&l__32__871, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s862, 0);
    u_value v_i = u_field(s862, 1);
    static struct u_object l__91__872 = U_LIT("[");
    static struct u_object l__93__873 = U_LIT("]");
    t863 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__872, u_concat(f_c_46_c_expr_1(v_i), &l__93__873)));
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s862, 0);
    u_value v_f = u_field(s862, 1);
    static struct u_object l__45__62__874 = U_LIT("->");
    t863 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__874, v_f));
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeCast)) {
    u_value v_t = u_field(s862, 0);
    u_value v_a = u_field(s862, 1);
    static struct u_object l__40__875 = U_LIT("(");
    static struct u_object l__41__876 = U_LIT(")");
    t863 = u_concat(&l__40__875, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__876, f_c_46_c_operand_1(v_a))));
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeCond)) {
    u_value v_c = u_field(s862, 0);
    u_value v_a = u_field(s862, 1);
    u_value v_b = u_field(s862, 2);
    static struct u_object l__32__63__32__877 = U_LIT(" ? ");
    static struct u_object l__32__58__32__878 = U_LIT(" : ");
    t863 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__877, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__878, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t863) && u_is_con(s862, &n_c_46_CeInit)) {
    u_value v_es = u_field(s862, 0);
    static struct u_object l__123__879 = U_LIT("{");
    static struct u_object l__44__32__880 = U_LIT(", ");
    u_value t881 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e882 = &t881;
    u_value l883 = v_es;
    while (u_is_con(l883, &n_list_46_Cons)) {
      u_value v_a = u_field(l883, 0);
      if (u_truth(u_mk_bool(1))) {
        e882 = u_append(e882, f_c_46_c_expr_1(v_a));
      }
      l883 = u_field(l883, 1);
    }
    static struct u_object l__125__884 = U_LIT("}");
    t863 = u_concat(&l__123__879, u_concat(u_str_join(&l__44__32__880, t881), &l__125__884));
  }
  if (!t863) {
    u_no_case(s862);
  }
  return t863;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s885 = v_e;
  u_value t886 = NULL;
  if ((!t886) && u_is_con(s885, &n_c_46_CeVar)) {
    t886 = f_c_46_c_expr_1(v_e);
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeNum)) {
    u_value v_n = u_field(s885, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t886 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeStr)) {
    t886 = f_c_46_c_expr_1(v_e);
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeCall)) {
    t886 = f_c_46_c_expr_1(v_e);
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeIndex)) {
    t886 = f_c_46_c_expr_1(v_e);
  }
  if ((!t886) && u_is_con(s885, &n_c_46_CeArrow)) {
    t886 = f_c_46_c_expr_1(v_e);
  }
  if (!t886) {
    static struct u_object l__40__887 = U_LIT("(");
    static struct u_object l__41__888 = U_LIT(")");
    t886 = u_concat(&l__40__887, u_concat(f_c_46_c_expr_1(v_e), &l__41__888));
  }
  if (!t886) {
    u_no_case(s885);
  }
  return t886;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s889 = v_s;
  u_value t890 = NULL;
  if ((!t890) && u_is_con(s889, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s889, 0);
    static struct u_object l__59__891 = U_LIT(";");
    t890 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__891)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t890) && (u_is_con(s889, &n_c_46_CsDecl) && u_is_con(u_field(s889, 2), &n_option_46_None))) {
    u_value v_t = u_field(s889, 0);
    u_value v_x = u_field(s889, 1);
    static struct u_object l__59__892 = U_LIT(";");
    t890 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__892)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t890) && (u_is_con(s889, &n_c_46_CsDecl) && u_is_con(u_field(s889, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s889, 0);
    u_value v_x = u_field(s889, 1);
    u_value v_e = u_field(u_field(s889, 2), 0);
    static struct u_object l__32__61__32__893 = U_LIT(" = ");
    static struct u_object l__59__894 = U_LIT(";");
    t890 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__893, u_concat(f_c_46_c_expr_1(v_e), &l__59__894)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s889, 0);
    u_value v_e = u_field(s889, 1);
    static struct u_object l__32__61__32__895 = U_LIT(" = ");
    static struct u_object l__59__896 = U_LIT(";");
    t890 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__895, u_concat(f_c_46_c_expr_1(v_e), &l__59__896)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t890) && (u_is_con(s889, &n_c_46_CsIf) && u_is_con(u_field(s889, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s889, 0);
    u_value v_l = u_field(s889, 1);
    static struct u_object l_if_32__40__897 = U_LIT("if (");
    static struct u_object l__41__32__123__898 = U_LIT(") {");
    static struct u_object l__32__32__899 = U_LIT("  ");
    static struct u_object l__125__900 = U_LIT("}");
    t890 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__897, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__898))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__899), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__900), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CsIf)) {
    u_value v_c = u_field(s889, 0);
    u_value v_l = u_field(s889, 1);
    u_value v_r = u_field(s889, 2);
    static struct u_object l_if_32__40__901 = U_LIT("if (");
    static struct u_object l__41__32__123__902 = U_LIT(") {");
    static struct u_object l__32__32__903 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__904 = U_LIT("} else {");
    static struct u_object l__32__32__905 = U_LIT("  ");
    static struct u_object l__125__906 = U_LIT("}");
    t890 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__901, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__902))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__903), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__904), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__905), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__906), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s889, 0);
    u_value v_b = u_field(s889, 1);
    static struct u_object l_while_32__40__907 = U_LIT("while (");
    static struct u_object l__41__32__123__908 = U_LIT(") {");
    static struct u_object l__32__32__909 = U_LIT("  ");
    static struct u_object l__125__910 = U_LIT("}");
    t890 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__907, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__908))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__909), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__910), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t890) && (u_is_con(s889, &n_c_46_CsReturn) && u_is_con(u_field(s889, 0), &n_option_46_None))) {
    static struct u_object l_return_59__911 = U_LIT("return;");
    t890 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__911), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t890) && (u_is_con(s889, &n_c_46_CsReturn) && u_is_con(u_field(s889, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s889, 0), 0);
    static struct u_object l_return_32__912 = U_LIT("return ");
    static struct u_object l__59__913 = U_LIT(";");
    t890 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__912, u_concat(f_c_46_c_expr_1(v_e), &l__59__913))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t890) && u_is_con(s889, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s889, 0);
    static struct u_object l__123__914 = U_LIT("{");
    static struct u_object l__32__32__915 = U_LIT("  ");
    static struct u_object l__125__916 = U_LIT("}");
    t890 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__914), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__915), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__916), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t890) {
    u_no_case(s889);
  }
  return t890;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t917 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e918 = &t917;
  u_value l919 = v_ss;
  while (u_is_con(l919, &n_list_46_Cons)) {
    u_value v_s = u_field(l919, 0);
    if (u_truth(u_mk_bool(1))) {
      e918 = u_append(e918, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l919 = u_field(l919, 1);
  }
  return f_list_46_concat_1(t917);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s920 = v_ps;
  u_value t921 = NULL;
  if ((!t921) && u_is_con(s920, &n_list_46_Nil)) {
    static struct u_object l_void_922 = U_LIT("void");
    t921 = &l_void_922;
  }
  if (!t921) {
    static struct u_object l__44__32__923 = U_LIT(", ");
    u_value t924 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e925 = &t924;
    u_value l926 = v_ps;
    while (u_is_con(l926, &n_list_46_Cons)) {
      u_value v_p = u_field(l926, 0);
      if (u_truth(u_mk_bool(1))) {
        e925 = u_append(e925, f_c_46_c_param_1(v_p));
      }
      l926 = u_field(l926, 1);
    }
    t921 = u_str_join(&l__44__32__923, t924);
  }
  if (!t921) {
    u_no_case(s920);
  }
  return t921;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s927 = v_p;
  u_value t928 = NULL;
  if (!t928) {
    u_value v_t = u_field(s927, 0);
    u_value v_x = u_field(s927, 1);
    t928 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t928) {
    u_no_case(s927);
  }
  return t928;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s929 = v_d;
  u_value t930 = NULL;
  if ((!t930) && u_is_con(s929, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s929, 0);
    static struct u_object l__35_include_32__931 = U_LIT("#include ");
    t930 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__931, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t930) && u_is_con(s929, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s929, 0);
    u_value v_f = u_field(s929, 1);
    u_value v_ps = u_field(s929, 2);
    static struct u_object l__40__932 = U_LIT("(");
    static struct u_object l__41__59__933 = U_LIT(");");
    t930 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__932, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__933))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t930) && u_is_con(s929, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s929, 0);
    u_value v_f = u_field(s929, 1);
    u_value v_ps = u_field(s929, 2);
    u_value v_b = u_field(s929, 3);
    static struct u_object l__40__934 = U_LIT("(");
    static struct u_object l__41__32__123__935 = U_LIT(") {");
    static struct u_object l__32__32__936 = U_LIT("  ");
    static struct u_object l__125__937 = U_LIT("}");
    t930 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__934, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__935))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__936, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__937, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t930) && (u_is_con(s929, &n_c_46_CdGlobal) && u_is_con(u_field(s929, 2), &n_option_46_None))) {
    u_value v_t = u_field(s929, 0);
    u_value v_x = u_field(s929, 1);
    static struct u_object l__59__938 = U_LIT(";");
    t930 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__938), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t930) && (u_is_con(s929, &n_c_46_CdGlobal) && u_is_con(u_field(s929, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s929, 0);
    u_value v_x = u_field(s929, 1);
    u_value v_e = u_field(u_field(s929, 2), 0);
    static struct u_object l__32__61__32__939 = U_LIT(" = ");
    static struct u_object l__59__940 = U_LIT(";");
    t930 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__939, u_concat(f_c_46_c_expr_1(v_e), &l__59__940))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t930) {
    u_no_case(s929);
  }
  return t930;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s941 = v_f;
  u_value t942 = NULL;
  if ((!t942) && u_is_con(s941, &n_c_46_CFile)) {
    u_value v_ds = u_field(s941, 0);
    static struct u_object l__10__10__943 = U_LIT("\n\n");
    u_value t944 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e945 = &t944;
    u_value l946 = v_ds;
    while (u_is_con(l946, &n_list_46_Cons)) {
      u_value v_d = u_field(l946, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__947 = U_LIT("\n");
        e945 = u_append(e945, u_str_join(&l__10__947, f_c_46_c_decl_1(v_d)));
      }
      l946 = u_field(l946, 1);
    }
    static struct u_object l__10__948 = U_LIT("\n");
    t942 = u_concat(u_str_join(&l__10__10__943, t944), &l__10__948);
  }
  if (!t942) {
    u_no_case(s941);
  }
  return t942;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__949 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__949, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__46__950 = U_LIT("()[],;.");
  return u_str_contains(&l__40__41__91__93__44__59__46__950, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t951 = u_is_letter(v_c);
  if (!u_truth(t951)) {
    t951 = u_is_digit(v_c);
  }
  u_value t952 = t951;
  if (!u_truth(t952)) {
    t952 = u_eq(v_c, u_mk_char(95));
  }
  return t952;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t953;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t953 = u_str_get(v_s, v_i);
  } else {
    t953 = u_chr(u_mk_int(0));
  }
  return t953;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s954 = v_c;
  u_value t955 = NULL;
  if ((!t955) && u_equals(s954, u_mk_char(34))) {
    t955 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t955) && u_equals(s954, u_mk_char(39))) {
    t955 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t955) && u_equals(s954, u_mk_char(92))) {
    t955 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t955) && u_equals(s954, u_mk_char(110))) {
    t955 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t955) && u_equals(s954, u_mk_char(116))) {
    t955 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t955) && u_equals(s954, u_mk_char(114))) {
    t955 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t955) {
    t955 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t955) {
    u_no_case(s954);
  }
  return t955;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t956;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t956 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t956 = v_i;
  }
  return t956;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t957 = u_mk_int(0);
    u_value t958 = u_mk_int(1);
    u_value t959 = u_mk_int(1);
    v_j = t957;
    *r_line = t958;
    *r_column = t959;
    u_value t964 = u_lt(v_j, v_i);
    while (u_truth(t964)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t960 = u_add(*r_line, u_mk_int(1));
            u_value t961 = u_mk_int(1);
            *r_line = t960;
            *r_column = t961;
          }
        } else {
          {
            u_value t962 = u_add(*r_column, u_mk_int(1));
            *r_column = t962;
          }
        }
        u_value t963 = u_add(v_j, u_mk_int(1));
        v_j = t963;
      }
      t964 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t965 = *r_i;
    u_value t966 = u_add(*r_i, u_mk_int(1));
    u_value t967 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t965;
    *r_i = t966;
    v_parts = t967;
    u_value t968 = *r_i;
    v_from = t968;
    u_value t979 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t979)) {
      {
        u_value t969 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t969;
        u_value t970 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t970)) {
          t970 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t970)) {
          {
            static struct u_object l_unterminated_32_lit_971 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_971, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s972 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d973 = 0;
            if ((!d973) && u_is_con(s972, &n_option_46_Some)) {
              u_value v_d = u_field(s972, 0);
              d973 = 1;
              {
                u_value t974 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t975 = u_add(*r_i, u_mk_int(2));
                v_parts = t974;
                *r_i = t975;
                u_value t976 = *r_i;
                v_from = t976;
              }
            }
            if ((!d973) && u_is_con(s972, &n_option_46_None)) {
              d973 = 1;
              {
                static struct u_object l_invalid_32_escape_977 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_977, *r_i), "lex.LexError");
              }
            }
            if (!d973) {
              u_no_case(s972);
            }
          }
        } else {
          {
            u_value t978 = u_add(*r_i, u_mk_int(1));
            *r_i = t978;
          }
        }
      }
      t979 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__980 = U_LIT("");
    u_value t981 = u_str_join(&l__980, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t982 = u_add(*r_i, u_mk_int(1));
    *r_text = t981;
    *r_i = t982;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t983 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t983;
    u_value t984 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t985 = v_j;
    *r_t = t984;
    *r_i = t985;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t986 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t986;
    u_value s987 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d988 = 0;
    if ((!d988) && u_is_con(s987, &n_option_46_Some)) {
      u_value v_n = u_field(s987, 0);
      d988 = 1;
      {
        u_value t989 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t990 = v_j;
        *r_t = t989;
        *r_i = t990;
      }
    }
    if ((!d988) && u_is_con(s987, &n_option_46_None)) {
      d988 = 1;
      {
        static struct u_object l_invalid_32_number_991 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_991, *r_i), "lex.LexError");
      }
    }
    if (!d988) {
      u_no_case(s987);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t992 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t992;
    u_value t993 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t994 = v_j;
    *r_t = t993;
    *r_i = t994;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t995 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t995;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t996 = *r_i;
    v_start = t996;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_997 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_997, v_start), "lex.LexError");
      }
    }
    u_value t998 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t998;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t999 = u_str_get(v_s, *r_i);
    v_c = t999;
    u_value s1000 = v_c;
    int d1001 = 0;
    if (!d1001) {
      u_value t1002 = u_is_letter(v_c);
      if (!u_truth(t1002)) {
        t1002 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t1002)) {
        d1001 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d1001) {
      if (u_truth(u_is_digit(v_c))) {
        d1001 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d1001) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d1001 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d1001) && u_equals(s1000, u_mk_char(34))) {
      d1001 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d1001) && u_equals(s1000, u_mk_char(39))) {
      d1001 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d1001) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d1001 = 1;
        {
          u_value t1003 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t1004 = u_add(*r_i, u_mk_int(1));
          *r_t = t1003;
          *r_i = t1004;
        }
      }
    }
    if (!d1001) {
      d1001 = 1;
      {
        static struct u_object l_unexpected_32_chara_1005 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_1005, *r_i), "lex.LexError");
      }
    }
    if (!d1001) {
      u_no_case(s1000);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t1006 = u_mk_int(0);
    v_i = t1006;
    u_value t1007 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1007;
    u_value t1014 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1014)) {
      {
        u_value t1008 = v_i;
        v_start = t1008;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t1009 = u_add(v_i, u_mk_int(1));
            v_i = t1009;
          }
        } else {
          u_value t1010 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t1010)) {
            t1010 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t1010)) {
            {
              u_value t1012 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1011, 0));
              v_i = t1012;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1013 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1013;
            }
          }
        }
      }
      t1014 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1015 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1015;
  }
}

u_value f_lex_46_tokens_1(u_value v_ts) {
  return f_list_46_firsts_1(v_ts);
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1016 = v_x;
  u_value t1017 = NULL;
  if ((!t1017) && u_is_con(s1016, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1016, 0);
    t1017 = v_y;
  }
  if (!t1017) {
    u_no_case(s1016);
  }
  return t1017;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1018 = v_x;
  u_value t1019 = NULL;
  if ((!t1019) && u_is_con(s1018, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1018, 1);
    t1019 = v_y;
  }
  if (!t1019) {
    u_no_case(s1018);
  }
  return t1019;
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
  static struct u_object l___1020 = U_LIT("_");
  static struct u_object l_assert_1021 = U_LIT("assert");
  static struct u_object l_assign_1022 = U_LIT("assign");
  static struct u_object l_call_1023 = U_LIT("call");
  static struct u_object l_case_1024 = U_LIT("case");
  static struct u_object l_catch_1025 = U_LIT("catch");
  static struct u_object l_data_1026 = U_LIT("data");
  static struct u_object l_else_1027 = U_LIT("else");
  static struct u_object l_end_1028 = U_LIT("end");
  static struct u_object l_eval_1029 = U_LIT("eval");
  static struct u_object l_false_1030 = U_LIT("false");
  static struct u_object l_for_1031 = U_LIT("for");
  static struct u_object l_function_1032 = U_LIT("function");
  static struct u_object l_if_1033 = U_LIT("if");
  static struct u_object l_import_1034 = U_LIT("import");
  static struct u_object l_in_1035 = U_LIT("in");
  static struct u_object l_infix_1036 = U_LIT("infix");
  static struct u_object l_lambda_1037 = U_LIT("lambda");
  static struct u_object l_let_1038 = U_LIT("let");
  static struct u_object l_local_1039 = U_LIT("local");
  static struct u_object l_match_1040 = U_LIT("match");
  static struct u_object l_out_1041 = U_LIT("out");
  static struct u_object l_postfix_1042 = U_LIT("postfix");
  static struct u_object l_prefix_1043 = U_LIT("prefix");
  static struct u_object l_procedure_1044 = U_LIT("procedure");
  static struct u_object l_raise_1045 = U_LIT("raise");
  static struct u_object l_ref_1046 = U_LIT("ref");
  static struct u_object l_run_1047 = U_LIT("run");
  static struct u_object l_skip_1048 = U_LIT("skip");
  static struct u_object l_test_1049 = U_LIT("test");
  static struct u_object l_then_1050 = U_LIT("then");
  static struct u_object l_true_1051 = U_LIT("true");
  static struct u_object l_type_1052 = U_LIT("type");
  static struct u_object l_try_1053 = U_LIT("try");
  static struct u_object l_while_1054 = U_LIT("while");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, &l___1020, u_mk_con(&n_list_46_Cons, 2, &l_assert_1021, u_mk_con(&n_list_46_Cons, 2, &l_assign_1022, u_mk_con(&n_list_46_Cons, 2, &l_call_1023, u_mk_con(&n_list_46_Cons, 2, &l_case_1024, u_mk_con(&n_list_46_Cons, 2, &l_catch_1025, u_mk_con(&n_list_46_Cons, 2, &l_data_1026, u_mk_con(&n_list_46_Cons, 2, &l_else_1027, u_mk_con(&n_list_46_Cons, 2, &l_end_1028, u_mk_con(&n_list_46_Cons, 2, &l_eval_1029, u_mk_con(&n_list_46_Cons, 2, &l_false_1030, u_mk_con(&n_list_46_Cons, 2, &l_for_1031, u_mk_con(&n_list_46_Cons, 2, &l_function_1032, u_mk_con(&n_list_46_Cons, 2, &l_if_1033, u_mk_con(&n_list_46_Cons, 2, &l_import_1034, u_mk_con(&n_list_46_Cons, 2, &l_in_1035, u_mk_con(&n_list_46_Cons, 2, &l_infix_1036, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1037, u_mk_con(&n_list_46_Cons, 2, &l_let_1038, u_mk_con(&n_list_46_Cons, 2, &l_local_1039, u_mk_con(&n_list_46_Cons, 2, &l_match_1040, u_mk_con(&n_list_46_Cons, 2, &l_out_1041, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1042, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1043, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1044, u_mk_con(&n_list_46_Cons, 2, &l_raise_1045, u_mk_con(&n_list_46_Cons, 2, &l_ref_1046, u_mk_con(&n_list_46_Cons, 2, &l_run_1047, u_mk_con(&n_list_46_Cons, 2, &l_skip_1048, u_mk_con(&n_list_46_Cons, 2, &l_test_1049, u_mk_con(&n_list_46_Cons, 2, &l_then_1050, u_mk_con(&n_list_46_Cons, 2, &l_true_1051, u_mk_con(&n_list_46_Cons, 2, &l_type_1052, u_mk_con(&n_list_46_Cons, 2, &l_try_1053, u_mk_con(&n_list_46_Cons, 2, &l_while_1054, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1055 = U_LIT("=");
  static struct u_object l__61__62__1056 = U_LIT("=>");
  static struct u_object l__58__61__1057 = U_LIT(":=");
  static struct u_object l__58__1058 = U_LIT(":");
  static struct u_object l__124__1059 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1055, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1056, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1057, u_mk_con(&n_list_46_Cons, 2, &l__58__1058, u_mk_con(&n_list_46_Cons, 2, &l__124__1059, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1060 = v_ns;
  u_value t1061 = NULL;
  if ((!t1061) && u_is_con(s1060, &n_list_46_Nil)) {
    t1061 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1061) && (u_is_con(s1060, &n_list_46_Cons) && (u_is_con(u_field(s1060, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1060, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1060, 0), 1);
    u_value v_ops = u_field(u_field(s1060, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1061 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1061) && u_is_con(s1060, &n_list_46_Cons)) {
    u_value v_more = u_field(s1060, 1);
    t1061 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1061) {
    u_no_case(s1060);
  }
  return t1061;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1062 = v_ns;
  u_value t1063 = NULL;
  if ((!t1063) && u_is_con(s1062, &n_list_46_Nil)) {
    t1063 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1063) && (u_is_con(s1062, &n_list_46_Cons) && (u_is_con(u_field(s1062, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1062, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1062, 0), 1);
    u_value v_ops = u_field(u_field(s1062, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1063 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1063) && u_is_con(s1062, &n_list_46_Cons)) {
    u_value v_more = u_field(s1062, 1);
    t1063 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1063) {
    u_no_case(s1062);
  }
  return t1063;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1064 = v_ns;
  u_value t1065 = NULL;
  if ((!t1065) && u_is_con(s1064, &n_list_46_Nil)) {
    t1065 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1065) && (u_is_con(s1064, &n_list_46_Cons) && u_is_con(u_field(s1064, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1064, 0), 0);
    u_value v_p = u_field(u_field(s1064, 0), 1);
    u_value v_ops = u_field(u_field(s1064, 0), 2);
    u_value t1066 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1066)) {
      t1066 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1067 = t1066;
    if (u_truth(t1067)) {
      t1067 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1067)) {
      t1065 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1065) && u_is_con(s1064, &n_list_46_Cons)) {
    u_value v_more = u_field(s1064, 1);
    t1065 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1065) {
    u_no_case(s1064);
  }
  return t1065;
}

u_value f_parse_46_declared_2(u_value v_ns, u_value v_op) {
  u_value s1068 = v_ns;
  u_value t1069 = NULL;
  if ((!t1069) && u_is_con(s1068, &n_list_46_Nil)) {
    t1069 = u_mk_bool(0);
  }
  if ((!t1069) && (u_is_con(s1068, &n_list_46_Cons) && u_is_con(u_field(s1068, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1068, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1069 = u_mk_bool(1);
    }
  }
  if ((!t1069) && u_is_con(s1068, &n_list_46_Cons)) {
    u_value v_more = u_field(s1068, 1);
    t1069 = f_parse_46_declared_2(v_more, v_op);
  }
  if (!t1069) {
    u_no_case(s1068);
  }
  return t1069;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1070 = v_ts;
  u_value t1071 = NULL;
  if ((!t1071) && u_is_con(s1070, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1070, 0), 0);
    t1071 = v_t;
  }
  if (!t1071) {
    t1071 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1071) {
    u_no_case(s1070);
  }
  return t1071;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1072 = v_ts;
  u_value t1073 = NULL;
  if ((!t1073) && (u_is_con(s1072, &n_list_46_Cons) && u_is_con(u_field(s1072, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1072, 1), 0), 0);
    t1073 = v_t;
  }
  if (!t1073) {
    t1073 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1073) {
    u_no_case(s1072);
  }
  return t1073;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1074 = v_ts;
  u_value t1075 = NULL;
  if ((!t1075) && u_is_con(s1074, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1074, 0), 1);
    t1075 = v_i;
  }
  if (!t1075) {
    t1075 = u_mk_int(0);
  }
  if (!t1075) {
    u_no_case(s1074);
  }
  return t1075;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1076 = v_t;
  u_value t1077 = NULL;
  if ((!t1077) && u_is_con(s1076, &n_lex_46_TName)) {
    u_value v_n = u_field(s1076, 0);
    t1077 = v_n;
  }
  if ((!t1077) && u_is_con(s1076, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1076, 0);
    t1077 = v_s;
  }
  if ((!t1077) && u_is_con(s1076, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1076, 0);
    t1077 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1077) && u_is_con(s1076, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1076, 0);
    t1077 = u_str_show(v_n);
  }
  if ((!t1077) && u_is_con(s1076, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1076, 0);
    t1077 = u_str_quote(v_s);
  }
  if ((!t1077) && u_is_con(s1076, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1076, 0);
    static struct u_object l__39__1078 = U_LIT("'");
    static struct u_object l__39__1079 = U_LIT("'");
    t1077 = u_concat(&l__39__1078, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1079));
  }
  if ((!t1077) && u_is_con(s1076, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1080 = U_LIT("end of input");
    t1077 = &l_end_32_of_32_input_1080;
  }
  if (!t1077) {
    u_no_case(s1076);
  }
  return t1077;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1081 = f_parse_46_head_token_1(v_ts);
  u_value t1082 = NULL;
  if ((!t1082) && u_is_con(s1081, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1081, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1082 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1082) && u_is_con(s1081, &n_lex_46_TName)) {
    u_value v_n = u_field(s1081, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1082 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1082) {
    t1082 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1082) {
    u_no_case(s1081);
  }
  return t1082;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1083 = *r_ts;
    int d1084 = 0;
    if ((!d1084) && u_is_con(s1083, &n_list_46_Cons)) {
      u_value v_more = u_field(s1083, 1);
      d1084 = 1;
      {
        u_value t1085 = v_more;
        *r_ts = t1085;
      }
    }
    if (!d1084) {
      d1084 = 1;
      {
      }
    }
    if (!d1084) {
      u_no_case(s1083);
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
        static struct u_object l__32_expected_32_but_32__1086 = U_LIT(" expected but ");
        static struct u_object l__32_found_1087 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1086, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1087))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1088 = f_parse_46_head_token_1(*r_ts);
    int d1089 = 0;
    if ((!d1089) && u_is_con(s1088, &n_lex_46_TName)) {
      u_value v_m = u_field(s1088, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1089 = 1;
        {
          u_value t1090 = v_m;
          *r_n = t1090;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1089) {
      d1089 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1091 = U_LIT("name expected but ");
        static struct u_object l__32_found_1092 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1091, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1092)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1089) {
      u_no_case(s1088);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1093 = f_parse_46_head_token_1(*r_ts);
    int d1094 = 0;
    if ((!d1094) && u_is_con(s1093, &n_lex_46_TName)) {
      u_value v_m = u_field(s1093, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1094 = 1;
        {
          static struct u_object l_operator_32__1095 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1096 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1095, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1096)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1094) {
      d1094 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1094) {
      u_no_case(s1093);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1097 = f_parse_46_head_token_1(*r_ts);
    int d1098 = 0;
    if ((!d1098) && u_is_con(s1097, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1097, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1098 = 1;
        {
          u_value t1099 = v_s;
          *r_n = t1099;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1098) {
      d1098 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1098) {
      u_no_case(s1097);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1100 = v_x;
  u_value t1101 = NULL;
  if ((!t1101) && u_is_con(s1100, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1100, 0);
    t1101 = v_y;
  }
  if (!t1101) {
    u_no_case(s1100);
  }
  return t1101;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1102 = v_x;
  u_value t1103 = NULL;
  if ((!t1103) && u_is_con(s1102, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1102, 1);
    t1103 = v_y;
  }
  if (!t1103) {
    u_no_case(s1102);
  }
  return t1103;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1104 = v_x;
  u_value t1105 = NULL;
  if ((!t1105) && u_is_con(s1104, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1104, 2);
    t1105 = v_y;
  }
  if (!t1105) {
    u_no_case(s1104);
  }
  return t1105;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1106 = v_x;
  u_value t1107 = NULL;
  if ((!t1107) && u_is_con(s1106, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1106, 3);
    t1107 = v_y;
  }
  if (!t1107) {
    u_no_case(s1106);
  }
  return t1107;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1108 = v_rest;
    int d1109 = 0;
    if ((!d1109) && u_is_con(s1108, &n_list_46_Nil)) {
      d1109 = 1;
      {
      }
    }
    if ((!d1109) && (u_is_con(s1108, &n_list_46_Cons) && u_is_con(u_field(s1108, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1108, 0), 0);
      u_value v_i = u_field(u_field(s1108, 0), 1);
      d1109 = 1;
      {
        static struct u_object l_unexpected_32_opera_1110 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1110, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1109) {
      d1109 = 1;
      {
        static struct u_object l_missing_32_operator_1111 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1111, v_at), "parse.ParseError");
      }
    }
    if (!d1109) {
      u_no_case(s1108);
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
    u_value s1112 = v_items;
    int d1113 = 0;
    if ((!d1113) && (u_is_con(s1112, &n_list_46_Cons) && u_is_con(u_field(s1112, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1112, 0), 0);
      u_value v_more = u_field(s1112, 1);
      d1113 = 1;
      {
        u_value t1114 = v_x;
        u_value t1115 = v_more;
        *r_a = t1114;
        *r_rest = t1115;
      }
    }
    if ((!d1113) && (u_is_con(s1112, &n_list_46_Cons) && u_is_con(u_field(s1112, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1112, 0), 0);
      u_value v_i = u_field(u_field(s1112, 0), 1);
      u_value v_more = u_field(s1112, 1);
      d1113 = 1;
      {
        u_value s1116 = f_parse_46_prefix_prec_2(f_parse_46_Ops_46_notations_1(v_o), v_op);
        int d1117 = 0;
        if ((!d1117) && u_is_con(s1116, &n_option_46_Some)) {
          u_value v_p = u_field(s1116, 0);
          d1117 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1118 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1118;
          }
        }
        if ((!d1117) && u_is_con(s1116, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1117 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1119 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1119), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1117) && u_is_con(s1116, &n_option_46_None)) {
          d1117 = 1;
          {
            static struct u_object l_undeclared_32_opera_1120 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1120, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1117) {
          u_no_case(s1116);
        }
      }
    }
    if ((!d1113) && u_is_con(s1112, &n_list_46_Nil)) {
      d1113 = 1;
      {
        static struct u_object l_missing_32_operand_1121 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1121, v_at), "parse.ParseError");
      }
    }
    if (!d1113) {
      u_no_case(s1112);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1122 = v_items;
    int d1123 = 0;
    if ((!d1123) && (u_is_con(s1122, &n_list_46_Cons) && u_is_con(u_field(s1122, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1122, 0), 0);
      u_value v_i = u_field(u_field(s1122, 0), 1);
      u_value v_tail = u_field(s1122, 1);
      d1123 = 1;
      {
        u_value s1124 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Ops_46_notations_1(v_o), v_op), f_parse_46_infix_of_2(f_parse_46_Ops_46_notations_1(v_o), v_op));
        int d1125 = 0;
        if ((!d1125) && u_is_con(u_field(s1124, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1124, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1125 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1125) && u_is_con(u_field(s1124, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1124, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1124, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1125 = 1;
            {
              u_value t1126;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1126 = v_p;
              } else {
                t1126 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1126, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Ops_46_notations_1(v_o));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1125) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1125 = 1;
            {
              static struct u_object l_undeclared_32_opera_1127 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1127, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1125) {
          d1125 = 1;
          {
            u_value t1128 = v_lhs;
            u_value t1129 = v_items;
            *r_a = t1128;
            *r_rest = t1129;
          }
        }
        if (!d1125) {
          u_no_case(s1124);
        }
      }
    }
    if (!d1123) {
      d1123 = 1;
      {
        u_value t1130 = v_lhs;
        u_value t1131 = v_items;
        *r_a = t1130;
        *r_rest = t1131;
      }
    }
    if (!d1123) {
      u_no_case(s1122);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1132 = v_rest;
    int d1133 = 0;
    if ((!d1133) && (u_is_con(s1132, &n_list_46_Cons) && u_is_con(u_field(s1132, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1132, 0), 0);
      u_value v_i = u_field(u_field(s1132, 0), 1);
      u_value t1136 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1136)) {
        t1136 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1136)) {
        d1133 = 1;
        {
          static struct u_object l_operator_32__1134 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1135 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1134, u_concat(v_op, &l__32_is_32_not_32_associat_1135)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1133) {
      d1133 = 1;
      {
      }
    }
    if (!d1133) {
      u_no_case(s1132);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1137 = v_fp;
  u_value t1138 = NULL;
  if ((!t1138) && u_is_con(s1137, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1137, 0), 1);
    t1138 = u_eq(v_q, v_p);
  }
  if (!t1138) {
    t1138 = u_mk_bool(0);
  }
  if (!t1138) {
    u_no_case(s1137);
  }
  return t1138;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1139 = v_ts;
  u_value t1140 = NULL;
  if ((!t1140) && (u_is_con(s1139, &n_list_46_Cons) && (u_is_con(u_field(s1139, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1139, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1139, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1139, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1139, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1139, 0), 1);
    t1140 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1140) && (u_is_con(s1139, &n_list_46_Cons) && u_is_con(u_field(s1139, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1139, 0);
    t1140 = v_t;
  }
  if ((!t1140) && u_is_con(s1139, &n_list_46_Nil)) {
    t1140 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1140) {
    static struct u_object l__40_invalid_41__1141 = U_LIT("(invalid)");
    t1140 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1141), v_ts);
  }
  if (!t1140) {
    u_no_case(s1139);
  }
  return t1140;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1142 = v_ts;
  u_value t1143 = NULL;
  if ((!t1143) && (u_is_con(s1142, &n_list_46_Cons) && (u_is_con(u_field(s1142, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1142, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1142, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1142, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1142, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1142, 0), 1);
    t1143 = v_cs;
  }
  if (!t1143) {
    t1143 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1143) {
    u_no_case(s1142);
  }
  return t1143;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1144 = U_LIT("->");
  u_value t1151;
  if (u_truth(u_eq(v_op, &l__45__62__1144))) {
    u_value t1145 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1146 = &t1145;
    u_value l1147 = v_l;
    while (u_is_con(l1147, &n_list_46_Cons)) {
      u_value v_t = u_field(l1147, 0);
      if (u_truth(u_mk_bool(1))) {
        e1146 = u_append(e1146, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1147 = u_field(l1147, 1);
    }
    t1151 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1145, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1148 = U_LIT("*");
    u_value t1150;
    if (u_truth(u_eq(v_op, &l__42__1148))) {
      static struct u_object l__40_product_41__1149 = U_LIT("(product)");
      t1150 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1149), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1150 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1151 = t1150;
  }
  return t1151;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1152 = v_t;
  u_value t1153 = NULL;
  if ((!t1153) && (u_is_con(s1152, &n_syntax_46_TSort) && (u_is_con(u_field(s1152, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1152, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1152, 0), 1), u_mk_lit("(invalid)")))))) {
    t1153 = u_mk_bool(1);
  }
  if ((!t1153) && u_is_con(s1152, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1152, 1);
    t1153 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1153) && u_is_con(s1152, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1152, 0);
    u_value v_r = u_field(s1152, 1);
    u_value t1154 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1154)) {
      t1154 = f_parse_46_has_invalid_1(v_r);
    }
    t1153 = t1154;
  }
  if ((!t1153) && u_is_con(s1152, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1152, 0);
    t1153 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1153) {
    t1153 = u_mk_bool(0);
  }
  if (!t1153) {
    u_no_case(s1152);
  }
  return t1153;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1155 = v_ts;
  u_value t1156 = NULL;
  if ((!t1156) && u_is_con(s1155, &n_list_46_Nil)) {
    t1156 = u_mk_bool(0);
  }
  if ((!t1156) && u_is_con(s1155, &n_list_46_Cons)) {
    u_value v_t = u_field(s1155, 0);
    u_value v_more = u_field(s1155, 1);
    u_value t1157 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1157)) {
      t1157 = f_parse_46_any_invalid_1(v_more);
    }
    t1156 = t1157;
  }
  if (!t1156) {
    u_no_case(s1155);
  }
  return t1156;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1158 = v_t;
  u_value t1159 = NULL;
  if ((!t1159) && u_is_con(s1158, &n_lex_46_TName)) {
    u_value v_n = u_field(s1158, 0);
    u_value t1160 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1160)) {
      t1160 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1159 = t1160;
  }
  if ((!t1159) && (u_is_con(s1158, &n_lex_46_TPunct) && u_equals(u_field(s1158, 0), u_mk_char(40)))) {
    t1159 = u_mk_bool(1);
  }
  if (!t1159) {
    t1159 = u_mk_bool(0);
  }
  if (!t1159) {
    u_no_case(s1158);
  }
  return t1159;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1161 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1161;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1162 = f_parse_46_type_one_1(v_r);
    *r_t = t1162;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1163 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1163, v_start), "parse.ParseError");
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
    u_value t1164 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1164;
    u_value t1165 = u_mk_bool(1);
    v_next = t1165;
    u_value t1166 = u_mk_bool(1);
    v_go = t1166;
    u_value t1175 = v_go;
    while (u_truth(t1175)) {
      {
        u_value s1167 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1168 = 0;
        if ((!d1168) && u_is_con(s1167, &n_option_46_Some)) {
          u_value v_op = u_field(s1167, 0);
          d1168 = 1;
          {
            u_value t1169 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1170 = u_mk_bool(1);
            v_rev = t1169;
            v_next = t1170;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1168) && u_is_con(s1167, &n_option_46_None)) {
          u_value t1173 = v_next;
          if (u_truth(t1173)) {
            t1173 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1173)) {
            d1168 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1171 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1172 = u_mk_bool(0);
              v_rev = t1171;
              v_next = t1172;
            }
          }
        }
        if (!d1168) {
          d1168 = 1;
          {
            u_value t1174 = u_mk_bool(0);
            v_go = t1174;
          }
        }
        if (!d1168) {
          u_no_case(s1167);
        }
      }
      t1175 = v_go;
    }
    u_value s1176 = v_rev;
    int d1177 = 0;
    if ((!d1177) && u_is_con(s1176, &n_list_46_Nil)) {
      d1177 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1178 = U_LIT("type expected but ");
        static struct u_object l__32_found_1179 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1178, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1179)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1177) {
      d1177 = 1;
      {
        u_value t1180 = f_list_46_reverse_1(v_rev);
        *r_items = t1180;
      }
    }
    if (!d1177) {
      u_no_case(s1176);
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
            u_value t1181 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1181;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1182 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1182;
            }
          } else {
            {
              u_value t1183 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1183;
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
    u_value t1184 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1184;
    u_value t1185 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1185)) {
      t1185 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1185)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1186 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1186;
        u_value t1188 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1188)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1187 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1187;
          }
          t1188 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1189 = f_list_46_reverse_1(v_rev);
    *r_r = t1189;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1190 = v_es;
  u_value t1191 = NULL;
  if ((!t1191) && u_is_con(s1190, &n_list_46_Nil)) {
    t1191 = v_rest;
  }
  if ((!t1191) && u_is_con(s1190, &n_list_46_Cons)) {
    u_value v_e = u_field(s1190, 0);
    u_value v_more = u_field(s1190, 1);
    static struct u_object l_Cons_1192 = U_LIT("Cons");
    t1191 = f_parse_46_expr_op2_3(&l_Cons_1192, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1191) {
    u_no_case(s1190);
  }
  return t1191;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1193 = v_t;
  u_value t1194 = NULL;
  if ((!t1194) && u_is_con(s1193, &n_lex_46_TName)) {
    u_value v_n = u_field(s1193, 0);
    static struct u_object l_let_1195 = U_LIT("let");
    static struct u_object l_if_1196 = U_LIT("if");
    static struct u_object l_lambda_1197 = U_LIT("lambda");
    static struct u_object l_match_1198 = U_LIT("match");
    static struct u_object l_true_1199 = U_LIT("true");
    static struct u_object l_false_1200 = U_LIT("false");
    u_value t1202 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1195, u_mk_con(&n_list_46_Cons, 2, &l_if_1196, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1197, u_mk_con(&n_list_46_Cons, 2, &l_match_1198, u_mk_con(&n_list_46_Cons, 2, &l_true_1199, u_mk_con(&n_list_46_Cons, 2, &l_false_1200, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1202)) {
      u_value t1201 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1201)) {
        t1201 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1202 = t1201;
    }
    t1194 = t1202;
  }
  if ((!t1194) && u_is_con(s1193, &n_lex_46_TNum)) {
    t1194 = u_mk_bool(1);
  }
  if ((!t1194) && u_is_con(s1193, &n_lex_46_TStr)) {
    t1194 = u_mk_bool(1);
  }
  if ((!t1194) && u_is_con(s1193, &n_lex_46_TChar)) {
    t1194 = u_mk_bool(1);
  }
  if ((!t1194) && u_is_con(s1193, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1193, 0);
    u_value t1203 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1203)) {
      t1203 = u_eq(v_c, u_mk_char(91));
    }
    t1194 = t1203;
  }
  if (!t1194) {
    t1194 = u_mk_bool(0);
  }
  if (!t1194) {
    u_no_case(s1193);
  }
  return t1194;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1204 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1204))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1205 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1205;
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
    u_value t1206 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1206;
    u_value t1207 = u_mk_bool(1);
    v_next = t1207;
    u_value t1208 = u_mk_bool(1);
    v_go = t1208;
    u_value t1217 = v_go;
    while (u_truth(t1217)) {
      {
        u_value s1209 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1210 = 0;
        if ((!d1210) && u_is_con(s1209, &n_option_46_Some)) {
          u_value v_op = u_field(s1209, 0);
          d1210 = 1;
          {
            u_value t1211 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1212 = u_mk_bool(1);
            v_rev = t1211;
            v_next = t1212;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1210) && u_is_con(s1209, &n_option_46_None)) {
          u_value t1215 = v_next;
          if (u_truth(t1215)) {
            t1215 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1215)) {
            d1210 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1213 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1214 = u_mk_bool(0);
              v_rev = t1213;
              v_next = t1214;
            }
          }
        }
        if (!d1210) {
          d1210 = 1;
          {
            u_value t1216 = u_mk_bool(0);
            v_go = t1216;
          }
        }
        if (!d1210) {
          u_no_case(s1209);
        }
      }
      t1217 = v_go;
    }
    u_value s1218 = v_rev;
    int d1219 = 0;
    if ((!d1219) && u_is_con(s1218, &n_list_46_Nil)) {
      d1219 = 1;
      {
        static struct u_object l_expression_32_expec_1220 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1221 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1220, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1221)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1219) {
      d1219 = 1;
      {
        u_value t1222 = f_list_46_reverse_1(v_rev);
        *r_items = t1222;
      }
    }
    if (!d1219) {
      u_no_case(s1218);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1223 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1223;
    u_value t1225 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1225)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1224 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1224;
      }
      t1225 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1226 = f_list_46_reverse_1(v_rev);
    *r_es = t1226;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1227 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1227;
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
    u_value t1228 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1228)) {
      t1228 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1232 = t1228;
    while (u_truth(t1232)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1229 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1229;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1230 = U_LIT(".");
            u_value t1231 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1230, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1231;
          }
        }
      }
      u_value t1233 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1233)) {
        t1233 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1232 = t1233;
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
    u_value s1234 = f_parse_46_head_token_1(*r_ts);
    int d1235 = 0;
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TName) && u_equals(u_field(s1234, 0), u_mk_lit("let")))) {
      d1235 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TName) && u_equals(u_field(s1234, 0), u_mk_lit("if")))) {
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1236 = U_LIT("then");
        p_parse_46_keyword(&l_then_1236, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1237 = U_LIT("else");
        p_parse_46_keyword(&l_else_1237, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1238 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1238;
      }
    }
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TName) && u_equals(u_field(s1234, 0), u_mk_lit("lambda")))) {
      d1235 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TName) && u_equals(u_field(s1234, 0), u_mk_lit("match")))) {
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1239 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1239;
      }
    }
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TName) && u_equals(u_field(s1234, 0), u_mk_lit("true")))) {
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1240 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1240;
      }
    }
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TName) && u_equals(u_field(s1234, 0), u_mk_lit("false")))) {
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1241 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1241;
      }
    }
    if ((!d1235) && u_is_con(s1234, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1234, 0);
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1242 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1242;
      }
    }
    if ((!d1235) && u_is_con(s1234, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1234, 0);
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1243 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1243;
      }
    }
    if ((!d1235) && u_is_con(s1234, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1234, 0);
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1244 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1244;
      }
    }
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TPunct) && u_equals(u_field(s1234, 0), u_mk_char(40)))) {
      d1235 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1245 = v_es;
        int d1246 = 0;
        if ((!d1246) && (u_is_con(s1245, &n_list_46_Cons) && u_is_con(u_field(s1245, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1245, 0);
          d1246 = 1;
          {
            u_value t1247 = v_x;
            *r_e = t1247;
          }
        }
        if (!d1246) {
          d1246 = 1;
          {
            u_value t1248 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1248;
          }
        }
        if (!d1246) {
          u_no_case(s1245);
        }
      }
    }
    if ((!d1235) && (u_is_con(s1234, &n_lex_46_TPunct) && u_equals(u_field(s1234, 0), u_mk_char(91)))) {
      d1235 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1235) && u_is_con(s1234, &n_lex_46_TName)) {
      u_value v_n = u_field(s1234, 0);
      u_value t1250 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1250)) {
        t1250 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1250)) {
        d1235 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1249 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1249;
        }
      }
    }
    if (!d1235) {
      d1235 = 1;
      {
        static struct u_object l_expression_32_expec_1251 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1252 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1251, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1252)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1235) {
      u_no_case(s1234);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1253 = v_ps;
  u_value t1254 = NULL;
  if ((!t1254) && u_is_con(s1253, &n_list_46_Nil)) {
    t1254 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1254) && (u_is_con(s1253, &n_list_46_Cons) && u_is_con(u_field(s1253, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1253, 0), 0);
    u_value v_more = u_field(s1253, 1);
    u_value s1255 = f_parse_46_pattern_vars_1(v_more);
    u_value t1256 = NULL;
    if ((!t1256) && u_is_con(s1255, &n_option_46_Some)) {
      u_value v_xs = u_field(s1255, 0);
      t1256 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1256) && u_is_con(s1255, &n_option_46_None)) {
      t1256 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1256) {
      u_no_case(s1255);
    }
    t1254 = t1256;
  }
  if (!t1254) {
    t1254 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1254) {
    u_no_case(s1253);
  }
  return t1254;
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
    static struct u_object l__58__61__1257 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1257), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1258 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1259 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1258;
    v_es = t1259;
    u_value t1263 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1263)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1260 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1260), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1261 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1262 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1261;
        v_es = t1262;
      }
      t1263 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1264 = U_LIT("in");
    p_parse_46_keyword(&l_in_1264, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1265 = f_list_46_reverse_1(v_ps);
    u_value t1266 = f_list_46_reverse_1(v_es);
    v_ps = t1265;
    v_es = t1266;
    u_value s1267 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1268 = 0;
    if ((!d1268) && u_is_con(u_field(s1267, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1267, 0), 0);
      d1268 = 1;
      {
        u_value t1269 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1269;
      }
    }
    if ((!d1268) && (u_is_con(u_field(s1267, 0), &n_option_46_None) && (u_is_con(u_field(s1267, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1267, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1267, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1267, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1267, 1), 0);
      u_value v_c = u_field(u_field(s1267, 2), 0);
      d1268 = 1;
      {
        u_value t1270 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1270;
      }
    }
    if (!d1268) {
      d1268 = 1;
      {
        u_value t1271 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1271;
      }
    }
    if (!d1268) {
      u_no_case(s1267);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1272 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1272;
    static struct u_object l__61__62__1273 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1273)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1274 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1274;
        u_value t1276 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1276)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1275 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1275;
          }
          t1276 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1277 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1277), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1278 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1278;
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
        static struct u_object l_Nil_1279 = U_LIT("Nil");
        u_value t1280 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1279), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1280;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1281 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1281))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1282 = U_LIT("in");
            p_parse_46_keyword(&l_in_1282, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1283 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1284 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1283;
            v_lists = t1284;
            u_value t1288 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1288)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1285 = U_LIT("in");
                p_parse_46_keyword(&l_in_1285, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1286 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1287 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1286;
                v_lists = t1287;
              }
              t1288 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1289 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1289;
            static struct u_object l_if_1290 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1290))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1291 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1291;
          }
        } else {
          {
            u_value t1292 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1292;
            u_value t1294 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1294)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1293 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1293;
              }
              t1294 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1295 = U_LIT("Nil");
            u_value t1296 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1295), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1296;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1297 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1297;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1298 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1298))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1299 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1299;
      }
    } else {
      {
        u_value t1300 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1300;
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
    u_value t1301 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1301;
    static struct u_object l_case_1302 = U_LIT("case");
    u_value t1305 = f_parse_46_at_keyword_2(*r_ts, &l_case_1302);
    while (u_truth(t1305)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1303 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1303), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1304 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1304;
      }
      static struct u_object l_case_1306 = U_LIT("case");
      t1305 = f_parse_46_at_keyword_2(*r_ts, &l_case_1306);
    }
    static struct u_object l_else_1307 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1307))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1308 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1308;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1309 = U_LIT("end");
    p_parse_46_keyword(&l_end_1309, r_ts);
    u_value t1310 = f_list_46_reverse_1(v_rev);
    *r_cs = t1310;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1311 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1311))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1312 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1312, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1313 = v_ps;
  u_value t1314 = NULL;
  if ((!t1314) && u_is_con(s1313, &n_list_46_Nil)) {
    t1314 = v_rest;
  }
  if ((!t1314) && u_is_con(s1313, &n_list_46_Cons)) {
    u_value v_p = u_field(s1313, 0);
    u_value v_more = u_field(s1313, 1);
    static struct u_object l_Cons_1315 = U_LIT("Cons");
    t1314 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1315), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1314) {
    u_no_case(s1313);
  }
  return t1314;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1316 = v_t;
  u_value t1317 = NULL;
  if ((!t1317) && u_is_con(s1316, &n_lex_46_TName)) {
    u_value v_n = u_field(s1316, 0);
    static struct u_object l___1318 = U_LIT("_");
    static struct u_object l_true_1319 = U_LIT("true");
    static struct u_object l_false_1320 = U_LIT("false");
    u_value t1322 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1318, u_mk_con(&n_list_46_Cons, 2, &l_true_1319, u_mk_con(&n_list_46_Cons, 2, &l_false_1320, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1322)) {
      u_value t1321 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1321)) {
        t1321 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1322 = t1321;
    }
    t1317 = t1322;
  }
  if ((!t1317) && u_is_con(s1316, &n_lex_46_TNum)) {
    t1317 = u_mk_bool(1);
  }
  if ((!t1317) && u_is_con(s1316, &n_lex_46_TStr)) {
    t1317 = u_mk_bool(1);
  }
  if ((!t1317) && u_is_con(s1316, &n_lex_46_TChar)) {
    t1317 = u_mk_bool(1);
  }
  if ((!t1317) && u_is_con(s1316, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1316, 0);
    u_value t1323 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1323)) {
      t1323 = u_eq(v_c, u_mk_char(91));
    }
    t1317 = t1323;
  }
  if (!t1317) {
    t1317 = u_mk_bool(0);
  }
  if (!t1317) {
    u_no_case(s1316);
  }
  return t1317;
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
    u_value t1324 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1324;
    u_value t1325 = u_mk_bool(1);
    v_next = t1325;
    u_value t1326 = u_mk_bool(1);
    v_go = t1326;
    u_value t1335 = v_go;
    while (u_truth(t1335)) {
      {
        u_value s1327 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1328 = 0;
        if ((!d1328) && u_is_con(s1327, &n_option_46_Some)) {
          u_value v_op = u_field(s1327, 0);
          d1328 = 1;
          {
            u_value t1329 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1330 = u_mk_bool(1);
            v_rev = t1329;
            v_next = t1330;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1328) && u_is_con(s1327, &n_option_46_None)) {
          u_value t1333 = v_next;
          if (u_truth(t1333)) {
            t1333 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1333)) {
            d1328 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1331 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1332 = u_mk_bool(0);
              v_rev = t1331;
              v_next = t1332;
            }
          }
        }
        if (!d1328) {
          d1328 = 1;
          {
            u_value t1334 = u_mk_bool(0);
            v_go = t1334;
          }
        }
        if (!d1328) {
          u_no_case(s1327);
        }
      }
      t1335 = v_go;
    }
    u_value s1336 = v_rev;
    int d1337 = 0;
    if ((!d1337) && u_is_con(s1336, &n_list_46_Nil)) {
      d1337 = 1;
      {
        static struct u_object l_pattern_32_expected_1338 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1339 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1338, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1339)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1337) {
      d1337 = 1;
      {
        u_value t1340 = f_list_46_reverse_1(v_rev);
        *r_items = t1340;
      }
    }
    if (!d1337) {
      u_no_case(s1336);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1341 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1341;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1342 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1342;
        u_value t1344 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1344)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1343 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1343;
          }
          t1344 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1345 = f_list_46_reverse_1(v_rev);
    *r_ps = t1345;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1346 = f_parse_46_head_token_1(*r_ts);
    int d1347 = 0;
    if ((!d1347) && (u_is_con(s1346, &n_lex_46_TName) && u_equals(u_field(s1346, 0), u_mk_lit("_")))) {
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1348 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1348;
      }
    }
    if ((!d1347) && (u_is_con(s1346, &n_lex_46_TName) && u_equals(u_field(s1346, 0), u_mk_lit("true")))) {
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1349 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1349;
      }
    }
    if ((!d1347) && (u_is_con(s1346, &n_lex_46_TName) && u_equals(u_field(s1346, 0), u_mk_lit("false")))) {
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1350 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1350;
      }
    }
    if ((!d1347) && u_is_con(s1346, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1346, 0);
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1351 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1351;
      }
    }
    if ((!d1347) && u_is_con(s1346, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1346, 0);
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1352 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1352;
      }
    }
    if ((!d1347) && u_is_con(s1346, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1346, 0);
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1353 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1353;
      }
    }
    if ((!d1347) && (u_is_con(s1346, &n_lex_46_TPunct) && u_equals(u_field(s1346, 0), u_mk_char(40)))) {
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1354 = v_ps;
        int d1355 = 0;
        if ((!d1355) && (u_is_con(s1354, &n_list_46_Cons) && u_is_con(u_field(s1354, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1354, 0);
          d1355 = 1;
          {
            u_value t1356 = v_q;
            *r_p = t1356;
          }
        }
        if (!d1355) {
          d1355 = 1;
          {
            u_value t1357 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1357;
          }
        }
        if (!d1355) {
          u_no_case(s1354);
        }
      }
    }
    if ((!d1347) && (u_is_con(s1346, &n_lex_46_TPunct) && u_equals(u_field(s1346, 0), u_mk_char(91)))) {
      d1347 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1358 = U_LIT("Nil");
            u_value t1359 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1358), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1359;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1360 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1360;
            u_value t1362 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1362)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1361 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1361;
              }
              t1362 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1363 = U_LIT("Nil");
            u_value t1364 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1363), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1364;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1365 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1365;
          }
        }
      }
    }
    if (!d1347) {
      d1347 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1366 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1366;
          }
        } else {
          {
            u_value t1367 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1367;
          }
        }
      }
    }
    if (!d1347) {
      u_no_case(s1346);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1368 = v_t;
  u_value t1369 = NULL;
  if ((!t1369) && u_is_con(s1368, &n_lex_46_TName)) {
    u_value v_n = u_field(s1368, 0);
    static struct u_object l_skip_1370 = U_LIT("skip");
    static struct u_object l_if_1371 = U_LIT("if");
    static struct u_object l_while_1372 = U_LIT("while");
    static struct u_object l_call_1373 = U_LIT("call");
    static struct u_object l_match_1374 = U_LIT("match");
    static struct u_object l_assert_1375 = U_LIT("assert");
    static struct u_object l_assign_1376 = U_LIT("assign");
    static struct u_object l_raise_1377 = U_LIT("raise");
    static struct u_object l_try_1378 = U_LIT("try");
    t1369 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1370, u_mk_con(&n_list_46_Cons, 2, &l_if_1371, u_mk_con(&n_list_46_Cons, 2, &l_while_1372, u_mk_con(&n_list_46_Cons, 2, &l_call_1373, u_mk_con(&n_list_46_Cons, 2, &l_match_1374, u_mk_con(&n_list_46_Cons, 2, &l_assert_1375, u_mk_con(&n_list_46_Cons, 2, &l_assign_1376, u_mk_con(&n_list_46_Cons, 2, &l_raise_1377, u_mk_con(&n_list_46_Cons, 2, &l_try_1378, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1369) {
    t1369 = u_mk_bool(0);
  }
  if (!t1369) {
    u_no_case(s1368);
  }
  return t1369;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1379 = v_cs;
  u_value t1380 = NULL;
  if ((!t1380) && u_is_con(s1379, &n_list_46_Nil)) {
    t1380 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1380) && (u_is_con(s1379, &n_list_46_Cons) && u_is_con(u_field(s1379, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1379, 0), 0);
    u_value v_g = u_field(u_field(s1379, 0), 1);
    u_value v_b = u_field(u_field(s1379, 0), 2);
    u_value v_more = u_field(s1379, 1);
    t1380 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1380) {
    u_no_case(s1379);
  }
  return t1380;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1381 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1381;
    u_value t1382 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1382;
    u_value t1383 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1383;
    static struct u_object l_local_1384 = U_LIT("local");
    u_value t1386 = f_parse_46_at_keyword_2(*r_ts, &l_local_1384);
    while (u_truth(t1386)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1385 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1385)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1385 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1387 = U_LIT("local");
      t1386 = f_parse_46_at_keyword_2(*r_ts, &l_local_1387);
    }
    u_value t1389 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1389)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1388 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1388;
      }
      t1389 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1390 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1390;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1391 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1391;
    static struct u_object l__58__1392 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1392)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1393 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1393)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1394 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1394;
      }
    }
    u_value t1395 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1395;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1396 = f_parse_46_head_token_1(*r_ts);
    int d1397 = 0;
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("skip")))) {
      d1397 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1398 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1398;
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("if")))) {
      d1397 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("while")))) {
      d1397 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1399 = U_LIT("end");
        p_parse_46_keyword(&l_end_1399, r_ts);
        u_value t1400 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1400;
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("call")))) {
      d1397 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("match")))) {
      d1397 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1401 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1401;
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("assert")))) {
      d1397 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1402 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1402;
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("assign")))) {
      d1397 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("raise")))) {
      d1397 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1403 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1403;
      }
    }
    if ((!d1397) && (u_is_con(s1396, &n_lex_46_TName) && u_equals(u_field(s1396, 0), u_mk_lit("try")))) {
      d1397 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1404 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1404, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1405 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1405;
      }
    }
    if (!d1397) {
      d1397 = 1;
      {
        static struct u_object l_statement_32_expect_1406 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1407 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1406, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1407)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1397) {
      u_no_case(s1396);
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
    static struct u_object l_else_1408 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1408)))) {
      {
        static struct u_object l_end_1409 = U_LIT("end");
        p_parse_46_keyword(&l_end_1409, r_ts);
        u_value t1410 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1410;
      }
    } else {
      static struct u_object l_if_1411 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1411)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1412 = U_LIT("end");
          p_parse_46_keyword(&l_end_1412, r_ts);
        }
      }
    }
    u_value t1413 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1413;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1414 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1414), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1415 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1415;
    u_value t1418 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1418)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1416 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1416), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1417 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1417;
      }
      t1418 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1419 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1419;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1420 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1420;
    static struct u_object l_case_1421 = U_LIT("case");
    u_value t1424 = f_parse_46_at_keyword_2(*r_ts, &l_case_1421);
    while (u_truth(t1424)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1422 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1422), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1423 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1423;
      }
      static struct u_object l_case_1425 = U_LIT("case");
      t1424 = f_parse_46_at_keyword_2(*r_ts, &l_case_1425);
    }
    static struct u_object l_else_1426 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1426))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1427 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1427;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1428 = U_LIT("end");
    p_parse_46_keyword(&l_end_1428, r_ts);
    u_value t1429 = f_list_46_reverse_1(v_rev);
    *r_cs = t1429;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1431;
  if (u_truth(v_first)) {
    t1431 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1430 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1430)) {
      t1430 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1431 = t1430;
  }
  return t1431;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1433 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1433)) {
    static struct u_object l_out_1432 = U_LIT("out");
    t1433 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1432));
  }
  u_value t1435 = t1433;
  if (u_truth(t1435)) {
    static struct u_object l_ref_1434 = U_LIT("ref");
    t1435 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1434));
  }
  return t1435;
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
    u_value t1436 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1437 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1438 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1439 = u_mk_bool(1);
    v_ins = t1436;
    v_outs = t1437;
    v_refs = t1438;
    v_first = t1439;
    static struct u_object l_in_1440 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1440))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1441 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1442 = u_mk_bool(0);
        v_ins = t1441;
        v_first = t1442;
        u_value t1444 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1444)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1443 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1443;
          }
          t1444 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1445 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1445, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1446 = u_mk_bool(0);
        v_first = t1446;
      }
    }
    static struct u_object l_ref_1447 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1447, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1448 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1448;
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
    u_value t1449 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1449;
    u_value t1451 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1451)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1450 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1450;
      }
      t1451 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1452 = f_list_46_reverse_1(v_rev);
    *r_xs = t1452;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1453 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1453;
    static struct u_object l__58__1454 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1454)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1455 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1455;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1456 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1456;
    u_value t1458 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1458)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1457 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1457;
      }
      t1458 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1459 = f_list_46_reverse_1(v_rev);
    *r_xs = t1459;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1460 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1460;
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
  u_value s1461 = v_cs;
  u_value t1462 = NULL;
  if ((!t1462) && u_is_con(s1461, &n_list_46_Nil)) {
    t1462 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1462) && (u_is_con(s1461, &n_list_46_Cons) && u_is_con(u_field(s1461, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1461, 1);
    t1462 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1462) && (u_is_con(s1461, &n_list_46_Cons) && u_is_con(u_field(s1461, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1461, 1);
    t1462 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1462) && (u_is_con(s1461, &n_list_46_Cons) && u_is_con(u_field(s1461, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1461, 1);
    t1462 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1462) && u_is_con(s1461, &n_list_46_Cons)) {
    u_value v_c = u_field(s1461, 0);
    u_value v_more = u_field(s1461, 1);
    t1462 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1462) {
    u_no_case(s1461);
  }
  return t1462;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1463 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1464 = &t1463;
  u_value l1465 = v_cs;
  while (u_is_con(l1465, &n_list_46_Cons)) {
    u_value v_c = u_field(l1465, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1464 = u_append(e1464, v_c);
    }
    l1465 = u_field(l1465, 1);
  }
  return t1463;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1466 = v_c;
  u_value t1467 = NULL;
  if ((!t1467) && u_is_con(s1466, &n_syntax_46_CNotation)) {
    t1467 = u_mk_bool(1);
  }
  if (!t1467) {
    t1467 = u_mk_bool(0);
  }
  if (!t1467) {
    u_no_case(s1466);
  }
  return t1467;
}

u_value f_parse_46_Loader_46_modules_1(u_value v_x) {
  u_value s1468 = v_x;
  u_value t1469 = NULL;
  if ((!t1469) && u_is_con(s1468, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1468, 0);
    t1469 = v_y;
  }
  if (!t1469) {
    u_no_case(s1468);
  }
  return t1469;
}

u_value f_parse_46_Loader_46_loading_1(u_value v_x) {
  u_value s1470 = v_x;
  u_value t1471 = NULL;
  if ((!t1471) && u_is_con(s1470, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1470, 1);
    t1471 = v_y;
  }
  if (!t1471) {
    u_no_case(s1470);
  }
  return t1471;
}

u_value f_parse_46_Loader_46_base_1(u_value v_x) {
  u_value s1472 = v_x;
  u_value t1473 = NULL;
  if ((!t1473) && u_is_con(s1472, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1472, 2);
    t1473 = v_y;
  }
  if (!t1473) {
    u_no_case(s1472);
  }
  return t1473;
}

u_value f_parse_46_Loader_46_dir_1(u_value v_x) {
  u_value s1474 = v_x;
  u_value t1475 = NULL;
  if ((!t1475) && u_is_con(s1474, &n_parse_46_Loader)) {
    u_value v_y = u_field(s1474, 3);
    t1475 = v_y;
  }
  if (!t1475) {
    u_no_case(s1474);
  }
  return t1475;
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1476 = v_c;
  u_value t1477 = NULL;
  if ((!t1477) && u_is_con(s1476, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1476, 0);
    u_value v_cs = u_field(s1476, 1);
    u_value t1478 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1479 = &t1478;
    u_value l1480 = v_cs;
    while (u_is_con(l1480, &n_list_46_Cons)) {
      u_value v_d = u_field(l1480, 0);
      if (u_truth(u_mk_bool(1))) {
        e1479 = u_append(e1479, f_parse_46_modules_of_1(v_d));
      }
      l1480 = u_field(l1480, 1);
    }
    t1477 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1478));
  }
  if (!t1477) {
    t1477 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1477) {
    u_no_case(s1476);
  }
  return t1477;
}

u_value f_parse_46_own_notations_2(u_value v_ld, u_value v_n) {
  u_value s1481 = f_list_46_lookup_2(v_n, f_parse_46_Loader_46_modules_1(v_ld));
  u_value t1482 = NULL;
  if ((!t1482) && u_is_con(s1481, &n_option_46_Some)) {
    u_value v_ns = u_field(s1481, 0);
    t1482 = v_ns;
  }
  if ((!t1482) && u_is_con(s1481, &n_option_46_None)) {
    t1482 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1482) {
    u_no_case(s1481);
  }
  return t1482;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1483 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1483;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1484 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1484;
        u_value t1486 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1486)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1485 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1485;
          }
          t1486 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1487 = f_list_46_reverse_1(v_rev);
    *r_ps = t1487;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1488 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1488;
    u_value s1489 = f_parse_46_head_token_1(*r_ts);
    int d1490 = 0;
    if ((!d1490) && (u_is_con(s1489, &n_lex_46_TName) && u_equals(u_field(s1489, 0), u_mk_lit("prefix")))) {
      d1490 = 1;
      {
        u_value t1491 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1491;
      }
    }
    if ((!d1490) && (u_is_con(s1489, &n_lex_46_TName) && u_equals(u_field(s1489, 0), u_mk_lit("postfix")))) {
      d1490 = 1;
      {
        u_value t1492 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1492;
      }
    }
    if (!d1490) {
      d1490 = 1;
      {
        u_value s1493 = f_parse_46_second_token_1(*r_ts);
        int d1494 = 0;
        if ((!d1494) && (u_is_con(s1493, &n_lex_46_TName) && u_equals(u_field(s1493, 0), u_mk_lit("left")))) {
          d1494 = 1;
          {
            u_value t1495 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1495;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1494) && (u_is_con(s1493, &n_lex_46_TName) && u_equals(u_field(s1493, 0), u_mk_lit("right")))) {
          d1494 = 1;
          {
            u_value t1496 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1496;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1494) {
          d1494 = 1;
          {
          }
        }
        if (!d1494) {
          u_no_case(s1493);
        }
      }
    }
    if (!d1490) {
      u_no_case(s1489);
    }
    p_parse_46_advance(r_ts);
    u_value s1497 = f_parse_46_head_token_1(*r_ts);
    int d1498 = 0;
    if ((!d1498) && u_is_con(s1497, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1497, 0);
      d1498 = 1;
      {
        u_value t1499 = v_n;
        v_p = t1499;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1498) {
      d1498 = 1;
      {
        static struct u_object l_precedence_32_expec_1500 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1501 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1500, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1501)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1498) {
      u_no_case(s1497);
    }
    u_value t1502 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1503 = u_mk_bool(1);
    v_ops = t1502;
    v_go = t1503;
    u_value t1509 = v_go;
    while (u_truth(t1509)) {
      {
        u_value s1504 = f_parse_46_head_token_1(*r_ts);
        int d1505 = 0;
        if ((!d1505) && u_is_con(s1504, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1504, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1505 = 1;
            {
              u_value t1506 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1506;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1505) && u_is_con(s1504, &n_lex_46_TName)) {
          u_value v_n = u_field(s1504, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1505 = 1;
            {
              u_value t1507 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1507;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1505) {
          d1505 = 1;
          {
            u_value t1508 = u_mk_bool(0);
            v_go = t1508;
          }
        }
        if (!d1505) {
          u_no_case(s1504);
        }
      }
      t1509 = v_go;
    }
    u_value s1510 = v_ops;
    int d1511 = 0;
    if ((!d1511) && u_is_con(s1510, &n_list_46_Nil)) {
      d1511 = 1;
      {
        static struct u_object l_operator_32_expecte_1512 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1513 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1512, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1513)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1511) {
      d1511 = 1;
      {
        u_value t1514 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1514;
      }
    }
    if (!d1511) {
      u_no_case(s1510);
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
    u_value t1515 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1516 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1515;
    *r_fs = t1516;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1517 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1518 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1517;
            *r_fs = t1518;
            u_value t1521 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1521)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1519 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1520 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1519;
                *r_fs = t1520;
              }
              t1521 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1522 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1523 = f_list_46_reverse_1(*r_fs);
    *r_k = t1522;
    *r_fs = t1523;
    u_value s1524 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1525 = 0;
    if ((!d1525) && u_is_con(s1524, &n_option_46_Some)) {
      u_value v_f = u_field(s1524, 0);
      d1525 = 1;
      {
        static struct u_object l_repeated_32_field_32__1526 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1527 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1526, u_concat(v_f, u_concat(&l__32_of_32__1527, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1525) && u_is_con(s1524, &n_option_46_None)) {
      d1525 = 1;
      {
      }
    }
    if (!d1525) {
      u_no_case(s1524);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1528 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1529 = 0;
    if ((!d1529) && (u_is_con(u_field(s1528, 0), &n_lex_46_TName) && (u_is_con(u_field(s1528, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1528, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1528, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1529 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1530 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1530;
        }
      }
    }
    if (!d1529) {
      d1529 = 1;
      {
        u_value t1531 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1531;
      }
    }
    if (!d1529) {
      u_no_case(s1528);
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
    static struct u_object l__58__61__1532 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1532), r_ts);
    static struct u_object l__124__1533 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1533)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1534 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1534;
    static struct u_object l__124__1535 = U_LIT("|");
    u_value t1537 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1535));
    while (u_truth(t1537)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1536 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1536;
      }
      static struct u_object l__124__1538 = U_LIT("|");
      t1537 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1538));
    }
    u_value s1539 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1540 = 0;
    if ((!d1540) && u_is_con(s1539, &n_option_46_Some)) {
      u_value v_f = u_field(s1539, 0);
      d1540 = 1;
      {
        static struct u_object l_field_32__1541 = U_LIT("field ");
        static struct u_object l__32_of_32__1542 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1543 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1541, u_concat(v_f, u_concat(&l__32_of_32__1542, u_concat(v_n, &l__32_has_32_different_32_t_1543)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1540) && u_is_con(s1539, &n_option_46_None)) {
      d1540 = 1;
      {
      }
    }
    if (!d1540) {
      u_no_case(s1539);
    }
    u_value t1544 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)));
    *r_cs = t1544;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1545 = v_fs;
  u_value t1546 = NULL;
  if ((!t1546) && u_is_con(s1545, &n_list_46_Nil)) {
    t1546 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1546) && (u_is_con(s1545, &n_list_46_Cons) && u_is_con(u_field(s1545, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1545, 0), 0);
    u_value v_rest = u_field(s1545, 1);
    t1546 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1546) && (u_is_con(s1545, &n_list_46_Cons) && u_is_con(u_field(s1545, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1545, 1);
    t1546 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1546) {
    u_no_case(s1545);
  }
  return t1546;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1547 = v_fs;
  u_value t1548 = NULL;
  if ((!t1548) && u_is_con(s1547, &n_list_46_Nil)) {
    t1548 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1548) && u_is_con(s1547, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1547, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1547, 0), 1), 3);
    u_value v_rest = u_field(s1547, 1);
    u_value t1549 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1550 = &t1549;
    u_value l1551 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1551, &n_list_46_Cons)) {
      u_value v_e = u_field(l1551, 0);
      if (u_truth(u_mk_bool(1))) {
        e1550 = u_append(e1550, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1551 = u_field(l1551, 1);
    }
    u_value t1552;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1549))) {
      t1552 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1552 = f_parse_46_differing_1(v_rest);
    }
    t1548 = t1552;
  }
  if (!t1548) {
    u_no_case(s1547);
  }
  return t1548;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1553 = v_e;
  u_value t1554 = NULL;
  if (!t1554) {
    u_value v_t = u_field(u_field(s1553, 1), 3);
    t1554 = v_t;
  }
  if (!t1554) {
    u_no_case(s1553);
  }
  return t1554;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1555 = v_fs;
  u_value t1556 = NULL;
  if ((!t1556) && u_is_con(s1555, &n_list_46_Nil)) {
    t1556 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1556) && u_is_con(s1555, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1555, 0), 0);
    u_value v_d = u_field(u_field(s1555, 0), 1);
    u_value v_rest = u_field(s1555, 1);
    u_value t1557;
    if (u_truth(u_eq(v_g, v_f))) {
      t1557 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1557 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1556 = t1557;
  }
  if (!t1556) {
    u_no_case(s1555);
  }
  return t1556;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1558 = v_ks;
  u_value t1559 = NULL;
  if ((!t1559) && u_is_con(s1558, &n_list_46_Nil)) {
    t1559 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1559) && (u_is_con(s1558, &n_list_46_Cons) && u_is_con(u_field(u_field(s1558, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1558, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1558, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1558, 0), 1);
    u_value v_rest = u_field(s1558, 1);
    t1559 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1559) {
    u_no_case(s1558);
  }
  return t1559;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1560 = v_fs;
  u_value t1561 = NULL;
  if ((!t1561) && u_is_con(s1560, &n_list_46_Nil)) {
    t1561 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1561) && (u_is_con(s1560, &n_list_46_Cons) && u_is_con(u_field(u_field(s1560, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1560, 0), 0), 0);
    u_value v_t = u_field(u_field(s1560, 0), 1);
    u_value v_rest = u_field(s1560, 1);
    t1561 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1561) && (u_is_con(s1560, &n_list_46_Cons) && u_is_con(u_field(u_field(s1560, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1560, 1);
    t1561 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1561) {
    u_no_case(s1560);
  }
  return t1561;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1566 = f_parse_46_fields_of_1(v_ks);
  u_value t1565;
  {
    u_value v_fs = t1566;
    u_value t1562 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1563 = &t1562;
    u_value l1564 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1564, &n_list_46_Cons)) {
      u_value v_f = u_field(l1564, 0);
      if (u_truth(u_mk_bool(1))) {
        e1563 = u_append(e1563, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1564 = u_field(l1564, 1);
    }
    t1565 = t1562;
  }
  return t1565;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1567 = U_LIT("x");
  u_value t1568 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1569 = &t1568;
  u_value l1570 = v_ps;
  while (u_is_con(l1570, &n_list_46_Cons)) {
    u_value v_p = u_field(l1570, 0);
    if (u_truth(u_mk_bool(1))) {
      e1569 = u_append(e1569, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1570 = u_field(l1570, 1);
  }
  u_value t1578 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1567), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1579 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1568);
  u_value t1577;
  {
    u_value v_x = t1578;
    u_value v_owner = t1579;
    static struct u_object l__46__1571 = U_LIT(".");
    static struct u_object l_x_1572 = U_LIT("x");
    u_value t1573 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1574 = &t1573;
    u_value l1575 = v_es;
    while (u_is_con(l1575, &n_list_46_Cons)) {
      u_value v_e = u_field(l1575, 0);
      if (u_truth(u_mk_bool(1))) {
        e1574 = u_append(e1574, f_parse_46_field_case_1(v_e));
      }
      l1575 = u_field(l1575, 1);
    }
    u_value t1576;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1576 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1576 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1577 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1571, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1572), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1573, t1576)));
  }
  return t1577;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1580 = v_es;
  u_value t1581 = NULL;
  if ((!t1581) && u_is_con(s1580, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1580, 0), 1), 3);
    t1581 = v_t;
  }
  if (!t1581) {
    t1581 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1581) {
    u_no_case(s1580);
  }
  return t1581;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1582 = v_e;
  u_value t1583 = NULL;
  if (!t1583) {
    u_value v_k = u_field(u_field(s1582, 1), 0);
    u_value v_i = u_field(u_field(s1582, 1), 1);
    u_value v_n = u_field(u_field(s1582, 1), 2);
    u_value t1584 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1585 = &t1584;
    u_value l1586 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1586, &n_list_46_Cons)) {
      u_value v_j = u_field(l1586, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1588;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1587 = U_LIT("y");
          t1588 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1587), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1588 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1585 = u_append(e1585, t1588);
      }
      l1586 = u_field(l1586, 1);
    }
    static struct u_object l_y_1589 = U_LIT("y");
    t1583 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1584), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1589), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1583) {
    u_no_case(s1582);
  }
  return t1583;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1590 = U_LIT("MissingField");
  static struct u_object l__46__1591 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1590), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1591, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1592;
  if (u_truth(u_eq(v_i, v_n))) {
    t1592 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1592 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1592;
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
    u_value t1593 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1593;
    u_value t1595 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1595)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1594 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1594;
      }
      t1595 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1596 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1596), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1597 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1597), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1598 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1598;
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
    u_value t1599 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1600 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1601 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1602 = u_mk_bool(1);
    v_ins = t1599;
    v_outs = t1600;
    v_refs = t1601;
    v_first = t1602;
    static struct u_object l_in_1603 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1603))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1604 = u_mk_bool(0);
        v_first = t1604;
      }
    }
    static struct u_object l_out_1605 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1605, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1606 = u_mk_bool(0);
        v_first = t1606;
      }
    }
    static struct u_object l_ref_1607 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1607, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1608 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1608), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1609 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1609;
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
    u_value s1610 = f_parse_46_head_token_1(*r_ts);
    int d1611 = 0;
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("import")))) {
      d1611 = 1;
      {
        u_value t1612 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1612;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Loader_46_modules_1(v_ld))))) {
          {
            u_value t1613 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1613;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_ns, v_ld, v_at, r_cs);
            u_value t1614 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1614;
          }
        }
      }
    }
    if ((!d1611) && u_is_con(s1610, &n_lex_46_TName)) {
      u_value v_w = u_field(s1610, 0);
      static struct u_object l_prefix_1616 = U_LIT("prefix");
      static struct u_object l_postfix_1617 = U_LIT("postfix");
      static struct u_object l_infix_1618 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1616, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1617, u_mk_con(&n_list_46_Cons, 2, &l_infix_1618, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1611 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1615 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1615;
        }
      }
    }
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("type")))) {
      d1611 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1619 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1619), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1620 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1620;
      }
    }
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("data")))) {
      d1611 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("function")))) {
      d1611 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1621 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1621;
      }
    }
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("procedure")))) {
      d1611 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1622 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1622;
      }
    }
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("eval")))) {
      d1611 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1623 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1623;
      }
    }
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("run")))) {
      d1611 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1624 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1624), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1625 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1625;
      }
    }
    if ((!d1611) && (u_is_con(s1610, &n_lex_46_TName) && u_equals(u_field(s1610, 0), u_mk_lit("test")))) {
      d1611 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1626 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1626), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1627 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1627;
      }
    }
    if (!d1611) {
      d1611 = 1;
      {
        static struct u_object l_declaration_32_expe_1628 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1629 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1628, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1629)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1611) {
      u_no_case(s1610);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_ns, u_value *r_ld) {
  {
    u_value s1630 = v_cs;
    int d1631 = 0;
    if ((!d1631) && u_is_con(s1630, &n_list_46_Nil)) {
      d1631 = 1;
      {
      }
    }
    if ((!d1631) && (u_is_con(s1630, &n_list_46_Cons) && u_is_con(u_field(s1630, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1630, 0), 0);
      u_value v_p = u_field(u_field(s1630, 0), 1);
      u_value v_ops = u_field(u_field(s1630, 0), 2);
      u_value v_more = u_field(s1630, 1);
      d1631 = 1;
      {
        u_value t1632 = f_list_46__43__43__2(*r_ns, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_ns = t1632;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1631) && (u_is_con(s1630, &n_list_46_Cons) && u_is_con(u_field(s1630, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1630, 0), 0);
      u_value v_ds = u_field(u_field(s1630, 0), 1);
      u_value v_more = u_field(s1630, 1);
      d1631 = 1;
      {
        u_value t1633 = u_mk_con(&n_parse_46_Loader, 4, f_list_46__43__43__2(f_parse_46_modules_of_1(u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds)), f_parse_46_Loader_46_modules_1(*r_ld)), f_parse_46_Loader_46_loading_1(*r_ld), f_parse_46_Loader_46_base_1(*r_ld), f_parse_46_Loader_46_dir_1(*r_ld));
        *r_ld = t1633;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1631) && (u_is_con(s1630, &n_list_46_Cons) && u_is_con(u_field(s1630, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1630, 0), 0);
      u_value v_more = u_field(s1630, 1);
      d1631 = 1;
      {
        u_value t1634 = f_list_46__43__43__2(*r_ns, f_parse_46_own_notations_2(*r_ld, v_n));
        *r_ns = t1634;
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if ((!d1631) && u_is_con(s1630, &n_list_46_Cons)) {
      u_value v_more = u_field(s1630, 1);
      d1631 = 1;
      {
        p_parse_46_declare_all(v_more, r_ns, r_ld);
      }
    }
    if (!d1631) {
      u_no_case(s1630);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_ns, u_value *r_ld, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1635 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1635;
    u_value t1637 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1637)) {
      {
        p_parse_46_parse_command(*r_ns, *r_ld, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_ns, r_ld);
        u_value t1636 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1636;
      }
      t1637 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1638 = f_list_46_reverse_1(v_rev);
    *r_cs = t1638;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1639 = v_dir;
    int d1640 = 0;
    if ((!d1640) && u_is_con(s1639, &n_option_46_Some)) {
      u_value v_d = u_field(s1639, 0);
      d1640 = 1;
      {
        {
          struct u_handler *h1643 = u_push();
          if (setjmp(h1643->env) == 0) {
            {
              static struct u_object l__47__1641 = U_LIT("/");
              static struct u_object l__46_u_1642 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1641, u_concat(v_n, &l__46_u_1642))), r_text);
            }
            u_pop(h1643);
          } else {
            u_pop(h1643);
            int d1644 = 0;
            if ((!d1644) && (u_has_type(h1643, "os.OsError") && u_is_con(u_caught(h1643), &n_os_46_OsError))) {
              d1644 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1644) {
              u_reraise(h1643);
            }
          }
        }
      }
    }
    if ((!d1640) && u_is_con(s1639, &n_option_46_None)) {
      d1640 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1640) {
      u_no_case(s1639);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1645 = u_push();
      if (setjmp(h1645->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1645);
      } else {
        u_pop(h1645);
        int d1646 = 0;
        if ((!d1646) && (u_has_type(h1645, "os.OsError") && u_is_con(u_caught(h1645), &n_os_46_OsError))) {
          d1646 = 1;
          {
            static struct u_object l_unknown_32_module_32__1647 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1647, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1646) {
          u_reraise(h1645);
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
        static struct u_object l_cyclic_32_import_32_of_1648 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1648, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Loader_46_dir_1(v_ld), v_at, &v_text);
    u_value t1649 = u_mk_con(&n_parse_46_Loader, 4, f_parse_46_Loader_46_modules_1(v_ld), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Loader_46_loading_1(v_ld)), f_parse_46_Loader_46_base_1(v_ld), f_parse_46_Loader_46_dir_1(v_ld));
    v_mld = t1649;
    u_value s1650 = f_parse_46_Loader_46_base_1(v_ld);
    int d1651 = 0;
    if ((!d1651) && u_is_con(s1650, &n_option_46_Some)) {
      u_value v_b = u_field(s1650, 0);
      d1651 = 1;
      {
        u_value t1652 = v_b;
        v_mns = t1652;
      }
    }
    if ((!d1651) && u_is_con(s1650, &n_option_46_None)) {
      d1651 = 1;
      {
        u_value t1653 = v_ns;
        v_mns = t1653;
      }
    }
    if (!d1651) {
      u_no_case(s1650);
    }
    {
      struct u_handler *h1654 = u_push();
      if (setjmp(h1654->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mns, &v_mld, &v_mts);
        }
        u_pop(h1654);
      } else {
        u_pop(h1654);
        int d1655 = 0;
        if ((!d1655) && (u_has_type(h1654, "lex.LexError") && u_is_con(u_caught(h1654), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1654), 0);
          u_value v_i = u_field(u_caught(h1654), 1);
          d1655 = 1;
          {
            static struct u_object l__32_in_32__1656 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1656, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1655) && (u_has_type(h1654, "parse.ParseError") && u_is_con(u_caught(h1654), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1654), 0);
          u_value v_i = u_field(u_caught(h1654), 1);
          d1655 = 1;
          {
            static struct u_object l__32_in_32__1657 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1657, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1655) {
          u_reraise(h1654);
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
      struct u_handler *h1658 = u_push();
      if (setjmp(h1658->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1658);
      } else {
        u_pop(h1658);
        int d1659 = 0;
        if ((!d1659) && (u_has_type(h1658, "lex.LexError") && u_is_con(u_caught(h1658), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1658), 0);
          u_value v_i = u_field(u_caught(h1658), 1);
          d1659 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1659) {
          u_reraise(h1658);
        }
      }
    }
    u_value t1660 = v_ns;
    u_value t1661 = v_ld;
    v_ns1 = t1660;
    v_ld1 = t1661;
    p_parse_46_parse_commands(&v_cs, &v_ns1, &v_ld1, &v_ts);
    u_value t1662 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1662;
  }
}

void p_parse_46_prelude_scope(u_value v_pre, u_value *r_ns, u_value *r_ms) {
  {
    u_value v_ld = NULL;
    u_value t1663 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1664 = u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    *r_ns = t1663;
    v_ld = t1664;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), r_ns, &v_ld);
    u_value t1665 = f_parse_46_Loader_46_modules_1(v_ld);
    *r_ms = t1665;
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
    static struct u_object l_prelude_1666 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1666))) {
      {
        static struct u_object l_prelude_1667 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_parse_46_Loader, 4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1667, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1668 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1668, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1669 = v_xs;
  u_value t1670 = NULL;
  if ((!t1670) && (u_is_con(s1669, &n_list_46_Cons) && u_is_con(u_field(s1669, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1669, 0);
    t1670 = v_x;
  }
  if ((!t1670) && u_is_con(s1669, &n_list_46_Cons)) {
    u_value v_more = u_field(s1669, 1);
    t1670 = f_parse_46_last_part_1(v_more);
  }
  if (!t1670) {
    static struct u_object l__1671 = U_LIT("");
    t1670 = &l__1671;
  }
  if (!t1670) {
    u_no_case(s1669);
  }
  return t1670;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1676 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1675;
  {
    u_value v_f = t1676;
    u_value t1673 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1673)) {
      static struct u_object l__46_u_1672 = U_LIT(".u");
      t1673 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1672);
    }
    u_value t1674;
    if (u_truth(t1673)) {
      t1674 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1674 = v_f;
    }
    t1675 = t1674;
  }
  return t1675;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1681 = u_str_split(v_path, u_mk_char(47));
  u_value t1680;
  {
    u_value v_parts = t1681;
    u_value t1679;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1677 = U_LIT(".");
      t1679 = &l__46__1677;
    } else {
      static struct u_object l__47__1678 = U_LIT("/");
      t1679 = u_str_join(&l__47__1678, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1680 = t1679;
  }
  return t1680;
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
    static struct u_object l_prelude_1682 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1682, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1683 = u_mk_tuple(2, v_cs, v_ds);
    int d1684 = 0;
    if ((!d1684) && (u_is_con(u_field(s1683, 0), &n_list_46_Cons) && u_is_con(u_field(s1683, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1683, 0), 0);
      u_value v_more = u_field(u_field(s1683, 0), 1);
      u_value v_d = u_field(u_field(s1683, 1), 0);
      u_value v_rest = u_field(u_field(s1683, 1), 1);
      d1684 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1684) {
      d1684 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1684) {
      u_no_case(s1683);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1685 = v_paths;
    int d1686 = 0;
    if ((!d1686) && u_is_con(s1685, &n_list_46_Nil)) {
      d1686 = 1;
      {
      }
    }
    if ((!d1686) && u_is_con(s1685, &n_list_46_Cons)) {
      u_value v_path = u_field(s1685, 0);
      u_value v_more = u_field(s1685, 1);
      d1686 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1686) {
      u_no_case(s1685);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1687 = v_paths;
    int d1688 = 0;
    if ((!d1688) && u_is_con(s1687, &n_list_46_Nil)) {
      d1688 = 1;
      {
      }
    }
    if ((!d1688) && u_is_con(s1687, &n_list_46_Cons)) {
      u_value v_path = u_field(s1687, 0);
      u_value v_more = u_field(s1687, 1);
      d1688 = 1;
      {
        u_value t1689 = u_mk_bool(0);
        v_failed = t1689;
        {
          struct u_handler *h1690 = u_push();
          if (setjmp(h1690->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1690);
          } else {
            u_pop(h1690);
            int d1691 = 0;
            if ((!d1691) && (u_has_type(h1690, "parse.ParseError") && u_is_con(u_caught(h1690), &n_parse_46_ParseError))) {
              d1691 = 1;
              {
                u_value t1692 = u_mk_bool(1);
                v_failed = t1692;
              }
            }
            if (!d1691) {
              u_reraise(h1690);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1688) {
      u_no_case(s1687);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1693 = v_x;
  u_value t1694 = NULL;
  if ((!t1694) && u_is_con(s1693, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1693, 0);
    t1694 = v_y;
  }
  if (!t1694) {
    u_no_case(s1693);
  }
  return t1694;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1695 = v_x;
  u_value t1696 = NULL;
  if ((!t1696) && u_is_con(s1695, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1695, 1);
    t1696 = v_y;
  }
  if (!t1696) {
    u_no_case(s1695);
  }
  return t1696;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1697 = v_x;
  u_value t1698 = NULL;
  if ((!t1698) && u_is_con(s1697, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1697, 2);
    t1698 = v_y;
  }
  if (!t1698) {
    u_no_case(s1697);
  }
  return t1698;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1699 = v_x;
  u_value t1700 = NULL;
  if ((!t1700) && u_is_con(s1699, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1699, 3);
    t1700 = v_y;
  }
  if (!t1700) {
    u_no_case(s1699);
  }
  return t1700;
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
  static struct u_object l_u_value_1701 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1701);
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
  static struct u_object l_u_value_1702 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1702));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1703 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1704 = &t1703;
  u_value l1705 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1705, &n_list_46_Cons)) {
    u_value v_c = u_field(l1705, 0);
    if (u_truth(u_mk_bool(1))) {
      e1704 = u_append(e1704, f_compile_46_mangle_char_1(v_c));
    }
    l1705 = u_field(l1705, 1);
  }
  return u_string(f_list_46_concat_1(t1703));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1706 = u_is_letter(v_c);
  if (!u_truth(t1706)) {
    t1706 = u_is_digit(v_c);
  }
  u_value t1707 = t1706;
  if (!u_truth(t1707)) {
    t1707 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1710;
  if (u_truth(t1707)) {
    t1710 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1708 = U_LIT("_");
    static struct u_object l___1709 = U_LIT("_");
    t1710 = u_chars(u_concat(&l___1708, u_concat(u_str_show(u_ord(v_c)), &l___1709)));
  }
  return t1710;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1711 = U_LIT("f_");
  static struct u_object l___1712 = U_LIT("_");
  return u_concat(&l_f__1711, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1712, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1713 = U_LIT("p_");
  return u_concat(&l_p__1713, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1714 = U_LIT("v_");
  return u_concat(&l_v__1714, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1715 = U_LIT("r_");
  return u_concat(&l_r__1715, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1716 = U_LIT("+");
  static struct u_object l_u_add_1717 = U_LIT("u_add");
  static struct u_object l__45__1718 = U_LIT("-");
  static struct u_object l_u_sub_1719 = U_LIT("u_sub");
  static struct u_object l__42__1720 = U_LIT("*");
  static struct u_object l_u_mul_1721 = U_LIT("u_mul");
  static struct u_object l__47__1722 = U_LIT("/");
  static struct u_object l_u_div_1723 = U_LIT("u_div");
  static struct u_object l__37__1724 = U_LIT("%");
  static struct u_object l_u_mod_1725 = U_LIT("u_mod");
  static struct u_object l__60__1726 = U_LIT("<");
  static struct u_object l_u_lt_1727 = U_LIT("u_lt");
  static struct u_object l__61__61__1728 = U_LIT("==");
  static struct u_object l_u_eq_1729 = U_LIT("u_eq");
  static struct u_object l_compare_1730 = U_LIT("compare");
  static struct u_object l_u_compare_1731 = U_LIT("u_compare");
  static struct u_object l_chars_1732 = U_LIT("chars");
  static struct u_object l_u_chars_1733 = U_LIT("u_chars");
  static struct u_object l_string_1734 = U_LIT("string");
  static struct u_object l_u_string_1735 = U_LIT("u_string");
  static struct u_object l_ord_1736 = U_LIT("ord");
  static struct u_object l_u_ord_1737 = U_LIT("u_ord");
  static struct u_object l_chr_1738 = U_LIT("chr");
  static struct u_object l_u_chr_1739 = U_LIT("u_chr");
  static struct u_object l__94__1740 = U_LIT("^");
  static struct u_object l_u_concat_1741 = U_LIT("u_concat");
  static struct u_object l_str_size_1742 = U_LIT("str_size");
  static struct u_object l_u_str_size_1743 = U_LIT("u_str_size");
  static struct u_object l_str_join_1744 = U_LIT("str_join");
  static struct u_object l_u_str_join_1745 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1746 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1747 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1748 = U_LIT("str_split");
  static struct u_object l_u_str_split_1749 = U_LIT("u_str_split");
  static struct u_object l_str_get_1750 = U_LIT("str_get");
  static struct u_object l_u_str_get_1751 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1752 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1753 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1754 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1755 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1756 = U_LIT("str_show");
  static struct u_object l_u_str_show_1757 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1758 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1759 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1760 = U_LIT("str_number");
  static struct u_object l_u_str_number_1761 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1762 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1763 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1764 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1765 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1766 = U_LIT("is_space");
  static struct u_object l_u_is_space_1767 = U_LIT("u_is_space");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1716), u_mk_tuple(2, &l_u_add_1717, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1718), u_mk_tuple(2, &l_u_sub_1719, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1720), u_mk_tuple(2, &l_u_mul_1721, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1722), u_mk_tuple(2, &l_u_div_1723, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1724), u_mk_tuple(2, &l_u_mod_1725, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1726), u_mk_tuple(2, &l_u_lt_1727, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1728), u_mk_tuple(2, &l_u_eq_1729, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1730), u_mk_tuple(2, &l_u_compare_1731, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1732), u_mk_tuple(2, &l_u_chars_1733, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1734), u_mk_tuple(2, &l_u_string_1735, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1736), u_mk_tuple(2, &l_u_ord_1737, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1738), u_mk_tuple(2, &l_u_chr_1739, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1740), u_mk_tuple(2, &l_u_concat_1741, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1742), u_mk_tuple(2, &l_u_str_size_1743, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1744), u_mk_tuple(2, &l_u_str_join_1745, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1746), u_mk_tuple(2, &l_u_str_slice_1747, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1748), u_mk_tuple(2, &l_u_str_split_1749, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1750), u_mk_tuple(2, &l_u_str_get_1751, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1752), u_mk_tuple(2, &l_u_str_contains_1753, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1754), u_mk_tuple(2, &l_u_str_compare_1755, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1756), u_mk_tuple(2, &l_u_str_show_1757, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1758), u_mk_tuple(2, &l_u_str_quote_1759, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1760), u_mk_tuple(2, &l_u_str_number_1761, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1762), u_mk_tuple(2, &l_u_is_digit_1763, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1764), u_mk_tuple(2, &l_u_is_letter_1765, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1766), u_mk_tuple(2, &l_u_is_space_1767, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1768 = U_LIT("&");
  static struct u_object l_n__1769 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1768, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1769, f_compile_46_mangle_1(v_n))));
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
  u_value s1770 = v_n;
  u_value t1771 = NULL;
  if ((!t1771) && (u_is_con(s1770, &n_syntax_46_Name) && u_is_con(u_field(s1770, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1770, 0), 0);
    u_value v_b = u_field(s1770, 1);
    static struct u_object l_const_32_struct_32_u_n_1772 = U_LIT("const struct u_name");
    static struct u_object l_n__1773 = U_LIT("n_");
    t1771 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1772), u_concat(&l_n__1773, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1771) && (u_is_con(s1770, &n_syntax_46_Name) && u_is_con(u_field(s1770, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1770, 1);
    static struct u_object l_const_32_struct_32_u_n_1774 = U_LIT("const struct u_name");
    static struct u_object l_n__1775 = U_LIT("n_");
    static struct u_object l_NULL_1776 = U_LIT("NULL");
    t1771 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1774), u_concat(&l_n__1775, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1776), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1771) {
    u_no_case(s1770);
  }
  return t1771;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1777 = v_cs;
  u_value t1778 = NULL;
  if ((!t1778) && u_is_con(s1777, &n_list_46_Nil)) {
    t1778 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1778) && (u_is_con(s1777, &n_list_46_Cons) && u_is_con(u_field(s1777, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1777, 0), 2);
    u_value v_rest = u_field(s1777, 1);
    u_value t1779 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1780 = &t1779;
    u_value l1781 = v_ks;
    while (u_is_con(l1781, &n_list_46_Cons)) {
      u_value v_k = u_field(l1781, 0);
      if (u_truth(u_mk_bool(1))) {
        e1780 = u_append(e1780, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1781 = u_field(l1781, 1);
    }
    t1778 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1779, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1778) && u_is_con(s1777, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1777, 1);
    t1778 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1778) {
    u_no_case(s1777);
  }
  return t1778;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1782 = v_ks;
  u_value t1783 = NULL;
  if ((!t1783) && u_is_con(s1782, &n_list_46_Nil)) {
    t1783 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1783) && u_is_con(s1782, &n_list_46_Cons)) {
    u_value v_k = u_field(s1782, 0);
    u_value v_rest = u_field(s1782, 1);
    u_value t1784;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1784 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1784 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1783 = t1784;
  }
  if (!t1783) {
    u_no_case(s1782);
  }
  return t1783;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1785 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1786 = &t1785;
  u_value l1787 = v_xs;
  while (u_is_con(l1787, &n_list_46_Cons)) {
    u_value v_x = u_field(l1787, 0);
    if (u_truth(u_mk_bool(1))) {
      e1786 = u_append(e1786, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1787 = u_field(l1787, 1);
  }
  return t1785;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1788 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1788, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1789 = f_list_46_lookup_2(v_x, v_env);
  u_value t1790 = NULL;
  if ((!t1790) && u_is_con(s1789, &n_option_46_Some)) {
    u_value v_c = u_field(s1789, 0);
    t1790 = v_c;
  }
  if (!t1790) {
    t1790 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1790) {
    u_no_case(s1789);
  }
  return t1790;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1791 = v_c;
  u_value t1792 = NULL;
  if ((!t1792) && (u_is_con(s1791, &n_c_46_CeUnary) && u_equals(u_field(s1791, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1791, 1);
    t1792 = v_p;
  }
  if (!t1792) {
    static struct u_object l__38__1793 = U_LIT("&");
    t1792 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1793, v_c);
  }
  if (!t1792) {
    u_no_case(s1791);
  }
  return t1792;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1794 = v_t;
  u_value t1795 = NULL;
  if ((!t1795) && (u_is_con(s1794, &n_syntax_46_TSort) && u_is_con(u_field(s1794, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1794, 0);
    t1795 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1795) && u_is_con(s1794, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1794, 0);
    u_value v_args = u_field(s1794, 1);
    static struct u_object l__91__1796 = U_LIT("[");
    static struct u_object l__44__32__1797 = U_LIT(", ");
    u_value t1798 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1799 = &t1798;
    u_value l1800 = v_args;
    while (u_is_con(l1800, &n_list_46_Cons)) {
      u_value v_a = u_field(l1800, 0);
      if (u_truth(u_mk_bool(1))) {
        e1799 = u_append(e1799, f_compile_46_type_key_1(v_a));
      }
      l1800 = u_field(l1800, 1);
    }
    static struct u_object l__93__1801 = U_LIT("]");
    t1795 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1796, u_concat(u_str_join(&l__44__32__1797, t1798), &l__93__1801)));
  }
  if ((!t1795) && u_is_con(s1794, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1794, 0);
    u_value v_r = u_field(s1794, 1);
    static struct u_object l__40__1802 = U_LIT("(");
    static struct u_object l__44__32__1803 = U_LIT(", ");
    u_value t1804 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1805 = &t1804;
    u_value l1806 = v_args;
    while (u_is_con(l1806, &n_list_46_Cons)) {
      u_value v_a = u_field(l1806, 0);
      if (u_truth(u_mk_bool(1))) {
        e1805 = u_append(e1805, f_compile_46_type_key_1(v_a));
      }
      l1806 = u_field(l1806, 1);
    }
    static struct u_object l__41__32__45__62__32__1807 = U_LIT(") -> ");
    t1795 = u_concat(&l__40__1802, u_concat(u_str_join(&l__44__32__1803, t1804), u_concat(&l__41__32__45__62__32__1807, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1795) && u_is_con(s1794, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1794, 0);
    static struct u_object l__40__1808 = U_LIT("(");
    static struct u_object l__32__42__32__1809 = U_LIT(" * ");
    u_value t1810 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1811 = &t1810;
    u_value l1812 = v_ts;
    while (u_is_con(l1812, &n_list_46_Cons)) {
      u_value v_a = u_field(l1812, 0);
      if (u_truth(u_mk_bool(1))) {
        e1811 = u_append(e1811, f_compile_46_type_key_1(v_a));
      }
      l1812 = u_field(l1812, 1);
    }
    static struct u_object l__41__1813 = U_LIT(")");
    t1795 = u_concat(&l__40__1808, u_concat(u_str_join(&l__32__42__32__1809, t1810), &l__41__1813));
  }
  if (!t1795) {
    static struct u_object l__63__1814 = U_LIT("?");
    t1795 = &l__63__1814;
  }
  if (!t1795) {
    u_no_case(s1794);
  }
  return t1795;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1815 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1816 = &t1815;
  u_value l1817 = v_cs;
  while (u_is_con(l1817, &n_list_46_Cons)) {
    u_value v_c = u_field(l1817, 0);
    if (u_truth(u_mk_bool(1))) {
      e1816 = u_append(e1816, f_compile_46_fun_arities_1(v_c));
    }
    l1817 = u_field(l1817, 1);
  }
  u_value t1818 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1819 = &t1818;
  u_value l1820 = v_cs;
  while (u_is_con(l1820, &n_list_46_Cons)) {
    u_value v_c = u_field(l1820, 0);
    if (u_truth(u_mk_bool(1))) {
      e1819 = u_append(e1819, f_compile_46_constr_arities_1(v_c));
    }
    l1820 = u_field(l1820, 1);
  }
  u_value t1822 = f_list_46_concat_1(t1815);
  u_value t1823 = f_list_46_concat_1(t1818);
  u_value t1821;
  {
    u_value v_fs = t1822;
    u_value v_ks = t1823;
    t1821 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1821;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1824 = v_fs;
  u_value t1825 = NULL;
  if ((!t1825) && u_is_con(s1824, &n_list_46_Nil)) {
    t1825 = v_m;
  }
  if ((!t1825) && u_is_con(s1824, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1824, 0), 0);
    u_value v_k = u_field(u_field(s1824, 0), 1);
    u_value v_rest = u_field(s1824, 1);
    u_value s1826 = f_map_46_get_2(v_f, v_m);
    u_value t1827 = NULL;
    if ((!t1827) && u_is_con(s1826, &n_option_46_Some)) {
      u_value v_ks = u_field(s1826, 0);
      t1827 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1827) && u_is_con(s1826, &n_option_46_None)) {
      t1827 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1827) {
      u_no_case(s1826);
    }
    t1825 = t1827;
  }
  if (!t1825) {
    u_no_case(s1824);
  }
  return t1825;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1828 = v_gl;
  u_value t1829 = NULL;
  if ((!t1829) && u_is_con(s1828, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1828, 2);
    u_value s1830 = f_map_46_get_2(v_n, v_fm);
    u_value t1831 = NULL;
    if ((!t1831) && u_is_con(s1830, &n_option_46_Some)) {
      u_value v_ks = u_field(s1830, 0);
      t1831 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1831) && u_is_con(s1830, &n_option_46_None)) {
      t1831 = u_mk_bool(0);
    }
    if (!t1831) {
      u_no_case(s1830);
    }
    t1829 = t1831;
  }
  if (!t1829) {
    u_no_case(s1828);
  }
  return t1829;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1832 = v_gl;
  u_value t1833 = NULL;
  if ((!t1833) && u_is_con(s1832, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1832, 3);
    t1833 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1833) {
    u_no_case(s1832);
  }
  return t1833;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1834 = v_c;
  u_value t1835 = NULL;
  if ((!t1835) && u_is_con(s1834, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1834, 0);
    u_value v_xss = u_field(s1834, 2);
    t1835 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1835) {
    t1835 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1835) {
    u_no_case(s1834);
  }
  return t1835;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1836 = v_c;
  u_value t1837 = NULL;
  if ((!t1837) && u_is_con(s1836, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1836, 2);
    u_value t1838 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1839 = &t1838;
    u_value l1840 = v_ks;
    while (u_is_con(l1840, &n_list_46_Cons)) {
      u_value v_k = u_field(l1840, 0);
      if (u_truth(u_mk_bool(1))) {
        e1839 = u_append(e1839, f_compile_46_constr_arity_1(v_k));
      }
      l1840 = u_field(l1840, 1);
    }
    t1837 = t1838;
  }
  if (!t1837) {
    t1837 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1837) {
    u_no_case(s1836);
  }
  return t1837;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1841 = v_k;
  u_value t1842 = NULL;
  if ((!t1842) && u_is_con(s1841, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1841, 0);
    u_value v_ts = u_field(s1841, 1);
    t1842 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1842) {
    u_no_case(s1841);
  }
  return t1842;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1843 = v_xs;
  u_value t1844 = NULL;
  if ((!t1844) && u_is_con(s1843, &n_list_46_Nil)) {
    t1844 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1844) && u_is_con(s1843, &n_list_46_Cons)) {
    u_value v_x = u_field(s1843, 0);
    u_value v_rest = u_field(s1843, 1);
    t1844 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1844) {
    u_no_case(s1843);
  }
  return t1844;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1845 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1845, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1846 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1846, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1847 = v_cs;
  u_value t1848 = NULL;
  if ((!t1848) && u_is_con(s1847, &n_list_46_Nil)) {
    t1848 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1848) && (u_is_con(s1847, &n_list_46_Cons) && u_is_con(u_field(s1847, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1847, 0);
    t1848 = v_d;
  }
  if ((!t1848) && u_is_con(s1847, &n_list_46_Cons)) {
    u_value v_d = u_field(s1847, 0);
    u_value v_rest = u_field(s1847, 1);
    static struct u_object l__38__38__1849 = U_LIT("&&");
    t1848 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1849, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1848) {
    u_no_case(s1847);
  }
  return t1848;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1850 = v_l;
  u_value t1851 = NULL;
  if ((!t1851) && u_is_con(s1850, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1852 = U_LIT("u_mk_tuple");
    t1851 = f_c_46_c_call_2(&l_u_mk_tuple_1852, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1851) && u_is_con(s1850, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1850, 0);
    static struct u_object l_u_mk_bool_1853 = U_LIT("u_mk_bool");
    u_value t1854;
    if (u_truth(v_b)) {
      t1854 = u_mk_int(1);
    } else {
      t1854 = u_mk_int(0);
    }
    t1851 = f_c_46_c_call_2(&l_u_mk_bool_1853, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1854), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1851) && u_is_con(s1850, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1850, 0);
    u_value t1855 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1855)) {
      t1855 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1858;
    if (u_truth(t1855)) {
      static struct u_object l_u_mk_int_1856 = U_LIT("u_mk_int");
      t1858 = f_c_46_c_call_2(&l_u_mk_int_1856, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1857 = U_LIT("u_mk_int_str");
      t1858 = f_c_46_c_call_2(&l_u_mk_int_str_1857, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1851 = t1858;
  }
  if ((!t1851) && u_is_con(s1850, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1850, 0);
    static struct u_object l_u_mk_lit_1859 = U_LIT("u_mk_lit");
    t1851 = f_c_46_c_call_2(&l_u_mk_lit_1859, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1851) && u_is_con(s1850, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1850, 0);
    static struct u_object l_u_mk_char_1860 = U_LIT("u_mk_char");
    t1851 = f_c_46_c_call_2(&l_u_mk_char_1860, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1851) {
    u_no_case(s1850);
  }
  return t1851;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1861 = v_p;
  u_value t1862 = NULL;
  if ((!t1862) && u_is_con(s1861, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1861, 0);
    static struct u_object l_u_equals_1863 = U_LIT("u_equals");
    t1862 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1863, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1862) && u_is_con(s1861, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1861, 0);
    u_value v_ps = u_field(s1861, 1);
    static struct u_object l_u_is_con_1864 = U_LIT("u_is_con");
    t1862 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1864, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1862) && u_is_con(s1861, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1861, 0);
    t1862 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1862) {
    t1862 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1862) {
    u_no_case(s1861);
  }
  return t1862;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1865 = v_ps;
  u_value t1866 = NULL;
  if ((!t1866) && u_is_con(s1865, &n_list_46_Nil)) {
    t1866 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1866) && u_is_con(s1865, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1865, 0), 0);
    u_value v_i = u_field(u_field(s1865, 0), 1);
    u_value v_rest = u_field(s1865, 1);
    t1866 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1866) {
    u_no_case(s1865);
  }
  return t1866;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1867 = v_p;
  u_value t1868 = NULL;
  if ((!t1868) && (u_is_con(s1867, &n_syntax_46_PVar) && u_is_con(u_field(s1867, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1867, 0), 0);
    t1868 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1868) && u_is_con(s1867, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1867, 1);
    t1868 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1868) && u_is_con(s1867, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1867, 0);
    t1868 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1868) {
    t1868 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1868) {
    u_no_case(s1867);
  }
  return t1868;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1869 = v_ps;
  u_value t1870 = NULL;
  if ((!t1870) && u_is_con(s1869, &n_list_46_Nil)) {
    t1870 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1870) && u_is_con(s1869, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1869, 0), 0);
    u_value v_i = u_field(u_field(s1869, 0), 1);
    u_value v_rest = u_field(s1869, 1);
    t1870 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1870) {
    u_no_case(s1869);
  }
  return t1870;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1871 = v_p;
  u_value t1872 = NULL;
  if ((!t1872) && (u_is_con(s1871, &n_syntax_46_PVar) && u_is_con(u_field(s1871, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1871, 0), 0);
    t1872 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1872) && u_is_con(s1871, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1871, 1);
    u_value t1873 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1874 = &t1873;
    u_value l1875 = v_ps;
    while (u_is_con(l1875, &n_list_46_Cons)) {
      u_value v_q = u_field(l1875, 0);
      if (u_truth(u_mk_bool(1))) {
        e1874 = u_append(e1874, f_compile_46_pattern_names_1(v_q));
      }
      l1875 = u_field(l1875, 1);
    }
    t1872 = f_list_46_concat_1(t1873);
  }
  if ((!t1872) && u_is_con(s1871, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1871, 0);
    u_value t1876 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1877 = &t1876;
    u_value l1878 = v_ps;
    while (u_is_con(l1878, &n_list_46_Cons)) {
      u_value v_q = u_field(l1878, 0);
      if (u_truth(u_mk_bool(1))) {
        e1877 = u_append(e1877, f_compile_46_pattern_names_1(v_q));
      }
      l1878 = u_field(l1878, 1);
    }
    t1872 = f_list_46_concat_1(t1876);
  }
  if (!t1872) {
    t1872 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1872) {
    u_no_case(s1871);
  }
  return t1872;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1879 = v_e;
  u_value t1880 = NULL;
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_ELit)) {
    t1880 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1880) && (u_is_con(s1879, &n_syntax_46_EVar) && u_is_con(u_field(s1879, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1879, 0), 0);
    u_value t1881;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1881 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1881 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1880 = t1881;
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1879, 0);
    u_value v_args = u_field(s1879, 1);
    u_value t1882 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1883 = &t1882;
    u_value l1884 = v_args;
    while (u_is_con(l1884, &n_list_46_Cons)) {
      u_value v_a = u_field(l1884, 0);
      if (u_truth(u_mk_bool(1))) {
        e1883 = u_append(e1883, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1884 = u_field(l1884, 1);
    }
    t1880 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1882));
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1879, 0);
    u_value v_b = u_field(s1879, 1);
    t1880 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1879, 0);
    u_value v_b = u_field(s1879, 1);
    u_value t1885 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1886 = &t1885;
    u_value l1887 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1887, &n_list_46_Cons)) {
      u_value v_a = u_field(l1887, 0);
      if (u_truth(u_mk_bool(1))) {
        e1886 = u_append(e1886, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1887 = u_field(l1887, 1);
    }
    t1880 = f_list_46__43__43__2(f_list_46_concat_1(t1885), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1879, 0);
    u_value v_l = u_field(s1879, 1);
    u_value v_r = u_field(s1879, 2);
    t1880 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1879, 0);
    u_value v_cs = u_field(s1879, 1);
    u_value t1888 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1889 = &t1888;
    u_value l1890 = v_cs;
    while (u_is_con(l1890, &n_list_46_Cons)) {
      u_value v_b = u_field(l1890, 0);
      if (u_truth(u_mk_bool(1))) {
        e1889 = u_append(e1889, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1890 = u_field(l1890, 1);
    }
    t1880 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1888));
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1879, 0);
    u_value v_xs = u_field(s1879, 1);
    u_value v_ls = u_field(s1879, 2);
    u_value v_c = u_field(s1879, 3);
    u_value t1891 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1892 = &t1891;
    u_value l1893 = v_ls;
    while (u_is_con(l1893, &n_list_46_Cons)) {
      u_value v_l = u_field(l1893, 0);
      if (u_truth(u_mk_bool(1))) {
        e1892 = u_append(e1892, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1893 = u_field(l1893, 1);
    }
    t1880 = f_list_46__43__43__2(f_list_46_concat_1(t1891), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1879, 0);
    u_value t1894 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1895 = &t1894;
    u_value l1896 = v_es;
    while (u_is_con(l1896, &n_list_46_Cons)) {
      u_value v_a = u_field(l1896, 0);
      if (u_truth(u_mk_bool(1))) {
        e1895 = u_append(e1895, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1896 = u_field(l1896, 1);
    }
    t1880 = f_list_46_concat_1(t1894);
  }
  if ((!t1880) && u_is_con(s1879, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s1879, 0);
    t1880 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t1880) {
    u_no_case(s1879);
  }
  return t1880;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1897 = v_b;
  u_value t1898 = NULL;
  if ((!t1898) && (u_is_con(s1897, &n_syntax_46_Branch) && u_is_con(u_field(s1897, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1897, 0);
    u_value v_g = u_field(u_field(s1897, 1), 0);
    u_value v_e = u_field(s1897, 2);
    t1898 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1898) && (u_is_con(s1897, &n_syntax_46_Branch) && u_is_con(u_field(s1897, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1897, 0);
    u_value v_e = u_field(s1897, 2);
    t1898 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1898) {
    u_no_case(s1897);
  }
  return t1898;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1899 = v_xss;
  u_value t1900 = NULL;
  if ((!t1900) && u_is_con(s1899, &n_list_46_Nil)) {
    t1900 = v_b;
  }
  if ((!t1900) && u_is_con(s1899, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1899, 0);
    u_value v_rest = u_field(s1899, 1);
    t1900 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1900) {
    u_no_case(s1899);
  }
  return t1900;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1901 = *r_g;
    int d1902 = 0;
    if ((!d1902) && u_is_con(s1901, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1901, 0);
      u_value v_ds = u_field(s1901, 1);
      d1902 = 1;
      {
        u_value t1903 = u_concat(v_stem, u_str_show(v_n));
        u_value t1904 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1903;
        *r_g = t1904;
      }
    }
    if (!d1902) {
      u_no_case(s1901);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1905 = *r_g;
    int d1906 = 0;
    if ((!d1906) && u_is_con(s1905, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1905, 0);
      u_value v_ds = u_field(s1905, 1);
      d1906 = 1;
      {
        u_value t1907 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1907;
      }
    }
    if (!d1906) {
      u_no_case(s1905);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1908 = U_LIT("u_mk_closure");
        static struct u_object l_w__1909 = U_LIT("w_");
        u_value t1910 = f_c_46_c_call_2(&l_u_mk_closure_1908, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1909, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1910;
      }
    } else {
      {
        u_value s1911 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1912 = 0;
        if ((!d1912) && (u_is_con(s1911, &n_option_46_Some) && u_equals(u_field(s1911, 0), u_mk_int(0)))) {
          d1912 = 1;
          {
            static struct u_object l_u_mk_con_1913 = U_LIT("u_mk_con");
            u_value t1914 = f_c_46_c_call_2(&l_u_mk_con_1913, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1914;
          }
        }
        if ((!d1912) && u_is_con(s1911, &n_option_46_Some)) {
          d1912 = 1;
          {
            static struct u_object l_u_mk_closure_1915 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1916 = U_LIT("wc_");
            u_value t1917 = f_c_46_c_call_2(&l_u_mk_closure_1915, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1916, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1917;
          }
        }
        if ((!d1912) && u_is_con(s1911, &n_option_46_None)) {
          d1912 = 1;
          {
            u_value s1918 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1919 = 0;
            if ((!d1919) && u_is_con(s1918, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1918, 0), 0);
              d1919 = 1;
              {
                static struct u_object l_u_mk_closure_1920 = U_LIT("u_mk_closure");
                static struct u_object l_w_1921 = U_LIT("w");
                u_value t1922 = f_c_46_c_call_2(&l_u_mk_closure_1920, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1921, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1922;
              }
            }
            if ((!d1919) && u_is_con(s1918, &n_option_46_None)) {
              d1919 = 1;
              {
                static struct u_object l_the_32_variable_32__1923 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1923, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1919) {
              u_no_case(s1918);
            }
          }
        }
        if (!d1912) {
          u_no_case(s1911);
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
    u_value s1924 = v_e;
    int d1925 = 0;
    if ((!d1925) && (u_is_con(s1924, &n_syntax_46_ELit) && u_is_con(u_field(s1924, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1924, 0), 0);
      d1925 = 1;
      {
        static struct u_object l_l__1926 = U_LIT("l_");
        u_value t1927;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t1927 = u_str_size(v_s);
        } else {
          t1927 = u_mk_int(16);
        }
        static struct u_object l___1928 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1926, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t1927))), &l___1928)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1929 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1930 = U_LIT("U_LIT");
        static struct u_object l__38__1931 = U_LIT("&");
        u_value t1932 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1929), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1930, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1933 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1931, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1932;
        *r_r = t1933;
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1924, 0);
      d1925 = 1;
      {
        u_value t1934 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1935 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1934;
        *r_r = t1935;
      }
    }
    if ((!d1925) && (u_is_con(s1924, &n_syntax_46_EVar) && u_is_con(u_field(s1924, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1924, 0), 0);
      u_value v_ty = u_field(u_field(s1924, 0), 1);
      d1925 = 1;
      {
        u_value s1936 = f_list_46_lookup_2(v_n, v_env);
        int d1937 = 0;
        if ((!d1937) && u_is_con(s1936, &n_option_46_Some)) {
          u_value v_c = u_field(s1936, 0);
          d1937 = 1;
          {
            u_value t1938 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1939 = v_c;
            *r_ss = t1938;
            *r_r = t1939;
          }
        }
        if ((!d1937) && u_is_con(s1936, &n_option_46_None)) {
          d1937 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1940 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1940;
          }
        }
        if (!d1937) {
          u_no_case(s1936);
        }
      }
    }
    if ((!d1925) && (u_is_con(s1924, &n_syntax_46_EApp) && (u_is_con(u_field(s1924, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1924, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1924, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1924, 0), 0), 1);
      u_value v_args = u_field(s1924, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1925 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1924, 0);
      u_value v_args = u_field(s1924, 1);
      d1925 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1941 = U_LIT("u_apply");
        u_value t1942 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1943 = f_c_46_c_call_2(&l_u_apply_1941, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1942;
        *r_r = t1943;
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1924, 0);
      u_value v_b = u_field(s1924, 1);
      d1925 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1924, 0);
      u_value v_b = u_field(s1924, 1);
      d1925 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1944 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1944, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1945 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1945;
        u_value t1946 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1946;
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1924, 0);
      u_value v_l = u_field(s1924, 1);
      u_value v_rr = u_field(s1924, 2);
      d1925 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1947 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1947, &v_t, r_g);
        u_value t1948 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1948;
        u_value t1949 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1949;
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1924, 0);
      u_value v_cases = u_field(s1924, 1);
      d1925 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1950 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1950, &v_d, r_g);
        static struct u_object l_t_1951 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1951, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1952 = U_LIT("NULL");
        u_value t1953 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1952))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1953;
        static struct u_object l__33__1954 = U_LIT("!");
        static struct u_object l_u_no_case_1955 = U_LIT("u_no_case");
        u_value t1956 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1954, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1955, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1956;
        u_value t1957 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1957;
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1924, 0);
      u_value v_xs = u_field(s1924, 1);
      u_value v_lists = u_field(s1924, 2);
      u_value v_c = u_field(s1924, 3);
      d1925 = 1;
      {
        static struct u_object l_t_1958 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1958, &v_t, r_g);
        static struct u_object l_e_1959 = U_LIT("e");
        p_compile_46_gen_name(&l_e_1959, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_1960 = U_LIT("u_mk_con");
        static struct u_object l__38__1961 = U_LIT("&");
        u_value t1962 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_1960, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__1961, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t1962;
        u_value t1963 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1963;
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1924, 0);
      d1925 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_1964 = U_LIT("u_mk_tuple");
        u_value t1965 = f_c_46_c_call_2(&l_u_mk_tuple_1964, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t1965;
      }
    }
    if ((!d1925) && u_is_con(s1924, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s1924, 0);
      u_value v_ty = u_field(s1924, 1);
      d1925 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_1966 = U_LIT("u_raise");
        static struct u_object l_NULL_1967 = U_LIT("NULL");
        u_value t1968 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_1966, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t1969 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1967);
        *r_ss = t1968;
        *r_r = t1969;
      }
    }
    if (!d1925) {
      u_no_case(s1924);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s1970 = v_es;
    int d1971 = 0;
    if ((!d1971) && u_is_con(s1970, &n_list_46_Nil)) {
      d1971 = 1;
      {
        u_value t1972 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1973 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1972;
        *r_rs = t1973;
      }
    }
    if ((!d1971) && u_is_con(s1970, &n_list_46_Cons)) {
      u_value v_e = u_field(s1970, 0);
      u_value v_rest = u_field(s1970, 1);
      d1971 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t1974 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1975 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t1974;
        *r_rs = t1975;
      }
    }
    if (!d1971) {
      u_no_case(s1970);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s1976 = v_rs;
    int d1977 = 0;
    if ((!d1977) && u_is_con(s1976, &n_list_46_Nil)) {
      d1977 = 1;
      {
        u_value t1978 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t1978;
      }
    }
    if ((!d1977) && u_is_con(s1976, &n_list_46_Cons)) {
      u_value v_r = u_field(s1976, 0);
      u_value v_rest = u_field(s1976, 1);
      d1977 = 1;
      {
        static struct u_object l_t_1979 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1979, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t1980 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t1980;
      }
    }
    if (!d1977) {
      u_no_case(s1976);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s1981 = u_mk_tuple(2, v_xs, v_ts);
  u_value t1982 = NULL;
  if ((!t1982) && (u_is_con(u_field(s1981, 0), &n_list_46_Cons) && (u_is_con(u_field(s1981, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1981, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s1981, 0), 0);
    u_value v_more = u_field(u_field(s1981, 0), 1);
    u_value v_t = u_field(u_field(u_field(s1981, 1), 0), 1);
    u_value v_rest = u_field(u_field(s1981, 1), 1);
    t1982 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t1982) {
    t1982 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1982) {
    u_no_case(s1981);
  }
  return t1982;
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
    u_value s1983 = v_args;
    int d1984 = 0;
    if ((!d1984) && (u_is_con(s1983, &n_list_46_Cons) && (u_is_con(u_field(s1983, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s1983, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s1983, 0);
      u_value v_b = u_field(u_field(s1983, 1), 0);
      static struct u_object l_and_1991 = U_LIT("and");
      u_value t1993 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1991));
      if (!u_truth(t1993)) {
        static struct u_object l_or_1992 = U_LIT("or");
        t1993 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_1992));
      }
      if (u_truth(t1993)) {
        d1984 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_1985 = U_LIT("t");
          p_compile_46_gen_name(&l_t_1985, &v_t, r_g);
          static struct u_object l_and_1986 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_1986)))) {
            {
              u_value t1987 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1987;
            }
          } else {
            {
              static struct u_object l__33__1988 = U_LIT("!");
              u_value t1989 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1988, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t1989;
            }
          }
          u_value t1990 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t1990;
        }
      }
    }
    if (!d1984) {
      d1984 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t1994 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t1994)) {
          t1994 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t1994)) {
          {
            u_value t1995 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t1995;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_1996 = U_LIT("u_mk_con");
              u_value t1997 = f_c_46_c_call_2(&l_u_mk_con_1996, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t1997;
            }
          } else {
            {
              u_value s1998 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d1999 = 0;
              if ((!d1999) && u_is_con(s1998, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s1998, 0), 0);
                u_value v_k = u_field(u_field(s1998, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d1999 = 1;
                  {
                    u_value t2000 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2000;
                  }
                }
              }
              if (!d1999) {
                d1999 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2001 = U_LIT("u_apply");
                  u_value t2002 = f_c_46_c_call_2(&l_u_apply_2001, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2002;
                }
              }
              if (!d1999) {
                u_no_case(s1998);
              }
            }
          }
        }
      }
    }
    if (!d1984) {
      u_no_case(s1983);
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
    u_value t2003 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2004 = &t2003;
    u_value l2005 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2005, &n_list_46_Cons)) {
      u_value v_x = u_field(l2005, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2004 = u_append(e2004, v_x);
      }
      l2005 = u_field(l2005, 1);
    }
    u_value t2006 = t2003;
    v_caps = t2006;
    static struct u_object l_lam_2007 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2007, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2008 = U_LIT("env");
    static struct u_object l_args_2009 = U_LIT("args");
    u_value t2010 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2008), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2009));
    v_decls = t2010;
    static struct u_object l_env_2011 = U_LIT("env");
    static struct u_object l_args_2012 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2011), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2012), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2013 = U_LIT("u_mk_closure");
    u_value t2014 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2015 = &t2014;
    u_value l2016 = v_caps;
    while (u_is_con(l2016, &n_list_46_Cons)) {
      u_value v_x = u_field(l2016, 0);
      if (u_truth(u_mk_bool(1))) {
        e2015 = u_append(e2015, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2016 = u_field(l2016, 1);
    }
    u_value t2017 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2018 = f_c_46_c_call_2(&l_u_mk_closure_2013, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2014));
    *r_ss = t2017;
    *r_r = t2018;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2019 = v_xs;
  u_value t2020 = NULL;
  if ((!t2020) && u_is_con(s2019, &n_list_46_Nil)) {
    t2020 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2020) && u_is_con(s2019, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2019, 0), 0);
    u_value v_i = u_field(u_field(s2019, 0), 1);
    u_value v_rest = u_field(s2019, 1);
    t2020 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2020) {
    u_no_case(s2019);
  }
  return t2020;
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
    u_value s2021 = v_cases;
    int d2022 = 0;
    if ((!d2022) && u_is_con(s2021, &n_list_46_Nil)) {
      d2022 = 1;
      {
        u_value t2023 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2023;
      }
    }
    if ((!d2022) && (u_is_con(s2021, &n_list_46_Cons) && u_is_con(u_field(s2021, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2021, 0), 0);
      u_value v_guard = u_field(u_field(s2021, 0), 1);
      u_value v_e = u_field(u_field(s2021, 0), 2);
      u_value v_more = u_field(s2021, 1);
      d2022 = 1;
      {
        u_value t2024 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2024;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2025 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2025;
        u_value s2026 = v_guard;
        int d2027 = 0;
        if ((!d2027) && u_is_con(s2026, &n_option_46_Some)) {
          u_value v_c = u_field(s2026, 0);
          d2027 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2028 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2028;
          }
        }
        if ((!d2027) && u_is_con(s2026, &n_option_46_None)) {
          d2027 = 1;
          {
          }
        }
        if (!d2027) {
          u_no_case(s2026);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2029 = U_LIT("!");
        u_value t2030 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2029, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2030;
      }
    }
    if (!d2022) {
      u_no_case(s2021);
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
    u_value s2031 = u_mk_tuple(2, v_xs, v_lists);
    int d2032 = 0;
    if ((!d2032) && (u_is_con(u_field(s2031, 0), &n_list_46_Cons) && u_is_con(u_field(s2031, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2031, 0), 0);
      u_value v_more = u_field(u_field(s2031, 0), 1);
      u_value v_list = u_field(u_field(s2031, 1), 0);
      u_value v_others = u_field(u_field(s2031, 1), 1);
      d2032 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2033 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2033, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2034 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2034;
        static struct u_object l_u_is_con_2035 = U_LIT("u_is_con");
        u_value t2036 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2035, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2036;
      }
    }
    if (!d2032) {
      d2032 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2037 = U_LIT("u_append");
        u_value t2038 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2037, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2038;
      }
    }
    if (!d2032) {
      u_no_case(s2031);
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
    u_value s2039 = v_s;
    int d2040 = 0;
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SSkip)) {
      d2040 = 1;
      {
        u_value t2041 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2041;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2039, 0);
      d2040 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2042 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2042;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2039, 0);
      u_value v_body = u_field(s2039, 1);
      d2040 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2043 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2044 = &t2043;
        u_value l2045 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2045, &n_list_46_Cons)) {
          u_value v_x = u_field(l2045, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2046 = U_LIT("NULL");
            e2044 = u_append(e2044, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2046))));
          }
          l2045 = u_field(l2045, 1);
        }
        u_value t2047 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2043, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2047;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2039, 0);
      u_value v_l = u_field(s2039, 1);
      u_value v_r = u_field(s2039, 2);
      d2040 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2048 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2048;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2039, 0);
      u_value v_b = u_field(s2039, 1);
      d2040 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2049 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2049, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2050 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2050;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2039, 0);
      u_value v_ins = u_field(s2039, 1);
      u_value v_outs = u_field(s2039, 2);
      u_value v_refs = u_field(s2039, 3);
      d2040 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2051 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2052 = &t2051;
        u_value l2053 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2053, &n_list_46_Cons)) {
          u_value v_x = u_field(l2053, 0);
          if (u_truth(u_mk_bool(1))) {
            e2052 = u_append(e2052, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2053 = u_field(l2053, 1);
        }
        u_value t2054 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2051))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2054;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2039, 0);
      u_value v_cases = u_field(s2039, 1);
      d2040 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2055 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2055, &v_d, r_g);
        static struct u_object l_d_2056 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2056, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2057 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2057;
        static struct u_object l__33__2058 = U_LIT("!");
        static struct u_object l_u_no_case_2059 = U_LIT("u_no_case");
        u_value t2060 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2058, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2059, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2060;
      }
    }
    if ((!d2040) && (u_is_con(s2039, &n_syntax_46_SAssert) && (u_is_con(u_field(s2039, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2039, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2039, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2039, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2039, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2039, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2039, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2039, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2039, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2039, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2039, 0), 1), 1), 0);
      d2040 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2061 = U_LIT("u_assert_equal");
        u_value t2062 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2061, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2062;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2039, 0);
      d2040 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2063 = U_LIT("u_assert_true");
        u_value t2064 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2063, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2064;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2039, 0);
      u_value v_ty = u_field(s2039, 1);
      d2040 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2065 = U_LIT("u_raise");
        u_value t2066 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2065, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2066;
      }
    }
    if ((!d2040) && u_is_con(s2039, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2039, 0);
      u_value v_hs = u_field(s2039, 1);
      d2040 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2067 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2067, &v_d, r_g);
        static struct u_object l_d_2068 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2068, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2069 = U_LIT("u_pop");
        static struct u_object l__33__2070 = U_LIT("!");
        static struct u_object l_u_reraise_2071 = U_LIT("u_reraise");
        u_value t2072 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2069, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2070, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2071, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2072;
        static struct u_object l_struct_32_u_handler_2073 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2074 = U_LIT("u_push");
        static struct u_object l__61__61__2075 = U_LIT("==");
        static struct u_object l_setjmp_2076 = U_LIT("setjmp");
        static struct u_object l_env_2077 = U_LIT("env");
        static struct u_object l_u_pop_2078 = U_LIT("u_pop");
        u_value t2079 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2073)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2074, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2075, f_c_46_c_call_2(&l_setjmp_2076, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2077), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2078, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2079;
      }
    }
    if (!d2040) {
      u_no_case(s2039);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2080 = v_xs;
    int d2081 = 0;
    if ((!d2081) && u_is_con(s2080, &n_list_46_Nil)) {
      d2081 = 1;
      {
        u_value t2082 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2082;
      }
    }
    if ((!d2081) && u_is_con(s2080, &n_list_46_Cons)) {
      u_value v_x = u_field(s2080, 0);
      u_value v_rest = u_field(s2080, 1);
      d2081 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2083 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2083;
      }
    }
    if (!d2081) {
      u_no_case(s2080);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2084 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2085 = NULL;
  if ((!t2085) && (u_is_con(u_field(s2084, 0), &n_list_46_Cons) && (u_is_con(u_field(s2084, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2084, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2084, 0), 0);
    u_value v_more = u_field(u_field(s2084, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2084, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2084, 1), 1);
    t2085 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2085) {
    t2085 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2085) {
    u_no_case(s2084);
  }
  return t2085;
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
    static struct u_object l_u_caught_2086 = U_LIT("u_caught");
    u_value t2087 = f_c_46_c_call_2(&l_u_caught_2086, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2087;
    u_value s2088 = v_hs;
    int d2089 = 0;
    if ((!d2089) && u_is_con(s2088, &n_list_46_Nil)) {
      d2089 = 1;
      {
        u_value t2090 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2090;
      }
    }
    if ((!d2089) && (u_is_con(s2088, &n_list_46_Cons) && u_is_con(u_field(s2088, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2088, 0), 0);
      u_value v_p = u_field(u_field(s2088, 0), 1);
      u_value v_guard = u_field(u_field(s2088, 0), 2);
      u_value v_s = u_field(u_field(s2088, 0), 3);
      u_value v_more = u_field(s2088, 1);
      d2089 = 1;
      {
        u_value t2091 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2091;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2092 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2092;
        u_value s2093 = v_guard;
        int d2094 = 0;
        if ((!d2094) && u_is_con(s2093, &n_option_46_Some)) {
          u_value v_c = u_field(s2093, 0);
          d2094 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2095 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2095;
          }
        }
        if ((!d2094) && u_is_con(s2093, &n_option_46_None)) {
          d2094 = 1;
          {
          }
        }
        if (!d2094) {
          u_no_case(s2093);
        }
        u_value s2096 = v_p;
        int d2097 = 0;
        if ((!d2097) && u_is_con(s2096, &n_syntax_46_PWild)) {
          d2097 = 1;
          {
            u_value t2098 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2098;
          }
        }
        if (!d2097) {
          d2097 = 1;
          {
            static struct u_object l_u_has_type_2099 = U_LIT("u_has_type");
            u_value t2100 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2099, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2100;
          }
        }
        if (!d2097) {
          u_no_case(s2096);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2101 = U_LIT("!");
        u_value t2102 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2101, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2102;
      }
    }
    if (!d2089) {
      u_no_case(s2088);
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
    u_value s2103 = v_cases;
    int d2104 = 0;
    if ((!d2104) && u_is_con(s2103, &n_list_46_Nil)) {
      d2104 = 1;
      {
        u_value t2105 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2105;
      }
    }
    if ((!d2104) && (u_is_con(s2103, &n_list_46_Cons) && u_is_con(u_field(s2103, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2103, 0), 0);
      u_value v_guard = u_field(u_field(s2103, 0), 1);
      u_value v_s = u_field(u_field(s2103, 0), 2);
      u_value v_more = u_field(s2103, 1);
      d2104 = 1;
      {
        u_value t2106 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2106;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2107 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2107;
        u_value s2108 = v_guard;
        int d2109 = 0;
        if ((!d2109) && u_is_con(s2108, &n_option_46_Some)) {
          u_value v_c = u_field(s2108, 0);
          d2109 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2110 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2110;
          }
        }
        if ((!d2109) && u_is_con(s2108, &n_option_46_None)) {
          d2109 = 1;
          {
          }
        }
        if (!d2109) {
          u_no_case(s2108);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2111 = U_LIT("!");
        u_value t2112 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2111, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2112;
      }
    }
    if (!d2104) {
      u_no_case(s2103);
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
    u_value s2113 = v_c;
    int d2114 = 0;
    if ((!d2114) && u_is_con(s2113, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2113, 0);
      u_value v_xss = u_field(s2113, 2);
      u_value v_b = u_field(s2113, 4);
      d2114 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2115 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2116 = &t2115;
        u_value l2117 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2117, &n_list_46_Cons)) {
          u_value v_x = u_field(l2117, 0);
          if (u_truth(u_mk_bool(1))) {
            e2116 = u_append(e2116, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2117 = u_field(l2117, 1);
        }
        u_value t2118 = t2115;
        v_params = t2118;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2119 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2119;
          }
        } else {
          {
            u_value t2120 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2120;
          }
        }
        u_value t2121 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2122 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2121;
        *r_ms = t2122;
      }
    }
    if ((!d2114) && u_is_con(s2113, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2113, 0);
      u_value v_ins = u_field(s2113, 2);
      u_value v_outs = u_field(s2113, 3);
      u_value v_refs = u_field(s2113, 4);
      u_value v_body = u_field(s2113, 5);
      d2114 = 1;
      {
        u_value t2123 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2124 = &t2123;
        u_value l2125 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2125, &n_list_46_Cons)) {
          u_value v_x = u_field(l2125, 0);
          if (u_truth(u_mk_bool(1))) {
            e2124 = u_append(e2124, f_compile_46_pointer_1(v_x));
          }
          l2125 = u_field(l2125, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2123), v_body, &v_bs, r_g);
        u_value t2126 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2127 = &t2126;
        u_value l2128 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2128, &n_list_46_Cons)) {
          u_value v_x = u_field(l2128, 0);
          if (u_truth(u_mk_bool(1))) {
            e2127 = u_append(e2127, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2128 = u_field(l2128, 1);
        }
        u_value t2129 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2130 = &t2129;
        u_value l2131 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2131, &n_list_46_Cons)) {
          u_value v_x = u_field(l2131, 0);
          if (u_truth(u_mk_bool(1))) {
            e2130 = u_append(e2130, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2131 = u_field(l2131, 1);
        }
        u_value t2132 = f_list_46__43__43__2(t2126, t2129);
        v_params = t2132;
        u_value t2133 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2134 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2133;
        *r_ms = t2134;
      }
    }
    if ((!d2114) && u_is_con(s2113, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2113, 0);
      d2114 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2135 = U_LIT("u_print_line");
        u_value t2136 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2137 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2135, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2136;
        *r_ms = t2137;
      }
    }
    if ((!d2114) && (u_is_con(s2113, &n_syntax_46_CRun) && u_is_con(u_field(s2113, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2113, 1), 0);
      u_value v_body = u_field(u_field(s2113, 1), 1);
      d2114 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2138 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2139 = &t2138;
        u_value l2140 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2140, &n_list_46_Cons)) {
          u_value v_x = u_field(l2140, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2141 = U_LIT("NULL");
            e2139 = u_append(e2139, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2141))));
          }
          l2140 = u_field(l2140, 1);
        }
        u_value t2142 = t2138;
        v_copy = t2142;
        u_value t2143 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2144 = &t2143;
        u_value l2145 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2145, &n_list_46_Cons)) {
          u_value v_x = u_field(l2145, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2146 = U_LIT("u_print_binding");
            e2144 = u_append(e2144, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2146, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2145 = u_field(l2145, 1);
        }
        u_value t2147 = t2143;
        v_back = t2147;
        u_value t2148 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2149 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2148;
        *r_ms = t2149;
      }
    }
    if ((!d2114) && u_is_con(s2113, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2113, 1);
      d2114 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2150 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2151 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2150;
        *r_ms = t2151;
      }
    }
    if ((!d2114) && u_is_con(s2113, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2113, 1);
      d2114 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2152 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2153 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2152;
        *r_ms = t2153;
      }
    }
    if (!d2114) {
      d2114 = 1;
      {
        u_value t2154 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2155 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2154;
        *r_ms = t2155;
      }
    }
    if (!d2114) {
      u_no_case(s2113);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2156 = v_cs;
    int d2157 = 0;
    if ((!d2157) && u_is_con(s2156, &n_list_46_Nil)) {
      d2157 = 1;
      {
        u_value t2158 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2159 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2158;
        *r_ms = t2159;
      }
    }
    if ((!d2157) && u_is_con(s2156, &n_list_46_Cons)) {
      u_value v_c = u_field(s2156, 0);
      u_value v_rest = u_field(s2156, 1);
      d2157 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2160 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2161 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2160;
        *r_ms = t2161;
      }
    }
    if (!d2157) {
      u_no_case(s2156);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2162 = U_LIT("handler");
  u_value t2189 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2162);
  u_value t2188;
  {
    u_value v_h = t2189;
    static struct u_object l_static_32_u_value_2163 = U_LIT("static u_value");
    static struct u_object l_cache_2164 = U_LIT("cache");
    static struct u_object l_NULL_2165 = U_LIT("NULL");
    static struct u_object l__61__61__2166 = U_LIT("==");
    static struct u_object l_cache_2167 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2168 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2169 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2170 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2171 = U_LIT("cache");
    static struct u_object l_cache_2172 = U_LIT("cache");
    static struct u_object l_cache_2173 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2174 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2175 = U_LIT("struct u_handler");
    static struct u_object l_handler_2176 = U_LIT("handler");
    static struct u_object l_u_push_2177 = U_LIT("u_push");
    static struct u_object l__33__61__2178 = U_LIT("!=");
    static struct u_object l_setjmp_2179 = U_LIT("setjmp");
    static struct u_object l_env_2180 = U_LIT("env");
    static struct u_object l_u_pop_2181 = U_LIT("u_pop");
    static struct u_object l_cache_2182 = U_LIT("cache");
    static struct u_object l_NULL_2183 = U_LIT("NULL");
    static struct u_object l_u_reraise_2184 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2185 = U_LIT("u_pop");
    static struct u_object l_cache_2186 = U_LIT("cache");
    static struct u_object l_cache_2187 = U_LIT("cache");
    t2188 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2163), &l_cache_2164, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2165))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2166, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2167), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2168)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2169, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2170, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2171), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2172))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2173), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2174)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2175)), &l_handler_2176, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2177, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2178, f_c_46_c_call_2(&l_setjmp_2179, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2180), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2181, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2182), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2183)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2184, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2185, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2186), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2187))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2188;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2190 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2191 = &t2190;
  u_value l2192 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2192, &n_list_46_Cons)) {
    u_value v_p = u_field(l2192, 0);
    if (u_truth(u_mk_bool(1))) {
      e2191 = u_append(e2191, f_compile_46_fun_wrapper_1(v_p));
    }
    l2192 = u_field(l2192, 1);
  }
  u_value t2193 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2194 = &t2193;
  u_value l2195 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2195, &n_list_46_Cons)) {
    u_value v_p = u_field(l2195, 0);
    if (u_truth(u_mk_bool(1))) {
      e2194 = u_append(e2194, f_compile_46_constr_wrapper_1(v_p));
    }
    l2195 = u_field(l2195, 1);
  }
  u_value t2196 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2197 = &t2196;
  u_value l2198 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2198, &n_list_46_Cons)) {
    u_value v_p = u_field(l2198, 0);
    if (u_truth(u_mk_bool(1))) {
      e2197 = u_append(e2197, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2198 = u_field(l2198, 1);
  }
  return f_list_46__43__43__2(t2190, f_list_46__43__43__2(f_list_46_concat_1(t2193), t2196));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2199 = v_p;
  u_value t2200 = NULL;
  if (!t2200) {
    u_value v_n = u_field(s2199, 0);
    u_value v_k = u_field(s2199, 1);
    static struct u_object l_w__2201 = U_LIT("w_");
    t2200 = f_compile_46_wrapper_3(u_concat(&l_w__2201, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2200) {
    u_no_case(s2199);
  }
  return t2200;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2202 = v_p;
  u_value t2203 = NULL;
  if (!t2203) {
    u_value v_f = u_field(s2202, 0);
    u_value v_k = u_field(s2202, 1);
    static struct u_object l_w_2204 = U_LIT("w");
    t2203 = f_compile_46_wrapper_3(u_concat(&l_w_2204, v_f), v_f, v_k);
  }
  if (!t2203) {
    u_no_case(s2202);
  }
  return t2203;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2205 = U_LIT("env");
  static struct u_object l_args_2206 = U_LIT("args");
  u_value t2207 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2208 = &t2207;
  u_value l2209 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2209, &n_list_46_Cons)) {
    u_value v_i = u_field(l2209, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2210 = U_LIT("args");
      e2208 = u_append(e2208, f_compile_46_index_2(&l_args_2210, v_i));
    }
    l2209 = u_field(l2209, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2205), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2206), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2207))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2211 = v_p;
  u_value t2212 = NULL;
  if ((!t2212) && u_equals(u_field(s2211, 1), u_mk_int(0))) {
    t2212 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2212) {
    u_value v_n = u_field(s2211, 0);
    u_value v_k = u_field(s2211, 1);
    static struct u_object l_wc__2213 = U_LIT("wc_");
    static struct u_object l_env_2214 = U_LIT("env");
    static struct u_object l_args_2215 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2216 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2217 = U_LIT("args");
    t2212 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2213, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2214), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2215), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2216, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2217), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2212) {
    u_no_case(s2211);
  }
  return t2212;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2218;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2218 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2218 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2218;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2219 = v_d;
  u_value t2220 = NULL;
  if ((!t2220) && u_is_con(s2219, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2219, 0);
    u_value v_f = u_field(s2219, 1);
    u_value v_ps = u_field(s2219, 2);
    t2220 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2220) {
    t2220 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2220) {
    u_no_case(s2219);
  }
  return t2220;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2221 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2221;
    u_value t2222 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2222;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2223 = v_g;
    int d2224 = 0;
    if ((!d2224) && u_is_con(s2223, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2223, 1);
      d2224 = 1;
      {
        u_value t2225 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2225;
      }
    }
    if (!d2224) {
      u_no_case(s2223);
    }
    static struct u_object l_ulang_46_h_2226 = U_LIT("ulang.h");
    u_value t2227 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2228 = &t2227;
    u_value l2229 = v_fs;
    while (u_is_con(l2229, &n_list_46_Cons)) {
      u_value v_d = u_field(l2229, 0);
      if (u_truth(u_mk_bool(1))) {
        e2228 = u_append(e2228, f_compile_46_prototype_1(v_d));
      }
      l2229 = u_field(l2229, 1);
    }
    static struct u_object l_main_2230 = U_LIT("main");
    static struct u_object l_argc_2231 = U_LIT("argc");
    static struct u_object l_char_2232 = U_LIT("char");
    static struct u_object l_argv_2233 = U_LIT("argv");
    static struct u_object l_u_args_2234 = U_LIT("u_args");
    static struct u_object l_argc_2235 = U_LIT("argc");
    static struct u_object l_argv_2236 = U_LIT("argv");
    u_value t2237 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2226), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2227), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2230, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2231), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2232))), &l_argv_2233), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2234, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2235), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2236), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2237;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2238 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2238, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2239 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2239, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam1011(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2240 = v_args;
    int d2241 = 0;
    if ((!d2241) && u_is_con(s2240, &n_list_46_Nil)) {
      d2241 = 1;
      {
      }
    }
    if ((!d2241) && (u_is_con(s2240, &n_list_46_Cons) && (u_is_con(u_field(s2240, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2240, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2240, 0);
      u_value v_target = u_field(u_field(s2240, 1), 0);
      d2241 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2241) {
      d2241 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2242 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2242), "os.OsError");
      }
    }
    if (!d2241) {
      u_no_case(s2240);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
