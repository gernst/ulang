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

u_value w_f_check_46_declared_in_3(u_value *env, u_value *args);

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

u_value f_check_46_declared_in_3(u_value v_ms, u_value v_b, u_value v_declared);

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

u_value lam317(u_value *env, u_value *args);

u_value lam324(u_value *env, u_value *args);

u_value lam549(u_value *env, u_value *args);

u_value lam658(u_value *env, u_value *args);

u_value lam661(u_value *env, u_value *args);

u_value lam723(u_value *env, u_value *args);

u_value lam1000(u_value *env, u_value *args);

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

u_value w_f_check_46_declared_in_3(u_value *env, u_value *args) {
  return f_check_46_declared_in_3(args[0], args[1], args[2]);
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
  u_value t175 = f_ctx_46_is_data_2(v_c, v_n);
  if (!u_truth(t175)) {
    t175 = f_ctx_46_is_alias_2(v_c, v_n);
  }
  return t175;
}

u_value f_ctx_46_names_of_2(u_value v_c, u_value v_b) {
  u_value s176 = f_map_46_get_2(v_b, f_ctx_46_Ctx_46_named_1(v_c));
  u_value t177 = NULL;
  if ((!t177) && u_is_con(s176, &n_option_46_Some)) {
    u_value v_ns = u_field(s176, 0);
    t177 = v_ns;
  }
  if ((!t177) && u_is_con(s176, &n_option_46_None)) {
    t177 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t177) {
    u_no_case(s176);
  }
  return t177;
}

u_value f_ctx_46_with_named_2(u_value v_bs, u_value v_n) {
  u_value s178 = f_map_46_get_2(f_syntax_46_Name_46_base_1(v_n), v_bs);
  u_value t179 = NULL;
  if ((!t179) && u_is_con(s178, &n_option_46_Some)) {
    u_value v_ns = u_field(s178, 0);
    u_value t180;
    if (u_truth(f_list_46_contains_2(v_n, v_ns))) {
      t180 = v_bs;
    } else {
      t180 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, v_ns), v_bs);
    }
    t179 = t180;
  }
  if ((!t179) && u_is_con(s178, &n_option_46_None)) {
    t179 = f_map_46_put_3(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_bs);
  }
  if (!t179) {
    u_no_case(s178);
  }
  return t179;
}

u_value f_ctx_46_with_alias_4(u_value v_c, u_value v_n, u_value v_ps, u_value v_t) {
  u_value s181 = v_c;
  u_value t182 = NULL;
  if ((!t182) && u_is_con(s181, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s181, 0);
    u_value v_f = u_field(s181, 1);
    u_value v_p = u_field(s181, 2);
    u_value v_d = u_field(s181, 3);
    u_value v_m = u_field(s181, 4);
    u_value v_k = u_field(s181, 5);
    u_value v_bs = u_field(s181, 6);
    t182 = u_mk_con(&n_ctx_46_Ctx, 7, f_map_46_put_3(v_n, u_mk_tuple(2, v_ps, v_t), v_a), v_f, v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t182) {
    u_no_case(s181);
  }
  return t182;
}

u_value f_ctx_46_with_fun_3(u_value v_c, u_value v_n, u_value v_sc) {
  u_value s183 = v_c;
  u_value t184 = NULL;
  if ((!t184) && u_is_con(s183, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s183, 0);
    u_value v_f = u_field(s183, 1);
    u_value v_p = u_field(s183, 2);
    u_value v_d = u_field(s183, 3);
    u_value v_m = u_field(s183, 4);
    u_value v_k = u_field(s183, 5);
    u_value v_bs = u_field(s183, 6);
    t184 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, f_map_46_put_3(v_n, f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), u_mk_con(&n_list_46_Cons, 2, v_sc, u_mk_con(&n_list_46_Nil, 0))), v_f), v_p, v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t184) {
    u_no_case(s183);
  }
  return t184;
}

u_value f_ctx_46_with_proc_3(u_value v_c, u_value v_n, u_value v_g) {
  u_value s185 = v_c;
  u_value t186 = NULL;
  if ((!t186) && u_is_con(s185, &n_ctx_46_Ctx)) {
    u_value v_a = u_field(s185, 0);
    u_value v_f = u_field(s185, 1);
    u_value v_p = u_field(s185, 2);
    u_value v_d = u_field(s185, 3);
    u_value v_m = u_field(s185, 4);
    u_value v_k = u_field(s185, 5);
    u_value v_bs = u_field(s185, 6);
    t186 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, f_map_46_put_3(v_n, v_g, v_p), v_d, v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t186) {
    u_no_case(s185);
  }
  return t186;
}

u_value f_ctx_46_with_data_3(u_value v_c, u_value v_n, u_value v_arity) {
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
    t188 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, f_map_46_put_3(v_n, v_arity, v_d), v_m, v_k, f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t188) {
    u_no_case(s187);
  }
  return t188;
}

u_value f_ctx_46_with_members_3(u_value v_c, u_value v_n, u_value v_cs) {
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
    t190 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, f_map_46_put_3(v_n, v_cs, v_m), v_k, v_bs);
  }
  if (!t190) {
    u_no_case(s189);
  }
  return t190;
}

u_value f_ctx_46_with_constr_3(u_value v_c, u_value v_n, u_value v_kc) {
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
    t192 = u_mk_con(&n_ctx_46_Ctx, 7, v_a, v_f, v_p, v_d, v_m, f_map_46_put_3(v_n, v_kc, v_k), f_ctx_46_with_named_2(v_bs, v_n));
  }
  if (!t192) {
    u_no_case(s191);
  }
  return t192;
}

u_value f_check_46_State_46_ctx_1(u_value v_x) {
  u_value s193 = v_x;
  u_value t194 = NULL;
  if ((!t194) && u_is_con(s193, &n_check_46_State)) {
    u_value v_y = u_field(s193, 0);
    t194 = v_y;
  }
  if (!t194) {
    u_no_case(s193);
  }
  return t194;
}

u_value f_check_46_State_46_sub_1(u_value v_x) {
  u_value s195 = v_x;
  u_value t196 = NULL;
  if ((!t196) && u_is_con(s195, &n_check_46_State)) {
    u_value v_y = u_field(s195, 1);
    t196 = v_y;
  }
  if (!t196) {
    u_no_case(s195);
  }
  return t196;
}

u_value f_check_46_State_46_count_1(u_value v_x) {
  u_value s197 = v_x;
  u_value t198 = NULL;
  if ((!t198) && u_is_con(s197, &n_check_46_State)) {
    u_value v_y = u_field(s197, 2);
    t198 = v_y;
  }
  if (!t198) {
    u_no_case(s197);
  }
  return t198;
}

u_value f_check_46_State_46_module_1(u_value v_x) {
  u_value s199 = v_x;
  u_value t200 = NULL;
  if ((!t200) && u_is_con(s199, &n_check_46_State)) {
    u_value v_y = u_field(s199, 3);
    t200 = v_y;
  }
  if (!t200) {
    u_no_case(s199);
  }
  return t200;
}

u_value f_check_46_State_46_visible_1(u_value v_x) {
  u_value s201 = v_x;
  u_value t202 = NULL;
  if ((!t202) && u_is_con(s201, &n_check_46_State)) {
    u_value v_y = u_field(s201, 4);
    t202 = v_y;
  }
  if (!t202) {
    u_no_case(s201);
  }
  return t202;
}

u_value f_check_46_State_46_table_1(u_value v_x) {
  u_value s203 = v_x;
  u_value t204 = NULL;
  if ((!t204) && u_is_con(s203, &n_check_46_State)) {
    u_value v_y = u_field(s203, 5);
    t204 = v_y;
  }
  if (!t204) {
    u_no_case(s203);
  }
  return t204;
}

u_value f_check_46_with_ctx_2(u_value v_s, u_value v_c) {
  u_value s205 = v_s;
  u_value t206 = NULL;
  if ((!t206) && u_is_con(s205, &n_check_46_State)) {
    u_value v_su = u_field(s205, 1);
    u_value v_n = u_field(s205, 2);
    u_value v_m = u_field(s205, 3);
    u_value v_ms = u_field(s205, 4);
    u_value v_vs = u_field(s205, 5);
    t206 = u_mk_con(&n_check_46_State, 6, v_c, v_su, v_n, v_m, v_ms, v_vs);
  }
  if (!t206) {
    u_no_case(s205);
  }
  return t206;
}

u_value f_check_46_enter_2(u_value v_s, u_value v_m) {
  u_value s207 = v_s;
  u_value t208 = NULL;
  if ((!t208) && u_is_con(s207, &n_check_46_State)) {
    u_value v_c = u_field(s207, 0);
    u_value v_vs = u_field(s207, 5);
    t208 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_empty_0(), u_mk_int(0), v_m, f_check_46_sees_2(v_m, v_vs), v_vs);
  }
  if (!t208) {
    u_no_case(s207);
  }
  return t208;
}

u_value f_check_46_sees_2(u_value v_m, u_value v_vs) {
  u_value s209 = f_list_46_lookup_2(v_m, v_vs);
  u_value t210 = NULL;
  if ((!t210) && u_is_con(s209, &n_option_46_Some)) {
    u_value v_ms = u_field(s209, 0);
    t210 = v_ms;
  }
  if ((!t210) && u_is_con(s209, &n_option_46_None)) {
    t210 = u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t210) {
    u_no_case(s209);
  }
  return t210;
}

u_value f_check_46_start_2(u_value v_main, u_value v_vs) {
  return u_mk_con(&n_check_46_State, 6, f_check_46_initial_0(), f_map_46_empty_0(), u_mk_int(0), v_main, f_check_46_sees_2(v_main, v_vs), v_vs);
}

u_value f_check_46_flatten_2(u_value v_m, u_value v_cs) {
  u_value s211 = v_cs;
  u_value t212 = NULL;
  if ((!t212) && u_is_con(s211, &n_list_46_Nil)) {
    t212 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t212) && (u_is_con(s211, &n_list_46_Cons) && u_is_con(u_field(s211, 0), &n_syntax_46_CModule))) {
    u_value v_n = u_field(u_field(s211, 0), 0);
    u_value v_ds = u_field(u_field(s211, 0), 1);
    u_value v_rest = u_field(s211, 1);
    t212 = f_list_46__43__43__2(f_check_46_flatten_2(v_n, v_ds), f_check_46_flatten_2(v_m, v_rest));
  }
  if ((!t212) && u_is_con(s211, &n_list_46_Cons)) {
    u_value v_c = u_field(s211, 0);
    u_value v_rest = u_field(s211, 1);
    t212 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_m, f_check_46_qualify_2(v_m, v_c)), f_check_46_flatten_2(v_m, v_rest));
  }
  if (!t212) {
    u_no_case(s211);
  }
  return t212;
}

u_value f_check_46_in_module_2(u_value v_m, u_value v_n) {
  return u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), f_syntax_46_Name_46_base_1(v_n));
}

u_value f_check_46_qualify_2(u_value v_m, u_value v_c) {
  u_value s213 = v_c;
  u_value t214 = NULL;
  if ((!t214) && u_is_con(s213, &n_syntax_46_CSort)) {
    u_value v_n = u_field(s213, 0);
    u_value v_ps = u_field(s213, 1);
    u_value v_t = u_field(s213, 2);
    t214 = u_mk_con(&n_syntax_46_CSort, 3, f_check_46_in_module_2(v_m, v_n), v_ps, v_t);
  }
  if ((!t214) && u_is_con(s213, &n_syntax_46_CData)) {
    u_value v_n = u_field(s213, 0);
    u_value v_ps = u_field(s213, 1);
    u_value v_ks = u_field(s213, 2);
    u_value t215 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e216 = &t215;
    u_value l217 = v_ks;
    while (u_is_con(l217, &n_list_46_Cons)) {
      u_value v_k = u_field(l217, 0);
      if (u_truth(u_mk_bool(1))) {
        e216 = u_append(e216, f_check_46_qualify_constr_2(v_m, v_k));
      }
      l217 = u_field(l217, 1);
    }
    t214 = u_mk_con(&n_syntax_46_CData, 3, f_check_46_in_module_2(v_m, v_n), v_ps, t215);
  }
  if ((!t214) && u_is_con(s213, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s213, 0);
    u_value v_ps = u_field(s213, 1);
    u_value v_xss = u_field(s213, 2);
    u_value v_r = u_field(s213, 3);
    u_value v_b = u_field(s213, 4);
    t214 = u_mk_con(&n_syntax_46_CFun, 5, f_check_46_in_module_2(v_m, v_n), v_ps, v_xss, v_r, v_b);
  }
  if ((!t214) && u_is_con(s213, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s213, 0);
    u_value v_ps = u_field(s213, 1);
    u_value v_ins = u_field(s213, 2);
    u_value v_outs = u_field(s213, 3);
    u_value v_refs = u_field(s213, 4);
    u_value v_b = u_field(s213, 5);
    t214 = u_mk_con(&n_syntax_46_CProc, 6, f_check_46_in_module_2(v_m, v_n), v_ps, v_ins, v_outs, v_refs, v_b);
  }
  if (!t214) {
    t214 = v_c;
  }
  if (!t214) {
    u_no_case(s213);
  }
  return t214;
}

u_value f_check_46_qualify_constr_2(u_value v_m, u_value v_k) {
  u_value s218 = v_k;
  u_value t219 = NULL;
  if ((!t219) && u_is_con(s218, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s218, 0);
    u_value v_ts = u_field(s218, 1);
    t219 = u_mk_con(&n_syntax_46_Constr, 2, f_check_46_in_module_2(v_m, v_n), v_ts);
  }
  if (!t219) {
    u_no_case(s218);
  }
  return t219;
}

u_value f_check_46_imports_in_2(u_value v_ds, u_value v_m) {
  u_value s220 = v_ds;
  u_value t221 = NULL;
  if ((!t221) && u_is_con(s220, &n_list_46_Nil)) {
    t221 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t221) && (u_is_con(s220, &n_list_46_Cons) && u_is_con(u_field(u_field(s220, 0), 1), &n_syntax_46_CImport))) {
    u_value v_k = u_field(u_field(s220, 0), 0);
    u_value v_n = u_field(u_field(u_field(s220, 0), 1), 0);
    u_value v_rest = u_field(s220, 1);
    if (u_truth(u_eq(v_k, v_m))) {
      t221 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_imports_in_2(v_rest, v_m));
    }
  }
  if ((!t221) && u_is_con(s220, &n_list_46_Cons)) {
    u_value v_rest = u_field(s220, 1);
    t221 = f_check_46_imports_in_2(v_rest, v_m);
  }
  if (!t221) {
    u_no_case(s220);
  }
  return t221;
}

u_value f_check_46_visibility_2(u_value v_ds, u_value v_main) {
  u_value t222 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e223 = &t222;
  u_value l224 = f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_main, u_mk_con(&n_list_46_Nil, 0)), f_list_46_firsts_1(v_ds)));
  while (u_is_con(l224, &n_list_46_Cons)) {
    u_value v_m = u_field(l224, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_prelude_225 = U_LIT("prelude");
      static struct u_object l_prelude_226 = U_LIT("prelude");
      e223 = u_append(e223, u_mk_tuple(2, v_m, f_list_46_distinct_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_m, u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_check_46_imports_in_2(v_ds, v_m), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, &l_prelude_225, u_mk_con(&n_list_46_Nil, 0)), f_check_46_imports_in_2(v_ds, &l_prelude_226)))))));
    }
    l224 = u_field(l224, 1);
  }
  return t222;
}

u_value f_check_46_visible_names_3(u_value v_s, u_value v_n, u_value v_declared) {
  u_value s227 = v_n;
  u_value t228 = NULL;
  if ((!t228) && (u_is_con(s227, &n_syntax_46_Name) && u_is_con(u_field(s227, 0), &n_option_46_Some))) {
    u_value t229;
    if (u_truth(u_apply(v_declared, 1, v_n))) {
      t229 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t229 = u_mk_con(&n_list_46_Nil, 0);
    }
    t228 = t229;
  }
  if ((!t228) && (u_is_con(s227, &n_syntax_46_Name) && u_is_con(u_field(s227, 0), &n_option_46_None))) {
    u_value v_b = u_field(s227, 1);
    u_value t230;
    if (u_truth(u_apply(v_declared, 1, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b)))) {
      t230 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, f_check_46_State_46_module_1(v_s)), v_b), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t230 = f_check_46_declared_in_3(f_list_46_drop_2(u_mk_int(1), f_check_46_State_46_visible_1(v_s)), v_b, v_declared);
    }
    t228 = t230;
  }
  if (!t228) {
    u_no_case(s227);
  }
  return t228;
}

u_value f_check_46_declared_in_3(u_value v_ms, u_value v_b, u_value v_declared) {
  u_value s231 = v_ms;
  u_value t232 = NULL;
  if ((!t232) && u_is_con(s231, &n_list_46_Nil)) {
    t232 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t232) && u_is_con(s231, &n_list_46_Cons)) {
    u_value v_m = u_field(s231, 0);
    u_value v_more = u_field(s231, 1);
    u_value t233;
    if (u_truth(u_apply(v_declared, 1, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_b)))) {
      t233 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), v_b), f_check_46_declared_in_3(v_more, v_b, v_declared));
    } else {
      t233 = f_check_46_declared_in_3(v_more, v_b, v_declared);
    }
    t232 = t233;
  }
  if (!t232) {
    u_no_case(s231);
  }
  return t232;
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
  static struct u_object l_Bool_234 = U_LIT("Bool");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Bool_234), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Int_235 = U_LIT("Int");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Int_235), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Str_236 = U_LIT("Str");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Str_236), u_mk_con(&n_list_46_Nil, 0));
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
  static struct u_object l_Char_237 = U_LIT("Char");
  u_pop(handler);
  cache = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_Char_237), u_mk_con(&n_list_46_Nil, 0));
  return cache;
}

u_value f_check_46_tlist_1(u_value v_t) {
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_list_name_0(), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_check_46_fun_2(u_value v_args, u_value v_res) {
  return u_mk_con(&n_syntax_46_TFun, 2, v_args, v_res);
}

u_value f_check_46_params_1(u_value v_ps) {
  u_value t238 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e239 = &t238;
  u_value l240 = v_ps;
  while (u_is_con(l240, &n_list_46_Cons)) {
    u_value v_p = u_field(l240, 0);
    if (u_truth(u_mk_bool(1))) {
      e239 = u_append(e239, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l240 = u_field(l240, 1);
  }
  return t238;
}

u_value f_check_46_basic_1(u_value v_n) {
  u_value s241 = v_n;
  u_value t242 = NULL;
  if ((!t242) && (u_is_con(s241, &n_syntax_46_Name) && u_is_con(u_field(s241, 0), &n_option_46_None))) {
    u_value v_b = u_field(s241, 1);
    static struct u_object l_Bool_243 = U_LIT("Bool");
    static struct u_object l_Int_244 = U_LIT("Int");
    static struct u_object l_Str_245 = U_LIT("Str");
    static struct u_object l_Char_246 = U_LIT("Char");
    t242 = f_list_46_contains_2(v_b, u_mk_con(&n_list_46_Cons, 2, &l_Bool_243, u_mk_con(&n_list_46_Cons, 2, &l_Int_244, u_mk_con(&n_list_46_Cons, 2, &l_Str_245, u_mk_con(&n_list_46_Cons, 2, &l_Char_246, u_mk_con(&n_list_46_Nil, 0))))));
  }
  if (!t242) {
    t242 = u_mk_bool(0);
  }
  if (!t242) {
    u_no_case(s241);
  }
  return t242;
}

u_value f_check_46_literal_1(u_value v_v) {
  u_value s247 = v_v;
  u_value t248 = NULL;
  if ((!t248) && u_is_con(s247, &n_syntax_46_VUnit)) {
    t248 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t248) && u_is_con(s247, &n_syntax_46_VBool)) {
    t248 = f_check_46_tbool_0();
  }
  if ((!t248) && u_is_con(s247, &n_syntax_46_VInt)) {
    t248 = f_check_46_tint_0();
  }
  if ((!t248) && u_is_con(s247, &n_syntax_46_VStr)) {
    t248 = f_check_46_tstr_0();
  }
  if ((!t248) && u_is_con(s247, &n_syntax_46_VChar)) {
    t248 = f_check_46_tchar_0();
  }
  if (!t248) {
    u_no_case(s247);
  }
  return t248;
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
  u_value s249 = v_n;
  u_value t250 = NULL;
  if ((!t250) && (u_is_con(s249, &n_syntax_46_Name) && u_is_con(u_field(s249, 0), &n_option_46_None))) {
    u_value v_b = u_field(s249, 1);
    t250 = f_check_46_builtin_scheme_1(v_b);
  }
  if (!t250) {
    t250 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t250) {
    u_no_case(s249);
  }
  return t250;
}

u_value f_check_46_builtin_scheme_1(u_value v_b) {
  static struct u_object l_A_251 = U_LIT("A");
  u_value t255 = u_mk_con(&n_syntax_46_TParam, 1, &l_A_251);
  u_value t254;
  {
    u_value v_a = t255;
    u_value s252 = v_b;
    u_value t253 = NULL;
    if ((!t253) && u_equals(s252, u_mk_lit("+"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("-"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("*"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("/"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("%"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, f_check_46_arith_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("<"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("=="))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("compare"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("and"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("or"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, f_check_46_logic_0(), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("chars"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tlist_1(f_check_46_tchar_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("string"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tchar_0()), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("ord"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("chr"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("^"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_size"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_join"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_slice"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)))), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_split"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tlist_1(f_check_46_tstr_0()))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_get"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tchar_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_contains"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_compare"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), f_check_46_tint_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_show"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_quote"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tstr_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("str_number"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_option_name_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tint_0(), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("is_digit"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("is_letter"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if ((!t253) && u_equals(s252, u_mk_lit("is_space"))) {
      t253 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, f_check_46_tchar_0(), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tbool_0())), u_mk_con(&n_list_46_Nil, 0));
    }
    if (!t253) {
      t253 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t253) {
      u_no_case(s252);
    }
    t254 = t253;
  }
  return t254;
}

u_value f_check_46_is_meta_1(u_value v_t) {
  u_value s256 = v_t;
  u_value t257 = NULL;
  if ((!t257) && u_is_con(s256, &n_syntax_46_TMeta)) {
    t257 = u_mk_bool(1);
  }
  if (!t257) {
    t257 = u_mk_bool(0);
  }
  if (!t257) {
    u_no_case(s256);
  }
  return t257;
}

u_value f_check_46_free_1(u_value v_t) {
  u_value s258 = v_t;
  u_value t259 = NULL;
  if ((!t259) && u_is_con(s258, &n_syntax_46_TParam)) {
    t259 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t259) && u_is_con(s258, &n_syntax_46_TMeta)) {
    t259 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t259) && u_is_con(s258, &n_syntax_46_THole)) {
    t259 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t259) && u_is_con(s258, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s258, 1);
    u_value t260 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e261 = &t260;
    u_value l262 = v_args;
    while (u_is_con(l262, &n_list_46_Cons)) {
      u_value v_a = u_field(l262, 0);
      if (u_truth(u_mk_bool(1))) {
        e261 = u_append(e261, f_check_46_free_1(v_a));
      }
      l262 = u_field(l262, 1);
    }
    t259 = f_list_46_distinct_1(f_list_46_concat_1(t260));
  }
  if ((!t259) && u_is_con(s258, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s258, 0);
    u_value v_r = u_field(s258, 1);
    u_value t263 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e264 = &t263;
    u_value l265 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l265, &n_list_46_Cons)) {
      u_value v_a = u_field(l265, 0);
      if (u_truth(u_mk_bool(1))) {
        e264 = u_append(e264, f_check_46_free_1(v_a));
      }
      l265 = u_field(l265, 1);
    }
    t259 = f_list_46_distinct_1(f_list_46_concat_1(t263));
  }
  if ((!t259) && u_is_con(s258, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s258, 0);
    u_value t266 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e267 = &t266;
    u_value l268 = v_ts;
    while (u_is_con(l268, &n_list_46_Cons)) {
      u_value v_a = u_field(l268, 0);
      if (u_truth(u_mk_bool(1))) {
        e267 = u_append(e267, f_check_46_free_1(v_a));
      }
      l268 = u_field(l268, 1);
    }
    t259 = f_list_46_distinct_1(f_list_46_concat_1(t266));
  }
  if (!t259) {
    u_no_case(s258);
  }
  return t259;
}

u_value f_check_46_holes_1(u_value v_t) {
  u_value s269 = v_t;
  u_value t270 = NULL;
  if ((!t270) && u_is_con(s269, &n_syntax_46_THole)) {
    t270 = u_mk_bool(1);
  }
  if ((!t270) && u_is_con(s269, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s269, 1);
    u_value t271 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e272 = &t271;
    u_value l273 = v_args;
    while (u_is_con(l273, &n_list_46_Cons)) {
      u_value v_a = u_field(l273, 0);
      if (u_truth(u_mk_bool(1))) {
        e272 = u_append(e272, f_check_46_holes_1(v_a));
      }
      l273 = u_field(l273, 1);
    }
    t270 = f_list_46_contains_2(u_mk_bool(1), t271);
  }
  if ((!t270) && u_is_con(s269, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s269, 0);
    u_value v_r = u_field(s269, 1);
    u_value t274 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e275 = &t274;
    u_value l276 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l276, &n_list_46_Cons)) {
      u_value v_a = u_field(l276, 0);
      if (u_truth(u_mk_bool(1))) {
        e275 = u_append(e275, f_check_46_holes_1(v_a));
      }
      l276 = u_field(l276, 1);
    }
    t270 = f_list_46_contains_2(u_mk_bool(1), t274);
  }
  if ((!t270) && u_is_con(s269, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s269, 0);
    u_value t277 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e278 = &t277;
    u_value l279 = v_ts;
    while (u_is_con(l279, &n_list_46_Cons)) {
      u_value v_a = u_field(l279, 0);
      if (u_truth(u_mk_bool(1))) {
        e278 = u_append(e278, f_check_46_holes_1(v_a));
      }
      l279 = u_field(l279, 1);
    }
    t270 = f_list_46_contains_2(u_mk_bool(1), t277);
  }
  if (!t270) {
    t270 = u_mk_bool(0);
  }
  if (!t270) {
    u_no_case(s269);
  }
  return t270;
}

u_value f_check_46_subst_2(u_value v_t, u_value v_m) {
  u_value s280 = v_t;
  u_value t281 = NULL;
  if ((!t281) && u_is_con(s280, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s280, 0);
    u_value v_args = u_field(s280, 1);
    u_value t282 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e283 = &t282;
    u_value l284 = v_args;
    while (u_is_con(l284, &n_list_46_Cons)) {
      u_value v_a = u_field(l284, 0);
      if (u_truth(u_mk_bool(1))) {
        e283 = u_append(e283, f_check_46_subst_2(v_a, v_m));
      }
      l284 = u_field(l284, 1);
    }
    t281 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t282);
  }
  if ((!t281) && u_is_con(s280, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s280, 0);
    u_value v_r = u_field(s280, 1);
    u_value t285 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e286 = &t285;
    u_value l287 = v_args;
    while (u_is_con(l287, &n_list_46_Cons)) {
      u_value v_a = u_field(l287, 0);
      if (u_truth(u_mk_bool(1))) {
        e286 = u_append(e286, f_check_46_subst_2(v_a, v_m));
      }
      l287 = u_field(l287, 1);
    }
    t281 = u_mk_con(&n_syntax_46_TFun, 2, t285, f_check_46_subst_2(v_r, v_m));
  }
  if ((!t281) && u_is_con(s280, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s280, 0);
    u_value t288 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e289 = &t288;
    u_value l290 = v_ts;
    while (u_is_con(l290, &n_list_46_Cons)) {
      u_value v_a = u_field(l290, 0);
      if (u_truth(u_mk_bool(1))) {
        e289 = u_append(e289, f_check_46_subst_2(v_a, v_m));
      }
      l290 = u_field(l290, 1);
    }
    t281 = u_mk_con(&n_syntax_46_TTuple, 1, t288);
  }
  if (!t281) {
    u_value s291 = f_list_46_lookup_2(v_t, v_m);
    u_value t292 = NULL;
    if ((!t292) && u_is_con(s291, &n_option_46_Some)) {
      u_value v_u = u_field(s291, 0);
      t292 = v_u;
    }
    if ((!t292) && u_is_con(s291, &n_option_46_None)) {
      t292 = v_t;
    }
    if (!t292) {
      u_no_case(s291);
    }
    t281 = t292;
  }
  if (!t281) {
    u_no_case(s280);
  }
  return t281;
}

u_value f_check_46_resolve_2(u_value v_su, u_value v_t) {
  u_value s293 = v_t;
  u_value t294 = NULL;
  if ((!t294) && u_is_con(s293, &n_syntax_46_TMeta)) {
    u_value v_i = u_field(s293, 0);
    u_value s295 = f_map_46_get_2(v_i, v_su);
    u_value t296 = NULL;
    if ((!t296) && u_is_con(s295, &n_option_46_Some)) {
      u_value v_u = u_field(s295, 0);
      t296 = f_check_46_resolve_2(v_su, v_u);
    }
    if ((!t296) && u_is_con(s295, &n_option_46_None)) {
      t296 = v_t;
    }
    if (!t296) {
      u_no_case(s295);
    }
    t294 = t296;
  }
  if (!t294) {
    t294 = v_t;
  }
  if (!t294) {
    u_no_case(s293);
  }
  return t294;
}

u_value f_check_46_occurs_3(u_value v_su, u_value v_i, u_value v_t) {
  u_value s297 = f_check_46_resolve_2(v_su, v_t);
  u_value t298 = NULL;
  if ((!t298) && u_is_con(s297, &n_syntax_46_TMeta)) {
    u_value v_j = u_field(s297, 0);
    t298 = u_eq(v_i, v_j);
  }
  if ((!t298) && u_is_con(s297, &n_syntax_46_TSort)) {
    u_value v_args = u_field(s297, 1);
    u_value t299 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e300 = &t299;
    u_value l301 = v_args;
    while (u_is_con(l301, &n_list_46_Cons)) {
      u_value v_a = u_field(l301, 0);
      if (u_truth(u_mk_bool(1))) {
        e300 = u_append(e300, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l301 = u_field(l301, 1);
    }
    t298 = f_list_46_contains_2(u_mk_bool(1), t299);
  }
  if ((!t298) && u_is_con(s297, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s297, 0);
    u_value v_r = u_field(s297, 1);
    u_value t302 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e303 = &t302;
    u_value l304 = f_list_46__43__43__2(v_args, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)));
    while (u_is_con(l304, &n_list_46_Cons)) {
      u_value v_a = u_field(l304, 0);
      if (u_truth(u_mk_bool(1))) {
        e303 = u_append(e303, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l304 = u_field(l304, 1);
    }
    t298 = f_list_46_contains_2(u_mk_bool(1), t302);
  }
  if ((!t298) && u_is_con(s297, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s297, 0);
    u_value t305 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e306 = &t305;
    u_value l307 = v_ts;
    while (u_is_con(l307, &n_list_46_Cons)) {
      u_value v_a = u_field(l307, 0);
      if (u_truth(u_mk_bool(1))) {
        e306 = u_append(e306, f_check_46_occurs_3(v_su, v_i, v_a));
      }
      l307 = u_field(l307, 1);
    }
    t298 = f_list_46_contains_2(u_mk_bool(1), t305);
  }
  if (!t298) {
    t298 = u_mk_bool(0);
  }
  if (!t298) {
    u_no_case(s297);
  }
  return t298;
}

u_value f_check_46_constructor_scheme_1(u_value v_k) {
  u_value s308 = v_k;
  u_value t309 = NULL;
  if (!t309) {
    u_value v_ps = u_field(s308, 0);
    u_value v_ks = u_field(s308, 1);
    u_value v_r = u_field(s308, 2);
    u_value t310;
    if (u_truth(u_eq(v_ks, u_mk_con(&n_list_46_Nil, 0)))) {
      t310 = v_r;
    } else {
      t310 = f_check_46_fun_2(v_ks, v_r);
    }
    t309 = u_mk_tuple(2, v_ps, t310);
  }
  if (!t309) {
    u_no_case(s308);
  }
  return t309;
}

u_value f_check_46_constructor_of_2(u_value v_c, u_value v_k) {
  u_value s311 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t312 = NULL;
  if ((!t312) && u_is_con(s311, &n_option_46_Some)) {
    u_value v_kc = u_field(s311, 0);
    t312 = f_check_46_constructor_scheme_1(v_kc);
  }
  if ((!t312) && u_is_con(s311, &n_option_46_None)) {
    t312 = u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t312) {
    u_no_case(s311);
  }
  return t312;
}

u_value f_check_46_candidates_3(u_value v_s, u_value v_env, u_value v_n) {
  u_value s313 = f_list_46_lookup_2(v_n, v_env);
  u_value t314 = NULL;
  if ((!t314) && u_is_con(s313, &n_option_46_Some)) {
    u_value v_sc = u_field(s313, 0);
    t314 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, v_sc), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t314) && u_is_con(s313, &n_option_46_None)) {
    u_value t315 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e316 = &t315;
    u_value l318 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam317, 1, v_s));
    while (u_is_con(l318, &n_list_46_Cons)) {
      u_value v_f = u_field(l318, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t319 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e320 = &t319;
        u_value l321 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(v_s), v_f);
        while (u_is_con(l321, &n_list_46_Cons)) {
          u_value v_sc = u_field(l321, 0);
          if (u_truth(u_mk_bool(1))) {
            e320 = u_append(e320, u_mk_tuple(2, v_f, v_sc));
          }
          l321 = u_field(l321, 1);
        }
        e316 = u_append(e316, t319);
      }
      l318 = u_field(l318, 1);
    }
    u_value t322 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e323 = &t322;
    u_value l325 = f_check_46_visible_names_3(v_s, v_n, u_mk_closure(lam324, 1, v_s));
    while (u_is_con(l325, &n_list_46_Cons)) {
      u_value v_k = u_field(l325, 0);
      if (u_truth(u_mk_bool(1))) {
        e323 = u_append(e323, u_mk_tuple(2, v_k, f_check_46_constructor_of_2(f_check_46_State_46_ctx_1(v_s), v_k)));
      }
      l325 = u_field(l325, 1);
    }
    u_value t326 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e327 = &t326;
    u_value l328 = f_check_46_builtin_1(v_n);
    while (u_is_con(l328, &n_list_46_Cons)) {
      u_value v_sc = u_field(l328, 0);
      if (u_truth(u_mk_bool(1))) {
        e327 = u_append(e327, u_mk_tuple(2, v_n, v_sc));
      }
      l328 = u_field(l328, 1);
    }
    t314 = f_list_46__43__43__2(f_list_46_concat_1(t315), f_list_46__43__43__2(t322, t326));
  }
  if (!t314) {
    u_no_case(s313);
  }
  return t314;
}

u_value f_check_46_with_arity_2(u_value v_cs, u_value v_k) {
  u_value s329 = v_cs;
  u_value t330 = NULL;
  if ((!t330) && u_is_con(s329, &n_list_46_Nil)) {
    t330 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t330) && u_is_con(s329, &n_list_46_Cons)) {
    u_value v_n = u_field(u_field(s329, 0), 0);
    u_value v_ps = u_field(u_field(u_field(s329, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s329, 0), 1), 1);
    u_value v_rest = u_field(s329, 1);
    u_value t331;
    if (u_truth(u_eq(f_syntax_46_arity_1(v_t), v_k))) {
      t331 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, u_mk_tuple(2, v_ps, v_t)), f_check_46_with_arity_2(v_rest, v_k));
    } else {
      t331 = f_check_46_with_arity_2(v_rest, v_k);
    }
    t330 = t331;
  }
  if (!t330) {
    u_no_case(s329);
  }
  return t330;
}

u_value f_check_46_has_arity_2(u_value v_scs, u_value v_k) {
  u_value s332 = v_scs;
  u_value t333 = NULL;
  if ((!t333) && u_is_con(s332, &n_list_46_Nil)) {
    t333 = u_mk_bool(0);
  }
  if ((!t333) && u_is_con(s332, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s332, 0), 1);
    u_value v_rest = u_field(s332, 1);
    u_value t334 = u_eq(f_syntax_46_arity_1(v_t), v_k);
    if (!u_truth(t334)) {
      t334 = f_check_46_has_arity_2(v_rest, v_k);
    }
    t333 = t334;
  }
  if (!t333) {
    u_no_case(s332);
  }
  return t333;
}

u_value f_check_46_bind_1(u_value v_xs) {
  u_value t335 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e336 = &t335;
  u_value l337 = v_xs;
  while (u_is_con(l337, &n_list_46_Cons)) {
    u_value v_x = u_field(l337, 0);
    if (u_truth(u_mk_bool(1))) {
      e336 = u_append(e336, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), u_mk_tuple(2, u_mk_con(&n_list_46_Nil, 0), f_syntax_46_Var_46_typ_1(v_x))));
    }
    l337 = u_field(l337, 1);
  }
  return t335;
}

u_value f_check_46_fixed_2(u_value v_su, u_value v_env) {
  u_value s338 = v_env;
  u_value t339 = NULL;
  if ((!t339) && u_is_con(s338, &n_list_46_Nil)) {
    t339 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t339) && u_is_con(s338, &n_list_46_Cons)) {
    u_value v_ps = u_field(u_field(u_field(s338, 0), 1), 0);
    u_value v_t = u_field(u_field(u_field(s338, 0), 1), 1);
    u_value v_rest = u_field(s338, 1);
    t339 = f_list_46__43__43__2(f_list_46_diff_2(f_check_46_free_1(f_check_46_zonk_type_2(v_su, v_t)), v_ps), f_check_46_fixed_2(v_su, v_rest));
  }
  if (!t339) {
    u_no_case(s338);
  }
  return t339;
}

u_value f_check_46_generalize_3(u_value v_su, u_value v_env, u_value v_t) {
  u_value t345 = f_check_46_fixed_2(v_su, v_env);
  u_value t346 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t344;
  {
    u_value v_fx = t345;
    u_value v_u = t346;
    u_value t340 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e341 = &t340;
    u_value l342 = f_check_46_free_1(v_u);
    while (u_is_con(l342, &n_list_46_Cons)) {
      u_value v_m = u_field(l342, 0);
      u_value t343 = f_check_46_is_meta_1(v_m);
      if (u_truth(t343)) {
        t343 = f_prelude_46_not_1(f_list_46_contains_2(v_m, v_fx));
      }
      if (u_truth(t343)) {
        e341 = u_append(e341, v_m);
      }
      l342 = u_field(l342, 1);
    }
    t344 = u_mk_tuple(2, t340, v_u);
  }
  return t344;
}

u_value f_check_46_generalized_3(u_value v_su, u_value v_env, u_value v_xs) {
  u_value t347 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e348 = &t347;
  u_value l349 = v_xs;
  while (u_is_con(l349, &n_list_46_Cons)) {
    u_value v_x = u_field(l349, 0);
    if (u_truth(u_mk_bool(1))) {
      e348 = u_append(e348, u_mk_tuple(2, f_syntax_46_Var_46_name_1(v_x), f_check_46_generalize_3(v_su, v_env, f_syntax_46_Var_46_typ_1(v_x))));
    }
    l349 = u_field(l349, 1);
  }
  return t347;
}

u_value f_check_46_case_of_1(u_value v_b) {
  u_value s350 = v_b;
  u_value t351 = NULL;
  if ((!t351) && u_is_con(s350, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s350, 0);
    u_value v_g = u_field(s350, 1);
    t351 = u_mk_tuple(2, v_p, u_eq(v_g, u_mk_con(&n_option_46_None, 0)));
  }
  if (!t351) {
    u_no_case(s350);
  }
  return t351;
}

u_value f_check_46_zonk_guard_2(u_value v_su, u_value v_g) {
  u_value s352 = v_g;
  u_value t353 = NULL;
  if ((!t353) && u_is_con(s352, &n_option_46_Some)) {
    u_value v_e = u_field(s352, 0);
    t353 = u_mk_con(&n_option_46_Some, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t353) && u_is_con(s352, &n_option_46_None)) {
    t353 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t353) {
    u_no_case(s352);
  }
  return t353;
}

u_value f_check_46_zonk_type_2(u_value v_su, u_value v_t) {
  u_value s354 = f_check_46_resolve_2(v_su, v_t);
  u_value t355 = NULL;
  if ((!t355) && u_is_con(s354, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s354, 0);
    u_value v_args = u_field(s354, 1);
    u_value t356 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e357 = &t356;
    u_value l358 = v_args;
    while (u_is_con(l358, &n_list_46_Cons)) {
      u_value v_a = u_field(l358, 0);
      if (u_truth(u_mk_bool(1))) {
        e357 = u_append(e357, f_check_46_zonk_type_2(v_su, v_a));
      }
      l358 = u_field(l358, 1);
    }
    t355 = u_mk_con(&n_syntax_46_TSort, 2, v_n, t356);
  }
  if ((!t355) && u_is_con(s354, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s354, 0);
    u_value v_r = u_field(s354, 1);
    u_value t359 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e360 = &t359;
    u_value l361 = v_args;
    while (u_is_con(l361, &n_list_46_Cons)) {
      u_value v_a = u_field(l361, 0);
      if (u_truth(u_mk_bool(1))) {
        e360 = u_append(e360, f_check_46_zonk_type_2(v_su, v_a));
      }
      l361 = u_field(l361, 1);
    }
    t355 = u_mk_con(&n_syntax_46_TFun, 2, t359, f_check_46_zonk_type_2(v_su, v_r));
  }
  if ((!t355) && u_is_con(s354, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s354, 0);
    u_value t362 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e363 = &t362;
    u_value l364 = v_ts;
    while (u_is_con(l364, &n_list_46_Cons)) {
      u_value v_a = u_field(l364, 0);
      if (u_truth(u_mk_bool(1))) {
        e363 = u_append(e363, f_check_46_zonk_type_2(v_su, v_a));
      }
      l364 = u_field(l364, 1);
    }
    t355 = u_mk_con(&n_syntax_46_TTuple, 1, t362);
  }
  if (!t355) {
    u_value v_u = s354;
    t355 = v_u;
  }
  if (!t355) {
    u_no_case(s354);
  }
  return t355;
}

u_value f_check_46_zonk_var_2(u_value v_su, u_value v_x) {
  u_value s365 = v_x;
  u_value t366 = NULL;
  if ((!t366) && u_is_con(s365, &n_syntax_46_Var)) {
    u_value v_n = u_field(s365, 0);
    u_value v_t = u_field(s365, 1);
    t366 = u_mk_con(&n_syntax_46_Var, 2, v_n, f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t366) {
    u_no_case(s365);
  }
  return t366;
}

u_value f_check_46_zonk_vars_2(u_value v_su, u_value v_xs) {
  u_value t367 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e368 = &t367;
  u_value l369 = v_xs;
  while (u_is_con(l369, &n_list_46_Cons)) {
    u_value v_x = u_field(l369, 0);
    if (u_truth(u_mk_bool(1))) {
      e368 = u_append(e368, f_check_46_zonk_var_2(v_su, v_x));
    }
    l369 = u_field(l369, 1);
  }
  return t367;
}

u_value f_check_46_zonk_expr_2(u_value v_su, u_value v_e) {
  u_value s370 = v_e;
  u_value t371 = NULL;
  if ((!t371) && u_is_con(s370, &n_syntax_46_ELit)) {
    t371 = v_e;
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_EVar)) {
    u_value v_x = u_field(s370, 0);
    t371 = u_mk_con(&n_syntax_46_EVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s370, 0);
    u_value v_args = u_field(s370, 1);
    u_value t372 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e373 = &t372;
    u_value l374 = v_args;
    while (u_is_con(l374, &n_list_46_Cons)) {
      u_value v_a = u_field(l374, 0);
      if (u_truth(u_mk_bool(1))) {
        e373 = u_append(e373, f_check_46_zonk_expr_2(v_su, v_a));
      }
      l374 = u_field(l374, 1);
    }
    t371 = u_mk_con(&n_syntax_46_EApp, 2, f_check_46_zonk_expr_2(v_su, v_f), t372);
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s370, 0);
    u_value v_b = u_field(s370, 1);
    t371 = u_mk_con(&n_syntax_46_ELam, 2, f_check_46_zonk_vars_2(v_su, v_xs), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s370, 0);
    u_value v_b = u_field(s370, 1);
    u_value t375 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e376 = &t375;
    u_value l377 = v_ps;
    while (u_is_con(l377, &n_list_46_Cons)) {
      u_value v_p = u_field(l377, 0);
      if (u_truth(u_mk_bool(1))) {
        e376 = u_append(e376, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l377 = u_field(l377, 1);
    }
    t371 = u_mk_con(&n_syntax_46_ELet, 2, t375, f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s370, 0);
    u_value v_l = u_field(s370, 1);
    u_value v_r = u_field(s370, 2);
    t371 = u_mk_con(&n_syntax_46_EIte, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_expr_2(v_su, v_l), f_check_46_zonk_expr_2(v_su, v_r));
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s370, 0);
    u_value v_bs = u_field(s370, 1);
    u_value t378 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e379 = &t378;
    u_value l380 = v_bs;
    while (u_is_con(l380, &n_list_46_Cons)) {
      u_value v_b = u_field(l380, 0);
      if (u_truth(u_mk_bool(1))) {
        e379 = u_append(e379, f_check_46_zonk_expr_branch_2(v_su, v_b));
      }
      l380 = u_field(l380, 1);
    }
    t371 = u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t378);
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s370, 0);
    u_value v_xs = u_field(s370, 1);
    u_value v_ls = u_field(s370, 2);
    u_value v_c = u_field(s370, 3);
    u_value t381 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e382 = &t381;
    u_value l383 = v_ls;
    while (u_is_con(l383, &n_list_46_Cons)) {
      u_value v_l = u_field(l383, 0);
      if (u_truth(u_mk_bool(1))) {
        e382 = u_append(e382, f_check_46_zonk_expr_2(v_su, v_l));
      }
      l383 = u_field(l383, 1);
    }
    t371 = u_mk_con(&n_syntax_46_EComp, 4, f_check_46_zonk_expr_2(v_su, v_r), f_check_46_zonk_vars_2(v_su, v_xs), t381, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s370, 0);
    u_value t384 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e385 = &t384;
    u_value l386 = v_es;
    while (u_is_con(l386, &n_list_46_Cons)) {
      u_value v_e = u_field(l386, 0);
      if (u_truth(u_mk_bool(1))) {
        e385 = u_append(e385, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l386 = u_field(l386, 1);
    }
    t371 = u_mk_con(&n_syntax_46_ETuple, 1, t384);
  }
  if ((!t371) && u_is_con(s370, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s370, 0);
    u_value v_t = u_field(s370, 1);
    t371 = u_mk_con(&n_syntax_46_ERaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if (!t371) {
    u_no_case(s370);
  }
  return t371;
}

u_value f_check_46_zonk_binding_2(u_value v_su, u_value v_p) {
  u_value s387 = v_p;
  u_value t388 = NULL;
  if (!t388) {
    u_value v_x = u_field(s387, 0);
    u_value v_e = u_field(s387, 1);
    t388 = u_mk_tuple(2, f_check_46_zonk_var_2(v_su, v_x), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t388) {
    u_no_case(s387);
  }
  return t388;
}

u_value f_check_46_zonk_expr_branch_2(u_value v_su, u_value v_b) {
  u_value s389 = v_b;
  u_value t390 = NULL;
  if ((!t390) && u_is_con(s389, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s389, 0);
    u_value v_g = u_field(s389, 1);
    u_value v_e = u_field(s389, 2);
    t390 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_expr_2(v_su, v_e));
  }
  if (!t390) {
    u_no_case(s389);
  }
  return t390;
}

u_value f_check_46_zonk_pat_2(u_value v_su, u_value v_p) {
  u_value s391 = v_p;
  u_value t392 = NULL;
  if ((!t392) && u_is_con(s391, &n_syntax_46_PVar)) {
    u_value v_x = u_field(s391, 0);
    t392 = u_mk_con(&n_syntax_46_PVar, 1, f_check_46_zonk_var_2(v_su, v_x));
  }
  if ((!t392) && u_is_con(s391, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s391, 0);
    u_value v_args = u_field(s391, 1);
    u_value t393 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e394 = &t393;
    u_value l395 = v_args;
    while (u_is_con(l395, &n_list_46_Cons)) {
      u_value v_a = u_field(l395, 0);
      if (u_truth(u_mk_bool(1))) {
        e394 = u_append(e394, f_check_46_zonk_pat_2(v_su, v_a));
      }
      l395 = u_field(l395, 1);
    }
    t392 = u_mk_con(&n_syntax_46_PApp, 2, v_n, t393);
  }
  if ((!t392) && u_is_con(s391, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s391, 0);
    u_value t396 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e397 = &t396;
    u_value l398 = v_ps;
    while (u_is_con(l398, &n_list_46_Cons)) {
      u_value v_q = u_field(l398, 0);
      if (u_truth(u_mk_bool(1))) {
        e397 = u_append(e397, f_check_46_zonk_pat_2(v_su, v_q));
      }
      l398 = u_field(l398, 1);
    }
    t392 = u_mk_con(&n_syntax_46_PTuple, 1, t396);
  }
  if (!t392) {
    t392 = v_p;
  }
  if (!t392) {
    u_no_case(s391);
  }
  return t392;
}

u_value f_check_46_zonk_stmt_2(u_value v_su, u_value v_x) {
  u_value s399 = v_x;
  u_value t400 = NULL;
  if ((!t400) && u_is_con(s399, &n_syntax_46_SSkip)) {
    t400 = v_x;
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s399, 0);
    u_value t401 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e402 = &t401;
    u_value l403 = v_ps;
    while (u_is_con(l403, &n_list_46_Cons)) {
      u_value v_p = u_field(l403, 0);
      if (u_truth(u_mk_bool(1))) {
        e402 = u_append(e402, f_check_46_zonk_binding_2(v_su, v_p));
      }
      l403 = u_field(l403, 1);
    }
    t400 = u_mk_con(&n_syntax_46_SAssign, 1, t401);
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SBlock)) {
    u_value v_xs = u_field(s399, 0);
    u_value v_ys = u_field(s399, 1);
    u_value t404 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e405 = &t404;
    u_value l406 = v_ys;
    while (u_is_con(l406, &n_list_46_Cons)) {
      u_value v_y = u_field(l406, 0);
      if (u_truth(u_mk_bool(1))) {
        e405 = u_append(e405, f_check_46_zonk_stmt_2(v_su, v_y));
      }
      l406 = u_field(l406, 1);
    }
    t400 = u_mk_con(&n_syntax_46_SBlock, 2, f_check_46_zonk_vars_2(v_su, v_xs), t404);
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s399, 0);
    u_value v_l = u_field(s399, 1);
    u_value v_r = u_field(s399, 2);
    t400 = u_mk_con(&n_syntax_46_SIf, 3, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_l), f_check_46_zonk_stmt_2(v_su, v_r));
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s399, 0);
    u_value v_b = u_field(s399, 1);
    t400 = u_mk_con(&n_syntax_46_SWhile, 2, f_check_46_zonk_expr_2(v_su, v_c), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SCall)) {
    u_value v_p = u_field(s399, 0);
    u_value v_ins = u_field(s399, 1);
    u_value v_outs = u_field(s399, 2);
    u_value v_refs = u_field(s399, 3);
    u_value t407 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e408 = &t407;
    u_value l409 = v_ins;
    while (u_is_con(l409, &n_list_46_Cons)) {
      u_value v_e = u_field(l409, 0);
      if (u_truth(u_mk_bool(1))) {
        e408 = u_append(e408, f_check_46_zonk_expr_2(v_su, v_e));
      }
      l409 = u_field(l409, 1);
    }
    t400 = u_mk_con(&n_syntax_46_SCall, 4, v_p, t407, f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs));
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s399, 0);
    u_value v_bs = u_field(s399, 1);
    u_value t410 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e411 = &t410;
    u_value l412 = v_bs;
    while (u_is_con(l412, &n_list_46_Cons)) {
      u_value v_b = u_field(l412, 0);
      if (u_truth(u_mk_bool(1))) {
        e411 = u_append(e411, f_check_46_zonk_stmt_branch_2(v_su, v_b));
      }
      l412 = u_field(l412, 1);
    }
    t400 = u_mk_con(&n_syntax_46_SMatch, 2, f_check_46_zonk_expr_2(v_su, v_a), t410);
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s399, 0);
    t400 = u_mk_con(&n_syntax_46_SAssert, 1, f_check_46_zonk_expr_2(v_su, v_c));
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s399, 0);
    u_value v_t = u_field(s399, 1);
    t400 = u_mk_con(&n_syntax_46_SRaise, 2, f_check_46_zonk_expr_2(v_su, v_a), f_check_46_zonk_type_2(v_su, v_t));
  }
  if ((!t400) && u_is_con(s399, &n_syntax_46_STry)) {
    u_value v_b = u_field(s399, 0);
    u_value v_hs = u_field(s399, 1);
    u_value t413 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e414 = &t413;
    u_value l415 = v_hs;
    while (u_is_con(l415, &n_list_46_Cons)) {
      u_value v_h = u_field(l415, 0);
      if (u_truth(u_mk_bool(1))) {
        e414 = u_append(e414, f_check_46_zonk_handler_2(v_su, v_h));
      }
      l415 = u_field(l415, 1);
    }
    t400 = u_mk_con(&n_syntax_46_STry, 2, f_check_46_zonk_stmt_2(v_su, v_b), t413);
  }
  if (!t400) {
    u_no_case(s399);
  }
  return t400;
}

u_value f_check_46_zonk_stmt_branch_2(u_value v_su, u_value v_b) {
  u_value s416 = v_b;
  u_value t417 = NULL;
  if ((!t417) && u_is_con(s416, &n_syntax_46_Branch)) {
    u_value v_p = u_field(s416, 0);
    u_value v_g = u_field(s416, 1);
    u_value v_y = u_field(s416, 2);
    t417 = u_mk_con(&n_syntax_46_Branch, 3, f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_y));
  }
  if (!t417) {
    u_no_case(s416);
  }
  return t417;
}

u_value f_check_46_zonk_handler_2(u_value v_su, u_value v_h) {
  u_value s418 = v_h;
  u_value t419 = NULL;
  if ((!t419) && u_is_con(s418, &n_syntax_46_Handler)) {
    u_value v_t = u_field(s418, 0);
    u_value v_p = u_field(s418, 1);
    u_value v_g = u_field(s418, 2);
    u_value v_b = u_field(s418, 3);
    t419 = u_mk_con(&n_syntax_46_Handler, 4, f_check_46_zonk_type_2(v_su, v_t), f_check_46_zonk_pat_2(v_su, v_p), f_check_46_zonk_guard_2(v_su, v_g), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t419) {
    u_no_case(s418);
  }
  return t419;
}

u_value f_check_46_zonk_cmd_2(u_value v_su, u_value v_c) {
  u_value s420 = v_c;
  u_value t421 = NULL;
  if ((!t421) && u_is_con(s420, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s420, 0);
    u_value v_ps = u_field(s420, 1);
    u_value v_xss = u_field(s420, 2);
    u_value v_r = u_field(s420, 3);
    u_value v_b = u_field(s420, 4);
    u_value t422 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e423 = &t422;
    u_value l424 = v_xss;
    while (u_is_con(l424, &n_list_46_Cons)) {
      u_value v_xs = u_field(l424, 0);
      if (u_truth(u_mk_bool(1))) {
        e423 = u_append(e423, f_check_46_zonk_vars_2(v_su, v_xs));
      }
      l424 = u_field(l424, 1);
    }
    t421 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, t422, f_check_46_zonk_type_2(v_su, v_r), f_check_46_zonk_expr_2(v_su, v_b));
  }
  if ((!t421) && u_is_con(s420, &n_syntax_46_CProc)) {
    u_value v_n = u_field(s420, 0);
    u_value v_ps = u_field(s420, 1);
    u_value v_ins = u_field(s420, 2);
    u_value v_outs = u_field(s420, 3);
    u_value v_refs = u_field(s420, 4);
    u_value v_b = u_field(s420, 5);
    t421 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, f_check_46_zonk_vars_2(v_su, v_ins), f_check_46_zonk_vars_2(v_su, v_outs), f_check_46_zonk_vars_2(v_su, v_refs), f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t421) && u_is_con(s420, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s420, 0);
    t421 = u_mk_con(&n_syntax_46_CEval, 1, f_check_46_zonk_expr_2(v_su, v_e));
  }
  if ((!t421) && u_is_con(s420, &n_syntax_46_CRun)) {
    u_value v_n = u_field(s420, 0);
    u_value v_b = u_field(s420, 1);
    t421 = u_mk_con(&n_syntax_46_CRun, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if ((!t421) && u_is_con(s420, &n_syntax_46_CTest)) {
    u_value v_n = u_field(s420, 0);
    u_value v_b = u_field(s420, 1);
    t421 = u_mk_con(&n_syntax_46_CTest, 2, v_n, f_check_46_zonk_stmt_2(v_su, v_b));
  }
  if (!t421) {
    t421 = v_c;
  }
  if (!t421) {
    u_no_case(s420);
  }
  return t421;
}

u_value f_check_46_unknown_type_1(u_value v_t) {
  u_value t425;
  if (u_truth(u_eq(f_check_46_free_1(v_t), u_mk_con(&n_list_46_Nil, 0)))) {
    t425 = u_mk_con(&n_option_46_None, 0);
  } else {
    t425 = u_mk_con(&n_option_46_Some, 1, v_t);
  }
  return t425;
}

u_value f_check_46_unknown_in_1(u_value v_x) {
  u_value s426 = v_x;
  u_value t427 = NULL;
  if ((!t427) && u_is_con(s426, &n_syntax_46_SAssign)) {
    u_value v_ps = u_field(s426, 0);
    t427 = f_check_46_unknown_in_exprs_1(f_list_46_seconds_1(v_ps));
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_SBlock)) {
    u_value v_ys = u_field(s426, 1);
    t427 = f_check_46_unknown_in_all_1(v_ys);
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_SIf)) {
    u_value v_c = u_field(s426, 0);
    u_value v_l = u_field(s426, 1);
    u_value v_r = u_field(s426, 2);
    t427 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_option_46_or_else_2(f_check_46_unknown_in_1(v_l), f_check_46_unknown_in_1(v_r)));
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_SWhile)) {
    u_value v_c = u_field(s426, 0);
    u_value v_b = u_field(s426, 1);
    t427 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_c), f_check_46_unknown_in_1(v_b));
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_SCall)) {
    u_value v_ins = u_field(s426, 1);
    t427 = f_check_46_unknown_in_exprs_1(v_ins);
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_SMatch)) {
    u_value v_a = u_field(s426, 0);
    u_value v_bs = u_field(s426, 1);
    t427 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_a), f_check_46_unknown_in_branches_1(v_bs));
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_SAssert)) {
    u_value v_c = u_field(s426, 0);
    t427 = f_check_46_unknown_in_expr_1(v_c);
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_SRaise)) {
    u_value v_a = u_field(s426, 0);
    u_value v_t = u_field(s426, 1);
    t427 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t427) && u_is_con(s426, &n_syntax_46_STry)) {
    u_value v_b = u_field(s426, 0);
    u_value v_hs = u_field(s426, 1);
    t427 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_hs));
  }
  if (!t427) {
    t427 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t427) {
    u_no_case(s426);
  }
  return t427;
}

u_value f_check_46_unknown_in_expr_1(u_value v_e) {
  u_value s428 = v_e;
  u_value t429 = NULL;
  if ((!t429) && u_is_con(s428, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s428, 0);
    u_value v_t = u_field(s428, 1);
    t429 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_check_46_unknown_in_expr_1(v_a));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s428, 0);
    u_value v_args = u_field(s428, 1);
    t429 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_f, v_args));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_ELam)) {
    u_value v_b = u_field(s428, 1);
    t429 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_ELet)) {
    u_value v_ps = u_field(s428, 0);
    u_value v_b = u_field(s428, 1);
    t429 = f_check_46_unknown_in_exprs_1(f_list_46__43__43__2(f_list_46_seconds_1(v_ps), u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s428, 0);
    u_value v_l = u_field(s428, 1);
    u_value v_r = u_field(s428, 2);
    t429 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Nil, 0)))));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s428, 0);
    u_value v_bs = u_field(s428, 1);
    u_value t430 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e431 = &t430;
    u_value l432 = v_bs;
    while (u_is_con(l432, &n_list_46_Cons)) {
      u_value v_b = u_field(l432, 0);
      if (u_truth(u_mk_bool(1))) {
        e431 = u_append(e431, f_check_46_branch_exprs_1(v_b));
      }
      l432 = u_field(l432, 1);
    }
    t429 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_a, f_list_46_concat_1(t430)));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s428, 0);
    u_value v_ls = u_field(s428, 2);
    u_value v_c = u_field(s428, 3);
    t429 = f_check_46_unknown_in_exprs_1(u_mk_con(&n_list_46_Cons, 2, v_r, u_mk_con(&n_list_46_Cons, 2, v_c, v_ls)));
  }
  if ((!t429) && u_is_con(s428, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s428, 0);
    t429 = f_check_46_unknown_in_exprs_1(v_es);
  }
  if (!t429) {
    t429 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t429) {
    u_no_case(s428);
  }
  return t429;
}

u_value f_check_46_unknown_in_exprs_1(u_value v_es) {
  u_value s433 = v_es;
  u_value t434 = NULL;
  if ((!t434) && u_is_con(s433, &n_list_46_Nil)) {
    t434 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t434) && u_is_con(s433, &n_list_46_Cons)) {
    u_value v_e = u_field(s433, 0);
    u_value v_rest = u_field(s433, 1);
    t434 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_e), f_check_46_unknown_in_exprs_1(v_rest));
  }
  if (!t434) {
    u_no_case(s433);
  }
  return t434;
}

u_value f_check_46_branch_exprs_1(u_value v_b) {
  u_value s435 = v_b;
  u_value t436 = NULL;
  if ((!t436) && (u_is_con(s435, &n_syntax_46_Branch) && u_is_con(u_field(s435, 1), &n_option_46_Some))) {
    u_value v_g = u_field(u_field(s435, 1), 0);
    u_value v_e = u_field(s435, 2);
    t436 = u_mk_con(&n_list_46_Cons, 2, v_g, u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t436) && (u_is_con(s435, &n_syntax_46_Branch) && u_is_con(u_field(s435, 1), &n_option_46_None))) {
    u_value v_e = u_field(s435, 2);
    t436 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t436) {
    u_no_case(s435);
  }
  return t436;
}

u_value f_check_46_unknown_in_all_1(u_value v_xs) {
  u_value s437 = v_xs;
  u_value t438 = NULL;
  if ((!t438) && u_is_con(s437, &n_list_46_Nil)) {
    t438 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t438) && u_is_con(s437, &n_list_46_Cons)) {
    u_value v_y = u_field(s437, 0);
    u_value v_ys = u_field(s437, 1);
    t438 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_all_1(v_ys));
  }
  if (!t438) {
    u_no_case(s437);
  }
  return t438;
}

u_value f_check_46_unknown_in_branches_1(u_value v_bs) {
  u_value s439 = v_bs;
  u_value t440 = NULL;
  if ((!t440) && u_is_con(s439, &n_list_46_Nil)) {
    t440 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t440) && (u_is_con(s439, &n_list_46_Cons) && (u_is_con(u_field(s439, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s439, 0), 1), &n_option_46_Some)))) {
    u_value v_g = u_field(u_field(u_field(s439, 0), 1), 0);
    u_value v_y = u_field(u_field(s439, 0), 2);
    u_value v_rest = u_field(s439, 1);
    t440 = f_option_46_or_else_2(f_check_46_unknown_in_expr_1(v_g), f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest)));
  }
  if ((!t440) && (u_is_con(s439, &n_list_46_Cons) && (u_is_con(u_field(s439, 0), &n_syntax_46_Branch) && u_is_con(u_field(u_field(s439, 0), 1), &n_option_46_None)))) {
    u_value v_y = u_field(u_field(s439, 0), 2);
    u_value v_rest = u_field(s439, 1);
    t440 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_y), f_check_46_unknown_in_branches_1(v_rest));
  }
  if (!t440) {
    u_no_case(s439);
  }
  return t440;
}

u_value f_check_46_unknown_in_handlers_1(u_value v_hs) {
  u_value s441 = v_hs;
  u_value t442 = NULL;
  if ((!t442) && u_is_con(s441, &n_list_46_Nil)) {
    t442 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t442) && (u_is_con(s441, &n_list_46_Cons) && (u_is_con(u_field(s441, 0), &n_syntax_46_Handler) && u_is_con(u_field(u_field(s441, 0), 1), &n_syntax_46_PWild)))) {
    u_value v_b = u_field(u_field(s441, 0), 3);
    u_value v_rest = u_field(s441, 1);
    t442 = f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest));
  }
  if ((!t442) && (u_is_con(s441, &n_list_46_Cons) && u_is_con(u_field(s441, 0), &n_syntax_46_Handler))) {
    u_value v_t = u_field(u_field(s441, 0), 0);
    u_value v_b = u_field(u_field(s441, 0), 3);
    u_value v_rest = u_field(s441, 1);
    t442 = f_option_46_or_else_2(f_check_46_unknown_type_1(v_t), f_option_46_or_else_2(f_check_46_unknown_in_1(v_b), f_check_46_unknown_in_handlers_1(v_rest)));
  }
  if (!t442) {
    u_no_case(s441);
  }
  return t442;
}

u_value f_check_46_unknown_in_cmd_1(u_value v_c) {
  u_value s443 = v_c;
  u_value t444 = NULL;
  if ((!t444) && u_is_con(s443, &n_syntax_46_CFun)) {
    u_value v_b = u_field(s443, 4);
    t444 = f_check_46_unknown_in_expr_1(v_b);
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_CEval)) {
    u_value v_e = u_field(s443, 0);
    t444 = f_check_46_unknown_in_expr_1(v_e);
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_CProc)) {
    u_value v_b = u_field(s443, 5);
    t444 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_CRun)) {
    u_value v_b = u_field(s443, 1);
    t444 = f_check_46_unknown_in_1(v_b);
  }
  if ((!t444) && u_is_con(s443, &n_syntax_46_CTest)) {
    u_value v_b = u_field(s443, 1);
    t444 = f_check_46_unknown_in_1(v_b);
  }
  if (!t444) {
    t444 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t444) {
    u_no_case(s443);
  }
  return t444;
}

u_value f_check_46_head_of_1(u_value v_p) {
  u_value s445 = v_p;
  u_value t446 = NULL;
  if ((!t446) && u_is_con(s445, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s445, 0);
    u_value v_args = u_field(s445, 1);
    t446 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_n), v_args));
  }
  if ((!t446) && u_is_con(s445, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s445, 0);
    t446 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ps));
  }
  if ((!t446) && u_is_con(s445, &n_syntax_46_PLit)) {
    u_value v_v = u_field(s445, 0);
    t446 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, v_v), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t446) {
    t446 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t446) {
    u_no_case(s445);
  }
  return t446;
}

u_value f_check_46_heads_1(u_value v_rows) {
  u_value s447 = v_rows;
  u_value t448 = NULL;
  if ((!t448) && u_is_con(s447, &n_list_46_Nil)) {
    t448 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t448) && (u_is_con(s447, &n_list_46_Cons) && u_is_con(u_field(s447, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s447, 0), 0);
    u_value v_rest = u_field(s447, 1);
    u_value s449 = f_check_46_head_of_1(v_p);
    u_value t450 = NULL;
    if ((!t450) && u_is_con(s449, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s449, 0), 0);
      t450 = u_mk_con(&n_list_46_Cons, 2, v_h, f_check_46_heads_1(v_rest));
    }
    if ((!t450) && u_is_con(s449, &n_option_46_None)) {
      t450 = f_check_46_heads_1(v_rest);
    }
    if (!t450) {
      u_no_case(s449);
    }
    t448 = t450;
  }
  if ((!t448) && (u_is_con(s447, &n_list_46_Cons) && u_is_con(u_field(s447, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s447, 1);
    t448 = f_check_46_heads_1(v_rest);
  }
  if (!t448) {
    u_no_case(s447);
  }
  return t448;
}

u_value f_check_46_field_types_3(u_value v_c, u_value v_k, u_value v_args) {
  u_value s451 = f_ctx_46_constr_of_2(v_c, v_k);
  u_value t452 = NULL;
  if ((!t452) && u_is_con(s451, &n_option_46_Some)) {
    u_value v_ps = u_field(u_field(s451, 0), 0);
    u_value v_ks = u_field(u_field(s451, 0), 1);
    u_value t453 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e454 = &t453;
    u_value l455 = v_ks;
    while (u_is_con(l455, &n_list_46_Cons)) {
      u_value v_a = u_field(l455, 0);
      if (u_truth(u_mk_bool(1))) {
        e454 = u_append(e454, f_check_46_subst_2(v_a, f_list_46_zip_2(v_ps, v_args)));
      }
      l455 = u_field(l455, 1);
    }
    t452 = t453;
  }
  if ((!t452) && u_is_con(s451, &n_option_46_None)) {
    t452 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t452) {
    u_no_case(s451);
  }
  return t452;
}

u_value f_check_46_constructors_of_3(u_value v_c, u_value v_su, u_value v_t) {
  u_value s456 = f_check_46_zonk_type_2(v_su, v_t);
  u_value t457 = NULL;
  if ((!t457) && (u_is_con(s456, &n_syntax_46_TSort) && (u_is_con(u_field(s456, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s456, 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(s456, 0), 1), u_mk_lit("Bool")) && u_is_con(u_field(s456, 1), &n_list_46_Nil)))))) {
    t457 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t457) && u_is_con(s456, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s456, 0);
    t457 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, f_syntax_46_tuple_name_0()), v_ts), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t457) && u_is_con(s456, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s456, 0);
    u_value v_args = u_field(s456, 1);
    u_value s458 = f_ctx_46_members_of_2(v_c, v_n);
    u_value t459 = NULL;
    if ((!t459) && u_is_con(s458, &n_option_46_Some)) {
      u_value v_ks = u_field(s458, 0);
      u_value t460 = u_mk_con(&n_list_46_Nil, 0);
      u_value *e461 = &t460;
      u_value l462 = v_ks;
      while (u_is_con(l462, &n_list_46_Cons)) {
        u_value v_k = u_field(l462, 0);
        if (u_truth(u_mk_bool(1))) {
          e461 = u_append(e461, u_mk_tuple(2, u_mk_con(&n_check_46_HCon, 1, v_k), f_check_46_field_types_3(v_c, v_k, v_args)));
        }
        l462 = u_field(l462, 1);
      }
      t459 = u_mk_con(&n_option_46_Some, 1, t460);
    }
    if ((!t459) && u_is_con(s458, &n_option_46_None)) {
      t459 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t459) {
      u_no_case(s458);
    }
    t457 = t459;
  }
  if (!t457) {
    t457 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t457) {
    u_no_case(s456);
  }
  return t457;
}

u_value f_check_46_fields_of_4(u_value v_c, u_value v_su, u_value v_t, u_value v_h) {
  u_value s463 = f_check_46_constructors_of_3(v_c, v_su, v_t);
  u_value t464 = NULL;
  if ((!t464) && u_is_con(s463, &n_option_46_Some)) {
    u_value v_ks = u_field(s463, 0);
    u_value s465 = f_list_46_lookup_2(v_h, v_ks);
    u_value t466 = NULL;
    if ((!t466) && u_is_con(s465, &n_option_46_Some)) {
      u_value v_ts = u_field(s465, 0);
      t466 = v_ts;
    }
    if ((!t466) && u_is_con(s465, &n_option_46_None)) {
      t466 = u_mk_con(&n_list_46_Nil, 0);
    }
    if (!t466) {
      u_no_case(s465);
    }
    t464 = t466;
  }
  if ((!t464) && u_is_con(s463, &n_option_46_None)) {
    t464 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t464) {
    u_no_case(s463);
  }
  return t464;
}

u_value f_check_46_covers_2(u_value v_hs, u_value v_ks) {
  u_value s467 = v_ks;
  u_value t468 = NULL;
  if ((!t468) && u_is_con(s467, &n_list_46_Nil)) {
    t468 = u_mk_bool(1);
  }
  if ((!t468) && u_is_con(s467, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s467, 0), 0);
    u_value v_rest = u_field(s467, 1);
    u_value t469 = f_list_46_contains_2(v_h, v_hs);
    if (u_truth(t469)) {
      t469 = f_check_46_covers_2(v_hs, v_rest);
    }
    t468 = t469;
  }
  if (!t468) {
    u_no_case(s467);
  }
  return t468;
}

u_value f_check_46_defaults_1(u_value v_rows) {
  u_value s470 = v_rows;
  u_value t471 = NULL;
  if ((!t471) && u_is_con(s470, &n_list_46_Nil)) {
    t471 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t471) && (u_is_con(s470, &n_list_46_Cons) && u_is_con(u_field(s470, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s470, 0), 0);
    u_value v_ps = u_field(u_field(s470, 0), 1);
    u_value v_rest = u_field(s470, 1);
    u_value s472 = f_check_46_head_of_1(v_p);
    u_value t473 = NULL;
    if ((!t473) && u_is_con(s472, &n_option_46_Some)) {
      t473 = f_check_46_defaults_1(v_rest);
    }
    if ((!t473) && u_is_con(s472, &n_option_46_None)) {
      t473 = u_mk_con(&n_list_46_Cons, 2, v_ps, f_check_46_defaults_1(v_rest));
    }
    if (!t473) {
      u_no_case(s472);
    }
    t471 = t473;
  }
  if ((!t471) && (u_is_con(s470, &n_list_46_Cons) && u_is_con(u_field(s470, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s470, 1);
    t471 = f_check_46_defaults_1(v_rest);
  }
  if (!t471) {
    u_no_case(s470);
  }
  return t471;
}

u_value f_check_46_specialize_3(u_value v_rows, u_value v_h, u_value v_n) {
  u_value s474 = v_rows;
  u_value t475 = NULL;
  if ((!t475) && u_is_con(s474, &n_list_46_Nil)) {
    t475 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t475) && (u_is_con(s474, &n_list_46_Cons) && u_is_con(u_field(s474, 0), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s474, 0), 0);
    u_value v_ps = u_field(u_field(s474, 0), 1);
    u_value v_rest = u_field(s474, 1);
    u_value s476 = f_check_46_head_of_1(v_p);
    u_value t477 = NULL;
    if ((!t477) && u_is_con(s476, &n_option_46_Some)) {
      u_value v_g = u_field(u_field(s476, 0), 0);
      u_value v_args = u_field(u_field(s476, 0), 1);
      u_value t478;
      if (u_truth(u_eq(v_g, v_h))) {
        t478 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(v_args, v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
      } else {
        t478 = f_check_46_specialize_3(v_rest, v_h, v_n);
      }
      t477 = t478;
    }
    if ((!t477) && u_is_con(s476, &n_option_46_None)) {
      t477 = u_mk_con(&n_list_46_Cons, 2, f_list_46__43__43__2(f_list_46_replicate_2(v_n, u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_check_46_specialize_3(v_rest, v_h, v_n));
    }
    if (!t477) {
      u_no_case(s476);
    }
    t475 = t477;
  }
  if ((!t475) && (u_is_con(s474, &n_list_46_Cons) && u_is_con(u_field(s474, 0), &n_list_46_Nil))) {
    u_value v_rest = u_field(s474, 1);
    t475 = f_check_46_specialize_3(v_rest, v_h, v_n);
  }
  if (!t475) {
    u_no_case(s474);
  }
  return t475;
}

u_value f_check_46_useful_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_q, u_value v_ts) {
  u_value s479 = u_mk_tuple(2, v_q, v_ts);
  u_value t480 = NULL;
  if ((!t480) && u_is_con(u_field(s479, 0), &n_list_46_Nil)) {
    t480 = u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t480) && (u_is_con(u_field(s479, 0), &n_list_46_Cons) && u_is_con(u_field(s479, 1), &n_list_46_Cons))) {
    u_value v_p = u_field(u_field(s479, 0), 0);
    u_value v_ps = u_field(u_field(s479, 0), 1);
    u_value v_t = u_field(u_field(s479, 1), 0);
    u_value v_rest = u_field(u_field(s479, 1), 1);
    u_value s481 = f_check_46_head_of_1(v_p);
    u_value t482 = NULL;
    if ((!t482) && u_is_con(s481, &n_option_46_Some)) {
      u_value v_h = u_field(u_field(s481, 0), 0);
      u_value v_args = u_field(u_field(s481, 0), 1);
      t482 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_ps), f_list_46__43__43__2(f_check_46_fields_of_4(v_c, v_su, v_t, v_h), v_rest));
    }
    if ((!t482) && u_is_con(s481, &n_option_46_None)) {
      u_value s483 = f_check_46_constructors_of_3(v_c, v_su, v_t);
      u_value t484 = NULL;
      if ((!t484) && u_is_con(s483, &n_option_46_Some)) {
        u_value v_ks = u_field(s483, 0);
        u_value t485;
        if (u_truth(f_check_46_covers_2(f_check_46_heads_1(v_rows), v_ks))) {
          t485 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_ks, v_ps, v_rest);
        } else {
          t485 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
        }
        t484 = t485;
      }
      if ((!t484) && u_is_con(s483, &n_option_46_None)) {
        t484 = f_check_46_useful_5(v_c, v_su, f_check_46_defaults_1(v_rows), v_ps, v_rest);
      }
      if (!t484) {
        u_no_case(s483);
      }
      t482 = t484;
    }
    if (!t482) {
      u_no_case(s481);
    }
    t480 = t482;
  }
  if ((!t480) && (u_is_con(u_field(s479, 0), &n_list_46_Cons) && u_is_con(u_field(s479, 1), &n_list_46_Nil))) {
    t480 = u_mk_bool(0);
  }
  if (!t480) {
    u_no_case(s479);
  }
  return t480;
}

u_value f_check_46_useful_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_ps, u_value v_rest) {
  u_value s486 = v_ks;
  u_value t487 = NULL;
  if ((!t487) && u_is_con(s486, &n_list_46_Nil)) {
    t487 = u_mk_bool(0);
  }
  if ((!t487) && u_is_con(s486, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s486, 0), 0);
    u_value v_args = u_field(u_field(s486, 0), 1);
    u_value v_more = u_field(s486, 1);
    u_value t488 = f_check_46_useful_5(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)), v_ps), f_list_46__43__43__2(v_args, v_rest));
    if (!u_truth(t488)) {
      t488 = f_check_46_useful_in_6(v_c, v_su, v_rows, v_more, v_ps, v_rest);
    }
    t487 = t488;
  }
  if (!t487) {
    u_no_case(s486);
  }
  return t487;
}

u_value f_check_46_witness_2(u_value v_h, u_value v_args) {
  u_value s489 = v_h;
  u_value t490 = NULL;
  if ((!t490) && u_is_con(s489, &n_check_46_HCon)) {
    u_value v_n = u_field(s489, 0);
    u_value t491;
    if (u_truth(u_eq(v_n, f_syntax_46_tuple_name_0()))) {
      t491 = u_mk_con(&n_syntax_46_PTuple, 1, v_args);
    } else {
      t491 = u_mk_con(&n_syntax_46_PApp, 2, v_n, v_args);
    }
    t490 = t491;
  }
  if ((!t490) && u_is_con(s489, &n_check_46_HLit)) {
    u_value v_v = u_field(s489, 0);
    t490 = u_mk_con(&n_syntax_46_PLit, 1, v_v);
  }
  if (!t490) {
    u_no_case(s489);
  }
  return t490;
}

u_value f_check_46_missing_4(u_value v_c, u_value v_su, u_value v_rows, u_value v_ts) {
  u_value s492 = v_ts;
  u_value t493 = NULL;
  if ((!t493) && u_is_con(s492, &n_list_46_Nil)) {
    u_value t494;
    if (u_truth(u_eq(v_rows, u_mk_con(&n_list_46_Nil, 0)))) {
      t494 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
    } else {
      t494 = u_mk_con(&n_option_46_None, 0);
    }
    t493 = t494;
  }
  if ((!t493) && u_is_con(s492, &n_list_46_Cons)) {
    u_value v_t = u_field(s492, 0);
    u_value v_rest = u_field(s492, 1);
    t493 = f_check_46_missing_in_6(v_c, v_su, v_rows, f_check_46_heads_1(v_rows), f_check_46_constructors_of_3(v_c, v_su, v_t), v_rest);
  }
  if (!t493) {
    u_no_case(s492);
  }
  return t493;
}

u_value f_check_46_missing_in_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s495 = v_cs;
  u_value t496 = NULL;
  if ((!t496) && u_is_con(s495, &n_option_46_Some)) {
    u_value v_ks = u_field(s495, 0);
    u_value t497;
    if (u_truth(f_check_46_covers_2(v_hs, v_ks))) {
      t497 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_ks, v_rest);
    } else {
      t497 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
    }
    t496 = t497;
  }
  if ((!t496) && u_is_con(s495, &n_option_46_None)) {
    t496 = f_check_46_other_missing_6(v_c, v_su, v_rows, v_hs, v_cs, v_rest);
  }
  if (!t496) {
    u_no_case(s495);
  }
  return t496;
}

u_value f_check_46_first_missing_5(u_value v_c, u_value v_su, u_value v_rows, u_value v_ks, u_value v_rest) {
  u_value s498 = v_ks;
  u_value t499 = NULL;
  if ((!t499) && u_is_con(s498, &n_list_46_Nil)) {
    t499 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t499) && u_is_con(s498, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s498, 0), 0);
    u_value v_args = u_field(u_field(s498, 0), 1);
    u_value v_more = u_field(s498, 1);
    u_value s500 = f_check_46_missing_4(v_c, v_su, f_check_46_specialize_3(v_rows, v_h, f_list_46_length_1(v_args)), f_list_46__43__43__2(v_args, v_rest));
    u_value t501 = NULL;
    if ((!t501) && u_is_con(s500, &n_option_46_Some)) {
      u_value v_ws = u_field(s500, 0);
      t501 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_witness_2(v_h, f_list_46_take_2(f_list_46_length_1(v_args), v_ws)), f_list_46_drop_2(f_list_46_length_1(v_args), v_ws)));
    }
    if ((!t501) && u_is_con(s500, &n_option_46_None)) {
      t501 = f_check_46_first_missing_5(v_c, v_su, v_rows, v_more, v_rest);
    }
    if (!t501) {
      u_no_case(s500);
    }
    t499 = t501;
  }
  if (!t499) {
    u_no_case(s498);
  }
  return t499;
}

u_value f_check_46_other_missing_6(u_value v_c, u_value v_su, u_value v_rows, u_value v_hs, u_value v_cs, u_value v_rest) {
  u_value s502 = f_check_46_missing_4(v_c, v_su, f_check_46_defaults_1(v_rows), v_rest);
  u_value t503 = NULL;
  if ((!t503) && u_is_con(s502, &n_option_46_Some)) {
    u_value v_ws = u_field(s502, 0);
    t503 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, f_check_46_uncovered_2(v_hs, v_cs), v_ws));
  }
  if ((!t503) && u_is_con(s502, &n_option_46_None)) {
    t503 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t503) {
    u_no_case(s502);
  }
  return t503;
}

u_value f_check_46_uncovered_2(u_value v_hs, u_value v_cs) {
  u_value s504 = v_cs;
  u_value t505 = NULL;
  if ((!t505) && u_is_con(s504, &n_option_46_Some)) {
    u_value v_ks = u_field(s504, 0);
    u_value t506;
    if (u_truth(u_eq(v_hs, u_mk_con(&n_list_46_Nil, 0)))) {
      t506 = u_mk_con(&n_syntax_46_PWild, 0);
    } else {
      t506 = f_check_46_first_uncovered_2(v_hs, v_ks);
    }
    t505 = t506;
  }
  if ((!t505) && u_is_con(s504, &n_option_46_None)) {
    t505 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if (!t505) {
    u_no_case(s504);
  }
  return t505;
}

u_value f_check_46_first_uncovered_2(u_value v_hs, u_value v_ks) {
  u_value s507 = v_ks;
  u_value t508 = NULL;
  if ((!t508) && u_is_con(s507, &n_list_46_Nil)) {
    t508 = u_mk_con(&n_syntax_46_PWild, 0);
  }
  if ((!t508) && u_is_con(s507, &n_list_46_Cons)) {
    u_value v_h = u_field(u_field(s507, 0), 0);
    u_value v_args = u_field(u_field(s507, 0), 1);
    u_value v_rest = u_field(s507, 1);
    u_value t509;
    if (u_truth(f_list_46_contains_2(v_h, v_hs))) {
      t509 = f_check_46_first_uncovered_2(v_hs, v_rest);
    } else {
      t509 = f_check_46_witness_2(v_h, f_list_46_replicate_2(f_list_46_length_1(v_args), u_mk_con(&n_syntax_46_PWild, 0)));
    }
    t508 = t509;
  }
  if (!t508) {
    u_no_case(s507);
  }
  return t508;
}

void p_check_46_fresh(u_value *r_t, u_value *r_s) {
  {
    u_value s510 = *r_s;
    int d511 = 0;
    if ((!d511) && u_is_con(s510, &n_check_46_State)) {
      u_value v_c = u_field(s510, 0);
      u_value v_su = u_field(s510, 1);
      u_value v_n = u_field(s510, 2);
      u_value v_m = u_field(s510, 3);
      u_value v_ms = u_field(s510, 4);
      u_value v_vs = u_field(s510, 5);
      d511 = 1;
      {
        u_value t512 = u_mk_con(&n_syntax_46_TMeta, 1, u_add(v_n, u_mk_int(1)));
        u_value t513 = u_mk_con(&n_check_46_State, 6, v_c, v_su, u_add(v_n, u_mk_int(1)), v_m, v_ms, v_vs);
        *r_t = t512;
        *r_s = t513;
      }
    }
    if (!d511) {
      u_no_case(s510);
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
    u_value s514 = *r_s;
    int d515 = 0;
    if ((!d515) && u_is_con(s514, &n_check_46_State)) {
      u_value v_c = u_field(s514, 0);
      u_value v_su = u_field(s514, 1);
      u_value v_n = u_field(s514, 2);
      u_value v_m = u_field(s514, 3);
      u_value v_ms = u_field(s514, 4);
      u_value v_vs = u_field(s514, 5);
      d515 = 1;
      {
        u_value t516 = u_mk_con(&n_check_46_State, 6, v_c, f_map_46_put_3(v_i, v_t, v_su), v_n, v_m, v_ms, v_vs);
        *r_s = t516;
      }
    }
    if (!d515) {
      u_no_case(s514);
    }
  }
}

void p_check_46_unify(u_value v_t1, u_value v_t2, u_value *r_s) {
  {
    u_value v_a = NULL;
    u_value v_b = NULL;
    u_value t517 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t1);
    v_a = t517;
    u_value t518 = f_check_46_resolve_2(f_check_46_State_46_sub_1(*r_s), v_t2);
    v_b = t518;
    u_value s519 = u_mk_tuple(2, v_a, v_b);
    int d520 = 0;
    if ((!d520) && (u_is_con(u_field(s519, 0), &n_syntax_46_TMeta) && u_is_con(u_field(s519, 1), &n_syntax_46_TMeta))) {
      u_value v_i = u_field(u_field(s519, 0), 0);
      u_value v_j = u_field(u_field(s519, 1), 0);
      d520 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_i, v_j))) {
          {
            p_check_46_bind_meta(v_i, v_b, r_s);
          }
        }
      }
    }
    if ((!d520) && u_is_con(u_field(s519, 0), &n_syntax_46_TMeta)) {
      u_value v_i = u_field(u_field(s519, 0), 0);
      d520 = 1;
      {
        p_check_46_bind_meta(v_i, v_b, r_s);
      }
    }
    if ((!d520) && u_is_con(u_field(s519, 1), &n_syntax_46_TMeta)) {
      u_value v_j = u_field(u_field(s519, 1), 0);
      d520 = 1;
      {
        p_check_46_bind_meta(v_j, v_a, r_s);
      }
    }
    if ((!d520) && (u_is_con(u_field(s519, 0), &n_syntax_46_TParam) && u_is_con(u_field(s519, 1), &n_syntax_46_TParam))) {
      u_value v_x = u_field(u_field(s519, 0), 0);
      u_value v_y = u_field(u_field(s519, 1), 0);
      d520 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(v_x, v_y))) {
          {
            u_raise(u_mk_con(&n_check_46_CannotUnify, 2, v_a, v_b), "check.TypeError");
          }
        }
      }
    }
    if ((!d520) && (u_is_con(u_field(s519, 0), &n_syntax_46_TSort) && u_is_con(u_field(s519, 1), &n_syntax_46_TSort))) {
      u_value v_f = u_field(u_field(s519, 0), 0);
      u_value v_xs = u_field(u_field(s519, 0), 1);
      u_value v_g = u_field(u_field(s519, 1), 0);
      u_value v_ys = u_field(u_field(s519, 1), 1);
      d520 = 1;
      {
        u_value t521 = u_eq(v_f, v_g);
        if (u_truth(t521)) {
          t521 = u_eq(f_list_46_length_1(v_xs), f_list_46_length_1(v_ys));
        }
        if (u_truth(t521)) {
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
    if ((!d520) && (u_is_con(u_field(s519, 0), &n_syntax_46_TFun) && u_is_con(u_field(s519, 1), &n_syntax_46_TFun))) {
      u_value v_xs = u_field(u_field(s519, 0), 0);
      u_value v_r1 = u_field(u_field(s519, 0), 1);
      u_value v_ys = u_field(u_field(s519, 1), 0);
      u_value v_r2 = u_field(u_field(s519, 1), 1);
      d520 = 1;
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
    if ((!d520) && (u_is_con(u_field(s519, 0), &n_syntax_46_TTuple) && u_is_con(u_field(s519, 1), &n_syntax_46_TTuple))) {
      u_value v_xs = u_field(u_field(s519, 0), 0);
      u_value v_ys = u_field(u_field(s519, 1), 0);
      d520 = 1;
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
    if (!d520) {
      d520 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CannotUnify, 2, f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t1), f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_t2)), "check.TypeError");
      }
    }
    if (!d520) {
      u_no_case(s519);
    }
  }
}

void p_check_46_unifies(u_value v_xs, u_value v_ys, u_value *r_s) {
  {
    u_value s522 = u_mk_tuple(2, v_xs, v_ys);
    int d523 = 0;
    if ((!d523) && (u_is_con(u_field(s522, 0), &n_list_46_Cons) && u_is_con(u_field(s522, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s522, 0), 0);
      u_value v_xs1 = u_field(u_field(s522, 0), 1);
      u_value v_y = u_field(u_field(s522, 1), 0);
      u_value v_ys1 = u_field(u_field(s522, 1), 1);
      d523 = 1;
      {
        p_check_46_unify(v_x, v_y, r_s);
        p_check_46_unifies(v_xs1, v_ys1, r_s);
      }
    }
    if ((!d523) && (u_is_con(u_field(s522, 0), &n_list_46_Nil) && u_is_con(u_field(s522, 1), &n_list_46_Nil))) {
      d523 = 1;
      {
      }
    }
    if (!d523) {
      d523 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_xs), f_list_46_length_1(v_ys)), "check.TypeError");
      }
    }
    if (!d523) {
      u_no_case(s522);
    }
  }
}

void p_check_46_fresh_list(u_value v_xs, u_value *r_ts, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s524 = v_xs;
    int d525 = 0;
    if ((!d525) && u_is_con(s524, &n_list_46_Nil)) {
      d525 = 1;
      {
        u_value t526 = u_mk_con(&n_list_46_Nil, 0);
        *r_ts = t526;
      }
    }
    if ((!d525) && u_is_con(s524, &n_list_46_Cons)) {
      u_value v_more = u_field(s524, 1);
      d525 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_fresh_list(v_more, &v_rest, r_s);
        u_value t527 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rest);
        *r_ts = t527;
      }
    }
    if (!d525) {
      u_no_case(s524);
    }
  }
}

void p_check_46_instance(u_value v_ps, u_value *r_m, u_value *r_s) {
  {
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value s528 = v_ps;
    int d529 = 0;
    if ((!d529) && u_is_con(s528, &n_list_46_Nil)) {
      d529 = 1;
      {
        u_value t530 = u_mk_con(&n_list_46_Nil, 0);
        *r_m = t530;
      }
    }
    if ((!d529) && u_is_con(s528, &n_list_46_Cons)) {
      u_value v_p = u_field(s528, 0);
      u_value v_qs = u_field(s528, 1);
      d529 = 1;
      {
        p_check_46_fresh(&v_t, r_s);
        p_check_46_instance(v_qs, &v_rest, r_s);
        u_value t531 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_p, v_t), v_rest);
        *r_m = t531;
      }
    }
    if (!d529) {
      u_no_case(s528);
    }
  }
}

void p_check_46_instantiate(u_value v_sc, u_value *r_t, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value s532 = v_sc;
    int d533 = 0;
    if (!d533) {
      u_value v_ps = u_field(s532, 0);
      u_value v_u = u_field(s532, 1);
      d533 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        u_value t534 = f_check_46_subst_2(f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u), v_m);
        *r_t = t534;
      }
    }
    if (!d533) {
      u_no_case(s532);
    }
  }
}

void p_check_46_unique(u_value v_ns) {
  {
    u_value s535 = f_list_46_repeated_1(v_ns);
    int d536 = 0;
    if ((!d536) && u_is_con(s535, &n_option_46_Some)) {
      u_value v_n = u_field(s535, 0);
      d536 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, f_syntax_46_qualified_1(v_n)), "check.TypeError");
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

void p_check_46_unique_ids(u_value v_ns) {
  {
    u_value s537 = f_list_46_repeated_1(v_ns);
    int d538 = 0;
    if ((!d538) && u_is_con(s537, &n_option_46_Some)) {
      u_value v_n = u_field(s537, 0);
      d538 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Duplicate, 1, v_n), "check.TypeError");
      }
    }
    if ((!d538) && u_is_con(s537, &n_option_46_None)) {
      d538 = 1;
      {
      }
    }
    if (!d538) {
      u_no_case(s537);
    }
  }
}

void p_check_46_one_of(u_value v_n, u_value v_ns, u_value *r_m) {
  {
    u_value s539 = v_ns;
    int d540 = 0;
    if ((!d540) && (u_is_con(s539, &n_list_46_Cons) && u_is_con(u_field(s539, 1), &n_list_46_Nil))) {
      u_value v_k = u_field(s539, 0);
      d540 = 1;
      {
        u_value t541 = v_k;
        *r_m = t541;
      }
    }
    if ((!d540) && u_is_con(s539, &n_list_46_Nil)) {
      d540 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if (!d540) {
      d540 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ns), "check.TypeError");
      }
    }
    if (!d540) {
      u_no_case(s539);
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
    u_value s542 = v_t;
    int d543 = 0;
    if ((!d543) && u_is_con(s542, &n_syntax_46_THole)) {
      d543 = 1;
      {
        p_check_46_fresh(r_u, r_s);
      }
    }
    if ((!d543) && u_is_con(s542, &n_syntax_46_TParam)) {
      d543 = 1;
      {
        u_value t544 = v_t;
        *r_u = t544;
      }
    }
    if ((!d543) && u_is_con(s542, &n_syntax_46_TMeta)) {
      d543 = 1;
      {
        u_value t545 = v_t;
        *r_u = t545;
      }
    }
    if ((!d543) && u_is_con(s542, &n_syntax_46_TFun)) {
      u_value v_args = u_field(s542, 0);
      u_value v_r = u_field(s542, 1);
      d543 = 1;
      {
        p_check_46_normalizes(v_args, &v_us, r_s);
        p_check_46_normalize(v_r, &v_v, r_s);
        u_value t546 = u_mk_con(&n_syntax_46_TFun, 2, v_us, v_v);
        *r_u = t546;
      }
    }
    if ((!d543) && u_is_con(s542, &n_syntax_46_TTuple)) {
      u_value v_ts = u_field(s542, 0);
      d543 = 1;
      {
        p_check_46_normalizes(v_ts, &v_us, r_s);
        u_value t547 = u_mk_con(&n_syntax_46_TTuple, 1, v_us);
        *r_u = t547;
      }
    }
    if ((!d543) && u_is_con(s542, &n_syntax_46_TSort)) {
      u_value v_n = u_field(s542, 0);
      u_value v_args = u_field(s542, 1);
      d543 = 1;
      {
        if (u_truth(f_check_46_basic_1(v_n))) {
          {
            p_check_46_arguments(v_n, u_mk_int(0), v_args);
            u_value t548 = v_t;
            *r_u = t548;
          }
        } else {
          {
            u_value s550 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam549, 1, *r_s));
            int d551 = 0;
            if ((!d551) && (u_is_con(s550, &n_list_46_Cons) && u_is_con(u_field(s550, 1), &n_list_46_Nil))) {
              u_value v_m = u_field(s550, 0);
              d551 = 1;
              {
                p_check_46_normalize_sort(v_m, v_args, r_u, r_s);
              }
            }
            if ((!d551) && u_is_con(s550, &n_list_46_Nil)) {
              d551 = 1;
              {
                u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
              }
            }
            if (!d551) {
              u_value v_ms = s550;
              d551 = 1;
              {
                u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ms), "check.TypeError");
              }
            }
            if (!d551) {
              u_no_case(s550);
            }
          }
        }
      }
    }
    if (!d543) {
      u_no_case(s542);
    }
  }
}

void p_check_46_normalize_sort(u_value v_n, u_value v_args, u_value *r_u, u_value *r_s) {
  {
    u_value v_us = NULL;
    u_value s552 = f_ctx_46_data_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
    int d553 = 0;
    if ((!d553) && u_is_con(s552, &n_option_46_Some)) {
      u_value v_k = u_field(s552, 0);
      d553 = 1;
      {
        p_check_46_arguments(v_n, v_k, v_args);
        p_check_46_normalizes(v_args, &v_us, r_s);
        u_value t554 = u_mk_con(&n_syntax_46_TSort, 2, v_n, v_us);
        *r_u = t554;
      }
    }
    if ((!d553) && u_is_con(s552, &n_option_46_None)) {
      d553 = 1;
      {
        u_value s555 = f_ctx_46_alias_of_2(f_check_46_State_46_ctx_1(*r_s), v_n);
        int d556 = 0;
        if ((!d556) && u_is_con(s555, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s555, 0), 0);
          u_value v_rhs = u_field(u_field(s555, 0), 1);
          d556 = 1;
          {
            p_check_46_arguments(v_n, f_list_46_length_1(v_ps), v_args);
            p_check_46_normalizes(v_args, &v_us, r_s);
            u_value t557 = f_check_46_subst_2(v_rhs, f_list_46_zip_2(f_check_46_params_1(v_ps), v_us));
            *r_u = t557;
          }
        }
        if ((!d556) && u_is_con(s555, &n_option_46_None)) {
          d556 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownSort, 1, v_n), "check.TypeError");
          }
        }
        if (!d556) {
          u_no_case(s555);
        }
      }
    }
    if (!d553) {
      u_no_case(s552);
    }
  }
}

void p_check_46_normalizes(u_value v_ts, u_value *r_us, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value v_rest = NULL;
    u_value s558 = v_ts;
    int d559 = 0;
    if ((!d559) && u_is_con(s558, &n_list_46_Nil)) {
      d559 = 1;
      {
        u_value t560 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t560;
      }
    }
    if ((!d559) && u_is_con(s558, &n_list_46_Cons)) {
      u_value v_t = u_field(s558, 0);
      u_value v_more = u_field(s558, 1);
      d559 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalizes(v_more, &v_rest, r_s);
        u_value t561 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t561;
      }
    }
    if (!d559) {
      u_no_case(s558);
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
    u_value s562 = v_ts;
    int d563 = 0;
    if ((!d563) && u_is_con(s562, &n_list_46_Nil)) {
      d563 = 1;
      {
        u_value t564 = u_mk_con(&n_list_46_Nil, 0);
        *r_us = t564;
      }
    }
    if ((!d563) && u_is_con(s562, &n_list_46_Cons)) {
      u_value v_t = u_field(s562, 0);
      u_value v_more = u_field(s562, 1);
      d563 = 1;
      {
        p_check_46_known(v_t, v_what, &v_u, r_s);
        p_check_46_knowns(v_more, v_what, &v_rest, r_s);
        u_value t565 = u_mk_con(&n_list_46_Cons, 2, v_u, v_rest);
        *r_us = t565;
      }
    }
    if (!d563) {
      u_no_case(s562);
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
    u_value s566 = v_xs;
    int d567 = 0;
    if ((!d567) && u_is_con(s566, &n_list_46_Nil)) {
      d567 = 1;
      {
        u_value t568 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t568;
      }
    }
    if ((!d567) && (u_is_con(s566, &n_list_46_Cons) && u_is_con(u_field(s566, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s566, 0), 0);
      u_value v_t = u_field(u_field(s566, 0), 1);
      u_value v_more = u_field(s566, 1);
      d567 = 1;
      {
        p_check_46_normalize(v_t, &v_u, r_s);
        p_check_46_normalize_vars(v_more, &v_rest, r_s);
        u_value t569 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t569;
      }
    }
    if (!d567) {
      u_no_case(s566);
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
    u_value s570 = v_xs;
    int d571 = 0;
    if ((!d571) && u_is_con(s570, &n_list_46_Nil)) {
      d571 = 1;
      {
        u_value t572 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t572;
      }
    }
    if ((!d571) && (u_is_con(s570, &n_list_46_Cons) && u_is_con(u_field(s570, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s570, 0), 0);
      u_value v_t = u_field(u_field(s570, 0), 1);
      u_value v_more = u_field(s570, 1);
      d571 = 1;
      {
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_known_vars(v_more, &v_rest, r_s);
        u_value t573 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), v_rest);
        *r_ys = t573;
      }
    }
    if (!d571) {
      u_no_case(s570);
    }
  }
}

void p_check_46_resolve_name(u_value v_s, u_value v_env, u_value v_n, u_value v_k, u_value *r_m, u_value *r_sc) {
  {
    u_value s574 = f_check_46_candidates_3(v_s, v_env, v_n);
    int d575 = 0;
    if ((!d575) && u_is_con(s574, &n_list_46_Nil)) {
      d575 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
      }
    }
    if ((!d575) && (u_is_con(s574, &n_list_46_Cons) && u_is_con(u_field(s574, 1), &n_list_46_Nil))) {
      u_value v_m1 = u_field(u_field(s574, 0), 0);
      u_value v_one = u_field(u_field(s574, 0), 1);
      d575 = 1;
      {
        u_value t576 = v_m1;
        u_value t577 = v_one;
        *r_m = t576;
        *r_sc = t577;
      }
    }
    if (!d575) {
      u_value v_cs = s574;
      d575 = 1;
      {
        if (u_truth(u_lt(v_k, u_mk_int(0)))) {
          {
            u_raise(u_mk_con(&n_check_46_OverloadedUnapplied, 1, v_n), "check.TypeError");
          }
        }
        u_value s578 = f_check_46_with_arity_2(v_cs, v_k);
        int d579 = 0;
        if ((!d579) && (u_is_con(s578, &n_list_46_Cons) && u_is_con(u_field(s578, 1), &n_list_46_Nil))) {
          u_value v_m1 = u_field(u_field(s578, 0), 0);
          u_value v_one = u_field(u_field(s578, 0), 1);
          d579 = 1;
          {
            u_value t580 = v_m1;
            u_value t581 = v_one;
            *r_m = t580;
            *r_sc = t581;
          }
        }
        if ((!d579) && u_is_con(s578, &n_list_46_Nil)) {
          d579 = 1;
          {
            u_raise(u_mk_con(&n_check_46_WrongArity, 2, v_n, v_k), "check.TypeError");
          }
        }
        if (!d579) {
          u_value v_more = s578;
          d579 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, f_list_46_firsts_1(v_more)), "check.TypeError");
          }
        }
        if (!d579) {
          u_no_case(s578);
        }
      }
    }
    if (!d575) {
      u_no_case(s574);
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
    u_value t582 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_u);
    *r_x = t582;
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
    u_value s583 = v_e;
    int d584 = 0;
    if ((!d584) && u_is_con(s583, &n_syntax_46_ELit)) {
      u_value v_v = u_field(s583, 0);
      d584 = 1;
      {
        u_value t585 = v_e;
        u_value t586 = f_check_46_literal_1(v_v);
        *r_e1 = t585;
        *r_t = t586;
      }
    }
    if ((!d584) && (u_is_con(s583, &n_syntax_46_EVar) && u_is_con(u_field(s583, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s583, 0), 0);
      u_value v_a = u_field(u_field(s583, 0), 1);
      d584 = 1;
      {
        p_check_46_resolve_name(*r_s, v_env, v_n, f_prelude_46__45__1(u_mk_int(1)), &v_m, &v_sc);
        p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
        u_value t587 = u_mk_con(&n_syntax_46_EVar, 1, v_x);
        u_value t588 = f_syntax_46_Var_46_typ_1(v_x);
        *r_e1 = t587;
        *r_t = t588;
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s583, 0);
      u_value v_args = u_field(s583, 1);
      d584 = 1;
      {
        p_check_46_application(v_env, v_f, v_args, r_e1, r_t, r_s);
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_ELam)) {
      u_value v_bound = u_field(s583, 0);
      u_value v_body = u_field(s583, 1);
      d584 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t589 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, v_b);
        u_value t590 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), v_u);
        *r_e1 = t589;
        *r_t = t590;
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_ELet)) {
      u_value v_bound = u_field(s583, 0);
      u_value v_body = u_field(s583, 1);
      d584 = 1;
      {
        p_check_46_formals(f_list_46_firsts_1(v_bound), &v_xs, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_bound), f_syntax_46_types_1(v_xs), &v_es, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_generalized_3(f_check_46_State_46_sub_1(*r_s), v_env, v_xs), v_env), v_body, &v_b, &v_u, r_s);
        u_value t591 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        u_value t592 = v_u;
        *r_e1 = t591;
        *r_t = t592;
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_EIte)) {
      u_value v_cond = u_field(s583, 0);
      u_value v_left = u_field(s583, 1);
      u_value v_right = u_field(s583, 2);
      d584 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(v_env, v_left, &v_l, &v_u, r_s);
        p_check_46_check(v_env, v_right, v_u, &v_b, r_s);
        u_value t593 = u_mk_con(&n_syntax_46_EIte, 3, v_c, v_l, v_b);
        u_value t594 = v_u;
        *r_e1 = t593;
        *r_t = t594;
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s583, 0);
      u_value v_bound = u_field(s583, 1);
      u_value v_lists = u_field(s583, 2);
      u_value v_cond = u_field(s583, 3);
      d584 = 1;
      {
        p_check_46_formals(v_bound, &v_xs, r_s);
        p_check_46_same_length(f_list_46_length_1(v_xs), f_list_46_length_1(v_lists));
        p_check_46_generators(v_env, v_xs, v_lists, &v_es, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_infer(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_result, &v_b, &v_u, r_s);
        u_value t595 = u_mk_con(&n_syntax_46_EComp, 4, v_b, v_xs, v_es, v_c);
        u_value t596 = f_check_46_tlist_1(v_u);
        *r_e1 = t595;
        *r_t = t596;
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s583, 0);
      d584 = 1;
      {
        p_check_46_infers(v_env, v_items, &v_es, &v_ts, r_s);
        u_value t597 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
        u_value t598 = u_mk_con(&n_syntax_46_TTuple, 1, v_ts);
        *r_e1 = t597;
        *r_t = t598;
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s583, 0);
      u_value v_a = u_field(s583, 1);
      d584 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_normalize(v_a, &v_w, r_s);
        p_check_46_unify(v_u, v_w, r_s);
        p_check_46_fresh(r_t, r_s);
        u_value t599 = u_mk_con(&n_syntax_46_ERaise, 2, v_c, v_u);
        *r_e1 = t599;
      }
    }
    if ((!d584) && u_is_con(s583, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s583, 0);
      u_value v_cases = u_field(s583, 1);
      d584 = 1;
      {
        p_check_46_fresh(r_t, r_s);
        p_check_46_infer(v_env, v_arg, &v_c, &v_u, r_s);
        p_check_46_expr_branches(v_env, v_cases, v_u, *r_t, &v_bs, r_s);
        u_value t600 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e601 = &t600;
        u_value l602 = v_bs;
        while (u_is_con(l602, &n_list_46_Cons)) {
          u_value v_br = u_field(l602, 0);
          if (u_truth(u_mk_bool(1))) {
            e601 = u_append(e601, f_check_46_case_of_1(v_br));
          }
          l602 = u_field(l602, 1);
        }
        p_check_46_coverage(*r_s, v_u, t600);
        u_value t603 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, v_bs);
        *r_e1 = t603;
      }
    }
    if (!d584) {
      u_no_case(s583);
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
    u_value s604 = v_f;
    int d605 = 0;
    if ((!d605) && (u_is_con(s604, &n_syntax_46_EVar) && (u_is_con(u_field(s604, 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(s604, 0), 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(u_field(s604, 0), 0), 0), &n_option_46_None))))) {
      u_value v_b = u_field(u_field(u_field(s604, 0), 0), 1);
      u_value v_a = u_field(u_field(s604, 0), 1);
      u_value t606 = f_prelude_46__62__2(u_str_size(v_b), u_mk_int(1));
      if (u_truth(t606)) {
        t606 = u_eq(u_str_get(v_b, u_mk_int(0)), u_mk_char(46));
      }
      if (u_truth(t606)) {
        d605 = 1;
        {
          p_check_46_field_access(v_env, v_b, v_a, v_args, r_e1, r_t, r_s);
        }
      }
    }
    if ((!d605) && (u_is_con(s604, &n_syntax_46_EVar) && u_is_con(u_field(s604, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s604, 0), 0);
      u_value v_a = u_field(u_field(s604, 0), 1);
      d605 = 1;
      {
        if (u_truth(f_prelude_46__62__2(f_list_46_length_1(f_check_46_candidates_3(*r_s, v_env, v_n)), u_mk_int(1)))) {
          {
            p_check_46_resolve_name(*r_s, v_env, v_n, f_list_46_length_1(v_args), &v_m, &v_sc);
            p_check_46_use(v_m, v_a, v_sc, &v_x, r_s);
            p_check_46_apply(v_env, f_syntax_46_Var_46_typ_1(v_x), v_args, &v_args1, r_t, r_s);
            u_value t607 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), v_args1);
            *r_e1 = t607;
          }
        } else {
          {
            p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
            p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
            u_value t608 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
            *r_e1 = t608;
          }
        }
      }
    }
    if (!d605) {
      d605 = 1;
      {
        p_check_46_infer(v_env, v_f, &v_f1, &v_u, r_s);
        p_check_46_apply(v_env, v_u, v_args, &v_args1, r_t, r_s);
        u_value t609 = u_mk_con(&n_syntax_46_EApp, 2, v_f1, v_args1);
        *r_e1 = t609;
      }
    }
    if (!d605) {
      u_no_case(s604);
    }
  }
}

void p_check_46_field_access(u_value v_env, u_value v_f, u_value v_a, u_value v_args, u_value *r_e1, u_value *r_t, u_value *r_s) {
  {
    u_value v_arg1 = NULL;
    u_value v_u = NULL;
    u_value v_acc = NULL;
    u_value v_x = NULL;
    u_value s610 = v_args;
    int d611 = 0;
    if ((!d611) && (u_is_con(s610, &n_list_46_Cons) && u_is_con(u_field(s610, 1), &n_list_46_Nil))) {
      u_value v_arg = u_field(s610, 0);
      d611 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_arg1, &v_u, r_s);
        u_value s612 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(*r_s), v_u);
        int d613 = 0;
        if ((!d613) && (u_is_con(s612, &n_syntax_46_TSort) && (u_is_con(u_field(s612, 0), &n_syntax_46_Name) && u_is_con(u_field(u_field(s612, 0), 0), &n_option_46_Some)))) {
          u_value v_m = u_field(u_field(u_field(s612, 0), 0), 0);
          u_value v_tn = u_field(u_field(s612, 0), 1);
          d613 = 1;
          {
            u_value t614 = u_mk_con(&n_syntax_46_Name, 2, u_mk_con(&n_option_46_Some, 1, v_m), u_concat(v_tn, v_f));
            v_acc = t614;
          }
        }
        if (!d613) {
          u_value v_v = s612;
          d613 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownFieldType, 2, v_f, v_v), "check.TypeError");
          }
        }
        if (!d613) {
          u_no_case(s612);
        }
        u_value s615 = f_ctx_46_schemes_of_2(f_check_46_State_46_ctx_1(*r_s), v_acc);
        int d616 = 0;
        if ((!d616) && (u_is_con(s615, &n_list_46_Cons) && u_is_con(u_field(s615, 1), &n_list_46_Nil))) {
          u_value v_sc = u_field(s615, 0);
          d616 = 1;
          {
            p_check_46_use(v_acc, v_a, v_sc, &v_x, r_s);
          }
        }
        if (!d616) {
          d616 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownField, 1, v_acc), "check.TypeError");
          }
        }
        if (!d616) {
          u_no_case(s615);
        }
        p_check_46_fresh(r_t, r_s);
        p_check_46_unify(f_syntax_46_Var_46_typ_1(v_x), f_check_46_fun_2(u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)), *r_t), r_s);
        u_value t617 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), u_mk_con(&n_list_46_Cons, 2, v_arg1, u_mk_con(&n_list_46_Nil, 0)));
        *r_e1 = t617;
      }
    }
    if (!d611) {
      d611 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, u_mk_int(1), f_list_46_length_1(v_args)), "check.TypeError");
      }
    }
    if (!d611) {
      u_no_case(s610);
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
    u_value s618 = u_mk_tuple(2, v_es, v_ts);
    int d619 = 0;
    if ((!d619) && (u_is_con(u_field(s618, 0), &n_list_46_Cons) && u_is_con(u_field(s618, 1), &n_list_46_Cons))) {
      u_value v_e = u_field(u_field(s618, 0), 0);
      u_value v_more = u_field(u_field(s618, 0), 1);
      u_value v_t = u_field(u_field(s618, 1), 0);
      u_value v_ts1 = u_field(u_field(s618, 1), 1);
      d619 = 1;
      {
        p_check_46_check(v_env, v_e, v_t, &v_e1, r_s);
        p_check_46_checks(v_env, v_more, v_ts1, &v_rest, r_s);
        u_value t620 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        *r_es1 = t620;
      }
    }
    if ((!d619) && (u_is_con(u_field(s618, 0), &n_list_46_Nil) && u_is_con(u_field(s618, 1), &n_list_46_Nil))) {
      d619 = 1;
      {
        u_value t621 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t621;
      }
    }
    if (!d619) {
      d619 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_es)), "check.TypeError");
      }
    }
    if (!d619) {
      u_no_case(s618);
    }
  }
}

void p_check_46_infers(u_value v_env, u_value v_es, u_value *r_es1, u_value *r_ts, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value v_t = NULL;
    u_value v_rest = NULL;
    u_value v_us = NULL;
    u_value s622 = v_es;
    int d623 = 0;
    if ((!d623) && u_is_con(s622, &n_list_46_Nil)) {
      d623 = 1;
      {
        u_value t624 = u_mk_con(&n_list_46_Nil, 0);
        u_value t625 = u_mk_con(&n_list_46_Nil, 0);
        *r_es1 = t624;
        *r_ts = t625;
      }
    }
    if ((!d623) && u_is_con(s622, &n_list_46_Cons)) {
      u_value v_e = u_field(s622, 0);
      u_value v_more = u_field(s622, 1);
      d623 = 1;
      {
        p_check_46_infer(v_env, v_e, &v_e1, &v_t, r_s);
        p_check_46_infers(v_env, v_more, &v_rest, &v_us, r_s);
        u_value t626 = u_mk_con(&n_list_46_Cons, 2, v_e1, v_rest);
        u_value t627 = u_mk_con(&n_list_46_Cons, 2, v_t, v_us);
        *r_es1 = t626;
        *r_ts = t627;
      }
    }
    if (!d623) {
      u_no_case(s622);
    }
  }
}

void p_check_46_generators(u_value v_env, u_value v_xs, u_value v_lists, u_value *r_lists1, u_value *r_s) {
  {
    u_value v_l = NULL;
    u_value v_rest = NULL;
    u_value s628 = u_mk_tuple(2, v_xs, v_lists);
    int d629 = 0;
    if ((!d629) && (u_is_con(u_field(s628, 0), &n_list_46_Cons) && u_is_con(u_field(s628, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s628, 0), 0);
      u_value v_ys = u_field(u_field(s628, 0), 1);
      u_value v_e = u_field(u_field(s628, 1), 0);
      u_value v_es = u_field(u_field(s628, 1), 1);
      d629 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tlist_1(f_syntax_46_Var_46_typ_1(v_x)), &v_l, r_s);
        p_check_46_generators(f_list_46__43__43__2(f_check_46_bind_1(u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0))), v_env), v_ys, v_es, &v_rest, r_s);
        u_value t630 = u_mk_con(&n_list_46_Cons, 2, v_l, v_rest);
        *r_lists1 = t630;
      }
    }
    if (!d629) {
      d629 = 1;
      {
        u_value t631 = u_mk_con(&n_list_46_Nil, 0);
        *r_lists1 = t631;
      }
    }
    if (!d629) {
      u_no_case(s628);
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
    u_value s632 = v_cases;
    int d633 = 0;
    if ((!d633) && u_is_con(s632, &n_list_46_Nil)) {
      d633 = 1;
      {
        u_value t634 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t634;
      }
    }
    if ((!d633) && (u_is_con(s632, &n_list_46_Cons) && u_is_con(u_field(s632, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s632, 0), 0);
      u_value v_g = u_field(u_field(s632, 0), 1);
      u_value v_b = u_field(u_field(s632, 0), 2);
      u_value v_more = u_field(s632, 1);
      d633 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_check(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_b, v_r, &v_b1, r_s);
        p_check_46_expr_branches(v_env, v_more, v_t, v_r, &v_rest, r_s);
        u_value t635 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t635;
      }
    }
    if (!d633) {
      u_no_case(s632);
    }
  }
}

void p_check_46_guard(u_value v_env, u_value v_g, u_value *r_g1, u_value *r_s) {
  {
    u_value v_e1 = NULL;
    u_value s636 = v_g;
    int d637 = 0;
    if ((!d637) && u_is_con(s636, &n_option_46_Some)) {
      u_value v_e = u_field(s636, 0);
      d637 = 1;
      {
        p_check_46_check(v_env, v_e, f_check_46_tbool_0(), &v_e1, r_s);
        u_value t638 = u_mk_con(&n_option_46_Some, 1, v_e1);
        *r_g1 = t638;
      }
    }
    if ((!d637) && u_is_con(s636, &n_option_46_None)) {
      d637 = 1;
      {
        u_value t639 = u_mk_con(&n_option_46_None, 0);
        *r_g1 = t639;
      }
    }
    if (!d637) {
      u_no_case(s636);
    }
  }
}

void p_check_46_coverage(u_value v_s, u_value v_t, u_value v_cs) {
  {
    p_check_46_reachable(v_s, v_t, u_mk_con(&n_list_46_Nil, 0), v_cs);
    u_value t640 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e641 = &t640;
    u_value l642 = f_check_46_unguarded_1(v_cs);
    while (u_is_con(l642, &n_list_46_Cons)) {
      u_value v_p = u_field(l642, 0);
      if (u_truth(u_mk_bool(1))) {
        e641 = u_append(e641, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)));
      }
      l642 = u_field(l642, 1);
    }
    u_value s643 = f_check_46_missing_4(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t640, u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
    int d644 = 0;
    if ((!d644) && (u_is_con(s643, &n_option_46_Some) && u_is_con(u_field(s643, 0), &n_list_46_Cons))) {
      u_value v_w = u_field(u_field(s643, 0), 0);
      d644 = 1;
      {
        u_raise(u_mk_con(&n_check_46_NotExhaustive, 1, v_w), "check.TypeError");
      }
    }
    if (!d644) {
      d644 = 1;
      {
      }
    }
    if (!d644) {
      u_no_case(s643);
    }
  }
}

u_value f_check_46_unguarded_1(u_value v_cs) {
  u_value s645 = v_cs;
  u_value t646 = NULL;
  if ((!t646) && u_is_con(s645, &n_list_46_Nil)) {
    t646 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t646) && (u_is_con(s645, &n_list_46_Cons) && u_equals(u_field(u_field(s645, 0), 1), u_mk_bool(1)))) {
    u_value v_p = u_field(u_field(s645, 0), 0);
    u_value v_rest = u_field(s645, 1);
    t646 = u_mk_con(&n_list_46_Cons, 2, v_p, f_check_46_unguarded_1(v_rest));
  }
  if ((!t646) && (u_is_con(s645, &n_list_46_Cons) && u_equals(u_field(u_field(s645, 0), 1), u_mk_bool(0)))) {
    u_value v_rest = u_field(s645, 1);
    t646 = f_check_46_unguarded_1(v_rest);
  }
  if (!t646) {
    u_no_case(s645);
  }
  return t646;
}

void p_check_46_reachable(u_value v_s, u_value v_t, u_value v_before, u_value v_cs) {
  {
    u_value s647 = v_cs;
    int d648 = 0;
    if ((!d648) && u_is_con(s647, &n_list_46_Nil)) {
      d648 = 1;
      {
      }
    }
    if ((!d648) && u_is_con(s647, &n_list_46_Cons)) {
      u_value v_p = u_field(u_field(s647, 0), 0);
      u_value v_open = u_field(u_field(s647, 0), 1);
      u_value v_rest = u_field(s647, 1);
      d648 = 1;
      {
        u_value t649 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e650 = &t649;
        u_value l651 = v_before;
        while (u_is_con(l651, &n_list_46_Cons)) {
          u_value v_q = u_field(l651, 0);
          if (u_truth(u_mk_bool(1))) {
            e650 = u_append(e650, u_mk_con(&n_list_46_Cons, 2, v_q, u_mk_con(&n_list_46_Nil, 0)));
          }
          l651 = u_field(l651, 1);
        }
        if (u_truth(f_prelude_46_not_1(f_check_46_useful_5(f_check_46_State_46_ctx_1(v_s), f_check_46_State_46_sub_1(v_s), t649, u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)))))) {
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
    if (!d648) {
      u_no_case(s647);
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
    u_value s652 = v_p;
    int d653 = 0;
    if ((!d653) && u_is_con(s652, &n_syntax_46_PWild)) {
      d653 = 1;
      {
        u_value t654 = v_p;
        u_value t655 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t654;
        *r_xs = t655;
      }
    }
    if ((!d653) && u_is_con(s652, &n_syntax_46_PLit)) {
      u_value v_v = u_field(s652, 0);
      d653 = 1;
      {
        p_check_46_unify(f_check_46_literal_1(v_v), v_t, r_s);
        u_value t656 = v_p;
        u_value t657 = u_mk_con(&n_list_46_Nil, 0);
        *r_p1 = t656;
        *r_xs = t657;
      }
    }
    if ((!d653) && (u_is_con(s652, &n_syntax_46_PVar) && u_is_con(u_field(s652, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s652, 0), 0);
      u_value v_a = u_field(u_field(s652, 0), 1);
      d653 = 1;
      {
        if (u_truth(f_prelude_46__33__61__2(f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam658, 1, *r_s)), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            p_check_46_binders(u_mk_con(&n_syntax_46_PApp, 2, v_n, u_mk_con(&n_list_46_Nil, 0)), v_t, r_p1, r_xs, r_s);
          }
        } else {
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_u, v_t, r_s);
            u_value t659 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u));
            u_value t660 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, v_n, v_u), u_mk_con(&n_list_46_Nil, 0));
            *r_p1 = t659;
            *r_xs = t660;
          }
        }
      }
    }
    if ((!d653) && u_is_con(s652, &n_syntax_46_PApp)) {
      u_value v_n = u_field(s652, 0);
      u_value v_args = u_field(s652, 1);
      d653 = 1;
      {
        u_value s662 = f_check_46_visible_names_3(*r_s, v_n, u_mk_closure(lam661, 1, *r_s));
        int d663 = 0;
        if ((!d663) && (u_is_con(s662, &n_list_46_Cons) && u_is_con(u_field(s662, 1), &n_list_46_Nil))) {
          u_value v_k1 = u_field(s662, 0);
          d663 = 1;
          {
            u_value t664 = v_k1;
            v_k = t664;
          }
        }
        if ((!d663) && u_is_con(s662, &n_list_46_Nil)) {
          d663 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d663) {
          u_value v_ks = s662;
          d663 = 1;
          {
            u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_n, v_ks), "check.TypeError");
          }
        }
        if (!d663) {
          u_no_case(s662);
        }
        u_value s665 = f_ctx_46_constr_of_2(f_check_46_State_46_ctx_1(*r_s), v_k);
        int d666 = 0;
        if ((!d666) && u_is_con(s665, &n_option_46_Some)) {
          u_value v_ps = u_field(u_field(s665, 0), 0);
          u_value v_ks = u_field(u_field(s665, 0), 1);
          u_value v_r = u_field(u_field(s665, 0), 2);
          d666 = 1;
          {
            p_check_46_same_length(f_list_46_length_1(v_ks), f_list_46_length_1(v_args));
            p_check_46_instance(v_ps, &v_m, r_s);
            p_check_46_unify(f_check_46_subst_2(v_r, v_m), v_t, r_s);
            u_value t667 = u_mk_con(&n_list_46_Nil, 0);
            u_value *e668 = &t667;
            u_value l669 = v_ks;
            while (u_is_con(l669, &n_list_46_Cons)) {
              u_value v_kt = u_field(l669, 0);
              if (u_truth(u_mk_bool(1))) {
                e668 = u_append(e668, f_check_46_subst_2(v_kt, v_m));
              }
              l669 = u_field(l669, 1);
            }
            p_check_46_binder_list(v_args, t667, &v_qs, r_xs, r_s);
            u_value t670 = u_mk_con(&n_syntax_46_PApp, 2, v_k, v_qs);
            *r_p1 = t670;
          }
        }
        if ((!d666) && u_is_con(s665, &n_option_46_None)) {
          d666 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownConstructor, 1, v_n), "check.TypeError");
          }
        }
        if (!d666) {
          u_no_case(s665);
        }
      }
    }
    if ((!d653) && u_is_con(s652, &n_syntax_46_PTuple)) {
      u_value v_items = u_field(s652, 0);
      d653 = 1;
      {
        p_check_46_fresh_list(v_items, &v_ts, r_s);
        p_check_46_unify(u_mk_con(&n_syntax_46_TTuple, 1, v_ts), v_t, r_s);
        p_check_46_binder_list(v_items, v_ts, &v_qs, r_xs, r_s);
        u_value t671 = u_mk_con(&n_syntax_46_PTuple, 1, v_qs);
        *r_p1 = t671;
      }
    }
    if (!d653) {
      u_no_case(s652);
    }
  }
}

void p_check_46_binder_list(u_value v_ps, u_value v_ts, u_value *r_qs, u_value *r_xs, u_value *r_s) {
  {
    u_value v_q = NULL;
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value v_zs = NULL;
    u_value s672 = u_mk_tuple(2, v_ps, v_ts);
    int d673 = 0;
    if ((!d673) && (u_is_con(u_field(s672, 0), &n_list_46_Cons) && u_is_con(u_field(s672, 1), &n_list_46_Cons))) {
      u_value v_p = u_field(u_field(s672, 0), 0);
      u_value v_more = u_field(u_field(s672, 0), 1);
      u_value v_t = u_field(u_field(s672, 1), 0);
      u_value v_ts1 = u_field(u_field(s672, 1), 1);
      d673 = 1;
      {
        p_check_46_binders(v_p, v_t, &v_q, &v_ys, r_s);
        p_check_46_binder_list(v_more, v_ts1, &v_rest, &v_zs, r_s);
        u_value t674 = u_mk_con(&n_list_46_Cons, 2, v_q, v_rest);
        u_value t675 = f_list_46__43__43__2(v_ys, v_zs);
        *r_qs = t674;
        *r_xs = t675;
      }
    }
    if ((!d673) && (u_is_con(u_field(s672, 0), &n_list_46_Nil) && u_is_con(u_field(s672, 1), &n_list_46_Nil))) {
      d673 = 1;
      {
        u_value t676 = u_mk_con(&n_list_46_Nil, 0);
        u_value t677 = u_mk_con(&n_list_46_Nil, 0);
        *r_qs = t676;
        *r_xs = t677;
      }
    }
    if (!d673) {
      d673 = 1;
      {
        u_raise(u_mk_con(&n_check_46_CountMismatch, 2, f_list_46_length_1(v_ts), f_list_46_length_1(v_ps)), "check.TypeError");
      }
    }
    if (!d673) {
      u_no_case(s672);
    }
  }
}

void p_check_46_variable(u_value v_env, u_value v_assignable, u_value v_x, u_value *r_y, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s678 = v_x;
    int d679 = 0;
    if ((!d679) && u_is_con(s678, &n_syntax_46_Var)) {
      u_value v_n = u_field(s678, 0);
      u_value v_a = u_field(s678, 1);
      d679 = 1;
      {
        if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, v_assignable)))) {
          {
            u_raise(u_mk_con(&n_check_46_NotAssignable, 1, v_n), "check.TypeError");
          }
        }
        u_value s680 = f_list_46_lookup_2(v_n, v_env);
        int d681 = 0;
        if ((!d681) && u_is_con(s680, &n_option_46_Some)) {
          u_value v_t = u_field(u_field(s680, 0), 1);
          d681 = 1;
          {
            p_check_46_normalize(v_a, &v_u, r_s);
            p_check_46_unify(v_t, v_u, r_s);
            u_value t682 = u_mk_con(&n_syntax_46_Var, 2, v_n, v_t);
            *r_y = t682;
          }
        }
        if ((!d681) && u_is_con(s680, &n_option_46_None)) {
          d681 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownVariable, 1, v_n), "check.TypeError");
          }
        }
        if (!d681) {
          u_no_case(s680);
        }
      }
    }
    if (!d679) {
      u_no_case(s678);
    }
  }
}

void p_check_46_variables(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s683 = v_xs;
    int d684 = 0;
    if ((!d684) && u_is_con(s683, &n_list_46_Nil)) {
      d684 = 1;
      {
        u_value t685 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t685;
      }
    }
    if ((!d684) && u_is_con(s683, &n_list_46_Cons)) {
      u_value v_x = u_field(s683, 0);
      u_value v_more = u_field(s683, 1);
      d684 = 1;
      {
        p_check_46_variable(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_variables(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t686 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t686;
      }
    }
    if (!d684) {
      u_no_case(s683);
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
    u_value s687 = v_x;
    int d688 = 0;
    if ((!d688) && u_is_con(s687, &n_syntax_46_SSkip)) {
      d688 = 1;
      {
        u_value t689 = v_x;
        *r_y = t689;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s687, 0);
      d688 = 1;
      {
        p_check_46_unique(f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)));
        p_check_46_variables(v_env, v_assignable, f_list_46_firsts_1(v_pairs), &v_ys, r_s);
        p_check_46_checks(v_env, f_list_46_seconds_1(v_pairs), f_syntax_46_types_1(v_ys), &v_es, r_s);
        u_value t690 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_zip_2(v_ys, v_es));
        *r_y = t690;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SBlock)) {
      u_value v_locals = u_field(s687, 0);
      u_value v_body = u_field(s687, 1);
      d688 = 1;
      {
        p_check_46_untyped(v_locals, f_check_46_initialized_1(v_body));
        p_check_46_formals(v_locals, &v_ys, r_s);
        p_check_46_statements(f_list_46__43__43__2(f_check_46_bind_1(v_ys), v_env), f_list_46__43__43__2(f_syntax_46_names_1(v_ys), v_assignable), v_body, &v_xs, r_s);
        u_value t691 = u_mk_con(&n_syntax_46_SBlock, 2, v_ys, v_xs);
        *r_y = t691;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SIf)) {
      u_value v_cond = u_field(s687, 0);
      u_value v_left = u_field(s687, 1);
      u_value v_right = u_field(s687, 2);
      d688 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_left, &v_l, r_s);
        p_check_46_statement(v_env, v_assignable, v_right, &v_r, r_s);
        u_value t692 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
        *r_y = t692;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SWhile)) {
      u_value v_cond = u_field(s687, 0);
      u_value v_body = u_field(s687, 1);
      d688 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        u_value t693 = u_mk_con(&n_syntax_46_SWhile, 2, v_c, v_l);
        *r_y = t693;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s687, 0);
      u_value v_ins = u_field(s687, 1);
      u_value v_outs = u_field(s687, 2);
      u_value v_refs = u_field(s687, 3);
      d688 = 1;
      {
        p_check_46_call_proc(v_env, v_assignable, v_p, v_ins, v_outs, v_refs, r_y, r_s);
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s687, 0);
      u_value v_cases = u_field(s687, 1);
      d688 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_cases, v_t, &v_bs, r_s);
        u_value t694 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e695 = &t694;
        u_value l696 = v_bs;
        while (u_is_con(l696, &n_list_46_Cons)) {
          u_value v_br = u_field(l696, 0);
          if (u_truth(u_mk_bool(1))) {
            e695 = u_append(e695, f_check_46_case_of_1(v_br));
          }
          l696 = u_field(l696, 1);
        }
        p_check_46_coverage(*r_s, v_t, t694);
        u_value t697 = u_mk_con(&n_syntax_46_SMatch, 2, v_c, v_bs);
        *r_y = t697;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SAssert)) {
      u_value v_cond = u_field(s687, 0);
      d688 = 1;
      {
        p_check_46_check(v_env, v_cond, f_check_46_tbool_0(), &v_c, r_s);
        u_value t698 = u_mk_con(&n_syntax_46_SAssert, 1, v_c);
        *r_y = t698;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s687, 0);
      u_value v_a = u_field(s687, 1);
      d688 = 1;
      {
        p_check_46_infer(v_env, v_arg, &v_c, &v_t, r_s);
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_unify(v_t, v_u, r_s);
        u_value t699 = u_mk_con(&n_syntax_46_SRaise, 2, v_c, v_t);
        *r_y = t699;
      }
    }
    if ((!d688) && u_is_con(s687, &n_syntax_46_STry)) {
      u_value v_body = u_field(s687, 0);
      u_value v_handlers = u_field(s687, 1);
      d688 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_body, &v_l, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_handlers, &v_hs, r_s);
        u_value t700 = u_mk_con(&n_syntax_46_STry, 2, v_l, v_hs);
        *r_y = t700;
      }
    }
    if (!d688) {
      u_no_case(s687);
    }
  }
}

u_value f_check_46_initialized_1(u_value v_xs) {
  u_value s701 = v_xs;
  u_value t702 = NULL;
  if ((!t702) && (u_is_con(s701, &n_list_46_Cons) && (u_is_con(u_field(s701, 0), &n_syntax_46_SAssign) && (u_is_con(u_field(u_field(s701, 0), 0), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(s701, 0), 0), 1), &n_list_46_Nil))))) {
    u_value v_x = u_field(u_field(u_field(u_field(s701, 0), 0), 0), 0);
    u_value v_rest = u_field(s701, 1);
    t702 = u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), f_check_46_initialized_1(v_rest));
  }
  if (!t702) {
    t702 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t702) {
    u_no_case(s701);
  }
  return t702;
}

void p_check_46_untyped(u_value v_xs, u_value v_inits) {
  {
    u_value s703 = v_xs;
    int d704 = 0;
    if ((!d704) && u_is_con(s703, &n_list_46_Nil)) {
      d704 = 1;
      {
      }
    }
    if ((!d704) && (u_is_con(s703, &n_list_46_Cons) && u_is_con(u_field(s703, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s703, 0), 0);
      u_value v_t = u_field(u_field(s703, 0), 1);
      u_value v_rest = u_field(s703, 1);
      d704 = 1;
      {
        u_value t705 = f_check_46_holes_1(v_t);
        if (u_truth(t705)) {
          t705 = f_prelude_46_not_1(f_list_46_contains_2(v_n, v_inits));
        }
        if (u_truth(t705)) {
          {
            u_raise(u_mk_con(&n_check_46_MissingType, 1, v_n), "check.TypeError");
          }
        }
        p_check_46_untyped(v_rest, v_inits);
      }
    }
    if (!d704) {
      u_no_case(s703);
    }
  }
}

void p_check_46_statements(u_value v_env, u_value v_assignable, u_value v_xs, u_value *r_ys, u_value *r_s) {
  {
    u_value v_y = NULL;
    u_value v_rest = NULL;
    u_value s706 = v_xs;
    int d707 = 0;
    if ((!d707) && u_is_con(s706, &n_list_46_Nil)) {
      d707 = 1;
      {
        u_value t708 = u_mk_con(&n_list_46_Nil, 0);
        *r_ys = t708;
      }
    }
    if ((!d707) && u_is_con(s706, &n_list_46_Cons)) {
      u_value v_x = u_field(s706, 0);
      u_value v_more = u_field(s706, 1);
      d707 = 1;
      {
        p_check_46_statement(v_env, v_assignable, v_x, &v_y, r_s);
        p_check_46_statements(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t709 = u_mk_con(&n_list_46_Cons, 2, v_y, v_rest);
        *r_ys = t709;
      }
    }
    if (!d707) {
      u_no_case(s706);
    }
  }
}

u_value f_check_46_os_procs_1(u_value v_c) {
  u_value t714;
  if (u_truth(f_ctx_46_is_data_2(v_c, f_syntax_46_os_error_name_0()))) {
    static struct u_object l_os_args_710 = U_LIT("os_args");
    static struct u_object l_os_read_711 = U_LIT("os_read");
    static struct u_object l_os_write_712 = U_LIT("os_write");
    static struct u_object l_os_library_713 = U_LIT("os_library");
    t714 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_args_710), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tlist_1(f_check_46_tstr_0()), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_read_711), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_write_712), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_os_library_713), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)))));
  } else {
    t714 = u_mk_con(&n_list_46_Nil, 0);
  }
  return t714;
}

u_value f_check_46_syntax_procs_1(u_value v_c) {
  u_value t715 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e716 = &t715;
  static struct u_object l_Program_717 = U_LIT("Program");
  u_value l718 = f_ctx_46_names_of_2(v_c, &l_Program_717);
  while (u_is_con(l718, &n_list_46_Cons)) {
    u_value v_d = u_field(l718, 0);
    if (u_truth(f_ctx_46_is_data_2(v_c, v_d))) {
      e716 = u_append(e716, v_d);
    }
    l718 = u_field(l718, 1);
  }
  u_value s719 = t715;
  u_value t720 = NULL;
  if ((!t720) && u_is_con(s719, &n_list_46_Cons)) {
    u_value v_d = u_field(s719, 0);
    static struct u_object l_read_721 = U_LIT("read");
    static struct u_object l_library_722 = U_LIT("library");
    t720 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_read_721), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_library_722), u_mk_tuple(4, u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_tstr_0(), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, v_d, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t720) {
    t720 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t720) {
    u_no_case(s719);
  }
  return t720;
}

void p_check_46_call_proc(u_value v_env, u_value v_assignable, u_value v_p, u_value v_ins, u_value v_outs, u_value v_refs, u_value *r_y, u_value *r_s) {
  {
    u_value v_m = NULL;
    u_value v_ins1 = NULL;
    u_value v_outs1 = NULL;
    u_value v_refs1 = NULL;
    u_value v_q = NULL;
    u_value s724 = f_check_46_visible_names_3(*r_s, v_p, u_mk_closure(lam723, 1, *r_s));
    int d725 = 0;
    if ((!d725) && (u_is_con(s724, &n_list_46_Cons) && u_is_con(u_field(s724, 1), &n_list_46_Nil))) {
      u_value v_q1 = u_field(s724, 0);
      d725 = 1;
      {
        u_value t726 = v_q1;
        v_q = t726;
      }
    }
    if ((!d725) && u_is_con(s724, &n_list_46_Nil)) {
      d725 = 1;
      {
        u_value t727 = v_p;
        v_q = t727;
      }
    }
    if (!d725) {
      u_value v_qs = s724;
      d725 = 1;
      {
        u_raise(u_mk_con(&n_check_46_Ambiguous, 2, v_p, v_qs), "check.TypeError");
      }
    }
    if (!d725) {
      u_no_case(s724);
    }
    u_value s728 = f_option_46_or_else_2(f_ctx_46_proc_of_2(f_check_46_State_46_ctx_1(*r_s), v_q), f_list_46_lookup_2(v_q, f_list_46__43__43__2(f_check_46_os_procs_1(f_check_46_State_46_ctx_1(*r_s)), f_check_46_syntax_procs_1(f_check_46_State_46_ctx_1(*r_s)))));
    int d729 = 0;
    if ((!d729) && u_is_con(s728, &n_option_46_Some)) {
      u_value v_ps = u_field(u_field(s728, 0), 0);
      u_value v_is = u_field(u_field(s728, 0), 1);
      u_value v_os = u_field(u_field(s728, 0), 2);
      u_value v_rs = u_field(u_field(s728, 0), 3);
      d729 = 1;
      {
        p_check_46_instance(v_ps, &v_m, r_s);
        p_check_46_same_length(f_list_46_length_1(v_is), f_list_46_length_1(v_ins));
        p_check_46_same_length(f_list_46_length_1(v_os), f_list_46_length_1(v_outs));
        p_check_46_same_length(f_list_46_length_1(v_rs), f_list_46_length_1(v_refs));
        p_check_46_unique(f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs)));
        u_value t730 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e731 = &t730;
        u_value l732 = v_is;
        while (u_is_con(l732, &n_list_46_Cons)) {
          u_value v_t = u_field(l732, 0);
          if (u_truth(u_mk_bool(1))) {
            e731 = u_append(e731, f_check_46_subst_2(v_t, v_m));
          }
          l732 = u_field(l732, 1);
        }
        p_check_46_checks(v_env, v_ins, t730, &v_ins1, r_s);
        p_check_46_variables(v_env, v_assignable, v_outs, &v_outs1, r_s);
        p_check_46_variables(v_env, v_assignable, v_refs, &v_refs1, r_s);
        u_value t733 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e734 = &t733;
        u_value l735 = f_list_46__43__43__2(v_os, v_rs);
        while (u_is_con(l735, &n_list_46_Cons)) {
          u_value v_t = u_field(l735, 0);
          if (u_truth(u_mk_bool(1))) {
            e734 = u_append(e734, f_check_46_subst_2(v_t, v_m));
          }
          l735 = u_field(l735, 1);
        }
        p_check_46_unifies(f_syntax_46_types_1(f_list_46__43__43__2(v_outs1, v_refs1)), t733, r_s);
        u_value t736 = u_mk_con(&n_syntax_46_SCall, 4, v_q, v_ins1, v_outs1, v_refs1);
        *r_y = t736;
      }
    }
    if ((!d729) && u_is_con(s728, &n_option_46_None)) {
      d729 = 1;
      {
        u_raise(u_mk_con(&n_check_46_UnknownProcedure, 1, v_p), "check.TypeError");
      }
    }
    if (!d729) {
      u_no_case(s728);
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
    u_value s737 = v_cases;
    int d738 = 0;
    if ((!d738) && u_is_con(s737, &n_list_46_Nil)) {
      d738 = 1;
      {
        u_value t739 = u_mk_con(&n_list_46_Nil, 0);
        *r_cases1 = t739;
      }
    }
    if ((!d738) && (u_is_con(s737, &n_list_46_Cons) && u_is_con(u_field(s737, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s737, 0), 0);
      u_value v_g = u_field(u_field(s737, 0), 1);
      u_value v_b = u_field(u_field(s737, 0), 2);
      u_value v_more = u_field(s737, 1);
      d738 = 1;
      {
        p_check_46_pattern(v_p, v_t, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_stmt_branches(v_env, v_assignable, v_more, v_t, &v_rest, r_s);
        u_value t740 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p1, v_g1, v_b1), v_rest);
        *r_cases1 = t740;
      }
    }
    if (!d738) {
      u_no_case(s737);
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
    u_value s741 = v_hs;
    int d742 = 0;
    if ((!d742) && u_is_con(s741, &n_list_46_Nil)) {
      d742 = 1;
      {
        u_value t743 = u_mk_con(&n_list_46_Nil, 0);
        *r_hs1 = t743;
      }
    }
    if ((!d742) && (u_is_con(s741, &n_list_46_Cons) && u_is_con(u_field(s741, 0), &n_syntax_46_Handler))) {
      u_value v_a = u_field(u_field(s741, 0), 0);
      u_value v_p = u_field(u_field(s741, 0), 1);
      u_value v_g = u_field(u_field(s741, 0), 2);
      u_value v_b = u_field(u_field(s741, 0), 3);
      u_value v_more = u_field(s741, 1);
      d742 = 1;
      {
        p_check_46_normalize(v_a, &v_u, r_s);
        p_check_46_pattern(v_p, v_u, &v_p1, &v_xs, r_s);
        p_check_46_guard(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), v_g, &v_g1, r_s);
        p_check_46_statement(f_list_46__43__43__2(f_check_46_bind_1(v_xs), v_env), f_list_46_diff_2(v_assignable, f_syntax_46_names_1(v_xs)), v_b, &v_b1, r_s);
        p_check_46_handler_list(v_env, v_assignable, v_more, &v_rest, r_s);
        u_value t744 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, v_u, v_p1, v_g1, v_b1), v_rest);
        *r_hs1 = t744;
      }
    }
    if (!d742) {
      u_no_case(s741);
    }
  }
}

void p_check_46_declare_name(u_value v_c, u_value v_n) {
  {
    u_value t745 = f_ctx_46_is_fun_2(v_c, v_n);
    if (!u_truth(t745)) {
      t745 = f_ctx_46_is_proc_2(v_c, v_n);
    }
    u_value t746 = t745;
    if (!u_truth(t746)) {
      t746 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t747 = t746;
    if (!u_truth(t747)) {
      t747 = f_prelude_46__33__61__2(f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n))), u_mk_con(&n_list_46_Nil, 0));
    }
    if (u_truth(t747)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_overload(u_value v_c, u_value v_n, u_value v_k) {
  {
    u_value t748 = f_ctx_46_is_proc_2(v_c, v_n);
    if (!u_truth(t748)) {
      t748 = f_ctx_46_is_constr_2(v_c, v_n);
    }
    u_value t749 = t748;
    if (!u_truth(t749)) {
      t749 = f_check_46_has_arity_2(f_list_46__43__43__2(f_ctx_46_schemes_of_2(v_c, v_n), f_check_46_builtin_1(f_syntax_46_unqualified_1(f_syntax_46_Name_46_base_1(v_n)))), v_k);
    }
    if (u_truth(t749)) {
      {
        u_raise(u_mk_con(&n_check_46_AlreadyDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_sort(u_value v_c, u_value v_n) {
  {
    static struct u_object l_Bool_750 = U_LIT("Bool");
    static struct u_object l_Int_751 = U_LIT("Int");
    static struct u_object l_Str_752 = U_LIT("Str");
    static struct u_object l_Char_753 = U_LIT("Char");
    static struct u_object l__45__62__754 = U_LIT("->");
    u_value t755 = f_list_46_contains_2(f_syntax_46_Name_46_base_1(v_n), u_mk_con(&n_list_46_Cons, 2, &l_Bool_750, u_mk_con(&n_list_46_Cons, 2, &l_Int_751, u_mk_con(&n_list_46_Cons, 2, &l_Str_752, u_mk_con(&n_list_46_Cons, 2, &l_Char_753, u_mk_con(&n_list_46_Cons, 2, &l__45__62__754, u_mk_con(&n_list_46_Nil, 0)))))));
    if (!u_truth(t755)) {
      t755 = f_ctx_46_is_alias_2(v_c, v_n);
    }
    u_value t756 = t755;
    if (!u_truth(t756)) {
      t756 = f_ctx_46_is_data_2(v_c, v_n);
    }
    if (u_truth(t756)) {
      {
        u_raise(u_mk_con(&n_check_46_SortDefined, 1, v_n), "check.TypeError");
      }
    }
  }
}

void p_check_46_closed(u_value v_n, u_value v_ps, u_value v_ts) {
  {
    u_value v_undeclared = NULL;
    u_value t757 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e758 = &t757;
    u_value l759 = v_ts;
    while (u_is_con(l759, &n_list_46_Cons)) {
      u_value v_t = u_field(l759, 0);
      if (u_truth(u_mk_bool(1))) {
        e758 = u_append(e758, f_check_46_free_1(v_t));
      }
      l759 = u_field(l759, 1);
    }
    u_value t760 = f_list_46_diff_2(f_list_46_distinct_1(f_list_46_concat_1(t757)), f_check_46_params_1(v_ps));
    v_undeclared = t760;
    if (u_truth(f_prelude_46__33__61__2(v_undeclared, u_mk_con(&n_list_46_Nil, 0)))) {
      {
        u_raise(u_mk_con(&n_check_46_UndeclaredParams, 2, v_n, v_undeclared), "check.TypeError");
      }
    }
  }
}

void p_check_46_declare_datas(u_value v_cs, u_value *r_s) {
  {
    u_value s761 = v_cs;
    int d762 = 0;
    if ((!d762) && u_is_con(s761, &n_list_46_Nil)) {
      d762 = 1;
      {
      }
    }
    if ((!d762) && (u_is_con(s761, &n_list_46_Cons) && u_is_con(u_field(u_field(s761, 0), 1), &n_syntax_46_CData))) {
      u_value v_m = u_field(u_field(s761, 0), 0);
      u_value v_n = u_field(u_field(u_field(s761, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s761, 0), 1), 1);
      u_value v_rest = u_field(s761, 1);
      d762 = 1;
      {
        u_value t763 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t763;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        u_value t764 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_data_3(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(v_ps)));
        *r_s = t764;
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if ((!d762) && u_is_con(s761, &n_list_46_Cons)) {
      u_value v_rest = u_field(s761, 1);
      d762 = 1;
      {
        p_check_46_declare_datas(v_rest, r_s);
      }
    }
    if (!d762) {
      u_no_case(s761);
    }
  }
}

void p_check_46_declare_sorts(u_value v_cs, u_value *r_s) {
  {
    u_value v_u = NULL;
    u_value s765 = v_cs;
    int d766 = 0;
    if ((!d766) && u_is_con(s765, &n_list_46_Nil)) {
      d766 = 1;
      {
      }
    }
    if ((!d766) && (u_is_con(s765, &n_list_46_Cons) && u_is_con(u_field(u_field(s765, 0), 1), &n_syntax_46_CSort))) {
      u_value v_m = u_field(u_field(s765, 0), 0);
      u_value v_n = u_field(u_field(u_field(s765, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s765, 0), 1), 1);
      u_value v_t = u_field(u_field(u_field(s765, 0), 1), 2);
      u_value v_rest = u_field(s765, 1);
      d766 = 1;
      {
        u_value t767 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t767;
        p_check_46_declare_sort(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_known(v_t, v_n, &v_u, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, v_u, u_mk_con(&n_list_46_Nil, 0)));
        u_value t768 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_alias_4(f_check_46_State_46_ctx_1(*r_s), v_n, v_ps, v_u));
        *r_s = t768;
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if ((!d766) && u_is_con(s765, &n_list_46_Cons)) {
      u_value v_rest = u_field(s765, 1);
      d766 = 1;
      {
        p_check_46_declare_sorts(v_rest, r_s);
      }
    }
    if (!d766) {
      u_no_case(s765);
    }
  }
}

void p_check_46_declare_constrs(u_value v_cs, u_value *r_s) {
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
      u_value v_ks = u_field(u_field(u_field(s769, 0), 1), 2);
      u_value v_rest = u_field(s769, 1);
      d770 = 1;
      {
        u_value t771 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t771;
        p_check_46_unique_ids(v_ps);
        u_value t772 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e773 = &t772;
        u_value l774 = v_ks;
        while (u_is_con(l774, &n_list_46_Cons)) {
          u_value v_k = u_field(l774, 0);
          if (u_truth(u_mk_bool(1))) {
            e773 = u_append(e773, f_syntax_46_Constr_46_name_1(v_k));
          }
          l774 = u_field(l774, 1);
        }
        u_value t775 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_members_3(f_check_46_State_46_ctx_1(*r_s), v_n, t772));
        *r_s = t775;
        p_check_46_declare_constructors(v_n, v_ps, v_ks, r_s);
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if ((!d770) && u_is_con(s769, &n_list_46_Cons)) {
      u_value v_rest = u_field(s769, 1);
      d770 = 1;
      {
        p_check_46_declare_constrs(v_rest, r_s);
      }
    }
    if (!d770) {
      u_no_case(s769);
    }
  }
}

void p_check_46_declare_constructors(u_value v_n, u_value v_ps, u_value v_ks, u_value *r_s) {
  {
    u_value v_ts = NULL;
    u_value s776 = v_ks;
    int d777 = 0;
    if ((!d777) && u_is_con(s776, &n_list_46_Nil)) {
      d777 = 1;
      {
      }
    }
    if ((!d777) && (u_is_con(s776, &n_list_46_Cons) && u_is_con(u_field(s776, 0), &n_syntax_46_Constr))) {
      u_value v_k = u_field(u_field(s776, 0), 0);
      u_value v_args = u_field(u_field(s776, 0), 1);
      u_value v_rest = u_field(s776, 1);
      d777 = 1;
      {
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_k);
        p_check_46_knowns(v_args, v_k, &v_ts, r_s);
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t778 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_constr_3(f_check_46_State_46_ctx_1(*r_s), v_k, u_mk_tuple(3, f_check_46_params_1(v_ps), v_ts, u_mk_con(&n_syntax_46_TSort, 2, v_n, f_check_46_params_1(v_ps)))));
        *r_s = t778;
        p_check_46_declare_constructors(v_n, v_ps, v_rest, r_s);
      }
    }
    if (!d777) {
      u_no_case(s776);
    }
  }
}

u_value f_check_46_curried_2(u_value v_xss, u_value v_r) {
  u_value s779 = v_xss;
  u_value t780 = NULL;
  if ((!t780) && u_is_con(s779, &n_list_46_Nil)) {
    t780 = v_r;
  }
  if ((!t780) && u_is_con(s779, &n_list_46_Cons)) {
    u_value v_xs = u_field(s779, 0);
    u_value v_rest = u_field(s779, 1);
    t780 = f_check_46_fun_2(f_syntax_46_types_1(v_xs), f_check_46_curried_2(v_rest, v_r));
  }
  if (!t780) {
    u_no_case(s779);
  }
  return t780;
}

u_value f_check_46_first_list_1(u_value v_xss) {
  u_value s781 = v_xss;
  u_value t782 = NULL;
  if ((!t782) && u_is_con(s781, &n_list_46_Nil)) {
    t782 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t782) && u_is_con(s781, &n_list_46_Cons)) {
    u_value v_xs = u_field(s781, 0);
    t782 = v_xs;
  }
  if (!t782) {
    u_no_case(s781);
  }
  return t782;
}

void p_check_46_signatures(u_value v_xss, u_value *r_yss, u_value *r_s) {
  {
    u_value v_ys = NULL;
    u_value v_rest = NULL;
    u_value s783 = v_xss;
    int d784 = 0;
    if ((!d784) && u_is_con(s783, &n_list_46_Nil)) {
      d784 = 1;
      {
        u_value t785 = u_mk_con(&n_list_46_Nil, 0);
        *r_yss = t785;
      }
    }
    if ((!d784) && u_is_con(s783, &n_list_46_Cons)) {
      u_value v_xs = u_field(s783, 0);
      u_value v_more = u_field(s783, 1);
      d784 = 1;
      {
        p_check_46_signature(v_xs, &v_ys, r_s);
        p_check_46_signatures(v_more, &v_rest, r_s);
        u_value t786 = u_mk_con(&n_list_46_Cons, 2, v_ys, v_rest);
        *r_yss = t786;
      }
    }
    if (!d784) {
      u_no_case(s783);
    }
  }
}

void p_check_46_declare_funs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xss = NULL;
    u_value v_r = NULL;
    u_value s787 = v_cs;
    int d788 = 0;
    if ((!d788) && u_is_con(s787, &n_list_46_Nil)) {
      d788 = 1;
      {
      }
    }
    if ((!d788) && (u_is_con(s787, &n_list_46_Cons) && u_is_con(u_field(u_field(s787, 0), 1), &n_syntax_46_CFun))) {
      u_value v_m = u_field(u_field(s787, 0), 0);
      u_value v_n = u_field(u_field(u_field(s787, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s787, 0), 1), 1);
      u_value v_bound = u_field(u_field(u_field(s787, 0), 1), 2);
      u_value v_res = u_field(u_field(u_field(s787, 0), 1), 3);
      u_value v_rest = u_field(s787, 1);
      d788 = 1;
      {
        u_value t789 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t789;
        p_check_46_overload(f_check_46_State_46_ctx_1(*r_s), v_n, f_list_46_length_1(f_check_46_first_list_1(v_bound)));
        p_check_46_unique_ids(v_ps);
        p_check_46_unique(f_syntax_46_names_1(f_list_46_concat_1(v_bound)));
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_closed(v_n, v_ps, u_mk_con(&n_list_46_Cons, 2, f_check_46_curried_2(v_xss, v_r), u_mk_con(&n_list_46_Nil, 0)));
        u_value t790 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_fun_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(2, f_check_46_params_1(v_ps), f_check_46_curried_2(v_xss, v_r))));
        *r_s = t790;
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if ((!d788) && u_is_con(s787, &n_list_46_Cons)) {
      u_value v_rest = u_field(s787, 1);
      d788 = 1;
      {
        p_check_46_declare_funs(v_rest, r_s);
      }
    }
    if (!d788) {
      u_no_case(s787);
    }
  }
}

void p_check_46_declare_procs(u_value v_cs, u_value *r_s) {
  {
    u_value v_xs = NULL;
    u_value v_ts = NULL;
    u_value s791 = v_cs;
    int d792 = 0;
    if ((!d792) && u_is_con(s791, &n_list_46_Nil)) {
      d792 = 1;
      {
      }
    }
    if ((!d792) && (u_is_con(s791, &n_list_46_Cons) && u_is_con(u_field(u_field(s791, 0), 1), &n_syntax_46_CProc))) {
      u_value v_m = u_field(u_field(s791, 0), 0);
      u_value v_n = u_field(u_field(u_field(s791, 0), 1), 0);
      u_value v_ps = u_field(u_field(u_field(s791, 0), 1), 1);
      u_value v_ins = u_field(u_field(u_field(s791, 0), 1), 2);
      u_value v_outs = u_field(u_field(u_field(s791, 0), 1), 3);
      u_value v_refs = u_field(u_field(u_field(s791, 0), 1), 4);
      u_value v_rest = u_field(s791, 1);
      d792 = 1;
      {
        u_value t793 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t793;
        p_check_46_declare_name(f_check_46_State_46_ctx_1(*r_s), v_n);
        p_check_46_unique_ids(v_ps);
        p_check_46_signature(f_list_46__43__43__2(v_ins, f_list_46__43__43__2(v_outs, v_refs)), &v_xs, r_s);
        u_value t794 = f_syntax_46_types_1(v_xs);
        v_ts = t794;
        p_check_46_closed(v_n, v_ps, v_ts);
        u_value t795 = f_check_46_with_ctx_2(*r_s, f_ctx_46_with_proc_3(f_check_46_State_46_ctx_1(*r_s), v_n, u_mk_tuple(4, f_check_46_params_1(v_ps), f_list_46_take_2(f_list_46_length_1(v_ins), v_ts), f_list_46_take_2(f_list_46_length_1(v_outs), f_list_46_drop_2(f_list_46_length_1(v_ins), v_ts)), f_list_46_drop_2(u_add(f_list_46_length_1(v_ins), f_list_46_length_1(v_outs)), v_ts))));
        *r_s = t795;
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if ((!d792) && u_is_con(s791, &n_list_46_Cons)) {
      u_value v_rest = u_field(s791, 1);
      d792 = 1;
      {
        p_check_46_declare_procs(v_rest, r_s);
      }
    }
    if (!d792) {
      u_no_case(s791);
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
    u_value s796 = v_c;
    int d797 = 0;
    if ((!d797) && u_is_con(s796, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s796, 0);
      u_value v_ps = u_field(s796, 1);
      u_value v_bound = u_field(s796, 2);
      u_value v_res = u_field(s796, 3);
      u_value v_body = u_field(s796, 4);
      d797 = 1;
      {
        p_check_46_signatures(v_bound, &v_xss, r_s);
        p_check_46_known(v_res, v_n, &v_r, r_s);
        p_check_46_check(f_check_46_bind_1(f_list_46_concat_1(v_xss)), v_body, v_r, &v_b, r_s);
        u_value t798 = u_mk_con(&n_syntax_46_CFun, 5, v_n, v_ps, v_xss, v_r, v_b);
        *r_c1 = t798;
      }
    }
    if ((!d797) && u_is_con(s796, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s796, 0);
      u_value v_ps = u_field(s796, 1);
      u_value v_ins = u_field(s796, 2);
      u_value v_outs = u_field(s796, 3);
      u_value v_refs = u_field(s796, 4);
      u_value v_body = u_field(s796, 5);
      d797 = 1;
      {
        p_check_46_signature(v_ins, &v_is, r_s);
        p_check_46_signature(v_outs, &v_os, r_s);
        p_check_46_signature(v_refs, &v_rs, r_s);
        p_check_46_statement(f_check_46_bind_1(f_list_46__43__43__2(v_is, f_list_46__43__43__2(v_os, v_rs))), f_syntax_46_names_1(f_list_46__43__43__2(v_os, v_rs)), v_body, &v_y, r_s);
        u_value t799 = u_mk_con(&n_syntax_46_CProc, 6, v_n, v_ps, v_is, v_os, v_rs, v_y);
        *r_c1 = t799;
      }
    }
    if ((!d797) && u_is_con(s796, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s796, 0);
      d797 = 1;
      {
        p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_b, &v_t, r_s);
        u_value t800 = u_mk_con(&n_syntax_46_CEval, 1, v_b);
        *r_c1 = t800;
      }
    }
    if ((!d797) && u_is_con(s796, &n_syntax_46_CRun)) {
      u_value v_n = u_field(s796, 0);
      u_value v_body = u_field(s796, 1);
      d797 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t801 = u_mk_con(&n_syntax_46_CRun, 2, v_n, v_y);
        *r_c1 = t801;
      }
    }
    if ((!d797) && u_is_con(s796, &n_syntax_46_CTest)) {
      u_value v_n = u_field(s796, 0);
      u_value v_body = u_field(s796, 1);
      d797 = 1;
      {
        p_check_46_statement(u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), v_body, &v_y, r_s);
        u_value t802 = u_mk_con(&n_syntax_46_CTest, 2, v_n, v_y);
        *r_c1 = t802;
      }
    }
    if (!d797) {
      d797 = 1;
      {
        u_value t803 = v_c;
        *r_c1 = t803;
      }
    }
    if (!d797) {
      u_no_case(s796);
    }
  }
}

void p_check_46_commands(u_value v_cs, u_value *r_cs1, u_value *r_s) {
  {
    u_value v_c1 = NULL;
    u_value v_c2 = NULL;
    u_value v_rest = NULL;
    u_value s804 = v_cs;
    int d805 = 0;
    if ((!d805) && u_is_con(s804, &n_list_46_Nil)) {
      d805 = 1;
      {
        u_value t806 = u_mk_con(&n_list_46_Nil, 0);
        *r_cs1 = t806;
      }
    }
    if ((!d805) && u_is_con(s804, &n_list_46_Cons)) {
      u_value v_m = u_field(u_field(s804, 0), 0);
      u_value v_c = u_field(u_field(s804, 0), 1);
      u_value v_more = u_field(s804, 1);
      d805 = 1;
      {
        u_value t807 = f_check_46_enter_2(*r_s, v_m);
        *r_s = t807;
        p_check_46_command(v_c, &v_c1, r_s);
        u_value t808 = f_check_46_zonk_cmd_2(f_check_46_State_46_sub_1(*r_s), v_c1);
        v_c2 = t808;
        u_value s809 = f_check_46_unknown_in_cmd_1(v_c2);
        int d810 = 0;
        if ((!d810) && u_is_con(s809, &n_option_46_Some)) {
          u_value v_t = u_field(s809, 0);
          d810 = 1;
          {
            u_raise(u_mk_con(&n_check_46_UnknownException, 1, v_t), "check.TypeError");
          }
        }
        if ((!d810) && u_is_con(s809, &n_option_46_None)) {
          d810 = 1;
          {
          }
        }
        if (!d810) {
          u_no_case(s809);
        }
        p_check_46_commands(v_more, &v_rest, r_s);
        u_value t811 = u_mk_con(&n_list_46_Cons, 2, v_c2, v_rest);
        *r_cs1 = t811;
      }
    }
    if (!d805) {
      u_no_case(s804);
    }
  }
}

u_value f_check_46_block_names_1(u_value v_cs) {
  u_value s812 = v_cs;
  u_value t813 = NULL;
  if ((!t813) && u_is_con(s812, &n_list_46_Nil)) {
    t813 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t813) && (u_is_con(s812, &n_list_46_Cons) && u_is_con(u_field(u_field(s812, 0), 1), &n_syntax_46_CRun))) {
    u_value v_n = u_field(u_field(u_field(s812, 0), 1), 0);
    u_value v_more = u_field(s812, 1);
    t813 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t813) && (u_is_con(s812, &n_list_46_Cons) && u_is_con(u_field(u_field(s812, 0), 1), &n_syntax_46_CTest))) {
    u_value v_n = u_field(u_field(u_field(s812, 0), 1), 0);
    u_value v_more = u_field(s812, 1);
    t813 = u_mk_con(&n_list_46_Cons, 2, v_n, f_check_46_block_names_1(v_more));
  }
  if ((!t813) && u_is_con(s812, &n_list_46_Cons)) {
    u_value v_more = u_field(s812, 1);
    t813 = f_check_46_block_names_1(v_more);
  }
  if (!t813) {
    u_no_case(s812);
  }
  return t813;
}

void p_check_46_check_program(u_value v_p, u_value v_main, u_value *r_q) {
  {
    u_value v_s = NULL;
    u_value v_ds = NULL;
    u_value v_cs = NULL;
    u_value s814 = v_p;
    int d815 = 0;
    if ((!d815) && u_is_con(s814, &n_syntax_46_Program)) {
      u_value v_cmds = u_field(s814, 0);
      d815 = 1;
      {
        u_value t816 = f_check_46_flatten_2(v_main, v_cmds);
        v_ds = t816;
        u_value t817 = f_check_46_start_2(v_main, f_check_46_visibility_2(v_ds, v_main));
        v_s = t817;
        p_check_46_declare_datas(v_ds, &v_s);
        p_check_46_declare_sorts(v_ds, &v_s);
        p_check_46_declare_constrs(v_ds, &v_s);
        p_check_46_declare_funs(v_ds, &v_s);
        p_check_46_declare_procs(v_ds, &v_s);
        p_check_46_unique_ids(f_check_46_block_names_1(v_ds));
        p_check_46_commands(v_ds, &v_cs, &v_s);
        u_value t818 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
        *r_q = t818;
      }
    }
    if (!d815) {
      u_no_case(s814);
    }
  }
}

void p_check_46_type_of(u_value v_e, u_value *r_t) {
  {
    u_value v_s = NULL;
    u_value v_e1 = NULL;
    u_value v_u = NULL;
    static struct u_object l_main_819 = U_LIT("main");
    u_value t820 = f_check_46_start_2(&l_main_819, u_mk_con(&n_list_46_Nil, 0));
    v_s = t820;
    p_check_46_infer(u_mk_con(&n_list_46_Nil, 0), v_e, &v_e1, &v_u, &v_s);
    u_value t821 = f_check_46_zonk_type_2(f_check_46_State_46_sub_1(v_s), v_u);
    *r_t = t821;
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
  static struct u_object l_List_822 = U_LIT("List");
  return u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l_List_822), u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0)));
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
  static struct u_object l_list_823 = U_LIT("list");
  static struct u_object l_List_824 = U_LIT("List");
  static struct u_object l_A_825 = U_LIT("A");
  static struct u_object l_Nil_826 = U_LIT("Nil");
  static struct u_object l_Cons_827 = U_LIT("Cons");
  static struct u_object l_A_828 = U_LIT("A");
  static struct u_object l_A_829 = U_LIT("A");
  static struct u_object l_list_830 = U_LIT("list");
  static struct u_object l_len_831 = U_LIT("len");
  static struct u_object l_A_832 = U_LIT("A");
  static struct u_object l_xs_833 = U_LIT("xs");
  static struct u_object l_A_834 = U_LIT("A");
  static struct u_object l_xs_835 = U_LIT("xs");
  static struct u_object l_Nil_836 = U_LIT("Nil");
  static struct u_object l_Cons_837 = U_LIT("Cons");
  static struct u_object l_ys_838 = U_LIT("ys");
  static struct u_object l__43__839 = U_LIT("+");
  static struct u_object l_len_840 = U_LIT("len");
  static struct u_object l_ys_841 = U_LIT("ys");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_list_823, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(&l_List_824), u_mk_con(&n_list_46_Cons, 2, &l_A_825, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Nil_826), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(&l_Cons_827), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, &l_A_828), u_mk_con(&n_list_46_Cons, 2, f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_829)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, &l_list_830), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(&l_len_831), u_mk_con(&n_list_46_Cons, 2, &l_A_832, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_xs_833), f_check_46_tl_1(u_mk_con(&n_syntax_46_TParam, 1, &l_A_834))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_check_46_tint_0(), u_mk_con(&n_syntax_46_EMatch, 2, f_check_46_ev_1(&l_xs_835), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, f_check_46_pv_1(&l_Nil_836), u_mk_con(&n_option_46_None, 0), f_check_46_num_1(u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_837), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_list_46_Cons, 2, f_check_46_pv_1(&l_ys_838), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_option_46_None, 0), f_check_46_ap_2(&l__43__839, u_mk_con(&n_list_46_Cons, 2, f_check_46_num_1(u_mk_int(1)), u_mk_con(&n_list_46_Cons, 2, f_check_46_ap_2(&l_len_840, u_mk_con(&n_list_46_Cons, 2, f_check_46_ev_1(&l_ys_841), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
  return cache;
}

u_value f_c_46_c_type_1(u_value v_t) {
  u_value s842 = v_t;
  u_value t843 = NULL;
  if ((!t843) && u_is_con(s842, &n_c_46_CtVoid)) {
    static struct u_object l_void_844 = U_LIT("void");
    t843 = &l_void_844;
  }
  if ((!t843) && u_is_con(s842, &n_c_46_CtInt)) {
    static struct u_object l_int_845 = U_LIT("int");
    t843 = &l_int_845;
  }
  if ((!t843) && u_is_con(s842, &n_c_46_CtNamed)) {
    u_value v_n = u_field(s842, 0);
    t843 = v_n;
  }
  if ((!t843) && (u_is_con(s842, &n_c_46_CtPtr) && u_is_con(u_field(s842, 0), &n_c_46_CtPtr))) {
    u_value v_u = u_field(u_field(s842, 0), 0);
    static struct u_object l__42__846 = U_LIT("*");
    t843 = u_concat(f_c_46_c_type_1(u_mk_con(&n_c_46_CtPtr, 1, v_u)), &l__42__846);
  }
  if ((!t843) && u_is_con(s842, &n_c_46_CtPtr)) {
    u_value v_u = u_field(s842, 0);
    static struct u_object l__32__42__847 = U_LIT(" *");
    t843 = u_concat(f_c_46_c_type_1(v_u), &l__32__42__847);
  }
  if (!t843) {
    u_no_case(s842);
  }
  return t843;
}

u_value f_c_46_c_declare_2(u_value v_t, u_value v_x) {
  u_value s848 = v_t;
  u_value t849 = NULL;
  if ((!t849) && u_is_con(s848, &n_c_46_CtPtr)) {
    t849 = u_concat(f_c_46_c_type_1(v_t), v_x);
  }
  if (!t849) {
    static struct u_object l__32__850 = U_LIT(" ");
    t849 = u_concat(f_c_46_c_type_1(v_t), u_concat(&l__32__850, v_x));
  }
  if (!t849) {
    u_no_case(s848);
  }
  return t849;
}

u_value f_c_46_c_expr_1(u_value v_e) {
  u_value s851 = v_e;
  u_value t852 = NULL;
  if ((!t852) && u_is_con(s851, &n_c_46_CeVar)) {
    u_value v_x = u_field(s851, 0);
    t852 = v_x;
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeNum)) {
    u_value v_n = u_field(s851, 0);
    t852 = u_str_show(v_n);
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeStr)) {
    u_value v_s = u_field(s851, 0);
    t852 = u_str_quote(v_s);
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeCall)) {
    u_value v_f = u_field(s851, 0);
    u_value v_args = u_field(s851, 1);
    static struct u_object l__40__853 = U_LIT("(");
    static struct u_object l__44__32__854 = U_LIT(", ");
    u_value t855 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e856 = &t855;
    u_value l857 = v_args;
    while (u_is_con(l857, &n_list_46_Cons)) {
      u_value v_a = u_field(l857, 0);
      if (u_truth(u_mk_bool(1))) {
        e856 = u_append(e856, f_c_46_c_expr_1(v_a));
      }
      l857 = u_field(l857, 1);
    }
    static struct u_object l__41__858 = U_LIT(")");
    t852 = u_concat(f_c_46_c_operand_1(v_f), u_concat(&l__40__853, u_concat(u_str_join(&l__44__32__854, t855), &l__41__858)));
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeUnary)) {
    u_value v_o = u_field(s851, 0);
    u_value v_a = u_field(s851, 1);
    t852 = u_concat(v_o, f_c_46_c_operand_1(v_a));
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeBinary)) {
    u_value v_o = u_field(s851, 0);
    u_value v_a = u_field(s851, 1);
    u_value v_b = u_field(s851, 2);
    static struct u_object l__32__859 = U_LIT(" ");
    static struct u_object l__32__860 = U_LIT(" ");
    t852 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__859, u_concat(v_o, u_concat(&l__32__860, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeIndex)) {
    u_value v_a = u_field(s851, 0);
    u_value v_i = u_field(s851, 1);
    static struct u_object l__91__861 = U_LIT("[");
    static struct u_object l__93__862 = U_LIT("]");
    t852 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__91__861, u_concat(f_c_46_c_expr_1(v_i), &l__93__862)));
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeArrow)) {
    u_value v_a = u_field(s851, 0);
    u_value v_f = u_field(s851, 1);
    static struct u_object l__45__62__863 = U_LIT("->");
    t852 = u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__45__62__863, v_f));
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeCast)) {
    u_value v_t = u_field(s851, 0);
    u_value v_a = u_field(s851, 1);
    static struct u_object l__40__864 = U_LIT("(");
    static struct u_object l__41__865 = U_LIT(")");
    t852 = u_concat(&l__40__864, u_concat(f_c_46_c_type_1(v_t), u_concat(&l__41__865, f_c_46_c_operand_1(v_a))));
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeCond)) {
    u_value v_c = u_field(s851, 0);
    u_value v_a = u_field(s851, 1);
    u_value v_b = u_field(s851, 2);
    static struct u_object l__32__63__32__866 = U_LIT(" ? ");
    static struct u_object l__32__58__32__867 = U_LIT(" : ");
    t852 = u_concat(f_c_46_c_operand_1(v_c), u_concat(&l__32__63__32__866, u_concat(f_c_46_c_operand_1(v_a), u_concat(&l__32__58__32__867, f_c_46_c_operand_1(v_b)))));
  }
  if ((!t852) && u_is_con(s851, &n_c_46_CeInit)) {
    u_value v_es = u_field(s851, 0);
    static struct u_object l__123__868 = U_LIT("{");
    static struct u_object l__44__32__869 = U_LIT(", ");
    u_value t870 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e871 = &t870;
    u_value l872 = v_es;
    while (u_is_con(l872, &n_list_46_Cons)) {
      u_value v_a = u_field(l872, 0);
      if (u_truth(u_mk_bool(1))) {
        e871 = u_append(e871, f_c_46_c_expr_1(v_a));
      }
      l872 = u_field(l872, 1);
    }
    static struct u_object l__125__873 = U_LIT("}");
    t852 = u_concat(&l__123__868, u_concat(u_str_join(&l__44__32__869, t870), &l__125__873));
  }
  if (!t852) {
    u_no_case(s851);
  }
  return t852;
}

u_value f_c_46_c_operand_1(u_value v_e) {
  u_value s874 = v_e;
  u_value t875 = NULL;
  if ((!t875) && u_is_con(s874, &n_c_46_CeVar)) {
    t875 = f_c_46_c_expr_1(v_e);
  }
  if ((!t875) && u_is_con(s874, &n_c_46_CeNum)) {
    u_value v_n = u_field(s874, 0);
    if (u_truth(f_prelude_46__62__61__2(v_n, u_mk_int(0)))) {
      t875 = f_c_46_c_expr_1(v_e);
    }
  }
  if ((!t875) && u_is_con(s874, &n_c_46_CeStr)) {
    t875 = f_c_46_c_expr_1(v_e);
  }
  if ((!t875) && u_is_con(s874, &n_c_46_CeCall)) {
    t875 = f_c_46_c_expr_1(v_e);
  }
  if ((!t875) && u_is_con(s874, &n_c_46_CeIndex)) {
    t875 = f_c_46_c_expr_1(v_e);
  }
  if ((!t875) && u_is_con(s874, &n_c_46_CeArrow)) {
    t875 = f_c_46_c_expr_1(v_e);
  }
  if (!t875) {
    static struct u_object l__40__876 = U_LIT("(");
    static struct u_object l__41__877 = U_LIT(")");
    t875 = u_concat(&l__40__876, u_concat(f_c_46_c_expr_1(v_e), &l__41__877));
  }
  if (!t875) {
    u_no_case(s874);
  }
  return t875;
}

u_value f_c_46_c_stmt_2(u_value v_ind, u_value v_s) {
  u_value s878 = v_s;
  u_value t879 = NULL;
  if ((!t879) && u_is_con(s878, &n_c_46_CsExpr)) {
    u_value v_e = u_field(s878, 0);
    static struct u_object l__59__880 = U_LIT(";");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_e), &l__59__880)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && (u_is_con(s878, &n_c_46_CsDecl) && u_is_con(u_field(s878, 2), &n_option_46_None))) {
    u_value v_t = u_field(s878, 0);
    u_value v_x = u_field(s878, 1);
    static struct u_object l__59__881 = U_LIT(";");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__881)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && (u_is_con(s878, &n_c_46_CsDecl) && u_is_con(u_field(s878, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s878, 0);
    u_value v_x = u_field(s878, 1);
    u_value v_e = u_field(u_field(s878, 2), 0);
    static struct u_object l__32__61__32__882 = U_LIT(" = ");
    static struct u_object l__59__883 = U_LIT(";");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__882, u_concat(f_c_46_c_expr_1(v_e), &l__59__883)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && u_is_con(s878, &n_c_46_CsAssign)) {
    u_value v_a = u_field(s878, 0);
    u_value v_e = u_field(s878, 1);
    static struct u_object l__32__61__32__884 = U_LIT(" = ");
    static struct u_object l__59__885 = U_LIT(";");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(f_c_46_c_expr_1(v_a), u_concat(&l__32__61__32__884, u_concat(f_c_46_c_expr_1(v_e), &l__59__885)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && (u_is_con(s878, &n_c_46_CsIf) && u_is_con(u_field(s878, 2), &n_list_46_Nil))) {
    u_value v_c = u_field(s878, 0);
    u_value v_l = u_field(s878, 1);
    static struct u_object l_if_32__40__886 = U_LIT("if (");
    static struct u_object l__41__32__123__887 = U_LIT(") {");
    static struct u_object l__32__32__888 = U_LIT("  ");
    static struct u_object l__125__889 = U_LIT("}");
    t879 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__886, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__887))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__888), v_l), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__889), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t879) && u_is_con(s878, &n_c_46_CsIf)) {
    u_value v_c = u_field(s878, 0);
    u_value v_l = u_field(s878, 1);
    u_value v_r = u_field(s878, 2);
    static struct u_object l_if_32__40__890 = U_LIT("if (");
    static struct u_object l__41__32__123__891 = U_LIT(") {");
    static struct u_object l__32__32__892 = U_LIT("  ");
    static struct u_object l__125__32_else_32__123__893 = U_LIT("} else {");
    static struct u_object l__32__32__894 = U_LIT("  ");
    static struct u_object l__125__895 = U_LIT("}");
    t879 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_if_32__40__890, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__891))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__892), v_l), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__32_else_32__123__893), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__894), v_r), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__895), u_mk_con(&n_list_46_Nil, 0))))));
  }
  if ((!t879) && u_is_con(s878, &n_c_46_CsWhile)) {
    u_value v_c = u_field(s878, 0);
    u_value v_b = u_field(s878, 1);
    static struct u_object l_while_32__40__896 = U_LIT("while (");
    static struct u_object l__41__32__123__897 = U_LIT(") {");
    static struct u_object l__32__32__898 = U_LIT("  ");
    static struct u_object l__125__899 = U_LIT("}");
    t879 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_while_32__40__896, u_concat(f_c_46_c_expr_1(v_c), &l__41__32__123__897))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__898), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__899), u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t879) && (u_is_con(s878, &n_c_46_CsReturn) && u_is_con(u_field(s878, 0), &n_option_46_None))) {
    static struct u_object l_return_59__900 = U_LIT("return;");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l_return_59__900), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && (u_is_con(s878, &n_c_46_CsReturn) && u_is_con(u_field(s878, 0), &n_option_46_Some))) {
    u_value v_e = u_field(u_field(s878, 0), 0);
    static struct u_object l_return_32__901 = U_LIT("return ");
    static struct u_object l__59__902 = U_LIT(";");
    t879 = u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, u_concat(&l_return_32__901, u_concat(f_c_46_c_expr_1(v_e), &l__59__902))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t879) && u_is_con(s878, &n_c_46_CsBlock)) {
    u_value v_b = u_field(s878, 0);
    static struct u_object l__123__903 = U_LIT("{");
    static struct u_object l__32__32__904 = U_LIT("  ");
    static struct u_object l__125__905 = U_LIT("}");
    t879 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__123__903), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(u_concat(v_ind, &l__32__32__904), v_b), u_mk_con(&n_list_46_Cons, 2, u_concat(v_ind, &l__125__905), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t879) {
    u_no_case(s878);
  }
  return t879;
}

u_value f_c_46_c_stmts_2(u_value v_ind, u_value v_ss) {
  u_value t906 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e907 = &t906;
  u_value l908 = v_ss;
  while (u_is_con(l908, &n_list_46_Cons)) {
    u_value v_s = u_field(l908, 0);
    if (u_truth(u_mk_bool(1))) {
      e907 = u_append(e907, f_c_46_c_stmt_2(v_ind, v_s));
    }
    l908 = u_field(l908, 1);
  }
  return f_list_46_concat_1(t906);
}

u_value f_c_46_c_params_1(u_value v_ps) {
  u_value s909 = v_ps;
  u_value t910 = NULL;
  if ((!t910) && u_is_con(s909, &n_list_46_Nil)) {
    static struct u_object l_void_911 = U_LIT("void");
    t910 = &l_void_911;
  }
  if (!t910) {
    static struct u_object l__44__32__912 = U_LIT(", ");
    u_value t913 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e914 = &t913;
    u_value l915 = v_ps;
    while (u_is_con(l915, &n_list_46_Cons)) {
      u_value v_p = u_field(l915, 0);
      if (u_truth(u_mk_bool(1))) {
        e914 = u_append(e914, f_c_46_c_param_1(v_p));
      }
      l915 = u_field(l915, 1);
    }
    t910 = u_str_join(&l__44__32__912, t913);
  }
  if (!t910) {
    u_no_case(s909);
  }
  return t910;
}

u_value f_c_46_c_param_1(u_value v_p) {
  u_value s916 = v_p;
  u_value t917 = NULL;
  if (!t917) {
    u_value v_t = u_field(s916, 0);
    u_value v_x = u_field(s916, 1);
    t917 = f_c_46_c_declare_2(v_t, v_x);
  }
  if (!t917) {
    u_no_case(s916);
  }
  return t917;
}

u_value f_c_46_c_decl_1(u_value v_d) {
  u_value s918 = v_d;
  u_value t919 = NULL;
  if ((!t919) && u_is_con(s918, &n_c_46_CdInclude)) {
    u_value v_h = u_field(s918, 0);
    static struct u_object l__35_include_32__920 = U_LIT("#include ");
    t919 = u_mk_con(&n_list_46_Cons, 2, u_concat(&l__35_include_32__920, u_str_quote(v_h)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CdPrototype)) {
    u_value v_t = u_field(s918, 0);
    u_value v_f = u_field(s918, 1);
    u_value v_ps = u_field(s918, 2);
    static struct u_object l__40__921 = U_LIT("(");
    static struct u_object l__41__59__922 = U_LIT(");");
    t919 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__921, u_concat(f_c_46_c_params_1(v_ps), &l__41__59__922))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t919) && u_is_con(s918, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s918, 0);
    u_value v_f = u_field(s918, 1);
    u_value v_ps = u_field(s918, 2);
    u_value v_b = u_field(s918, 3);
    static struct u_object l__40__923 = U_LIT("(");
    static struct u_object l__41__32__123__924 = U_LIT(") {");
    static struct u_object l__32__32__925 = U_LIT("  ");
    static struct u_object l__125__926 = U_LIT("}");
    t919 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_f), u_concat(&l__40__923, u_concat(f_c_46_c_params_1(v_ps), &l__41__32__123__924))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_c_46_c_stmts_2(&l__32__32__925, v_b), u_mk_con(&n_list_46_Cons, 2, &l__125__926, u_mk_con(&n_list_46_Nil, 0))));
  }
  if ((!t919) && (u_is_con(s918, &n_c_46_CdGlobal) && u_is_con(u_field(s918, 2), &n_option_46_None))) {
    u_value v_t = u_field(s918, 0);
    u_value v_x = u_field(s918, 1);
    static struct u_object l__59__927 = U_LIT(";");
    t919 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), &l__59__927), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t919) && (u_is_con(s918, &n_c_46_CdGlobal) && u_is_con(u_field(s918, 2), &n_option_46_Some))) {
    u_value v_t = u_field(s918, 0);
    u_value v_x = u_field(s918, 1);
    u_value v_e = u_field(u_field(s918, 2), 0);
    static struct u_object l__32__61__32__928 = U_LIT(" = ");
    static struct u_object l__59__929 = U_LIT(";");
    t919 = u_mk_con(&n_list_46_Cons, 2, u_concat(f_c_46_c_declare_2(v_t, v_x), u_concat(&l__32__61__32__928, u_concat(f_c_46_c_expr_1(v_e), &l__59__929))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t919) {
    u_no_case(s918);
  }
  return t919;
}

u_value f_c_46_c_file_1(u_value v_f) {
  u_value s930 = v_f;
  u_value t931 = NULL;
  if ((!t931) && u_is_con(s930, &n_c_46_CFile)) {
    u_value v_ds = u_field(s930, 0);
    static struct u_object l__10__10__932 = U_LIT("\n\n");
    u_value t933 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e934 = &t933;
    u_value l935 = v_ds;
    while (u_is_con(l935, &n_list_46_Cons)) {
      u_value v_d = u_field(l935, 0);
      if (u_truth(u_mk_bool(1))) {
        static struct u_object l__10__936 = U_LIT("\n");
        e934 = u_append(e934, u_str_join(&l__10__936, f_c_46_c_decl_1(v_d)));
      }
      l935 = u_field(l935, 1);
    }
    static struct u_object l__10__937 = U_LIT("\n");
    t931 = u_concat(u_str_join(&l__10__10__932, t933), &l__10__937);
  }
  if (!t931) {
    u_no_case(s930);
  }
  return t931;
}

u_value f_c_46_c_call_2(u_value v_f, u_value v_args) {
  return u_mk_con(&n_c_46_CeCall, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), v_args);
}

u_value f_lex_46_is_symbol_1(u_value v_c) {
  static struct u_object l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__938 = U_LIT("-+*/%<>=!&|:^~?@#$");
  return u_str_contains(&l__45__43__42__47__37__60__62__61__33__38__124__58__94__126__63__64__938, v_c);
}

u_value f_lex_46_is_punct_1(u_value v_c) {
  static struct u_object l__40__41__91__93__44__59__46__939 = U_LIT("()[],;.");
  return u_str_contains(&l__40__41__91__93__44__59__46__939, v_c);
}

u_value f_lex_46_is_name_char_1(u_value v_c) {
  u_value t940 = u_is_letter(v_c);
  if (!u_truth(t940)) {
    t940 = u_is_digit(v_c);
  }
  u_value t941 = t940;
  if (!u_truth(t941)) {
    t941 = u_eq(v_c, u_mk_char(95));
  }
  return t941;
}

u_value f_lex_46_peek_2(u_value v_s, u_value v_i) {
  u_value t942;
  if (u_truth(u_lt(v_i, u_str_size(v_s)))) {
    t942 = u_str_get(v_s, v_i);
  } else {
    t942 = u_chr(u_mk_int(0));
  }
  return t942;
}

u_value f_lex_46_unescape_1(u_value v_c) {
  u_value s943 = v_c;
  u_value t944 = NULL;
  if ((!t944) && u_equals(s943, u_mk_char(34))) {
    t944 = u_mk_con(&n_option_46_Some, 1, u_mk_char(34));
  }
  if ((!t944) && u_equals(s943, u_mk_char(39))) {
    t944 = u_mk_con(&n_option_46_Some, 1, u_mk_char(39));
  }
  if ((!t944) && u_equals(s943, u_mk_char(92))) {
    t944 = u_mk_con(&n_option_46_Some, 1, u_mk_char(92));
  }
  if ((!t944) && u_equals(s943, u_mk_char(110))) {
    t944 = u_mk_con(&n_option_46_Some, 1, u_mk_char(10));
  }
  if ((!t944) && u_equals(s943, u_mk_char(116))) {
    t944 = u_mk_con(&n_option_46_Some, 1, u_mk_char(9));
  }
  if ((!t944) && u_equals(s943, u_mk_char(114))) {
    t944 = u_mk_con(&n_option_46_Some, 1, u_mk_char(13));
  }
  if (!t944) {
    t944 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t944) {
    u_no_case(s943);
  }
  return t944;
}

u_value f_lex_46_scan_3(u_value v_s, u_value v_i, u_value v_p) {
  u_value t945;
  if (u_truth(u_apply(v_p, 1, f_lex_46_peek_2(v_s, v_i)))) {
    t945 = f_lex_46_scan_3(v_s, u_add(v_i, u_mk_int(1)), v_p);
  } else {
    t945 = v_i;
  }
  return t945;
}

void p_lex_46_position(u_value v_s, u_value v_i, u_value *r_line, u_value *r_column) {
  {
    u_value v_j = NULL;
    u_value t946 = u_mk_int(0);
    u_value t947 = u_mk_int(1);
    u_value t948 = u_mk_int(1);
    v_j = t946;
    *r_line = t947;
    *r_column = t948;
    u_value t953 = u_lt(v_j, v_i);
    while (u_truth(t953)) {
      {
        if (u_truth(u_eq(u_str_get(v_s, v_j), u_mk_char(10)))) {
          {
            u_value t949 = u_add(*r_line, u_mk_int(1));
            u_value t950 = u_mk_int(1);
            *r_line = t949;
            *r_column = t950;
          }
        } else {
          {
            u_value t951 = u_add(*r_column, u_mk_int(1));
            *r_column = t951;
          }
        }
        u_value t952 = u_add(v_j, u_mk_int(1));
        v_j = t952;
      }
      t953 = u_lt(v_j, v_i);
    }
  }
}

void p_lex_46_lex_literal(u_value v_s, u_value v_q, u_value *r_text, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_from = NULL;
    u_value v_parts = NULL;
    u_value v_c = NULL;
    u_value t954 = *r_i;
    u_value t955 = u_add(*r_i, u_mk_int(1));
    u_value t956 = u_mk_con(&n_list_46_Nil, 0);
    v_start = t954;
    *r_i = t955;
    v_parts = t956;
    u_value t957 = *r_i;
    v_from = t957;
    u_value t968 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    while (u_truth(t968)) {
      {
        u_value t958 = f_lex_46_peek_2(v_s, *r_i);
        v_c = t958;
        u_value t959 = u_eq(v_c, u_mk_char(10));
        if (!u_truth(t959)) {
          t959 = f_prelude_46__62__61__2(*r_i, u_str_size(v_s));
        }
        if (u_truth(t959)) {
          {
            static struct u_object l_unterminated_32_lit_960 = U_LIT("unterminated literal");
            u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unterminated_32_lit_960, v_start), "lex.LexError");
          }
        }
        if (u_truth(u_eq(v_c, u_mk_char(92)))) {
          {
            u_value s961 = f_lex_46_unescape_1(f_lex_46_peek_2(v_s, u_add(*r_i, u_mk_int(1))));
            int d962 = 0;
            if ((!d962) && u_is_con(s961, &n_option_46_Some)) {
              u_value v_d = u_field(s961, 0);
              d962 = 1;
              {
                u_value t963 = u_mk_con(&n_list_46_Cons, 2, u_string(u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts));
                u_value t964 = u_add(*r_i, u_mk_int(2));
                v_parts = t963;
                *r_i = t964;
                u_value t965 = *r_i;
                v_from = t965;
              }
            }
            if ((!d962) && u_is_con(s961, &n_option_46_None)) {
              d962 = 1;
              {
                static struct u_object l_invalid_32_escape_966 = U_LIT("invalid escape");
                u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_escape_966, *r_i), "lex.LexError");
              }
            }
            if (!d962) {
              u_no_case(s961);
            }
          }
        } else {
          {
            u_value t967 = u_add(*r_i, u_mk_int(1));
            *r_i = t967;
          }
        }
      }
      t968 = f_prelude_46__33__61__2(f_lex_46_peek_2(v_s, *r_i), v_q);
    }
    static struct u_object l__969 = U_LIT("");
    u_value t970 = u_str_join(&l__969, f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_str_slice(v_s, v_from, *r_i), v_parts)));
    u_value t971 = u_add(*r_i, u_mk_int(1));
    *r_text = t970;
    *r_i = t971;
  }
}

void p_lex_46_lex_name(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t972 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_name_char_1, 0));
    v_j = t972;
    u_value t973 = u_mk_con(&n_lex_46_TName, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t974 = v_j;
    *r_t = t973;
    *r_i = t974;
  }
}

void p_lex_46_lex_number(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t975 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(wu_is_digit, 0));
    v_j = t975;
    u_value s976 = u_str_number(u_str_slice(v_s, *r_i, v_j));
    int d977 = 0;
    if ((!d977) && u_is_con(s976, &n_option_46_Some)) {
      u_value v_n = u_field(s976, 0);
      d977 = 1;
      {
        u_value t978 = u_mk_con(&n_lex_46_TNum, 1, v_n);
        u_value t979 = v_j;
        *r_t = t978;
        *r_i = t979;
      }
    }
    if ((!d977) && u_is_con(s976, &n_option_46_None)) {
      d977 = 1;
      {
        static struct u_object l_invalid_32_number_980 = U_LIT("invalid number");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_number_980, *r_i), "lex.LexError");
      }
    }
    if (!d977) {
      u_no_case(s976);
    }
  }
}

void p_lex_46_lex_symbol(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_j = NULL;
    u_value t981 = f_lex_46_scan_3(v_s, *r_i, u_mk_closure(w_f_lex_46_is_symbol_1, 0));
    v_j = t981;
    u_value t982 = u_mk_con(&n_lex_46_TSym, 1, u_str_slice(v_s, *r_i, v_j));
    u_value t983 = v_j;
    *r_t = t982;
    *r_i = t983;
  }
}

void p_lex_46_lex_string(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_text = NULL;
    p_lex_46_lex_literal(v_s, u_mk_char(34), &v_text, r_i);
    u_value t984 = u_mk_con(&n_lex_46_TStr, 1, v_text);
    *r_t = t984;
  }
}

void p_lex_46_lex_char(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_start = NULL;
    u_value v_text = NULL;
    u_value t985 = *r_i;
    v_start = t985;
    p_lex_46_lex_literal(v_s, u_mk_char(39), &v_text, r_i);
    if (u_truth(f_prelude_46__33__61__2(u_str_size(v_text), u_mk_int(1)))) {
      {
        static struct u_object l_invalid_32_characte_986 = U_LIT("invalid character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_invalid_32_characte_986, v_start), "lex.LexError");
      }
    }
    u_value t987 = u_mk_con(&n_lex_46_TChar, 1, u_str_get(v_text, u_mk_int(0)));
    *r_t = t987;
  }
}

void p_lex_46_lex_token(u_value v_s, u_value *r_t, u_value *r_i) {
  {
    u_value v_c = NULL;
    u_value t988 = u_str_get(v_s, *r_i);
    v_c = t988;
    u_value s989 = v_c;
    int d990 = 0;
    if (!d990) {
      u_value t991 = u_is_letter(v_c);
      if (!u_truth(t991)) {
        t991 = u_eq(v_c, u_mk_char(95));
      }
      if (u_truth(t991)) {
        d990 = 1;
        {
          p_lex_46_lex_name(v_s, r_t, r_i);
        }
      }
    }
    if (!d990) {
      if (u_truth(u_is_digit(v_c))) {
        d990 = 1;
        {
          p_lex_46_lex_number(v_s, r_t, r_i);
        }
      }
    }
    if (!d990) {
      if (u_truth(f_lex_46_is_symbol_1(v_c))) {
        d990 = 1;
        {
          p_lex_46_lex_symbol(v_s, r_t, r_i);
        }
      }
    }
    if ((!d990) && u_equals(s989, u_mk_char(34))) {
      d990 = 1;
      {
        p_lex_46_lex_string(v_s, r_t, r_i);
      }
    }
    if ((!d990) && u_equals(s989, u_mk_char(39))) {
      d990 = 1;
      {
        p_lex_46_lex_char(v_s, r_t, r_i);
      }
    }
    if (!d990) {
      if (u_truth(f_lex_46_is_punct_1(v_c))) {
        d990 = 1;
        {
          u_value t992 = u_mk_con(&n_lex_46_TPunct, 1, v_c);
          u_value t993 = u_add(*r_i, u_mk_int(1));
          *r_t = t992;
          *r_i = t993;
        }
      }
    }
    if (!d990) {
      d990 = 1;
      {
        static struct u_object l_unexpected_32_chara_994 = U_LIT("unexpected character");
        u_raise(u_mk_con(&n_lex_46_LexError, 2, &l_unexpected_32_chara_994, *r_i), "lex.LexError");
      }
    }
    if (!d990) {
      u_no_case(s989);
    }
  }
}

void p_lex_46_lex(u_value v_s, u_value *r_ts) {
  {
    u_value v_i = NULL;
    u_value v_rev = NULL;
    u_value v_start = NULL;
    u_value v_t = NULL;
    u_value t995 = u_mk_int(0);
    v_i = t995;
    u_value t996 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t996;
    u_value t1003 = u_lt(v_i, u_str_size(v_s));
    while (u_truth(t1003)) {
      {
        u_value t997 = v_i;
        v_start = t997;
        if (u_truth(u_is_space(u_str_get(v_s, v_i)))) {
          {
            u_value t998 = u_add(v_i, u_mk_int(1));
            v_i = t998;
          }
        } else {
          u_value t999 = u_eq(u_str_get(v_s, v_i), u_mk_char(47));
          if (u_truth(t999)) {
            t999 = u_eq(f_lex_46_peek_2(v_s, u_add(v_i, u_mk_int(1))), u_mk_char(47));
          }
          if (u_truth(t999)) {
            {
              u_value t1001 = f_lex_46_scan_3(v_s, v_i, u_mk_closure(lam1000, 0));
              v_i = t1001;
            }
          } else {
            {
              p_lex_46_lex_token(v_s, &v_t, &v_i);
              u_value t1002 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_t, v_start), v_rev);
              v_rev = t1002;
            }
          }
        }
      }
      t1003 = u_lt(v_i, u_str_size(v_s));
    }
    u_value t1004 = f_list_46_reverse_1(u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_lex_46_TEnd, 0), v_i), v_rev));
    *r_ts = t1004;
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
  u_value s1005 = v_s;
  u_value t1006 = NULL;
  if ((!t1006) && u_is_con(s1005, &n_set_46_SLeaf)) {
    t1006 = u_mk_bool(0);
  }
  if ((!t1006) && u_is_con(s1005, &n_set_46_SNode)) {
    u_value v_l = u_field(s1005, 0);
    u_value v_y = u_field(s1005, 1);
    u_value v_r = u_field(s1005, 2);
    u_value t1010 = u_compare(v_x, v_y);
    u_value t1009;
    {
      u_value v_c = t1010;
      u_value t1008;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1008 = f_set_46_member_2(v_x, v_l);
      } else {
        u_value t1007;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1007 = f_set_46_member_2(v_x, v_r);
        } else {
          t1007 = u_mk_bool(1);
        }
        t1008 = t1007;
      }
      t1009 = t1008;
    }
    t1006 = t1009;
  }
  if (!t1006) {
    u_no_case(s1005);
  }
  return t1006;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1011 = v_s;
  u_value t1012 = NULL;
  if ((!t1012) && u_is_con(s1011, &n_set_46_SLeaf)) {
    t1012 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1012) && u_is_con(s1011, &n_set_46_SNode)) {
    u_value v_l = u_field(s1011, 0);
    u_value v_y = u_field(s1011, 1);
    u_value v_r = u_field(s1011, 2);
    u_value t1016 = u_compare(v_x, v_y);
    u_value t1015;
    {
      u_value v_c = t1016;
      u_value t1014;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1014 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
      } else {
        u_value t1013;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1013 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
        } else {
          t1013 = v_s;
        }
        t1014 = t1013;
      }
      t1015 = t1014;
    }
    t1012 = t1015;
  }
  if (!t1012) {
    u_no_case(s1011);
  }
  return t1012;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1017 = v_xs;
  u_value t1018 = NULL;
  if ((!t1018) && u_is_con(s1017, &n_list_46_Nil)) {
    t1018 = v_s;
  }
  if ((!t1018) && u_is_con(s1017, &n_list_46_Cons)) {
    u_value v_x = u_field(s1017, 0);
    u_value v_rest = u_field(s1017, 1);
    t1018 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1018) {
    u_no_case(s1017);
  }
  return t1018;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1019 = v_s;
  u_value t1020 = NULL;
  if ((!t1020) && u_is_con(s1019, &n_set_46_SLeaf)) {
    t1020 = v_rest;
  }
  if ((!t1020) && u_is_con(s1019, &n_set_46_SNode)) {
    u_value v_l = u_field(s1019, 0);
    u_value v_x = u_field(s1019, 1);
    u_value v_r = u_field(s1019, 2);
    t1020 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1020) {
    u_no_case(s1019);
  }
  return t1020;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1021 = v_s;
  u_value t1022 = NULL;
  if ((!t1022) && u_is_con(s1021, &n_set_46_SLeaf)) {
    t1022 = u_mk_int(0);
  }
  if ((!t1022) && u_is_con(s1021, &n_set_46_SNode)) {
    u_value v_l = u_field(s1021, 0);
    u_value v_r = u_field(s1021, 2);
    t1022 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1022) {
    u_no_case(s1021);
  }
  return t1022;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1023 = v_s;
  u_value t1024 = NULL;
  if ((!t1024) && u_is_con(s1023, &n_set_46_SLeaf)) {
    t1024 = u_mk_int(0);
  }
  if ((!t1024) && u_is_con(s1023, &n_set_46_SNode)) {
    u_value v_d = u_field(s1023, 3);
    t1024 = v_d;
  }
  if (!t1024) {
    u_no_case(s1023);
  }
  return t1024;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1025;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1025 = f_set_46_depth_1(v_r);
  } else {
    t1025 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1025);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1026 = v_s;
  u_value t1027 = NULL;
  if ((!t1027) && (u_is_con(s1026, &n_set_46_SNode) && u_is_con(u_field(s1026, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1026, 0);
    u_value v_x = u_field(s1026, 1);
    u_value v_b = u_field(u_field(s1026, 2), 0);
    u_value v_y = u_field(u_field(s1026, 2), 1);
    u_value v_c = u_field(u_field(s1026, 2), 2);
    t1027 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1027) {
    t1027 = v_s;
  }
  if (!t1027) {
    u_no_case(s1026);
  }
  return t1027;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1028 = v_s;
  u_value t1029 = NULL;
  if ((!t1029) && (u_is_con(s1028, &n_set_46_SNode) && u_is_con(u_field(s1028, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1028, 0), 0);
    u_value v_x = u_field(u_field(s1028, 0), 1);
    u_value v_b = u_field(u_field(s1028, 0), 2);
    u_value v_y = u_field(s1028, 1);
    u_value v_c = u_field(s1028, 2);
    t1029 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1029) {
    t1029 = v_s;
  }
  if (!t1029) {
    u_no_case(s1028);
  }
  return t1029;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1030 = v_s;
  u_value t1031 = NULL;
  if ((!t1031) && u_is_con(s1030, &n_set_46_SNode)) {
    u_value v_l = u_field(s1030, 0);
    t1031 = v_l;
  }
  if (!t1031) {
    t1031 = v_s;
  }
  if (!t1031) {
    u_no_case(s1030);
  }
  return t1031;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1032 = v_s;
  u_value t1033 = NULL;
  if ((!t1033) && u_is_con(s1032, &n_set_46_SNode)) {
    u_value v_r = u_field(s1032, 2);
    t1033 = v_r;
  }
  if (!t1033) {
    t1033 = v_s;
  }
  if (!t1033) {
    u_no_case(s1032);
  }
  return t1033;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1037;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1034;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1034 = f_set_46_turn_left_1(v_l);
    } else {
      t1034 = v_l;
    }
    t1037 = f_set_46_turn_right_1(f_set_46_snode_3(t1034, v_x, v_r));
  } else {
    u_value t1036;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1035;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1035 = f_set_46_turn_right_1(v_r);
      } else {
        t1035 = v_r;
      }
      t1036 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1035));
    } else {
      t1036 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1037 = t1036;
  }
  return t1037;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1038 = v_s;
  u_value t1039 = NULL;
  if ((!t1039) && u_is_con(s1038, &n_set_46_SLeaf)) {
    t1039 = u_mk_bool(1);
  }
  if ((!t1039) && u_is_con(s1038, &n_set_46_SNode)) {
    u_value v_l = u_field(s1038, 0);
    u_value v_r = u_field(s1038, 2);
    u_value v_d = u_field(s1038, 3);
    u_value t1040 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1040)) {
      t1040 = f_set_46_well_formed_1(v_r);
    }
    u_value t1041 = t1040;
    if (u_truth(t1041)) {
      t1041 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1042 = t1041;
    if (u_truth(t1042)) {
      t1042 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1043 = t1042;
    if (u_truth(t1043)) {
      t1043 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1039 = t1043;
  }
  if (!t1039) {
    u_no_case(s1038);
  }
  return t1039;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1044;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1044 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1044 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1044;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1045 = v_x;
  u_value t1046 = NULL;
  if ((!t1046) && u_is_con(s1045, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1045, 0);
    t1046 = v_y;
  }
  if (!t1046) {
    u_no_case(s1045);
  }
  return t1046;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1047 = v_x;
  u_value t1048 = NULL;
  if ((!t1048) && u_is_con(s1047, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1047, 1);
    t1048 = v_y;
  }
  if (!t1048) {
    u_no_case(s1047);
  }
  return t1048;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1049 = v_x;
  u_value t1050 = NULL;
  if ((!t1050) && u_is_con(s1049, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1049, 0);
    t1050 = v_y;
  }
  if (!t1050) {
    u_no_case(s1049);
  }
  return t1050;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1051 = v_x;
  u_value t1052 = NULL;
  if ((!t1052) && u_is_con(s1051, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1051, 1);
    t1052 = v_y;
  }
  if (!t1052) {
    u_no_case(s1051);
  }
  return t1052;
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
  static struct u_object l___1053 = U_LIT("_");
  static struct u_object l_assert_1054 = U_LIT("assert");
  static struct u_object l_assign_1055 = U_LIT("assign");
  static struct u_object l_call_1056 = U_LIT("call");
  static struct u_object l_case_1057 = U_LIT("case");
  static struct u_object l_catch_1058 = U_LIT("catch");
  static struct u_object l_data_1059 = U_LIT("data");
  static struct u_object l_else_1060 = U_LIT("else");
  static struct u_object l_end_1061 = U_LIT("end");
  static struct u_object l_eval_1062 = U_LIT("eval");
  static struct u_object l_false_1063 = U_LIT("false");
  static struct u_object l_for_1064 = U_LIT("for");
  static struct u_object l_function_1065 = U_LIT("function");
  static struct u_object l_if_1066 = U_LIT("if");
  static struct u_object l_import_1067 = U_LIT("import");
  static struct u_object l_in_1068 = U_LIT("in");
  static struct u_object l_infix_1069 = U_LIT("infix");
  static struct u_object l_lambda_1070 = U_LIT("lambda");
  static struct u_object l_let_1071 = U_LIT("let");
  static struct u_object l_local_1072 = U_LIT("local");
  static struct u_object l_match_1073 = U_LIT("match");
  static struct u_object l_out_1074 = U_LIT("out");
  static struct u_object l_postfix_1075 = U_LIT("postfix");
  static struct u_object l_prefix_1076 = U_LIT("prefix");
  static struct u_object l_procedure_1077 = U_LIT("procedure");
  static struct u_object l_raise_1078 = U_LIT("raise");
  static struct u_object l_ref_1079 = U_LIT("ref");
  static struct u_object l_run_1080 = U_LIT("run");
  static struct u_object l_skip_1081 = U_LIT("skip");
  static struct u_object l_test_1082 = U_LIT("test");
  static struct u_object l_then_1083 = U_LIT("then");
  static struct u_object l_true_1084 = U_LIT("true");
  static struct u_object l_type_1085 = U_LIT("type");
  static struct u_object l_try_1086 = U_LIT("try");
  static struct u_object l_while_1087 = U_LIT("while");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, &l___1053, u_mk_con(&n_list_46_Cons, 2, &l_assert_1054, u_mk_con(&n_list_46_Cons, 2, &l_assign_1055, u_mk_con(&n_list_46_Cons, 2, &l_call_1056, u_mk_con(&n_list_46_Cons, 2, &l_case_1057, u_mk_con(&n_list_46_Cons, 2, &l_catch_1058, u_mk_con(&n_list_46_Cons, 2, &l_data_1059, u_mk_con(&n_list_46_Cons, 2, &l_else_1060, u_mk_con(&n_list_46_Cons, 2, &l_end_1061, u_mk_con(&n_list_46_Cons, 2, &l_eval_1062, u_mk_con(&n_list_46_Cons, 2, &l_false_1063, u_mk_con(&n_list_46_Cons, 2, &l_for_1064, u_mk_con(&n_list_46_Cons, 2, &l_function_1065, u_mk_con(&n_list_46_Cons, 2, &l_if_1066, u_mk_con(&n_list_46_Cons, 2, &l_import_1067, u_mk_con(&n_list_46_Cons, 2, &l_in_1068, u_mk_con(&n_list_46_Cons, 2, &l_infix_1069, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1070, u_mk_con(&n_list_46_Cons, 2, &l_let_1071, u_mk_con(&n_list_46_Cons, 2, &l_local_1072, u_mk_con(&n_list_46_Cons, 2, &l_match_1073, u_mk_con(&n_list_46_Cons, 2, &l_out_1074, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1075, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1076, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1077, u_mk_con(&n_list_46_Cons, 2, &l_raise_1078, u_mk_con(&n_list_46_Cons, 2, &l_ref_1079, u_mk_con(&n_list_46_Cons, 2, &l_run_1080, u_mk_con(&n_list_46_Cons, 2, &l_skip_1081, u_mk_con(&n_list_46_Cons, 2, &l_test_1082, u_mk_con(&n_list_46_Cons, 2, &l_then_1083, u_mk_con(&n_list_46_Cons, 2, &l_true_1084, u_mk_con(&n_list_46_Cons, 2, &l_type_1085, u_mk_con(&n_list_46_Cons, 2, &l_try_1086, u_mk_con(&n_list_46_Cons, 2, &l_while_1087, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1088 = U_LIT("=");
  static struct u_object l__61__62__1089 = U_LIT("=>");
  static struct u_object l__58__61__1090 = U_LIT(":=");
  static struct u_object l__58__1091 = U_LIT(":");
  static struct u_object l__124__1092 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1088, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1089, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1090, u_mk_con(&n_list_46_Cons, 2, &l__58__1091, u_mk_con(&n_list_46_Cons, 2, &l__124__1092, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1093 = v_ns;
  u_value t1094 = NULL;
  if ((!t1094) && u_is_con(s1093, &n_list_46_Nil)) {
    t1094 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1094) && (u_is_con(s1093, &n_list_46_Cons) && (u_is_con(u_field(s1093, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1093, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1093, 0), 1);
    u_value v_ops = u_field(u_field(s1093, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1094 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1094) && u_is_con(s1093, &n_list_46_Cons)) {
    u_value v_more = u_field(s1093, 1);
    t1094 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1094) {
    u_no_case(s1093);
  }
  return t1094;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1095 = v_ns;
  u_value t1096 = NULL;
  if ((!t1096) && u_is_con(s1095, &n_list_46_Nil)) {
    t1096 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1096) && (u_is_con(s1095, &n_list_46_Cons) && (u_is_con(u_field(s1095, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1095, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1095, 0), 1);
    u_value v_ops = u_field(u_field(s1095, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1096 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1096) && u_is_con(s1095, &n_list_46_Cons)) {
    u_value v_more = u_field(s1095, 1);
    t1096 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1096) {
    u_no_case(s1095);
  }
  return t1096;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1097 = v_ns;
  u_value t1098 = NULL;
  if ((!t1098) && u_is_con(s1097, &n_list_46_Nil)) {
    t1098 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1098) && (u_is_con(s1097, &n_list_46_Cons) && u_is_con(u_field(s1097, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1097, 0), 0);
    u_value v_p = u_field(u_field(s1097, 0), 1);
    u_value v_ops = u_field(u_field(s1097, 0), 2);
    u_value t1099 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1099)) {
      t1099 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1100 = t1099;
    if (u_truth(t1100)) {
      t1100 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1100)) {
      t1098 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1098) && u_is_con(s1097, &n_list_46_Cons)) {
    u_value v_more = u_field(s1097, 1);
    t1098 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1098) {
    u_no_case(s1097);
  }
  return t1098;
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
  u_value s1101 = v_cs;
  u_value t1102 = NULL;
  if ((!t1102) && u_is_con(s1101, &n_list_46_Nil)) {
    t1102 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1102) && (u_is_con(s1101, &n_list_46_Cons) && u_is_con(u_field(s1101, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1101, 0), 2);
    u_value v_more = u_field(s1101, 1);
    t1102 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1102) && u_is_con(s1101, &n_list_46_Cons)) {
    u_value v_more = u_field(s1101, 1);
    t1102 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1102) {
    u_no_case(s1101);
  }
  return t1102;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1103 = v_ts;
  u_value t1104 = NULL;
  if ((!t1104) && u_is_con(s1103, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1103, 0), 0);
    t1104 = v_t;
  }
  if (!t1104) {
    t1104 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1104) {
    u_no_case(s1103);
  }
  return t1104;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1105 = v_ts;
  u_value t1106 = NULL;
  if ((!t1106) && (u_is_con(s1105, &n_list_46_Cons) && u_is_con(u_field(s1105, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1105, 1), 0), 0);
    t1106 = v_t;
  }
  if (!t1106) {
    t1106 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1106) {
    u_no_case(s1105);
  }
  return t1106;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1107 = v_ts;
  u_value t1108 = NULL;
  if ((!t1108) && u_is_con(s1107, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1107, 0), 1);
    t1108 = v_i;
  }
  if (!t1108) {
    t1108 = u_mk_int(0);
  }
  if (!t1108) {
    u_no_case(s1107);
  }
  return t1108;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1109 = v_t;
  u_value t1110 = NULL;
  if ((!t1110) && u_is_con(s1109, &n_lex_46_TName)) {
    u_value v_n = u_field(s1109, 0);
    t1110 = v_n;
  }
  if ((!t1110) && u_is_con(s1109, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1109, 0);
    t1110 = v_s;
  }
  if ((!t1110) && u_is_con(s1109, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1109, 0);
    t1110 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1110) && u_is_con(s1109, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1109, 0);
    t1110 = u_str_show(v_n);
  }
  if ((!t1110) && u_is_con(s1109, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1109, 0);
    t1110 = u_str_quote(v_s);
  }
  if ((!t1110) && u_is_con(s1109, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1109, 0);
    static struct u_object l__39__1111 = U_LIT("'");
    static struct u_object l__39__1112 = U_LIT("'");
    t1110 = u_concat(&l__39__1111, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1112));
  }
  if ((!t1110) && u_is_con(s1109, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1113 = U_LIT("end of input");
    t1110 = &l_end_32_of_32_input_1113;
  }
  if (!t1110) {
    u_no_case(s1109);
  }
  return t1110;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1114 = f_parse_46_head_token_1(v_ts);
  u_value t1115 = NULL;
  if ((!t1115) && u_is_con(s1114, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1114, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1115 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1115) && u_is_con(s1114, &n_lex_46_TName)) {
    u_value v_n = u_field(s1114, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1115 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1115) {
    t1115 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1115) {
    u_no_case(s1114);
  }
  return t1115;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1116 = *r_ts;
    int d1117 = 0;
    if ((!d1117) && u_is_con(s1116, &n_list_46_Cons)) {
      u_value v_more = u_field(s1116, 1);
      d1117 = 1;
      {
        u_value t1118 = v_more;
        *r_ts = t1118;
      }
    }
    if (!d1117) {
      d1117 = 1;
      {
      }
    }
    if (!d1117) {
      u_no_case(s1116);
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
        static struct u_object l__32_expected_32_but_32__1119 = U_LIT(" expected but ");
        static struct u_object l__32_found_1120 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1119, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1120))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1121 = f_parse_46_head_token_1(*r_ts);
    int d1122 = 0;
    if ((!d1122) && u_is_con(s1121, &n_lex_46_TName)) {
      u_value v_m = u_field(s1121, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1122 = 1;
        {
          u_value t1123 = v_m;
          *r_n = t1123;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1122) {
      d1122 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1124 = U_LIT("name expected but ");
        static struct u_object l__32_found_1125 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1124, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1125)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1122) {
      u_no_case(s1121);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1126 = f_parse_46_head_token_1(*r_ts);
    int d1127 = 0;
    if ((!d1127) && u_is_con(s1126, &n_lex_46_TName)) {
      u_value v_m = u_field(s1126, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1127 = 1;
        {
          static struct u_object l_operator_32__1128 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1129 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1128, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1129)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1127) {
      d1127 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1127) {
      u_no_case(s1126);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1130 = f_parse_46_head_token_1(*r_ts);
    int d1131 = 0;
    if ((!d1131) && u_is_con(s1130, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1130, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1131 = 1;
        {
          u_value t1132 = v_s;
          *r_n = t1132;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1131) {
      d1131 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1131) {
      u_no_case(s1130);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1133 = v_x;
  u_value t1134 = NULL;
  if ((!t1134) && u_is_con(s1133, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1133, 0);
    t1134 = v_y;
  }
  if (!t1134) {
    u_no_case(s1133);
  }
  return t1134;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1135 = v_x;
  u_value t1136 = NULL;
  if ((!t1136) && u_is_con(s1135, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1135, 1);
    t1136 = v_y;
  }
  if (!t1136) {
    u_no_case(s1135);
  }
  return t1136;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1137 = v_x;
  u_value t1138 = NULL;
  if ((!t1138) && u_is_con(s1137, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1137, 2);
    t1138 = v_y;
  }
  if (!t1138) {
    u_no_case(s1137);
  }
  return t1138;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1139 = v_x;
  u_value t1140 = NULL;
  if ((!t1140) && u_is_con(s1139, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1139, 3);
    t1140 = v_y;
  }
  if (!t1140) {
    u_no_case(s1139);
  }
  return t1140;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1141 = v_rest;
    int d1142 = 0;
    if ((!d1142) && u_is_con(s1141, &n_list_46_Nil)) {
      d1142 = 1;
      {
      }
    }
    if ((!d1142) && (u_is_con(s1141, &n_list_46_Cons) && u_is_con(u_field(s1141, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1141, 0), 0);
      u_value v_i = u_field(u_field(s1141, 0), 1);
      d1142 = 1;
      {
        static struct u_object l_unexpected_32_opera_1143 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1143, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1142) {
      d1142 = 1;
      {
        static struct u_object l_missing_32_operator_1144 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1144, v_at), "parse.ParseError");
      }
    }
    if (!d1142) {
      u_no_case(s1141);
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
    u_value s1145 = v_items;
    int d1146 = 0;
    if ((!d1146) && (u_is_con(s1145, &n_list_46_Cons) && u_is_con(u_field(s1145, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1145, 0), 0);
      u_value v_more = u_field(s1145, 1);
      d1146 = 1;
      {
        u_value t1147 = v_x;
        u_value t1148 = v_more;
        *r_a = t1147;
        *r_rest = t1148;
      }
    }
    if ((!d1146) && (u_is_con(s1145, &n_list_46_Cons) && u_is_con(u_field(s1145, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1145, 0), 0);
      u_value v_i = u_field(u_field(s1145, 0), 1);
      u_value v_more = u_field(s1145, 1);
      d1146 = 1;
      {
        u_value s1149 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1150 = 0;
        if ((!d1150) && u_is_con(s1149, &n_option_46_Some)) {
          u_value v_p = u_field(s1149, 0);
          d1150 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1151 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1151;
          }
        }
        if ((!d1150) && u_is_con(s1149, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1150 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1152 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1152), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1150) && u_is_con(s1149, &n_option_46_None)) {
          d1150 = 1;
          {
            static struct u_object l_undeclared_32_opera_1153 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1153, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1150) {
          u_no_case(s1149);
        }
      }
    }
    if ((!d1146) && u_is_con(s1145, &n_list_46_Nil)) {
      d1146 = 1;
      {
        static struct u_object l_missing_32_operand_1154 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1154, v_at), "parse.ParseError");
      }
    }
    if (!d1146) {
      u_no_case(s1145);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1155 = v_items;
    int d1156 = 0;
    if ((!d1156) && (u_is_con(s1155, &n_list_46_Cons) && u_is_con(u_field(s1155, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1155, 0), 0);
      u_value v_i = u_field(u_field(s1155, 0), 1);
      u_value v_tail = u_field(s1155, 1);
      d1156 = 1;
      {
        u_value s1157 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1158 = 0;
        if ((!d1158) && u_is_con(u_field(s1157, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1157, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1158 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1158) && u_is_con(u_field(s1157, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1157, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1157, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1158 = 1;
            {
              u_value t1159;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1159 = v_p;
              } else {
                t1159 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1159, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1158) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1158 = 1;
            {
              static struct u_object l_undeclared_32_opera_1160 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1160, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1158) {
          d1158 = 1;
          {
            u_value t1161 = v_lhs;
            u_value t1162 = v_items;
            *r_a = t1161;
            *r_rest = t1162;
          }
        }
        if (!d1158) {
          u_no_case(s1157);
        }
      }
    }
    if (!d1156) {
      d1156 = 1;
      {
        u_value t1163 = v_lhs;
        u_value t1164 = v_items;
        *r_a = t1163;
        *r_rest = t1164;
      }
    }
    if (!d1156) {
      u_no_case(s1155);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1165 = v_rest;
    int d1166 = 0;
    if ((!d1166) && (u_is_con(s1165, &n_list_46_Cons) && u_is_con(u_field(s1165, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1165, 0), 0);
      u_value v_i = u_field(u_field(s1165, 0), 1);
      u_value t1169 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1169)) {
        t1169 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1169)) {
        d1166 = 1;
        {
          static struct u_object l_operator_32__1167 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1168 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1167, u_concat(v_op, &l__32_is_32_not_32_associat_1168)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1166) {
      d1166 = 1;
      {
      }
    }
    if (!d1166) {
      u_no_case(s1165);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1170 = v_fp;
  u_value t1171 = NULL;
  if ((!t1171) && u_is_con(s1170, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1170, 0), 1);
    t1171 = u_eq(v_q, v_p);
  }
  if (!t1171) {
    t1171 = u_mk_bool(0);
  }
  if (!t1171) {
    u_no_case(s1170);
  }
  return t1171;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1172 = v_ts;
  u_value t1173 = NULL;
  if ((!t1173) && (u_is_con(s1172, &n_list_46_Cons) && (u_is_con(u_field(s1172, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1172, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1172, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1172, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1172, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1172, 0), 1);
    t1173 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1173) && (u_is_con(s1172, &n_list_46_Cons) && u_is_con(u_field(s1172, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1172, 0);
    t1173 = v_t;
  }
  if ((!t1173) && u_is_con(s1172, &n_list_46_Nil)) {
    t1173 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1173) {
    static struct u_object l__40_invalid_41__1174 = U_LIT("(invalid)");
    t1173 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1174), v_ts);
  }
  if (!t1173) {
    u_no_case(s1172);
  }
  return t1173;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1175 = v_ts;
  u_value t1176 = NULL;
  if ((!t1176) && (u_is_con(s1175, &n_list_46_Cons) && (u_is_con(u_field(s1175, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1175, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1175, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1175, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1175, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1175, 0), 1);
    t1176 = v_cs;
  }
  if (!t1176) {
    t1176 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1176) {
    u_no_case(s1175);
  }
  return t1176;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1177 = U_LIT("->");
  u_value t1184;
  if (u_truth(u_eq(v_op, &l__45__62__1177))) {
    u_value t1178 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1179 = &t1178;
    u_value l1180 = v_l;
    while (u_is_con(l1180, &n_list_46_Cons)) {
      u_value v_t = u_field(l1180, 0);
      if (u_truth(u_mk_bool(1))) {
        e1179 = u_append(e1179, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1180 = u_field(l1180, 1);
    }
    t1184 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1178, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1181 = U_LIT("*");
    u_value t1183;
    if (u_truth(u_eq(v_op, &l__42__1181))) {
      static struct u_object l__40_product_41__1182 = U_LIT("(product)");
      t1183 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1182), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1183 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1184 = t1183;
  }
  return t1184;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1185 = v_t;
  u_value t1186 = NULL;
  if ((!t1186) && (u_is_con(s1185, &n_syntax_46_TSort) && (u_is_con(u_field(s1185, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1185, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1185, 0), 1), u_mk_lit("(invalid)")))))) {
    t1186 = u_mk_bool(1);
  }
  if ((!t1186) && u_is_con(s1185, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1185, 1);
    t1186 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1186) && u_is_con(s1185, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1185, 0);
    u_value v_r = u_field(s1185, 1);
    u_value t1187 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1187)) {
      t1187 = f_parse_46_has_invalid_1(v_r);
    }
    t1186 = t1187;
  }
  if ((!t1186) && u_is_con(s1185, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1185, 0);
    t1186 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1186) {
    t1186 = u_mk_bool(0);
  }
  if (!t1186) {
    u_no_case(s1185);
  }
  return t1186;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1188 = v_ts;
  u_value t1189 = NULL;
  if ((!t1189) && u_is_con(s1188, &n_list_46_Nil)) {
    t1189 = u_mk_bool(0);
  }
  if ((!t1189) && u_is_con(s1188, &n_list_46_Cons)) {
    u_value v_t = u_field(s1188, 0);
    u_value v_more = u_field(s1188, 1);
    u_value t1190 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1190)) {
      t1190 = f_parse_46_any_invalid_1(v_more);
    }
    t1189 = t1190;
  }
  if (!t1189) {
    u_no_case(s1188);
  }
  return t1189;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1191 = v_t;
  u_value t1192 = NULL;
  if ((!t1192) && u_is_con(s1191, &n_lex_46_TName)) {
    u_value v_n = u_field(s1191, 0);
    u_value t1193 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1193)) {
      t1193 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1192 = t1193;
  }
  if ((!t1192) && (u_is_con(s1191, &n_lex_46_TPunct) && u_equals(u_field(s1191, 0), u_mk_char(40)))) {
    t1192 = u_mk_bool(1);
  }
  if (!t1192) {
    t1192 = u_mk_bool(0);
  }
  if (!t1192) {
    u_no_case(s1191);
  }
  return t1192;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1194 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1194;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1195 = f_parse_46_type_one_1(v_r);
    *r_t = t1195;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1196 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1196, v_start), "parse.ParseError");
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
    u_value t1197 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1197;
    u_value t1198 = u_mk_bool(1);
    v_next = t1198;
    u_value t1199 = u_mk_bool(1);
    v_go = t1199;
    u_value t1208 = v_go;
    while (u_truth(t1208)) {
      {
        u_value s1200 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1201 = 0;
        if ((!d1201) && u_is_con(s1200, &n_option_46_Some)) {
          u_value v_op = u_field(s1200, 0);
          d1201 = 1;
          {
            u_value t1202 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1203 = u_mk_bool(1);
            v_rev = t1202;
            v_next = t1203;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1201) && u_is_con(s1200, &n_option_46_None)) {
          u_value t1206 = v_next;
          if (u_truth(t1206)) {
            t1206 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1206)) {
            d1201 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1204 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1205 = u_mk_bool(0);
              v_rev = t1204;
              v_next = t1205;
            }
          }
        }
        if (!d1201) {
          d1201 = 1;
          {
            u_value t1207 = u_mk_bool(0);
            v_go = t1207;
          }
        }
        if (!d1201) {
          u_no_case(s1200);
        }
      }
      t1208 = v_go;
    }
    u_value s1209 = v_rev;
    int d1210 = 0;
    if ((!d1210) && u_is_con(s1209, &n_list_46_Nil)) {
      d1210 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1211 = U_LIT("type expected but ");
        static struct u_object l__32_found_1212 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1211, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1212)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1210) {
      d1210 = 1;
      {
        u_value t1213 = f_list_46_reverse_1(v_rev);
        *r_items = t1213;
      }
    }
    if (!d1210) {
      u_no_case(s1209);
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
            u_value t1214 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1214;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1215 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1215;
            }
          } else {
            {
              u_value t1216 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1216;
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
    u_value t1217 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1217;
    u_value t1218 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1218)) {
      t1218 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1218)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1219 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1219;
        u_value t1221 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1221)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1220 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1220;
          }
          t1221 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1222 = f_list_46_reverse_1(v_rev);
    *r_r = t1222;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1223 = v_es;
  u_value t1224 = NULL;
  if ((!t1224) && u_is_con(s1223, &n_list_46_Nil)) {
    t1224 = v_rest;
  }
  if ((!t1224) && u_is_con(s1223, &n_list_46_Cons)) {
    u_value v_e = u_field(s1223, 0);
    u_value v_more = u_field(s1223, 1);
    static struct u_object l_Cons_1225 = U_LIT("Cons");
    t1224 = f_parse_46_expr_op2_3(&l_Cons_1225, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1224) {
    u_no_case(s1223);
  }
  return t1224;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1226 = v_t;
  u_value t1227 = NULL;
  if ((!t1227) && u_is_con(s1226, &n_lex_46_TName)) {
    u_value v_n = u_field(s1226, 0);
    static struct u_object l_let_1228 = U_LIT("let");
    static struct u_object l_if_1229 = U_LIT("if");
    static struct u_object l_lambda_1230 = U_LIT("lambda");
    static struct u_object l_match_1231 = U_LIT("match");
    static struct u_object l_true_1232 = U_LIT("true");
    static struct u_object l_false_1233 = U_LIT("false");
    u_value t1235 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1228, u_mk_con(&n_list_46_Cons, 2, &l_if_1229, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1230, u_mk_con(&n_list_46_Cons, 2, &l_match_1231, u_mk_con(&n_list_46_Cons, 2, &l_true_1232, u_mk_con(&n_list_46_Cons, 2, &l_false_1233, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1235)) {
      u_value t1234 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1234)) {
        t1234 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1235 = t1234;
    }
    t1227 = t1235;
  }
  if ((!t1227) && u_is_con(s1226, &n_lex_46_TNum)) {
    t1227 = u_mk_bool(1);
  }
  if ((!t1227) && u_is_con(s1226, &n_lex_46_TStr)) {
    t1227 = u_mk_bool(1);
  }
  if ((!t1227) && u_is_con(s1226, &n_lex_46_TChar)) {
    t1227 = u_mk_bool(1);
  }
  if ((!t1227) && u_is_con(s1226, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1226, 0);
    u_value t1236 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1236)) {
      t1236 = u_eq(v_c, u_mk_char(91));
    }
    t1227 = t1236;
  }
  if (!t1227) {
    t1227 = u_mk_bool(0);
  }
  if (!t1227) {
    u_no_case(s1226);
  }
  return t1227;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1237 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1237))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1238 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1238;
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
    u_value t1239 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1239;
    u_value t1240 = u_mk_bool(1);
    v_next = t1240;
    u_value t1241 = u_mk_bool(1);
    v_go = t1241;
    u_value t1250 = v_go;
    while (u_truth(t1250)) {
      {
        u_value s1242 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1243 = 0;
        if ((!d1243) && u_is_con(s1242, &n_option_46_Some)) {
          u_value v_op = u_field(s1242, 0);
          d1243 = 1;
          {
            u_value t1244 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1245 = u_mk_bool(1);
            v_rev = t1244;
            v_next = t1245;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1243) && u_is_con(s1242, &n_option_46_None)) {
          u_value t1248 = v_next;
          if (u_truth(t1248)) {
            t1248 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1248)) {
            d1243 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1246 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1247 = u_mk_bool(0);
              v_rev = t1246;
              v_next = t1247;
            }
          }
        }
        if (!d1243) {
          d1243 = 1;
          {
            u_value t1249 = u_mk_bool(0);
            v_go = t1249;
          }
        }
        if (!d1243) {
          u_no_case(s1242);
        }
      }
      t1250 = v_go;
    }
    u_value s1251 = v_rev;
    int d1252 = 0;
    if ((!d1252) && u_is_con(s1251, &n_list_46_Nil)) {
      d1252 = 1;
      {
        static struct u_object l_expression_32_expec_1253 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1254 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1253, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1254)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1252) {
      d1252 = 1;
      {
        u_value t1255 = f_list_46_reverse_1(v_rev);
        *r_items = t1255;
      }
    }
    if (!d1252) {
      u_no_case(s1251);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1256 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1256;
    u_value t1258 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1258)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1257 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1257;
      }
      t1258 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1259 = f_list_46_reverse_1(v_rev);
    *r_es = t1259;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1260 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1260;
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
    u_value t1261 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1261)) {
      t1261 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1265 = t1261;
    while (u_truth(t1265)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1262 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1262;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1263 = U_LIT(".");
            u_value t1264 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1263, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1264;
          }
        }
      }
      u_value t1266 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1266)) {
        t1266 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1265 = t1266;
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
    u_value s1267 = f_parse_46_head_token_1(*r_ts);
    int d1268 = 0;
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TName) && u_equals(u_field(s1267, 0), u_mk_lit("let")))) {
      d1268 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TName) && u_equals(u_field(s1267, 0), u_mk_lit("if")))) {
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1269 = U_LIT("then");
        p_parse_46_keyword(&l_then_1269, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1270 = U_LIT("else");
        p_parse_46_keyword(&l_else_1270, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1271 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1271;
      }
    }
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TName) && u_equals(u_field(s1267, 0), u_mk_lit("lambda")))) {
      d1268 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TName) && u_equals(u_field(s1267, 0), u_mk_lit("match")))) {
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1272 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1272;
      }
    }
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TName) && u_equals(u_field(s1267, 0), u_mk_lit("true")))) {
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1273 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1273;
      }
    }
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TName) && u_equals(u_field(s1267, 0), u_mk_lit("false")))) {
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1274 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1274;
      }
    }
    if ((!d1268) && u_is_con(s1267, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1267, 0);
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1275 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1275;
      }
    }
    if ((!d1268) && u_is_con(s1267, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1267, 0);
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1276 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1276;
      }
    }
    if ((!d1268) && u_is_con(s1267, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1267, 0);
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1277 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1277;
      }
    }
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TPunct) && u_equals(u_field(s1267, 0), u_mk_char(40)))) {
      d1268 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1278 = v_es;
        int d1279 = 0;
        if ((!d1279) && (u_is_con(s1278, &n_list_46_Cons) && u_is_con(u_field(s1278, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1278, 0);
          d1279 = 1;
          {
            u_value t1280 = v_x;
            *r_e = t1280;
          }
        }
        if (!d1279) {
          d1279 = 1;
          {
            u_value t1281 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1281;
          }
        }
        if (!d1279) {
          u_no_case(s1278);
        }
      }
    }
    if ((!d1268) && (u_is_con(s1267, &n_lex_46_TPunct) && u_equals(u_field(s1267, 0), u_mk_char(91)))) {
      d1268 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1268) && u_is_con(s1267, &n_lex_46_TName)) {
      u_value v_n = u_field(s1267, 0);
      u_value t1283 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1283)) {
        t1283 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1283)) {
        d1268 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1282 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1282;
        }
      }
    }
    if (!d1268) {
      d1268 = 1;
      {
        static struct u_object l_expression_32_expec_1284 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1285 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1284, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1285)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1268) {
      u_no_case(s1267);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1286 = v_ps;
  u_value t1287 = NULL;
  if ((!t1287) && u_is_con(s1286, &n_list_46_Nil)) {
    t1287 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1287) && (u_is_con(s1286, &n_list_46_Cons) && u_is_con(u_field(s1286, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1286, 0), 0);
    u_value v_more = u_field(s1286, 1);
    u_value s1288 = f_parse_46_pattern_vars_1(v_more);
    u_value t1289 = NULL;
    if ((!t1289) && u_is_con(s1288, &n_option_46_Some)) {
      u_value v_xs = u_field(s1288, 0);
      t1289 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1289) && u_is_con(s1288, &n_option_46_None)) {
      t1289 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1289) {
      u_no_case(s1288);
    }
    t1287 = t1289;
  }
  if (!t1287) {
    t1287 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1287) {
    u_no_case(s1286);
  }
  return t1287;
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
    static struct u_object l__58__61__1290 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1290), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1291 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1292 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1291;
    v_es = t1292;
    u_value t1296 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1296)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1293 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1293), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1294 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1295 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1294;
        v_es = t1295;
      }
      t1296 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1297 = U_LIT("in");
    p_parse_46_keyword(&l_in_1297, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1298 = f_list_46_reverse_1(v_ps);
    u_value t1299 = f_list_46_reverse_1(v_es);
    v_ps = t1298;
    v_es = t1299;
    u_value s1300 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1301 = 0;
    if ((!d1301) && u_is_con(u_field(s1300, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1300, 0), 0);
      d1301 = 1;
      {
        u_value t1302 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1302;
      }
    }
    if ((!d1301) && (u_is_con(u_field(s1300, 0), &n_option_46_None) && (u_is_con(u_field(s1300, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1300, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1300, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1300, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1300, 1), 0);
      u_value v_c = u_field(u_field(s1300, 2), 0);
      d1301 = 1;
      {
        u_value t1303 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1303;
      }
    }
    if (!d1301) {
      d1301 = 1;
      {
        u_value t1304 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1304;
      }
    }
    if (!d1301) {
      u_no_case(s1300);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1305 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1305;
    static struct u_object l__61__62__1306 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1306)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1307 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1307;
        u_value t1309 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1309)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1308 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1308;
          }
          t1309 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1310 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1310), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1311 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1311;
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
        static struct u_object l_Nil_1312 = U_LIT("Nil");
        u_value t1313 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1312), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1313;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1314 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1314))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1315 = U_LIT("in");
            p_parse_46_keyword(&l_in_1315, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1316 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1317 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1316;
            v_lists = t1317;
            u_value t1321 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1321)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1318 = U_LIT("in");
                p_parse_46_keyword(&l_in_1318, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1319 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1320 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1319;
                v_lists = t1320;
              }
              t1321 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1322 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1322;
            static struct u_object l_if_1323 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1323))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1324 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1324;
          }
        } else {
          {
            u_value t1325 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1325;
            u_value t1327 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1327)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1326 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1326;
              }
              t1327 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1328 = U_LIT("Nil");
            u_value t1329 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1328), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1329;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1330 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1330;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1331 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1331))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1332 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1332;
      }
    } else {
      {
        u_value t1333 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1333;
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
    u_value t1334 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1334;
    static struct u_object l_case_1335 = U_LIT("case");
    u_value t1338 = f_parse_46_at_keyword_2(*r_ts, &l_case_1335);
    while (u_truth(t1338)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1336 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1336), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1337 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1337;
      }
      static struct u_object l_case_1339 = U_LIT("case");
      t1338 = f_parse_46_at_keyword_2(*r_ts, &l_case_1339);
    }
    static struct u_object l_else_1340 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1340))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1341 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1341;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1342 = U_LIT("end");
    p_parse_46_keyword(&l_end_1342, r_ts);
    u_value t1343 = f_list_46_reverse_1(v_rev);
    *r_cs = t1343;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1344 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1344))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1345 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1345, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1346 = v_ps;
  u_value t1347 = NULL;
  if ((!t1347) && u_is_con(s1346, &n_list_46_Nil)) {
    t1347 = v_rest;
  }
  if ((!t1347) && u_is_con(s1346, &n_list_46_Cons)) {
    u_value v_p = u_field(s1346, 0);
    u_value v_more = u_field(s1346, 1);
    static struct u_object l_Cons_1348 = U_LIT("Cons");
    t1347 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1348), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1347) {
    u_no_case(s1346);
  }
  return t1347;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1349 = v_t;
  u_value t1350 = NULL;
  if ((!t1350) && u_is_con(s1349, &n_lex_46_TName)) {
    u_value v_n = u_field(s1349, 0);
    static struct u_object l___1351 = U_LIT("_");
    static struct u_object l_true_1352 = U_LIT("true");
    static struct u_object l_false_1353 = U_LIT("false");
    u_value t1355 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1351, u_mk_con(&n_list_46_Cons, 2, &l_true_1352, u_mk_con(&n_list_46_Cons, 2, &l_false_1353, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1355)) {
      u_value t1354 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1354)) {
        t1354 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1355 = t1354;
    }
    t1350 = t1355;
  }
  if ((!t1350) && u_is_con(s1349, &n_lex_46_TNum)) {
    t1350 = u_mk_bool(1);
  }
  if ((!t1350) && u_is_con(s1349, &n_lex_46_TStr)) {
    t1350 = u_mk_bool(1);
  }
  if ((!t1350) && u_is_con(s1349, &n_lex_46_TChar)) {
    t1350 = u_mk_bool(1);
  }
  if ((!t1350) && u_is_con(s1349, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1349, 0);
    u_value t1356 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1356)) {
      t1356 = u_eq(v_c, u_mk_char(91));
    }
    t1350 = t1356;
  }
  if (!t1350) {
    t1350 = u_mk_bool(0);
  }
  if (!t1350) {
    u_no_case(s1349);
  }
  return t1350;
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
    u_value t1357 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1357;
    u_value t1358 = u_mk_bool(1);
    v_next = t1358;
    u_value t1359 = u_mk_bool(1);
    v_go = t1359;
    u_value t1368 = v_go;
    while (u_truth(t1368)) {
      {
        u_value s1360 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1361 = 0;
        if ((!d1361) && u_is_con(s1360, &n_option_46_Some)) {
          u_value v_op = u_field(s1360, 0);
          d1361 = 1;
          {
            u_value t1362 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1363 = u_mk_bool(1);
            v_rev = t1362;
            v_next = t1363;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1361) && u_is_con(s1360, &n_option_46_None)) {
          u_value t1366 = v_next;
          if (u_truth(t1366)) {
            t1366 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1366)) {
            d1361 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1364 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1365 = u_mk_bool(0);
              v_rev = t1364;
              v_next = t1365;
            }
          }
        }
        if (!d1361) {
          d1361 = 1;
          {
            u_value t1367 = u_mk_bool(0);
            v_go = t1367;
          }
        }
        if (!d1361) {
          u_no_case(s1360);
        }
      }
      t1368 = v_go;
    }
    u_value s1369 = v_rev;
    int d1370 = 0;
    if ((!d1370) && u_is_con(s1369, &n_list_46_Nil)) {
      d1370 = 1;
      {
        static struct u_object l_pattern_32_expected_1371 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1372 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1371, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1372)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1370) {
      d1370 = 1;
      {
        u_value t1373 = f_list_46_reverse_1(v_rev);
        *r_items = t1373;
      }
    }
    if (!d1370) {
      u_no_case(s1369);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1374 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1374;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1375 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1375;
        u_value t1377 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1377)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1376 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1376;
          }
          t1377 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1378 = f_list_46_reverse_1(v_rev);
    *r_ps = t1378;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1379 = f_parse_46_head_token_1(*r_ts);
    int d1380 = 0;
    if ((!d1380) && (u_is_con(s1379, &n_lex_46_TName) && u_equals(u_field(s1379, 0), u_mk_lit("_")))) {
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1381 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1381;
      }
    }
    if ((!d1380) && (u_is_con(s1379, &n_lex_46_TName) && u_equals(u_field(s1379, 0), u_mk_lit("true")))) {
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1382 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1382;
      }
    }
    if ((!d1380) && (u_is_con(s1379, &n_lex_46_TName) && u_equals(u_field(s1379, 0), u_mk_lit("false")))) {
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1383 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1383;
      }
    }
    if ((!d1380) && u_is_con(s1379, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1379, 0);
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1384 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1384;
      }
    }
    if ((!d1380) && u_is_con(s1379, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1379, 0);
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1385 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1385;
      }
    }
    if ((!d1380) && u_is_con(s1379, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1379, 0);
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1386 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1386;
      }
    }
    if ((!d1380) && (u_is_con(s1379, &n_lex_46_TPunct) && u_equals(u_field(s1379, 0), u_mk_char(40)))) {
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1387 = v_ps;
        int d1388 = 0;
        if ((!d1388) && (u_is_con(s1387, &n_list_46_Cons) && u_is_con(u_field(s1387, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1387, 0);
          d1388 = 1;
          {
            u_value t1389 = v_q;
            *r_p = t1389;
          }
        }
        if (!d1388) {
          d1388 = 1;
          {
            u_value t1390 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1390;
          }
        }
        if (!d1388) {
          u_no_case(s1387);
        }
      }
    }
    if ((!d1380) && (u_is_con(s1379, &n_lex_46_TPunct) && u_equals(u_field(s1379, 0), u_mk_char(91)))) {
      d1380 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1391 = U_LIT("Nil");
            u_value t1392 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1391), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1392;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1393 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1393;
            u_value t1395 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1395)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1394 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1394;
              }
              t1395 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1396 = U_LIT("Nil");
            u_value t1397 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1396), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1397;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1398 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1398;
          }
        }
      }
    }
    if (!d1380) {
      d1380 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1399 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1399;
          }
        } else {
          {
            u_value t1400 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1400;
          }
        }
      }
    }
    if (!d1380) {
      u_no_case(s1379);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1401 = v_t;
  u_value t1402 = NULL;
  if ((!t1402) && u_is_con(s1401, &n_lex_46_TName)) {
    u_value v_n = u_field(s1401, 0);
    static struct u_object l_skip_1403 = U_LIT("skip");
    static struct u_object l_if_1404 = U_LIT("if");
    static struct u_object l_while_1405 = U_LIT("while");
    static struct u_object l_call_1406 = U_LIT("call");
    static struct u_object l_match_1407 = U_LIT("match");
    static struct u_object l_assert_1408 = U_LIT("assert");
    static struct u_object l_assign_1409 = U_LIT("assign");
    static struct u_object l_raise_1410 = U_LIT("raise");
    static struct u_object l_try_1411 = U_LIT("try");
    t1402 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1403, u_mk_con(&n_list_46_Cons, 2, &l_if_1404, u_mk_con(&n_list_46_Cons, 2, &l_while_1405, u_mk_con(&n_list_46_Cons, 2, &l_call_1406, u_mk_con(&n_list_46_Cons, 2, &l_match_1407, u_mk_con(&n_list_46_Cons, 2, &l_assert_1408, u_mk_con(&n_list_46_Cons, 2, &l_assign_1409, u_mk_con(&n_list_46_Cons, 2, &l_raise_1410, u_mk_con(&n_list_46_Cons, 2, &l_try_1411, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1402) {
    t1402 = u_mk_bool(0);
  }
  if (!t1402) {
    u_no_case(s1401);
  }
  return t1402;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1412 = v_cs;
  u_value t1413 = NULL;
  if ((!t1413) && u_is_con(s1412, &n_list_46_Nil)) {
    t1413 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1413) && (u_is_con(s1412, &n_list_46_Cons) && u_is_con(u_field(s1412, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1412, 0), 0);
    u_value v_g = u_field(u_field(s1412, 0), 1);
    u_value v_b = u_field(u_field(s1412, 0), 2);
    u_value v_more = u_field(s1412, 1);
    t1413 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1413) {
    u_no_case(s1412);
  }
  return t1413;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1414 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1414;
    u_value t1415 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1415;
    u_value t1416 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1416;
    static struct u_object l_local_1417 = U_LIT("local");
    u_value t1419 = f_parse_46_at_keyword_2(*r_ts, &l_local_1417);
    while (u_truth(t1419)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1418 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1418)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1418 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1420 = U_LIT("local");
      t1419 = f_parse_46_at_keyword_2(*r_ts, &l_local_1420);
    }
    u_value t1422 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1422)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1421 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1421;
      }
      t1422 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1423 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1423;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1424 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1424;
    static struct u_object l__58__1425 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1425)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1426 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1426)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1427 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1427;
      }
    }
    u_value t1428 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1428;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1429 = f_parse_46_head_token_1(*r_ts);
    int d1430 = 0;
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("skip")))) {
      d1430 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1431 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1431;
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("if")))) {
      d1430 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("while")))) {
      d1430 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1432 = U_LIT("end");
        p_parse_46_keyword(&l_end_1432, r_ts);
        u_value t1433 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1433;
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("call")))) {
      d1430 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("match")))) {
      d1430 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1434 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1434;
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("assert")))) {
      d1430 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1435 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1435;
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("assign")))) {
      d1430 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("raise")))) {
      d1430 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1436 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1436;
      }
    }
    if ((!d1430) && (u_is_con(s1429, &n_lex_46_TName) && u_equals(u_field(s1429, 0), u_mk_lit("try")))) {
      d1430 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1437 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1437, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1438 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1438;
      }
    }
    if (!d1430) {
      d1430 = 1;
      {
        static struct u_object l_statement_32_expect_1439 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1440 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1439, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1440)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1430) {
      u_no_case(s1429);
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
    static struct u_object l_else_1441 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1441)))) {
      {
        static struct u_object l_end_1442 = U_LIT("end");
        p_parse_46_keyword(&l_end_1442, r_ts);
        u_value t1443 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1443;
      }
    } else {
      static struct u_object l_if_1444 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1444)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1445 = U_LIT("end");
          p_parse_46_keyword(&l_end_1445, r_ts);
        }
      }
    }
    u_value t1446 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1446;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1447 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1447), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1448 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1448;
    u_value t1451 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1451)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1449 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1449), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1450 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1450;
      }
      t1451 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1452 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1452;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1453 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1453;
    static struct u_object l_case_1454 = U_LIT("case");
    u_value t1457 = f_parse_46_at_keyword_2(*r_ts, &l_case_1454);
    while (u_truth(t1457)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1455 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1455), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1456 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1456;
      }
      static struct u_object l_case_1458 = U_LIT("case");
      t1457 = f_parse_46_at_keyword_2(*r_ts, &l_case_1458);
    }
    static struct u_object l_else_1459 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1459))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1460 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1460;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1461 = U_LIT("end");
    p_parse_46_keyword(&l_end_1461, r_ts);
    u_value t1462 = f_list_46_reverse_1(v_rev);
    *r_cs = t1462;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1464;
  if (u_truth(v_first)) {
    t1464 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1463 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1463)) {
      t1463 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1464 = t1463;
  }
  return t1464;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1466 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1466)) {
    static struct u_object l_out_1465 = U_LIT("out");
    t1466 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1465));
  }
  u_value t1468 = t1466;
  if (u_truth(t1468)) {
    static struct u_object l_ref_1467 = U_LIT("ref");
    t1468 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1467));
  }
  return t1468;
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
    u_value t1469 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1470 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1471 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1472 = u_mk_bool(1);
    v_ins = t1469;
    v_outs = t1470;
    v_refs = t1471;
    v_first = t1472;
    static struct u_object l_in_1473 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1473))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1474 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1475 = u_mk_bool(0);
        v_ins = t1474;
        v_first = t1475;
        u_value t1477 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1477)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1476 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1476;
          }
          t1477 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1478 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1478, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1479 = u_mk_bool(0);
        v_first = t1479;
      }
    }
    static struct u_object l_ref_1480 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1480, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1481 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1481;
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
    u_value t1482 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1482;
    u_value t1484 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1484)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1483 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1483;
      }
      t1484 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1485 = f_list_46_reverse_1(v_rev);
    *r_xs = t1485;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1486 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1486;
    static struct u_object l__58__1487 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1487)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1488 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1488;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1489 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1489;
    u_value t1491 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1491)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1490 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1490;
      }
      t1491 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1492 = f_list_46_reverse_1(v_rev);
    *r_xs = t1492;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1493 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1493;
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
  u_value s1494 = v_cs;
  u_value t1495 = NULL;
  if ((!t1495) && u_is_con(s1494, &n_list_46_Nil)) {
    t1495 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1495) && (u_is_con(s1494, &n_list_46_Cons) && u_is_con(u_field(s1494, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1494, 1);
    t1495 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1495) && (u_is_con(s1494, &n_list_46_Cons) && u_is_con(u_field(s1494, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1494, 1);
    t1495 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1495) && (u_is_con(s1494, &n_list_46_Cons) && u_is_con(u_field(s1494, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1494, 1);
    t1495 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1495) && u_is_con(s1494, &n_list_46_Cons)) {
    u_value v_c = u_field(s1494, 0);
    u_value v_more = u_field(s1494, 1);
    t1495 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1495) {
    u_no_case(s1494);
  }
  return t1495;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1496 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1497 = &t1496;
  u_value l1498 = v_cs;
  while (u_is_con(l1498, &n_list_46_Cons)) {
    u_value v_c = u_field(l1498, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1497 = u_append(e1497, v_c);
    }
    l1498 = u_field(l1498, 1);
  }
  return t1496;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1499 = v_c;
  u_value t1500 = NULL;
  if ((!t1500) && u_is_con(s1499, &n_syntax_46_CNotation)) {
    t1500 = u_mk_bool(1);
  }
  if (!t1500) {
    t1500 = u_mk_bool(0);
  }
  if (!t1500) {
    u_no_case(s1499);
  }
  return t1500;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1501 = v_x;
  u_value t1502 = NULL;
  if ((!t1502) && u_is_con(s1501, &n_parse_46_Context)) {
    u_value v_y = u_field(s1501, 0);
    t1502 = v_y;
  }
  if (!t1502) {
    u_no_case(s1501);
  }
  return t1502;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1503 = v_x;
  u_value t1504 = NULL;
  if ((!t1504) && u_is_con(s1503, &n_parse_46_Context)) {
    u_value v_y = u_field(s1503, 1);
    t1504 = v_y;
  }
  if (!t1504) {
    u_no_case(s1503);
  }
  return t1504;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1505 = v_x;
  u_value t1506 = NULL;
  if ((!t1506) && u_is_con(s1505, &n_parse_46_Context)) {
    u_value v_y = u_field(s1505, 2);
    t1506 = v_y;
  }
  if (!t1506) {
    u_no_case(s1505);
  }
  return t1506;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1507 = v_x;
  u_value t1508 = NULL;
  if ((!t1508) && u_is_con(s1507, &n_parse_46_Context)) {
    u_value v_y = u_field(s1507, 3);
    t1508 = v_y;
  }
  if (!t1508) {
    u_no_case(s1507);
  }
  return t1508;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1509 = v_x;
  u_value t1510 = NULL;
  if ((!t1510) && u_is_con(s1509, &n_parse_46_Context)) {
    u_value v_y = u_field(s1509, 4);
    t1510 = v_y;
  }
  if (!t1510) {
    u_no_case(s1509);
  }
  return t1510;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs), f_parse_46_Context_46_modules_1(v_cx), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1511 = v_c;
  u_value t1512 = NULL;
  if ((!t1512) && u_is_con(s1511, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1511, 0);
    u_value v_cs = u_field(s1511, 1);
    u_value t1513 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1514 = &t1513;
    u_value l1515 = v_cs;
    while (u_is_con(l1515, &n_list_46_Cons)) {
      u_value v_d = u_field(l1515, 0);
      if (u_truth(u_mk_bool(1))) {
        e1514 = u_append(e1514, f_parse_46_modules_of_1(v_d));
      }
      l1515 = u_field(l1515, 1);
    }
    t1512 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1513));
  }
  if (!t1512) {
    t1512 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1512) {
    u_no_case(s1511);
  }
  return t1512;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1516 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1517 = NULL;
  if ((!t1517) && u_is_con(s1516, &n_option_46_Some)) {
    u_value v_ns = u_field(s1516, 0);
    t1517 = v_ns;
  }
  if ((!t1517) && u_is_con(s1516, &n_option_46_None)) {
    t1517 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1517) {
    u_no_case(s1516);
  }
  return t1517;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1518 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1518;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1519 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1519;
        u_value t1521 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1521)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1520 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1520;
          }
          t1521 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1522 = f_list_46_reverse_1(v_rev);
    *r_ps = t1522;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1523 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1523;
    u_value s1524 = f_parse_46_head_token_1(*r_ts);
    int d1525 = 0;
    if ((!d1525) && (u_is_con(s1524, &n_lex_46_TName) && u_equals(u_field(s1524, 0), u_mk_lit("prefix")))) {
      d1525 = 1;
      {
        u_value t1526 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1526;
      }
    }
    if ((!d1525) && (u_is_con(s1524, &n_lex_46_TName) && u_equals(u_field(s1524, 0), u_mk_lit("postfix")))) {
      d1525 = 1;
      {
        u_value t1527 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1527;
      }
    }
    if (!d1525) {
      d1525 = 1;
      {
        u_value s1528 = f_parse_46_second_token_1(*r_ts);
        int d1529 = 0;
        if ((!d1529) && (u_is_con(s1528, &n_lex_46_TName) && u_equals(u_field(s1528, 0), u_mk_lit("left")))) {
          d1529 = 1;
          {
            u_value t1530 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1530;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1529) && (u_is_con(s1528, &n_lex_46_TName) && u_equals(u_field(s1528, 0), u_mk_lit("right")))) {
          d1529 = 1;
          {
            u_value t1531 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1531;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1529) {
          d1529 = 1;
          {
          }
        }
        if (!d1529) {
          u_no_case(s1528);
        }
      }
    }
    if (!d1525) {
      u_no_case(s1524);
    }
    p_parse_46_advance(r_ts);
    u_value s1532 = f_parse_46_head_token_1(*r_ts);
    int d1533 = 0;
    if ((!d1533) && u_is_con(s1532, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1532, 0);
      d1533 = 1;
      {
        u_value t1534 = v_n;
        v_p = t1534;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1533) {
      d1533 = 1;
      {
        static struct u_object l_precedence_32_expec_1535 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1536 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1535, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1536)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1533) {
      u_no_case(s1532);
    }
    u_value t1537 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1538 = u_mk_bool(1);
    v_ops = t1537;
    v_go = t1538;
    u_value t1544 = v_go;
    while (u_truth(t1544)) {
      {
        u_value s1539 = f_parse_46_head_token_1(*r_ts);
        int d1540 = 0;
        if ((!d1540) && u_is_con(s1539, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1539, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1540 = 1;
            {
              u_value t1541 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1541;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1540) && u_is_con(s1539, &n_lex_46_TName)) {
          u_value v_n = u_field(s1539, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1540 = 1;
            {
              u_value t1542 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1542;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1540) {
          d1540 = 1;
          {
            u_value t1543 = u_mk_bool(0);
            v_go = t1543;
          }
        }
        if (!d1540) {
          u_no_case(s1539);
        }
      }
      t1544 = v_go;
    }
    u_value s1545 = v_ops;
    int d1546 = 0;
    if ((!d1546) && u_is_con(s1545, &n_list_46_Nil)) {
      d1546 = 1;
      {
        static struct u_object l_operator_32_expecte_1547 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1548 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1547, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1548)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1546) {
      d1546 = 1;
      {
        u_value t1549 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1549;
      }
    }
    if (!d1546) {
      u_no_case(s1545);
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
    u_value t1550 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1551 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1550;
    *r_fs = t1551;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1552 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1553 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1552;
            *r_fs = t1553;
            u_value t1556 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1556)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1554 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1555 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1554;
                *r_fs = t1555;
              }
              t1556 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1557 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1558 = f_list_46_reverse_1(*r_fs);
    *r_k = t1557;
    *r_fs = t1558;
    u_value s1559 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1560 = 0;
    if ((!d1560) && u_is_con(s1559, &n_option_46_Some)) {
      u_value v_f = u_field(s1559, 0);
      d1560 = 1;
      {
        static struct u_object l_repeated_32_field_32__1561 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1562 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1561, u_concat(v_f, u_concat(&l__32_of_32__1562, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1560) && u_is_con(s1559, &n_option_46_None)) {
      d1560 = 1;
      {
      }
    }
    if (!d1560) {
      u_no_case(s1559);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1563 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1564 = 0;
    if ((!d1564) && (u_is_con(u_field(s1563, 0), &n_lex_46_TName) && (u_is_con(u_field(s1563, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1563, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1563, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1564 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1565 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1565;
        }
      }
    }
    if (!d1564) {
      d1564 = 1;
      {
        u_value t1566 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1566;
      }
    }
    if (!d1564) {
      u_no_case(s1563);
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
    static struct u_object l__58__61__1567 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1567), r_ts);
    static struct u_object l__124__1568 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1568)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1569 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1569;
    static struct u_object l__124__1570 = U_LIT("|");
    u_value t1572 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1570));
    while (u_truth(t1572)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1571 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1571;
      }
      static struct u_object l__124__1573 = U_LIT("|");
      t1572 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1573));
    }
    u_value s1574 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1575 = 0;
    if ((!d1575) && u_is_con(s1574, &n_option_46_Some)) {
      u_value v_f = u_field(s1574, 0);
      d1575 = 1;
      {
        static struct u_object l_field_32__1576 = U_LIT("field ");
        static struct u_object l__32_of_32__1577 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1578 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1576, u_concat(v_f, u_concat(&l__32_of_32__1577, u_concat(v_n, &l__32_has_32_different_32_t_1578)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1575) && u_is_con(s1574, &n_option_46_None)) {
      d1575 = 1;
      {
      }
    }
    if (!d1575) {
      u_no_case(s1574);
    }
    u_value t1579 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)));
    *r_cs = t1579;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1580 = v_fs;
  u_value t1581 = NULL;
  if ((!t1581) && u_is_con(s1580, &n_list_46_Nil)) {
    t1581 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1581) && (u_is_con(s1580, &n_list_46_Cons) && u_is_con(u_field(s1580, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1580, 0), 0);
    u_value v_rest = u_field(s1580, 1);
    t1581 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1581) && (u_is_con(s1580, &n_list_46_Cons) && u_is_con(u_field(s1580, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1580, 1);
    t1581 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1581) {
    u_no_case(s1580);
  }
  return t1581;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1582 = v_fs;
  u_value t1583 = NULL;
  if ((!t1583) && u_is_con(s1582, &n_list_46_Nil)) {
    t1583 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1583) && u_is_con(s1582, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1582, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1582, 0), 1), 3);
    u_value v_rest = u_field(s1582, 1);
    u_value t1584 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1585 = &t1584;
    u_value l1586 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1586, &n_list_46_Cons)) {
      u_value v_e = u_field(l1586, 0);
      if (u_truth(u_mk_bool(1))) {
        e1585 = u_append(e1585, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1586 = u_field(l1586, 1);
    }
    u_value t1587;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1584))) {
      t1587 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1587 = f_parse_46_differing_1(v_rest);
    }
    t1583 = t1587;
  }
  if (!t1583) {
    u_no_case(s1582);
  }
  return t1583;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1588 = v_e;
  u_value t1589 = NULL;
  if (!t1589) {
    u_value v_t = u_field(u_field(s1588, 1), 3);
    t1589 = v_t;
  }
  if (!t1589) {
    u_no_case(s1588);
  }
  return t1589;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1590 = v_fs;
  u_value t1591 = NULL;
  if ((!t1591) && u_is_con(s1590, &n_list_46_Nil)) {
    t1591 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1591) && u_is_con(s1590, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1590, 0), 0);
    u_value v_d = u_field(u_field(s1590, 0), 1);
    u_value v_rest = u_field(s1590, 1);
    u_value t1592;
    if (u_truth(u_eq(v_g, v_f))) {
      t1592 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1592 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1591 = t1592;
  }
  if (!t1591) {
    u_no_case(s1590);
  }
  return t1591;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1593 = v_ks;
  u_value t1594 = NULL;
  if ((!t1594) && u_is_con(s1593, &n_list_46_Nil)) {
    t1594 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1594) && (u_is_con(s1593, &n_list_46_Cons) && u_is_con(u_field(u_field(s1593, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1593, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1593, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1593, 0), 1);
    u_value v_rest = u_field(s1593, 1);
    t1594 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1594) {
    u_no_case(s1593);
  }
  return t1594;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1595 = v_fs;
  u_value t1596 = NULL;
  if ((!t1596) && u_is_con(s1595, &n_list_46_Nil)) {
    t1596 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1596) && (u_is_con(s1595, &n_list_46_Cons) && u_is_con(u_field(u_field(s1595, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1595, 0), 0), 0);
    u_value v_t = u_field(u_field(s1595, 0), 1);
    u_value v_rest = u_field(s1595, 1);
    t1596 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1596) && (u_is_con(s1595, &n_list_46_Cons) && u_is_con(u_field(u_field(s1595, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1595, 1);
    t1596 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1596) {
    u_no_case(s1595);
  }
  return t1596;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1601 = f_parse_46_fields_of_1(v_ks);
  u_value t1600;
  {
    u_value v_fs = t1601;
    u_value t1597 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1598 = &t1597;
    u_value l1599 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1599, &n_list_46_Cons)) {
      u_value v_f = u_field(l1599, 0);
      if (u_truth(u_mk_bool(1))) {
        e1598 = u_append(e1598, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1599 = u_field(l1599, 1);
    }
    t1600 = t1597;
  }
  return t1600;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1602 = U_LIT("x");
  u_value t1603 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1604 = &t1603;
  u_value l1605 = v_ps;
  while (u_is_con(l1605, &n_list_46_Cons)) {
    u_value v_p = u_field(l1605, 0);
    if (u_truth(u_mk_bool(1))) {
      e1604 = u_append(e1604, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1605 = u_field(l1605, 1);
  }
  u_value t1613 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1602), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1614 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1603);
  u_value t1612;
  {
    u_value v_x = t1613;
    u_value v_owner = t1614;
    static struct u_object l__46__1606 = U_LIT(".");
    static struct u_object l_x_1607 = U_LIT("x");
    u_value t1608 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1609 = &t1608;
    u_value l1610 = v_es;
    while (u_is_con(l1610, &n_list_46_Cons)) {
      u_value v_e = u_field(l1610, 0);
      if (u_truth(u_mk_bool(1))) {
        e1609 = u_append(e1609, f_parse_46_field_case_1(v_e));
      }
      l1610 = u_field(l1610, 1);
    }
    u_value t1611;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1611 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1611 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1612 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1606, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1607), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1608, t1611)));
  }
  return t1612;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1615 = v_es;
  u_value t1616 = NULL;
  if ((!t1616) && u_is_con(s1615, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1615, 0), 1), 3);
    t1616 = v_t;
  }
  if (!t1616) {
    t1616 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1616) {
    u_no_case(s1615);
  }
  return t1616;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1617 = v_e;
  u_value t1618 = NULL;
  if (!t1618) {
    u_value v_k = u_field(u_field(s1617, 1), 0);
    u_value v_i = u_field(u_field(s1617, 1), 1);
    u_value v_n = u_field(u_field(s1617, 1), 2);
    u_value t1619 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1620 = &t1619;
    u_value l1621 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1621, &n_list_46_Cons)) {
      u_value v_j = u_field(l1621, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1623;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1622 = U_LIT("y");
          t1623 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1622), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1623 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1620 = u_append(e1620, t1623);
      }
      l1621 = u_field(l1621, 1);
    }
    static struct u_object l_y_1624 = U_LIT("y");
    t1618 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1619), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1624), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1618) {
    u_no_case(s1617);
  }
  return t1618;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1625 = U_LIT("MissingField");
  static struct u_object l__46__1626 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1625), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1626, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1627;
  if (u_truth(u_eq(v_i, v_n))) {
    t1627 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1627 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1627;
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
    u_value t1628 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1628;
    u_value t1630 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1630)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1629 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1629;
      }
      t1630 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1631 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1631), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1632 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1632), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1633 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1633;
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
    u_value t1634 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1635 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1636 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1637 = u_mk_bool(1);
    v_ins = t1634;
    v_outs = t1635;
    v_refs = t1636;
    v_first = t1637;
    static struct u_object l_in_1638 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1638))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1639 = u_mk_bool(0);
        v_first = t1639;
      }
    }
    static struct u_object l_out_1640 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1640, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1641 = u_mk_bool(0);
        v_first = t1641;
      }
    }
    static struct u_object l_ref_1642 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1642, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1643 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1643), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1644 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1644;
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
    u_value t1645 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1645;
    u_value s1646 = f_parse_46_head_token_1(*r_ts);
    int d1647 = 0;
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("import")))) {
      d1647 = 1;
      {
        u_value t1648 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1648;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1649 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1649;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1650 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1650;
          }
        }
      }
    }
    if ((!d1647) && u_is_con(s1646, &n_lex_46_TName)) {
      u_value v_w = u_field(s1646, 0);
      static struct u_object l_prefix_1652 = U_LIT("prefix");
      static struct u_object l_postfix_1653 = U_LIT("postfix");
      static struct u_object l_infix_1654 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1652, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1653, u_mk_con(&n_list_46_Cons, 2, &l_infix_1654, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1647 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1651 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1651;
        }
      }
    }
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("type")))) {
      d1647 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1655 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1655), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1656 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1656;
      }
    }
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("data")))) {
      d1647 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("function")))) {
      d1647 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1657 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1657;
      }
    }
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("procedure")))) {
      d1647 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1658 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1658;
      }
    }
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("eval")))) {
      d1647 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1659 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1659;
      }
    }
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("run")))) {
      d1647 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1660 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1660), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1661 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1661;
      }
    }
    if ((!d1647) && (u_is_con(s1646, &n_lex_46_TName) && u_equals(u_field(s1646, 0), u_mk_lit("test")))) {
      d1647 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1662 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1662), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1663 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1663;
      }
    }
    if (!d1647) {
      d1647 = 1;
      {
        static struct u_object l_declaration_32_expe_1664 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1665 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1664, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1665)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1647) {
      u_no_case(s1646);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1666 = v_cs;
    int d1667 = 0;
    if ((!d1667) && u_is_con(s1666, &n_list_46_Nil)) {
      d1667 = 1;
      {
      }
    }
    if ((!d1667) && (u_is_con(s1666, &n_list_46_Cons) && u_is_con(u_field(s1666, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1666, 0), 0);
      u_value v_p = u_field(u_field(s1666, 0), 1);
      u_value v_ops = u_field(u_field(s1666, 0), 2);
      u_value v_more = u_field(s1666, 1);
      d1667 = 1;
      {
        u_value t1668 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1668;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1667) && (u_is_con(s1666, &n_list_46_Cons) && u_is_con(u_field(s1666, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1666, 0), 0);
      u_value v_ds = u_field(u_field(s1666, 0), 1);
      u_value v_more = u_field(s1666, 1);
      d1667 = 1;
      {
        u_value t1669 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1669;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1667) && (u_is_con(s1666, &n_list_46_Cons) && u_is_con(u_field(s1666, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1666, 0), 0);
      u_value v_more = u_field(s1666, 1);
      d1667 = 1;
      {
        u_value t1670 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1670;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1667) && u_is_con(s1666, &n_list_46_Cons)) {
      u_value v_more = u_field(s1666, 1);
      d1667 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1667) {
      u_no_case(s1666);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1671 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1671;
    u_value t1673 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1673)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1672 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1672;
      }
      t1673 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1674 = f_list_46_reverse_1(v_rev);
    *r_cs = t1674;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1675 = v_dir;
    int d1676 = 0;
    if ((!d1676) && u_is_con(s1675, &n_option_46_Some)) {
      u_value v_d = u_field(s1675, 0);
      d1676 = 1;
      {
        {
          struct u_handler *h1679 = u_push();
          if (setjmp(h1679->env) == 0) {
            {
              static struct u_object l__47__1677 = U_LIT("/");
              static struct u_object l__46_u_1678 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1677, u_concat(v_n, &l__46_u_1678))), r_text);
            }
            u_pop(h1679);
          } else {
            u_pop(h1679);
            int d1680 = 0;
            if ((!d1680) && (u_has_type(h1679, "os.OsError") && u_is_con(u_caught(h1679), &n_os_46_OsError))) {
              d1680 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1680) {
              u_reraise(h1679);
            }
          }
        }
      }
    }
    if ((!d1676) && u_is_con(s1675, &n_option_46_None)) {
      d1676 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1676) {
      u_no_case(s1675);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1681 = u_push();
      if (setjmp(h1681->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1681);
      } else {
        u_pop(h1681);
        int d1682 = 0;
        if ((!d1682) && (u_has_type(h1681, "os.OsError") && u_is_con(u_caught(h1681), &n_os_46_OsError))) {
          d1682 = 1;
          {
            static struct u_object l_unknown_32_module_32__1683 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1683, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1682) {
          u_reraise(h1681);
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
        static struct u_object l_cyclic_32_import_32_of_1684 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1684, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1685 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_base_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
    v_mcx = t1685;
    {
      struct u_handler *h1686 = u_push();
      if (setjmp(h1686->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1686);
      } else {
        u_pop(h1686);
        int d1687 = 0;
        if ((!d1687) && (u_has_type(h1686, "lex.LexError") && u_is_con(u_caught(h1686), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1686), 0);
          u_value v_i = u_field(u_caught(h1686), 1);
          d1687 = 1;
          {
            static struct u_object l__32_in_32__1688 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1688, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1687) && (u_has_type(h1686, "parse.ParseError") && u_is_con(u_caught(h1686), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1686), 0);
          u_value v_i = u_field(u_caught(h1686), 1);
          d1687 = 1;
          {
            static struct u_object l__32_in_32__1689 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1689, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1687) {
          u_reraise(h1686);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1690 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1691 = NULL;
  if ((!t1691) && u_is_con(s1690, &n_option_46_Some)) {
    u_value v_b = u_field(s1690, 0);
    t1691 = v_b;
  }
  if ((!t1691) && u_is_con(s1690, &n_option_46_None)) {
    t1691 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1691) {
    u_no_case(s1690);
  }
  return t1691;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1692 = v_cx;
    v_after = t1692;
    {
      struct u_handler *h1693 = u_push();
      if (setjmp(h1693->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1693);
      } else {
        u_pop(h1693);
        int d1694 = 0;
        if ((!d1694) && (u_has_type(h1693, "lex.LexError") && u_is_con(u_caught(h1693), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1693), 0);
          u_value v_i = u_field(u_caught(h1693), 1);
          d1694 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1694) {
          u_reraise(h1693);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1695 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1695;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1696 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1696;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx)), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1697 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1697))) {
      {
        static struct u_object l_prelude_1698 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1698, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1699 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1699, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1700 = v_xs;
  u_value t1701 = NULL;
  if ((!t1701) && (u_is_con(s1700, &n_list_46_Cons) && u_is_con(u_field(s1700, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1700, 0);
    t1701 = v_x;
  }
  if ((!t1701) && u_is_con(s1700, &n_list_46_Cons)) {
    u_value v_more = u_field(s1700, 1);
    t1701 = f_parse_46_last_part_1(v_more);
  }
  if (!t1701) {
    static struct u_object l__1702 = U_LIT("");
    t1701 = &l__1702;
  }
  if (!t1701) {
    u_no_case(s1700);
  }
  return t1701;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1707 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1706;
  {
    u_value v_f = t1707;
    u_value t1704 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1704)) {
      static struct u_object l__46_u_1703 = U_LIT(".u");
      t1704 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1703);
    }
    u_value t1705;
    if (u_truth(t1704)) {
      t1705 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1705 = v_f;
    }
    t1706 = t1705;
  }
  return t1706;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1712 = u_str_split(v_path, u_mk_char(47));
  u_value t1711;
  {
    u_value v_parts = t1712;
    u_value t1710;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1708 = U_LIT(".");
      t1710 = &l__46__1708;
    } else {
      static struct u_object l__47__1709 = U_LIT("/");
      t1710 = u_str_join(&l__47__1709, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1711 = t1710;
  }
  return t1711;
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
    static struct u_object l_prelude_1713 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1713, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1714 = u_mk_tuple(2, v_cs, v_ds);
    int d1715 = 0;
    if ((!d1715) && (u_is_con(u_field(s1714, 0), &n_list_46_Cons) && u_is_con(u_field(s1714, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1714, 0), 0);
      u_value v_more = u_field(u_field(s1714, 0), 1);
      u_value v_d = u_field(u_field(s1714, 1), 0);
      u_value v_rest = u_field(u_field(s1714, 1), 1);
      d1715 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1715) {
      d1715 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1715) {
      u_no_case(s1714);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1716 = v_paths;
    int d1717 = 0;
    if ((!d1717) && u_is_con(s1716, &n_list_46_Nil)) {
      d1717 = 1;
      {
      }
    }
    if ((!d1717) && u_is_con(s1716, &n_list_46_Cons)) {
      u_value v_path = u_field(s1716, 0);
      u_value v_more = u_field(s1716, 1);
      d1717 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1717) {
      u_no_case(s1716);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1718 = v_paths;
    int d1719 = 0;
    if ((!d1719) && u_is_con(s1718, &n_list_46_Nil)) {
      d1719 = 1;
      {
      }
    }
    if ((!d1719) && u_is_con(s1718, &n_list_46_Cons)) {
      u_value v_path = u_field(s1718, 0);
      u_value v_more = u_field(s1718, 1);
      d1719 = 1;
      {
        u_value t1720 = u_mk_bool(0);
        v_failed = t1720;
        {
          struct u_handler *h1721 = u_push();
          if (setjmp(h1721->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1721);
          } else {
            u_pop(h1721);
            int d1722 = 0;
            if ((!d1722) && (u_has_type(h1721, "parse.ParseError") && u_is_con(u_caught(h1721), &n_parse_46_ParseError))) {
              d1722 = 1;
              {
                u_value t1723 = u_mk_bool(1);
                v_failed = t1723;
              }
            }
            if (!d1722) {
              u_reraise(h1721);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1719) {
      u_no_case(s1718);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1724 = v_x;
  u_value t1725 = NULL;
  if ((!t1725) && u_is_con(s1724, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1724, 0);
    t1725 = v_y;
  }
  if (!t1725) {
    u_no_case(s1724);
  }
  return t1725;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1726 = v_x;
  u_value t1727 = NULL;
  if ((!t1727) && u_is_con(s1726, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1726, 1);
    t1727 = v_y;
  }
  if (!t1727) {
    u_no_case(s1726);
  }
  return t1727;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1728 = v_x;
  u_value t1729 = NULL;
  if ((!t1729) && u_is_con(s1728, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1728, 2);
    t1729 = v_y;
  }
  if (!t1729) {
    u_no_case(s1728);
  }
  return t1729;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1730 = v_x;
  u_value t1731 = NULL;
  if ((!t1731) && u_is_con(s1730, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1730, 3);
    t1731 = v_y;
  }
  if (!t1731) {
    u_no_case(s1730);
  }
  return t1731;
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
  static struct u_object l_u_value_1732 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1732);
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
  static struct u_object l_u_value_1733 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1733));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1734 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1735 = &t1734;
  u_value l1736 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1736, &n_list_46_Cons)) {
    u_value v_c = u_field(l1736, 0);
    if (u_truth(u_mk_bool(1))) {
      e1735 = u_append(e1735, f_compile_46_mangle_char_1(v_c));
    }
    l1736 = u_field(l1736, 1);
  }
  return u_string(f_list_46_concat_1(t1734));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1737 = u_is_letter(v_c);
  if (!u_truth(t1737)) {
    t1737 = u_is_digit(v_c);
  }
  u_value t1738 = t1737;
  if (!u_truth(t1738)) {
    t1738 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1741;
  if (u_truth(t1738)) {
    t1741 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1739 = U_LIT("_");
    static struct u_object l___1740 = U_LIT("_");
    t1741 = u_chars(u_concat(&l___1739, u_concat(u_str_show(u_ord(v_c)), &l___1740)));
  }
  return t1741;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1742 = U_LIT("f_");
  static struct u_object l___1743 = U_LIT("_");
  return u_concat(&l_f__1742, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1743, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1744 = U_LIT("p_");
  return u_concat(&l_p__1744, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1745 = U_LIT("v_");
  return u_concat(&l_v__1745, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1746 = U_LIT("r_");
  return u_concat(&l_r__1746, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1747 = U_LIT("+");
  static struct u_object l_u_add_1748 = U_LIT("u_add");
  static struct u_object l__45__1749 = U_LIT("-");
  static struct u_object l_u_sub_1750 = U_LIT("u_sub");
  static struct u_object l__42__1751 = U_LIT("*");
  static struct u_object l_u_mul_1752 = U_LIT("u_mul");
  static struct u_object l__47__1753 = U_LIT("/");
  static struct u_object l_u_div_1754 = U_LIT("u_div");
  static struct u_object l__37__1755 = U_LIT("%");
  static struct u_object l_u_mod_1756 = U_LIT("u_mod");
  static struct u_object l__60__1757 = U_LIT("<");
  static struct u_object l_u_lt_1758 = U_LIT("u_lt");
  static struct u_object l__61__61__1759 = U_LIT("==");
  static struct u_object l_u_eq_1760 = U_LIT("u_eq");
  static struct u_object l_compare_1761 = U_LIT("compare");
  static struct u_object l_u_compare_1762 = U_LIT("u_compare");
  static struct u_object l_chars_1763 = U_LIT("chars");
  static struct u_object l_u_chars_1764 = U_LIT("u_chars");
  static struct u_object l_string_1765 = U_LIT("string");
  static struct u_object l_u_string_1766 = U_LIT("u_string");
  static struct u_object l_ord_1767 = U_LIT("ord");
  static struct u_object l_u_ord_1768 = U_LIT("u_ord");
  static struct u_object l_chr_1769 = U_LIT("chr");
  static struct u_object l_u_chr_1770 = U_LIT("u_chr");
  static struct u_object l__94__1771 = U_LIT("^");
  static struct u_object l_u_concat_1772 = U_LIT("u_concat");
  static struct u_object l_str_size_1773 = U_LIT("str_size");
  static struct u_object l_u_str_size_1774 = U_LIT("u_str_size");
  static struct u_object l_str_join_1775 = U_LIT("str_join");
  static struct u_object l_u_str_join_1776 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1777 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1778 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1779 = U_LIT("str_split");
  static struct u_object l_u_str_split_1780 = U_LIT("u_str_split");
  static struct u_object l_str_get_1781 = U_LIT("str_get");
  static struct u_object l_u_str_get_1782 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1783 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1784 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1785 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1786 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1787 = U_LIT("str_show");
  static struct u_object l_u_str_show_1788 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1789 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1790 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1791 = U_LIT("str_number");
  static struct u_object l_u_str_number_1792 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1793 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1794 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1795 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1796 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1797 = U_LIT("is_space");
  static struct u_object l_u_is_space_1798 = U_LIT("u_is_space");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1747), u_mk_tuple(2, &l_u_add_1748, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1749), u_mk_tuple(2, &l_u_sub_1750, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1751), u_mk_tuple(2, &l_u_mul_1752, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1753), u_mk_tuple(2, &l_u_div_1754, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1755), u_mk_tuple(2, &l_u_mod_1756, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1757), u_mk_tuple(2, &l_u_lt_1758, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1759), u_mk_tuple(2, &l_u_eq_1760, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1761), u_mk_tuple(2, &l_u_compare_1762, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1763), u_mk_tuple(2, &l_u_chars_1764, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1765), u_mk_tuple(2, &l_u_string_1766, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1767), u_mk_tuple(2, &l_u_ord_1768, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1769), u_mk_tuple(2, &l_u_chr_1770, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1771), u_mk_tuple(2, &l_u_concat_1772, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1773), u_mk_tuple(2, &l_u_str_size_1774, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1775), u_mk_tuple(2, &l_u_str_join_1776, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1777), u_mk_tuple(2, &l_u_str_slice_1778, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1779), u_mk_tuple(2, &l_u_str_split_1780, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1781), u_mk_tuple(2, &l_u_str_get_1782, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1783), u_mk_tuple(2, &l_u_str_contains_1784, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1785), u_mk_tuple(2, &l_u_str_compare_1786, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1787), u_mk_tuple(2, &l_u_str_show_1788, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1789), u_mk_tuple(2, &l_u_str_quote_1790, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1791), u_mk_tuple(2, &l_u_str_number_1792, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1793), u_mk_tuple(2, &l_u_is_digit_1794, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1795), u_mk_tuple(2, &l_u_is_letter_1796, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1797), u_mk_tuple(2, &l_u_is_space_1798, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1799 = U_LIT("&");
  static struct u_object l_n__1800 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1799, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1800, f_compile_46_mangle_1(v_n))));
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
  u_value s1801 = v_n;
  u_value t1802 = NULL;
  if ((!t1802) && (u_is_con(s1801, &n_syntax_46_Name) && u_is_con(u_field(s1801, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1801, 0), 0);
    u_value v_b = u_field(s1801, 1);
    static struct u_object l_const_32_struct_32_u_n_1803 = U_LIT("const struct u_name");
    static struct u_object l_n__1804 = U_LIT("n_");
    t1802 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1803), u_concat(&l_n__1804, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1802) && (u_is_con(s1801, &n_syntax_46_Name) && u_is_con(u_field(s1801, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1801, 1);
    static struct u_object l_const_32_struct_32_u_n_1805 = U_LIT("const struct u_name");
    static struct u_object l_n__1806 = U_LIT("n_");
    static struct u_object l_NULL_1807 = U_LIT("NULL");
    t1802 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1805), u_concat(&l_n__1806, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1807), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1802) {
    u_no_case(s1801);
  }
  return t1802;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1808 = v_cs;
  u_value t1809 = NULL;
  if ((!t1809) && u_is_con(s1808, &n_list_46_Nil)) {
    t1809 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1809) && (u_is_con(s1808, &n_list_46_Cons) && u_is_con(u_field(s1808, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1808, 0), 2);
    u_value v_rest = u_field(s1808, 1);
    u_value t1810 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1811 = &t1810;
    u_value l1812 = v_ks;
    while (u_is_con(l1812, &n_list_46_Cons)) {
      u_value v_k = u_field(l1812, 0);
      if (u_truth(u_mk_bool(1))) {
        e1811 = u_append(e1811, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1812 = u_field(l1812, 1);
    }
    t1809 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1810, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1809) && u_is_con(s1808, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1808, 1);
    t1809 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1809) {
    u_no_case(s1808);
  }
  return t1809;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1813 = v_ks;
  u_value t1814 = NULL;
  if ((!t1814) && u_is_con(s1813, &n_list_46_Nil)) {
    t1814 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1814) && u_is_con(s1813, &n_list_46_Cons)) {
    u_value v_k = u_field(s1813, 0);
    u_value v_rest = u_field(s1813, 1);
    u_value t1815;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1815 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1815 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1814 = t1815;
  }
  if (!t1814) {
    u_no_case(s1813);
  }
  return t1814;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1816 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1817 = &t1816;
  u_value l1818 = v_xs;
  while (u_is_con(l1818, &n_list_46_Cons)) {
    u_value v_x = u_field(l1818, 0);
    if (u_truth(u_mk_bool(1))) {
      e1817 = u_append(e1817, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1818 = u_field(l1818, 1);
  }
  return t1816;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1819 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1819, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1820 = f_list_46_lookup_2(v_x, v_env);
  u_value t1821 = NULL;
  if ((!t1821) && u_is_con(s1820, &n_option_46_Some)) {
    u_value v_c = u_field(s1820, 0);
    t1821 = v_c;
  }
  if (!t1821) {
    t1821 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1821) {
    u_no_case(s1820);
  }
  return t1821;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1822 = v_c;
  u_value t1823 = NULL;
  if ((!t1823) && (u_is_con(s1822, &n_c_46_CeUnary) && u_equals(u_field(s1822, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1822, 1);
    t1823 = v_p;
  }
  if (!t1823) {
    static struct u_object l__38__1824 = U_LIT("&");
    t1823 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1824, v_c);
  }
  if (!t1823) {
    u_no_case(s1822);
  }
  return t1823;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1825 = v_t;
  u_value t1826 = NULL;
  if ((!t1826) && (u_is_con(s1825, &n_syntax_46_TSort) && u_is_con(u_field(s1825, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1825, 0);
    t1826 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1826) && u_is_con(s1825, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1825, 0);
    u_value v_args = u_field(s1825, 1);
    static struct u_object l__91__1827 = U_LIT("[");
    static struct u_object l__44__32__1828 = U_LIT(", ");
    u_value t1829 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1830 = &t1829;
    u_value l1831 = v_args;
    while (u_is_con(l1831, &n_list_46_Cons)) {
      u_value v_a = u_field(l1831, 0);
      if (u_truth(u_mk_bool(1))) {
        e1830 = u_append(e1830, f_compile_46_type_key_1(v_a));
      }
      l1831 = u_field(l1831, 1);
    }
    static struct u_object l__93__1832 = U_LIT("]");
    t1826 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1827, u_concat(u_str_join(&l__44__32__1828, t1829), &l__93__1832)));
  }
  if ((!t1826) && u_is_con(s1825, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1825, 0);
    u_value v_r = u_field(s1825, 1);
    static struct u_object l__40__1833 = U_LIT("(");
    static struct u_object l__44__32__1834 = U_LIT(", ");
    u_value t1835 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1836 = &t1835;
    u_value l1837 = v_args;
    while (u_is_con(l1837, &n_list_46_Cons)) {
      u_value v_a = u_field(l1837, 0);
      if (u_truth(u_mk_bool(1))) {
        e1836 = u_append(e1836, f_compile_46_type_key_1(v_a));
      }
      l1837 = u_field(l1837, 1);
    }
    static struct u_object l__41__32__45__62__32__1838 = U_LIT(") -> ");
    t1826 = u_concat(&l__40__1833, u_concat(u_str_join(&l__44__32__1834, t1835), u_concat(&l__41__32__45__62__32__1838, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1826) && u_is_con(s1825, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1825, 0);
    static struct u_object l__40__1839 = U_LIT("(");
    static struct u_object l__32__42__32__1840 = U_LIT(" * ");
    u_value t1841 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1842 = &t1841;
    u_value l1843 = v_ts;
    while (u_is_con(l1843, &n_list_46_Cons)) {
      u_value v_a = u_field(l1843, 0);
      if (u_truth(u_mk_bool(1))) {
        e1842 = u_append(e1842, f_compile_46_type_key_1(v_a));
      }
      l1843 = u_field(l1843, 1);
    }
    static struct u_object l__41__1844 = U_LIT(")");
    t1826 = u_concat(&l__40__1839, u_concat(u_str_join(&l__32__42__32__1840, t1841), &l__41__1844));
  }
  if (!t1826) {
    static struct u_object l__63__1845 = U_LIT("?");
    t1826 = &l__63__1845;
  }
  if (!t1826) {
    u_no_case(s1825);
  }
  return t1826;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1846 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1847 = &t1846;
  u_value l1848 = v_cs;
  while (u_is_con(l1848, &n_list_46_Cons)) {
    u_value v_c = u_field(l1848, 0);
    if (u_truth(u_mk_bool(1))) {
      e1847 = u_append(e1847, f_compile_46_fun_arities_1(v_c));
    }
    l1848 = u_field(l1848, 1);
  }
  u_value t1849 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1850 = &t1849;
  u_value l1851 = v_cs;
  while (u_is_con(l1851, &n_list_46_Cons)) {
    u_value v_c = u_field(l1851, 0);
    if (u_truth(u_mk_bool(1))) {
      e1850 = u_append(e1850, f_compile_46_constr_arities_1(v_c));
    }
    l1851 = u_field(l1851, 1);
  }
  u_value t1853 = f_list_46_concat_1(t1846);
  u_value t1854 = f_list_46_concat_1(t1849);
  u_value t1852;
  {
    u_value v_fs = t1853;
    u_value v_ks = t1854;
    t1852 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1852;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1855 = v_fs;
  u_value t1856 = NULL;
  if ((!t1856) && u_is_con(s1855, &n_list_46_Nil)) {
    t1856 = v_m;
  }
  if ((!t1856) && u_is_con(s1855, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1855, 0), 0);
    u_value v_k = u_field(u_field(s1855, 0), 1);
    u_value v_rest = u_field(s1855, 1);
    u_value s1857 = f_map_46_get_2(v_f, v_m);
    u_value t1858 = NULL;
    if ((!t1858) && u_is_con(s1857, &n_option_46_Some)) {
      u_value v_ks = u_field(s1857, 0);
      t1858 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1858) && u_is_con(s1857, &n_option_46_None)) {
      t1858 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1858) {
      u_no_case(s1857);
    }
    t1856 = t1858;
  }
  if (!t1856) {
    u_no_case(s1855);
  }
  return t1856;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1859 = v_gl;
  u_value t1860 = NULL;
  if ((!t1860) && u_is_con(s1859, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1859, 2);
    u_value s1861 = f_map_46_get_2(v_n, v_fm);
    u_value t1862 = NULL;
    if ((!t1862) && u_is_con(s1861, &n_option_46_Some)) {
      u_value v_ks = u_field(s1861, 0);
      t1862 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1862) && u_is_con(s1861, &n_option_46_None)) {
      t1862 = u_mk_bool(0);
    }
    if (!t1862) {
      u_no_case(s1861);
    }
    t1860 = t1862;
  }
  if (!t1860) {
    u_no_case(s1859);
  }
  return t1860;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1863 = v_gl;
  u_value t1864 = NULL;
  if ((!t1864) && u_is_con(s1863, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1863, 3);
    t1864 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1864) {
    u_no_case(s1863);
  }
  return t1864;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1865 = v_c;
  u_value t1866 = NULL;
  if ((!t1866) && u_is_con(s1865, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1865, 0);
    u_value v_xss = u_field(s1865, 2);
    t1866 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1866) {
    t1866 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1866) {
    u_no_case(s1865);
  }
  return t1866;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1867 = v_c;
  u_value t1868 = NULL;
  if ((!t1868) && u_is_con(s1867, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1867, 2);
    u_value t1869 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1870 = &t1869;
    u_value l1871 = v_ks;
    while (u_is_con(l1871, &n_list_46_Cons)) {
      u_value v_k = u_field(l1871, 0);
      if (u_truth(u_mk_bool(1))) {
        e1870 = u_append(e1870, f_compile_46_constr_arity_1(v_k));
      }
      l1871 = u_field(l1871, 1);
    }
    t1868 = t1869;
  }
  if (!t1868) {
    t1868 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1868) {
    u_no_case(s1867);
  }
  return t1868;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1872 = v_k;
  u_value t1873 = NULL;
  if ((!t1873) && u_is_con(s1872, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1872, 0);
    u_value v_ts = u_field(s1872, 1);
    t1873 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1873) {
    u_no_case(s1872);
  }
  return t1873;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1874 = v_xs;
  u_value t1875 = NULL;
  if ((!t1875) && u_is_con(s1874, &n_list_46_Nil)) {
    t1875 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1875) && u_is_con(s1874, &n_list_46_Cons)) {
    u_value v_x = u_field(s1874, 0);
    u_value v_rest = u_field(s1874, 1);
    t1875 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1875) {
    u_no_case(s1874);
  }
  return t1875;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1876 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1876, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1877 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1877, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1878 = v_cs;
  u_value t1879 = NULL;
  if ((!t1879) && u_is_con(s1878, &n_list_46_Nil)) {
    t1879 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1879) && (u_is_con(s1878, &n_list_46_Cons) && u_is_con(u_field(s1878, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1878, 0);
    t1879 = v_d;
  }
  if ((!t1879) && u_is_con(s1878, &n_list_46_Cons)) {
    u_value v_d = u_field(s1878, 0);
    u_value v_rest = u_field(s1878, 1);
    static struct u_object l__38__38__1880 = U_LIT("&&");
    t1879 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1880, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1879) {
    u_no_case(s1878);
  }
  return t1879;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1881 = v_l;
  u_value t1882 = NULL;
  if ((!t1882) && u_is_con(s1881, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1883 = U_LIT("u_mk_tuple");
    t1882 = f_c_46_c_call_2(&l_u_mk_tuple_1883, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1882) && u_is_con(s1881, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1881, 0);
    static struct u_object l_u_mk_bool_1884 = U_LIT("u_mk_bool");
    u_value t1885;
    if (u_truth(v_b)) {
      t1885 = u_mk_int(1);
    } else {
      t1885 = u_mk_int(0);
    }
    t1882 = f_c_46_c_call_2(&l_u_mk_bool_1884, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1885), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1882) && u_is_con(s1881, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1881, 0);
    u_value t1886 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1886)) {
      t1886 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1889;
    if (u_truth(t1886)) {
      static struct u_object l_u_mk_int_1887 = U_LIT("u_mk_int");
      t1889 = f_c_46_c_call_2(&l_u_mk_int_1887, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1888 = U_LIT("u_mk_int_str");
      t1889 = f_c_46_c_call_2(&l_u_mk_int_str_1888, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1882 = t1889;
  }
  if ((!t1882) && u_is_con(s1881, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1881, 0);
    static struct u_object l_u_mk_lit_1890 = U_LIT("u_mk_lit");
    t1882 = f_c_46_c_call_2(&l_u_mk_lit_1890, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1882) && u_is_con(s1881, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1881, 0);
    static struct u_object l_u_mk_char_1891 = U_LIT("u_mk_char");
    t1882 = f_c_46_c_call_2(&l_u_mk_char_1891, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1882) {
    u_no_case(s1881);
  }
  return t1882;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1892 = v_p;
  u_value t1893 = NULL;
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1892, 0);
    static struct u_object l_u_equals_1894 = U_LIT("u_equals");
    t1893 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1894, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1892, 0);
    u_value v_ps = u_field(s1892, 1);
    static struct u_object l_u_is_con_1895 = U_LIT("u_is_con");
    t1893 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1895, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1892, 0);
    t1893 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1893) {
    t1893 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1893) {
    u_no_case(s1892);
  }
  return t1893;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1896 = v_ps;
  u_value t1897 = NULL;
  if ((!t1897) && u_is_con(s1896, &n_list_46_Nil)) {
    t1897 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1897) && u_is_con(s1896, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1896, 0), 0);
    u_value v_i = u_field(u_field(s1896, 0), 1);
    u_value v_rest = u_field(s1896, 1);
    t1897 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1897) {
    u_no_case(s1896);
  }
  return t1897;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1898 = v_p;
  u_value t1899 = NULL;
  if ((!t1899) && (u_is_con(s1898, &n_syntax_46_PVar) && u_is_con(u_field(s1898, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1898, 0), 0);
    t1899 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1899) && u_is_con(s1898, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1898, 1);
    t1899 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1899) && u_is_con(s1898, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1898, 0);
    t1899 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1899) {
    t1899 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1899) {
    u_no_case(s1898);
  }
  return t1899;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1900 = v_ps;
  u_value t1901 = NULL;
  if ((!t1901) && u_is_con(s1900, &n_list_46_Nil)) {
    t1901 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1901) && u_is_con(s1900, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1900, 0), 0);
    u_value v_i = u_field(u_field(s1900, 0), 1);
    u_value v_rest = u_field(s1900, 1);
    t1901 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1901) {
    u_no_case(s1900);
  }
  return t1901;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1902 = v_p;
  u_value t1903 = NULL;
  if ((!t1903) && (u_is_con(s1902, &n_syntax_46_PVar) && u_is_con(u_field(s1902, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1902, 0), 0);
    t1903 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1903) && u_is_con(s1902, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1902, 1);
    u_value t1904 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1905 = &t1904;
    u_value l1906 = v_ps;
    while (u_is_con(l1906, &n_list_46_Cons)) {
      u_value v_q = u_field(l1906, 0);
      if (u_truth(u_mk_bool(1))) {
        e1905 = u_append(e1905, f_compile_46_pattern_names_1(v_q));
      }
      l1906 = u_field(l1906, 1);
    }
    t1903 = f_list_46_concat_1(t1904);
  }
  if ((!t1903) && u_is_con(s1902, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1902, 0);
    u_value t1907 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1908 = &t1907;
    u_value l1909 = v_ps;
    while (u_is_con(l1909, &n_list_46_Cons)) {
      u_value v_q = u_field(l1909, 0);
      if (u_truth(u_mk_bool(1))) {
        e1908 = u_append(e1908, f_compile_46_pattern_names_1(v_q));
      }
      l1909 = u_field(l1909, 1);
    }
    t1903 = f_list_46_concat_1(t1907);
  }
  if (!t1903) {
    t1903 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1903) {
    u_no_case(s1902);
  }
  return t1903;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1910 = v_e;
  u_value t1911 = NULL;
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_ELit)) {
    t1911 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1911) && (u_is_con(s1910, &n_syntax_46_EVar) && u_is_con(u_field(s1910, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1910, 0), 0);
    u_value t1912;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1912 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1912 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1911 = t1912;
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1910, 0);
    u_value v_args = u_field(s1910, 1);
    u_value t1913 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1914 = &t1913;
    u_value l1915 = v_args;
    while (u_is_con(l1915, &n_list_46_Cons)) {
      u_value v_a = u_field(l1915, 0);
      if (u_truth(u_mk_bool(1))) {
        e1914 = u_append(e1914, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1915 = u_field(l1915, 1);
    }
    t1911 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1913));
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1910, 0);
    u_value v_b = u_field(s1910, 1);
    t1911 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1910, 0);
    u_value v_b = u_field(s1910, 1);
    u_value t1916 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1917 = &t1916;
    u_value l1918 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1918, &n_list_46_Cons)) {
      u_value v_a = u_field(l1918, 0);
      if (u_truth(u_mk_bool(1))) {
        e1917 = u_append(e1917, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1918 = u_field(l1918, 1);
    }
    t1911 = f_list_46__43__43__2(f_list_46_concat_1(t1916), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1910, 0);
    u_value v_l = u_field(s1910, 1);
    u_value v_r = u_field(s1910, 2);
    t1911 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1910, 0);
    u_value v_cs = u_field(s1910, 1);
    u_value t1919 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1920 = &t1919;
    u_value l1921 = v_cs;
    while (u_is_con(l1921, &n_list_46_Cons)) {
      u_value v_b = u_field(l1921, 0);
      if (u_truth(u_mk_bool(1))) {
        e1920 = u_append(e1920, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1921 = u_field(l1921, 1);
    }
    t1911 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1919));
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1910, 0);
    u_value v_xs = u_field(s1910, 1);
    u_value v_ls = u_field(s1910, 2);
    u_value v_c = u_field(s1910, 3);
    u_value t1922 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1923 = &t1922;
    u_value l1924 = v_ls;
    while (u_is_con(l1924, &n_list_46_Cons)) {
      u_value v_l = u_field(l1924, 0);
      if (u_truth(u_mk_bool(1))) {
        e1923 = u_append(e1923, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1924 = u_field(l1924, 1);
    }
    t1911 = f_list_46__43__43__2(f_list_46_concat_1(t1922), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1910, 0);
    u_value t1925 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1926 = &t1925;
    u_value l1927 = v_es;
    while (u_is_con(l1927, &n_list_46_Cons)) {
      u_value v_a = u_field(l1927, 0);
      if (u_truth(u_mk_bool(1))) {
        e1926 = u_append(e1926, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1927 = u_field(l1927, 1);
    }
    t1911 = f_list_46_concat_1(t1925);
  }
  if ((!t1911) && u_is_con(s1910, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s1910, 0);
    t1911 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t1911) {
    u_no_case(s1910);
  }
  return t1911;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1928 = v_b;
  u_value t1929 = NULL;
  if ((!t1929) && (u_is_con(s1928, &n_syntax_46_Branch) && u_is_con(u_field(s1928, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1928, 0);
    u_value v_g = u_field(u_field(s1928, 1), 0);
    u_value v_e = u_field(s1928, 2);
    t1929 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1929) && (u_is_con(s1928, &n_syntax_46_Branch) && u_is_con(u_field(s1928, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1928, 0);
    u_value v_e = u_field(s1928, 2);
    t1929 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1929) {
    u_no_case(s1928);
  }
  return t1929;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1930 = v_xss;
  u_value t1931 = NULL;
  if ((!t1931) && u_is_con(s1930, &n_list_46_Nil)) {
    t1931 = v_b;
  }
  if ((!t1931) && u_is_con(s1930, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1930, 0);
    u_value v_rest = u_field(s1930, 1);
    t1931 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1931) {
    u_no_case(s1930);
  }
  return t1931;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1932 = *r_g;
    int d1933 = 0;
    if ((!d1933) && u_is_con(s1932, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1932, 0);
      u_value v_ds = u_field(s1932, 1);
      d1933 = 1;
      {
        u_value t1934 = u_concat(v_stem, u_str_show(v_n));
        u_value t1935 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1934;
        *r_g = t1935;
      }
    }
    if (!d1933) {
      u_no_case(s1932);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1936 = *r_g;
    int d1937 = 0;
    if ((!d1937) && u_is_con(s1936, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1936, 0);
      u_value v_ds = u_field(s1936, 1);
      d1937 = 1;
      {
        u_value t1938 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1938;
      }
    }
    if (!d1937) {
      u_no_case(s1936);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1939 = U_LIT("u_mk_closure");
        static struct u_object l_w__1940 = U_LIT("w_");
        u_value t1941 = f_c_46_c_call_2(&l_u_mk_closure_1939, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1940, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1941;
      }
    } else {
      {
        u_value s1942 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1943 = 0;
        if ((!d1943) && (u_is_con(s1942, &n_option_46_Some) && u_equals(u_field(s1942, 0), u_mk_int(0)))) {
          d1943 = 1;
          {
            static struct u_object l_u_mk_con_1944 = U_LIT("u_mk_con");
            u_value t1945 = f_c_46_c_call_2(&l_u_mk_con_1944, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1945;
          }
        }
        if ((!d1943) && u_is_con(s1942, &n_option_46_Some)) {
          d1943 = 1;
          {
            static struct u_object l_u_mk_closure_1946 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1947 = U_LIT("wc_");
            u_value t1948 = f_c_46_c_call_2(&l_u_mk_closure_1946, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1947, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1948;
          }
        }
        if ((!d1943) && u_is_con(s1942, &n_option_46_None)) {
          d1943 = 1;
          {
            u_value s1949 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1950 = 0;
            if ((!d1950) && u_is_con(s1949, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1949, 0), 0);
              d1950 = 1;
              {
                static struct u_object l_u_mk_closure_1951 = U_LIT("u_mk_closure");
                static struct u_object l_w_1952 = U_LIT("w");
                u_value t1953 = f_c_46_c_call_2(&l_u_mk_closure_1951, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1952, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1953;
              }
            }
            if ((!d1950) && u_is_con(s1949, &n_option_46_None)) {
              d1950 = 1;
              {
                static struct u_object l_the_32_variable_32__1954 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1954, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1950) {
              u_no_case(s1949);
            }
          }
        }
        if (!d1943) {
          u_no_case(s1942);
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
    u_value s1955 = v_e;
    int d1956 = 0;
    if ((!d1956) && (u_is_con(s1955, &n_syntax_46_ELit) && u_is_con(u_field(s1955, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1955, 0), 0);
      d1956 = 1;
      {
        static struct u_object l_l__1957 = U_LIT("l_");
        u_value t1958;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t1958 = u_str_size(v_s);
        } else {
          t1958 = u_mk_int(16);
        }
        static struct u_object l___1959 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1957, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t1958))), &l___1959)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1960 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1961 = U_LIT("U_LIT");
        static struct u_object l__38__1962 = U_LIT("&");
        u_value t1963 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1960), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1961, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1964 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1962, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1963;
        *r_r = t1964;
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1955, 0);
      d1956 = 1;
      {
        u_value t1965 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1966 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1965;
        *r_r = t1966;
      }
    }
    if ((!d1956) && (u_is_con(s1955, &n_syntax_46_EVar) && u_is_con(u_field(s1955, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1955, 0), 0);
      u_value v_ty = u_field(u_field(s1955, 0), 1);
      d1956 = 1;
      {
        u_value s1967 = f_list_46_lookup_2(v_n, v_env);
        int d1968 = 0;
        if ((!d1968) && u_is_con(s1967, &n_option_46_Some)) {
          u_value v_c = u_field(s1967, 0);
          d1968 = 1;
          {
            u_value t1969 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1970 = v_c;
            *r_ss = t1969;
            *r_r = t1970;
          }
        }
        if ((!d1968) && u_is_con(s1967, &n_option_46_None)) {
          d1968 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1971 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1971;
          }
        }
        if (!d1968) {
          u_no_case(s1967);
        }
      }
    }
    if ((!d1956) && (u_is_con(s1955, &n_syntax_46_EApp) && (u_is_con(u_field(s1955, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1955, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1955, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1955, 0), 0), 1);
      u_value v_args = u_field(s1955, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1956 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1955, 0);
      u_value v_args = u_field(s1955, 1);
      d1956 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1972 = U_LIT("u_apply");
        u_value t1973 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1974 = f_c_46_c_call_2(&l_u_apply_1972, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1973;
        *r_r = t1974;
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1955, 0);
      u_value v_b = u_field(s1955, 1);
      d1956 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1955, 0);
      u_value v_b = u_field(s1955, 1);
      d1956 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1975 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1975, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1976 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1976;
        u_value t1977 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1977;
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1955, 0);
      u_value v_l = u_field(s1955, 1);
      u_value v_rr = u_field(s1955, 2);
      d1956 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1978 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1978, &v_t, r_g);
        u_value t1979 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1979;
        u_value t1980 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1980;
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1955, 0);
      u_value v_cases = u_field(s1955, 1);
      d1956 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1981 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1981, &v_d, r_g);
        static struct u_object l_t_1982 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1982, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1983 = U_LIT("NULL");
        u_value t1984 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1983))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1984;
        static struct u_object l__33__1985 = U_LIT("!");
        static struct u_object l_u_no_case_1986 = U_LIT("u_no_case");
        u_value t1987 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1985, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1986, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1987;
        u_value t1988 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1988;
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1955, 0);
      u_value v_xs = u_field(s1955, 1);
      u_value v_lists = u_field(s1955, 2);
      u_value v_c = u_field(s1955, 3);
      d1956 = 1;
      {
        static struct u_object l_t_1989 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1989, &v_t, r_g);
        static struct u_object l_e_1990 = U_LIT("e");
        p_compile_46_gen_name(&l_e_1990, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_1991 = U_LIT("u_mk_con");
        static struct u_object l__38__1992 = U_LIT("&");
        u_value t1993 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_1991, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__1992, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t1993;
        u_value t1994 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1994;
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1955, 0);
      d1956 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_1995 = U_LIT("u_mk_tuple");
        u_value t1996 = f_c_46_c_call_2(&l_u_mk_tuple_1995, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t1996;
      }
    }
    if ((!d1956) && u_is_con(s1955, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s1955, 0);
      u_value v_ty = u_field(s1955, 1);
      d1956 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_1997 = U_LIT("u_raise");
        static struct u_object l_NULL_1998 = U_LIT("NULL");
        u_value t1999 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_1997, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2000 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1998);
        *r_ss = t1999;
        *r_r = t2000;
      }
    }
    if (!d1956) {
      u_no_case(s1955);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2001 = v_es;
    int d2002 = 0;
    if ((!d2002) && u_is_con(s2001, &n_list_46_Nil)) {
      d2002 = 1;
      {
        u_value t2003 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2004 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2003;
        *r_rs = t2004;
      }
    }
    if ((!d2002) && u_is_con(s2001, &n_list_46_Cons)) {
      u_value v_e = u_field(s2001, 0);
      u_value v_rest = u_field(s2001, 1);
      d2002 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2005 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2006 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2005;
        *r_rs = t2006;
      }
    }
    if (!d2002) {
      u_no_case(s2001);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2007 = v_rs;
    int d2008 = 0;
    if ((!d2008) && u_is_con(s2007, &n_list_46_Nil)) {
      d2008 = 1;
      {
        u_value t2009 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2009;
      }
    }
    if ((!d2008) && u_is_con(s2007, &n_list_46_Cons)) {
      u_value v_r = u_field(s2007, 0);
      u_value v_rest = u_field(s2007, 1);
      d2008 = 1;
      {
        static struct u_object l_t_2010 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2010, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2011 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2011;
      }
    }
    if (!d2008) {
      u_no_case(s2007);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2012 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2013 = NULL;
  if ((!t2013) && (u_is_con(u_field(s2012, 0), &n_list_46_Cons) && (u_is_con(u_field(s2012, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2012, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2012, 0), 0);
    u_value v_more = u_field(u_field(s2012, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2012, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2012, 1), 1);
    t2013 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2013) {
    t2013 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2013) {
    u_no_case(s2012);
  }
  return t2013;
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
    u_value s2014 = v_args;
    int d2015 = 0;
    if ((!d2015) && (u_is_con(s2014, &n_list_46_Cons) && (u_is_con(u_field(s2014, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2014, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2014, 0);
      u_value v_b = u_field(u_field(s2014, 1), 0);
      static struct u_object l_and_2022 = U_LIT("and");
      u_value t2024 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2022));
      if (!u_truth(t2024)) {
        static struct u_object l_or_2023 = U_LIT("or");
        t2024 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2023));
      }
      if (u_truth(t2024)) {
        d2015 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2016 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2016, &v_t, r_g);
          static struct u_object l_and_2017 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2017)))) {
            {
              u_value t2018 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2018;
            }
          } else {
            {
              static struct u_object l__33__2019 = U_LIT("!");
              u_value t2020 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2019, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2020;
            }
          }
          u_value t2021 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2021;
        }
      }
    }
    if (!d2015) {
      d2015 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2025 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2025)) {
          t2025 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2025)) {
          {
            u_value t2026 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2026;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2027 = U_LIT("u_mk_con");
              u_value t2028 = f_c_46_c_call_2(&l_u_mk_con_2027, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2028;
            }
          } else {
            {
              u_value s2029 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d2030 = 0;
              if ((!d2030) && u_is_con(s2029, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2029, 0), 0);
                u_value v_k = u_field(u_field(s2029, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2030 = 1;
                  {
                    u_value t2031 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2031;
                  }
                }
              }
              if (!d2030) {
                d2030 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2032 = U_LIT("u_apply");
                  u_value t2033 = f_c_46_c_call_2(&l_u_apply_2032, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2033;
                }
              }
              if (!d2030) {
                u_no_case(s2029);
              }
            }
          }
        }
      }
    }
    if (!d2015) {
      u_no_case(s2014);
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
    u_value t2034 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2035 = &t2034;
    u_value l2036 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2036, &n_list_46_Cons)) {
      u_value v_x = u_field(l2036, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2035 = u_append(e2035, v_x);
      }
      l2036 = u_field(l2036, 1);
    }
    u_value t2037 = t2034;
    v_caps = t2037;
    static struct u_object l_lam_2038 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2038, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2039 = U_LIT("env");
    static struct u_object l_args_2040 = U_LIT("args");
    u_value t2041 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2039), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2040));
    v_decls = t2041;
    static struct u_object l_env_2042 = U_LIT("env");
    static struct u_object l_args_2043 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2042), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2043), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2044 = U_LIT("u_mk_closure");
    u_value t2045 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2046 = &t2045;
    u_value l2047 = v_caps;
    while (u_is_con(l2047, &n_list_46_Cons)) {
      u_value v_x = u_field(l2047, 0);
      if (u_truth(u_mk_bool(1))) {
        e2046 = u_append(e2046, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2047 = u_field(l2047, 1);
    }
    u_value t2048 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2049 = f_c_46_c_call_2(&l_u_mk_closure_2044, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2045));
    *r_ss = t2048;
    *r_r = t2049;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2050 = v_xs;
  u_value t2051 = NULL;
  if ((!t2051) && u_is_con(s2050, &n_list_46_Nil)) {
    t2051 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2051) && u_is_con(s2050, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2050, 0), 0);
    u_value v_i = u_field(u_field(s2050, 0), 1);
    u_value v_rest = u_field(s2050, 1);
    t2051 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2051) {
    u_no_case(s2050);
  }
  return t2051;
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
    u_value s2052 = v_cases;
    int d2053 = 0;
    if ((!d2053) && u_is_con(s2052, &n_list_46_Nil)) {
      d2053 = 1;
      {
        u_value t2054 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2054;
      }
    }
    if ((!d2053) && (u_is_con(s2052, &n_list_46_Cons) && u_is_con(u_field(s2052, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2052, 0), 0);
      u_value v_guard = u_field(u_field(s2052, 0), 1);
      u_value v_e = u_field(u_field(s2052, 0), 2);
      u_value v_more = u_field(s2052, 1);
      d2053 = 1;
      {
        u_value t2055 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2055;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2056 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2056;
        u_value s2057 = v_guard;
        int d2058 = 0;
        if ((!d2058) && u_is_con(s2057, &n_option_46_Some)) {
          u_value v_c = u_field(s2057, 0);
          d2058 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2059 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2059;
          }
        }
        if ((!d2058) && u_is_con(s2057, &n_option_46_None)) {
          d2058 = 1;
          {
          }
        }
        if (!d2058) {
          u_no_case(s2057);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2060 = U_LIT("!");
        u_value t2061 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2060, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2061;
      }
    }
    if (!d2053) {
      u_no_case(s2052);
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
    u_value s2062 = u_mk_tuple(2, v_xs, v_lists);
    int d2063 = 0;
    if ((!d2063) && (u_is_con(u_field(s2062, 0), &n_list_46_Cons) && u_is_con(u_field(s2062, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2062, 0), 0);
      u_value v_more = u_field(u_field(s2062, 0), 1);
      u_value v_list = u_field(u_field(s2062, 1), 0);
      u_value v_others = u_field(u_field(s2062, 1), 1);
      d2063 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2064 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2064, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2065 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2065;
        static struct u_object l_u_is_con_2066 = U_LIT("u_is_con");
        u_value t2067 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2066, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2067;
      }
    }
    if (!d2063) {
      d2063 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2068 = U_LIT("u_append");
        u_value t2069 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2068, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2069;
      }
    }
    if (!d2063) {
      u_no_case(s2062);
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
    u_value s2070 = v_s;
    int d2071 = 0;
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SSkip)) {
      d2071 = 1;
      {
        u_value t2072 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2072;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2070, 0);
      d2071 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2073 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2073;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2070, 0);
      u_value v_body = u_field(s2070, 1);
      d2071 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
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
        u_value t2078 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2074, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2078;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2070, 0);
      u_value v_l = u_field(s2070, 1);
      u_value v_r = u_field(s2070, 2);
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2079 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2079;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2070, 0);
      u_value v_b = u_field(s2070, 1);
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2080 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2080, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2081 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2081;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2070, 0);
      u_value v_ins = u_field(s2070, 1);
      u_value v_outs = u_field(s2070, 2);
      u_value v_refs = u_field(s2070, 3);
      d2071 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2082 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2083 = &t2082;
        u_value l2084 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2084, &n_list_46_Cons)) {
          u_value v_x = u_field(l2084, 0);
          if (u_truth(u_mk_bool(1))) {
            e2083 = u_append(e2083, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2084 = u_field(l2084, 1);
        }
        u_value t2085 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2082))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2085;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2070, 0);
      u_value v_cases = u_field(s2070, 1);
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2086 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2086, &v_d, r_g);
        static struct u_object l_d_2087 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2087, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2088 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2088;
        static struct u_object l__33__2089 = U_LIT("!");
        static struct u_object l_u_no_case_2090 = U_LIT("u_no_case");
        u_value t2091 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2089, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2090, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2091;
      }
    }
    if ((!d2071) && (u_is_con(s2070, &n_syntax_46_SAssert) && (u_is_con(u_field(s2070, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2070, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2070, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2070, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2070, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2070, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2070, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2070, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2070, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2070, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2070, 0), 1), 1), 0);
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2092 = U_LIT("u_assert_equal");
        u_value t2093 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2092, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2093;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2070, 0);
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2094 = U_LIT("u_assert_true");
        u_value t2095 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2094, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2095;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2070, 0);
      u_value v_ty = u_field(s2070, 1);
      d2071 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2096 = U_LIT("u_raise");
        u_value t2097 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2096, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2097;
      }
    }
    if ((!d2071) && u_is_con(s2070, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2070, 0);
      u_value v_hs = u_field(s2070, 1);
      d2071 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2098 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2098, &v_d, r_g);
        static struct u_object l_d_2099 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2099, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2100 = U_LIT("u_pop");
        static struct u_object l__33__2101 = U_LIT("!");
        static struct u_object l_u_reraise_2102 = U_LIT("u_reraise");
        u_value t2103 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2100, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2101, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2102, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2103;
        static struct u_object l_struct_32_u_handler_2104 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2105 = U_LIT("u_push");
        static struct u_object l__61__61__2106 = U_LIT("==");
        static struct u_object l_setjmp_2107 = U_LIT("setjmp");
        static struct u_object l_env_2108 = U_LIT("env");
        static struct u_object l_u_pop_2109 = U_LIT("u_pop");
        u_value t2110 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2104)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2105, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2106, f_c_46_c_call_2(&l_setjmp_2107, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2108), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2109, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2110;
      }
    }
    if (!d2071) {
      u_no_case(s2070);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2111 = v_xs;
    int d2112 = 0;
    if ((!d2112) && u_is_con(s2111, &n_list_46_Nil)) {
      d2112 = 1;
      {
        u_value t2113 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2113;
      }
    }
    if ((!d2112) && u_is_con(s2111, &n_list_46_Cons)) {
      u_value v_x = u_field(s2111, 0);
      u_value v_rest = u_field(s2111, 1);
      d2112 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2114 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2114;
      }
    }
    if (!d2112) {
      u_no_case(s2111);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2115 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2116 = NULL;
  if ((!t2116) && (u_is_con(u_field(s2115, 0), &n_list_46_Cons) && (u_is_con(u_field(s2115, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2115, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2115, 0), 0);
    u_value v_more = u_field(u_field(s2115, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2115, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2115, 1), 1);
    t2116 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2116) {
    t2116 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2116) {
    u_no_case(s2115);
  }
  return t2116;
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
    static struct u_object l_u_caught_2117 = U_LIT("u_caught");
    u_value t2118 = f_c_46_c_call_2(&l_u_caught_2117, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2118;
    u_value s2119 = v_hs;
    int d2120 = 0;
    if ((!d2120) && u_is_con(s2119, &n_list_46_Nil)) {
      d2120 = 1;
      {
        u_value t2121 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2121;
      }
    }
    if ((!d2120) && (u_is_con(s2119, &n_list_46_Cons) && u_is_con(u_field(s2119, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2119, 0), 0);
      u_value v_p = u_field(u_field(s2119, 0), 1);
      u_value v_guard = u_field(u_field(s2119, 0), 2);
      u_value v_s = u_field(u_field(s2119, 0), 3);
      u_value v_more = u_field(s2119, 1);
      d2120 = 1;
      {
        u_value t2122 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2122;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2123 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2123;
        u_value s2124 = v_guard;
        int d2125 = 0;
        if ((!d2125) && u_is_con(s2124, &n_option_46_Some)) {
          u_value v_c = u_field(s2124, 0);
          d2125 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2126 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2126;
          }
        }
        if ((!d2125) && u_is_con(s2124, &n_option_46_None)) {
          d2125 = 1;
          {
          }
        }
        if (!d2125) {
          u_no_case(s2124);
        }
        u_value s2127 = v_p;
        int d2128 = 0;
        if ((!d2128) && u_is_con(s2127, &n_syntax_46_PWild)) {
          d2128 = 1;
          {
            u_value t2129 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2129;
          }
        }
        if (!d2128) {
          d2128 = 1;
          {
            static struct u_object l_u_has_type_2130 = U_LIT("u_has_type");
            u_value t2131 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2130, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2131;
          }
        }
        if (!d2128) {
          u_no_case(s2127);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2132 = U_LIT("!");
        u_value t2133 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2132, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2133;
      }
    }
    if (!d2120) {
      u_no_case(s2119);
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
    u_value s2134 = v_cases;
    int d2135 = 0;
    if ((!d2135) && u_is_con(s2134, &n_list_46_Nil)) {
      d2135 = 1;
      {
        u_value t2136 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2136;
      }
    }
    if ((!d2135) && (u_is_con(s2134, &n_list_46_Cons) && u_is_con(u_field(s2134, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2134, 0), 0);
      u_value v_guard = u_field(u_field(s2134, 0), 1);
      u_value v_s = u_field(u_field(s2134, 0), 2);
      u_value v_more = u_field(s2134, 1);
      d2135 = 1;
      {
        u_value t2137 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2137;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2138 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2138;
        u_value s2139 = v_guard;
        int d2140 = 0;
        if ((!d2140) && u_is_con(s2139, &n_option_46_Some)) {
          u_value v_c = u_field(s2139, 0);
          d2140 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2141 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2141;
          }
        }
        if ((!d2140) && u_is_con(s2139, &n_option_46_None)) {
          d2140 = 1;
          {
          }
        }
        if (!d2140) {
          u_no_case(s2139);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2142 = U_LIT("!");
        u_value t2143 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2142, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2143;
      }
    }
    if (!d2135) {
      u_no_case(s2134);
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
    u_value s2144 = v_c;
    int d2145 = 0;
    if ((!d2145) && u_is_con(s2144, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2144, 0);
      u_value v_xss = u_field(s2144, 2);
      u_value v_b = u_field(s2144, 4);
      d2145 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2146 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2147 = &t2146;
        u_value l2148 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2148, &n_list_46_Cons)) {
          u_value v_x = u_field(l2148, 0);
          if (u_truth(u_mk_bool(1))) {
            e2147 = u_append(e2147, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2148 = u_field(l2148, 1);
        }
        u_value t2149 = t2146;
        v_params = t2149;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2150 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2150;
          }
        } else {
          {
            u_value t2151 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2151;
          }
        }
        u_value t2152 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2153 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2152;
        *r_ms = t2153;
      }
    }
    if ((!d2145) && u_is_con(s2144, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2144, 0);
      u_value v_ins = u_field(s2144, 2);
      u_value v_outs = u_field(s2144, 3);
      u_value v_refs = u_field(s2144, 4);
      u_value v_body = u_field(s2144, 5);
      d2145 = 1;
      {
        u_value t2154 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2155 = &t2154;
        u_value l2156 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2156, &n_list_46_Cons)) {
          u_value v_x = u_field(l2156, 0);
          if (u_truth(u_mk_bool(1))) {
            e2155 = u_append(e2155, f_compile_46_pointer_1(v_x));
          }
          l2156 = u_field(l2156, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2154), v_body, &v_bs, r_g);
        u_value t2157 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2158 = &t2157;
        u_value l2159 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2159, &n_list_46_Cons)) {
          u_value v_x = u_field(l2159, 0);
          if (u_truth(u_mk_bool(1))) {
            e2158 = u_append(e2158, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2159 = u_field(l2159, 1);
        }
        u_value t2160 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2161 = &t2160;
        u_value l2162 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2162, &n_list_46_Cons)) {
          u_value v_x = u_field(l2162, 0);
          if (u_truth(u_mk_bool(1))) {
            e2161 = u_append(e2161, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2162 = u_field(l2162, 1);
        }
        u_value t2163 = f_list_46__43__43__2(t2157, t2160);
        v_params = t2163;
        u_value t2164 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2165 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2164;
        *r_ms = t2165;
      }
    }
    if ((!d2145) && u_is_con(s2144, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2144, 0);
      d2145 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2166 = U_LIT("u_print_line");
        u_value t2167 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2168 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2166, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2167;
        *r_ms = t2168;
      }
    }
    if ((!d2145) && (u_is_con(s2144, &n_syntax_46_CRun) && u_is_con(u_field(s2144, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2144, 1), 0);
      u_value v_body = u_field(u_field(s2144, 1), 1);
      d2145 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2169 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2170 = &t2169;
        u_value l2171 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2171, &n_list_46_Cons)) {
          u_value v_x = u_field(l2171, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2172 = U_LIT("NULL");
            e2170 = u_append(e2170, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2172))));
          }
          l2171 = u_field(l2171, 1);
        }
        u_value t2173 = t2169;
        v_copy = t2173;
        u_value t2174 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2175 = &t2174;
        u_value l2176 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2176, &n_list_46_Cons)) {
          u_value v_x = u_field(l2176, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2177 = U_LIT("u_print_binding");
            e2175 = u_append(e2175, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2177, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2176 = u_field(l2176, 1);
        }
        u_value t2178 = t2174;
        v_back = t2178;
        u_value t2179 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2180 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2179;
        *r_ms = t2180;
      }
    }
    if ((!d2145) && u_is_con(s2144, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2144, 1);
      d2145 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2181 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2182 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2181;
        *r_ms = t2182;
      }
    }
    if ((!d2145) && u_is_con(s2144, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2144, 1);
      d2145 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2183 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2184 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2183;
        *r_ms = t2184;
      }
    }
    if (!d2145) {
      d2145 = 1;
      {
        u_value t2185 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2186 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2185;
        *r_ms = t2186;
      }
    }
    if (!d2145) {
      u_no_case(s2144);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2187 = v_cs;
    int d2188 = 0;
    if ((!d2188) && u_is_con(s2187, &n_list_46_Nil)) {
      d2188 = 1;
      {
        u_value t2189 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2190 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2189;
        *r_ms = t2190;
      }
    }
    if ((!d2188) && u_is_con(s2187, &n_list_46_Cons)) {
      u_value v_c = u_field(s2187, 0);
      u_value v_rest = u_field(s2187, 1);
      d2188 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2191 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2192 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2191;
        *r_ms = t2192;
      }
    }
    if (!d2188) {
      u_no_case(s2187);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2193 = U_LIT("handler");
  u_value t2220 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2193);
  u_value t2219;
  {
    u_value v_h = t2220;
    static struct u_object l_static_32_u_value_2194 = U_LIT("static u_value");
    static struct u_object l_cache_2195 = U_LIT("cache");
    static struct u_object l_NULL_2196 = U_LIT("NULL");
    static struct u_object l__61__61__2197 = U_LIT("==");
    static struct u_object l_cache_2198 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2199 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2200 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2201 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2202 = U_LIT("cache");
    static struct u_object l_cache_2203 = U_LIT("cache");
    static struct u_object l_cache_2204 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2205 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2206 = U_LIT("struct u_handler");
    static struct u_object l_handler_2207 = U_LIT("handler");
    static struct u_object l_u_push_2208 = U_LIT("u_push");
    static struct u_object l__33__61__2209 = U_LIT("!=");
    static struct u_object l_setjmp_2210 = U_LIT("setjmp");
    static struct u_object l_env_2211 = U_LIT("env");
    static struct u_object l_u_pop_2212 = U_LIT("u_pop");
    static struct u_object l_cache_2213 = U_LIT("cache");
    static struct u_object l_NULL_2214 = U_LIT("NULL");
    static struct u_object l_u_reraise_2215 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2216 = U_LIT("u_pop");
    static struct u_object l_cache_2217 = U_LIT("cache");
    static struct u_object l_cache_2218 = U_LIT("cache");
    t2219 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2194), &l_cache_2195, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2196))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2197, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2198), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2199)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2200, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2201, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2202), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2203))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2204), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2205)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2206)), &l_handler_2207, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2208, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2209, f_c_46_c_call_2(&l_setjmp_2210, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2211), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2212, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2213), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2214)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2215, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2216, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2217), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2218))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2219;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2221 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2222 = &t2221;
  u_value l2223 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2223, &n_list_46_Cons)) {
    u_value v_p = u_field(l2223, 0);
    if (u_truth(u_mk_bool(1))) {
      e2222 = u_append(e2222, f_compile_46_fun_wrapper_1(v_p));
    }
    l2223 = u_field(l2223, 1);
  }
  u_value t2224 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2225 = &t2224;
  u_value l2226 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2226, &n_list_46_Cons)) {
    u_value v_p = u_field(l2226, 0);
    if (u_truth(u_mk_bool(1))) {
      e2225 = u_append(e2225, f_compile_46_constr_wrapper_1(v_p));
    }
    l2226 = u_field(l2226, 1);
  }
  u_value t2227 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2228 = &t2227;
  u_value l2229 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2229, &n_list_46_Cons)) {
    u_value v_p = u_field(l2229, 0);
    if (u_truth(u_mk_bool(1))) {
      e2228 = u_append(e2228, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2229 = u_field(l2229, 1);
  }
  return f_list_46__43__43__2(t2221, f_list_46__43__43__2(f_list_46_concat_1(t2224), t2227));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2230 = v_p;
  u_value t2231 = NULL;
  if (!t2231) {
    u_value v_n = u_field(s2230, 0);
    u_value v_k = u_field(s2230, 1);
    static struct u_object l_w__2232 = U_LIT("w_");
    t2231 = f_compile_46_wrapper_3(u_concat(&l_w__2232, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2231) {
    u_no_case(s2230);
  }
  return t2231;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2233 = v_p;
  u_value t2234 = NULL;
  if (!t2234) {
    u_value v_f = u_field(s2233, 0);
    u_value v_k = u_field(s2233, 1);
    static struct u_object l_w_2235 = U_LIT("w");
    t2234 = f_compile_46_wrapper_3(u_concat(&l_w_2235, v_f), v_f, v_k);
  }
  if (!t2234) {
    u_no_case(s2233);
  }
  return t2234;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2236 = U_LIT("env");
  static struct u_object l_args_2237 = U_LIT("args");
  u_value t2238 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2239 = &t2238;
  u_value l2240 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2240, &n_list_46_Cons)) {
    u_value v_i = u_field(l2240, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2241 = U_LIT("args");
      e2239 = u_append(e2239, f_compile_46_index_2(&l_args_2241, v_i));
    }
    l2240 = u_field(l2240, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2236), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2237), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2238))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2242 = v_p;
  u_value t2243 = NULL;
  if ((!t2243) && u_equals(u_field(s2242, 1), u_mk_int(0))) {
    t2243 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2243) {
    u_value v_n = u_field(s2242, 0);
    u_value v_k = u_field(s2242, 1);
    static struct u_object l_wc__2244 = U_LIT("wc_");
    static struct u_object l_env_2245 = U_LIT("env");
    static struct u_object l_args_2246 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2247 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2248 = U_LIT("args");
    t2243 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2244, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2245), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2246), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2247, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2248), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2243) {
    u_no_case(s2242);
  }
  return t2243;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2249;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2249 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2249 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2249;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2250 = v_d;
  u_value t2251 = NULL;
  if ((!t2251) && u_is_con(s2250, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2250, 0);
    u_value v_f = u_field(s2250, 1);
    u_value v_ps = u_field(s2250, 2);
    t2251 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2251) {
    t2251 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2251) {
    u_no_case(s2250);
  }
  return t2251;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2252 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2252;
    u_value t2253 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2253;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2254 = v_g;
    int d2255 = 0;
    if ((!d2255) && u_is_con(s2254, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2254, 1);
      d2255 = 1;
      {
        u_value t2256 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2256;
      }
    }
    if (!d2255) {
      u_no_case(s2254);
    }
    static struct u_object l_ulang_46_h_2257 = U_LIT("ulang.h");
    u_value t2258 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2259 = &t2258;
    u_value l2260 = v_fs;
    while (u_is_con(l2260, &n_list_46_Cons)) {
      u_value v_d = u_field(l2260, 0);
      if (u_truth(u_mk_bool(1))) {
        e2259 = u_append(e2259, f_compile_46_prototype_1(v_d));
      }
      l2260 = u_field(l2260, 1);
    }
    static struct u_object l_main_2261 = U_LIT("main");
    static struct u_object l_argc_2262 = U_LIT("argc");
    static struct u_object l_char_2263 = U_LIT("char");
    static struct u_object l_argv_2264 = U_LIT("argv");
    static struct u_object l_u_args_2265 = U_LIT("u_args");
    static struct u_object l_argc_2266 = U_LIT("argc");
    static struct u_object l_argv_2267 = U_LIT("argv");
    u_value t2268 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2257), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2258), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2261, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2262), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2263))), &l_argv_2264), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2265, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2266), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2267), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2268;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2269 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2269, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2270 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2270, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
    p_compile_46_compile_program(v_q, &v_f);
    p_os_write(v_target, f_c_46_c_file_1(v_f));
  }
}

u_value lam317(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_f = args[0];
  return f_ctx_46_is_fun_2(f_check_46_State_46_ctx_1(v_s), v_f);
}

u_value lam324(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam549(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_t = args[0];
  return f_ctx_46_is_sort_2(f_check_46_State_46_ctx_1(v_s), v_t);
}

u_value lam658(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam661(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_k = args[0];
  return f_ctx_46_is_constr_2(f_check_46_State_46_ctx_1(v_s), v_k);
}

u_value lam723(u_value *env, u_value *args) {
  u_value v_s = env[0];
  u_value v_q = args[0];
  return f_ctx_46_is_proc_2(f_check_46_State_46_ctx_1(v_s), v_q);
}

u_value lam1000(u_value *env, u_value *args) {
  u_value v_c = args[0];
  return f_prelude_46__33__61__2(v_c, u_mk_char(10));
}

int main(int argc, char **argv) {
  u_args(argc, argv);
  {
    u_value v_args = NULL;
    p_os_args(&v_args);
    u_value s2271 = v_args;
    int d2272 = 0;
    if ((!d2272) && u_is_con(s2271, &n_list_46_Nil)) {
      d2272 = 1;
      {
      }
    }
    if ((!d2272) && (u_is_con(s2271, &n_list_46_Cons) && (u_is_con(u_field(s2271, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2271, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2271, 0);
      u_value v_target = u_field(u_field(s2271, 1), 0);
      d2272 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2272) {
      d2272 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2273 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2273), "os.OsError");
      }
    }
    if (!d2272) {
      u_no_case(s2271);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
