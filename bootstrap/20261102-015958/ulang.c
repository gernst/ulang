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
  u_value s1016 = v_s;
  u_value t1017 = NULL;
  if ((!t1017) && u_is_con(s1016, &n_set_46_SLeaf)) {
    t1017 = u_mk_bool(0);
  }
  if ((!t1017) && u_is_con(s1016, &n_set_46_SNode)) {
    u_value v_l = u_field(s1016, 0);
    u_value v_y = u_field(s1016, 1);
    u_value v_r = u_field(s1016, 2);
    u_value t1021 = u_compare(v_x, v_y);
    u_value t1020;
    {
      u_value v_c = t1021;
      u_value t1019;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1019 = f_set_46_member_2(v_x, v_l);
      } else {
        u_value t1018;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1018 = f_set_46_member_2(v_x, v_r);
        } else {
          t1018 = u_mk_bool(1);
        }
        t1019 = t1018;
      }
      t1020 = t1019;
    }
    t1017 = t1020;
  }
  if (!t1017) {
    u_no_case(s1016);
  }
  return t1017;
}

u_value f_set_46_insert_2(u_value v_x, u_value v_s) {
  u_value s1022 = v_s;
  u_value t1023 = NULL;
  if ((!t1023) && u_is_con(s1022, &n_set_46_SLeaf)) {
    t1023 = u_mk_con(&n_set_46_SNode, 4, u_mk_con(&n_set_46_SLeaf, 0), v_x, u_mk_con(&n_set_46_SLeaf, 0), u_mk_int(1));
  }
  if ((!t1023) && u_is_con(s1022, &n_set_46_SNode)) {
    u_value v_l = u_field(s1022, 0);
    u_value v_y = u_field(s1022, 1);
    u_value v_r = u_field(s1022, 2);
    u_value t1027 = u_compare(v_x, v_y);
    u_value t1026;
    {
      u_value v_c = t1027;
      u_value t1025;
      if (u_truth(u_lt(v_c, u_mk_int(0)))) {
        t1025 = f_set_46_rebalance_3(f_set_46_insert_2(v_x, v_l), v_y, v_r);
      } else {
        u_value t1024;
        if (u_truth(f_prelude_46__62__2(v_c, u_mk_int(0)))) {
          t1024 = f_set_46_rebalance_3(v_l, v_y, f_set_46_insert_2(v_x, v_r));
        } else {
          t1024 = v_s;
        }
        t1025 = t1024;
      }
      t1026 = t1025;
    }
    t1023 = t1026;
  }
  if (!t1023) {
    u_no_case(s1022);
  }
  return t1023;
}

u_value f_set_46_from_list_1(u_value v_xs) {
  return f_set_46_insert_all_2(v_xs, u_mk_con(&n_set_46_SLeaf, 0));
}

u_value f_set_46_insert_all_2(u_value v_xs, u_value v_s) {
  u_value s1028 = v_xs;
  u_value t1029 = NULL;
  if ((!t1029) && u_is_con(s1028, &n_list_46_Nil)) {
    t1029 = v_s;
  }
  if ((!t1029) && u_is_con(s1028, &n_list_46_Cons)) {
    u_value v_x = u_field(s1028, 0);
    u_value v_rest = u_field(s1028, 1);
    t1029 = f_set_46_insert_all_2(v_rest, f_set_46_insert_2(v_x, v_s));
  }
  if (!t1029) {
    u_no_case(s1028);
  }
  return t1029;
}

u_value f_set_46_elements_1(u_value v_s) {
  return f_set_46_elements_onto_2(v_s, u_mk_con(&n_list_46_Nil, 0));
}

u_value f_set_46_elements_onto_2(u_value v_s, u_value v_rest) {
  u_value s1030 = v_s;
  u_value t1031 = NULL;
  if ((!t1031) && u_is_con(s1030, &n_set_46_SLeaf)) {
    t1031 = v_rest;
  }
  if ((!t1031) && u_is_con(s1030, &n_set_46_SNode)) {
    u_value v_l = u_field(s1030, 0);
    u_value v_x = u_field(s1030, 1);
    u_value v_r = u_field(s1030, 2);
    t1031 = f_set_46_elements_onto_2(v_l, u_mk_con(&n_list_46_Cons, 2, v_x, f_set_46_elements_onto_2(v_r, v_rest)));
  }
  if (!t1031) {
    u_no_case(s1030);
  }
  return t1031;
}

u_value f_set_46_count_1(u_value v_s) {
  u_value s1032 = v_s;
  u_value t1033 = NULL;
  if ((!t1033) && u_is_con(s1032, &n_set_46_SLeaf)) {
    t1033 = u_mk_int(0);
  }
  if ((!t1033) && u_is_con(s1032, &n_set_46_SNode)) {
    u_value v_l = u_field(s1032, 0);
    u_value v_r = u_field(s1032, 2);
    t1033 = u_add(u_add(f_set_46_count_1(v_l), u_mk_int(1)), f_set_46_count_1(v_r));
  }
  if (!t1033) {
    u_no_case(s1032);
  }
  return t1033;
}

u_value f_set_46_depth_1(u_value v_s) {
  u_value s1034 = v_s;
  u_value t1035 = NULL;
  if ((!t1035) && u_is_con(s1034, &n_set_46_SLeaf)) {
    t1035 = u_mk_int(0);
  }
  if ((!t1035) && u_is_con(s1034, &n_set_46_SNode)) {
    u_value v_d = u_field(s1034, 3);
    t1035 = v_d;
  }
  if (!t1035) {
    u_no_case(s1034);
  }
  return t1035;
}

u_value f_set_46_snode_depth_2(u_value v_l, u_value v_r) {
  u_value t1036;
  if (u_truth(u_lt(f_set_46_depth_1(v_l), f_set_46_depth_1(v_r)))) {
    t1036 = f_set_46_depth_1(v_r);
  } else {
    t1036 = f_set_46_depth_1(v_l);
  }
  return u_add(u_mk_int(1), t1036);
}

u_value f_set_46_snode_3(u_value v_l, u_value v_x, u_value v_r) {
  return u_mk_con(&n_set_46_SNode, 4, v_l, v_x, v_r, f_set_46_snode_depth_2(v_l, v_r));
}

u_value f_set_46_turn_left_1(u_value v_s) {
  u_value s1037 = v_s;
  u_value t1038 = NULL;
  if ((!t1038) && (u_is_con(s1037, &n_set_46_SNode) && u_is_con(u_field(s1037, 2), &n_set_46_SNode))) {
    u_value v_a = u_field(s1037, 0);
    u_value v_x = u_field(s1037, 1);
    u_value v_b = u_field(u_field(s1037, 2), 0);
    u_value v_y = u_field(u_field(s1037, 2), 1);
    u_value v_c = u_field(u_field(s1037, 2), 2);
    t1038 = f_set_46_snode_3(f_set_46_snode_3(v_a, v_x, v_b), v_y, v_c);
  }
  if (!t1038) {
    t1038 = v_s;
  }
  if (!t1038) {
    u_no_case(s1037);
  }
  return t1038;
}

u_value f_set_46_turn_right_1(u_value v_s) {
  u_value s1039 = v_s;
  u_value t1040 = NULL;
  if ((!t1040) && (u_is_con(s1039, &n_set_46_SNode) && u_is_con(u_field(s1039, 0), &n_set_46_SNode))) {
    u_value v_a = u_field(u_field(s1039, 0), 0);
    u_value v_x = u_field(u_field(s1039, 0), 1);
    u_value v_b = u_field(u_field(s1039, 0), 2);
    u_value v_y = u_field(s1039, 1);
    u_value v_c = u_field(s1039, 2);
    t1040 = f_set_46_snode_3(v_a, v_x, f_set_46_snode_3(v_b, v_y, v_c));
  }
  if (!t1040) {
    t1040 = v_s;
  }
  if (!t1040) {
    u_no_case(s1039);
  }
  return t1040;
}

u_value f_set_46_smaller_1(u_value v_s) {
  u_value s1041 = v_s;
  u_value t1042 = NULL;
  if ((!t1042) && u_is_con(s1041, &n_set_46_SNode)) {
    u_value v_l = u_field(s1041, 0);
    t1042 = v_l;
  }
  if (!t1042) {
    t1042 = v_s;
  }
  if (!t1042) {
    u_no_case(s1041);
  }
  return t1042;
}

u_value f_set_46_greater_1(u_value v_s) {
  u_value s1043 = v_s;
  u_value t1044 = NULL;
  if ((!t1044) && u_is_con(s1043, &n_set_46_SNode)) {
    u_value v_r = u_field(s1043, 2);
    t1044 = v_r;
  }
  if (!t1044) {
    t1044 = v_s;
  }
  if (!t1044) {
    u_no_case(s1043);
  }
  return t1044;
}

u_value f_set_46_rebalance_3(u_value v_l, u_value v_x, u_value v_r) {
  u_value t1048;
  if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1))))) {
    u_value t1045;
    if (u_truth(u_lt(f_set_46_depth_1(f_set_46_smaller_1(v_l)), f_set_46_depth_1(f_set_46_greater_1(v_l))))) {
      t1045 = f_set_46_turn_left_1(v_l);
    } else {
      t1045 = v_l;
    }
    t1048 = f_set_46_turn_right_1(f_set_46_snode_3(t1045, v_x, v_r));
  } else {
    u_value t1047;
    if (u_truth(f_prelude_46__62__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1))))) {
      u_value t1046;
      if (u_truth(u_lt(f_set_46_depth_1(f_set_46_greater_1(v_r)), f_set_46_depth_1(f_set_46_smaller_1(v_r))))) {
        t1046 = f_set_46_turn_right_1(v_r);
      } else {
        t1046 = v_r;
      }
      t1047 = f_set_46_turn_left_1(f_set_46_snode_3(v_l, v_x, t1046));
    } else {
      t1047 = f_set_46_snode_3(v_l, v_x, v_r);
    }
    t1048 = t1047;
  }
  return t1048;
}

u_value f_set_46_well_formed_1(u_value v_s) {
  u_value s1049 = v_s;
  u_value t1050 = NULL;
  if ((!t1050) && u_is_con(s1049, &n_set_46_SLeaf)) {
    t1050 = u_mk_bool(1);
  }
  if ((!t1050) && u_is_con(s1049, &n_set_46_SNode)) {
    u_value v_l = u_field(s1049, 0);
    u_value v_r = u_field(s1049, 2);
    u_value v_d = u_field(s1049, 3);
    u_value t1051 = f_set_46_well_formed_1(v_l);
    if (u_truth(t1051)) {
      t1051 = f_set_46_well_formed_1(v_r);
    }
    u_value t1052 = t1051;
    if (u_truth(t1052)) {
      t1052 = u_eq(v_d, f_set_46_snode_depth_2(v_l, v_r));
    }
    u_value t1053 = t1052;
    if (u_truth(t1053)) {
      t1053 = f_prelude_46__60__61__2(f_set_46_depth_1(v_l), u_add(f_set_46_depth_1(v_r), u_mk_int(1)));
    }
    u_value t1054 = t1053;
    if (u_truth(t1054)) {
      t1054 = f_prelude_46__60__61__2(f_set_46_depth_1(v_r), u_add(f_set_46_depth_1(v_l), u_mk_int(1)));
    }
    t1050 = t1054;
  }
  if (!t1050) {
    u_no_case(s1049);
  }
  return t1050;
}

u_value f_set_46_upto_1(u_value v_n) {
  u_value t1055;
  if (u_truth(u_eq(v_n, u_mk_int(0)))) {
    t1055 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1055 = f_list_46__43__43__2(f_set_46_upto_1(u_sub(v_n, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0)));
  }
  return t1055;
}

u_value f_parse_46_Notations_46_cmds_1(u_value v_x) {
  u_value s1056 = v_x;
  u_value t1057 = NULL;
  if ((!t1057) && u_is_con(s1056, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1056, 0);
    t1057 = v_y;
  }
  if (!t1057) {
    u_no_case(s1056);
  }
  return t1057;
}

u_value f_parse_46_Notations_46_ops_1(u_value v_x) {
  u_value s1058 = v_x;
  u_value t1059 = NULL;
  if ((!t1059) && u_is_con(s1058, &n_parse_46_Notations)) {
    u_value v_y = u_field(s1058, 1);
    t1059 = v_y;
  }
  if (!t1059) {
    u_no_case(s1058);
  }
  return t1059;
}

u_value f_parse_46_Scope_46_params_1(u_value v_x) {
  u_value s1060 = v_x;
  u_value t1061 = NULL;
  if ((!t1061) && u_is_con(s1060, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1060, 0);
    t1061 = v_y;
  }
  if (!t1061) {
    u_no_case(s1060);
  }
  return t1061;
}

u_value f_parse_46_Scope_46_notations_1(u_value v_x) {
  u_value s1062 = v_x;
  u_value t1063 = NULL;
  if ((!t1063) && u_is_con(s1062, &n_parse_46_Scope)) {
    u_value v_y = u_field(s1062, 1);
    t1063 = v_y;
  }
  if (!t1063) {
    u_no_case(s1062);
  }
  return t1063;
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
  static struct u_object l___1064 = U_LIT("_");
  static struct u_object l_assert_1065 = U_LIT("assert");
  static struct u_object l_assign_1066 = U_LIT("assign");
  static struct u_object l_call_1067 = U_LIT("call");
  static struct u_object l_case_1068 = U_LIT("case");
  static struct u_object l_catch_1069 = U_LIT("catch");
  static struct u_object l_data_1070 = U_LIT("data");
  static struct u_object l_else_1071 = U_LIT("else");
  static struct u_object l_end_1072 = U_LIT("end");
  static struct u_object l_eval_1073 = U_LIT("eval");
  static struct u_object l_false_1074 = U_LIT("false");
  static struct u_object l_for_1075 = U_LIT("for");
  static struct u_object l_function_1076 = U_LIT("function");
  static struct u_object l_if_1077 = U_LIT("if");
  static struct u_object l_import_1078 = U_LIT("import");
  static struct u_object l_in_1079 = U_LIT("in");
  static struct u_object l_infix_1080 = U_LIT("infix");
  static struct u_object l_lambda_1081 = U_LIT("lambda");
  static struct u_object l_let_1082 = U_LIT("let");
  static struct u_object l_local_1083 = U_LIT("local");
  static struct u_object l_match_1084 = U_LIT("match");
  static struct u_object l_out_1085 = U_LIT("out");
  static struct u_object l_postfix_1086 = U_LIT("postfix");
  static struct u_object l_prefix_1087 = U_LIT("prefix");
  static struct u_object l_procedure_1088 = U_LIT("procedure");
  static struct u_object l_raise_1089 = U_LIT("raise");
  static struct u_object l_ref_1090 = U_LIT("ref");
  static struct u_object l_run_1091 = U_LIT("run");
  static struct u_object l_skip_1092 = U_LIT("skip");
  static struct u_object l_test_1093 = U_LIT("test");
  static struct u_object l_then_1094 = U_LIT("then");
  static struct u_object l_true_1095 = U_LIT("true");
  static struct u_object l_type_1096 = U_LIT("type");
  static struct u_object l_try_1097 = U_LIT("try");
  static struct u_object l_while_1098 = U_LIT("while");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, &l___1064, u_mk_con(&n_list_46_Cons, 2, &l_assert_1065, u_mk_con(&n_list_46_Cons, 2, &l_assign_1066, u_mk_con(&n_list_46_Cons, 2, &l_call_1067, u_mk_con(&n_list_46_Cons, 2, &l_case_1068, u_mk_con(&n_list_46_Cons, 2, &l_catch_1069, u_mk_con(&n_list_46_Cons, 2, &l_data_1070, u_mk_con(&n_list_46_Cons, 2, &l_else_1071, u_mk_con(&n_list_46_Cons, 2, &l_end_1072, u_mk_con(&n_list_46_Cons, 2, &l_eval_1073, u_mk_con(&n_list_46_Cons, 2, &l_false_1074, u_mk_con(&n_list_46_Cons, 2, &l_for_1075, u_mk_con(&n_list_46_Cons, 2, &l_function_1076, u_mk_con(&n_list_46_Cons, 2, &l_if_1077, u_mk_con(&n_list_46_Cons, 2, &l_import_1078, u_mk_con(&n_list_46_Cons, 2, &l_in_1079, u_mk_con(&n_list_46_Cons, 2, &l_infix_1080, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1081, u_mk_con(&n_list_46_Cons, 2, &l_let_1082, u_mk_con(&n_list_46_Cons, 2, &l_local_1083, u_mk_con(&n_list_46_Cons, 2, &l_match_1084, u_mk_con(&n_list_46_Cons, 2, &l_out_1085, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1086, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1087, u_mk_con(&n_list_46_Cons, 2, &l_procedure_1088, u_mk_con(&n_list_46_Cons, 2, &l_raise_1089, u_mk_con(&n_list_46_Cons, 2, &l_ref_1090, u_mk_con(&n_list_46_Cons, 2, &l_run_1091, u_mk_con(&n_list_46_Cons, 2, &l_skip_1092, u_mk_con(&n_list_46_Cons, 2, &l_test_1093, u_mk_con(&n_list_46_Cons, 2, &l_then_1094, u_mk_con(&n_list_46_Cons, 2, &l_true_1095, u_mk_con(&n_list_46_Cons, 2, &l_type_1096, u_mk_con(&n_list_46_Cons, 2, &l_try_1097, u_mk_con(&n_list_46_Cons, 2, &l_while_1098, u_mk_con(&n_list_46_Nil, 0))))))))))))))))))))))))))))))))))));
  return cache;
}

u_value f_parse_46_is_keyword_1(u_value v_n) {
  return f_list_46_contains_2(v_n, f_parse_46_keywords_0());
}

u_value f_parse_46_is_reserved_1(u_value v_s) {
  static struct u_object l__61__1099 = U_LIT("=");
  static struct u_object l__61__62__1100 = U_LIT("=>");
  static struct u_object l__58__61__1101 = U_LIT(":=");
  static struct u_object l__58__1102 = U_LIT(":");
  static struct u_object l__124__1103 = U_LIT("|");
  return f_list_46_contains_2(v_s, u_mk_con(&n_list_46_Cons, 2, &l__61__1099, u_mk_con(&n_list_46_Cons, 2, &l__61__62__1100, u_mk_con(&n_list_46_Cons, 2, &l__58__61__1101, u_mk_con(&n_list_46_Cons, 2, &l__58__1102, u_mk_con(&n_list_46_Cons, 2, &l__124__1103, u_mk_con(&n_list_46_Nil, 0)))))));
}

u_value f_parse_46_prefix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1104 = v_ns;
  u_value t1105 = NULL;
  if ((!t1105) && u_is_con(s1104, &n_list_46_Nil)) {
    t1105 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1105) && (u_is_con(s1104, &n_list_46_Cons) && (u_is_con(u_field(s1104, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1104, 0), 0), &n_syntax_46_FPrefix)))) {
    u_value v_p = u_field(u_field(s1104, 0), 1);
    u_value v_ops = u_field(u_field(s1104, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1105 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1105) && u_is_con(s1104, &n_list_46_Cons)) {
    u_value v_more = u_field(s1104, 1);
    t1105 = f_parse_46_prefix_prec_2(v_more, v_op);
  }
  if (!t1105) {
    u_no_case(s1104);
  }
  return t1105;
}

u_value f_parse_46_postfix_prec_2(u_value v_ns, u_value v_op) {
  u_value s1106 = v_ns;
  u_value t1107 = NULL;
  if ((!t1107) && u_is_con(s1106, &n_list_46_Nil)) {
    t1107 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1107) && (u_is_con(s1106, &n_list_46_Cons) && (u_is_con(u_field(s1106, 0), &n_syntax_46_CNotation) && u_is_con(u_field(u_field(s1106, 0), 0), &n_syntax_46_FPostfix)))) {
    u_value v_p = u_field(u_field(s1106, 0), 1);
    u_value v_ops = u_field(u_field(s1106, 0), 2);
    if (u_truth(f_list_46_contains_2(v_op, v_ops))) {
      t1107 = u_mk_con(&n_option_46_Some, 1, v_p);
    }
  }
  if ((!t1107) && u_is_con(s1106, &n_list_46_Cons)) {
    u_value v_more = u_field(s1106, 1);
    t1107 = f_parse_46_postfix_prec_2(v_more, v_op);
  }
  if (!t1107) {
    u_no_case(s1106);
  }
  return t1107;
}

u_value f_parse_46_infix_of_2(u_value v_ns, u_value v_op) {
  u_value s1108 = v_ns;
  u_value t1109 = NULL;
  if ((!t1109) && u_is_con(s1108, &n_list_46_Nil)) {
    t1109 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1109) && (u_is_con(s1108, &n_list_46_Cons) && u_is_con(u_field(s1108, 0), &n_syntax_46_CNotation))) {
    u_value v_f = u_field(u_field(s1108, 0), 0);
    u_value v_p = u_field(u_field(s1108, 0), 1);
    u_value v_ops = u_field(u_field(s1108, 0), 2);
    u_value t1110 = f_list_46_contains_2(v_op, v_ops);
    if (u_truth(t1110)) {
      t1110 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPrefix, 0));
    }
    u_value t1111 = t1110;
    if (u_truth(t1111)) {
      t1111 = f_prelude_46__33__61__2(v_f, u_mk_con(&n_syntax_46_FPostfix, 0));
    }
    if (u_truth(t1111)) {
      t1109 = u_mk_con(&n_option_46_Some, 1, u_mk_tuple(2, v_f, v_p));
    }
  }
  if ((!t1109) && u_is_con(s1108, &n_list_46_Cons)) {
    u_value v_more = u_field(s1108, 1);
    t1109 = f_parse_46_infix_of_2(v_more, v_op);
  }
  if (!t1109) {
    u_no_case(s1108);
  }
  return t1109;
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
  u_value s1112 = v_cs;
  u_value t1113 = NULL;
  if ((!t1113) && u_is_con(s1112, &n_list_46_Nil)) {
    t1113 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1113) && (u_is_con(s1112, &n_list_46_Cons) && u_is_con(u_field(s1112, 0), &n_syntax_46_CNotation))) {
    u_value v_ops = u_field(u_field(s1112, 0), 2);
    u_value v_more = u_field(s1112, 1);
    t1113 = f_list_46__43__43__2(v_ops, f_parse_46_operators_of_1(v_more));
  }
  if ((!t1113) && u_is_con(s1112, &n_list_46_Cons)) {
    u_value v_more = u_field(s1112, 1);
    t1113 = f_parse_46_operators_of_1(v_more);
  }
  if (!t1113) {
    u_no_case(s1112);
  }
  return t1113;
}

u_value f_parse_46_head_token_1(u_value v_ts) {
  u_value s1114 = v_ts;
  u_value t1115 = NULL;
  if ((!t1115) && u_is_con(s1114, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(s1114, 0), 0);
    t1115 = v_t;
  }
  if (!t1115) {
    t1115 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1115) {
    u_no_case(s1114);
  }
  return t1115;
}

u_value f_parse_46_second_token_1(u_value v_ts) {
  u_value s1116 = v_ts;
  u_value t1117 = NULL;
  if ((!t1117) && (u_is_con(s1116, &n_list_46_Cons) && u_is_con(u_field(s1116, 1), &n_list_46_Cons))) {
    u_value v_t = u_field(u_field(u_field(s1116, 1), 0), 0);
    t1117 = v_t;
  }
  if (!t1117) {
    t1117 = u_mk_con(&n_lex_46_TEnd, 0);
  }
  if (!t1117) {
    u_no_case(s1116);
  }
  return t1117;
}

u_value f_parse_46_head_offset_1(u_value v_ts) {
  u_value s1118 = v_ts;
  u_value t1119 = NULL;
  if ((!t1119) && u_is_con(s1118, &n_list_46_Cons)) {
    u_value v_i = u_field(u_field(s1118, 0), 1);
    t1119 = v_i;
  }
  if (!t1119) {
    t1119 = u_mk_int(0);
  }
  if (!t1119) {
    u_no_case(s1118);
  }
  return t1119;
}

u_value f_parse_46_at_keyword_2(u_value v_ts, u_value v_w) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
}

u_value f_parse_46_at_punct_2(u_value v_ts, u_value v_c) {
  return u_eq(f_parse_46_head_token_1(v_ts), u_mk_con(&n_lex_46_TPunct, 1, v_c));
}

u_value f_parse_46_describe_1(u_value v_t) {
  u_value s1120 = v_t;
  u_value t1121 = NULL;
  if ((!t1121) && u_is_con(s1120, &n_lex_46_TName)) {
    u_value v_n = u_field(s1120, 0);
    t1121 = v_n;
  }
  if ((!t1121) && u_is_con(s1120, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1120, 0);
    t1121 = v_s;
  }
  if ((!t1121) && u_is_con(s1120, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1120, 0);
    t1121 = u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1121) && u_is_con(s1120, &n_lex_46_TNum)) {
    u_value v_n = u_field(s1120, 0);
    t1121 = u_str_show(v_n);
  }
  if ((!t1121) && u_is_con(s1120, &n_lex_46_TStr)) {
    u_value v_s = u_field(s1120, 0);
    t1121 = u_str_quote(v_s);
  }
  if ((!t1121) && u_is_con(s1120, &n_lex_46_TChar)) {
    u_value v_c = u_field(s1120, 0);
    static struct u_object l__39__1122 = U_LIT("'");
    static struct u_object l__39__1123 = U_LIT("'");
    t1121 = u_concat(&l__39__1122, u_concat(u_string(u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0))), &l__39__1123));
  }
  if ((!t1121) && u_is_con(s1120, &n_lex_46_TEnd)) {
    static struct u_object l_end_32_of_32_input_1124 = U_LIT("end of input");
    t1121 = &l_end_32_of_32_input_1124;
  }
  if (!t1121) {
    u_no_case(s1120);
  }
  return t1121;
}

u_value f_parse_46_operator_at_2(u_value v_ns, u_value v_ts) {
  u_value s1125 = f_parse_46_head_token_1(v_ts);
  u_value t1126 = NULL;
  if ((!t1126) && u_is_con(s1125, &n_lex_46_TSym)) {
    u_value v_s = u_field(s1125, 0);
    if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
      t1126 = u_mk_con(&n_option_46_Some, 1, v_s);
    }
  }
  if ((!t1126) && u_is_con(s1125, &n_lex_46_TName)) {
    u_value v_n = u_field(s1125, 0);
    if (u_truth(f_parse_46_declared_2(v_ns, v_n))) {
      t1126 = u_mk_con(&n_option_46_Some, 1, v_n);
    }
  }
  if (!t1126) {
    t1126 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1126) {
    u_no_case(s1125);
  }
  return t1126;
}

void p_parse_46_advance(u_value *r_ts) {
  {
    u_value s1127 = *r_ts;
    int d1128 = 0;
    if ((!d1128) && u_is_con(s1127, &n_list_46_Cons)) {
      u_value v_more = u_field(s1127, 1);
      d1128 = 1;
      {
        u_value t1129 = v_more;
        *r_ts = t1129;
      }
    }
    if (!d1128) {
      d1128 = 1;
      {
      }
    }
    if (!d1128) {
      u_no_case(s1127);
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
        static struct u_object l__32_expected_32_but_32__1130 = U_LIT(" expected but ");
        static struct u_object l__32_found_1131 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(f_parse_46_describe_1(v_t), u_concat(&l__32_expected_32_but_32__1130, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1131))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
    u_value s1132 = f_parse_46_head_token_1(*r_ts);
    int d1133 = 0;
    if ((!d1133) && u_is_con(s1132, &n_lex_46_TName)) {
      u_value v_m = u_field(s1132, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_m)))) {
        d1133 = 1;
        {
          u_value t1134 = v_m;
          *r_n = t1134;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1133) {
      d1133 = 1;
      {
        static struct u_object l_name_32_expected_32_bu_1135 = U_LIT("name expected but ");
        static struct u_object l__32_found_1136 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_name_32_expected_32_bu_1135, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1136)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1133) {
      u_no_case(s1132);
    }
  }
}

void p_parse_46_parse_plain(u_value v_sc, u_value *r_n, u_value *r_ts) {
  {
    u_value s1137 = f_parse_46_head_token_1(*r_ts);
    int d1138 = 0;
    if ((!d1138) && u_is_con(s1137, &n_lex_46_TName)) {
      u_value v_m = u_field(s1137, 0);
      if (u_truth(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_m))) {
        d1138 = 1;
        {
          static struct u_object l_operator_32__1139 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_a_32_name_1140 = U_LIT(" is not a name");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1139, u_concat(v_m, &l__32_is_32_not_32_a_32_name_1140)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
        }
      }
    }
    if (!d1138) {
      d1138 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1138) {
      u_no_case(s1137);
    }
  }
}

void p_parse_46_parse_name_or_symbol(u_value *r_n, u_value *r_ts) {
  {
    u_value s1141 = f_parse_46_head_token_1(*r_ts);
    int d1142 = 0;
    if ((!d1142) && u_is_con(s1141, &n_lex_46_TSym)) {
      u_value v_s = u_field(s1141, 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
        d1142 = 1;
        {
          u_value t1143 = v_s;
          *r_n = t1143;
          p_parse_46_advance(r_ts);
        }
      }
    }
    if (!d1142) {
      d1142 = 1;
      {
        p_parse_46_parse_name(r_n, r_ts);
      }
    }
    if (!d1142) {
      u_no_case(s1141);
    }
  }
}

u_value f_parse_46_Ops_46_notations_1(u_value v_x) {
  u_value s1144 = v_x;
  u_value t1145 = NULL;
  if ((!t1145) && u_is_con(s1144, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1144, 0);
    t1145 = v_y;
  }
  if (!t1145) {
    u_no_case(s1144);
  }
  return t1145;
}

u_value f_parse_46_Ops_46_prefix_op_1(u_value v_x) {
  u_value s1146 = v_x;
  u_value t1147 = NULL;
  if ((!t1147) && u_is_con(s1146, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1146, 1);
    t1147 = v_y;
  }
  if (!t1147) {
    u_no_case(s1146);
  }
  return t1147;
}

u_value f_parse_46_Ops_46_postfix_op_1(u_value v_x) {
  u_value s1148 = v_x;
  u_value t1149 = NULL;
  if ((!t1149) && u_is_con(s1148, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1148, 2);
    t1149 = v_y;
  }
  if (!t1149) {
    u_no_case(s1148);
  }
  return t1149;
}

u_value f_parse_46_Ops_46_infix_op_1(u_value v_x) {
  u_value s1150 = v_x;
  u_value t1151 = NULL;
  if ((!t1151) && u_is_con(s1150, &n_parse_46_Ops)) {
    u_value v_y = u_field(s1150, 3);
    t1151 = v_y;
  }
  if (!t1151) {
    u_no_case(s1150);
  }
  return t1151;
}

void p_parse_46_resolve_items(u_value v_items, u_value v_o, u_value v_at, u_value *r_a) {
  {
    u_value v_rest = NULL;
    p_parse_46_climb(v_items, u_mk_int(0), v_o, v_at, r_a, &v_rest);
    u_value s1152 = v_rest;
    int d1153 = 0;
    if ((!d1153) && u_is_con(s1152, &n_list_46_Nil)) {
      d1153 = 1;
      {
      }
    }
    if ((!d1153) && (u_is_con(s1152, &n_list_46_Cons) && u_is_con(u_field(s1152, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1152, 0), 0);
      u_value v_i = u_field(u_field(s1152, 0), 1);
      d1153 = 1;
      {
        static struct u_object l_unexpected_32_opera_1154 = U_LIT("unexpected operator ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unexpected_32_opera_1154, v_op), v_i), "parse.ParseError");
      }
    }
    if (!d1153) {
      d1153 = 1;
      {
        static struct u_object l_missing_32_operator_1155 = U_LIT("missing operator");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operator_1155, v_at), "parse.ParseError");
      }
    }
    if (!d1153) {
      u_no_case(s1152);
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
    u_value s1156 = v_items;
    int d1157 = 0;
    if ((!d1157) && (u_is_con(s1156, &n_list_46_Cons) && u_is_con(u_field(s1156, 0), &n_parse_46_Operand))) {
      u_value v_x = u_field(u_field(s1156, 0), 0);
      u_value v_more = u_field(s1156, 1);
      d1157 = 1;
      {
        u_value t1158 = v_x;
        u_value t1159 = v_more;
        *r_a = t1158;
        *r_rest = t1159;
      }
    }
    if ((!d1157) && (u_is_con(s1156, &n_list_46_Cons) && u_is_con(u_field(s1156, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1156, 0), 0);
      u_value v_i = u_field(u_field(s1156, 0), 1);
      u_value v_more = u_field(s1156, 1);
      d1157 = 1;
      {
        u_value s1160 = f_parse_46_prefix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op);
        int d1161 = 0;
        if ((!d1161) && u_is_con(s1160, &n_option_46_Some)) {
          u_value v_p = u_field(s1160, 0);
          d1161 = 1;
          {
            p_parse_46_climb(v_more, v_p, v_o, v_at, &v_b, r_rest);
            u_value t1162 = u_apply(f_parse_46_Ops_46_prefix_op_1(v_o), 2, v_op, v_b);
            *r_a = t1162;
          }
        }
        if ((!d1161) && u_is_con(s1160, &n_option_46_None)) {
          if (u_truth(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op))) {
            d1161 = 1;
            {
              static struct u_object l__32_is_32_not_32_a_32_prefix_1163 = U_LIT(" is not a prefix operator");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_op, &l__32_is_32_not_32_a_32_prefix_1163), v_i), "parse.ParseError");
            }
          }
        }
        if ((!d1161) && u_is_con(s1160, &n_option_46_None)) {
          d1161 = 1;
          {
            static struct u_object l_undeclared_32_opera_1164 = U_LIT("undeclared operator ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1164, v_op), v_i), "parse.ParseError");
          }
        }
        if (!d1161) {
          u_no_case(s1160);
        }
      }
    }
    if ((!d1157) && u_is_con(s1156, &n_list_46_Nil)) {
      d1157 = 1;
      {
        static struct u_object l_missing_32_operand_1165 = U_LIT("missing operand");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_missing_32_operand_1165, v_at), "parse.ParseError");
      }
    }
    if (!d1157) {
      u_no_case(s1156);
    }
  }
}

void p_parse_46_climb_loop(u_value v_lhs, u_value v_items, u_value v_min, u_value v_o, u_value v_at, u_value *r_a, u_value *r_rest) {
  {
    u_value v_rhs = NULL;
    u_value v_more = NULL;
    u_value s1166 = v_items;
    int d1167 = 0;
    if ((!d1167) && (u_is_con(s1166, &n_list_46_Cons) && u_is_con(u_field(s1166, 0), &n_parse_46_Operator))) {
      u_value v_op = u_field(u_field(s1166, 0), 0);
      u_value v_i = u_field(u_field(s1166, 0), 1);
      u_value v_tail = u_field(s1166, 1);
      d1167 = 1;
      {
        u_value s1168 = u_mk_tuple(2, f_parse_46_postfix_prec_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op), f_parse_46_infix_of_2(f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)), v_op));
        int d1169 = 0;
        if ((!d1169) && u_is_con(u_field(s1168, 0), &n_option_46_Some)) {
          u_value v_p = u_field(u_field(s1168, 0), 0);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1169 = 1;
            {
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_postfix_op_1(v_o), 2, v_op, v_lhs), v_tail, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if ((!d1169) && u_is_con(u_field(s1168, 1), &n_option_46_Some)) {
          u_value v_f = u_field(u_field(u_field(s1168, 1), 0), 0);
          u_value v_p = u_field(u_field(u_field(s1168, 1), 0), 1);
          if (u_truth(f_prelude_46__62__61__2(v_p, v_min))) {
            d1169 = 1;
            {
              u_value t1170;
              if (u_truth(u_eq(v_f, u_mk_con(&n_syntax_46_FRight, 0)))) {
                t1170 = v_p;
              } else {
                t1170 = u_add(v_p, u_mk_int(1));
              }
              p_parse_46_climb(v_tail, t1170, v_o, v_at, &v_rhs, &v_more);
              p_parse_46_nonassociative(v_op, v_f, v_p, v_more, f_parse_46_Notations_46_cmds_1(f_parse_46_Ops_46_notations_1(v_o)));
              p_parse_46_climb_loop(u_apply(f_parse_46_Ops_46_infix_op_1(v_o), 3, v_op, v_lhs, v_rhs), v_more, v_min, v_o, v_at, r_a, r_rest);
            }
          }
        }
        if (!d1169) {
          if (u_truth(f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Ops_46_notations_1(v_o), v_op)))) {
            d1169 = 1;
            {
              static struct u_object l_undeclared_32_opera_1171 = U_LIT("undeclared operator ");
              u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_undeclared_32_opera_1171, v_op), v_i), "parse.ParseError");
            }
          }
        }
        if (!d1169) {
          d1169 = 1;
          {
            u_value t1172 = v_lhs;
            u_value t1173 = v_items;
            *r_a = t1172;
            *r_rest = t1173;
          }
        }
        if (!d1169) {
          u_no_case(s1168);
        }
      }
    }
    if (!d1167) {
      d1167 = 1;
      {
        u_value t1174 = v_lhs;
        u_value t1175 = v_items;
        *r_a = t1174;
        *r_rest = t1175;
      }
    }
    if (!d1167) {
      u_no_case(s1166);
    }
  }
}

void p_parse_46_nonassociative(u_value v_op, u_value v_f, u_value v_p, u_value v_rest, u_value v_ns) {
  {
    u_value s1176 = v_rest;
    int d1177 = 0;
    if ((!d1177) && (u_is_con(s1176, &n_list_46_Cons) && u_is_con(u_field(s1176, 0), &n_parse_46_Operator))) {
      u_value v_next = u_field(u_field(s1176, 0), 0);
      u_value v_i = u_field(u_field(s1176, 0), 1);
      u_value t1180 = u_eq(v_f, u_mk_con(&n_syntax_46_FInfix, 0));
      if (u_truth(t1180)) {
        t1180 = f_parse_46_is_prec_2(f_parse_46_infix_of_2(v_ns, v_next), v_p);
      }
      if (u_truth(t1180)) {
        d1177 = 1;
        {
          static struct u_object l_operator_32__1178 = U_LIT("operator ");
          static struct u_object l__32_is_32_not_32_associat_1179 = U_LIT(" is not associative");
          u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32__1178, u_concat(v_op, &l__32_is_32_not_32_associat_1179)), v_i), "parse.ParseError");
        }
      }
    }
    if (!d1177) {
      d1177 = 1;
      {
      }
    }
    if (!d1177) {
      u_no_case(s1176);
    }
  }
}

u_value f_parse_46_is_prec_2(u_value v_fp, u_value v_p) {
  u_value s1181 = v_fp;
  u_value t1182 = NULL;
  if ((!t1182) && u_is_con(s1181, &n_option_46_Some)) {
    u_value v_q = u_field(u_field(s1181, 0), 1);
    t1182 = u_eq(v_q, v_p);
  }
  if (!t1182) {
    t1182 = u_mk_bool(0);
  }
  if (!t1182) {
    u_no_case(s1181);
  }
  return t1182;
}

u_value f_parse_46_type_one_1(u_value v_ts) {
  u_value s1183 = v_ts;
  u_value t1184 = NULL;
  if ((!t1184) && (u_is_con(s1183, &n_list_46_Cons) && (u_is_con(u_field(s1183, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1183, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1183, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1183, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1183, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1183, 0), 1);
    t1184 = u_mk_con(&n_syntax_46_TTuple, 1, v_cs);
  }
  if ((!t1184) && (u_is_con(s1183, &n_list_46_Cons) && u_is_con(u_field(s1183, 1), &n_list_46_Nil))) {
    u_value v_t = u_field(s1183, 0);
    t1184 = v_t;
  }
  if ((!t1184) && u_is_con(s1183, &n_list_46_Nil)) {
    t1184 = u_mk_con(&n_syntax_46_TTuple, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1184) {
    static struct u_object l__40_invalid_41__1185 = U_LIT("(invalid)");
    t1184 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_invalid_41__1185), v_ts);
  }
  if (!t1184) {
    u_no_case(s1183);
  }
  return t1184;
}

u_value f_parse_46_type_components_1(u_value v_ts) {
  u_value s1186 = v_ts;
  u_value t1187 = NULL;
  if ((!t1187) && (u_is_con(s1186, &n_list_46_Cons) && (u_is_con(u_field(s1186, 0), &n_syntax_46_TSort) && (u_is_con(u_field(u_field(s1186, 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(s1186, 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(s1186, 0), 0), 1), u_mk_lit("(product)")) && u_is_con(u_field(s1186, 1), &n_list_46_Nil))))))) {
    u_value v_cs = u_field(u_field(s1186, 0), 1);
    t1187 = v_cs;
  }
  if (!t1187) {
    t1187 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_ts), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1187) {
    u_no_case(s1186);
  }
  return t1187;
}

u_value f_parse_46_type_prefix_2(u_value v_op, u_value v_t) {
  return u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_t), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
}

u_value f_parse_46_type_infix_3(u_value v_op, u_value v_l, u_value v_r) {
  static struct u_object l__45__62__1188 = U_LIT("->");
  u_value t1195;
  if (u_truth(u_eq(v_op, &l__45__62__1188))) {
    u_value t1189 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1190 = &t1189;
    u_value l1191 = v_l;
    while (u_is_con(l1191, &n_list_46_Cons)) {
      u_value v_t = u_field(l1191, 0);
      if (u_truth(u_mk_bool(1))) {
        e1190 = u_append(e1190, f_parse_46_type_one_1(u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0))));
      }
      l1191 = u_field(l1191, 1);
    }
    t1195 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TFun, 2, t1189, f_parse_46_type_one_1(v_r)), u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l__42__1192 = U_LIT("*");
    u_value t1194;
    if (u_truth(u_eq(v_op, &l__42__1192))) {
      static struct u_object l__40_product_41__1193 = U_LIT("(product)");
      t1194 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(&l__40_product_41__1193), f_list_46__43__43__2(f_parse_46_type_components_1(v_l), f_parse_46_type_components_1(v_r))), u_mk_con(&n_list_46_Nil, 0));
    } else {
      t1194 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_l), u_mk_con(&n_list_46_Cons, 2, f_parse_46_type_one_1(v_r), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
    }
    t1195 = t1194;
  }
  return t1195;
}

u_value f_parse_46_has_invalid_1(u_value v_t) {
  u_value s1196 = v_t;
  u_value t1197 = NULL;
  if ((!t1197) && (u_is_con(s1196, &n_syntax_46_TSort) && (u_is_con(u_field(s1196, 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(s1196, 0), 0), &n_option_46_None) && u_equals(u_field(u_field(s1196, 0), 1), u_mk_lit("(invalid)")))))) {
    t1197 = u_mk_bool(1);
  }
  if ((!t1197) && u_is_con(s1196, &n_syntax_46_TSort)) {
    u_value v_ts = u_field(s1196, 1);
    t1197 = f_parse_46_any_invalid_1(v_ts);
  }
  if ((!t1197) && u_is_con(s1196, &n_syntax_46_TFun)) {
    u_value v_ts = u_field(s1196, 0);
    u_value v_r = u_field(s1196, 1);
    u_value t1198 = f_parse_46_any_invalid_1(v_ts);
    if (!u_truth(t1198)) {
      t1198 = f_parse_46_has_invalid_1(v_r);
    }
    t1197 = t1198;
  }
  if ((!t1197) && u_is_con(s1196, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1196, 0);
    t1197 = f_parse_46_any_invalid_1(v_ts);
  }
  if (!t1197) {
    t1197 = u_mk_bool(0);
  }
  if (!t1197) {
    u_no_case(s1196);
  }
  return t1197;
}

u_value f_parse_46_any_invalid_1(u_value v_ts) {
  u_value s1199 = v_ts;
  u_value t1200 = NULL;
  if ((!t1200) && u_is_con(s1199, &n_list_46_Nil)) {
    t1200 = u_mk_bool(0);
  }
  if ((!t1200) && u_is_con(s1199, &n_list_46_Cons)) {
    u_value v_t = u_field(s1199, 0);
    u_value v_more = u_field(s1199, 1);
    u_value t1201 = f_parse_46_has_invalid_1(v_t);
    if (!u_truth(t1201)) {
      t1201 = f_parse_46_any_invalid_1(v_more);
    }
    t1200 = t1201;
  }
  if (!t1200) {
    u_no_case(s1199);
  }
  return t1200;
}

u_value f_parse_46_starts_type_2(u_value v_ns, u_value v_t) {
  u_value s1202 = v_t;
  u_value t1203 = NULL;
  if ((!t1203) && u_is_con(s1202, &n_lex_46_TName)) {
    u_value v_n = u_field(s1202, 0);
    u_value t1204 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
    if (u_truth(t1204)) {
      t1204 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
    }
    t1203 = t1204;
  }
  if ((!t1203) && (u_is_con(s1202, &n_lex_46_TPunct) && u_equals(u_field(s1202, 0), u_mk_char(40)))) {
    t1203 = u_mk_bool(1);
  }
  if (!t1203) {
    t1203 = u_mk_bool(0);
  }
  if (!t1203) {
    u_no_case(s1202);
  }
  return t1203;
}

void p_parse_46_parse_type(u_value v_sc, u_value *r_t, u_value *r_ts) {
  {
    u_value v_start = NULL;
    u_value v_items = NULL;
    u_value v_r = NULL;
    u_value t1205 = f_parse_46_head_offset_1(*r_ts);
    v_start = t1205;
    p_parse_46_type_items(v_sc, &v_items, r_ts);
    p_parse_46_resolve_items(v_items, u_mk_con(&n_parse_46_Ops, 4, f_parse_46_Scope_46_notations_1(v_sc), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_prefix_2, 0), u_mk_closure(w_f_parse_46_type_infix_3, 0)), f_parse_46_head_offset_1(*r_ts), &v_r);
    u_value t1206 = f_parse_46_type_one_1(v_r);
    *r_t = t1206;
    if (u_truth(f_parse_46_has_invalid_1(*r_t))) {
      {
        static struct u_object l_tuple_32_types_32_are_32__1207 = U_LIT("tuple types are written A * B");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_tuple_32_types_32_are_32__1207, v_start), "parse.ParseError");
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
    u_value t1208 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1208;
    u_value t1209 = u_mk_bool(1);
    v_next = t1209;
    u_value t1210 = u_mk_bool(1);
    v_go = t1210;
    u_value t1219 = v_go;
    while (u_truth(t1219)) {
      {
        u_value s1211 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1212 = 0;
        if ((!d1212) && u_is_con(s1211, &n_option_46_Some)) {
          u_value v_op = u_field(s1211, 0);
          d1212 = 1;
          {
            u_value t1213 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1214 = u_mk_bool(1);
            v_rev = t1213;
            v_next = t1214;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1212) && u_is_con(s1211, &n_option_46_None)) {
          u_value t1217 = v_next;
          if (u_truth(t1217)) {
            t1217 = f_parse_46_starts_type_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1217)) {
            d1212 = 1;
            {
              p_parse_46_type_basic(v_sc, &v_b, r_ts);
              u_value t1215 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_b), v_rev);
              u_value t1216 = u_mk_bool(0);
              v_rev = t1215;
              v_next = t1216;
            }
          }
        }
        if (!d1212) {
          d1212 = 1;
          {
            u_value t1218 = u_mk_bool(0);
            v_go = t1218;
          }
        }
        if (!d1212) {
          u_no_case(s1211);
        }
      }
      t1219 = v_go;
    }
    u_value s1220 = v_rev;
    int d1221 = 0;
    if ((!d1221) && u_is_con(s1220, &n_list_46_Nil)) {
      d1221 = 1;
      {
        static struct u_object l_type_32_expected_32_bu_1222 = U_LIT("type expected but ");
        static struct u_object l__32_found_1223 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_type_32_expected_32_bu_1222, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1223)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1221) {
      d1221 = 1;
      {
        u_value t1224 = f_list_46_reverse_1(v_rev);
        *r_items = t1224;
      }
    }
    if (!d1221) {
      u_no_case(s1220);
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
            u_value t1225 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), v_args), u_mk_con(&n_list_46_Nil, 0));
            *r_r = t1225;
          }
        } else {
          if (u_truth(f_list_46_contains_2(v_n, f_parse_46_Scope_46_params_1(v_sc)))) {
            {
              u_value t1226 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TParam, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1226;
            }
          } else {
            {
              u_value t1227 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0));
              *r_r = t1227;
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
    u_value t1228 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1228;
    u_value t1229 = u_eq(v_close, u_mk_char(41));
    if (u_truth(t1229)) {
      t1229 = f_parse_46_at_punct_2(*r_ts, v_close);
    }
    if (u_truth(t1229)) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
        u_value t1230 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1230;
        u_value t1232 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1232)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_type(v_sc, &v_t, r_ts);
            u_value t1231 = u_mk_con(&n_list_46_Cons, 2, v_t, v_rev);
            v_rev = t1231;
          }
          t1232 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1233 = f_list_46_reverse_1(v_rev);
    *r_r = t1233;
  }
}

u_value f_parse_46_expr_op1_2(u_value v_op, u_value v_a) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_parse_46_expr_op2_3(u_value v_op, u_value v_a, u_value v_b) {
  return u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_op), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Cons, 2, v_b, u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_parse_46_cons_exprs_2(u_value v_es, u_value v_rest) {
  u_value s1234 = v_es;
  u_value t1235 = NULL;
  if ((!t1235) && u_is_con(s1234, &n_list_46_Nil)) {
    t1235 = v_rest;
  }
  if ((!t1235) && u_is_con(s1234, &n_list_46_Cons)) {
    u_value v_e = u_field(s1234, 0);
    u_value v_more = u_field(s1234, 1);
    static struct u_object l_Cons_1236 = U_LIT("Cons");
    t1235 = f_parse_46_expr_op2_3(&l_Cons_1236, v_e, f_parse_46_cons_exprs_2(v_more, v_rest));
  }
  if (!t1235) {
    u_no_case(s1234);
  }
  return t1235;
}

u_value f_parse_46_starts_expr_2(u_value v_ns, u_value v_t) {
  u_value s1237 = v_t;
  u_value t1238 = NULL;
  if ((!t1238) && u_is_con(s1237, &n_lex_46_TName)) {
    u_value v_n = u_field(s1237, 0);
    static struct u_object l_let_1239 = U_LIT("let");
    static struct u_object l_if_1240 = U_LIT("if");
    static struct u_object l_lambda_1241 = U_LIT("lambda");
    static struct u_object l_match_1242 = U_LIT("match");
    static struct u_object l_true_1243 = U_LIT("true");
    static struct u_object l_false_1244 = U_LIT("false");
    u_value t1246 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_let_1239, u_mk_con(&n_list_46_Cons, 2, &l_if_1240, u_mk_con(&n_list_46_Cons, 2, &l_lambda_1241, u_mk_con(&n_list_46_Cons, 2, &l_match_1242, u_mk_con(&n_list_46_Cons, 2, &l_true_1243, u_mk_con(&n_list_46_Cons, 2, &l_false_1244, u_mk_con(&n_list_46_Nil, 0))))))));
    if (!u_truth(t1246)) {
      u_value t1245 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1245)) {
        t1245 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1246 = t1245;
    }
    t1238 = t1246;
  }
  if ((!t1238) && u_is_con(s1237, &n_lex_46_TNum)) {
    t1238 = u_mk_bool(1);
  }
  if ((!t1238) && u_is_con(s1237, &n_lex_46_TStr)) {
    t1238 = u_mk_bool(1);
  }
  if ((!t1238) && u_is_con(s1237, &n_lex_46_TChar)) {
    t1238 = u_mk_bool(1);
  }
  if ((!t1238) && u_is_con(s1237, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1237, 0);
    u_value t1247 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1247)) {
      t1247 = u_eq(v_c, u_mk_char(91));
    }
    t1238 = t1247;
  }
  if (!t1238) {
    t1238 = u_mk_bool(0);
  }
  if (!t1238) {
    u_no_case(s1237);
  }
  return t1238;
}

void p_parse_46_parse_expr(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    static struct u_object l_raise_1248 = U_LIT("raise");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_raise_1248))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, r_e, r_ts);
        u_value t1249 = u_mk_con(&n_syntax_46_ERaise, 2, *r_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_e = t1249;
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
    u_value t1250 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1250;
    u_value t1251 = u_mk_bool(1);
    v_next = t1251;
    u_value t1252 = u_mk_bool(1);
    v_go = t1252;
    u_value t1261 = v_go;
    while (u_truth(t1261)) {
      {
        u_value s1253 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1254 = 0;
        if ((!d1254) && u_is_con(s1253, &n_option_46_Some)) {
          u_value v_op = u_field(s1253, 0);
          d1254 = 1;
          {
            u_value t1255 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1256 = u_mk_bool(1);
            v_rev = t1255;
            v_next = t1256;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1254) && u_is_con(s1253, &n_option_46_None)) {
          u_value t1259 = v_next;
          if (u_truth(t1259)) {
            t1259 = f_parse_46_starts_expr_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1259)) {
            d1254 = 1;
            {
              p_parse_46_parse_app(v_sc, &v_e, r_ts);
              u_value t1257 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_e), v_rev);
              u_value t1258 = u_mk_bool(0);
              v_rev = t1257;
              v_next = t1258;
            }
          }
        }
        if (!d1254) {
          d1254 = 1;
          {
            u_value t1260 = u_mk_bool(0);
            v_go = t1260;
          }
        }
        if (!d1254) {
          u_no_case(s1253);
        }
      }
      t1261 = v_go;
    }
    u_value s1262 = v_rev;
    int d1263 = 0;
    if ((!d1263) && u_is_con(s1262, &n_list_46_Nil)) {
      d1263 = 1;
      {
        static struct u_object l_expression_32_expec_1264 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1265 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1264, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1265)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1263) {
      d1263 = 1;
      {
        u_value t1266 = f_list_46_reverse_1(v_rev);
        *r_items = t1266;
      }
    }
    if (!d1263) {
      u_no_case(s1262);
    }
  }
}

void p_parse_46_parse_exprs(u_value v_sc, u_value *r_es, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1267 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1267;
    u_value t1269 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1269)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1268 = u_mk_con(&n_list_46_Cons, 2, v_e, v_rev);
        v_rev = t1268;
      }
      t1269 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1270 = f_list_46_reverse_1(v_rev);
    *r_es = t1270;
  }
}

void p_parse_46_expr_list(u_value v_sc, u_value v_close, u_value *r_es, u_value *r_ts) {
  {
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
        u_value t1271 = u_mk_con(&n_list_46_Nil, 0);
        *r_es = t1271;
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
    u_value t1272 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    if (!u_truth(t1272)) {
      t1272 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
    }
    u_value t1276 = t1272;
    while (u_truth(t1276)) {
      {
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_expr_list(v_sc, u_mk_char(41), &v_args, r_ts);
            u_value t1273 = u_mk_con(&n_syntax_46_EApp, 2, *r_e, v_args);
            *r_e = t1273;
          }
        } else {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_f, r_ts);
            static struct u_object l__46__1274 = U_LIT(".");
            u_value t1275 = u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(u_concat(&l__46__1274, v_f)), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, *r_e, u_mk_con(&n_list_46_Nil, 0)));
            *r_e = t1275;
          }
        }
      }
      u_value t1277 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
      if (!u_truth(t1277)) {
        t1277 = f_parse_46_at_punct_2(*r_ts, u_mk_char(46));
      }
      t1276 = t1277;
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
    u_value s1278 = f_parse_46_head_token_1(*r_ts);
    int d1279 = 0;
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TName) && u_equals(u_field(s1278, 0), u_mk_lit("let")))) {
      d1279 = 1;
      {
        p_parse_46_parse_let(v_sc, r_e, r_ts);
      }
    }
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TName) && u_equals(u_field(s1278, 0), u_mk_lit("if")))) {
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        static struct u_object l_then_1280 = U_LIT("then");
        p_parse_46_keyword(&l_then_1280, r_ts);
        p_parse_46_parse_expr(v_sc, &v_l, r_ts);
        static struct u_object l_else_1281 = U_LIT("else");
        p_parse_46_keyword(&l_else_1281, r_ts);
        p_parse_46_parse_expr(v_sc, &v_r, r_ts);
        u_value t1282 = u_mk_con(&n_syntax_46_EIte, 3, v_a, v_l, v_r);
        *r_e = t1282;
      }
    }
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TName) && u_equals(u_field(s1278, 0), u_mk_lit("lambda")))) {
      d1279 = 1;
      {
        p_parse_46_parse_lambda(v_sc, r_e, r_ts);
      }
    }
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TName) && u_equals(u_field(s1278, 0), u_mk_lit("match")))) {
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        p_parse_46_parse_expr_cases(v_sc, &v_cs, r_ts);
        u_value t1283 = u_mk_con(&n_syntax_46_EMatch, 2, v_a, v_cs);
        *r_e = t1283;
      }
    }
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TName) && u_equals(u_field(s1278, 0), u_mk_lit("true")))) {
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1284 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_e = t1284;
      }
    }
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TName) && u_equals(u_field(s1278, 0), u_mk_lit("false")))) {
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1285 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_e = t1285;
      }
    }
    if ((!d1279) && u_is_con(s1278, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1278, 0);
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1286 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_n));
        *r_e = t1286;
      }
    }
    if ((!d1279) && u_is_con(s1278, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1278, 0);
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1287 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_e = t1287;
      }
    }
    if ((!d1279) && u_is_con(s1278, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1278, 0);
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1288 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_e = t1288;
      }
    }
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TPunct) && u_equals(u_field(s1278, 0), u_mk_char(40)))) {
      d1279 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_expr_list(v_sc, u_mk_char(41), &v_es, r_ts);
        u_value s1289 = v_es;
        int d1290 = 0;
        if ((!d1290) && (u_is_con(s1289, &n_list_46_Cons) && u_is_con(u_field(s1289, 1), &n_list_46_Nil))) {
          u_value v_x = u_field(s1289, 0);
          d1290 = 1;
          {
            u_value t1291 = v_x;
            *r_e = t1291;
          }
        }
        if (!d1290) {
          d1290 = 1;
          {
            u_value t1292 = u_mk_con(&n_syntax_46_ETuple, 1, v_es);
            *r_e = t1292;
          }
        }
        if (!d1290) {
          u_no_case(s1289);
        }
      }
    }
    if ((!d1279) && (u_is_con(s1278, &n_lex_46_TPunct) && u_equals(u_field(s1278, 0), u_mk_char(91)))) {
      d1279 = 1;
      {
        p_parse_46_parse_brackets(v_sc, r_e, r_ts);
      }
    }
    if ((!d1279) && u_is_con(s1278, &n_lex_46_TName)) {
      u_value v_n = u_field(s1278, 0);
      u_value t1294 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1294)) {
        t1294 = f_prelude_46_not_1(f_parse_46_declared_2(f_parse_46_Scope_46_notations_1(v_sc), v_n));
      }
      if (u_truth(t1294)) {
        d1279 = 1;
        {
          p_parse_46_advance(r_ts);
          u_value t1293 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
          *r_e = t1293;
        }
      }
    }
    if (!d1279) {
      d1279 = 1;
      {
        static struct u_object l_expression_32_expec_1295 = U_LIT("expression expected but ");
        static struct u_object l__32_found_1296 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_expression_32_expec_1295, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1296)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1279) {
      u_no_case(s1278);
    }
  }
}

u_value f_parse_46_pattern_vars_1(u_value v_ps) {
  u_value s1297 = v_ps;
  u_value t1298 = NULL;
  if ((!t1298) && u_is_con(s1297, &n_list_46_Nil)) {
    t1298 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1298) && (u_is_con(s1297, &n_list_46_Cons) && u_is_con(u_field(s1297, 0), &n_syntax_46_PVar))) {
    u_value v_x = u_field(u_field(s1297, 0), 0);
    u_value v_more = u_field(s1297, 1);
    u_value s1299 = f_parse_46_pattern_vars_1(v_more);
    u_value t1300 = NULL;
    if ((!t1300) && u_is_con(s1299, &n_option_46_Some)) {
      u_value v_xs = u_field(s1299, 0);
      t1300 = u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_list_46_Cons, 2, v_x, v_xs));
    }
    if ((!t1300) && u_is_con(s1299, &n_option_46_None)) {
      t1300 = u_mk_con(&n_option_46_None, 0);
    }
    if (!t1300) {
      u_no_case(s1299);
    }
    t1298 = t1300;
  }
  if (!t1298) {
    t1298 = u_mk_con(&n_option_46_None, 0);
  }
  if (!t1298) {
    u_no_case(s1297);
  }
  return t1298;
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
    static struct u_object l__58__61__1301 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1301), r_ts);
    p_parse_46_parse_expr(v_sc, &v_a, r_ts);
    u_value t1302 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
    u_value t1303 = u_mk_con(&n_list_46_Cons, 2, v_a, u_mk_con(&n_list_46_Nil, 0));
    v_ps = t1302;
    v_es = t1303;
    u_value t1307 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1307)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        static struct u_object l__58__61__1304 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1304), r_ts);
        p_parse_46_parse_expr(v_sc, &v_a, r_ts);
        u_value t1305 = u_mk_con(&n_list_46_Cons, 2, v_p, v_ps);
        u_value t1306 = u_mk_con(&n_list_46_Cons, 2, v_a, v_es);
        v_ps = t1305;
        v_es = t1306;
      }
      t1307 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    static struct u_object l_in_1308 = U_LIT("in");
    p_parse_46_keyword(&l_in_1308, r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1309 = f_list_46_reverse_1(v_ps);
    u_value t1310 = f_list_46_reverse_1(v_es);
    v_ps = t1309;
    v_es = t1310;
    u_value s1311 = u_mk_tuple(3, f_parse_46_pattern_vars_1(v_ps), v_ps, v_es);
    int d1312 = 0;
    if ((!d1312) && u_is_con(u_field(s1311, 0), &n_option_46_Some)) {
      u_value v_xs = u_field(u_field(s1311, 0), 0);
      d1312 = 1;
      {
        u_value t1313 = u_mk_con(&n_syntax_46_ELet, 2, f_list_46_zip_2(v_xs, v_es), v_b);
        *r_e = t1313;
      }
    }
    if ((!d1312) && (u_is_con(u_field(s1311, 0), &n_option_46_None) && (u_is_con(u_field(s1311, 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(s1311, 1), 1), &n_list_46_Nil) && (u_is_con(u_field(s1311, 2), &n_list_46_Cons) && u_is_con(u_field(u_field(s1311, 2), 1), &n_list_46_Nil)))))) {
      u_value v_q = u_field(u_field(s1311, 1), 0);
      u_value v_c = u_field(u_field(s1311, 2), 0);
      d1312 = 1;
      {
        u_value t1314 = u_mk_con(&n_syntax_46_EMatch, 2, v_c, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_q, u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1314;
      }
    }
    if (!d1312) {
      d1312 = 1;
      {
        u_value t1315 = u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_ETuple, 1, v_es), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PTuple, 1, v_ps), u_mk_con(&n_option_46_None, 0), v_b), u_mk_con(&n_list_46_Nil, 0)));
        *r_e = t1315;
      }
    }
    if (!d1312) {
      u_no_case(s1311);
    }
  }
}

void p_parse_46_parse_lambda(u_value v_sc, u_value *r_e, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_xs = NULL;
    u_value v_b = NULL;
    p_parse_46_advance(r_ts);
    u_value t1316 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1316;
    static struct u_object l__61__62__1317 = U_LIT("=>");
    if (u_truth(f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1317)))) {
      {
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1318 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
        v_xs = t1318;
        u_value t1320 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1320)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            u_value t1319 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
            v_xs = t1319;
          }
          t1320 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
    }
    static struct u_object l__61__62__1321 = U_LIT("=>");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1321), r_ts);
    p_parse_46_parse_expr(v_sc, &v_b, r_ts);
    u_value t1322 = u_mk_con(&n_syntax_46_ELam, 2, f_list_46_reverse_1(v_xs), v_b);
    *r_e = t1322;
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
        static struct u_object l_Nil_1323 = U_LIT("Nil");
        u_value t1324 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1323), u_mk_con(&n_syntax_46_THole, 0)));
        *r_e = t1324;
      }
    } else {
      {
        p_parse_46_parse_expr(v_sc, &v_first, r_ts);
        static struct u_object l_for_1325 = U_LIT("for");
        if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_for_1325))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_plain(v_sc, &v_x, r_ts);
            static struct u_object l_in_1326 = U_LIT("in");
            p_parse_46_keyword(&l_in_1326, r_ts);
            p_parse_46_parse_expr(v_sc, &v_l, r_ts);
            u_value t1327 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
            u_value t1328 = u_mk_con(&n_list_46_Cons, 2, v_l, u_mk_con(&n_list_46_Nil, 0));
            v_xs = t1327;
            v_lists = t1328;
            u_value t1332 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1332)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_plain(v_sc, &v_x, r_ts);
                static struct u_object l_in_1329 = U_LIT("in");
                p_parse_46_keyword(&l_in_1329, r_ts);
                p_parse_46_parse_expr(v_sc, &v_l, r_ts);
                u_value t1330 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_xs);
                u_value t1331 = u_mk_con(&n_list_46_Cons, 2, v_l, v_lists);
                v_xs = t1330;
                v_lists = t1331;
              }
              t1332 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            u_value t1333 = u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
            v_c = t1333;
            static struct u_object l_if_1334 = U_LIT("if");
            if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1334))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_c, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1335 = u_mk_con(&n_syntax_46_EComp, 4, v_first, f_list_46_reverse_1(v_xs), f_list_46_reverse_1(v_lists), v_c);
            *r_e = t1335;
          }
        } else {
          {
            u_value t1336 = u_mk_con(&n_list_46_Cons, 2, v_first, u_mk_con(&n_list_46_Nil, 0));
            v_es = t1336;
            u_value t1338 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1338)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_first, r_ts);
                u_value t1337 = u_mk_con(&n_list_46_Cons, 2, v_first, v_es);
                v_es = t1337;
              }
              t1338 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1339 = U_LIT("Nil");
            u_value t1340 = u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_Nil_1339), u_mk_con(&n_syntax_46_THole, 0)));
            v_rest = t1340;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_expr(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1341 = f_parse_46_cons_exprs_2(f_list_46_reverse_1(v_es), v_rest);
            *r_e = t1341;
          }
        }
      }
    }
  }
}

void p_parse_46_parse_guard(u_value v_sc, u_value *r_g, u_value *r_ts) {
  {
    u_value v_e = NULL;
    static struct u_object l_if_1342 = U_LIT("if");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_if_1342))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1343 = u_mk_con(&n_option_46_Some, 1, v_e);
        *r_g = t1343;
      }
    } else {
      {
        u_value t1344 = u_mk_con(&n_option_46_None, 0);
        *r_g = t1344;
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
    u_value t1345 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1345;
    static struct u_object l_case_1346 = U_LIT("case");
    u_value t1349 = f_parse_46_at_keyword_2(*r_ts, &l_case_1346);
    while (u_truth(t1349)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1347 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1347), r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1348 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1348;
      }
      static struct u_object l_case_1350 = U_LIT("case");
      t1349 = f_parse_46_at_keyword_2(*r_ts, &l_case_1350);
    }
    static struct u_object l_else_1351 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1351))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_b, r_ts);
        u_value t1352 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1352;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1353 = U_LIT("end");
    p_parse_46_keyword(&l_end_1353, r_ts);
    u_value t1354 = f_list_46_reverse_1(v_rev);
    *r_cs = t1354;
  }
}

void p_parse_46_last_case(u_value *r_ts) {
  {
    static struct u_object l_case_1355 = U_LIT("case");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_case_1355))) {
      {
        static struct u_object l_else_32_must_32_be_32_the_1356 = U_LIT("else must be the last case");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, &l_else_32_must_32_be_32_the_1356, f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
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
  u_value s1357 = v_ps;
  u_value t1358 = NULL;
  if ((!t1358) && u_is_con(s1357, &n_list_46_Nil)) {
    t1358 = v_rest;
  }
  if ((!t1358) && u_is_con(s1357, &n_list_46_Cons)) {
    u_value v_p = u_field(s1357, 0);
    u_value v_more = u_field(s1357, 1);
    static struct u_object l_Cons_1359 = U_LIT("Cons");
    t1358 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Cons_1359), u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Cons, 2, f_parse_46_cons_pats_2(v_more, v_rest), u_mk_con(&n_list_46_Nil, 0))));
  }
  if (!t1358) {
    u_no_case(s1357);
  }
  return t1358;
}

u_value f_parse_46_starts_pattern_2(u_value v_ns, u_value v_t) {
  u_value s1360 = v_t;
  u_value t1361 = NULL;
  if ((!t1361) && u_is_con(s1360, &n_lex_46_TName)) {
    u_value v_n = u_field(s1360, 0);
    static struct u_object l___1362 = U_LIT("_");
    static struct u_object l_true_1363 = U_LIT("true");
    static struct u_object l_false_1364 = U_LIT("false");
    u_value t1366 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l___1362, u_mk_con(&n_list_46_Cons, 2, &l_true_1363, u_mk_con(&n_list_46_Cons, 2, &l_false_1364, u_mk_con(&n_list_46_Nil, 0)))));
    if (!u_truth(t1366)) {
      u_value t1365 = f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n));
      if (u_truth(t1365)) {
        t1365 = f_prelude_46_not_1(f_parse_46_declared_2(v_ns, v_n));
      }
      t1366 = t1365;
    }
    t1361 = t1366;
  }
  if ((!t1361) && u_is_con(s1360, &n_lex_46_TNum)) {
    t1361 = u_mk_bool(1);
  }
  if ((!t1361) && u_is_con(s1360, &n_lex_46_TStr)) {
    t1361 = u_mk_bool(1);
  }
  if ((!t1361) && u_is_con(s1360, &n_lex_46_TChar)) {
    t1361 = u_mk_bool(1);
  }
  if ((!t1361) && u_is_con(s1360, &n_lex_46_TPunct)) {
    u_value v_c = u_field(s1360, 0);
    u_value t1367 = u_eq(v_c, u_mk_char(40));
    if (!u_truth(t1367)) {
      t1367 = u_eq(v_c, u_mk_char(91));
    }
    t1361 = t1367;
  }
  if (!t1361) {
    t1361 = u_mk_bool(0);
  }
  if (!t1361) {
    u_no_case(s1360);
  }
  return t1361;
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
    u_value t1368 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1368;
    u_value t1369 = u_mk_bool(1);
    v_next = t1369;
    u_value t1370 = u_mk_bool(1);
    v_go = t1370;
    u_value t1379 = v_go;
    while (u_truth(t1379)) {
      {
        u_value s1371 = f_parse_46_operator_at_2(f_parse_46_Scope_46_notations_1(v_sc), *r_ts);
        int d1372 = 0;
        if ((!d1372) && u_is_con(s1371, &n_option_46_Some)) {
          u_value v_op = u_field(s1371, 0);
          d1372 = 1;
          {
            u_value t1373 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operator, 2, v_op, f_parse_46_head_offset_1(*r_ts)), v_rev);
            u_value t1374 = u_mk_bool(1);
            v_rev = t1373;
            v_next = t1374;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1372) && u_is_con(s1371, &n_option_46_None)) {
          u_value t1377 = v_next;
          if (u_truth(t1377)) {
            t1377 = f_parse_46_starts_pattern_2(f_parse_46_Scope_46_notations_1(v_sc), f_parse_46_head_token_1(*r_ts));
          }
          if (u_truth(t1377)) {
            d1372 = 1;
            {
              p_parse_46_pattern_simple(v_sc, &v_p, r_ts);
              u_value t1375 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_parse_46_Operand, 1, v_p), v_rev);
              u_value t1376 = u_mk_bool(0);
              v_rev = t1375;
              v_next = t1376;
            }
          }
        }
        if (!d1372) {
          d1372 = 1;
          {
            u_value t1378 = u_mk_bool(0);
            v_go = t1378;
          }
        }
        if (!d1372) {
          u_no_case(s1371);
        }
      }
      t1379 = v_go;
    }
    u_value s1380 = v_rev;
    int d1381 = 0;
    if ((!d1381) && u_is_con(s1380, &n_list_46_Nil)) {
      d1381 = 1;
      {
        static struct u_object l_pattern_32_expected_1382 = U_LIT("pattern expected but ");
        static struct u_object l__32_found_1383 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_pattern_32_expected_1382, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1383)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1381) {
      d1381 = 1;
      {
        u_value t1384 = f_list_46_reverse_1(v_rev);
        *r_items = t1384;
      }
    }
    if (!d1381) {
      u_no_case(s1380);
    }
  }
}

void p_parse_46_pattern_list(u_value v_sc, u_value v_close, u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value t1385 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1385;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, v_close))) {
      {
        p_parse_46_advance(r_ts);
      }
    } else {
      {
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        u_value t1386 = u_mk_con(&n_list_46_Cons, 2, v_p, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1386;
        u_value t1388 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1388)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
            u_value t1387 = u_mk_con(&n_list_46_Cons, 2, v_p, v_rev);
            v_rev = t1387;
          }
          t1388 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, v_close), r_ts);
      }
    }
    u_value t1389 = f_list_46_reverse_1(v_rev);
    *r_ps = t1389;
  }
}

void p_parse_46_pattern_simple(u_value v_sc, u_value *r_p, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_ps = NULL;
    u_value v_rest = NULL;
    u_value s1390 = f_parse_46_head_token_1(*r_ts);
    int d1391 = 0;
    if ((!d1391) && (u_is_con(s1390, &n_lex_46_TName) && u_equals(u_field(s1390, 0), u_mk_lit("_")))) {
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1392 = u_mk_con(&n_syntax_46_PWild, 0);
        *r_p = t1392;
      }
    }
    if ((!d1391) && (u_is_con(s1390, &n_lex_46_TName) && u_equals(u_field(s1390, 0), u_mk_lit("true")))) {
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1393 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(1)));
        *r_p = t1393;
      }
    }
    if ((!d1391) && (u_is_con(s1390, &n_lex_46_TName) && u_equals(u_field(s1390, 0), u_mk_lit("false")))) {
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1394 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VBool, 1, u_mk_bool(0)));
        *r_p = t1394;
      }
    }
    if ((!d1391) && u_is_con(s1390, &n_lex_46_TNum)) {
      u_value v_k = u_field(s1390, 0);
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1395 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VInt, 1, v_k));
        *r_p = t1395;
      }
    }
    if ((!d1391) && u_is_con(s1390, &n_lex_46_TStr)) {
      u_value v_s = u_field(s1390, 0);
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1396 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VStr, 1, v_s));
        *r_p = t1396;
      }
    }
    if ((!d1391) && u_is_con(s1390, &n_lex_46_TChar)) {
      u_value v_c = u_field(s1390, 0);
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1397 = u_mk_con(&n_syntax_46_PLit, 1, u_mk_con(&n_syntax_46_VChar, 1, v_c));
        *r_p = t1397;
      }
    }
    if ((!d1391) && (u_is_con(s1390, &n_lex_46_TPunct) && u_equals(u_field(s1390, 0), u_mk_char(40)))) {
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
        u_value s1398 = v_ps;
        int d1399 = 0;
        if ((!d1399) && (u_is_con(s1398, &n_list_46_Cons) && u_is_con(u_field(s1398, 1), &n_list_46_Nil))) {
          u_value v_q = u_field(s1398, 0);
          d1399 = 1;
          {
            u_value t1400 = v_q;
            *r_p = t1400;
          }
        }
        if (!d1399) {
          d1399 = 1;
          {
            u_value t1401 = u_mk_con(&n_syntax_46_PTuple, 1, v_ps);
            *r_p = t1401;
          }
        }
        if (!d1399) {
          u_no_case(s1398);
        }
      }
    }
    if ((!d1391) && (u_is_con(s1390, &n_lex_46_TPunct) && u_equals(u_field(s1390, 0), u_mk_char(91)))) {
      d1391 = 1;
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(93)))) {
          {
            p_parse_46_advance(r_ts);
            static struct u_object l_Nil_1402 = U_LIT("Nil");
            u_value t1403 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1402), u_mk_con(&n_list_46_Nil, 0));
            *r_p = t1403;
          }
        } else {
          {
            p_parse_46_parse_pattern(v_sc, r_p, r_ts);
            u_value t1404 = u_mk_con(&n_list_46_Cons, 2, *r_p, u_mk_con(&n_list_46_Nil, 0));
            v_ps = t1404;
            u_value t1406 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1406)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, r_p, r_ts);
                u_value t1405 = u_mk_con(&n_list_46_Cons, 2, *r_p, v_ps);
                v_ps = t1405;
              }
              t1406 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
            static struct u_object l_Nil_1407 = U_LIT("Nil");
            u_value t1408 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(&l_Nil_1407), u_mk_con(&n_list_46_Nil, 0));
            v_rest = t1408;
            if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(59)))) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_parse_pattern(v_sc, &v_rest, r_ts);
              }
            }
            p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
            u_value t1409 = f_parse_46_cons_pats_2(f_list_46_reverse_1(v_ps), v_rest);
            *r_p = t1409;
          }
        }
      }
    }
    if (!d1391) {
      d1391 = 1;
      {
        p_parse_46_parse_plain(v_sc, &v_n, r_ts);
        if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_pattern_list(v_sc, u_mk_char(41), &v_ps, r_ts);
            u_value t1410 = u_mk_con(&n_syntax_46_PApp, 2, f_syntax_46_unqualified_1(v_n), v_ps);
            *r_p = t1410;
          }
        } else {
          {
            u_value t1411 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)));
            *r_p = t1411;
          }
        }
      }
    }
    if (!d1391) {
      u_no_case(s1390);
    }
  }
}

u_value f_parse_46_starts_stmt_1(u_value v_t) {
  u_value s1412 = v_t;
  u_value t1413 = NULL;
  if ((!t1413) && u_is_con(s1412, &n_lex_46_TName)) {
    u_value v_n = u_field(s1412, 0);
    static struct u_object l_skip_1414 = U_LIT("skip");
    static struct u_object l_if_1415 = U_LIT("if");
    static struct u_object l_while_1416 = U_LIT("while");
    static struct u_object l_call_1417 = U_LIT("call");
    static struct u_object l_match_1418 = U_LIT("match");
    static struct u_object l_assert_1419 = U_LIT("assert");
    static struct u_object l_assign_1420 = U_LIT("assign");
    static struct u_object l_raise_1421 = U_LIT("raise");
    static struct u_object l_try_1422 = U_LIT("try");
    t1413 = f_list_46_contains_2(v_n, u_mk_con(&n_list_46_Cons, 2, &l_skip_1414, u_mk_con(&n_list_46_Cons, 2, &l_if_1415, u_mk_con(&n_list_46_Cons, 2, &l_while_1416, u_mk_con(&n_list_46_Cons, 2, &l_call_1417, u_mk_con(&n_list_46_Cons, 2, &l_match_1418, u_mk_con(&n_list_46_Cons, 2, &l_assert_1419, u_mk_con(&n_list_46_Cons, 2, &l_assign_1420, u_mk_con(&n_list_46_Cons, 2, &l_raise_1421, u_mk_con(&n_list_46_Cons, 2, &l_try_1422, u_mk_con(&n_list_46_Nil, 0)))))))))));
  }
  if (!t1413) {
    t1413 = u_mk_bool(0);
  }
  if (!t1413) {
    u_no_case(s1412);
  }
  return t1413;
}

u_value f_parse_46_handlers_1(u_value v_cs) {
  u_value s1423 = v_cs;
  u_value t1424 = NULL;
  if ((!t1424) && u_is_con(s1423, &n_list_46_Nil)) {
    t1424 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1424) && (u_is_con(s1423, &n_list_46_Cons) && u_is_con(u_field(s1423, 0), &n_syntax_46_Branch))) {
    u_value v_p = u_field(u_field(s1423, 0), 0);
    u_value v_g = u_field(u_field(s1423, 0), 1);
    u_value v_b = u_field(u_field(s1423, 0), 2);
    u_value v_more = u_field(s1423, 1);
    t1424 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Handler, 4, u_mk_con(&n_syntax_46_THole, 0), v_p, v_g, v_b), f_parse_46_handlers_1(v_more));
  }
  if (!t1424) {
    u_no_case(s1423);
  }
  return t1424;
}

void p_parse_46_parse_body(u_value v_sc, u_value *r_b, u_value *r_ts) {
  {
    u_value v_xs = NULL;
    u_value v_inits = NULL;
    u_value v_rev = NULL;
    u_value v_s = NULL;
    u_value t1425 = u_mk_con(&n_list_46_Nil, 0);
    v_xs = t1425;
    u_value t1426 = u_mk_con(&n_list_46_Nil, 0);
    v_inits = t1426;
    u_value t1427 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1427;
    static struct u_object l_local_1428 = U_LIT("local");
    u_value t1430 = f_parse_46_at_keyword_2(*r_ts, &l_local_1428);
    while (u_truth(t1430)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
        u_value t1429 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1429)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_local(v_sc, &v_xs, &v_inits, r_ts);
          }
          t1429 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
      }
      static struct u_object l_local_1431 = U_LIT("local");
      t1430 = f_parse_46_at_keyword_2(*r_ts, &l_local_1431);
    }
    u_value t1433 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    while (u_truth(t1433)) {
      {
        p_parse_46_parse_stmt(v_sc, &v_s, r_ts);
        u_value t1432 = u_mk_con(&n_list_46_Cons, 2, v_s, v_rev);
        v_rev = t1432;
      }
      t1433 = f_parse_46_starts_stmt_1(f_parse_46_head_token_1(*r_ts));
    }
    u_value t1434 = u_mk_con(&n_syntax_46_SBlock, 2, v_xs, f_list_46__43__43__2(v_inits, f_list_46_reverse_1(v_rev)));
    *r_b = t1434;
  }
}

void p_parse_46_parse_local(u_value v_sc, u_value *r_xs, u_value *r_inits, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    u_value v_e = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1435 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1435;
    static struct u_object l__58__1436 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1436)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    static struct u_object l__58__61__1437 = U_LIT(":=");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1437)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1438 = f_list_46__43__43__2(*r_inits, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_SAssign, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)));
        *r_inits = t1438;
      }
    }
    u_value t1439 = f_list_46__43__43__2(*r_xs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t), u_mk_con(&n_list_46_Nil, 0)));
    *r_xs = t1439;
  }
}

void p_parse_46_parse_stmt(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_e = NULL;
    u_value v_b = NULL;
    u_value v_cs = NULL;
    u_value s1440 = f_parse_46_head_token_1(*r_ts);
    int d1441 = 0;
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("skip")))) {
      d1441 = 1;
      {
        p_parse_46_advance(r_ts);
        u_value t1442 = u_mk_con(&n_syntax_46_SSkip, 0);
        *r_s = t1442;
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("if")))) {
      d1441 = 1;
      {
        p_parse_46_parse_if(v_sc, r_s, r_ts);
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("while")))) {
      d1441 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_end_1443 = U_LIT("end");
        p_parse_46_keyword(&l_end_1443, r_ts);
        u_value t1444 = u_mk_con(&n_syntax_46_SWhile, 2, v_e, v_b);
        *r_s = t1444;
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("call")))) {
      d1441 = 1;
      {
        p_parse_46_parse_call(v_sc, r_s, r_ts);
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("match")))) {
      d1441 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1445 = u_mk_con(&n_syntax_46_SMatch, 2, v_e, v_cs);
        *r_s = t1445;
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("assert")))) {
      d1441 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1446 = u_mk_con(&n_syntax_46_SAssert, 1, v_e);
        *r_s = t1446;
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("assign")))) {
      d1441 = 1;
      {
        p_parse_46_parse_assign(v_sc, r_s, r_ts);
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("raise")))) {
      d1441 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1447 = u_mk_con(&n_syntax_46_SRaise, 2, v_e, u_mk_con(&n_syntax_46_THole, 0));
        *r_s = t1447;
      }
    }
    if ((!d1441) && (u_is_con(s1440, &n_lex_46_TName) && u_equals(u_field(s1440, 0), u_mk_lit("try")))) {
      d1441 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        static struct u_object l_catch_1448 = U_LIT("catch");
        p_parse_46_keyword(&l_catch_1448, r_ts);
        p_parse_46_parse_stmt_cases(v_sc, &v_cs, r_ts);
        u_value t1449 = u_mk_con(&n_syntax_46_STry, 2, v_b, f_parse_46_handlers_1(v_cs));
        *r_s = t1449;
      }
    }
    if (!d1441) {
      d1441 = 1;
      {
        static struct u_object l_statement_32_expect_1450 = U_LIT("statement expected but ");
        static struct u_object l__32_found_1451 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_statement_32_expect_1450, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1451)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1441) {
      u_no_case(s1440);
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
    static struct u_object l_else_1452 = U_LIT("else");
    if (u_truth(f_prelude_46_not_1(f_parse_46_at_keyword_2(*r_ts, &l_else_1452)))) {
      {
        static struct u_object l_end_1453 = U_LIT("end");
        p_parse_46_keyword(&l_end_1453, r_ts);
        u_value t1454 = u_mk_con(&n_syntax_46_SSkip, 0);
        v_r = t1454;
      }
    } else {
      static struct u_object l_if_1455 = U_LIT("if");
      if (u_truth(u_eq(f_parse_46_second_token_1(*r_ts), u_mk_con(&n_lex_46_TName, 1, &l_if_1455)))) {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_if(v_sc, &v_r, r_ts);
        }
      } else {
        {
          p_parse_46_advance(r_ts);
          p_parse_46_parse_body(v_sc, &v_r, r_ts);
          static struct u_object l_end_1456 = U_LIT("end");
          p_parse_46_keyword(&l_end_1456, r_ts);
        }
      }
    }
    u_value t1457 = u_mk_con(&n_syntax_46_SIf, 3, v_c, v_l, v_r);
    *r_s = t1457;
  }
}

void p_parse_46_parse_assign(u_value v_sc, u_value *r_s, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_e = NULL;
    u_value v_rev = NULL;
    p_parse_46_advance(r_ts);
    p_parse_46_parse_plain(v_sc, &v_x, r_ts);
    static struct u_object l__58__61__1458 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1458), r_ts);
    p_parse_46_parse_expr(v_sc, &v_e, r_ts);
    u_value t1459 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1459;
    u_value t1462 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    while (u_truth(t1462)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        static struct u_object l__58__61__1460 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1460), r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1461 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_e), v_rev);
        v_rev = t1461;
      }
      t1462 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
    }
    u_value t1463 = u_mk_con(&n_syntax_46_SAssign, 1, f_list_46_reverse_1(v_rev));
    *r_s = t1463;
  }
}

void p_parse_46_parse_stmt_cases(u_value v_sc, u_value *r_cs, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_p = NULL;
    u_value v_g = NULL;
    u_value v_b = NULL;
    u_value t1464 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1464;
    static struct u_object l_case_1465 = U_LIT("case");
    u_value t1468 = f_parse_46_at_keyword_2(*r_ts, &l_case_1465);
    while (u_truth(t1468)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_pattern(v_sc, &v_p, r_ts);
        p_parse_46_parse_guard(v_sc, &v_g, r_ts);
        static struct u_object l__61__62__1466 = U_LIT("=>");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__61__62__1466), r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1467 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, v_p, v_g, v_b), v_rev);
        v_rev = t1467;
      }
      static struct u_object l_case_1469 = U_LIT("case");
      t1468 = f_parse_46_at_keyword_2(*r_ts, &l_case_1469);
    }
    static struct u_object l_else_1470 = U_LIT("else");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_else_1470))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_body(v_sc, &v_b, r_ts);
        u_value t1471 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), v_b), v_rev);
        v_rev = t1471;
        p_parse_46_last_case(r_ts);
      }
    }
    static struct u_object l_end_1472 = U_LIT("end");
    p_parse_46_keyword(&l_end_1472, r_ts);
    u_value t1473 = f_list_46_reverse_1(v_rev);
    *r_cs = t1473;
  }
}

u_value f_parse_46_section_at_3(u_value v_ts, u_value v_w, u_value v_first) {
  u_value t1475;
  if (u_truth(v_first)) {
    t1475 = f_parse_46_at_keyword_2(v_ts, v_w);
  } else {
    u_value t1474 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
    if (u_truth(t1474)) {
      t1474 = u_eq(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, v_w));
    }
    t1475 = t1474;
  }
  return t1475;
}

u_value f_parse_46_item_follows_1(u_value v_ts) {
  u_value t1477 = f_parse_46_at_punct_2(v_ts, u_mk_char(44));
  if (u_truth(t1477)) {
    static struct u_object l_out_1476 = U_LIT("out");
    t1477 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_out_1476));
  }
  u_value t1479 = t1477;
  if (u_truth(t1479)) {
    static struct u_object l_ref_1478 = U_LIT("ref");
    t1479 = f_prelude_46__33__61__2(f_parse_46_second_token_1(v_ts), u_mk_con(&n_lex_46_TName, 1, &l_ref_1478));
  }
  return t1479;
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
    u_value t1480 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1481 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1482 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1483 = u_mk_bool(1);
    v_ins = t1480;
    v_outs = t1481;
    v_refs = t1482;
    v_first = t1483;
    static struct u_object l_in_1484 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1484))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(v_sc, &v_e, r_ts);
        u_value t1485 = u_mk_con(&n_list_46_Cons, 2, v_e, u_mk_con(&n_list_46_Nil, 0));
        u_value t1486 = u_mk_bool(0);
        v_ins = t1485;
        v_first = t1486;
        u_value t1488 = f_parse_46_item_follows_1(*r_ts);
        while (u_truth(t1488)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_expr(v_sc, &v_e, r_ts);
            u_value t1487 = u_mk_con(&n_list_46_Cons, 2, v_e, v_ins);
            v_ins = t1487;
          }
          t1488 = f_parse_46_item_follows_1(*r_ts);
        }
      }
    }
    static struct u_object l_out_1489 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1489, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_outs, r_ts);
        u_value t1490 = u_mk_bool(0);
        v_first = t1490;
      }
    }
    static struct u_object l_ref_1491 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1491, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_variables(v_sc, &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    u_value t1492 = u_mk_con(&n_syntax_46_SCall, 4, f_syntax_46_unqualified_1(v_p), f_list_46_reverse_1(v_ins), v_outs, v_refs);
    *r_s = t1492;
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
    u_value t1493 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1493;
    u_value t1495 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1495)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_plain(v_sc, &v_x, r_ts);
        u_value t1494 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_x), u_mk_con(&n_syntax_46_THole, 0)), v_rev);
        v_rev = t1494;
      }
      t1495 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1496 = f_list_46_reverse_1(v_rev);
    *r_xs = t1496;
  }
}

void p_parse_46_parse_formal(u_value v_sc, u_value *r_x, u_value *r_ts) {
  {
    u_value v_n = NULL;
    u_value v_t = NULL;
    p_parse_46_parse_plain(v_sc, &v_n, r_ts);
    u_value t1497 = u_mk_con(&n_syntax_46_THole, 0);
    v_t = t1497;
    static struct u_object l__58__1498 = U_LIT(":");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__58__1498)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_type(v_sc, &v_t, r_ts);
      }
    }
    u_value t1499 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(v_n), v_t);
    *r_x = t1499;
  }
}

void p_parse_46_parse_formals1(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    u_value v_x = NULL;
    u_value v_rev = NULL;
    p_parse_46_parse_formal(v_sc, &v_x, r_ts);
    u_value t1500 = u_mk_con(&n_list_46_Cons, 2, v_x, u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1500;
    u_value t1502 = f_parse_46_item_follows_1(*r_ts);
    while (u_truth(t1502)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formal(v_sc, &v_x, r_ts);
        u_value t1501 = u_mk_con(&n_list_46_Cons, 2, v_x, v_rev);
        v_rev = t1501;
      }
      t1502 = f_parse_46_item_follows_1(*r_ts);
    }
    u_value t1503 = f_list_46_reverse_1(v_rev);
    *r_xs = t1503;
  }
}

void p_parse_46_parse_formals(u_value v_sc, u_value *r_xs, u_value *r_ts) {
  {
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(40)), r_ts);
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(41)))) {
      {
        u_value t1504 = u_mk_con(&n_list_46_Nil, 0);
        *r_xs = t1504;
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
  u_value s1505 = v_cs;
  u_value t1506 = NULL;
  if ((!t1506) && u_is_con(s1505, &n_list_46_Nil)) {
    t1506 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1506) && (u_is_con(s1505, &n_list_46_Cons) && u_is_con(u_field(s1505, 0), &n_syntax_46_CEval))) {
    u_value v_more = u_field(s1505, 1);
    t1506 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1506) && (u_is_con(s1505, &n_list_46_Cons) && u_is_con(u_field(s1505, 0), &n_syntax_46_CRun))) {
    u_value v_more = u_field(s1505, 1);
    t1506 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1506) && (u_is_con(s1505, &n_list_46_Cons) && u_is_con(u_field(s1505, 0), &n_syntax_46_CTest))) {
    u_value v_more = u_field(s1505, 1);
    t1506 = f_parse_46_declarations_1(v_more);
  }
  if ((!t1506) && u_is_con(s1505, &n_list_46_Cons)) {
    u_value v_c = u_field(s1505, 0);
    u_value v_more = u_field(s1505, 1);
    t1506 = u_mk_con(&n_list_46_Cons, 2, v_c, f_parse_46_declarations_1(v_more));
  }
  if (!t1506) {
    u_no_case(s1505);
  }
  return t1506;
}

u_value f_parse_46_notations_of_1(u_value v_cs) {
  u_value t1507 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1508 = &t1507;
  u_value l1509 = v_cs;
  while (u_is_con(l1509, &n_list_46_Cons)) {
    u_value v_c = u_field(l1509, 0);
    if (u_truth(f_parse_46_is_notation_1(v_c))) {
      e1508 = u_append(e1508, v_c);
    }
    l1509 = u_field(l1509, 1);
  }
  return t1507;
}

u_value f_parse_46_is_notation_1(u_value v_c) {
  u_value s1510 = v_c;
  u_value t1511 = NULL;
  if ((!t1511) && u_is_con(s1510, &n_syntax_46_CNotation)) {
    t1511 = u_mk_bool(1);
  }
  if (!t1511) {
    t1511 = u_mk_bool(0);
  }
  if (!t1511) {
    u_no_case(s1510);
  }
  return t1511;
}

u_value f_parse_46_Context_46_notations_1(u_value v_x) {
  u_value s1512 = v_x;
  u_value t1513 = NULL;
  if ((!t1513) && u_is_con(s1512, &n_parse_46_Context)) {
    u_value v_y = u_field(s1512, 0);
    t1513 = v_y;
  }
  if (!t1513) {
    u_no_case(s1512);
  }
  return t1513;
}

u_value f_parse_46_Context_46_modules_1(u_value v_x) {
  u_value s1514 = v_x;
  u_value t1515 = NULL;
  if ((!t1515) && u_is_con(s1514, &n_parse_46_Context)) {
    u_value v_y = u_field(s1514, 1);
    t1515 = v_y;
  }
  if (!t1515) {
    u_no_case(s1514);
  }
  return t1515;
}

u_value f_parse_46_Context_46_loading_1(u_value v_x) {
  u_value s1516 = v_x;
  u_value t1517 = NULL;
  if ((!t1517) && u_is_con(s1516, &n_parse_46_Context)) {
    u_value v_y = u_field(s1516, 2);
    t1517 = v_y;
  }
  if (!t1517) {
    u_no_case(s1516);
  }
  return t1517;
}

u_value f_parse_46_Context_46_base_1(u_value v_x) {
  u_value s1518 = v_x;
  u_value t1519 = NULL;
  if ((!t1519) && u_is_con(s1518, &n_parse_46_Context)) {
    u_value v_y = u_field(s1518, 3);
    t1519 = v_y;
  }
  if (!t1519) {
    u_no_case(s1518);
  }
  return t1519;
}

u_value f_parse_46_Context_46_dir_1(u_value v_x) {
  u_value s1520 = v_x;
  u_value t1521 = NULL;
  if ((!t1521) && u_is_con(s1520, &n_parse_46_Context)) {
    u_value v_y = u_field(s1520, 4);
    t1521 = v_y;
  }
  if (!t1521) {
    u_no_case(s1520);
  }
  return t1521;
}

u_value f_parse_46_with_commands_2(u_value v_cx, u_value v_cs) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_with_notations_2(f_parse_46_Context_46_notations_1(v_cx), v_cs), f_parse_46_Context_46_modules_1(v_cx), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_with_module_2(u_value v_cx, u_value v_c) {
  return u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_list_46__43__43__2(f_parse_46_modules_of_1(v_c), f_parse_46_Context_46_modules_1(v_cx)), f_parse_46_Context_46_loading_1(v_cx), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
}

u_value f_parse_46_modules_of_1(u_value v_c) {
  u_value s1522 = v_c;
  u_value t1523 = NULL;
  if ((!t1523) && u_is_con(s1522, &n_syntax_46_CModule)) {
    u_value v_n = u_field(s1522, 0);
    u_value v_cs = u_field(s1522, 1);
    u_value t1524 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1525 = &t1524;
    u_value l1526 = v_cs;
    while (u_is_con(l1526, &n_list_46_Cons)) {
      u_value v_d = u_field(l1526, 0);
      if (u_truth(u_mk_bool(1))) {
        e1525 = u_append(e1525, f_parse_46_modules_of_1(v_d));
      }
      l1526 = u_field(l1526, 1);
    }
    t1523 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_parse_46_notations_of_1(v_cs)), f_list_46_concat_1(t1524));
  }
  if (!t1523) {
    t1523 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1523) {
    u_no_case(s1522);
  }
  return t1523;
}

u_value f_parse_46_own_notations_2(u_value v_cx, u_value v_n) {
  u_value s1527 = f_list_46_lookup_2(v_n, f_parse_46_Context_46_modules_1(v_cx));
  u_value t1528 = NULL;
  if ((!t1528) && u_is_con(s1527, &n_option_46_Some)) {
    u_value v_ns = u_field(s1527, 0);
    t1528 = v_ns;
  }
  if ((!t1528) && u_is_con(s1527, &n_option_46_None)) {
    t1528 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1528) {
    u_no_case(s1527);
  }
  return t1528;
}

void p_parse_46_type_params(u_value *r_ps, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_n = NULL;
    u_value t1529 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1529;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(91)))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        u_value t1530 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
        v_rev = t1530;
        u_value t1532 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        while (u_truth(t1532)) {
          {
            p_parse_46_advance(r_ts);
            p_parse_46_parse_name(&v_n, r_ts);
            u_value t1531 = u_mk_con(&n_list_46_Cons, 2, v_n, v_rev);
            v_rev = t1531;
          }
          t1532 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(93)), r_ts);
      }
    }
    u_value t1533 = f_list_46_reverse_1(v_rev);
    *r_ps = t1533;
  }
}

void p_parse_46_parse_notation(u_value *r_c, u_value *r_ts) {
  {
    u_value v_f = NULL;
    u_value v_p = NULL;
    u_value v_ops = NULL;
    u_value v_go = NULL;
    u_value t1534 = u_mk_con(&n_syntax_46_FInfix, 0);
    v_f = t1534;
    u_value s1535 = f_parse_46_head_token_1(*r_ts);
    int d1536 = 0;
    if ((!d1536) && (u_is_con(s1535, &n_lex_46_TName) && u_equals(u_field(s1535, 0), u_mk_lit("prefix")))) {
      d1536 = 1;
      {
        u_value t1537 = u_mk_con(&n_syntax_46_FPrefix, 0);
        v_f = t1537;
      }
    }
    if ((!d1536) && (u_is_con(s1535, &n_lex_46_TName) && u_equals(u_field(s1535, 0), u_mk_lit("postfix")))) {
      d1536 = 1;
      {
        u_value t1538 = u_mk_con(&n_syntax_46_FPostfix, 0);
        v_f = t1538;
      }
    }
    if (!d1536) {
      d1536 = 1;
      {
        u_value s1539 = f_parse_46_second_token_1(*r_ts);
        int d1540 = 0;
        if ((!d1540) && (u_is_con(s1539, &n_lex_46_TName) && u_equals(u_field(s1539, 0), u_mk_lit("left")))) {
          d1540 = 1;
          {
            u_value t1541 = u_mk_con(&n_syntax_46_FLeft, 0);
            v_f = t1541;
            p_parse_46_advance(r_ts);
          }
        }
        if ((!d1540) && (u_is_con(s1539, &n_lex_46_TName) && u_equals(u_field(s1539, 0), u_mk_lit("right")))) {
          d1540 = 1;
          {
            u_value t1542 = u_mk_con(&n_syntax_46_FRight, 0);
            v_f = t1542;
            p_parse_46_advance(r_ts);
          }
        }
        if (!d1540) {
          d1540 = 1;
          {
          }
        }
        if (!d1540) {
          u_no_case(s1539);
        }
      }
    }
    if (!d1536) {
      u_no_case(s1535);
    }
    p_parse_46_advance(r_ts);
    u_value s1543 = f_parse_46_head_token_1(*r_ts);
    int d1544 = 0;
    if ((!d1544) && u_is_con(s1543, &n_lex_46_TNum)) {
      u_value v_n = u_field(s1543, 0);
      d1544 = 1;
      {
        u_value t1545 = v_n;
        v_p = t1545;
        p_parse_46_advance(r_ts);
      }
    }
    if (!d1544) {
      d1544 = 1;
      {
        static struct u_object l_precedence_32_expec_1546 = U_LIT("precedence expected but ");
        static struct u_object l__32_found_1547 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_precedence_32_expec_1546, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1547)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1544) {
      u_no_case(s1543);
    }
    u_value t1548 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1549 = u_mk_bool(1);
    v_ops = t1548;
    v_go = t1549;
    u_value t1555 = v_go;
    while (u_truth(t1555)) {
      {
        u_value s1550 = f_parse_46_head_token_1(*r_ts);
        int d1551 = 0;
        if ((!d1551) && u_is_con(s1550, &n_lex_46_TSym)) {
          u_value v_s = u_field(s1550, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_reserved_1(v_s)))) {
            d1551 = 1;
            {
              u_value t1552 = u_mk_con(&n_list_46_Cons, 2, v_s, v_ops);
              v_ops = t1552;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if ((!d1551) && u_is_con(s1550, &n_lex_46_TName)) {
          u_value v_n = u_field(s1550, 0);
          if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
            d1551 = 1;
            {
              u_value t1553 = u_mk_con(&n_list_46_Cons, 2, v_n, v_ops);
              v_ops = t1553;
              p_parse_46_advance(r_ts);
            }
          }
        }
        if (!d1551) {
          d1551 = 1;
          {
            u_value t1554 = u_mk_bool(0);
            v_go = t1554;
          }
        }
        if (!d1551) {
          u_no_case(s1550);
        }
      }
      t1555 = v_go;
    }
    u_value s1556 = v_ops;
    int d1557 = 0;
    if ((!d1557) && u_is_con(s1556, &n_list_46_Nil)) {
      d1557 = 1;
      {
        static struct u_object l_operator_32_expecte_1558 = U_LIT("operator expected but ");
        static struct u_object l__32_found_1559 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_operator_32_expecte_1558, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1559)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1557) {
      d1557 = 1;
      {
        u_value t1560 = u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, f_list_46_reverse_1(v_ops));
        *r_c = t1560;
      }
    }
    if (!d1557) {
      u_no_case(s1556);
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
    u_value t1561 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1562 = u_mk_con(&n_list_46_Nil, 0);
    v_args = t1561;
    *r_fs = t1562;
    if (u_truth(f_parse_46_at_punct_2(*r_ts, u_mk_char(40)))) {
      {
        p_parse_46_advance(r_ts);
        if (u_truth(f_prelude_46_not_1(f_parse_46_at_punct_2(*r_ts, u_mk_char(41))))) {
          {
            p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
            u_value t1563 = u_mk_con(&n_list_46_Cons, 2, v_t, u_mk_con(&n_list_46_Nil, 0));
            u_value t1564 = u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Nil, 0));
            v_args = t1563;
            *r_fs = t1564;
            u_value t1567 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            while (u_truth(t1567)) {
              {
                p_parse_46_advance(r_ts);
                p_parse_46_field(v_sc, &v_f, &v_t, r_ts);
                u_value t1565 = u_mk_con(&n_list_46_Cons, 2, v_t, v_args);
                u_value t1566 = u_mk_con(&n_list_46_Cons, 2, v_f, *r_fs);
                v_args = t1565;
                *r_fs = t1566;
              }
              t1567 = f_parse_46_at_punct_2(*r_ts, u_mk_char(44));
            }
          }
        }
        p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
      }
    }
    u_value t1568 = u_mk_con(&n_syntax_46_Constr, 2, f_syntax_46_unqualified_1(v_n), f_list_46_reverse_1(v_args));
    u_value t1569 = f_list_46_reverse_1(*r_fs);
    *r_k = t1568;
    *r_fs = t1569;
    u_value s1570 = f_list_46_repeated_1(f_parse_46_field_names_1(*r_fs));
    int d1571 = 0;
    if ((!d1571) && u_is_con(s1570, &n_option_46_Some)) {
      u_value v_f = u_field(s1570, 0);
      d1571 = 1;
      {
        static struct u_object l_repeated_32_field_32__1572 = U_LIT("repeated field ");
        static struct u_object l__32_of_32__1573 = U_LIT(" of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_repeated_32_field_32__1572, u_concat(v_f, u_concat(&l__32_of_32__1573, v_n))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1571) && u_is_con(s1570, &n_option_46_None)) {
      d1571 = 1;
      {
      }
    }
    if (!d1571) {
      u_no_case(s1570);
    }
  }
}

void p_parse_46_field(u_value v_sc, u_value *r_f, u_value *r_t, u_value *r_ts) {
  {
    u_value s1574 = u_mk_tuple(2, f_parse_46_head_token_1(*r_ts), f_parse_46_second_token_1(*r_ts));
    int d1575 = 0;
    if ((!d1575) && (u_is_con(u_field(s1574, 0), &n_lex_46_TName) && (u_is_con(u_field(s1574, 1), &n_lex_46_TSym) && u_equals(u_field(u_field(s1574, 1), 0), u_mk_lit(":"))))) {
      u_value v_n = u_field(u_field(s1574, 0), 0);
      if (u_truth(f_prelude_46_not_1(f_parse_46_is_keyword_1(v_n)))) {
        d1575 = 1;
        {
          p_parse_46_advance(r_ts);
          p_parse_46_advance(r_ts);
          u_value t1576 = u_mk_con(&n_option_46_Some, 1, v_n);
          *r_f = t1576;
        }
      }
    }
    if (!d1575) {
      d1575 = 1;
      {
        u_value t1577 = u_mk_con(&n_option_46_None, 0);
        *r_f = t1577;
      }
    }
    if (!d1575) {
      u_no_case(s1574);
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
    static struct u_object l__58__61__1578 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1578), r_ts);
    static struct u_object l__124__1579 = U_LIT("|");
    if (u_truth(u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1579)))) {
      {
        p_parse_46_advance(r_ts);
      }
    }
    p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
    u_value t1580 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), u_mk_con(&n_list_46_Nil, 0));
    v_rev = t1580;
    static struct u_object l__124__1581 = U_LIT("|");
    u_value t1583 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1581));
    while (u_truth(t1583)) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_constructor(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_k, &v_fs, r_ts);
        u_value t1582 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_k, v_fs), v_rev);
        v_rev = t1582;
      }
      static struct u_object l__124__1584 = U_LIT("|");
      t1583 = u_eq(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TSym, 1, &l__124__1584));
    }
    u_value s1585 = f_parse_46_differing_1(f_parse_46_fields_of_1(f_list_46_reverse_1(v_rev)));
    int d1586 = 0;
    if ((!d1586) && u_is_con(s1585, &n_option_46_Some)) {
      u_value v_f = u_field(s1585, 0);
      d1586 = 1;
      {
        static struct u_object l_field_32__1587 = U_LIT("field ");
        static struct u_object l__32_of_32__1588 = U_LIT(" of ");
        static struct u_object l__32_has_32_different_32_t_1589 = U_LIT(" has different types");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_field_32__1587, u_concat(v_f, u_concat(&l__32_of_32__1588, u_concat(v_n, &l__32_has_32_different_32_t_1589)))), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if ((!d1586) && u_is_con(s1585, &n_option_46_None)) {
      d1586 = 1;
      {
      }
    }
    if (!d1586) {
      u_no_case(s1585);
    }
    u_value t1590 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CData, 3, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_firsts_1(f_list_46_reverse_1(v_rev))), f_parse_46_accessors_3(v_n, v_ps, f_list_46_reverse_1(v_rev)));
    *r_cs = t1590;
  }
}

u_value f_parse_46_field_names_1(u_value v_fs) {
  u_value s1591 = v_fs;
  u_value t1592 = NULL;
  if ((!t1592) && u_is_con(s1591, &n_list_46_Nil)) {
    t1592 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1592) && (u_is_con(s1591, &n_list_46_Cons) && u_is_con(u_field(s1591, 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(s1591, 0), 0);
    u_value v_rest = u_field(s1591, 1);
    t1592 = u_mk_con(&n_list_46_Cons, 2, v_f, f_parse_46_field_names_1(v_rest));
  }
  if ((!t1592) && (u_is_con(s1591, &n_list_46_Cons) && u_is_con(u_field(s1591, 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1591, 1);
    t1592 = f_parse_46_field_names_1(v_rest);
  }
  if (!t1592) {
    u_no_case(s1591);
  }
  return t1592;
}

u_value f_parse_46_differing_1(u_value v_fs) {
  u_value s1593 = v_fs;
  u_value t1594 = NULL;
  if ((!t1594) && u_is_con(s1593, &n_list_46_Nil)) {
    t1594 = u_mk_con(&n_option_46_None, 0);
  }
  if ((!t1594) && u_is_con(s1593, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1593, 0), 0);
    u_value v_t = u_field(u_field(u_field(s1593, 0), 1), 3);
    u_value v_rest = u_field(s1593, 1);
    u_value t1595 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1596 = &t1595;
    u_value l1597 = f_parse_46_fields_named_2(v_f, v_rest);
    while (u_is_con(l1597, &n_list_46_Cons)) {
      u_value v_e = u_field(l1597, 0);
      if (u_truth(u_mk_bool(1))) {
        e1596 = u_append(e1596, f_prelude_46__33__61__2(f_parse_46_type_of_field_1(v_e), v_t));
      }
      l1597 = u_field(l1597, 1);
    }
    u_value t1598;
    if (u_truth(f_list_46_contains_2(u_mk_bool(1), t1595))) {
      t1598 = u_mk_con(&n_option_46_Some, 1, v_f);
    } else {
      t1598 = f_parse_46_differing_1(v_rest);
    }
    t1594 = t1598;
  }
  if (!t1594) {
    u_no_case(s1593);
  }
  return t1594;
}

u_value f_parse_46_type_of_field_1(u_value v_e) {
  u_value s1599 = v_e;
  u_value t1600 = NULL;
  if (!t1600) {
    u_value v_t = u_field(u_field(s1599, 1), 3);
    t1600 = v_t;
  }
  if (!t1600) {
    u_no_case(s1599);
  }
  return t1600;
}

u_value f_parse_46_fields_named_2(u_value v_f, u_value v_fs) {
  u_value s1601 = v_fs;
  u_value t1602 = NULL;
  if ((!t1602) && u_is_con(s1601, &n_list_46_Nil)) {
    t1602 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1602) && u_is_con(s1601, &n_list_46_Cons)) {
    u_value v_g = u_field(u_field(s1601, 0), 0);
    u_value v_d = u_field(u_field(s1601, 0), 1);
    u_value v_rest = u_field(s1601, 1);
    u_value t1603;
    if (u_truth(u_eq(v_g, v_f))) {
      t1603 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_g, v_d), f_parse_46_fields_named_2(v_f, v_rest));
    } else {
      t1603 = f_parse_46_fields_named_2(v_f, v_rest);
    }
    t1602 = t1603;
  }
  if (!t1602) {
    u_no_case(s1601);
  }
  return t1602;
}

u_value f_parse_46_fields_of_1(u_value v_ks) {
  u_value s1604 = v_ks;
  u_value t1605 = NULL;
  if ((!t1605) && u_is_con(s1604, &n_list_46_Nil)) {
    t1605 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1605) && (u_is_con(s1604, &n_list_46_Cons) && u_is_con(u_field(u_field(s1604, 0), 0), &n_syntax_46_Constr))) {
    u_value v_k = u_field(u_field(u_field(s1604, 0), 0), 0);
    u_value v_args = u_field(u_field(u_field(s1604, 0), 0), 1);
    u_value v_fs = u_field(u_field(s1604, 0), 1);
    u_value v_rest = u_field(s1604, 1);
    t1605 = f_list_46__43__43__2(f_parse_46_named_fields_4(v_k, f_list_46_length_1(v_args), f_list_46_zip_2(v_fs, v_args), u_mk_int(0)), f_parse_46_fields_of_1(v_rest));
  }
  if (!t1605) {
    u_no_case(s1604);
  }
  return t1605;
}

u_value f_parse_46_named_fields_4(u_value v_k, u_value v_n, u_value v_fs, u_value v_i) {
  u_value s1606 = v_fs;
  u_value t1607 = NULL;
  if ((!t1607) && u_is_con(s1606, &n_list_46_Nil)) {
    t1607 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1607) && (u_is_con(s1606, &n_list_46_Cons) && u_is_con(u_field(u_field(s1606, 0), 0), &n_option_46_Some))) {
    u_value v_f = u_field(u_field(u_field(s1606, 0), 0), 0);
    u_value v_t = u_field(u_field(s1606, 0), 1);
    u_value v_rest = u_field(s1606, 1);
    t1607 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_f, u_mk_tuple(4, v_k, v_i, v_n, v_t)), f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1))));
  }
  if ((!t1607) && (u_is_con(s1606, &n_list_46_Cons) && u_is_con(u_field(u_field(s1606, 0), 0), &n_option_46_None))) {
    u_value v_rest = u_field(s1606, 1);
    t1607 = f_parse_46_named_fields_4(v_k, v_n, v_rest, u_add(v_i, u_mk_int(1)));
  }
  if (!t1607) {
    u_no_case(s1606);
  }
  return t1607;
}

u_value f_parse_46_accessors_3(u_value v_t, u_value v_ps, u_value v_ks) {
  u_value t1612 = f_parse_46_fields_of_1(v_ks);
  u_value t1611;
  {
    u_value v_fs = t1612;
    u_value t1608 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1609 = &t1608;
    u_value l1610 = f_list_46_distinct_1(f_list_46_firsts_1(v_fs));
    while (u_is_con(l1610, &n_list_46_Cons)) {
      u_value v_f = u_field(l1610, 0);
      if (u_truth(u_mk_bool(1))) {
        e1609 = u_append(e1609, f_parse_46_accessor_5(v_t, v_ps, v_f, f_parse_46_fields_named_2(v_f, v_fs), f_list_46_length_1(v_ks)));
      }
      l1610 = u_field(l1610, 1);
    }
    t1611 = t1608;
  }
  return t1611;
}

u_value f_parse_46_accessor_5(u_value v_t, u_value v_ps, u_value v_f, u_value v_es, u_value v_n) {
  static struct u_object l_x_1613 = U_LIT("x");
  u_value t1614 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1615 = &t1614;
  u_value l1616 = v_ps;
  while (u_is_con(l1616, &n_list_46_Cons)) {
    u_value v_p = u_field(l1616, 0);
    if (u_truth(u_mk_bool(1))) {
      e1615 = u_append(e1615, u_mk_con(&n_syntax_46_TParam, 1, v_p));
    }
    l1616 = u_field(l1616, 1);
  }
  u_value t1624 = u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1613), u_mk_con(&n_syntax_46_THole, 0));
  u_value t1625 = u_mk_con(&n_syntax_46_TSort, 2, f_syntax_46_unqualified_1(v_t), t1614);
  u_value t1623;
  {
    u_value v_x = t1624;
    u_value v_owner = t1625;
    static struct u_object l__46__1617 = U_LIT(".");
    static struct u_object l_x_1618 = U_LIT("x");
    u_value t1619 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1620 = &t1619;
    u_value l1621 = v_es;
    while (u_is_con(l1621, &n_list_46_Cons)) {
      u_value v_e = u_field(l1621, 0);
      if (u_truth(u_mk_bool(1))) {
        e1620 = u_append(e1620, f_parse_46_field_case_1(v_e));
      }
      l1621 = u_field(l1621, 1);
    }
    u_value t1622;
    if (u_truth(u_eq(f_list_46_length_1(v_es), v_n))) {
      t1622 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1622 = u_mk_con(&n_list_46_Cons, 2, f_parse_46_missing_field_2(v_t, v_f), u_mk_con(&n_list_46_Nil, 0));
    }
    t1623 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(u_concat(v_t, u_concat(&l__46__1617, v_f))), v_ps, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_x_1618), v_owner), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), f_parse_46_field_type_1(v_es), u_mk_con(&n_syntax_46_EMatch, 2, u_mk_con(&n_syntax_46_EVar, 1, v_x), f_list_46__43__43__2(t1619, t1622)));
  }
  return t1623;
}

u_value f_parse_46_field_type_1(u_value v_es) {
  u_value s1626 = v_es;
  u_value t1627 = NULL;
  if ((!t1627) && u_is_con(s1626, &n_list_46_Cons)) {
    u_value v_t = u_field(u_field(u_field(s1626, 0), 1), 3);
    t1627 = v_t;
  }
  if (!t1627) {
    t1627 = u_mk_con(&n_syntax_46_THole, 0);
  }
  if (!t1627) {
    u_no_case(s1626);
  }
  return t1627;
}

u_value f_parse_46_field_case_1(u_value v_e) {
  u_value s1628 = v_e;
  u_value t1629 = NULL;
  if (!t1629) {
    u_value v_k = u_field(u_field(s1628, 1), 0);
    u_value v_i = u_field(u_field(s1628, 1), 1);
    u_value v_n = u_field(u_field(s1628, 1), 2);
    u_value t1630 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1631 = &t1630;
    u_value l1632 = f_parse_46_positions_1(v_n);
    while (u_is_con(l1632, &n_list_46_Cons)) {
      u_value v_j = u_field(l1632, 0);
      if (u_truth(u_mk_bool(1))) {
        u_value t1634;
        if (u_truth(u_eq(v_j, v_i))) {
          static struct u_object l_y_1633 = U_LIT("y");
          t1634 = u_mk_con(&n_syntax_46_PVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1633), u_mk_con(&n_syntax_46_THole, 0)));
        } else {
          t1634 = u_mk_con(&n_syntax_46_PWild, 0);
        }
        e1631 = u_append(e1631, t1634);
      }
      l1632 = u_field(l1632, 1);
    }
    static struct u_object l_y_1635 = U_LIT("y");
    t1629 = u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PApp, 2, v_k, t1630), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_y_1635), u_mk_con(&n_syntax_46_THole, 0))));
  }
  if (!t1629) {
    u_no_case(s1628);
  }
  return t1629;
}

u_value f_parse_46_missing_field_2(u_value v_t, u_value v_f) {
  static struct u_object l_MissingField_1636 = U_LIT("MissingField");
  static struct u_object l__46__1637 = U_LIT(".");
  return u_mk_con(&n_syntax_46_Branch, 3, u_mk_con(&n_syntax_46_PWild, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_syntax_46_ERaise, 2, u_mk_con(&n_syntax_46_EApp, 2, u_mk_con(&n_syntax_46_EVar, 1, u_mk_con(&n_syntax_46_Var, 2, f_syntax_46_unqualified_1(&l_MissingField_1636), u_mk_con(&n_syntax_46_THole, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_ELit, 1, u_mk_con(&n_syntax_46_VStr, 1, u_concat(v_t, u_concat(&l__46__1637, v_f)))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_syntax_46_THole, 0)));
}

u_value f_parse_46_positions_1(u_value v_n) {
  return f_parse_46_positions_from_2(u_mk_int(0), v_n);
}

u_value f_parse_46_positions_from_2(u_value v_i, u_value v_n) {
  u_value t1638;
  if (u_truth(u_eq(v_i, v_n))) {
    t1638 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t1638 = u_mk_con(&n_list_46_Cons, 2, v_i, f_parse_46_positions_from_2(u_add(v_i, u_mk_int(1)), v_n));
  }
  return t1638;
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
    u_value t1639 = u_mk_con(&n_list_46_Cons, 2, v_xs, u_mk_con(&n_list_46_Nil, 0));
    v_xss = t1639;
    u_value t1641 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    while (u_truth(t1641)) {
      {
        p_parse_46_parse_formals(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_xs, r_ts);
        u_value t1640 = u_mk_con(&n_list_46_Cons, 2, v_xs, v_xss);
        v_xss = t1640;
      }
      t1641 = f_parse_46_at_punct_2(*r_ts, u_mk_char(40));
    }
    static struct u_object l__58__1642 = U_LIT(":");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__1642), r_ts);
    p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_r, r_ts);
    static struct u_object l__58__61__1643 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1643), r_ts);
    p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1644 = u_mk_con(&n_syntax_46_CFun, 5, f_syntax_46_unqualified_1(v_n), v_ps, f_list_46_reverse_1(v_xss), v_r, v_b);
    *r_c = t1644;
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
    u_value t1645 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1646 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1647 = u_mk_con(&n_list_46_Nil, 0);
    u_value t1648 = u_mk_bool(1);
    v_ins = t1645;
    v_outs = t1646;
    v_refs = t1647;
    v_first = t1648;
    static struct u_object l_in_1649 = U_LIT("in");
    if (u_truth(f_parse_46_at_keyword_2(*r_ts, &l_in_1649))) {
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_ins, r_ts);
        u_value t1650 = u_mk_bool(0);
        v_first = t1650;
      }
    }
    static struct u_object l_out_1651 = U_LIT("out");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_out_1651, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_outs, r_ts);
        u_value t1652 = u_mk_bool(0);
        v_first = t1652;
      }
    }
    static struct u_object l_ref_1653 = U_LIT("ref");
    if (u_truth(f_parse_46_section_at_3(*r_ts, &l_ref_1653, v_first))) {
      {
        p_parse_46_skip_comma(v_first, r_ts);
        p_parse_46_parse_formals1(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_refs, r_ts);
      }
    }
    p_parse_46_expect(u_mk_con(&n_lex_46_TPunct, 1, u_mk_char(41)), r_ts);
    static struct u_object l__58__61__1654 = U_LIT(":=");
    p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1654), r_ts);
    p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_b, r_ts);
    u_value t1655 = u_mk_con(&n_syntax_46_CProc, 6, f_syntax_46_unqualified_1(v_n), v_ps, v_ins, v_outs, v_refs, v_b);
    *r_c = t1655;
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
    u_value t1656 = f_parse_46_Context_46_notations_1(v_cx);
    v_ns = t1656;
    u_value s1657 = f_parse_46_head_token_1(*r_ts);
    int d1658 = 0;
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("import")))) {
      d1658 = 1;
      {
        u_value t1659 = f_parse_46_head_offset_1(*r_ts);
        v_at = t1659;
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        if (u_truth(f_list_46_contains_2(v_n, f_list_46_keys_1(f_parse_46_Context_46_modules_1(v_cx))))) {
          {
            u_value t1660 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0));
            *r_cs = t1660;
          }
        } else {
          {
            p_parse_46_load_module(v_n, v_cx, v_at, r_cs);
            u_value t1661 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, v_n, f_parse_46_declarations_1(*r_cs)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CImport, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
            *r_cs = t1661;
          }
        }
      }
    }
    if ((!d1658) && u_is_con(s1657, &n_lex_46_TName)) {
      u_value v_w = u_field(s1657, 0);
      static struct u_object l_prefix_1663 = U_LIT("prefix");
      static struct u_object l_postfix_1664 = U_LIT("postfix");
      static struct u_object l_infix_1665 = U_LIT("infix");
      if (u_truth(f_list_46_contains_2(v_w, u_mk_con(&n_list_46_Cons, 2, &l_prefix_1663, u_mk_con(&n_list_46_Cons, 2, &l_postfix_1664, u_mk_con(&n_list_46_Cons, 2, &l_infix_1665, u_mk_con(&n_list_46_Nil, 0))))))) {
        d1658 = 1;
        {
          p_parse_46_parse_notation(&v_c, r_ts);
          u_value t1662 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
          *r_cs = t1662;
        }
      }
    }
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("type")))) {
      d1658 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        p_parse_46_type_params(&v_ps, r_ts);
        static struct u_object l__58__61__1666 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1666), r_ts);
        p_parse_46_parse_type(u_mk_con(&n_parse_46_Scope, 2, v_ps, v_ns), &v_t, r_ts);
        u_value t1667 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CSort, 3, f_syntax_46_unqualified_1(v_n), v_ps, v_t), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1667;
      }
    }
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("data")))) {
      d1658 = 1;
      {
        p_parse_46_parse_data(v_ns, r_cs, r_ts);
      }
    }
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("function")))) {
      d1658 = 1;
      {
        p_parse_46_parse_function(v_ns, &v_c, r_ts);
        u_value t1668 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1668;
      }
    }
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("procedure")))) {
      d1658 = 1;
      {
        p_parse_46_parse_procedure(v_ns, &v_c, r_ts);
        u_value t1669 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1669;
      }
    }
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("eval")))) {
      d1658 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_expr(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_e, r_ts);
        u_value t1670 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CEval, 1, v_e), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1670;
      }
    }
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("run")))) {
      d1658 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1671 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1671), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1672 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CRun, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1672;
      }
    }
    if ((!d1658) && (u_is_con(s1657, &n_lex_46_TName) && u_equals(u_field(s1657, 0), u_mk_lit("test")))) {
      d1658 = 1;
      {
        p_parse_46_advance(r_ts);
        p_parse_46_parse_name(&v_n, r_ts);
        static struct u_object l__58__61__1673 = U_LIT(":=");
        p_parse_46_expect(u_mk_con(&n_lex_46_TSym, 1, &l__58__61__1673), r_ts);
        p_parse_46_parse_body(u_mk_con(&n_parse_46_Scope, 2, u_mk_con(&n_list_46_Nil, 0), v_ns), &v_b, r_ts);
        u_value t1674 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CTest, 2, v_n, v_b), u_mk_con(&n_list_46_Nil, 0));
        *r_cs = t1674;
      }
    }
    if (!d1658) {
      d1658 = 1;
      {
        static struct u_object l_declaration_32_expe_1675 = U_LIT("declaration expected but ");
        static struct u_object l__32_found_1676 = U_LIT(" found");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_declaration_32_expe_1675, u_concat(f_parse_46_describe_1(f_parse_46_head_token_1(*r_ts)), &l__32_found_1676)), f_parse_46_head_offset_1(*r_ts)), "parse.ParseError");
      }
    }
    if (!d1658) {
      u_no_case(s1657);
    }
  }
}

void p_parse_46_declare_all(u_value v_cs, u_value *r_cx) {
  {
    u_value s1677 = v_cs;
    int d1678 = 0;
    if ((!d1678) && u_is_con(s1677, &n_list_46_Nil)) {
      d1678 = 1;
      {
      }
    }
    if ((!d1678) && (u_is_con(s1677, &n_list_46_Cons) && u_is_con(u_field(s1677, 0), &n_syntax_46_CNotation))) {
      u_value v_f = u_field(u_field(s1677, 0), 0);
      u_value v_p = u_field(u_field(s1677, 0), 1);
      u_value v_ops = u_field(u_field(s1677, 0), 2);
      u_value v_more = u_field(s1677, 1);
      d1678 = 1;
      {
        u_value t1679 = f_parse_46_with_commands_2(*r_cx, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CNotation, 3, v_f, v_p, v_ops), u_mk_con(&n_list_46_Nil, 0)));
        *r_cx = t1679;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1678) && (u_is_con(s1677, &n_list_46_Cons) && u_is_con(u_field(s1677, 0), &n_syntax_46_CModule))) {
      u_value v_n = u_field(u_field(s1677, 0), 0);
      u_value v_ds = u_field(u_field(s1677, 0), 1);
      u_value v_more = u_field(s1677, 1);
      d1678 = 1;
      {
        u_value t1680 = f_parse_46_with_module_2(*r_cx, u_mk_con(&n_syntax_46_CModule, 2, v_n, v_ds));
        *r_cx = t1680;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1678) && (u_is_con(s1677, &n_list_46_Cons) && u_is_con(u_field(s1677, 0), &n_syntax_46_CImport))) {
      u_value v_n = u_field(u_field(s1677, 0), 0);
      u_value v_more = u_field(s1677, 1);
      d1678 = 1;
      {
        u_value t1681 = f_parse_46_with_commands_2(*r_cx, f_parse_46_own_notations_2(*r_cx, v_n));
        *r_cx = t1681;
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if ((!d1678) && u_is_con(s1677, &n_list_46_Cons)) {
      u_value v_more = u_field(s1677, 1);
      d1678 = 1;
      {
        p_parse_46_declare_all(v_more, r_cx);
      }
    }
    if (!d1678) {
      u_no_case(s1677);
    }
  }
}

void p_parse_46_parse_commands(u_value *r_cs, u_value *r_cx, u_value *r_ts) {
  {
    u_value v_rev = NULL;
    u_value v_more = NULL;
    u_value t1682 = u_mk_con(&n_list_46_Nil, 0);
    v_rev = t1682;
    u_value t1684 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    while (u_truth(t1684)) {
      {
        p_parse_46_parse_command(*r_cx, &v_more, r_ts);
        p_parse_46_declare_all(v_more, r_cx);
        u_value t1683 = f_list_46_reverse_onto_2(v_more, v_rev);
        v_rev = t1683;
      }
      t1684 = f_prelude_46__33__61__2(f_parse_46_head_token_1(*r_ts), u_mk_con(&n_lex_46_TEnd, 0));
    }
    u_value t1685 = f_list_46_reverse_1(v_rev);
    *r_cs = t1685;
  }
}

void p_parse_46_module_text(u_value v_n, u_value v_dir, u_value v_at, u_value *r_text) {
  {
    u_value s1686 = v_dir;
    int d1687 = 0;
    if ((!d1687) && u_is_con(s1686, &n_option_46_Some)) {
      u_value v_d = u_field(s1686, 0);
      d1687 = 1;
      {
        {
          struct u_handler *h1690 = u_push();
          if (setjmp(h1690->env) == 0) {
            {
              static struct u_object l__47__1688 = U_LIT("/");
              static struct u_object l__46_u_1689 = U_LIT(".u");
              p_os_read(u_concat(v_d, u_concat(&l__47__1688, u_concat(v_n, &l__46_u_1689))), r_text);
            }
            u_pop(h1690);
          } else {
            u_pop(h1690);
            int d1691 = 0;
            if ((!d1691) && (u_has_type(h1690, "os.OsError") && u_is_con(u_caught(h1690), &n_os_46_OsError))) {
              d1691 = 1;
              {
                p_parse_46_library_text(v_n, v_at, r_text);
              }
            }
            if (!d1691) {
              u_reraise(h1690);
            }
          }
        }
      }
    }
    if ((!d1687) && u_is_con(s1686, &n_option_46_None)) {
      d1687 = 1;
      {
        p_parse_46_library_text(v_n, v_at, r_text);
      }
    }
    if (!d1687) {
      u_no_case(s1686);
    }
  }
}

void p_parse_46_library_text(u_value v_n, u_value v_at, u_value *r_text) {
  {
    {
      struct u_handler *h1692 = u_push();
      if (setjmp(h1692->env) == 0) {
        {
          p_os_library(v_n, r_text);
        }
        u_pop(h1692);
      } else {
        u_pop(h1692);
        int d1693 = 0;
        if ((!d1693) && (u_has_type(h1692, "os.OsError") && u_is_con(u_caught(h1692), &n_os_46_OsError))) {
          d1693 = 1;
          {
            static struct u_object l_unknown_32_module_32__1694 = U_LIT("unknown module ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_unknown_32_module_32__1694, v_n), v_at), "parse.ParseError");
          }
        }
        if (!d1693) {
          u_reraise(h1692);
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
        static struct u_object l_cyclic_32_import_32_of_1695 = U_LIT("cyclic import of ");
        u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(&l_cyclic_32_import_32_of_1695, v_n), v_at), "parse.ParseError");
      }
    }
    p_parse_46_module_text(v_n, f_parse_46_Context_46_dir_1(v_cx), v_at, &v_text);
    u_value t1696 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_base_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_n, f_parse_46_Context_46_loading_1(v_cx)), f_parse_46_Context_46_base_1(v_cx), f_parse_46_Context_46_dir_1(v_cx));
    v_mcx = t1696;
    {
      struct u_handler *h1697 = u_push();
      if (setjmp(h1697->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_mts);
          p_parse_46_parse_commands(r_cs, &v_mcx, &v_mts);
        }
        u_pop(h1697);
      } else {
        u_pop(h1697);
        int d1698 = 0;
        if ((!d1698) && (u_has_type(h1697, "lex.LexError") && u_is_con(u_caught(h1697), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1697), 0);
          u_value v_i = u_field(u_caught(h1697), 1);
          d1698 = 1;
          {
            static struct u_object l__32_in_32__1699 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1699, v_n)), v_i), "parse.ParseError");
          }
        }
        if ((!d1698) && (u_has_type(h1697, "parse.ParseError") && u_is_con(u_caught(h1697), &n_parse_46_ParseError))) {
          u_value v_m = u_field(u_caught(h1697), 0);
          u_value v_i = u_field(u_caught(h1697), 1);
          d1698 = 1;
          {
            static struct u_object l__32_in_32__1700 = U_LIT(" in ");
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, u_concat(v_m, u_concat(&l__32_in_32__1700, v_n)), v_i), "parse.ParseError");
          }
        }
        if (!d1698) {
          u_reraise(h1697);
        }
      }
    }
  }
}

u_value f_parse_46_base_notations_1(u_value v_cx) {
  u_value s1701 = f_parse_46_Context_46_base_1(v_cx);
  u_value t1702 = NULL;
  if ((!t1702) && u_is_con(s1701, &n_option_46_Some)) {
    u_value v_b = u_field(s1701, 0);
    t1702 = v_b;
  }
  if ((!t1702) && u_is_con(s1701, &n_option_46_None)) {
    t1702 = f_parse_46_Context_46_notations_1(v_cx);
  }
  if (!t1702) {
    u_no_case(s1701);
  }
  return t1702;
}

void p_parse_46_parse_text(u_value v_text, u_value v_cx, u_value *r_p) {
  {
    u_value v_ts = NULL;
    u_value v_cs = NULL;
    u_value v_after = NULL;
    u_value t1703 = v_cx;
    v_after = t1703;
    {
      struct u_handler *h1704 = u_push();
      if (setjmp(h1704->env) == 0) {
        {
          p_lex_46_lex(v_text, &v_ts);
        }
        u_pop(h1704);
      } else {
        u_pop(h1704);
        int d1705 = 0;
        if ((!d1705) && (u_has_type(h1704, "lex.LexError") && u_is_con(u_caught(h1704), &n_lex_46_LexError))) {
          u_value v_m = u_field(u_caught(h1704), 0);
          u_value v_i = u_field(u_caught(h1704), 1);
          d1705 = 1;
          {
            u_raise(u_mk_con(&n_parse_46_ParseError, 2, v_m, v_i), "parse.ParseError");
          }
        }
        if (!d1705) {
          u_reraise(h1704);
        }
      }
    }
    p_parse_46_parse_commands(&v_cs, &v_after, &v_ts);
    u_value t1706 = u_mk_con(&n_syntax_46_Program, 1, v_cs);
    *r_p = t1706;
  }
}

void p_parse_46_module_after(u_value v_name, u_value v_pre, u_value v_dir, u_value v_text, u_value *r_p) {
  {
    u_value v_cx = NULL;
    u_value t1707 = u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0));
    v_cx = t1707;
    p_parse_46_declare_all(f_syntax_46_Program_46_cmds_1(v_pre), &v_cx);
    p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_Context_46_notations_1(v_cx), f_parse_46_Context_46_modules_1(v_cx), u_mk_con(&n_list_46_Cons, 2, v_name, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_Some, 1, f_parse_46_Context_46_notations_1(v_cx)), v_dir), r_p);
  }
}

void p_parse_46_parse_library(u_value v_n, u_value *r_p) {
  {
    u_value v_text = NULL;
    u_value v_pre = NULL;
    p_parse_46_library_text(v_n, u_mk_int(0), &v_text);
    static struct u_object l_prelude_1708 = U_LIT("prelude");
    if (u_truth(u_eq(v_n, &l_prelude_1708))) {
      {
        static struct u_object l_prelude_1709 = U_LIT("prelude");
        p_parse_46_parse_text(v_text, u_mk_con(&n_parse_46_Context, 5, f_parse_46_no_notations_0(), u_mk_con(&n_list_46_Nil, 0), u_mk_con(&n_list_46_Cons, 2, &l_prelude_1709, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_option_46_None, 0), u_mk_con(&n_option_46_None, 0)), r_p);
      }
    } else {
      {
        static struct u_object l_prelude_1710 = U_LIT("prelude");
        p_parse_46_parse_library(&l_prelude_1710, &v_pre);
        p_parse_46_module_after(v_n, v_pre, u_mk_con(&n_option_46_None, 0), v_text, r_p);
      }
    }
  }
}

u_value f_parse_46_last_part_1(u_value v_xs) {
  u_value s1711 = v_xs;
  u_value t1712 = NULL;
  if ((!t1712) && (u_is_con(s1711, &n_list_46_Cons) && u_is_con(u_field(s1711, 1), &n_list_46_Nil))) {
    u_value v_x = u_field(s1711, 0);
    t1712 = v_x;
  }
  if ((!t1712) && u_is_con(s1711, &n_list_46_Cons)) {
    u_value v_more = u_field(s1711, 1);
    t1712 = f_parse_46_last_part_1(v_more);
  }
  if (!t1712) {
    static struct u_object l__1713 = U_LIT("");
    t1712 = &l__1713;
  }
  if (!t1712) {
    u_no_case(s1711);
  }
  return t1712;
}

u_value f_parse_46_base_name_1(u_value v_path) {
  u_value t1718 = f_parse_46_last_part_1(u_str_split(v_path, u_mk_char(47)));
  u_value t1717;
  {
    u_value v_f = t1718;
    u_value t1715 = f_prelude_46__62__61__2(u_str_size(v_f), u_mk_int(2));
    if (u_truth(t1715)) {
      static struct u_object l__46_u_1714 = U_LIT(".u");
      t1715 = u_eq(u_str_slice(v_f, u_sub(u_str_size(v_f), u_mk_int(2)), u_str_size(v_f)), &l__46_u_1714);
    }
    u_value t1716;
    if (u_truth(t1715)) {
      t1716 = u_str_slice(v_f, u_mk_int(0), u_sub(u_str_size(v_f), u_mk_int(2)));
    } else {
      t1716 = v_f;
    }
    t1717 = t1716;
  }
  return t1717;
}

u_value f_parse_46_dir_name_1(u_value v_path) {
  u_value t1723 = u_str_split(v_path, u_mk_char(47));
  u_value t1722;
  {
    u_value v_parts = t1723;
    u_value t1721;
    if (u_truth(u_eq(f_list_46_length_1(v_parts), u_mk_int(1)))) {
      static struct u_object l__46__1719 = U_LIT(".");
      t1721 = &l__46__1719;
    } else {
      static struct u_object l__47__1720 = U_LIT("/");
      t1721 = u_str_join(&l__47__1720, f_list_46_take_2(u_sub(f_list_46_length_1(v_parts), u_mk_int(1)), v_parts));
    }
    t1722 = t1721;
  }
  return t1722;
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
    static struct u_object l_prelude_1724 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_1724, &v_pre);
    p_parse_46_parse_file_after(v_path, v_pre, r_p);
  }
}

void p_parse_46_same_commands(u_value v_path, u_value v_cs, u_value v_ds) {
  {
    u_value s1725 = u_mk_tuple(2, v_cs, v_ds);
    int d1726 = 0;
    if ((!d1726) && (u_is_con(u_field(s1725, 0), &n_list_46_Cons) && u_is_con(u_field(s1725, 1), &n_list_46_Cons))) {
      u_value v_c = u_field(u_field(s1725, 0), 0);
      u_value v_more = u_field(u_field(s1725, 0), 1);
      u_value v_d = u_field(u_field(s1725, 1), 0);
      u_value v_rest = u_field(u_field(s1725, 1), 1);
      d1726 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, v_c), u_mk_tuple(2, v_path, v_d));
        p_parse_46_same_commands(v_path, v_more, v_rest);
      }
    }
    if (!d1726) {
      d1726 = 1;
      {
        u_assert_equal(u_mk_tuple(2, v_path, f_list_46_length_1(v_cs)), u_mk_tuple(2, v_path, f_list_46_length_1(v_ds)));
      }
    }
    if (!d1726) {
      u_no_case(s1725);
    }
  }
}

void p_parse_46_same_parses(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value s1727 = v_paths;
    int d1728 = 0;
    if ((!d1728) && u_is_con(s1727, &n_list_46_Nil)) {
      d1728 = 1;
      {
      }
    }
    if ((!d1728) && u_is_con(s1727, &n_list_46_Cons)) {
      u_value v_path = u_field(s1727, 0);
      u_value v_more = u_field(s1727, 1);
      d1728 = 1;
      {
        p_read(v_path, &v_p);
        p_parse_46_parse_file_after(v_path, v_pre, &v_q);
        p_parse_46_same_commands(v_path, f_syntax_46_Program_46_cmds_1(v_p), f_syntax_46_Program_46_cmds_1(v_q));
        p_parse_46_same_parses(v_more, v_pre);
      }
    }
    if (!d1728) {
      u_no_case(s1727);
    }
  }
}

void p_parse_46_parse_failures(u_value v_paths, u_value v_pre) {
  {
    u_value v_p = NULL;
    u_value v_failed = NULL;
    u_value s1729 = v_paths;
    int d1730 = 0;
    if ((!d1730) && u_is_con(s1729, &n_list_46_Nil)) {
      d1730 = 1;
      {
      }
    }
    if ((!d1730) && u_is_con(s1729, &n_list_46_Cons)) {
      u_value v_path = u_field(s1729, 0);
      u_value v_more = u_field(s1729, 1);
      d1730 = 1;
      {
        u_value t1731 = u_mk_bool(0);
        v_failed = t1731;
        {
          struct u_handler *h1732 = u_push();
          if (setjmp(h1732->env) == 0) {
            {
              p_parse_46_parse_file_after(v_path, v_pre, &v_p);
            }
            u_pop(h1732);
          } else {
            u_pop(h1732);
            int d1733 = 0;
            if ((!d1733) && (u_has_type(h1732, "parse.ParseError") && u_is_con(u_caught(h1732), &n_parse_46_ParseError))) {
              d1733 = 1;
              {
                u_value t1734 = u_mk_bool(1);
                v_failed = t1734;
              }
            }
            if (!d1733) {
              u_reraise(h1732);
            }
          }
        }
        u_assert_equal(u_mk_tuple(2, v_path, v_failed), u_mk_tuple(2, v_path, u_mk_bool(1)));
        p_parse_46_parse_failures(v_more, v_pre);
      }
    }
    if (!d1730) {
      u_no_case(s1729);
    }
  }
}

u_value f_compile_46_Globals_46_funs_1(u_value v_x) {
  u_value s1735 = v_x;
  u_value t1736 = NULL;
  if ((!t1736) && u_is_con(s1735, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1735, 0);
    t1736 = v_y;
  }
  if (!t1736) {
    u_no_case(s1735);
  }
  return t1736;
}

u_value f_compile_46_Globals_46_constrs_1(u_value v_x) {
  u_value s1737 = v_x;
  u_value t1738 = NULL;
  if ((!t1738) && u_is_con(s1737, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1737, 1);
    t1738 = v_y;
  }
  if (!t1738) {
    u_no_case(s1737);
  }
  return t1738;
}

u_value f_compile_46_Globals_46_arities_1(u_value v_x) {
  u_value s1739 = v_x;
  u_value t1740 = NULL;
  if ((!t1740) && u_is_con(s1739, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1739, 2);
    t1740 = v_y;
  }
  if (!t1740) {
    u_no_case(s1739);
  }
  return t1740;
}

u_value f_compile_46_Globals_46_fields_1(u_value v_x) {
  u_value s1741 = v_x;
  u_value t1742 = NULL;
  if ((!t1742) && u_is_con(s1741, &n_compile_46_Globals)) {
    u_value v_y = u_field(s1741, 3);
    t1742 = v_y;
  }
  if (!t1742) {
    u_no_case(s1741);
  }
  return t1742;
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
  static struct u_object l_u_value_1743 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1743);
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
  static struct u_object l_u_value_1744 = U_LIT("u_value");
  u_pop(handler);
  cache = u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_u_value_1744));
  return cache;
}

u_value f_compile_46_mangle_1(u_value v_n) {
  u_value t1745 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1746 = &t1745;
  u_value l1747 = u_chars(f_syntax_46_qualified_1(v_n));
  while (u_is_con(l1747, &n_list_46_Cons)) {
    u_value v_c = u_field(l1747, 0);
    if (u_truth(u_mk_bool(1))) {
      e1746 = u_append(e1746, f_compile_46_mangle_char_1(v_c));
    }
    l1747 = u_field(l1747, 1);
  }
  return u_string(f_list_46_concat_1(t1745));
}

u_value f_compile_46_mangle_char_1(u_value v_c) {
  u_value t1748 = u_is_letter(v_c);
  if (!u_truth(t1748)) {
    t1748 = u_is_digit(v_c);
  }
  u_value t1749 = t1748;
  if (!u_truth(t1749)) {
    t1749 = u_eq(v_c, u_mk_char(95));
  }
  u_value t1752;
  if (u_truth(t1749)) {
    t1752 = u_mk_con(&n_list_46_Cons, 2, v_c, u_mk_con(&n_list_46_Nil, 0));
  } else {
    static struct u_object l___1750 = U_LIT("_");
    static struct u_object l___1751 = U_LIT("_");
    t1752 = u_chars(u_concat(&l___1750, u_concat(u_str_show(u_ord(v_c)), &l___1751)));
  }
  return t1752;
}

u_value f_compile_46_fun_name_2(u_value v_n, u_value v_k) {
  static struct u_object l_f__1753 = U_LIT("f_");
  static struct u_object l___1754 = U_LIT("_");
  return u_concat(&l_f__1753, u_concat(f_compile_46_mangle_1(v_n), u_concat(&l___1754, u_str_show(v_k))));
}

u_value f_compile_46_proc_name_1(u_value v_n) {
  static struct u_object l_p__1755 = U_LIT("p_");
  return u_concat(&l_p__1755, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_var_name_1(u_value v_n) {
  static struct u_object l_v__1756 = U_LIT("v_");
  return u_concat(&l_v__1756, f_compile_46_mangle_1(v_n));
}

u_value f_compile_46_ref_name_1(u_value v_n) {
  static struct u_object l_r__1757 = U_LIT("r_");
  return u_concat(&l_r__1757, f_compile_46_mangle_1(v_n));
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
  static struct u_object l__43__1758 = U_LIT("+");
  static struct u_object l_u_add_1759 = U_LIT("u_add");
  static struct u_object l__45__1760 = U_LIT("-");
  static struct u_object l_u_sub_1761 = U_LIT("u_sub");
  static struct u_object l__42__1762 = U_LIT("*");
  static struct u_object l_u_mul_1763 = U_LIT("u_mul");
  static struct u_object l__47__1764 = U_LIT("/");
  static struct u_object l_u_div_1765 = U_LIT("u_div");
  static struct u_object l__37__1766 = U_LIT("%");
  static struct u_object l_u_mod_1767 = U_LIT("u_mod");
  static struct u_object l__60__1768 = U_LIT("<");
  static struct u_object l_u_lt_1769 = U_LIT("u_lt");
  static struct u_object l__61__61__1770 = U_LIT("==");
  static struct u_object l_u_eq_1771 = U_LIT("u_eq");
  static struct u_object l_compare_1772 = U_LIT("compare");
  static struct u_object l_u_compare_1773 = U_LIT("u_compare");
  static struct u_object l_chars_1774 = U_LIT("chars");
  static struct u_object l_u_chars_1775 = U_LIT("u_chars");
  static struct u_object l_string_1776 = U_LIT("string");
  static struct u_object l_u_string_1777 = U_LIT("u_string");
  static struct u_object l_ord_1778 = U_LIT("ord");
  static struct u_object l_u_ord_1779 = U_LIT("u_ord");
  static struct u_object l_chr_1780 = U_LIT("chr");
  static struct u_object l_u_chr_1781 = U_LIT("u_chr");
  static struct u_object l__94__1782 = U_LIT("^");
  static struct u_object l_u_concat_1783 = U_LIT("u_concat");
  static struct u_object l_str_size_1784 = U_LIT("str_size");
  static struct u_object l_u_str_size_1785 = U_LIT("u_str_size");
  static struct u_object l_str_join_1786 = U_LIT("str_join");
  static struct u_object l_u_str_join_1787 = U_LIT("u_str_join");
  static struct u_object l_str_slice_1788 = U_LIT("str_slice");
  static struct u_object l_u_str_slice_1789 = U_LIT("u_str_slice");
  static struct u_object l_str_split_1790 = U_LIT("str_split");
  static struct u_object l_u_str_split_1791 = U_LIT("u_str_split");
  static struct u_object l_str_get_1792 = U_LIT("str_get");
  static struct u_object l_u_str_get_1793 = U_LIT("u_str_get");
  static struct u_object l_str_contains_1794 = U_LIT("str_contains");
  static struct u_object l_u_str_contains_1795 = U_LIT("u_str_contains");
  static struct u_object l_str_compare_1796 = U_LIT("str_compare");
  static struct u_object l_u_str_compare_1797 = U_LIT("u_str_compare");
  static struct u_object l_str_show_1798 = U_LIT("str_show");
  static struct u_object l_u_str_show_1799 = U_LIT("u_str_show");
  static struct u_object l_str_quote_1800 = U_LIT("str_quote");
  static struct u_object l_u_str_quote_1801 = U_LIT("u_str_quote");
  static struct u_object l_str_number_1802 = U_LIT("str_number");
  static struct u_object l_u_str_number_1803 = U_LIT("u_str_number");
  static struct u_object l_is_digit_1804 = U_LIT("is_digit");
  static struct u_object l_u_is_digit_1805 = U_LIT("u_is_digit");
  static struct u_object l_is_letter_1806 = U_LIT("is_letter");
  static struct u_object l_u_is_letter_1807 = U_LIT("u_is_letter");
  static struct u_object l_is_space_1808 = U_LIT("is_space");
  static struct u_object l_u_is_space_1809 = U_LIT("u_is_space");
  u_pop(handler);
  cache = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__43__1758), u_mk_tuple(2, &l_u_add_1759, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__45__1760), u_mk_tuple(2, &l_u_sub_1761, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__42__1762), u_mk_tuple(2, &l_u_mul_1763, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__47__1764), u_mk_tuple(2, &l_u_div_1765, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__37__1766), u_mk_tuple(2, &l_u_mod_1767, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__60__1768), u_mk_tuple(2, &l_u_lt_1769, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__61__61__1770), u_mk_tuple(2, &l_u_eq_1771, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_compare_1772), u_mk_tuple(2, &l_u_compare_1773, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chars_1774), u_mk_tuple(2, &l_u_chars_1775, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_string_1776), u_mk_tuple(2, &l_u_string_1777, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_ord_1778), u_mk_tuple(2, &l_u_ord_1779, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_chr_1780), u_mk_tuple(2, &l_u_chr_1781, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l__94__1782), u_mk_tuple(2, &l_u_concat_1783, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_size_1784), u_mk_tuple(2, &l_u_str_size_1785, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_join_1786), u_mk_tuple(2, &l_u_str_join_1787, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_slice_1788), u_mk_tuple(2, &l_u_str_slice_1789, u_mk_int(3))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_split_1790), u_mk_tuple(2, &l_u_str_split_1791, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_get_1792), u_mk_tuple(2, &l_u_str_get_1793, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_contains_1794), u_mk_tuple(2, &l_u_str_contains_1795, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_compare_1796), u_mk_tuple(2, &l_u_str_compare_1797, u_mk_int(2))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_show_1798), u_mk_tuple(2, &l_u_str_show_1799, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_quote_1800), u_mk_tuple(2, &l_u_str_quote_1801, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_str_number_1802), u_mk_tuple(2, &l_u_str_number_1803, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_digit_1804), u_mk_tuple(2, &l_u_is_digit_1805, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_letter_1806), u_mk_tuple(2, &l_u_is_letter_1807, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_syntax_46_unqualified_1(&l_is_space_1808), u_mk_tuple(2, &l_u_is_space_1809, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)))))))))))))))))))))))))));
  return cache;
}

u_value f_compile_46_descriptor_1(u_value v_n) {
  static struct u_object l__38__1810 = U_LIT("&");
  static struct u_object l_n__1811 = U_LIT("n_");
  return u_mk_con(&n_c_46_CeUnary, 2, &l__38__1810, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_n__1811, f_compile_46_mangle_1(v_n))));
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
  u_value s1812 = v_n;
  u_value t1813 = NULL;
  if ((!t1813) && (u_is_con(s1812, &n_syntax_46_Name) && u_is_con(u_field(s1812, 0), &n_option_46_Some))) {
    u_value v_m = u_field(u_field(s1812, 0), 0);
    u_value v_b = u_field(s1812, 1);
    static struct u_object l_const_32_struct_32_u_n_1814 = U_LIT("const struct u_name");
    static struct u_object l_n__1815 = U_LIT("n_");
    t1813 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1814), u_concat(&l_n__1815, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_m), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if ((!t1813) && (u_is_con(s1812, &n_syntax_46_Name) && u_is_con(u_field(s1812, 0), &n_option_46_None))) {
    u_value v_b = u_field(s1812, 1);
    static struct u_object l_const_32_struct_32_u_n_1816 = U_LIT("const struct u_name");
    static struct u_object l_n__1817 = U_LIT("n_");
    static struct u_object l_NULL_1818 = U_LIT("NULL");
    t1813 = u_mk_con(&n_c_46_CdGlobal, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_const_32_struct_32_u_n_1816), u_concat(&l_n__1817, f_compile_46_mangle_1(v_n)), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeInit, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1818), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_b), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0)))))));
  }
  if (!t1813) {
    u_no_case(s1812);
  }
  return t1813;
}

u_value f_compile_46_descriptor_decls_1(u_value v_cs) {
  u_value s1819 = v_cs;
  u_value t1820 = NULL;
  if ((!t1820) && u_is_con(s1819, &n_list_46_Nil)) {
    t1820 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1820) && (u_is_con(s1819, &n_list_46_Cons) && u_is_con(u_field(s1819, 0), &n_syntax_46_CData))) {
    u_value v_ks = u_field(u_field(s1819, 0), 2);
    u_value v_rest = u_field(s1819, 1);
    u_value t1821 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1822 = &t1821;
    u_value l1823 = v_ks;
    while (u_is_con(l1823, &n_list_46_Cons)) {
      u_value v_k = u_field(l1823, 0);
      if (u_truth(u_mk_bool(1))) {
        e1822 = u_append(e1822, f_syntax_46_Constr_46_name_1(v_k));
      }
      l1823 = u_field(l1823, 1);
    }
    t1820 = f_list_46__43__43__2(f_compile_46_constructor_decls_2(t1821, u_mk_int(0)), f_compile_46_descriptor_decls_1(v_rest));
  }
  if ((!t1820) && u_is_con(s1819, &n_list_46_Cons)) {
    u_value v_rest = u_field(s1819, 1);
    t1820 = f_compile_46_descriptor_decls_1(v_rest);
  }
  if (!t1820) {
    u_no_case(s1819);
  }
  return t1820;
}

u_value f_compile_46_constructor_decls_2(u_value v_ks, u_value v_i) {
  u_value s1824 = v_ks;
  u_value t1825 = NULL;
  if ((!t1825) && u_is_con(s1824, &n_list_46_Nil)) {
    t1825 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1825) && u_is_con(s1824, &n_list_46_Cons)) {
    u_value v_k = u_field(s1824, 0);
    u_value v_rest = u_field(s1824, 1);
    u_value t1826;
    if (u_truth(f_list_46_contains_2(v_k, f_compile_46_runtime_constructors_0()))) {
      t1826 = f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1)));
    } else {
      t1826 = u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_decl_2(v_k, v_i), f_compile_46_constructor_decls_2(v_rest, u_add(v_i, u_mk_int(1))));
    }
    t1825 = t1826;
  }
  if (!t1825) {
    u_no_case(s1824);
  }
  return t1825;
}

u_value f_compile_46_in_scope_1(u_value v_xs) {
  u_value t1827 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1828 = &t1827;
  u_value l1829 = v_xs;
  while (u_is_con(l1829, &n_list_46_Cons)) {
    u_value v_x = u_field(l1829, 0);
    if (u_truth(u_mk_bool(1))) {
      e1828 = u_append(e1828, u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x))));
    }
    l1829 = u_field(l1829, 1);
  }
  return t1827;
}

u_value f_compile_46_pointer_1(u_value v_x) {
  static struct u_object l__42__1830 = U_LIT("*");
  return u_mk_tuple(2, v_x, u_mk_con(&n_c_46_CeUnary, 2, &l__42__1830, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_ref_name_1(v_x))));
}

u_value f_compile_46_var_expr_2(u_value v_env, u_value v_x) {
  u_value s1831 = f_list_46_lookup_2(v_x, v_env);
  u_value t1832 = NULL;
  if ((!t1832) && u_is_con(s1831, &n_option_46_Some)) {
    u_value v_c = u_field(s1831, 0);
    t1832 = v_c;
  }
  if (!t1832) {
    t1832 = u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x));
  }
  if (!t1832) {
    u_no_case(s1831);
  }
  return t1832;
}

u_value f_compile_46_address_1(u_value v_c) {
  u_value s1833 = v_c;
  u_value t1834 = NULL;
  if ((!t1834) && (u_is_con(s1833, &n_c_46_CeUnary) && u_equals(u_field(s1833, 0), u_mk_lit("*")))) {
    u_value v_p = u_field(s1833, 1);
    t1834 = v_p;
  }
  if (!t1834) {
    static struct u_object l__38__1835 = U_LIT("&");
    t1834 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1835, v_c);
  }
  if (!t1834) {
    u_no_case(s1833);
  }
  return t1834;
}

u_value f_compile_46_type_key_1(u_value v_t) {
  u_value s1836 = v_t;
  u_value t1837 = NULL;
  if ((!t1837) && (u_is_con(s1836, &n_syntax_46_TSort) && u_is_con(u_field(s1836, 1), &n_list_46_Nil))) {
    u_value v_n = u_field(s1836, 0);
    t1837 = f_syntax_46_qualified_1(v_n);
  }
  if ((!t1837) && u_is_con(s1836, &n_syntax_46_TSort)) {
    u_value v_n = u_field(s1836, 0);
    u_value v_args = u_field(s1836, 1);
    static struct u_object l__91__1838 = U_LIT("[");
    static struct u_object l__44__32__1839 = U_LIT(", ");
    u_value t1840 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1841 = &t1840;
    u_value l1842 = v_args;
    while (u_is_con(l1842, &n_list_46_Cons)) {
      u_value v_a = u_field(l1842, 0);
      if (u_truth(u_mk_bool(1))) {
        e1841 = u_append(e1841, f_compile_46_type_key_1(v_a));
      }
      l1842 = u_field(l1842, 1);
    }
    static struct u_object l__93__1843 = U_LIT("]");
    t1837 = u_concat(f_syntax_46_qualified_1(v_n), u_concat(&l__91__1838, u_concat(u_str_join(&l__44__32__1839, t1840), &l__93__1843)));
  }
  if ((!t1837) && u_is_con(s1836, &n_syntax_46_TFun)) {
    u_value v_args = u_field(s1836, 0);
    u_value v_r = u_field(s1836, 1);
    static struct u_object l__40__1844 = U_LIT("(");
    static struct u_object l__44__32__1845 = U_LIT(", ");
    u_value t1846 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1847 = &t1846;
    u_value l1848 = v_args;
    while (u_is_con(l1848, &n_list_46_Cons)) {
      u_value v_a = u_field(l1848, 0);
      if (u_truth(u_mk_bool(1))) {
        e1847 = u_append(e1847, f_compile_46_type_key_1(v_a));
      }
      l1848 = u_field(l1848, 1);
    }
    static struct u_object l__41__32__45__62__32__1849 = U_LIT(") -> ");
    t1837 = u_concat(&l__40__1844, u_concat(u_str_join(&l__44__32__1845, t1846), u_concat(&l__41__32__45__62__32__1849, f_compile_46_type_key_1(v_r))));
  }
  if ((!t1837) && u_is_con(s1836, &n_syntax_46_TTuple)) {
    u_value v_ts = u_field(s1836, 0);
    static struct u_object l__40__1850 = U_LIT("(");
    static struct u_object l__32__42__32__1851 = U_LIT(" * ");
    u_value t1852 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1853 = &t1852;
    u_value l1854 = v_ts;
    while (u_is_con(l1854, &n_list_46_Cons)) {
      u_value v_a = u_field(l1854, 0);
      if (u_truth(u_mk_bool(1))) {
        e1853 = u_append(e1853, f_compile_46_type_key_1(v_a));
      }
      l1854 = u_field(l1854, 1);
    }
    static struct u_object l__41__1855 = U_LIT(")");
    t1837 = u_concat(&l__40__1850, u_concat(u_str_join(&l__32__42__32__1851, t1852), &l__41__1855));
  }
  if (!t1837) {
    static struct u_object l__63__1856 = U_LIT("?");
    t1837 = &l__63__1856;
  }
  if (!t1837) {
    u_no_case(s1836);
  }
  return t1837;
}

u_value f_compile_46_globals_1(u_value v_cs) {
  u_value t1857 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1858 = &t1857;
  u_value l1859 = v_cs;
  while (u_is_con(l1859, &n_list_46_Cons)) {
    u_value v_c = u_field(l1859, 0);
    if (u_truth(u_mk_bool(1))) {
      e1858 = u_append(e1858, f_compile_46_fun_arities_1(v_c));
    }
    l1859 = u_field(l1859, 1);
  }
  u_value t1860 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e1861 = &t1860;
  u_value l1862 = v_cs;
  while (u_is_con(l1862, &n_list_46_Cons)) {
    u_value v_c = u_field(l1862, 0);
    if (u_truth(u_mk_bool(1))) {
      e1861 = u_append(e1861, f_compile_46_constr_arities_1(v_c));
    }
    l1862 = u_field(l1862, 1);
  }
  u_value t1864 = f_list_46_concat_1(t1857);
  u_value t1865 = f_list_46_concat_1(t1860);
  u_value t1863;
  {
    u_value v_fs = t1864;
    u_value v_ks = t1865;
    t1863 = u_mk_con(&n_compile_46_Globals, 4, v_fs, v_ks, f_compile_46_arity_map_2(v_fs, f_map_46_empty_0()), f_map_46_put_all_2(v_ks, f_map_46_empty_0()));
  }
  return t1863;
}

u_value f_compile_46_arity_map_2(u_value v_fs, u_value v_m) {
  u_value s1866 = v_fs;
  u_value t1867 = NULL;
  if ((!t1867) && u_is_con(s1866, &n_list_46_Nil)) {
    t1867 = v_m;
  }
  if ((!t1867) && u_is_con(s1866, &n_list_46_Cons)) {
    u_value v_f = u_field(u_field(s1866, 0), 0);
    u_value v_k = u_field(u_field(s1866, 0), 1);
    u_value v_rest = u_field(s1866, 1);
    u_value s1868 = f_map_46_get_2(v_f, v_m);
    u_value t1869 = NULL;
    if ((!t1869) && u_is_con(s1868, &n_option_46_Some)) {
      u_value v_ks = u_field(s1868, 0);
      t1869 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, v_ks), v_m));
    }
    if ((!t1869) && u_is_con(s1868, &n_option_46_None)) {
      t1869 = f_compile_46_arity_map_2(v_rest, f_map_46_put_3(v_f, u_mk_con(&n_list_46_Cons, 2, v_k, u_mk_con(&n_list_46_Nil, 0)), v_m));
    }
    if (!t1869) {
      u_no_case(s1868);
    }
    t1867 = t1869;
  }
  if (!t1867) {
    u_no_case(s1866);
  }
  return t1867;
}

u_value f_compile_46_is_global_fun_3(u_value v_gl, u_value v_n, u_value v_k) {
  u_value s1870 = v_gl;
  u_value t1871 = NULL;
  if ((!t1871) && u_is_con(s1870, &n_compile_46_Globals)) {
    u_value v_fm = u_field(s1870, 2);
    u_value s1872 = f_map_46_get_2(v_n, v_fm);
    u_value t1873 = NULL;
    if ((!t1873) && u_is_con(s1872, &n_option_46_Some)) {
      u_value v_ks = u_field(s1872, 0);
      t1873 = f_list_46_contains_2(v_k, v_ks);
    }
    if ((!t1873) && u_is_con(s1872, &n_option_46_None)) {
      t1873 = u_mk_bool(0);
    }
    if (!t1873) {
      u_no_case(s1872);
    }
    t1871 = t1873;
  }
  if (!t1871) {
    u_no_case(s1870);
  }
  return t1871;
}

u_value f_compile_46_fields_count_2(u_value v_gl, u_value v_n) {
  u_value s1874 = v_gl;
  u_value t1875 = NULL;
  if ((!t1875) && u_is_con(s1874, &n_compile_46_Globals)) {
    u_value v_km = u_field(s1874, 3);
    t1875 = f_map_46_get_2(v_n, v_km);
  }
  if (!t1875) {
    u_no_case(s1874);
  }
  return t1875;
}

u_value f_compile_46_fun_arities_1(u_value v_c) {
  u_value s1876 = v_c;
  u_value t1877 = NULL;
  if ((!t1877) && u_is_con(s1876, &n_syntax_46_CFun)) {
    u_value v_n = u_field(s1876, 0);
    u_value v_xss = u_field(s1876, 2);
    t1877 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t1877) {
    t1877 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1877) {
    u_no_case(s1876);
  }
  return t1877;
}

u_value f_compile_46_constr_arities_1(u_value v_c) {
  u_value s1878 = v_c;
  u_value t1879 = NULL;
  if ((!t1879) && u_is_con(s1878, &n_syntax_46_CData)) {
    u_value v_ks = u_field(s1878, 2);
    u_value t1880 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1881 = &t1880;
    u_value l1882 = v_ks;
    while (u_is_con(l1882, &n_list_46_Cons)) {
      u_value v_k = u_field(l1882, 0);
      if (u_truth(u_mk_bool(1))) {
        e1881 = u_append(e1881, f_compile_46_constr_arity_1(v_k));
      }
      l1882 = u_field(l1882, 1);
    }
    t1879 = t1880;
  }
  if (!t1879) {
    t1879 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1879) {
    u_no_case(s1878);
  }
  return t1879;
}

u_value f_compile_46_constr_arity_1(u_value v_k) {
  u_value s1883 = v_k;
  u_value t1884 = NULL;
  if ((!t1884) && u_is_con(s1883, &n_syntax_46_Constr)) {
    u_value v_n = u_field(s1883, 0);
    u_value v_ts = u_field(s1883, 1);
    t1884 = u_mk_tuple(2, v_n, f_list_46_length_1(v_ts));
  }
  if (!t1884) {
    u_no_case(s1883);
  }
  return t1884;
}

u_value f_compile_46_numbered_2(u_value v_xs, u_value v_i) {
  u_value s1885 = v_xs;
  u_value t1886 = NULL;
  if ((!t1886) && u_is_con(s1885, &n_list_46_Nil)) {
    t1886 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1886) && u_is_con(s1885, &n_list_46_Cons)) {
    u_value v_x = u_field(s1885, 0);
    u_value v_rest = u_field(s1885, 1);
    t1886 = u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, v_x, v_i), f_compile_46_numbered_2(v_rest, u_add(v_i, u_mk_int(1))));
  }
  if (!t1886) {
    u_no_case(s1885);
  }
  return t1886;
}

u_value f_compile_46_index_2(u_value v_a, u_value v_i) {
  return u_mk_con(&n_c_46_CeIndex, 2, u_mk_con(&n_c_46_CeVar, 1, v_a), u_mk_con(&n_c_46_CeNum, 1, v_i));
}

u_value f_compile_46_field_2(u_value v_v, u_value v_i) {
  static struct u_object l_u_field_1887 = U_LIT("u_field");
  return f_c_46_c_call_2(&l_u_field_1887, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_i), u_mk_con(&n_list_46_Nil, 0))));
}

u_value f_compile_46_truth_1(u_value v_v) {
  static struct u_object l_u_truth_1888 = U_LIT("u_truth");
  return f_c_46_c_call_2(&l_u_truth_1888, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_conjunction_1(u_value v_cs) {
  u_value s1889 = v_cs;
  u_value t1890 = NULL;
  if ((!t1890) && u_is_con(s1889, &n_list_46_Nil)) {
    t1890 = u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1));
  }
  if ((!t1890) && (u_is_con(s1889, &n_list_46_Cons) && u_is_con(u_field(s1889, 1), &n_list_46_Nil))) {
    u_value v_d = u_field(s1889, 0);
    t1890 = v_d;
  }
  if ((!t1890) && u_is_con(s1889, &n_list_46_Cons)) {
    u_value v_d = u_field(s1889, 0);
    u_value v_rest = u_field(s1889, 1);
    static struct u_object l__38__38__1891 = U_LIT("&&");
    t1890 = u_mk_con(&n_c_46_CeBinary, 3, &l__38__38__1891, v_d, f_compile_46_conjunction_1(v_rest));
  }
  if (!t1890) {
    u_no_case(s1889);
  }
  return t1890;
}

u_value f_compile_46_literal_value_1(u_value v_l) {
  u_value s1892 = v_l;
  u_value t1893 = NULL;
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_VUnit)) {
    static struct u_object l_u_mk_tuple_1894 = U_LIT("u_mk_tuple");
    t1893 = f_c_46_c_call_2(&l_u_mk_tuple_1894, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_VBool)) {
    u_value v_b = u_field(s1892, 0);
    static struct u_object l_u_mk_bool_1895 = U_LIT("u_mk_bool");
    u_value t1896;
    if (u_truth(v_b)) {
      t1896 = u_mk_int(1);
    } else {
      t1896 = u_mk_int(0);
    }
    t1893 = f_c_46_c_call_2(&l_u_mk_bool_1895, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, t1896), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_VInt)) {
    u_value v_n = u_field(s1892, 0);
    u_value t1897 = u_lt(f_prelude_46__45__1(u_mk_int_str("1000000000000000000")), v_n);
    if (u_truth(t1897)) {
      t1897 = u_lt(v_n, u_mk_int_str("1000000000000000000"));
    }
    u_value t1900;
    if (u_truth(t1897)) {
      static struct u_object l_u_mk_int_1898 = U_LIT("u_mk_int");
      t1900 = f_c_46_c_call_2(&l_u_mk_int_1898, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_n), u_mk_con(&n_list_46_Nil, 0)));
    } else {
      static struct u_object l_u_mk_int_str_1899 = U_LIT("u_mk_int_str");
      t1900 = f_c_46_c_call_2(&l_u_mk_int_str_1899, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_str_show(v_n)), u_mk_con(&n_list_46_Nil, 0)));
    }
    t1893 = t1900;
  }
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_VStr)) {
    u_value v_s = u_field(s1892, 0);
    static struct u_object l_u_mk_lit_1901 = U_LIT("u_mk_lit");
    t1893 = f_c_46_c_call_2(&l_u_mk_lit_1901, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0)));
  }
  if ((!t1893) && u_is_con(s1892, &n_syntax_46_VChar)) {
    u_value v_c = u_field(s1892, 0);
    static struct u_object l_u_mk_char_1902 = U_LIT("u_mk_char");
    t1893 = f_c_46_c_call_2(&l_u_mk_char_1902, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_ord(v_c)), u_mk_con(&n_list_46_Nil, 0)));
  }
  if (!t1893) {
    u_no_case(s1892);
  }
  return t1893;
}

u_value f_compile_46_pattern_tests_2(u_value v_p, u_value v_v) {
  u_value s1903 = v_p;
  u_value t1904 = NULL;
  if ((!t1904) && u_is_con(s1903, &n_syntax_46_PLit)) {
    u_value v_l = u_field(s1903, 0);
    static struct u_object l_u_equals_1905 = U_LIT("u_equals");
    t1904 = u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_equals_1905, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_literal_value_1(v_l), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1904) && u_is_con(s1903, &n_syntax_46_PApp)) {
    u_value v_n = u_field(s1903, 0);
    u_value v_ps = u_field(s1903, 1);
    static struct u_object l_u_is_con_1906 = U_LIT("u_is_con");
    t1904 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_is_con_1906, u_mk_con(&n_list_46_Cons, 2, v_v, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v));
  }
  if ((!t1904) && u_is_con(s1903, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1903, 0);
    t1904 = f_compile_46_field_tests_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1904) {
    t1904 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1904) {
    u_no_case(s1903);
  }
  return t1904;
}

u_value f_compile_46_field_tests_2(u_value v_ps, u_value v_v) {
  u_value s1907 = v_ps;
  u_value t1908 = NULL;
  if ((!t1908) && u_is_con(s1907, &n_list_46_Nil)) {
    t1908 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1908) && u_is_con(s1907, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1907, 0), 0);
    u_value v_i = u_field(u_field(s1907, 0), 1);
    u_value v_rest = u_field(s1907, 1);
    t1908 = f_list_46__43__43__2(f_compile_46_pattern_tests_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_tests_2(v_rest, v_v));
  }
  if (!t1908) {
    u_no_case(s1907);
  }
  return t1908;
}

u_value f_compile_46_pattern_binds_2(u_value v_p, u_value v_v) {
  u_value s1909 = v_p;
  u_value t1910 = NULL;
  if ((!t1910) && (u_is_con(s1909, &n_syntax_46_PVar) && u_is_con(u_field(s1909, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1909, 0), 0);
    t1910 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_n), u_mk_con(&n_option_46_Some, 1, v_v)), u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1910) && u_is_con(s1909, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1909, 1);
    t1910 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if ((!t1910) && u_is_con(s1909, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1909, 0);
    t1910 = f_compile_46_field_binds_2(f_compile_46_numbered_2(v_ps, u_mk_int(0)), v_v);
  }
  if (!t1910) {
    t1910 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1910) {
    u_no_case(s1909);
  }
  return t1910;
}

u_value f_compile_46_field_binds_2(u_value v_ps, u_value v_v) {
  u_value s1911 = v_ps;
  u_value t1912 = NULL;
  if ((!t1912) && u_is_con(s1911, &n_list_46_Nil)) {
    t1912 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1912) && u_is_con(s1911, &n_list_46_Cons)) {
    u_value v_p = u_field(u_field(s1911, 0), 0);
    u_value v_i = u_field(u_field(s1911, 0), 1);
    u_value v_rest = u_field(s1911, 1);
    t1912 = f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, f_compile_46_field_2(v_v, v_i)), f_compile_46_field_binds_2(v_rest, v_v));
  }
  if (!t1912) {
    u_no_case(s1911);
  }
  return t1912;
}

u_value f_compile_46_pattern_names_1(u_value v_p) {
  u_value s1913 = v_p;
  u_value t1914 = NULL;
  if ((!t1914) && (u_is_con(s1913, &n_syntax_46_PVar) && u_is_con(u_field(s1913, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1913, 0), 0);
    t1914 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
  }
  if ((!t1914) && u_is_con(s1913, &n_syntax_46_PApp)) {
    u_value v_ps = u_field(s1913, 1);
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
    t1914 = f_list_46_concat_1(t1915);
  }
  if ((!t1914) && u_is_con(s1913, &n_syntax_46_PTuple)) {
    u_value v_ps = u_field(s1913, 0);
    u_value t1918 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1919 = &t1918;
    u_value l1920 = v_ps;
    while (u_is_con(l1920, &n_list_46_Cons)) {
      u_value v_q = u_field(l1920, 0);
      if (u_truth(u_mk_bool(1))) {
        e1919 = u_append(e1919, f_compile_46_pattern_names_1(v_q));
      }
      l1920 = u_field(l1920, 1);
    }
    t1914 = f_list_46_concat_1(t1918);
  }
  if (!t1914) {
    t1914 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t1914) {
    u_no_case(s1913);
  }
  return t1914;
}

u_value f_compile_46_free_vars_2(u_value v_e, u_value v_bound) {
  u_value s1921 = v_e;
  u_value t1922 = NULL;
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_ELit)) {
    t1922 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t1922) && (u_is_con(s1921, &n_syntax_46_EVar) && u_is_con(u_field(s1921, 0), &n_syntax_46_Var))) {
    u_value v_n = u_field(u_field(s1921, 0), 0);
    u_value t1923;
    if (u_truth(f_list_46_contains_2(v_n, v_bound))) {
      t1923 = u_mk_con(&n_list_46_Nil, 0);
    } else {
      t1923 = u_mk_con(&n_list_46_Cons, 2, v_n, u_mk_con(&n_list_46_Nil, 0));
    }
    t1922 = t1923;
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_EApp)) {
    u_value v_f = u_field(s1921, 0);
    u_value v_args = u_field(s1921, 1);
    u_value t1924 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1925 = &t1924;
    u_value l1926 = v_args;
    while (u_is_con(l1926, &n_list_46_Cons)) {
      u_value v_a = u_field(l1926, 0);
      if (u_truth(u_mk_bool(1))) {
        e1925 = u_append(e1925, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1926 = u_field(l1926, 1);
    }
    t1922 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_f, v_bound), f_list_46_concat_1(t1924));
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_ELam)) {
    u_value v_xs = u_field(s1921, 0);
    u_value v_b = u_field(s1921, 1);
    t1922 = f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound));
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_ELet)) {
    u_value v_bs = u_field(s1921, 0);
    u_value v_b = u_field(s1921, 1);
    u_value t1927 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1928 = &t1927;
    u_value l1929 = f_list_46_seconds_1(v_bs);
    while (u_is_con(l1929, &n_list_46_Cons)) {
      u_value v_a = u_field(l1929, 0);
      if (u_truth(u_mk_bool(1))) {
        e1928 = u_append(e1928, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1929 = u_field(l1929, 1);
    }
    t1922 = f_list_46__43__43__2(f_list_46_concat_1(t1927), f_compile_46_free_vars_2(v_b, f_list_46__43__43__2(f_syntax_46_names_1(f_list_46_firsts_1(v_bs)), v_bound)));
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_EIte)) {
    u_value v_c = u_field(s1921, 0);
    u_value v_l = u_field(s1921, 1);
    u_value v_r = u_field(s1921, 2);
    t1922 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_c, v_bound), f_list_46__43__43__2(f_compile_46_free_vars_2(v_l, v_bound), f_compile_46_free_vars_2(v_r, v_bound)));
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_EMatch)) {
    u_value v_a = u_field(s1921, 0);
    u_value v_cs = u_field(s1921, 1);
    u_value t1930 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1931 = &t1930;
    u_value l1932 = v_cs;
    while (u_is_con(l1932, &n_list_46_Cons)) {
      u_value v_b = u_field(l1932, 0);
      if (u_truth(u_mk_bool(1))) {
        e1931 = u_append(e1931, f_compile_46_free_in_branch_2(v_b, v_bound));
      }
      l1932 = u_field(l1932, 1);
    }
    t1922 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_a, v_bound), f_list_46_concat_1(t1930));
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_EComp)) {
    u_value v_r = u_field(s1921, 0);
    u_value v_xs = u_field(s1921, 1);
    u_value v_ls = u_field(s1921, 2);
    u_value v_c = u_field(s1921, 3);
    u_value t1933 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1934 = &t1933;
    u_value l1935 = v_ls;
    while (u_is_con(l1935, &n_list_46_Cons)) {
      u_value v_l = u_field(l1935, 0);
      if (u_truth(u_mk_bool(1))) {
        e1934 = u_append(e1934, f_compile_46_free_vars_2(v_l, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)));
      }
      l1935 = u_field(l1935, 1);
    }
    t1922 = f_list_46__43__43__2(f_list_46_concat_1(t1933), f_list_46__43__43__2(f_compile_46_free_vars_2(v_r, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound)), f_compile_46_free_vars_2(v_c, f_list_46__43__43__2(f_syntax_46_names_1(v_xs), v_bound))));
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_ETuple)) {
    u_value v_es = u_field(s1921, 0);
    u_value t1936 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e1937 = &t1936;
    u_value l1938 = v_es;
    while (u_is_con(l1938, &n_list_46_Cons)) {
      u_value v_a = u_field(l1938, 0);
      if (u_truth(u_mk_bool(1))) {
        e1937 = u_append(e1937, f_compile_46_free_vars_2(v_a, v_bound));
      }
      l1938 = u_field(l1938, 1);
    }
    t1922 = f_list_46_concat_1(t1936);
  }
  if ((!t1922) && u_is_con(s1921, &n_syntax_46_ERaise)) {
    u_value v_a = u_field(s1921, 0);
    t1922 = f_compile_46_free_vars_2(v_a, v_bound);
  }
  if (!t1922) {
    u_no_case(s1921);
  }
  return t1922;
}

u_value f_compile_46_free_in_branch_2(u_value v_b, u_value v_bound) {
  u_value s1939 = v_b;
  u_value t1940 = NULL;
  if ((!t1940) && (u_is_con(s1939, &n_syntax_46_Branch) && u_is_con(u_field(s1939, 1), &n_option_46_Some))) {
    u_value v_p = u_field(s1939, 0);
    u_value v_g = u_field(u_field(s1939, 1), 0);
    u_value v_e = u_field(s1939, 2);
    t1940 = f_list_46__43__43__2(f_compile_46_free_vars_2(v_g, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)), f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound)));
  }
  if ((!t1940) && (u_is_con(s1939, &n_syntax_46_Branch) && u_is_con(u_field(s1939, 1), &n_option_46_None))) {
    u_value v_p = u_field(s1939, 0);
    u_value v_e = u_field(s1939, 2);
    t1940 = f_compile_46_free_vars_2(v_e, f_list_46__43__43__2(f_compile_46_pattern_names_1(v_p), v_bound));
  }
  if (!t1940) {
    u_no_case(s1939);
  }
  return t1940;
}

u_value f_compile_46_lambdas_2(u_value v_xss, u_value v_b) {
  u_value s1941 = v_xss;
  u_value t1942 = NULL;
  if ((!t1942) && u_is_con(s1941, &n_list_46_Nil)) {
    t1942 = v_b;
  }
  if ((!t1942) && u_is_con(s1941, &n_list_46_Cons)) {
    u_value v_xs = u_field(s1941, 0);
    u_value v_rest = u_field(s1941, 1);
    t1942 = u_mk_con(&n_syntax_46_ELam, 2, v_xs, f_compile_46_lambdas_2(v_rest, v_b));
  }
  if (!t1942) {
    u_no_case(s1941);
  }
  return t1942;
}

void p_compile_46_gen_name(u_value v_stem, u_value *r_x, u_value *r_g) {
  {
    u_value s1943 = *r_g;
    int d1944 = 0;
    if ((!d1944) && u_is_con(s1943, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1943, 0);
      u_value v_ds = u_field(s1943, 1);
      d1944 = 1;
      {
        u_value t1945 = u_concat(v_stem, u_str_show(v_n));
        u_value t1946 = u_mk_con(&n_compile_46_Gen, 2, u_add(v_n, u_mk_int(1)), v_ds);
        *r_x = t1945;
        *r_g = t1946;
      }
    }
    if (!d1944) {
      u_no_case(s1943);
    }
  }
}

void p_compile_46_lift(u_value v_d, u_value *r_g) {
  {
    u_value s1947 = *r_g;
    int d1948 = 0;
    if ((!d1948) && u_is_con(s1947, &n_compile_46_Gen)) {
      u_value v_n = u_field(s1947, 0);
      u_value v_ds = u_field(s1947, 1);
      d1948 = 1;
      {
        u_value t1949 = u_mk_con(&n_compile_46_Gen, 2, v_n, f_list_46__43__43__2(v_ds, u_mk_con(&n_list_46_Cons, 2, v_d, u_mk_con(&n_list_46_Nil, 0))));
        *r_g = t1949;
      }
    }
    if (!d1948) {
      u_no_case(s1947);
    }
  }
}

void p_compile_46_global_value(u_value v_gl, u_value v_n, u_value v_t, u_value *r_r) {
  {
    if (u_truth(f_compile_46_is_global_fun_3(v_gl, v_n, f_syntax_46_arity_1(v_t)))) {
      {
        static struct u_object l_u_mk_closure_1950 = U_LIT("u_mk_closure");
        static struct u_object l_w__1951 = U_LIT("w_");
        u_value t1952 = f_c_46_c_call_2(&l_u_mk_closure_1950, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w__1951, f_compile_46_fun_name_2(v_n, f_syntax_46_arity_1(v_t)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
        *r_r = t1952;
      }
    } else {
      {
        u_value s1953 = f_compile_46_fields_count_2(v_gl, v_n);
        int d1954 = 0;
        if ((!d1954) && (u_is_con(s1953, &n_option_46_Some) && u_equals(u_field(s1953, 0), u_mk_int(0)))) {
          d1954 = 1;
          {
            static struct u_object l_u_mk_con_1955 = U_LIT("u_mk_con");
            u_value t1956 = f_c_46_c_call_2(&l_u_mk_con_1955, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1956;
          }
        }
        if ((!d1954) && u_is_con(s1953, &n_option_46_Some)) {
          d1954 = 1;
          {
            static struct u_object l_u_mk_closure_1957 = U_LIT("u_mk_closure");
            static struct u_object l_wc__1958 = U_LIT("wc_");
            u_value t1959 = f_c_46_c_call_2(&l_u_mk_closure_1957, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_wc__1958, f_compile_46_mangle_1(v_n))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
            *r_r = t1959;
          }
        }
        if ((!d1954) && u_is_con(s1953, &n_option_46_None)) {
          d1954 = 1;
          {
            u_value s1960 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
            int d1961 = 0;
            if ((!d1961) && u_is_con(s1960, &n_option_46_Some)) {
              u_value v_f = u_field(u_field(s1960, 0), 0);
              d1961 = 1;
              {
                static struct u_object l_u_mk_closure_1962 = U_LIT("u_mk_closure");
                static struct u_object l_w_1963 = U_LIT("w");
                u_value t1964 = f_c_46_c_call_2(&l_u_mk_closure_1962, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, u_concat(&l_w_1963, v_f)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0))));
                *r_r = t1964;
              }
            }
            if ((!d1961) && u_is_con(s1960, &n_option_46_None)) {
              d1961 = 1;
              {
                static struct u_object l_the_32_variable_32__1965 = U_LIT("the variable ");
                u_raise(u_mk_con(&n_compile_46_Unsupported, 1, u_concat(&l_the_32_variable_32__1965, f_syntax_46_qualified_1(v_n))), "compile.CompileError");
              }
            }
            if (!d1961) {
              u_no_case(s1960);
            }
          }
        }
        if (!d1954) {
          u_no_case(s1953);
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
    u_value s1966 = v_e;
    int d1967 = 0;
    if ((!d1967) && (u_is_con(s1966, &n_syntax_46_ELit) && u_is_con(u_field(s1966, 0), &n_syntax_46_VStr))) {
      u_value v_s = u_field(u_field(s1966, 0), 0);
      d1967 = 1;
      {
        static struct u_object l_l__1968 = U_LIT("l_");
        u_value t1969;
        if (u_truth(u_lt(u_str_size(v_s), u_mk_int(16)))) {
          t1969 = u_str_size(v_s);
        } else {
          t1969 = u_mk_int(16);
        }
        static struct u_object l___1970 = U_LIT("_");
        p_compile_46_gen_name(u_concat(&l_l__1968, u_concat(f_compile_46_mangle_1(f_syntax_46_unqualified_1(u_str_slice(v_s, u_mk_int(0), t1969))), &l___1970)), &v_t, r_g);
        static struct u_object l_static_32_struct_32_u__1971 = U_LIT("static struct u_object");
        static struct u_object l_U_LIT_1972 = U_LIT("U_LIT");
        static struct u_object l__38__1973 = U_LIT("&");
        u_value t1974 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_struct_32_u__1971), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_U_LIT_1972, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, v_s), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0));
        u_value t1975 = u_mk_con(&n_c_46_CeUnary, 2, &l__38__1973, u_mk_con(&n_c_46_CeVar, 1, v_t));
        *r_ss = t1974;
        *r_r = t1975;
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_ELit)) {
      u_value v_l = u_field(s1966, 0);
      d1967 = 1;
      {
        u_value t1976 = u_mk_con(&n_list_46_Nil, 0);
        u_value t1977 = f_compile_46_literal_value_1(v_l);
        *r_ss = t1976;
        *r_r = t1977;
      }
    }
    if ((!d1967) && (u_is_con(s1966, &n_syntax_46_EVar) && u_is_con(u_field(s1966, 0), &n_syntax_46_Var))) {
      u_value v_n = u_field(u_field(s1966, 0), 0);
      u_value v_ty = u_field(u_field(s1966, 0), 1);
      d1967 = 1;
      {
        u_value s1978 = f_list_46_lookup_2(v_n, v_env);
        int d1979 = 0;
        if ((!d1979) && u_is_con(s1978, &n_option_46_Some)) {
          u_value v_c = u_field(s1978, 0);
          d1979 = 1;
          {
            u_value t1980 = u_mk_con(&n_list_46_Nil, 0);
            u_value t1981 = v_c;
            *r_ss = t1980;
            *r_r = t1981;
          }
        }
        if ((!d1979) && u_is_con(s1978, &n_option_46_None)) {
          d1979 = 1;
          {
            p_compile_46_global_value(v_gl, v_n, v_ty, r_r);
            u_value t1982 = u_mk_con(&n_list_46_Nil, 0);
            *r_ss = t1982;
          }
        }
        if (!d1979) {
          u_no_case(s1978);
        }
      }
    }
    if ((!d1967) && (u_is_con(s1966, &n_syntax_46_EApp) && (u_is_con(u_field(s1966, 0), &n_syntax_46_EVar) && u_is_con(u_field(u_field(s1966, 0), 0), &n_syntax_46_Var)))) {
      u_value v_n = u_field(u_field(u_field(s1966, 0), 0), 0);
      u_value v_ty = u_field(u_field(u_field(s1966, 0), 0), 1);
      u_value v_args = u_field(s1966, 1);
      if (u_truth(f_prelude_46_not_1(f_list_46_contains_2(v_n, f_list_46_keys_1(v_env))))) {
        d1967 = 1;
        {
          p_compile_46_compile_named(v_gl, v_env, v_n, v_ty, v_args, r_ss, r_r, r_g);
        }
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_EApp)) {
      u_value v_f = u_field(s1966, 0);
      u_value v_args = u_field(s1966, 1);
      d1967 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_f, &v_as, &v_ra, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_args, &v_bs, &v_rs, r_g);
        static struct u_object l_u_apply_1983 = U_LIT("u_apply");
        u_value t1984 = f_list_46__43__43__2(v_as, v_bs);
        u_value t1985 = f_c_46_c_call_2(&l_u_apply_1983, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
        *r_ss = t1984;
        *r_r = t1985;
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_ELam)) {
      u_value v_xs = u_field(s1966, 0);
      u_value v_b = u_field(s1966, 1);
      d1967 = 1;
      {
        p_compile_46_compile_lambda(v_gl, v_env, v_xs, v_b, r_ss, r_r, r_g);
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_ELet)) {
      u_value v_binds = u_field(s1966, 0);
      u_value v_b = u_field(s1966, 1);
      d1967 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_binds), &v_as, &v_rs, r_g);
        p_compile_46_compile_expr(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(f_list_46_firsts_1(v_binds))), v_env), v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1986 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1986, &v_t, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t1987 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(f_compile_46_rebind_2(f_syntax_46_names_1(f_list_46_firsts_1(v_binds)), v_cs), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))));
        *r_ss = t1987;
        u_value t1988 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1988;
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_EIte)) {
      u_value v_c = u_field(s1966, 0);
      u_value v_l = u_field(s1966, 1);
      u_value v_rr = u_field(s1966, 2);
      d1967 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_l, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_rr, &v_bs, &v_rb, r_g);
        static struct u_object l_t_1989 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1989, &v_t, r_g);
        u_value t1990 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_None, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t1990;
        u_value t1991 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1991;
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_EMatch)) {
      u_value v_arg = u_field(s1966, 0);
      u_value v_cases = u_field(s1966, 1);
      d1967 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_1992 = U_LIT("s");
        p_compile_46_gen_name(&l_s_1992, &v_d, r_g);
        static struct u_object l_t_1993 = U_LIT("t");
        p_compile_46_gen_name(&l_t_1993, &v_t, r_g);
        p_compile_46_expr_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        static struct u_object l_NULL_1994 = U_LIT("NULL");
        u_value t1995 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_1994))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t1995;
        static struct u_object l__33__1996 = U_LIT("!");
        static struct u_object l_u_no_case_1997 = U_LIT("u_no_case");
        u_value t1998 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__1996, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_1997, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t1998;
        u_value t1999 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t1999;
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_EComp)) {
      u_value v_result = u_field(s1966, 0);
      u_value v_xs = u_field(s1966, 1);
      u_value v_lists = u_field(s1966, 2);
      u_value v_c = u_field(s1966, 3);
      d1967 = 1;
      {
        static struct u_object l_t_2000 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2000, &v_t, r_g);
        static struct u_object l_e_2001 = U_LIT("e");
        p_compile_46_gen_name(&l_e_2001, &v_d, r_g);
        p_compile_46_compile_loops(v_gl, v_env, v_xs, v_lists, v_result, v_c, v_d, &v_bs, r_g);
        static struct u_object l_u_mk_con_2002 = U_LIT("u_mk_con");
        static struct u_object l__38__2003 = U_LIT("&");
        u_value t2004 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_2002, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_nil_name_0()), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)), u_mk_con(&n_list_46_Nil, 0)))))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_ptr_0(), v_d, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeUnary, 2, &l__38__2003, u_mk_con(&n_c_46_CeVar, 1, v_t)))), u_mk_con(&n_list_46_Nil, 0))), v_bs);
        *r_ss = t2004;
        u_value t2005 = u_mk_con(&n_c_46_CeVar, 1, v_t);
        *r_r = t2005;
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_ETuple)) {
      u_value v_items = u_field(s1966, 0);
      d1967 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_items, r_ss, &v_rs, r_g);
        static struct u_object l_u_mk_tuple_2006 = U_LIT("u_mk_tuple");
        u_value t2007 = f_c_46_c_call_2(&l_u_mk_tuple_2006, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_items)), u_mk_con(&n_list_46_Nil, 0)), v_rs));
        *r_r = t2007;
      }
    }
    if ((!d1967) && u_is_con(s1966, &n_syntax_46_ERaise)) {
      u_value v_arg = u_field(s1966, 0);
      u_value v_ty = u_field(s1966, 1);
      d1967 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2008 = U_LIT("u_raise");
        static struct u_object l_NULL_2009 = U_LIT("NULL");
        u_value t2010 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2008, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        u_value t2011 = u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2009);
        *r_ss = t2010;
        *r_r = t2011;
      }
    }
    if (!d1967) {
      u_no_case(s1966);
    }
  }
}

void p_compile_46_compile_exprs(u_value v_gl, u_value v_env, u_value v_es, u_value *r_ss, u_value *r_rs, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_r = NULL;
    u_value v_bs = NULL;
    u_value v_more = NULL;
    u_value s2012 = v_es;
    int d2013 = 0;
    if ((!d2013) && u_is_con(s2012, &n_list_46_Nil)) {
      d2013 = 1;
      {
        u_value t2014 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2015 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2014;
        *r_rs = t2015;
      }
    }
    if ((!d2013) && u_is_con(s2012, &n_list_46_Cons)) {
      u_value v_e = u_field(s2012, 0);
      u_value v_rest = u_field(s2012, 1);
      d2013 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_e, &v_as, &v_r, r_g);
        p_compile_46_compile_exprs(v_gl, v_env, v_rest, &v_bs, &v_more, r_g);
        u_value t2016 = f_list_46__43__43__2(v_as, v_bs);
        u_value t2017 = u_mk_con(&n_list_46_Cons, 2, v_r, v_more);
        *r_ss = t2016;
        *r_rs = t2017;
      }
    }
    if (!d2013) {
      u_no_case(s2012);
    }
  }
}

void p_compile_46_temps(u_value v_rs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_t = NULL;
    u_value v_more = NULL;
    u_value s2018 = v_rs;
    int d2019 = 0;
    if ((!d2019) && u_is_con(s2018, &n_list_46_Nil)) {
      d2019 = 1;
      {
        u_value t2020 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2020;
      }
    }
    if ((!d2019) && u_is_con(s2018, &n_list_46_Cons)) {
      u_value v_r = u_field(s2018, 0);
      u_value v_rest = u_field(s2018, 1);
      d2019 = 1;
      {
        static struct u_object l_t_2021 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2021, &v_t, r_g);
        p_compile_46_temps(v_rest, &v_more, r_g);
        u_value t2022 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_r)), v_more);
        *r_ss = t2022;
      }
    }
    if (!d2019) {
      u_no_case(s2018);
    }
  }
}

u_value f_compile_46_rebind_2(u_value v_xs, u_value v_ts) {
  u_value s2023 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2024 = NULL;
  if ((!t2024) && (u_is_con(u_field(s2023, 0), &n_list_46_Cons) && (u_is_con(u_field(s2023, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2023, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2023, 0), 0);
    u_value v_more = u_field(u_field(s2023, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2023, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2023, 1), 1);
    t2024 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, v_t))), f_compile_46_rebind_2(v_more, v_rest));
  }
  if (!t2024) {
    t2024 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2024) {
    u_no_case(s2023);
  }
  return t2024;
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
    u_value s2025 = v_args;
    int d2026 = 0;
    if ((!d2026) && (u_is_con(s2025, &n_list_46_Cons) && (u_is_con(u_field(s2025, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2025, 1), 1), &n_list_46_Nil)))) {
      u_value v_a = u_field(s2025, 0);
      u_value v_b = u_field(u_field(s2025, 1), 0);
      static struct u_object l_and_2033 = U_LIT("and");
      u_value t2035 = u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2033));
      if (!u_truth(t2035)) {
        static struct u_object l_or_2034 = U_LIT("or");
        t2035 = u_eq(v_n, f_syntax_46_unqualified_1(&l_or_2034));
      }
      if (u_truth(t2035)) {
        d2026 = 1;
        {
          p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
          p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
          static struct u_object l_t_2027 = U_LIT("t");
          p_compile_46_gen_name(&l_t_2027, &v_t, r_g);
          static struct u_object l_and_2028 = U_LIT("and");
          if (u_truth(u_eq(v_n, f_syntax_46_unqualified_1(&l_and_2028)))) {
            {
              u_value t2029 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2029;
            }
          } else {
            {
              static struct u_object l__33__2030 = U_LIT("!");
              u_value t2031 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2030, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
              *r_ss = t2031;
            }
          }
          u_value t2032 = u_mk_con(&n_c_46_CeVar, 1, v_t);
          *r_r = t2032;
        }
      }
    }
    if (!d2026) {
      d2026 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_args, r_ss, &v_rs, r_g);
        u_value t2036 = f_compile_46_is_global_fun_3(v_gl, v_n, f_list_46_length_1(v_args));
        if (u_truth(t2036)) {
          t2036 = u_eq(f_syntax_46_arity_1(v_ty), f_list_46_length_1(v_args));
        }
        if (u_truth(t2036)) {
          {
            u_value t2037 = f_c_46_c_call_2(f_compile_46_fun_name_2(v_n, f_list_46_length_1(v_args)), v_rs);
            *r_r = t2037;
          }
        } else {
          if (u_truth(u_eq(f_compile_46_fields_count_2(v_gl, v_n), u_mk_con(&n_option_46_Some, 1, f_list_46_length_1(v_args))))) {
            {
              static struct u_object l_u_mk_con_2038 = U_LIT("u_mk_con");
              u_value t2039 = f_c_46_c_call_2(&l_u_mk_con_2038, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
              *r_r = t2039;
            }
          } else {
            {
              u_value s2040 = f_list_46_lookup_2(v_n, f_compile_46_builtin_functions_0());
              int d2041 = 0;
              if ((!d2041) && u_is_con(s2040, &n_option_46_Some)) {
                u_value v_b = u_field(u_field(s2040, 0), 0);
                u_value v_k = u_field(u_field(s2040, 0), 1);
                if (u_truth(u_eq(v_k, f_list_46_length_1(v_args)))) {
                  d2041 = 1;
                  {
                    u_value t2042 = f_c_46_c_call_2(v_b, v_rs);
                    *r_r = t2042;
                  }
                }
              }
              if (!d2041) {
                d2041 = 1;
                {
                  p_compile_46_global_value(v_gl, v_n, v_ty, &v_f);
                  static struct u_object l_u_apply_2043 = U_LIT("u_apply");
                  u_value t2044 = f_c_46_c_call_2(&l_u_apply_2043, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_args)), u_mk_con(&n_list_46_Nil, 0))), v_rs));
                  *r_r = t2044;
                }
              }
              if (!d2041) {
                u_no_case(s2040);
              }
            }
          }
        }
      }
    }
    if (!d2026) {
      u_no_case(s2025);
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
    u_value t2045 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2046 = &t2045;
    u_value l2047 = f_list_46_distinct_1(f_compile_46_free_vars_2(v_b, f_syntax_46_names_1(v_xs)));
    while (u_is_con(l2047, &n_list_46_Cons)) {
      u_value v_x = u_field(l2047, 0);
      if (u_truth(f_list_46_contains_2(v_x, f_list_46_keys_1(v_env)))) {
        e2046 = u_append(e2046, v_x);
      }
      l2047 = u_field(l2047, 1);
    }
    u_value t2048 = t2045;
    v_caps = t2048;
    static struct u_object l_lam_2049 = U_LIT("lam");
    p_compile_46_gen_name(&l_lam_2049, &v_f, r_g);
    p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_list_46__43__43__2(v_caps, f_syntax_46_names_1(v_xs))), v_b, &v_bs, &v_rb, r_g);
    static struct u_object l_env_2050 = U_LIT("env");
    static struct u_object l_args_2051 = U_LIT("args");
    u_value t2052 = f_list_46__43__43__2(f_compile_46_unpack_2(f_compile_46_numbered_2(v_caps, u_mk_int(0)), &l_env_2050), f_compile_46_unpack_2(f_compile_46_numbered_2(f_syntax_46_names_1(v_xs), u_mk_int(0)), &l_args_2051));
    v_decls = t2052;
    static struct u_object l_env_2053 = U_LIT("env");
    static struct u_object l_args_2054 = U_LIT("args");
    p_compile_46_lift(u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_f, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2053), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2054), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_decls, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0))))), r_g);
    static struct u_object l_u_mk_closure_2055 = U_LIT("u_mk_closure");
    u_value t2056 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2057 = &t2056;
    u_value l2058 = v_caps;
    while (u_is_con(l2058, &n_list_46_Cons)) {
      u_value v_x = u_field(l2058, 0);
      if (u_truth(u_mk_bool(1))) {
        e2057 = u_append(e2057, f_compile_46_var_expr_2(v_env, v_x));
      }
      l2058 = u_field(l2058, 1);
    }
    u_value t2059 = u_mk_con(&n_list_46_Nil, 0);
    u_value t2060 = f_c_46_c_call_2(&l_u_mk_closure_2055, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_f), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, f_list_46_length_1(v_caps)), u_mk_con(&n_list_46_Nil, 0))), t2056));
    *r_ss = t2059;
    *r_r = t2060;
  }
}

u_value f_compile_46_unpack_2(u_value v_xs, u_value v_a) {
  u_value s2061 = v_xs;
  u_value t2062 = NULL;
  if ((!t2062) && u_is_con(s2061, &n_list_46_Nil)) {
    t2062 = u_mk_con(&n_list_46_Nil, 0);
  }
  if ((!t2062) && u_is_con(s2061, &n_list_46_Cons)) {
    u_value v_x = u_field(u_field(s2061, 0), 0);
    u_value v_i = u_field(u_field(s2061, 0), 1);
    u_value v_rest = u_field(s2061, 1);
    t2062 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, f_compile_46_index_2(v_a, v_i))), f_compile_46_unpack_2(v_rest, v_a));
  }
  if (!t2062) {
    u_no_case(s2061);
  }
  return t2062;
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
    u_value s2063 = v_cases;
    int d2064 = 0;
    if ((!d2064) && u_is_con(s2063, &n_list_46_Nil)) {
      d2064 = 1;
      {
        u_value t2065 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2065;
      }
    }
    if ((!d2064) && (u_is_con(s2063, &n_list_46_Cons) && u_is_con(u_field(s2063, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2063, 0), 0);
      u_value v_guard = u_field(u_field(s2063, 0), 1);
      u_value v_e = u_field(u_field(s2063, 0), 2);
      u_value v_more = u_field(s2063, 1);
      d2064 = 1;
      {
        u_value t2066 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2066;
        p_compile_46_compile_expr(v_gl, v_env1, v_e, &v_bs, &v_rb, r_g);
        u_value t2067 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_rb), u_mk_con(&n_list_46_Nil, 0)));
        v_body = t2067;
        u_value s2068 = v_guard;
        int d2069 = 0;
        if ((!d2069) && u_is_con(s2068, &n_option_46_Some)) {
          u_value v_c = u_field(s2068, 0);
          d2069 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2070 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2070;
          }
        }
        if ((!d2069) && u_is_con(s2068, &n_option_46_None)) {
          d2069 = 1;
          {
          }
        }
        if (!d2069) {
          u_no_case(s2068);
        }
        p_compile_46_expr_cases(v_gl, v_env, v_more, v_v, v_t, &v_rest, r_g);
        static struct u_object l__33__2071 = U_LIT("!");
        u_value t2072 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2071, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2072;
      }
    }
    if (!d2064) {
      u_no_case(s2063);
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
    u_value s2073 = u_mk_tuple(2, v_xs, v_lists);
    int d2074 = 0;
    if ((!d2074) && (u_is_con(u_field(s2073, 0), &n_list_46_Cons) && u_is_con(u_field(s2073, 1), &n_list_46_Cons))) {
      u_value v_x = u_field(u_field(s2073, 0), 0);
      u_value v_more = u_field(u_field(s2073, 0), 1);
      u_value v_list = u_field(u_field(s2073, 1), 0);
      u_value v_others = u_field(u_field(s2073, 1), 1);
      d2074 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_list, &v_ls, &v_rl, r_g);
        static struct u_object l_l_2075 = U_LIT("l");
        p_compile_46_gen_name(&l_l_2075, &v_l, r_g);
        p_compile_46_compile_loops(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(u_mk_con(&n_list_46_Cons, 2, f_syntax_46_Var_46_name_1(v_x), u_mk_con(&n_list_46_Nil, 0))), v_env), v_more, v_others, v_result, v_c, v_t, &v_inner, r_g);
        u_value t2076 = f_list_46__43__43__2(v_ls, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_l, u_mk_con(&n_option_46_Some, 1, v_rl)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2076;
        static struct u_object l_u_is_con_2077 = U_LIT("u_is_con");
        u_value t2078 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_c_46_c_call_2(&l_u_is_con_2077, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(f_syntax_46_cons_name_0()), u_mk_con(&n_list_46_Nil, 0)))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(f_syntax_46_Var_46_name_1(v_x)), u_mk_con(&n_option_46_Some, 1, f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_inner, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_l), f_compile_46_field_2(u_mk_con(&n_c_46_CeVar, 1, v_l), u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2078;
      }
    }
    if (!d2074) {
      d2074 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_result, &v_bs, &v_rb, r_g);
        static struct u_object l_u_append_2079 = U_LIT("u_append");
        u_value t2080 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), f_c_46_c_call_2(&l_u_append_2079, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2080;
      }
    }
    if (!d2074) {
      u_no_case(s2073);
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
    u_value s2081 = v_s;
    int d2082 = 0;
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SSkip)) {
      d2082 = 1;
      {
        u_value t2083 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2083;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SAssign)) {
      u_value v_pairs = u_field(s2081, 0);
      d2082 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, f_list_46_seconds_1(v_pairs), &v_as, &v_rs, r_g);
        p_compile_46_temps(v_rs, &v_cs, r_g);
        u_value t2084 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_cs, f_compile_46_reassign_3(v_env, f_syntax_46_names_1(f_list_46_firsts_1(v_pairs)), v_cs)));
        *r_ss = t2084;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SBlock)) {
      u_value v_xs = u_field(s2081, 0);
      u_value v_body = u_field(s2081, 1);
      d2082 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_env), v_body, &v_bs, r_g);
        u_value t2085 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2086 = &t2085;
        u_value l2087 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2087, &n_list_46_Cons)) {
          u_value v_x = u_field(l2087, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2088 = U_LIT("NULL");
            e2086 = u_append(e2086, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2088))));
          }
          l2087 = u_field(l2087, 1);
        }
        u_value t2089 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(t2085, v_bs)), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2089;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SIf)) {
      u_value v_c = u_field(s2081, 0);
      u_value v_l = u_field(s2081, 1);
      u_value v_r = u_field(s2081, 2);
      d2082 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_l, &v_as, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_r, &v_bs, r_g);
        u_value t2090 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rc), v_as, v_bs), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2090;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SWhile)) {
      u_value v_c = u_field(s2081, 0);
      u_value v_b = u_field(s2081, 1);
      d2082 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        p_compile_46_compile_stmt(v_gl, v_env, v_b, &v_bs, r_g);
        static struct u_object l_t_2091 = U_LIT("t");
        p_compile_46_gen_name(&l_t_2091, &v_t, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_as, &v_ra, r_g);
        u_value t2092 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_t, u_mk_con(&n_option_46_Some, 1, v_rc)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsWhile, 2, f_compile_46_truth_1(u_mk_con(&n_c_46_CeVar, 1, v_t)), f_list_46__43__43__2(v_bs, f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_t), v_ra), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2092;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SCall)) {
      u_value v_p = u_field(s2081, 0);
      u_value v_ins = u_field(s2081, 1);
      u_value v_outs = u_field(s2081, 2);
      u_value v_refs = u_field(s2081, 3);
      d2082 = 1;
      {
        p_compile_46_compile_exprs(v_gl, v_env, v_ins, &v_as, &v_rs, r_g);
        u_value t2093 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2094 = &t2093;
        u_value l2095 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2095, &n_list_46_Cons)) {
          u_value v_x = u_field(l2095, 0);
          if (u_truth(u_mk_bool(1))) {
            e2094 = u_append(e2094, f_compile_46_address_1(f_compile_46_var_expr_2(v_env, v_x)));
          }
          l2095 = u_field(l2095, 1);
        }
        u_value t2096 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(f_compile_46_proc_name_1(v_p), f_list_46__43__43__2(v_rs, t2093))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2096;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SMatch)) {
      u_value v_arg = u_field(s2081, 0);
      u_value v_cases = u_field(s2081, 1);
      d2082 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_s_2097 = U_LIT("s");
        p_compile_46_gen_name(&l_s_2097, &v_d, r_g);
        static struct u_object l_d_2098 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2098, &v_t, r_g);
        p_compile_46_stmt_cases(v_gl, v_env, v_cases, u_mk_con(&n_c_46_CeVar, 1, v_d), v_t, &v_bs, r_g);
        u_value t2099 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), v_d, u_mk_con(&n_option_46_Some, 1, v_ra)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), v_bs));
        *r_ss = t2099;
        static struct u_object l__33__2100 = U_LIT("!");
        static struct u_object l_u_no_case_2101 = U_LIT("u_no_case");
        u_value t2102 = f_list_46__43__43__2(*r_ss, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2100, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_no_case_2101, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2102;
      }
    }
    if ((!d2082) && (u_is_con(s2081, &n_syntax_46_SAssert) && (u_is_con(u_field(s2081, 0), &n_syntax_46_EApp) && (u_is_con(u_field(u_field(s2081, 0), 0), &n_syntax_46_EVar) && (u_is_con(u_field(u_field(u_field(s2081, 0), 0), 0), &n_syntax_46_Var) && (u_is_con(u_field(u_field(u_field(u_field(s2081, 0), 0), 0), 0), &n_syntax_46_Name) && (u_is_con(u_field(u_field(u_field(u_field(u_field(s2081, 0), 0), 0), 0), 0), &n_option_46_None) && (u_equals(u_field(u_field(u_field(u_field(u_field(s2081, 0), 0), 0), 0), 1), u_mk_lit("==")) && (u_is_con(u_field(u_field(s2081, 0), 1), &n_list_46_Cons) && (u_is_con(u_field(u_field(u_field(s2081, 0), 1), 1), &n_list_46_Cons) && u_is_con(u_field(u_field(u_field(u_field(s2081, 0), 1), 1), 1), &n_list_46_Nil))))))))))) {
      u_value v_a = u_field(u_field(u_field(s2081, 0), 1), 0);
      u_value v_b = u_field(u_field(u_field(u_field(s2081, 0), 1), 1), 0);
      d2082 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_a, &v_as, &v_ra, r_g);
        p_compile_46_compile_expr(v_gl, v_env, v_b, &v_bs, &v_rb, r_g);
        static struct u_object l_u_assert_equal_2103 = U_LIT("u_assert_equal");
        u_value t2104 = f_list_46__43__43__2(v_as, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_equal_2103, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0))));
        *r_ss = t2104;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SAssert)) {
      u_value v_c = u_field(s2081, 0);
      d2082 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_c, &v_cs, &v_rc, r_g);
        static struct u_object l_u_assert_true_2105 = U_LIT("u_assert_true");
        u_value t2106 = f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_assert_true_2105, u_mk_con(&n_list_46_Cons, 2, v_rc, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2106;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_SRaise)) {
      u_value v_arg = u_field(s2081, 0);
      u_value v_ty = u_field(s2081, 1);
      d2082 = 1;
      {
        p_compile_46_compile_expr(v_gl, v_env, v_arg, &v_as, &v_ra, r_g);
        static struct u_object l_u_raise_2107 = U_LIT("u_raise");
        u_value t2108 = f_list_46__43__43__2(v_as, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_raise_2107, u_mk_con(&n_list_46_Cons, 2, v_ra, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)));
        *r_ss = t2108;
      }
    }
    if ((!d2082) && u_is_con(s2081, &n_syntax_46_STry)) {
      u_value v_body = u_field(s2081, 0);
      u_value v_hs = u_field(s2081, 1);
      d2082 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_body, &v_bs, r_g);
        static struct u_object l_h_2109 = U_LIT("h");
        p_compile_46_gen_name(&l_h_2109, &v_d, r_g);
        static struct u_object l_d_2110 = U_LIT("d");
        p_compile_46_gen_name(&l_d_2110, &v_t, r_g);
        p_compile_46_handler_cases(v_gl, v_env, v_hs, v_d, v_t, &v_cs, r_g);
        static struct u_object l_u_pop_2111 = U_LIT("u_pop");
        static struct u_object l__33__2112 = U_LIT("!");
        static struct u_object l_u_reraise_2113 = U_LIT("u_reraise");
        u_value t2114 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2111, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtInt, 0), v_t, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(v_cs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2112, u_mk_con(&n_c_46_CeVar, 1, v_t)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2113, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))));
        v_as = t2114;
        static struct u_object l_struct_32_u_handler_2115 = U_LIT("struct u_handler");
        static struct u_object l_u_push_2116 = U_LIT("u_push");
        static struct u_object l__61__61__2117 = U_LIT("==");
        static struct u_object l_setjmp_2118 = U_LIT("setjmp");
        static struct u_object l_env_2119 = U_LIT("env");
        static struct u_object l_u_pop_2120 = U_LIT("u_pop");
        u_value t2121 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2115)), v_d, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2116, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2117, f_c_46_c_call_2(&l_setjmp_2118, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), &l_env_2119), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2120, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0))), v_as), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ss = t2121;
      }
    }
    if (!d2082) {
      u_no_case(s2081);
    }
  }
}

void p_compile_46_compile_stmts(u_value v_gl, u_value v_env, u_value v_xs, u_value *r_ss, u_value *r_g) {
  {
    u_value v_as = NULL;
    u_value v_bs = NULL;
    u_value s2122 = v_xs;
    int d2123 = 0;
    if ((!d2123) && u_is_con(s2122, &n_list_46_Nil)) {
      d2123 = 1;
      {
        u_value t2124 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2124;
      }
    }
    if ((!d2123) && u_is_con(s2122, &n_list_46_Cons)) {
      u_value v_x = u_field(s2122, 0);
      u_value v_rest = u_field(s2122, 1);
      d2123 = 1;
      {
        p_compile_46_compile_stmt(v_gl, v_env, v_x, &v_as, r_g);
        p_compile_46_compile_stmts(v_gl, v_env, v_rest, &v_bs, r_g);
        u_value t2125 = f_list_46__43__43__2(v_as, v_bs);
        *r_ss = t2125;
      }
    }
    if (!d2123) {
      u_no_case(s2122);
    }
  }
}

u_value f_compile_46_reassign_3(u_value v_env, u_value v_xs, u_value v_ts) {
  u_value s2126 = u_mk_tuple(2, v_xs, v_ts);
  u_value t2127 = NULL;
  if ((!t2127) && (u_is_con(u_field(s2126, 0), &n_list_46_Cons) && (u_is_con(u_field(s2126, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2126, 1), 0), &n_c_46_CsDecl)))) {
    u_value v_x = u_field(u_field(s2126, 0), 0);
    u_value v_more = u_field(u_field(s2126, 0), 1);
    u_value v_t = u_field(u_field(u_field(s2126, 1), 0), 1);
    u_value v_rest = u_field(u_field(s2126, 1), 1);
    t2127 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, f_compile_46_var_expr_2(v_env, v_x), u_mk_con(&n_c_46_CeVar, 1, v_t)), f_compile_46_reassign_3(v_env, v_more, v_rest));
  }
  if (!t2127) {
    t2127 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2127) {
    u_no_case(s2126);
  }
  return t2127;
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
    static struct u_object l_u_caught_2128 = U_LIT("u_caught");
    u_value t2129 = f_c_46_c_call_2(&l_u_caught_2128, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Nil, 0)));
    v_v = t2129;
    u_value s2130 = v_hs;
    int d2131 = 0;
    if ((!d2131) && u_is_con(s2130, &n_list_46_Nil)) {
      d2131 = 1;
      {
        u_value t2132 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2132;
      }
    }
    if ((!d2131) && (u_is_con(s2130, &n_list_46_Cons) && u_is_con(u_field(s2130, 0), &n_syntax_46_Handler))) {
      u_value v_ty = u_field(u_field(s2130, 0), 0);
      u_value v_p = u_field(u_field(s2130, 0), 1);
      u_value v_guard = u_field(u_field(s2130, 0), 2);
      u_value v_s = u_field(u_field(s2130, 0), 3);
      u_value v_more = u_field(s2130, 1);
      d2131 = 1;
      {
        u_value t2133 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2133;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2134 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2134;
        u_value s2135 = v_guard;
        int d2136 = 0;
        if ((!d2136) && u_is_con(s2135, &n_option_46_Some)) {
          u_value v_c = u_field(s2135, 0);
          d2136 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2137 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2137;
          }
        }
        if ((!d2136) && u_is_con(s2135, &n_option_46_None)) {
          d2136 = 1;
          {
          }
        }
        if (!d2136) {
          u_no_case(s2135);
        }
        u_value s2138 = v_p;
        int d2139 = 0;
        if ((!d2139) && u_is_con(s2138, &n_syntax_46_PWild)) {
          d2139 = 1;
          {
            u_value t2140 = u_mk_con(&n_list_46_Nil, 0);
            v_tests = t2140;
          }
        }
        if (!d2139) {
          d2139 = 1;
          {
            static struct u_object l_u_has_type_2141 = U_LIT("u_has_type");
            u_value t2142 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, f_c_46_c_call_2(&l_u_has_type_2141, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, v_h), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_compile_46_type_key_1(v_ty)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v));
            v_tests = t2142;
          }
        }
        if (!d2139) {
          u_no_case(s2138);
        }
        p_compile_46_handler_cases(v_gl, v_env, v_more, v_h, v_d, &v_rest, r_g);
        static struct u_object l__33__2143 = U_LIT("!");
        u_value t2144 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2143, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), v_tests)), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2144;
      }
    }
    if (!d2131) {
      u_no_case(s2130);
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
    u_value s2145 = v_cases;
    int d2146 = 0;
    if ((!d2146) && u_is_con(s2145, &n_list_46_Nil)) {
      d2146 = 1;
      {
        u_value t2147 = u_mk_con(&n_list_46_Nil, 0);
        *r_ss = t2147;
      }
    }
    if ((!d2146) && (u_is_con(s2145, &n_list_46_Cons) && u_is_con(u_field(s2145, 0), &n_syntax_46_Branch))) {
      u_value v_p = u_field(u_field(s2145, 0), 0);
      u_value v_guard = u_field(u_field(s2145, 0), 1);
      u_value v_s = u_field(u_field(s2145, 0), 2);
      u_value v_more = u_field(s2145, 1);
      d2146 = 1;
      {
        u_value t2148 = f_list_46__43__43__2(f_compile_46_in_scope_1(f_compile_46_pattern_names_1(v_p)), v_env);
        v_env1 = t2148;
        p_compile_46_compile_stmt(v_gl, v_env1, v_s, &v_bs, r_g);
        u_value t2149 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, v_d), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(1))), u_mk_con(&n_list_46_Nil, 0)), v_bs);
        v_body = t2149;
        u_value s2150 = v_guard;
        int d2151 = 0;
        if ((!d2151) && u_is_con(s2150, &n_option_46_Some)) {
          u_value v_c = u_field(s2150, 0);
          d2151 = 1;
          {
            p_compile_46_compile_expr(v_gl, v_env1, v_c, &v_gs, &v_rg, r_g);
            u_value t2152 = f_list_46__43__43__2(v_gs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_truth_1(v_rg), v_body, u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)));
            v_body = t2152;
          }
        }
        if ((!d2151) && u_is_con(s2150, &n_option_46_None)) {
          d2151 = 1;
          {
          }
        }
        if (!d2151) {
          u_no_case(s2150);
        }
        p_compile_46_stmt_cases(v_gl, v_env, v_more, v_v, v_d, &v_rest, r_g);
        static struct u_object l__33__2153 = U_LIT("!");
        u_value t2154 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, f_compile_46_conjunction_1(f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeUnary, 2, &l__33__2153, u_mk_con(&n_c_46_CeVar, 1, v_d)), u_mk_con(&n_list_46_Nil, 0)), f_compile_46_pattern_tests_2(v_p, v_v))), f_list_46__43__43__2(f_compile_46_pattern_binds_2(v_p, v_v), v_body), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), v_rest);
        *r_ss = t2154;
      }
    }
    if (!d2146) {
      u_no_case(s2145);
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
    u_value s2155 = v_c;
    int d2156 = 0;
    if ((!d2156) && u_is_con(s2155, &n_syntax_46_CFun)) {
      u_value v_n = u_field(s2155, 0);
      u_value v_xss = u_field(s2155, 2);
      u_value v_b = u_field(s2155, 4);
      d2156 = 1;
      {
        p_compile_46_compile_expr(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(f_check_46_first_list_1(v_xss))), f_compile_46_lambdas_2(f_list_46_drop_2(u_mk_int(1), v_xss), v_b), &v_bs, &v_rb, r_g);
        u_value t2157 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2158 = &t2157;
        u_value l2159 = f_syntax_46_names_1(f_check_46_first_list_1(v_xss));
        while (u_is_con(l2159, &n_list_46_Cons)) {
          u_value v_x = u_field(l2159, 0);
          if (u_truth(u_mk_bool(1))) {
            e2158 = u_append(e2158, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2159 = u_field(l2159, 1);
        }
        u_value t2160 = t2157;
        v_params = t2160;
        if (u_truth(u_eq(f_check_46_first_list_1(v_xss), u_mk_con(&n_list_46_Nil, 0)))) {
          {
            u_value t2161 = f_compile_46_cached_3(f_syntax_46_qualified_1(v_n), v_bs, v_rb);
            v_bs = t2161;
          }
        } else {
          {
            u_value t2162 = f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, v_rb)), u_mk_con(&n_list_46_Nil, 0)));
            v_bs = t2162;
          }
        }
        u_value t2163 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), f_compile_46_fun_name_2(v_n, f_list_46_length_1(f_check_46_first_list_1(v_xss))), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2164 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2163;
        *r_ms = t2164;
      }
    }
    if ((!d2156) && u_is_con(s2155, &n_syntax_46_CProc)) {
      u_value v_n = u_field(s2155, 0);
      u_value v_ins = u_field(s2155, 2);
      u_value v_outs = u_field(s2155, 3);
      u_value v_refs = u_field(s2155, 4);
      u_value v_body = u_field(s2155, 5);
      d2156 = 1;
      {
        u_value t2165 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2166 = &t2165;
        u_value l2167 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2167, &n_list_46_Cons)) {
          u_value v_x = u_field(l2167, 0);
          if (u_truth(u_mk_bool(1))) {
            e2166 = u_append(e2166, f_compile_46_pointer_1(v_x));
          }
          l2167 = u_field(l2167, 1);
        }
        p_compile_46_compile_stmt(v_gl, f_list_46__43__43__2(f_compile_46_in_scope_1(f_syntax_46_names_1(v_ins)), t2165), v_body, &v_bs, r_g);
        u_value t2168 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2169 = &t2168;
        u_value l2170 = f_syntax_46_names_1(v_ins);
        while (u_is_con(l2170, &n_list_46_Cons)) {
          u_value v_x = u_field(l2170, 0);
          if (u_truth(u_mk_bool(1))) {
            e2169 = u_append(e2169, u_mk_tuple(2, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x)));
          }
          l2170 = u_field(l2170, 1);
        }
        u_value t2171 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2172 = &t2171;
        u_value l2173 = f_syntax_46_names_1(f_list_46__43__43__2(v_outs, v_refs));
        while (u_is_con(l2173, &n_list_46_Cons)) {
          u_value v_x = u_field(l2173, 0);
          if (u_truth(u_mk_bool(1))) {
            e2172 = u_append(e2172, u_mk_tuple(2, f_compile_46_value_ptr_0(), f_compile_46_ref_name_1(v_x)));
          }
          l2173 = u_field(l2173, 1);
        }
        u_value t2174 = f_list_46__43__43__2(t2168, t2171);
        v_params = t2174;
        u_value t2175 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtVoid, 0), f_compile_46_proc_name_1(v_n), v_params, v_bs), u_mk_con(&n_list_46_Nil, 0));
        u_value t2176 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2175;
        *r_ms = t2176;
      }
    }
    if ((!d2156) && u_is_con(s2155, &n_syntax_46_CEval)) {
      u_value v_e = u_field(s2155, 0);
      d2156 = 1;
      {
        p_compile_46_compile_expr(v_gl, u_mk_con(&n_list_46_Nil, 0), v_e, &v_bs, &v_rb, r_g);
        static struct u_object l_u_print_line_2177 = U_LIT("u_print_line");
        u_value t2178 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2179 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_line_2177, u_mk_con(&n_list_46_Cons, 2, v_rb, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2178;
        *r_ms = t2179;
      }
    }
    if ((!d2156) && (u_is_con(s2155, &n_syntax_46_CRun) && u_is_con(u_field(s2155, 1), &n_syntax_46_SBlock))) {
      u_value v_xs = u_field(u_field(s2155, 1), 0);
      u_value v_body = u_field(u_field(s2155, 1), 1);
      d2156 = 1;
      {
        p_compile_46_compile_stmts(v_gl, f_compile_46_in_scope_1(f_syntax_46_names_1(v_xs)), v_body, &v_bs, r_g);
        u_value t2180 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2181 = &t2180;
        u_value l2182 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2182, &n_list_46_Cons)) {
          u_value v_x = u_field(l2182, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_NULL_2183 = U_LIT("NULL");
            e2181 = u_append(e2181, u_mk_con(&n_c_46_CsDecl, 3, f_compile_46_value_type_0(), f_compile_46_var_name_1(v_x), u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2183))));
          }
          l2182 = u_field(l2182, 1);
        }
        u_value t2184 = t2180;
        v_copy = t2184;
        u_value t2185 = u_mk_con(&n_list_46_Nil, 0);
        u_value *e2186 = &t2185;
        u_value l2187 = f_syntax_46_names_1(v_xs);
        while (u_is_con(l2187, &n_list_46_Cons)) {
          u_value v_x = u_field(l2187, 0);
          if (u_truth(u_mk_bool(1))) {
            static struct u_object l_u_print_binding_2188 = U_LIT("u_print_binding");
            e2186 = u_append(e2186, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_print_binding_2188, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, f_syntax_46_Name_46_base_1(v_x)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, f_compile_46_var_name_1(v_x)), u_mk_con(&n_list_46_Nil, 0))))));
          }
          l2187 = u_field(l2187, 1);
        }
        u_value t2189 = t2185;
        v_back = t2189;
        u_value t2190 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2191 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, f_list_46__43__43__2(v_copy, f_list_46__43__43__2(v_bs, v_back))), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2190;
        *r_ms = t2191;
      }
    }
    if ((!d2156) && u_is_con(s2155, &n_syntax_46_CRun)) {
      u_value v_body = u_field(s2155, 1);
      d2156 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2192 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2193 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2192;
        *r_ms = t2193;
      }
    }
    if ((!d2156) && u_is_con(s2155, &n_syntax_46_CTest)) {
      u_value v_body = u_field(s2155, 1);
      d2156 = 1;
      {
        p_compile_46_compile_stmt(v_gl, u_mk_con(&n_list_46_Nil, 0), v_body, &v_bs, r_g);
        u_value t2194 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2195 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsBlock, 1, v_bs), u_mk_con(&n_list_46_Nil, 0));
        *r_ds = t2194;
        *r_ms = t2195;
      }
    }
    if (!d2156) {
      d2156 = 1;
      {
        u_value t2196 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2197 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2196;
        *r_ms = t2197;
      }
    }
    if (!d2156) {
      u_no_case(s2155);
    }
  }
}

void p_compile_46_compile_cmds(u_value v_gl, u_value v_cs, u_value *r_ds, u_value *r_ms, u_value *r_g) {
  {
    u_value v_d1 = NULL;
    u_value v_m1 = NULL;
    u_value v_d2 = NULL;
    u_value v_m2 = NULL;
    u_value s2198 = v_cs;
    int d2199 = 0;
    if ((!d2199) && u_is_con(s2198, &n_list_46_Nil)) {
      d2199 = 1;
      {
        u_value t2200 = u_mk_con(&n_list_46_Nil, 0);
        u_value t2201 = u_mk_con(&n_list_46_Nil, 0);
        *r_ds = t2200;
        *r_ms = t2201;
      }
    }
    if ((!d2199) && u_is_con(s2198, &n_list_46_Cons)) {
      u_value v_c = u_field(s2198, 0);
      u_value v_rest = u_field(s2198, 1);
      d2199 = 1;
      {
        p_compile_46_compile_cmd(v_gl, v_c, &v_d1, &v_m1, r_g);
        p_compile_46_compile_cmds(v_gl, v_rest, &v_d2, &v_m2, r_g);
        u_value t2202 = f_list_46__43__43__2(v_d1, v_d2);
        u_value t2203 = f_list_46__43__43__2(v_m1, v_m2);
        *r_ds = t2202;
        *r_ms = t2203;
      }
    }
    if (!d2199) {
      u_no_case(s2198);
    }
  }
}

u_value f_compile_46_cached_3(u_value v_n, u_value v_bs, u_value v_r) {
  static struct u_object l_handler_2204 = U_LIT("handler");
  u_value t2231 = u_mk_con(&n_c_46_CeVar, 1, &l_handler_2204);
  u_value t2230;
  {
    u_value v_h = t2231;
    static struct u_object l_static_32_u_value_2205 = U_LIT("static u_value");
    static struct u_object l_cache_2206 = U_LIT("cache");
    static struct u_object l_NULL_2207 = U_LIT("NULL");
    static struct u_object l__61__61__2208 = U_LIT("==");
    static struct u_object l_cache_2209 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2210 = U_LIT("U_COMPUTING");
    static struct u_object l_u_fail_2211 = U_LIT("u_fail");
    static struct u_object l_recursive_32_initia_2212 = U_LIT("recursive initialization of ");
    static struct u_object l_cache_2213 = U_LIT("cache");
    static struct u_object l_cache_2214 = U_LIT("cache");
    static struct u_object l_cache_2215 = U_LIT("cache");
    static struct u_object l_U_COMPUTING_2216 = U_LIT("U_COMPUTING");
    static struct u_object l_struct_32_u_handler_2217 = U_LIT("struct u_handler");
    static struct u_object l_handler_2218 = U_LIT("handler");
    static struct u_object l_u_push_2219 = U_LIT("u_push");
    static struct u_object l__33__61__2220 = U_LIT("!=");
    static struct u_object l_setjmp_2221 = U_LIT("setjmp");
    static struct u_object l_env_2222 = U_LIT("env");
    static struct u_object l_u_pop_2223 = U_LIT("u_pop");
    static struct u_object l_cache_2224 = U_LIT("cache");
    static struct u_object l_NULL_2225 = U_LIT("NULL");
    static struct u_object l_u_reraise_2226 = U_LIT("u_reraise");
    static struct u_object l_u_pop_2227 = U_LIT("u_pop");
    static struct u_object l_cache_2228 = U_LIT("cache");
    static struct u_object l_cache_2229 = U_LIT("cache");
    t2230 = f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtNamed, 1, &l_static_32_u_value_2205), &l_cache_2206, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2207))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__61__61__2208, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2209), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2210)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_fail_2211, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeStr, 1, u_concat(&l_recursive_32_initia_2212, v_n)), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2213), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2214))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2215), u_mk_con(&n_c_46_CeVar, 1, &l_U_COMPUTING_2216)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsDecl, 3, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_struct_32_u_handler_2217)), &l_handler_2218, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_push_2219, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsIf, 3, u_mk_con(&n_c_46_CeBinary, 3, &l__33__61__2220, f_c_46_c_call_2(&l_setjmp_2221, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeArrow, 2, v_h, &l_env_2222), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2223, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2224), u_mk_con(&n_c_46_CeVar, 1, &l_NULL_2225)), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_reraise_2226, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Nil, 0)), u_mk_con(&n_list_46_Nil, 0))))))), f_list_46__43__43__2(v_bs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_pop_2227, u_mk_con(&n_list_46_Cons, 2, v_h, u_mk_con(&n_list_46_Nil, 0)))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsAssign, 2, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2228), v_r), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeVar, 1, &l_cache_2229))), u_mk_con(&n_list_46_Nil, 0))))));
  }
  return t2230;
}

u_value f_compile_46_wrappers_1(u_value v_gl) {
  u_value t2232 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2233 = &t2232;
  u_value l2234 = f_compile_46_Globals_46_funs_1(v_gl);
  while (u_is_con(l2234, &n_list_46_Cons)) {
    u_value v_p = u_field(l2234, 0);
    if (u_truth(u_mk_bool(1))) {
      e2233 = u_append(e2233, f_compile_46_fun_wrapper_1(v_p));
    }
    l2234 = u_field(l2234, 1);
  }
  u_value t2235 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2236 = &t2235;
  u_value l2237 = f_compile_46_Globals_46_constrs_1(v_gl);
  while (u_is_con(l2237, &n_list_46_Cons)) {
    u_value v_p = u_field(l2237, 0);
    if (u_truth(u_mk_bool(1))) {
      e2236 = u_append(e2236, f_compile_46_constr_wrapper_1(v_p));
    }
    l2237 = u_field(l2237, 1);
  }
  u_value t2238 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2239 = &t2238;
  u_value l2240 = f_list_46_seconds_1(f_compile_46_builtin_functions_0());
  while (u_is_con(l2240, &n_list_46_Cons)) {
    u_value v_p = u_field(l2240, 0);
    if (u_truth(u_mk_bool(1))) {
      e2239 = u_append(e2239, f_compile_46_builtin_wrapper_1(v_p));
    }
    l2240 = u_field(l2240, 1);
  }
  return f_list_46__43__43__2(t2232, f_list_46__43__43__2(f_list_46_concat_1(t2235), t2238));
}

u_value f_compile_46_fun_wrapper_1(u_value v_p) {
  u_value s2241 = v_p;
  u_value t2242 = NULL;
  if (!t2242) {
    u_value v_n = u_field(s2241, 0);
    u_value v_k = u_field(s2241, 1);
    static struct u_object l_w__2243 = U_LIT("w_");
    t2242 = f_compile_46_wrapper_3(u_concat(&l_w__2243, f_compile_46_fun_name_2(v_n, v_k)), f_compile_46_fun_name_2(v_n, v_k), v_k);
  }
  if (!t2242) {
    u_no_case(s2241);
  }
  return t2242;
}

u_value f_compile_46_builtin_wrapper_1(u_value v_p) {
  u_value s2244 = v_p;
  u_value t2245 = NULL;
  if (!t2245) {
    u_value v_f = u_field(s2244, 0);
    u_value v_k = u_field(s2244, 1);
    static struct u_object l_w_2246 = U_LIT("w");
    t2245 = f_compile_46_wrapper_3(u_concat(&l_w_2246, v_f), v_f, v_k);
  }
  if (!t2245) {
    u_no_case(s2244);
  }
  return t2245;
}

u_value f_compile_46_wrapper_3(u_value v_w, u_value v_f, u_value v_k) {
  static struct u_object l_env_2247 = U_LIT("env");
  static struct u_object l_args_2248 = U_LIT("args");
  u_value t2249 = u_mk_con(&n_list_46_Nil, 0);
  u_value *e2250 = &t2249;
  u_value l2251 = f_compile_46_range_to_1(v_k);
  while (u_is_con(l2251, &n_list_46_Cons)) {
    u_value v_i = u_field(l2251, 0);
    if (u_truth(u_mk_bool(1))) {
      static struct u_object l_args_2252 = U_LIT("args");
      e2250 = u_append(e2250, f_compile_46_index_2(&l_args_2252, v_i));
    }
    l2251 = u_field(l2251, 1);
  }
  return u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), v_w, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2247), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2248), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(v_f, t2249))), u_mk_con(&n_list_46_Nil, 0)));
}

u_value f_compile_46_constr_wrapper_1(u_value v_p) {
  u_value s2253 = v_p;
  u_value t2254 = NULL;
  if ((!t2254) && u_equals(u_field(s2253, 1), u_mk_int(0))) {
    t2254 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2254) {
    u_value v_n = u_field(s2253, 0);
    u_value v_k = u_field(s2253, 1);
    static struct u_object l_wc__2255 = U_LIT("wc_");
    static struct u_object l_env_2256 = U_LIT("env");
    static struct u_object l_args_2257 = U_LIT("args");
    static struct u_object l_u_mk_con_array_2258 = U_LIT("u_mk_con_array");
    static struct u_object l_args_2259 = U_LIT("args");
    t2254 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, f_compile_46_value_type_0(), u_concat(&l_wc__2255, f_compile_46_mangle_1(v_n)), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_env_2256), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, f_compile_46_value_ptr_0(), &l_args_2257), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, f_c_46_c_call_2(&l_u_mk_con_array_2258, u_mk_con(&n_list_46_Cons, 2, f_compile_46_descriptor_1(v_n), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeNum, 1, v_k), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_args_2259), u_mk_con(&n_list_46_Nil, 0))))))), u_mk_con(&n_list_46_Nil, 0))), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2254) {
    u_no_case(s2253);
  }
  return t2254;
}

u_value f_compile_46_range_to_1(u_value v_k) {
  u_value t2260;
  if (u_truth(u_eq(v_k, u_mk_int(0)))) {
    t2260 = u_mk_con(&n_list_46_Nil, 0);
  } else {
    t2260 = f_list_46__43__43__2(f_compile_46_range_to_1(u_sub(v_k, u_mk_int(1))), u_mk_con(&n_list_46_Cons, 2, u_sub(v_k, u_mk_int(1)), u_mk_con(&n_list_46_Nil, 0)));
  }
  return t2260;
}

u_value f_compile_46_prototype_1(u_value v_d) {
  u_value s2261 = v_d;
  u_value t2262 = NULL;
  if ((!t2262) && u_is_con(s2261, &n_c_46_CdFunction)) {
    u_value v_t = u_field(s2261, 0);
    u_value v_f = u_field(s2261, 1);
    u_value v_ps = u_field(s2261, 2);
    t2262 = u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdPrototype, 3, v_t, v_f, v_ps), u_mk_con(&n_list_46_Nil, 0));
  }
  if (!t2262) {
    t2262 = u_mk_con(&n_list_46_Nil, 0);
  }
  if (!t2262) {
    u_no_case(s2261);
  }
  return t2262;
}

void p_compile_46_compile_program(u_value v_q, u_value *r_f) {
  {
    u_value v_g = NULL;
    u_value v_gl = NULL;
    u_value v_ds = NULL;
    u_value v_ms = NULL;
    u_value v_fs = NULL;
    u_value t2263 = u_mk_con(&n_compile_46_Gen, 2, u_mk_int(0), u_mk_con(&n_list_46_Nil, 0));
    v_g = t2263;
    u_value t2264 = f_compile_46_globals_1(f_syntax_46_Program_46_cmds_1(v_q));
    v_gl = t2264;
    p_compile_46_compile_cmds(v_gl, f_syntax_46_Program_46_cmds_1(v_q), &v_ds, &v_ms, &v_g);
    u_value s2265 = v_g;
    int d2266 = 0;
    if ((!d2266) && u_is_con(s2265, &n_compile_46_Gen)) {
      u_value v_lifted = u_field(s2265, 1);
      d2266 = 1;
      {
        u_value t2267 = f_list_46__43__43__2(f_compile_46_wrappers_1(v_gl), f_list_46__43__43__2(v_ds, v_lifted));
        v_fs = t2267;
      }
    }
    if (!d2266) {
      u_no_case(s2265);
    }
    static struct u_object l_ulang_46_h_2268 = U_LIT("ulang.h");
    u_value t2269 = u_mk_con(&n_list_46_Nil, 0);
    u_value *e2270 = &t2269;
    u_value l2271 = v_fs;
    while (u_is_con(l2271, &n_list_46_Cons)) {
      u_value v_d = u_field(l2271, 0);
      if (u_truth(u_mk_bool(1))) {
        e2270 = u_append(e2270, f_compile_46_prototype_1(v_d));
      }
      l2271 = u_field(l2271, 1);
    }
    static struct u_object l_main_2272 = U_LIT("main");
    static struct u_object l_argc_2273 = U_LIT("argc");
    static struct u_object l_char_2274 = U_LIT("char");
    static struct u_object l_argv_2275 = U_LIT("argv");
    static struct u_object l_u_args_2276 = U_LIT("u_args");
    static struct u_object l_argc_2277 = U_LIT("argc");
    static struct u_object l_argv_2278 = U_LIT("argv");
    u_value t2279 = u_mk_con(&n_c_46_CFile, 1, f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdInclude, 1, &l_ulang_46_h_2268), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(f_compile_46_descriptor_decls_1(f_syntax_46_Program_46_cmds_1(v_q)), f_list_46__43__43__2(f_list_46_concat_1(t2269), f_list_46__43__43__2(v_fs, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CdFunction, 4, u_mk_con(&n_c_46_CtInt, 0), &l_main_2272, u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtInt, 0), &l_argc_2273), u_mk_con(&n_list_46_Cons, 2, u_mk_tuple(2, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtPtr, 1, u_mk_con(&n_c_46_CtNamed, 1, &l_char_2274))), &l_argv_2275), u_mk_con(&n_list_46_Nil, 0))), f_list_46__43__43__2(u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsExpr, 1, f_c_46_c_call_2(&l_u_args_2276, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argc_2277), u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CeVar, 1, &l_argv_2278), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)), f_list_46__43__43__2(v_ms, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_c_46_CsReturn, 1, u_mk_con(&n_option_46_Some, 1, u_mk_con(&n_c_46_CeNum, 1, u_mk_int(0)))), u_mk_con(&n_list_46_Nil, 0))))), u_mk_con(&n_list_46_Nil, 0)))))));
    *r_f = t2279;
  }
}

void p_compile_46_compile_file(u_value v_source, u_value v_target) {
  {
    u_value v_pre = NULL;
    u_value v_p = NULL;
    u_value v_q = NULL;
    u_value v_f = NULL;
    static struct u_object l_prelude_2280 = U_LIT("prelude");
    p_parse_46_parse_library(&l_prelude_2280, &v_pre);
    p_parse_46_parse_file_after(v_source, v_pre, &v_p);
    static struct u_object l_prelude_2281 = U_LIT("prelude");
    p_check_46_check_program(u_mk_con(&n_syntax_46_Program, 1, u_mk_con(&n_list_46_Cons, 2, u_mk_con(&n_syntax_46_CModule, 2, &l_prelude_2281, f_syntax_46_Program_46_cmds_1(v_pre)), f_syntax_46_Program_46_cmds_1(v_p))), f_parse_46_base_name_1(v_source), &v_q);
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
    u_value s2282 = v_args;
    int d2283 = 0;
    if ((!d2283) && u_is_con(s2282, &n_list_46_Nil)) {
      d2283 = 1;
      {
      }
    }
    if ((!d2283) && (u_is_con(s2282, &n_list_46_Cons) && (u_is_con(u_field(s2282, 1), &n_list_46_Cons) && u_is_con(u_field(u_field(s2282, 1), 1), &n_list_46_Nil)))) {
      u_value v_source = u_field(s2282, 0);
      u_value v_target = u_field(u_field(s2282, 1), 0);
      d2283 = 1;
      {
        p_compile_46_compile_file(v_source, v_target);
      }
    }
    if (!d2283) {
      d2283 = 1;
      {
        static struct u_object l_usage_58__32_ulang_32_sou_2284 = U_LIT("usage: ulang source.u target.c");
        u_raise(u_mk_con(&n_os_46_OsError, 1, &l_usage_58__32_ulang_32_sou_2284), "os.OsError");
      }
    }
    if (!d2283) {
      u_no_case(s2282);
    }
    u_print_binding("args", v_args);
  }
  return 0;
}
