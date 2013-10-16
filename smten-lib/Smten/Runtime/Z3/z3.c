
// This library loads and links to the dynamic z3 library at runtime if
// one is found.

#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>

typedef void* z3cfg;
typedef void* z3ctx;
typedef void* z3decl;
typedef void* z3expr;
typedef void* z3model;
typedef void* z3solver;
typedef void* z3sort;
typedef void* z3sym;
typedef int z3bool;
typedef int z3lbool;

z3sort (*z3_mk_bool_sort_f)(z3ctx) = NULL;
z3sort (*z3_mk_int_sort_f)(z3ctx) = NULL;
z3sort (*z3_mk_bv_sort_f)(z3ctx, unsigned int) = NULL;
z3sym (*z3_mk_string_symbol_f)(z3ctx, const char*) = NULL;
z3decl (*z3_mk_func_decl_f)(z3ctx, z3sym, unsigned int, z3sort*, z3sort) = NULL;
z3expr (*z3_mk_app_f)(z3ctx, z3decl, unsigned int, z3expr*, z3expr) = NULL;
z3model (*z3_solver_get_model_f)(z3ctx, z3solver) = NULL;
z3bool (*z3_model_eval_f)(z3ctx, z3model, z3expr, z3bool, z3expr*) = NULL;
z3expr (*z3_model_get_const_interp_f)(z3ctx, z3model, z3decl) = NULL;
z3lbool (*z3_get_bool_value_f)(z3ctx, z3expr) = NULL;
char* (*z3_get_numeral_string_f)(z3ctx, z3expr) = NULL;
z3lbool (*z3_solver_check_f)(z3ctx, z3solver) = NULL;
void (*z3_del_context_f)(z3ctx) = NULL;
void (*z3_solver_assert_f)(z3ctx, z3solver, z3expr) = NULL;
z3expr (*z3_mk_true_f)(z3ctx) = NULL;
z3expr (*z3_mk_false_f)(z3ctx) = NULL;
z3expr (*z3_mk_and_f)(z3ctx, unsigned int, z3expr*) = NULL;
z3expr (*z3_mk_not_f)(z3ctx, z3expr) = NULL;
z3expr (*z3_mk_ite_f)(z3ctx, z3expr, z3expr, z3expr) = NULL;
z3expr (*z3_mk_eq_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_le_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_add_f)(z3ctx, unsigned int, z3expr*) = NULL;
z3expr (*z3_mk_sub_f)(z3ctx, unsigned int, z3expr*) = NULL;
z3expr (*z3_mk_mul_f)(z3ctx, unsigned int, z3expr*) = NULL;
z3expr (*z3_mk_bvule_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvadd_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvsub_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvmul_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvor_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvand_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_concat_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvshl_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvlshr_f)(z3ctx, z3expr, z3expr) = NULL;
z3expr (*z3_mk_bvnot_f)(z3ctx, z3expr) = NULL;
z3expr (*z3_mk_sign_ext_f)(z3ctx, unsigned int, z3expr) = NULL;
z3expr (*z3_mk_extract_f)(z3ctx, unsigned int, unsigned int, z3expr) = NULL;
z3ctx (*z3_mk_context_f)(z3cfg) = NULL;
z3solver (*z3_mk_solver_f)(z3ctx) = NULL;
void (*z3_solver_inc_ref_f)(z3ctx, z3solver) = NULL;
void (*z3_solver_dec_ref_f)(z3ctx, z3solver) = NULL;
z3cfg (*z3_mk_config_f)(void) = NULL;
void (*z3_del_config_f)(z3cfg) = NULL;
void (*z3_set_param_value_f)(z3cfg, const char*, const char*) = NULL;
z3expr (*z3_mk_numeral_f)(z3ctx, const char*, z3sort) = NULL;
void (*z3_inc_ref_f)(z3ctx, z3expr) = NULL;

// Load the z3 dynamic library.
// Returns 0 on success, non-zero if there is some sort of error.
int z3_load()
{
    if (z3_mk_config_f) {
        // The z3 library has already been loaded. Don't load it again.
        return 0;
    }

    void* z3h = dlopen("libz3.so", RTLD_NOW);
    if (!z3h) {
        return 1;
    }

    z3_mk_bool_sort_f = dlsym(z3h, "Z3_mk_bool_sort");
    z3_mk_int_sort_f = dlsym(z3h, "Z3_mk_int_sort");
    z3_mk_bv_sort_f = dlsym(z3h, "Z3_mk_bv_sort");
    z3_mk_string_symbol_f = dlsym(z3h, "Z3_mk_string_symbol");
    z3_mk_func_decl_f = dlsym(z3h, "Z3_mk_func_decl");
    z3_mk_app_f = dlsym(z3h, "Z3_mk_app");
    z3_solver_get_model_f = dlsym(z3h, "Z3_solver_get_model");
    z3_model_eval_f = dlsym(z3h, "Z3_model_eval");
    z3_model_get_const_interp_f = dlsym(z3h, "Z3_model_get_const_interp");
    z3_get_bool_value_f = dlsym(z3h, "Z3_get_bool_value");
    z3_get_numeral_string_f = dlsym(z3h, "Z3_get_numeral_string");
    z3_solver_check_f = dlsym(z3h, "Z3_solver_check");
    z3_del_context_f = dlsym(z3h, "Z3_del_context");
    z3_solver_assert_f = dlsym(z3h, "Z3_solver_assert");
    z3_mk_true_f = dlsym(z3h, "Z3_mk_true");
    z3_mk_false_f = dlsym(z3h, "Z3_mk_false");
    z3_mk_and_f = dlsym(z3h, "Z3_mk_and");
    z3_mk_not_f = dlsym(z3h, "Z3_mk_not");
    z3_mk_ite_f = dlsym(z3h, "Z3_mk_ite");
    z3_mk_eq_f = dlsym(z3h, "Z3_mk_eq");
    z3_mk_le_f = dlsym(z3h, "Z3_mk_le");
    z3_mk_add_f = dlsym(z3h, "Z3_mk_add");
    z3_mk_sub_f = dlsym(z3h, "Z3_mk_sub");
    z3_mk_mul_f = dlsym(z3h, "Z3_mk_mul");
    z3_mk_bvule_f = dlsym(z3h, "Z3_mk_bvule");
    z3_mk_bvadd_f = dlsym(z3h, "Z3_mk_bvadd");
    z3_mk_bvsub_f = dlsym(z3h, "Z3_mk_bvsub");
    z3_mk_bvmul_f = dlsym(z3h, "Z3_mk_bvmul");
    z3_mk_bvor_f = dlsym(z3h, "Z3_mk_bvor");
    z3_mk_bvand_f = dlsym(z3h, "Z3_mk_bvand");
    z3_mk_concat_f = dlsym(z3h, "Z3_mk_concat");
    z3_mk_bvshl_f = dlsym(z3h, "Z3_mk_bvshl");
    z3_mk_bvlshr_f = dlsym(z3h, "Z3_mk_bvlshr");
    z3_mk_bvnot_f = dlsym(z3h, "Z3_mk_bvnot");
    z3_mk_sign_ext_f = dlsym(z3h, "Z3_mk_sign_ext");
    z3_mk_extract_f = dlsym(z3h, "Z3_mk_extract");
    z3_mk_context_f = dlsym(z3h, "Z3_mk_context");
    z3_mk_solver_f = dlsym(z3h, "Z3_mk_solver");
    z3_solver_inc_ref_f = dlsym(z3h, "Z3_solver_inc_ref");
    z3_solver_dec_ref_f = dlsym(z3h, "Z3_solver_dec_ref");
    z3_mk_config_f = dlsym(z3h, "Z3_mk_config");
    z3_del_config_f = dlsym(z3h, "Z3_del_config");
    z3_set_param_value_f = dlsym(z3h, "Z3_set_param_value");
    z3_mk_numeral_f = dlsym(z3h, "Z3_mk_numeral");
    z3_inc_ref_f = dlsym(z3h, "Z3_inc_ref");
    return 0;
}

z3sort z3_mk_bool_sort(z3ctx a) { return z3_mk_bool_sort_f(a); }
z3sort z3_mk_int_sort(z3ctx a) { return z3_mk_int_sort_f(a); }
z3sort z3_mk_bv_sort(z3ctx b, unsigned int c) { return z3_mk_bv_sort_f(b, c); }
z3sym z3_mk_string_symbol(z3ctx b, const char* c) { return z3_mk_string_symbol_f(b, c); }
z3decl z3_mk_func_decl(z3ctx a, z3sym b, unsigned int c, z3sort* d, z3sort e) { return z3_mk_func_decl_f(a, b, c, d, e); }
z3expr z3_mk_app(z3ctx a, z3decl b, unsigned int c, z3expr* d, z3expr e) { return z3_mk_app_f(a, b, c, d, e); }
z3model z3_solver_get_model(z3ctx b, z3solver c) { return z3_solver_get_model_f(b, c); }
z3bool z3_model_eval(z3ctx a, z3model b, z3expr c, z3bool d, z3expr* e) { return z3_model_eval_f(a, b, c, d, e); }
z3expr z3_model_get_const_interp(z3ctx a, z3model b, z3decl c) { return z3_model_get_const_interp_f(a, b, c); }
z3lbool z3_get_bool_value(z3ctx b, z3expr c) { return z3_get_bool_value_f(b, c); }
char* z3_get_numeral_string(z3ctx b, z3expr c) { return z3_get_numeral_string_f(b, c); }
z3lbool z3_solver_check(z3ctx b, z3solver c) { return z3_solver_check_f(b, c); }
void z3_del_context(z3ctx a) { z3_del_context_f(a); }
void z3_solver_assert(z3ctx a, z3solver b, z3expr c) { z3_solver_assert_f(a, b, c); }
z3expr z3_mk_true(z3ctx a) { return z3_mk_true_f(a); }
z3expr z3_mk_false(z3ctx a) { return z3_mk_false_f(a); }
z3expr z3_mk_and(z3ctx a, unsigned int b, z3expr* c) { return z3_mk_and_f(a, b, c); }
z3expr z3_mk_not(z3ctx b, z3expr c) { return z3_mk_not_f(b, c); }
z3expr z3_mk_ite(z3ctx a, z3expr b, z3expr c, z3expr d) { return z3_mk_ite_f(a, b, c, d); }
z3expr z3_mk_eq(z3ctx a, z3expr b, z3expr c) { return z3_mk_eq_f(a, b, c); }
z3expr z3_mk_le(z3ctx a, z3expr b, z3expr c) { return z3_mk_le_f(a, b, c); }
z3expr z3_mk_add(z3ctx a, unsigned int b, z3expr* c) { return z3_mk_add_f(a, b, c); }
z3expr z3_mk_sub(z3ctx a, unsigned int b, z3expr* c) { return z3_mk_sub_f(a, b, c); }
z3expr z3_mk_mul(z3ctx a, unsigned int b, z3expr* c) { return z3_mk_mul_f(a, b, c); }
z3expr z3_mk_bvule(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvule_f(a, b, c); }
z3expr z3_mk_bvadd(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvadd_f(a, b, c); }
z3expr z3_mk_bvsub(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvsub_f(a, b, c); }
z3expr z3_mk_bvmul(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvmul_f(a, b, c); }
z3expr z3_mk_bvor(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvor_f(a, b, c); }
z3expr z3_mk_bvand(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvand_f(a, b, c); }
z3expr z3_mk_concat(z3ctx a, z3expr b, z3expr c) { return z3_mk_concat_f(a, b, c); }
z3expr z3_mk_bvshl(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvshl_f(a, b, c); }
z3expr z3_mk_bvlshr(z3ctx a, z3expr b, z3expr c) { return z3_mk_bvlshr_f(a, b, c); }
z3expr z3_mk_bvnot(z3ctx a, z3expr b) { return z3_mk_bvnot_f(a, b); }
z3expr z3_mk_sign_ext(z3ctx a, unsigned int b, z3expr c) { return z3_mk_sign_ext_f(a, b, c); }
z3expr z3_mk_extract(z3ctx a, unsigned int b, unsigned int c, z3expr d) { return z3_mk_extract_f(a, b, c, d); }
z3ctx z3_mk_context(z3cfg a) { return z3_mk_context_f(a); }
z3solver z3_mk_solver(z3ctx a) { return z3_mk_solver_f(a); }
void z3_solver_inc_ref(z3ctx a, z3solver b) { z3_solver_inc_ref_f(a, b); }
void z3_solver_dec_ref(z3ctx a, z3solver b) { z3_solver_dec_ref_f(a, b); }
z3cfg z3_mk_config(void) { return z3_mk_config_f(); }
void z3_del_config(z3cfg a) { z3_del_config_f(a); }
void z3_set_param_value(z3cfg a, const char* b, const char* c) { z3_set_param_value_f(a, b, c); }
z3expr z3_mk_numeral(z3ctx a, const char* b, z3sort c) { return z3_mk_numeral_f(a, b, c); }
void z3_inc_ref(z3ctx a, z3expr b) { z3_inc_ref_f(a, b); }

