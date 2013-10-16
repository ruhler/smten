
// This library loads and links to the dynamic yices2 library at runtime if
// one is found.

#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>

void (*y2_init_f)(void) = NULL;
void (*y2_exit_f)(void) = NULL;
int32_t (*y2_new_uninterpreted_term_f)(int32_t) = NULL;
int32_t (*y2_set_term_name_f)(int32_t, const char*) = NULL;
int32_t (*y2_get_term_by_name_f)(const char*) = NULL;
int32_t (*y2_bv_type_f)(uint32_t) = NULL;
int32_t (*y2_int_type_f)(void) = NULL;
int32_t (*y2_bool_type_f)(void) = NULL;

int32_t (*y2_true_f)(void) = NULL;
int32_t (*y2_false_f)(void) = NULL;
int32_t (*y2_int64_f)(int64_t) = NULL;
int32_t (*y2_not_f)(int32_t) = NULL;
int32_t (*y2_eq_f)(int32_t, int32_t) = NULL;
int32_t (*y2_ite_f)(int32_t, int32_t, int32_t) = NULL;
int32_t (*y2_arith_leq_atom_f)(int32_t, int32_t) = NULL;
int32_t (*y2_add_f)(int32_t, int32_t) = NULL;
int32_t (*y2_sub_f)(int32_t, int32_t) = NULL;
int32_t (*y2_and2_f)(int32_t, int32_t) = NULL;

int32_t (*y2_bvconst_uint64_f)(uint32_t, uint64_t) = NULL;
int32_t (*y2_parse_bvbin_f)(const char*) = NULL;
int32_t (*y2_bvadd_f)(int32_t, int32_t) = NULL;
int32_t (*y2_bvsub_f)(int32_t, int32_t) = NULL;
int32_t (*y2_bvmul_f)(int32_t, int32_t) = NULL;
int32_t (*y2_bvand_f)(int32_t, int32_t) = NULL;
int32_t (*y2_bvor_f)(int32_t, int32_t) = NULL;
int32_t (*y2_bvnot_f)(int32_t) = NULL;
int32_t (*y2_sign_extend_f)(int32_t, uint32_t) = NULL;
int32_t (*y2_bvshl_f)(int32_t, int32_t) = NULL;
int32_t (*y2_bvlshr_f)(int32_t, int32_t) = NULL;
int32_t (*y2_bvextract_f)(int32_t, uint32_t, uint32_t) = NULL;
int32_t (*y2_bvconcat_f)(int32_t, int32_t) = NULL;
void* (*y2_new_context_f)(const void*) = NULL;
void (*y2_free_context_f)(void*) = NULL;
int32_t (*y2_assert_formula_f)(void*, int32_t) = NULL;
int (*y2_check_context_f)(void*, const void*) = NULL;
void* (*y2_get_model_f)(void*, int32_t) = NULL;
void (*y2_free_model_f)(void*) = NULL;
int32_t (*y2_get_bool_value_f)(void*, int32_t, int32_t*) = NULL;
int32_t (*y2_get_int64_value_f)(void*, int32_t, int64_t*) = NULL;
int32_t (*y2_get_bv_value_f)(void*, int32_t, int32_t*) = NULL;
int32_t (*y2_bvle_atom_f)(int32_t, int32_t) = NULL;

// Load the yices2 dynamic library.
// Returns 0 on success, non-zero if there is some sort of error.
int y2_load()
{
    if (y2_init_f) {
        // The yices2 library has already been loaded. Don't load it again.
        return 0;
    }

    void* y2h = dlopen("libyices.so.2.1", RTLD_NOW);
    if (!y2h) {
        return 1;
    }

    y2_init_f = dlsym(y2h, "yices_init");
    y2_exit_f = dlsym(y2h, "yices_exit");
    y2_new_uninterpreted_term_f = dlsym(y2h, "yices_new_uninterpreted_term");
    y2_set_term_name_f = dlsym(y2h, "yices_set_term_name");
    y2_get_term_by_name_f = dlsym(y2h, "yices_get_term_by_name");
    y2_bv_type_f = dlsym(y2h, "yices_bv_type");
    y2_int_type_f = dlsym(y2h, "yices_int_type");
    y2_bool_type_f = dlsym(y2h, "yices_bool_type");
    
    y2_true_f = dlsym(y2h, "yices_true");
    y2_false_f = dlsym(y2h, "yices_false");
    y2_int64_f = dlsym(y2h, "yices_int64");
    y2_not_f = dlsym(y2h, "yices_not");
    y2_eq_f = dlsym(y2h, "yices_eq");
    y2_ite_f = dlsym(y2h, "yices_ite");
    y2_arith_leq_atom_f = dlsym(y2h, "yices_arith_leq_atom");
    y2_add_f = dlsym(y2h, "yices_add");
    y2_sub_f = dlsym(y2h, "yices_sub");
    y2_and2_f = dlsym(y2h, "yices_and2");

    y2_bvconst_uint64_f = dlsym(y2h, "yices_bvconst_uint64");
    y2_parse_bvbin_f = dlsym(y2h, "yices_parse_bvbin");
    y2_bvadd_f = dlsym(y2h, "yices_bvadd");
    y2_bvsub_f = dlsym(y2h, "yices_bvsub");
    y2_bvmul_f = dlsym(y2h, "yices_bvmul");
    y2_bvand_f = dlsym(y2h, "yices_bvand");
    y2_bvor_f = dlsym(y2h, "yices_bvor");
    y2_bvnot_f = dlsym(y2h, "yices_bvnot");
    y2_sign_extend_f = dlsym(y2h, "yices_sign_extend");
    y2_bvshl_f = dlsym(y2h, "yices_bvshl");
    y2_bvlshr_f = dlsym(y2h, "yices_bvlshr");
    y2_bvextract_f = dlsym(y2h, "yices_bvextract");
    y2_bvconcat_f = dlsym(y2h, "yices_bvconcat");
    y2_new_context_f = dlsym(y2h, "yices_new_context");
    y2_free_context_f = dlsym(y2h, "yices_free_context");
    y2_assert_formula_f = dlsym(y2h, "yices_assert_formula");
    y2_check_context_f = dlsym(y2h, "yices_check_context");
    y2_get_model_f = dlsym(y2h, "yices_get_model");
    y2_free_model_f = dlsym(y2h, "yices_free_model");
    y2_get_bool_value_f = dlsym(y2h, "yices_get_bool_value");
    y2_get_int64_value_f = dlsym(y2h, "yices_get_int64_value");
    y2_get_bv_value_f = dlsym(y2h, "yices_get_bv_value");
    y2_bvle_atom_f = dlsym(y2h, "yices_bvle_atom");
    return 0;
}

void y2_init(void) { y2_init_f(); }
void y2_exit(void) { y2_exit_f(); }
int32_t y2_new_uninterpreted_term(int32_t a) { y2_new_uninterpreted_term_f(a); }
int32_t y2_set_term_name(int32_t a, const char* b) { y2_set_term_name_f(a, b); }
int32_t y2_get_term_by_name(const char* a) { return y2_get_term_by_name_f(a); }
int32_t y2_bv_type(uint32_t a) { return y2_bv_type_f(a); }
int32_t y2_int_type(void) { return y2_int_type_f(); }
int32_t y2_bool_type(void) { return y2_bool_type_f(); }

int32_t y2_true(void) { return y2_true_f(); }
int32_t y2_false(void) { return y2_false_f(); }
int32_t y2_int64(int64_t a) { return y2_int64_f(a); }
int32_t y2_not(int32_t a) { return y2_not_f(a); }
int32_t y2_eq(int32_t a, int32_t b) { return y2_eq_f(a, b); }
int32_t y2_ite(int32_t a, int32_t b, int32_t c) { return y2_ite_f(a, b, c); }
int32_t y2_arith_leq_atom(int32_t a, int32_t b) { return y2_arith_leq_atom_f(a, b); }
int32_t y2_add(int32_t a, int32_t b) { return y2_add_f(a, b); }
int32_t y2_sub(int32_t a, int32_t b) { return y2_sub_f(a, b); }
int32_t y2_and2(int32_t a, int32_t b) { return y2_and2_f(a, b); }

int32_t y2_bvconst_uint64(uint32_t a, uint64_t b) { return y2_bvconst_uint64_f(a, b); }
int32_t y2_parse_bvbin(const char* a) { return y2_parse_bvbin_f(a); }
int32_t y2_bvadd(int32_t a, int32_t b) { return y2_bvadd_f(a, b); }
int32_t y2_bvsub(int32_t a, int32_t b) { return y2_bvsub_f(a, b); }
int32_t y2_bvmul(int32_t a, int32_t b) { return y2_bvmul_f(a, b); }
int32_t y2_bvand(int32_t a, int32_t b) { return y2_bvand_f(a, b); }
int32_t y2_bvor(int32_t a, int32_t b) { return y2_bvor_f(a, b); }
int32_t y2_bvnot(int32_t a) { return y2_bvnot_f(a); }
int32_t y2_sign_extend(int32_t a, uint32_t b) { return y2_sign_extend_f(a, b); }
int32_t y2_bvshl(int32_t a, int32_t b) { return y2_bvshl_f(a, b); }
int32_t y2_bvlshr(int32_t a, int32_t b) { return y2_bvlshr_f(a, b); }
int32_t y2_bvextract(int32_t a, uint32_t b, uint32_t c) { return y2_bvextract_f(a, b, c); }
int32_t y2_bvconcat(int32_t a, int32_t b) { return y2_bvconcat_f(a, b); }
void* y2_new_context(const void* a) {y2_new_context_f(a); }
void y2_free_context(void* a) { y2_free_context_f(a); }
int32_t y2_assert_formula(void* a, int32_t b) { return y2_assert_formula_f(a, b); }
int y2_check_context(void* a, const void* b) { return y2_check_context_f(a, b); }
void* y2_get_model(void* a, int32_t b) { return y2_get_model_f(a, b); }
void y2_free_model(void* a) { y2_free_model_f(a); }
int32_t y2_get_bool_value(void* a, int32_t b, int32_t* c) { return y2_get_bool_value_f(a, b, c); }
int32_t y2_get_int64_value(void* a, int32_t b, int64_t* c) { return y2_get_int64_value_f(a, b, c); }
int32_t y2_get_bv_value(void* a, int32_t b, int32_t* c) { return y2_get_bv_value_f(a, b, c); }
int32_t y2_bvle_atom(int32_t a, int32_t b) { return y2_bvle_atom_f(a, b); }
