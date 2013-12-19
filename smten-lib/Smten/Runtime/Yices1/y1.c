
// This library loads and links to the dynamic yices1 library at runtime if
// one is found.

#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>

typedef void* y1ctx;
typedef void* y1decl;
typedef void* y1expr;
typedef void* y1model;
typedef enum { y1_false=-1, y1_undef, y1_true } y1bool;

y1ctx (*y1_mk_context_f)() = NULL;
void (*y1_del_context_f)(y1ctx) = NULL;
int (*y1_parse_command_f)(y1ctx, const char*) = NULL;
y1bool (*y1_check_f)(y1ctx) = NULL;
void (*y1_assert_f)(y1ctx, y1expr) = NULL;
y1model (*y1_get_model_f)(y1ctx) = NULL;
const char* (*y1_get_last_error_message_f)(void) = NULL;
y1bool (*y1_get_value_f)(y1model, y1decl) = NULL;
int (*y1_get_int_value_f)(y1model, y1decl, long*) = NULL;
int (*y1_get_bitvector_value_f)(y1model, y1decl, unsigned, int v[]) = NULL;
y1decl (*y1_get_var_decl_from_name_f)(y1ctx, const char*) = NULL;
y1expr (*y1_mk_var_from_decl_f)(y1ctx, y1decl) = NULL;
y1expr (*y1_mk_true_f)(y1ctx) = NULL;
y1expr (*y1_mk_false_f)(y1ctx) = NULL;
y1expr (*y1_mk_not_f)(y1ctx, y1expr) = NULL;
y1expr (*y1_mk_bv_not_f)(y1ctx, y1expr) = NULL;
y1expr (*y1_mk_and_f)(y1ctx, y1expr xs[], unsigned) = NULL;
y1expr (*y1_mk_sum_f)(y1ctx, y1expr xs[], unsigned) = NULL;
y1expr (*y1_mk_sub_f)(y1ctx, y1expr xs[], unsigned) = NULL;
y1expr (*y1_mk_ite_f)(y1ctx, y1expr, y1expr, y1expr) = NULL;
y1expr (*y1_mk_eq_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_le_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_bv_add_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_bv_sub_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_bv_mul_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_bv_and_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_bv_or_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_bv_concat_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_bv_sign_extend_f)(y1ctx, y1expr, unsigned) = NULL;
y1expr (*y1_mk_bv_extract_f)(y1ctx, unsigned , unsigned , y1expr) = NULL;
y1expr (*y1_mk_bv_le_f)(y1ctx, y1expr, y1expr) = NULL;
y1expr (*y1_mk_num_f)(y1ctx, int) = NULL;
y1expr (*y1_mk_bv_constant_f)(y1ctx, unsigned, unsigned long) = NULL;
y1expr (*y1_mk_bv_constant_from_array_f)(y1ctx, unsigned, int bv[]) = NULL;
y1expr (*y1_mk_bv_shift_left0_f)(y1ctx, y1expr, unsigned) = NULL;
y1expr (*y1_mk_bv_shift_right0_f)(y1ctx, y1expr, unsigned) = NULL;

// Load the yices1 dynamic library.
// Returns 0 on success, non-zero if there is some sort of error.
int y1_load()
{
    if (y1_mk_context_f) {
        // The yices1 library has already been loaded. Don't load it again.
        return 0;
    }

    void* y1h = dlopen("libyices.so", RTLD_NOW);
    if (!y1h) {
        return 1;
    }

    y1_mk_context_f = dlsym(y1h, "yices_mk_context");
    y1_del_context_f = dlsym(y1h, "yices_del_context");
    y1_parse_command_f = dlsym(y1h, "yices_parse_command");
    y1_check_f = dlsym(y1h, "yices_check");
    y1_assert_f = dlsym(y1h, "yices_assert");
    y1_get_model_f = dlsym(y1h, "yices_get_model");
    y1_get_last_error_message_f = dlsym(y1h, "yices_get_last_error_message");
    y1_get_value_f = dlsym(y1h, "yices_get_value");
    y1_get_int_value_f = dlsym(y1h, "yices_get_int_value");
    y1_get_bitvector_value_f = dlsym(y1h, "yices_get_bitvector_value");
    y1_get_var_decl_from_name_f = dlsym(y1h, "yices_get_var_decl_from_name");
    y1_mk_var_from_decl_f = dlsym(y1h, "yices_mk_var_from_decl");
    y1_mk_true_f = dlsym(y1h, "yices_mk_true");
    y1_mk_false_f = dlsym(y1h, "yices_mk_false");
    y1_mk_not_f = dlsym(y1h, "yices_mk_not");
    y1_mk_bv_not_f = dlsym(y1h, "yices_mk_bv_not");
    y1_mk_and_f = dlsym(y1h, "yices_mk_and");
    y1_mk_sum_f = dlsym(y1h, "yices_mk_sum");
    y1_mk_sub_f = dlsym(y1h, "yices_mk_sub");
    y1_mk_ite_f = dlsym(y1h, "yices_mk_ite");
    y1_mk_eq_f = dlsym(y1h, "yices_mk_eq");
    y1_mk_le_f = dlsym(y1h, "yices_mk_le");
    y1_mk_bv_add_f = dlsym(y1h, "yices_mk_bv_add");
    y1_mk_bv_sub_f = dlsym(y1h, "yices_mk_bv_sub");
    y1_mk_bv_mul_f = dlsym(y1h, "yices_mk_bv_mul");
    y1_mk_bv_and_f = dlsym(y1h, "yices_mk_bv_and");
    y1_mk_bv_or_f = dlsym(y1h, "yices_mk_bv_or");
    y1_mk_bv_concat_f = dlsym(y1h, "yices_mk_bv_concat");
    y1_mk_bv_sign_extend_f = dlsym(y1h, "yices_mk_bv_sign_extend");
    y1_mk_bv_extract_f = dlsym(y1h, "yices_mk_bv_extract");
    y1_mk_bv_le_f = dlsym(y1h, "yices_mk_bv_le");
    y1_mk_num_f = dlsym(y1h, "yices_mk_num");
    y1_mk_bv_constant_f = dlsym(y1h, "yices_mk_bv_constant");
    y1_mk_bv_constant_from_array_f = dlsym(y1h, "yices_mk_bv_constant_from_array");
    y1_mk_bv_shift_left0_f = dlsym(y1h, "yices_mk_bv_shift_left0");
    y1_mk_bv_shift_right0_f = dlsym(y1h, "yices_mk_bv_shift_right0");
    return 0;
}

y1ctx y1_mk_context() { return  y1_mk_context_f(); }
void y1_del_context(y1ctx a) { y1_del_context_f(a); }
int y1_parse_command(y1ctx a, const char* b) { return  y1_parse_command_f(a, b); }
y1bool y1_check(y1ctx a) { return  y1_check_f(a); }
void y1_assert(y1ctx a, y1expr b) { y1_assert_f(a, b); }
y1model y1_get_model(y1ctx a) { return  y1_get_model_f(a); }
const char* y1_get_last_error_message(void) { return  y1_get_last_error_message_f(); }
y1bool y1_get_value(y1model a, y1decl b) { return  y1_get_value_f(a, b); }
int y1_get_int_value(y1model a, y1decl b, long* c) { return  y1_get_int_value_f(a, b, c); }
int y1_get_bitvector_value(y1model a, y1decl b, unsigned c, int d[])
{
    return  y1_get_bitvector_value_f(a, b, c, d);
}

y1decl y1_get_var_decl_from_name(y1ctx a, const char* b) { return  y1_get_var_decl_from_name_f(a, b); }
y1expr y1_mk_var_from_decl(y1ctx a, y1decl b) { return  y1_mk_var_from_decl_f(a, b); }
y1expr y1_mk_true(y1ctx a) { return  y1_mk_true_f(a); }
y1expr y1_mk_false(y1ctx a) { return  y1_mk_false_f(a); }
y1expr y1_mk_not(y1ctx a, y1expr b) { return  y1_mk_not_f(a, b); }
y1expr y1_mk_bv_not(y1ctx a, y1expr b) { return  y1_mk_bv_not_f(a, b); }
y1expr y1_mk_and(y1ctx a, y1expr b[], unsigned c) { return  y1_mk_and_f(a, b, c); }
y1expr y1_mk_sum(y1ctx a, y1expr b[], unsigned c) { return  y1_mk_sum_f(a, b, c); }
y1expr y1_mk_sub(y1ctx a, y1expr b[], unsigned c) { return  y1_mk_sub_f(a, b, c); }
y1expr y1_mk_ite(y1ctx a, y1expr b, y1expr c, y1expr d) { return  y1_mk_ite_f(a, b, c, d); }
y1expr y1_mk_eq(y1ctx a, y1expr b, y1expr c) { return  y1_mk_eq_f(a, b, c); }
y1expr y1_mk_le(y1ctx a, y1expr b, y1expr c) { return  y1_mk_le_f(a, b, c); }
y1expr y1_mk_bv_add(y1ctx a, y1expr b, y1expr c) { return  y1_mk_bv_add_f(a, b, c); }
y1expr y1_mk_bv_sub(y1ctx a, y1expr b, y1expr c) { return  y1_mk_bv_sub_f(a, b, c); }
y1expr y1_mk_bv_mul(y1ctx a, y1expr b, y1expr c) { return  y1_mk_bv_mul_f(a, b, c); }
y1expr y1_mk_bv_and(y1ctx a, y1expr b, y1expr c) { return  y1_mk_bv_and_f(a, b, c); }
y1expr y1_mk_bv_or(y1ctx a, y1expr b, y1expr c) { return  y1_mk_bv_or_f(a, b, c); }
y1expr y1_mk_bv_concat(y1ctx a, y1expr b, y1expr c) { return  y1_mk_bv_concat_f(a, b, c); }
y1expr y1_mk_bv_sign_extend(y1ctx a, y1expr b, unsigned c) { return  y1_mk_bv_sign_extend_f(a, b, c); }
y1expr y1_mk_bv_extract(y1ctx a, unsigned b, unsigned c, y1expr d) { return  y1_mk_bv_extract_f(a, b, c, d); }
y1expr y1_mk_bv_le(y1ctx a, y1expr b, y1expr c) { return  y1_mk_bv_le_f(a, b, c); }
y1expr y1_mk_num(y1ctx a, int b) { return  y1_mk_num_f(a, b); }
y1expr y1_mk_bv_constant(y1ctx a, unsigned b, unsigned long c)
{
    return  y1_mk_bv_constant_f(a, b, c);
}

y1expr y1_mk_bv_constant_from_array(y1ctx a, unsigned b, int c[])
{
    return y1_mk_bv_constant_from_array_f(a, b, c);
}

y1expr y1_mk_bv_shift_left0(y1ctx a, y1expr b, unsigned c) { return  y1_mk_bv_shift_left0_f(a, b, c); }
y1expr y1_mk_bv_shift_right0(y1ctx a, y1expr b, unsigned c) { return  y1_mk_bv_shift_right0_f(a, b, c); }

