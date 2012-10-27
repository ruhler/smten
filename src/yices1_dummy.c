
#define NULL 0

#include <yices_c.h>

yices_context yices1_mk_context()
{
    return NULL;
}

void yices1_del_context(yices_context ctx)
{
}

int yices1_parse_command(yices_context ctx, const char* s)
{
    return 0;
}

lbool yices1_check(yices_context ctx)
{
    return l_undef;
}

yices_model yices1_get_model(yices_context ctx)
{
    return NULL;
}

void yices1_display_model(yices_model m)
{
}

void yices1_enable_type_checker(int flag)
{
}

const char* yices1_get_last_error_message(void)
{
    return "yices1 dummy. nothing will work";
}

lbool yices1_get_value(yices_model m, yices_var_decl v)
{
    return l_undef;
}

int yices1_get_int_value(yices_model m, yices_var_decl d, long* value)
{
    return 0;
}

int yices1_get_bitvector_value(yices_model m, yices_var_decl d, unsigned n, int bv[])
{
    return 0;
}

yices_var_decl yices1_get_var_decl_from_name(yices_context ctx, const char* name)
{
    return NULL;
}

