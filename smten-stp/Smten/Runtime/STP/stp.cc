
#include <stdlib.h>

// STP c interface.
//
// This interface is a wrapper over the c_interface provided by STP.
// It handles garbage collection of STP expressions.
//
// TODO: support multiple simultaneous solvers.
// Currently we assume there is only one solver active at a time.
// This means we can manage a single list of expressions to free rather
// than a per-solver list.

typedef void* VC;
typedef void* Expr;
typedef void* Type;

struct FreeList {
    Expr expr;
    FreeList* next;
};

FreeList* freelist = NULL;

Expr gc(Expr e)
{
    FreeList* old = freelist;
    freelist = new FreeList();
    freelist->expr = e;
    freelist->next = old;
    return e;
}

extern "C" VC vc_createValidityChecker(void);
extern "C" VC stp_createValidityChecker(void)
{
    return vc_createValidityChecker();
}

extern "C" void vc_Destroy(VC vc);
extern "C" void vc_DeleteExpr(Expr e);
extern "C" void stp_Destroy(VC vc)
{
    while (freelist) {
        FreeList* old = freelist;
        freelist = freelist->next;
        vc_DeleteExpr(old->expr);
        delete old;
    }
    vc_Destroy(vc);
}

extern "C" Type vc_boolType(VC vc);
extern "C" Type stp_boolType(VC vc)
{
    // Don't gc this, because STP already does for us.
    return vc_boolType(vc);
}

extern "C" Type vc_bvType(VC vc, int no_bits);
extern "C" Type stp_bvType(VC vc, int no_bits)
{
    // Don't gc this, because STP already does for us.
    return vc_bvType(vc, no_bits);
}

extern "C" Expr vc_varExpr(VC vc, const char* name, Type type);
extern "C" Expr stp_varExpr(VC vc, const char* name, Type type)
{
    return gc(vc_varExpr(vc, name, type));
}

extern "C" void vc_assertFormula(VC vc, Expr e);
extern "C" void stp_assertFormula(VC vc, Expr e)
{
    return vc_assertFormula(vc, e);
}

extern "C" int vc_query(VC vc, Expr e);
extern "C" int stp_query(VC vc, Expr e)
{
    return vc_query(vc, e);
}

extern "C" Expr vc_trueExpr(VC vc);
extern "C" Expr stp_trueExpr(VC vc)
{
    return gc(vc_trueExpr(vc));
}

extern "C" Expr vc_falseExpr(VC vc);
extern "C" Expr stp_falseExpr(VC vc)
{
    return gc(vc_falseExpr(vc));
}

extern "C" Expr vc_notExpr(VC vc, Expr child);
extern "C" Expr stp_notExpr(VC vc, Expr child)
{
    return gc(vc_notExpr(vc, child));
}

extern "C" Expr vc_orExpr(VC vc, Expr left, Expr right);
extern "C" Expr stp_orExpr(VC vc, Expr left, Expr right)
{
    return gc(vc_orExpr(vc, left, right));
}

extern "C" Expr vc_andExpr(VC vc, Expr left, Expr right);
extern "C" Expr stp_andExpr(VC vc, Expr left, Expr right)
{
    return gc(vc_andExpr(vc, left, right));
}

extern "C" Expr vc_eqExpr(VC vc, Expr child0, Expr child1);
extern "C" Expr stp_eqExpr(VC vc, Expr child0, Expr child1)
{
    return gc(vc_eqExpr(vc, child0, child1));
}

extern "C" Expr vc_iteExpr(VC vc, Expr conditional, Expr ifthenpart, Expr elsepart);
extern "C" Expr stp_iteExpr(VC vc, Expr conditional, Expr ifthenpart, Expr elsepart)
{
    return gc(vc_iteExpr(vc, conditional, ifthenpart, elsepart));
}

extern "C" Expr vc_bvConstExprFromLL(VC vc, int n_bits, unsigned long long value);
extern "C" Expr stp_bvConstExprFromLL(VC vc, int n_bits, unsigned long long value)
{
    return gc(vc_bvConstExprFromLL(vc, n_bits, value));
}

extern "C" Expr vc_bvConstExprFromDecStr(VC vc, int width, const char* decimalInput);
extern "C" Expr stp_bvConstExprFromDecStr(VC vc, int width, const char* decimalInput)
{
    return gc(vc_bvConstExprFromDecStr(vc, width, decimalInput));
}

extern "C" Expr vc_bvPlusExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvPlusExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvPlusExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvMinusExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvMinusExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvMinusExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvMultExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvMultExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvMultExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvDivExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvDivExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvDivExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvModExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvModExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvModExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_sbvDivExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_sbvDivExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_sbvDivExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_sbvModExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_sbvModExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_sbvModExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_sbvRemExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_sbvRemExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_sbvRemExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvOrExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvOrExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvOrExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvAndExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvAndExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvAndExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvNotExpr(VC vc, Expr child);
extern "C" Expr stp_bvNotExpr(VC vc, Expr child)
{
    return gc(vc_bvNotExpr(vc, child));
}

extern "C" Expr vc_bvLeftShiftExprExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvLeftShiftExprExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvLeftShiftExprExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvRightShiftExpr(VC vc, int sh_amt, Expr child);
extern "C" Expr stp_bvRightShiftExpr(VC vc, int sh_amt, Expr child)
{
    return gc(vc_bvRightShiftExpr(vc, sh_amt, child));
}

extern "C" Expr vc_bvRightShiftExprExpr(VC vc, int n_bits, Expr left, Expr right);
extern "C" Expr stp_bvRightShiftExprExpr(VC vc, int n_bits, Expr left, Expr right)
{
    return gc(vc_bvRightShiftExprExpr(vc, n_bits, left, right));
}

extern "C" Expr vc_bvConcatExpr(VC vc, Expr left, Expr right);
extern "C" Expr stp_bvConcatExpr(VC vc, Expr left, Expr right)
{
    return gc(vc_bvConcatExpr(vc, left, right));
}

extern "C" Expr vc_bvSignExtend(VC vc, Expr child, int nbits);
extern "C" Expr stp_bvSignExtend(VC vc, Expr child, int nbits)
{
    return gc(vc_bvSignExtend(vc, child, nbits));
}

extern "C" Expr vc_bvExtract(VC vc, Expr child, int high_bit_no, int low_bit_no);
extern "C" Expr stp_bvExtract(VC vc, Expr child, int high_bit_no, int low_bit_no)
{
    return gc(vc_bvExtract(vc, child, high_bit_no, low_bit_no));
}

extern "C" Expr vc_getCounterExample(VC vc, Expr e);
extern "C" Expr stp_getCounterExample(VC vc, Expr e)
{
    return gc(vc_getCounterExample(vc, e));
}

extern "C" int vc_isBool(Expr e);
extern "C" int stp_isBool(Expr e)
{
    return vc_isBool(e);
}

extern "C" int vc_getBVLength(Expr e);
extern "C" int stp_getBVLength(Expr e)
{
    return vc_getBVLength(e);
}

extern "C" unsigned long long int getBVUnsignedLongLong(Expr e);
extern "C" unsigned long long int stp_getBVUnsignedLongLong(Expr e)
{
    return getBVUnsignedLongLong(e);
}

extern "C" Expr vc_bvLtExpr(VC vc, Expr left, Expr right);
extern "C" Expr stp_bvLtExpr(VC vc, Expr left, Expr right)
{
    return gc(vc_bvLtExpr(vc, left, right));
}

extern "C" Expr vc_bvLeExpr(VC vc, Expr left, Expr right);
extern "C" Expr stp_bvLeExpr(VC vc, Expr left, Expr right)
{
    return gc(vc_bvLeExpr(vc, left, right));
}

extern "C" Expr vc_bvGtExpr(VC vc, Expr left, Expr right);
extern "C" Expr stp_bvGtExpr(VC vc, Expr left, Expr right)
{
    return gc(vc_bvGtExpr(vc, left, right));
}

extern "C" Expr vc_bvGeExpr(VC vc, Expr left, Expr right);
extern "C" Expr stp_bvGeExpr(VC vc, Expr left, Expr right)
{
    return gc(vc_bvGeExpr(vc, left, right));
}

