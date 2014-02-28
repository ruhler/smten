
#include "minisat/core/Solver.h"

using namespace SmtenMinisat;

Lit lit(int x)
{
    Lit l;
    l.x = x;
    return l;
}

int unlit(Lit x)
{
    return x.x;
}

extern "C" Solver* minisat_new()
{
    return new Solver();
}

extern "C" void minisat_delete(Solver* s)
{
    delete s;
}

extern "C" int minisat_true(Solver* s)
{
    Var x = s->newVar();
    s->addClause(mkLit(x, true));
    return unlit(mkLit(x, true));
}

extern "C" int minisat_false(Solver* s)
{
    Var x = s->newVar();
    s->addClause(mkLit(x, true));
    return unlit(mkLit(x, false));
}


extern "C" int minisat_var(Solver* s)
{
    return unlit(mkLit(s->newVar(), true));
}

extern "C" int minisat_not(Solver* s, int x)
{
    return unlit(~lit(x));
}

extern "C" int minisat_and(Solver* s, int a, int b)
{
    Lit x = mkLit(s->newVar(), true);
    s->addClause(~x, lit(a));               // x -> a
    s->addClause(~x, lit(b));               // x -> b
    s->addClause(~lit(a), ~lit(b), x);      // a & b ==> x
    return unlit(x);
}

extern "C" int minisat_or(Solver* s, int a, int b)
{
    Lit x = mkLit(s->newVar(), true);
    s->addClause(~lit(a), x);           // a -> x
    s->addClause(~lit(b), x);           // b -> x
    s->addClause(~x, lit(a), lit(b));   // x ==> a | b
    return unlit(x);
}

extern "C" void minisat_assert(Solver* s, int x)
{
    s->addClause(lit(x));
}


// 0 if UNSAT
// 1 if SAT
extern "C" int minisat_check(Solver* s)
{
    if (!s->simplify()) {
        return 0;
    }
    bool r = s->solve();
    return r ? 1 : 0;
}

// 0 if False
// 1 if True
extern "C" int minisat_getvar(Solver* s, int v)
{
    return toInt(s->model[var(lit(v))]);
}

