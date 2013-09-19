
#include "minisat/core/Solver.h"

using namespace Minisat;

extern "C" Solver* minisat_mksolver()
{
    Solver* s = new Solver();
    return s;
}

extern "C" void minisat_delsolver(Solver* s)
{
    delete s;
}

extern "C" Var minisat_mkvar(Solver* s)
{
    s->newVar();
}

extern "C" bool minisat_getvar(Solver* s, Var v)
{
    // Note: this defaults to 'false' for undefined variable values.
    // We might at some point want to return 'undefined' as a distinct value
    // from 'false'.
    return (s->model[v] == l_True);
}

extern "C" bool minisat_issat(Solver* s)
{
    if (!s->simplify()) {
        printf("unsat by simplify\n");
        return false;
    }
    return s->solve();
}

extern "C" void minisat_addclause1(Solver* s, Var v1, bool s1)
{
    printf("clause: %c%i\n", s1 ? '+' : '-', v1);
    s->addClause(mkLit(v1, s1));
}

extern "C" void minisat_addclause2(Solver* s, Var v1, bool s1,
                                   Var v2, bool s2)
{
    printf("clause: %c%i %c%i\n", s1 ? '+' : '-', v1,
                                  s2 ? '+' : '-', v2);
    s->addClause(mkLit(v1, s1), mkLit(v2, s2));
}

extern "C" void minisat_addclause3(Solver* s, Var v1, bool s1,
                                   Var v2, bool s2,
                                   Var v3, bool s3)
{
    printf("clause: %c%i %c%i %c%i\n", s1 ? '+' : '-', v1,
                                       s2 ? '+' : '-', v2,
                                       s3 ? '+' : '-', v3);
    s->addClause(mkLit(v1, s1), mkLit(v2, s2), mkLit(v3, s3));
}

