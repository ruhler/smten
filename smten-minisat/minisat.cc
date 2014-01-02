
#include "minisat/core/Solver.h"

using namespace SmtenMinisat;

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
    Var v = s->newVar();
    //fprintf(stderr, "mkvar %i\n", v);
    return v;
}

extern "C" int minisat_getvar(Solver* s, Var v)
{
    //fprintf(stderr, "getvar %i\n", v);
    return (toInt(s->model[v]));
}

// Returns 1 for SAT, 0 for UNSAT
extern "C" int minisat_issat(Solver* s)
{
    if (!s->simplify()) {
        //printf("unsat by simplify\n");
        return 0;
    }
    bool r = s->solve();
    //printf("issat: %s\n", r ? "SAT" : "UNSAT");
    return r ? 1 : 0;
}

extern "C" void minisat_addclause1(Solver* s, Var v1, bool s1)
{
    //fprintf(stderr, "clause: %c%i\n", s1 ? '+' : '-', v1);
    s->addClause(mkLit(v1, s1));
}

extern "C" void minisat_addclause2(Solver* s, Var v1, bool s1,
                                   Var v2, bool s2)
{
    //fprintf(stderr, "clause: %c%i %c%i\n", s1 ? '+' : '-', v1, s2 ? '+' : '-', v2);
    s->addClause(mkLit(v1, s1), mkLit(v2, s2));
}

extern "C" void minisat_addclause3(Solver* s, Var v1, bool s1,
                                   Var v2, bool s2,
                                   Var v3, bool s3)
{
    //fprintf(stderr, "clause: %c%i %c%i %c%i\n", s1 ? '+' : '-', v1, s2 ? '+' : '-', v2, s3 ? '+' : '-', v3);
    s->addClause(mkLit(v1, s1), mkLit(v2, s2), mkLit(v3, s3));
}

