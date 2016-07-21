#define N 4
#define OFF (-2)

#define T1 2
#define T2 3
#define T3 1

/* timer[i] -- How many units of time process with pid i wants to wait, OFF = none */
short timer[N + 2] = OFF;
/* An auxiliary timer used to implement bounded(T, I) */
short left[N + 2] = 0;

/* Waits T units of time */
#define sleep(T) { timer[_pid] = (T); timer[_pid] == 0; }

/* Executes I at most in at most T units of time */
#define bounded(T, I) {      \
    left[_pid] = T;          \
    do                       \
    /* Stop waiting */       \
    :: left[_pid] = 0;       \
       I;                    \
       break;                \
    :: left[_pid] > 0 ->     \
    /* Wait 1 unit of time */\
       sleep(1);             \
       left[_pid]--;         \
    od                       \
    }                        \

#define some_time() { bounded(T3, skip); }

short kto = 0;

/* Who set kto first, who entered the crit. section first */
short first_kto = -1, first_crit = -1;

/* How many processes are in the critical section right now */
short in_crit = 0;

active [N] proctype P() {
    end_P: do
    ::  
        some_time();
        kto == 0 ->

Wants:
        bounded(T1, { 
            d_step {
                kto = _pid;

                if
                :: first_kto == -1 ->
                    first_kto = _pid;
                :: else ->
                    skip
                fi;
            }
        });

        sleep(T2);
        some_time();

        if
        :: kto == _pid ->
progress_CS:
            in_crit++;

CritSection:
            assert(in_crit == 1);
            d_step {
                if
                :: first_crit == -1 ->
                    first_crit = _pid;
                :: else ->
                    skip
                fi;
            }
            some_time();
            skip;
            d_step{
              in_crit--;
              kto = 0;
            }
        :: else ->
            skip
        fi
    od
}

/* Uncomment active to verify, that when a process sets kto, then in T1 + T2 units of time
   some process will have entered the critical secion */
//active
proctype checker() {
end_checker:
    do
    :: first_kto != -1 ->
        sleep(T1 + T2 + 1); /* Wait strictly longer than T1 + T2 units of time */
        assert(first_crit != -1);
        break;
    od;

    do
    :: sleep(1); /* Just to ensure timeouts */
    od
}

init {
    int i;
    end_init: do
    /* When everybody is sleeping */
    :: timeout ->
        d_step {
            for (i : 1 .. N) {
                if
                :: timer[i] >= 0 ->
                    timer[i]--;
                :: else -> skip
                fi;
            }
        }
    od
}


ltl liveness { ([] (P[1]@Wants -> <>P[1]@CritSection)) }
ltl first_kto_crit { [] ((first_crit != -1) -> (first_kto == first_crit)) }

