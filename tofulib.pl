:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(spawn)).

% SHEET CHECKING
missing_workers(Missing) :-
    get_workers(Available),
    findall(worker(E),worker(E),AllWorkers),
    subtract(AllWorkers, Available, Missing).

newfangled_workers(Newf) :-
    get_workers(Available),
    findall(worker(E),worker(E),AllWorkers),
    subtract(Available, AllWorkers, Newf).

% Sanity check on DB -- make sure there's no discrepancy
% between worker names in skills, prefs, etc.
unknown_worker(W) :-
    findall(worker(E),worker(E), Ws),
    (worker_max(W,_)
    ;worker_skill_max(W,_,_)
    ;worker_skill(W,_)),
    not(member(worker(W),Ws)).

getch(Ch) :-
    get(ChId),
    string_codes(Ch,[ChId]).

sheet_check() :-
    missing_workers(Missing),
    format("Missing Sheets:~n"),
    format("~w~n",[Missing]),
    format("~n"),

    newfangled_workers(Newfangled),
    format("Newfangled Workers:~n"),
    format("~w~n",[Newfangled]),
    format("~n"),

    findall(W,unknown_worker(W),Ws),
    format("Unknown workers found in DB:~n"),
    format("~w~n",[Ws]),
    format("~n"),

    format("Is this all OK [y/n]?" ),
    getch(OK),
    OK == "y".

% MISC

comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

create_assoc_list(Es,Ts,Assoc) :-
    empty_assoc(EmptyAssoc),
    findall(assign(E,T), 
            (member(E,Es),
             member(T,Ts)),
        AssignmentPairs),
    build_assoc_list(EmptyAssoc, AssignmentPairs, Assoc).

build_assoc_list(Assoc, [], Assoc).
build_assoc_list(AssocAcc, [Pair|Pairs], Assoc) :-
    put_assoc(Pair, AssocAcc, _Var, AssocAcc2),
    build_assoc_list(AssocAcc2, Pairs, Assoc).

assoc_keys_vars(Assoc, Keys, Vars) :-
        maplist(assoc_key_var(Assoc), Keys, Vars).
assoc_key_var(Assoc, Key, Var) :- get_assoc(Key, Assoc, Var).

list_or([L|Ls], Or) :- foldl(disjunction_, Ls, L, Or).
disjunction_(A, B, B#\/A).

print_list([]).
print_list([X|Xs]) :- format("~w~n",[X]), print_list(Xs).

% CONSTRAINTS

constraints(Assoc, Es, Ts) :-
    core_constraints(Assoc, Es, Ts),
    skill_const(Assoc,Es,Ts),
    overlap_const(Assoc,Es,Ts),
    max_shifts_const(Assoc,Es,Ts),
    worker_available_const(Assoc,Es,Ts),
    max_daily_const(Assoc,Es,Ts),
    max_per_skill_const(Assoc,Es,Ts).

% core_constraints(+Assoc,+Employees,+Tasks)
core_constraints(Assoc,Es,Ts) :-
    maplist(core_constraints_disj(Assoc,Es),Ts).

% core_constraints_disj(+Assoc,+Employees,+Task)
% Helper for core_constraints, builds a disjunction of sub-expressions, such that
% one and only one employee must be assigned to Task
core_constraints_disj(Assoc,Es,T) :-
    findall(assign(E,T),member(E,Es),Keys),
    assoc_keys_vars(Assoc,Keys,Vars),
    sum(Vars,#=,1).

assignments(Ws,Js,As) :- 
    findall(assign(W,J), 
            (member(W,Ws),member(J,Js)),
        As).

untrained(worker(Worker),job(_,Skill,_,_)) :-
    findall(S,worker_skill(Worker,S),SS),
    not(worker_skill(Worker,Skill) ; worker_skill(Worker,any)).

skill_const(Assoc,Es,Ts) :-
    findall(assign(W,J), 
            (member(W,Es),member(J,Ts), untrained(W,J)),
        As),
    assoc_keys_vars(Assoc,As,Vars),
    sum(Vars,#=,0).

max_shifts_const(Assoc,Es,Ts) :-
    maplist(max_shifts_sub(Assoc,Ts),Es).

max_shifts_sub(Assoc,Ts, W) :-
    findall(assign(W,J), 
            member(J,Ts),
        As),
    W = worker(WName),
    worker_max(WName,Max),
    assoc_keys_vars(Assoc,As,Vars),
    sum(Vars, #=<, Max).

max_shifts_sub(Assoc,Ts, W) :-
    findall(assign(W,J), 
            member(J,Ts),
        As),
    assoc_keys_vars(Assoc,As,Vars),
    sum(Vars, #=<, 3).

max_daily_const(Assoc,Es,Ts) :-
    maplist(max_daily_sub(Assoc,Ts),Es).

max_daily_sub(Assoc,Ts, W) :-
    Days = [fri,sat,sun,mon,tue,wed,thu],
    maplist(max_daily_sub2(Assoc,Ts,W), Days).
max_daily_sub2(Assoc,Ts,W,Day) :-
    findall(assign(W,job(N1,Sk1,Day,Time1)), 
            member(job(N1,Sk1,Day,Time1),Ts),
        As),
    assoc_keys_vars(Assoc,As,Vars),
    sum(Vars, #=<, 2).

unfree(worker(E),job(_,_,Day,T)) :-
    worker_unavailable(E,Day,T2),
    overlap(T,T2).

worker_available_const(Assoc,Es,Ts) :-
    findall(assign(E,T),
        (member(T,Ts), member(E,Es), unfree(E,T)),
        As),
    assoc_keys_vars(Assoc,As,Vars),
    sum(Vars,#=,0).

consecn(job(_,_,Day,S1-E1), job(_,_,Day,E1-E2)).

overlap_const(Assoc,Es,Ts) :-
    findall(assign(E,T1)-assign(E,T2),
        (member(E,Es),member(T1,Ts), member(T2,Ts), T1 \= T2, (job_overlap(T1,T2) ; consecn(T1,T2))),
        As),
    maplist(overlap_const_sub(Assoc),As).

overlap_const_sub(Assoc,A1-A2) :-
    assoc_keys_vars(Assoc,[A1,A2],Vars),
    sum(Vars, #=<, 1).

% Constrains shifts to fulfill worker_skill_max settings,
% which allow a worker to say things like, "I only want three pack
% shifts per week."
max_per_skill_const(Assoc,Es,Ts) :-
    maplist(max_per_skill_sub(Assoc,Ts),Es).

max_per_skill_sub(Assoc, Ts, W) :-
    worker(WName),
    findall(sm(Skill,MaxNum),worker_skill_max(WName,Skill,MaxNum),WSs),
    maplist(max_per_skill_sub2(Assoc,Ts,W), WSs). 

max_per_skill_sub2(Assoc, Ts, W, sm(Skill,Max)) :-
    findall(assign(W,J),
        (member(J,Ts), J = job(_,Skill,_,_)),
        SAs),
    assoc_keys_vars(Assoc, SAs, Vars),
    sum(Vars, #=<, Max).

% MAIN 

overlap(S1-E1,S2-E2) :- max(S1,S2) < min(E1,E2).

job_overlap(job(_,_,Day,T1),job(_,_,Day,T2)) :- overlap(T1,T2).

subseq(_-T2,T2-_).

subseqs(As,M,[A1,A2]) :-
    J1 = job(_,_,D,T1),
    J2 = job(_,_,D,T2),
    A1 = assign(M,J1),
    A2 = assign(M,J2),
    comb2(As,[A1,A2]),
    subseq(T1,T2).

get_workers(WSet) :-
    findall(worker(E),worker_unavailable(E,_,_),Workers),
    list_to_set(Workers,WSet).

get_jobs(DBId,Jobs) :-
    findall(
        job(JName,JSkill,JDay,JTime), 
        recorded(DBId, job(JName,JSkill,JDay,JTime),_),
        Jobs).

output_schedule(Schedule,R) :-
    writeln('Assignments = '),
    findall(_,(
        member(assign(W,J),Schedule),
        format('(~w,~w)~n',[W,J])
    ),_),
    format('Rating: ~w~n',[R]).

load_shifts(Option) :-
    number_string(Option,Os),
    string_concat("week",Os,S1),
    string_concat(S1,".pl",S2),
    consult(S2).

rating(Schedule,Rating) :-
    findall(_,comb2(Schedule, [assign(P,job(_,_,Day,_)), assign(P,job(_,_,Day,_))]), DoubleShifts),
    length(DoubleShifts,DSs),
    DSPenalty is DSs * -100, 

    findall(_,comb2(Schedule, [assign(P2,J), assign(P2,J2)]), WeeklyShifts),
    length(WeeklyShifts,WSs),
    WSPenalty is WSs * -25,

    Rating is DSPenalty + WSPenalty.


schedule(DBId, Schedule) :-
    get_workers(Ws),
    get_jobs(DBId, Js),
    
    %Assoc contains every possible assignment as a key
    create_assoc_list(Ws,Js,Assoc),
    assoc_to_keys(Assoc,AssocKeys),
    assoc_to_values(Assoc,AssocValues),
    AssocValues ins 0..1,
    constraints(Assoc, Ws, Js),
    label(AssocValues),

    findall(Assignment,
        (member(Assignment,AssocKeys),
        get_assoc(Assignment,Assoc,1)),
        Assignments),

    Schedule = Assignments.


test_day(Day,Schedule) :-
    schedules(Day, Schedule),
    format("."),
    flush_output,
    gensym(Day,DBId),
    maplist(recorda(DBId),Schedule),
    schedule(DBId,_) -> format("~n~w day(s) is schedulable.~n", [Day]) 
              ; format("~nCannot schedule ~w day(s).~n",[Day]). 

max_days_parallel() :-
    spawn(ignore(test_day(1,_))),
    spawn(ignore(test_day(2,_))),
    spawn(ignore(test_day(3,_))),
    spawn(ignore(test_day(4,_))),
    spawn(ignore(test_day(5,F5))),
    await(F5),
    format("~nDONE~n").

max_days() :-
    test_day(1,R1),
    format("~n~nJOBS 1~n~n"),
    format("~w~n~n",[R1]),
    test_day(2,R2),
    format("~n~nJOBS 2~n~n"),
    format("~w~n~n",[R2]),
    test_day(3,R3),
    format("~n~nJOBS 3~n~n"),
    format("~w~n~n",[R3]),
    test_day(4,R4),
    format("~n~nJOBS 4~n~n"),
    format("~w~n~n",[R4]),
    test_day(5,R5),
    format("~n~nJOBS 5~n~n"),
    format("~w~n~n",[R5]),
    format("~nDONE~n").

curmax(-10000000).
