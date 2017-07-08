:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- [worker_db].
:- [unavailable].

comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

overlap(S1-E1,S2-E2) :- max(S1,S2) < min(E1,E2).

overlapping(As,M,[A1,A2]) :-
	J1 = job(_,_,D,T1),
	J2 = job(_,_,D,T2),
	A1 = assign(M,J1),
	A2 = assign(M,J2),
	comb2(As,[A1,A2]),
	overlap(T1,T2).

missing_workers(Missing) :-
	get_workers(Available),
	findall(worker(E),worker(E),AllWorkers),
	subtract(AllWorkers, Available, Missing).

newfangled_workers(Newf) :-
	get_workers(Available),
	findall(worker(E),worker(E),AllWorkers),
	subtract(Available, AllWorkers, Newf).

get_workers(WSet) :-
	findall(worker(E),worker_unavailable(E,_,_),Workers),
	list_to_set(Workers,WSet).

get_jobs(Jobs) :-
	findall(job(JName,JSkill,JDay,JTime), job(JName,JSkill,JDay,JTime), Jobs).

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

% list_or(+Exprs,-Disjunction)
list_or([L|Ls], Or) :- foldl(disjunction_, Ls, L, Or).
disjunction_(A, B, B#\/A).

output_schedule(Schedule,R) :-
	writeln('Assignments = '),
	findall(_,(
		member(assign(W,J),Schedule),
		format('(~w,~w)~n',[W,J])
	),_),
	format('Rating: ~w~n',[R]).

curmax(-10000000).

load_shifts(Option) :-
	number_string(Option,Os),
	string_concat("week",Os,S1),
	string_concat(S1,".pl",S2),
	consult(S2).

go(Schedule,Option) :-
	load_shifts(Option),
	findall(Schedule-R,
		(schedule(Schedule,Option),
			rating(Schedule,R),
			curmax(CM),
			R > CM,
			abolish(curmax/1),
			asserta(curmax(R)),
			output_schedule(Schedule,R)),
		Schedules),
	length(Schedules,L),
	L > 0.

rating(Schedule,Rating) :-
    findall(_,comb2(Schedule, [assign(P,job(_,_,Day,_)), assign(P,job(_,_,Day,_))]), DoubleShifts),
    length(DoubleShifts,DSs),
    DSPenalty is DSs * -100, 

    findall(_,comb2(Schedule, [assign(P2,J), assign(P2,J2)]), WeeklyShifts),
    length(WeeklyShifts,WSs),
    WSPenalty is WSs * -25,
    Rating is DSPenalty + WSPenalty.

sheets() :-
	missing_workers(Missing),
	format("Missing Sheets:~n"),
	format("~w~n",[Missing]),
	format("~n"),

	newfangled_workers(Newfangled),
	format("Newfangled Workers:~n"),
	format("~w~n",[Newfangled]),
	format("~n").

schedule(Schedule,Option) :-
	
	get_workers(Ws),
	get_jobs(Js),
	
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

	%writeln('Assignments = '),
	%findall(_,(
	%	member(assign(W,J),Assignments),
	%	format('(~w,~w)~n',[W,J])
	%),_),
	Schedule = Assignments.

constraints(Assoc, Es, Ts) :-
	core_constraints(Assoc, Es, Ts),
	skill_const(Assoc,Es,Ts),
	%no_overlap_const(Assoc),
	overlap_const(Assoc,Es,Ts),
	max_shifts_const(Assoc,Es,Ts),
	worker_available_const(Assoc,Es,Ts),
	max_daily_const(Assoc,Es,Ts).
	%no_subseq_const(Assoc).

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

print_list([]).
print_list([X|Xs]) :- format("~w~n",[X]), print_list(Xs).

assignments(As) :- 
	get_workers(Ws),
	get_jobs(Js),
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
	sum(Vars,#=,0) -> true ; (format("~w~n",[Vars]);fail).

max_shifts_const(Assoc,Es,Ts) :-
	maplist(max_shifts_sub(Assoc,Ts),Es).

max_shifts_sub(Assoc,Ts, W) :-
	findall(assign(W,J), 
	        member(J,Ts),
		As),
	assoc_keys_vars(Assoc,As,Vars),
	sum(Vars, #=<, 4).

max_daily_const(Assoc,Es,Ts) :-
	maplist(max_daily_sub(Assoc,Ts),Es).

max_daily_sub(Assoc,Ts, W) :-
	findall(assign(W,job(_,_,Day,_)), 
	        member(job(_,_,Day_),Ts),
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

overlapn(job(_,_,Day,T1),job(_,_,Day,T2)) :- overlap(T1,T2).
consecn(job(_,_,Day,S1-E1), job(_,_,Day,E1-E2)).

overlap_const(Assoc,Es,Ts) :-
	findall(assign(E,T1)-assign(E,T2),
		(member(E,Es),member(T1,Ts), member(T2,Ts), T1 \= T2, (overlapn(T1,T2) ; consecn(T1,T2))),
		As),
	maplist(overlap_const_sub(Assoc),As).

overlap_const_sub(Assoc,A1-A2) :-
	assoc_keys_vars(Assoc,[A1,A2],Vars),
	sum(Vars, #=<, 1).

subseq(T1-T2,T2-T3).

subseqs(As,M,[A1,A2]) :-
	J1 = job(_,_,D,T1),
	J2 = job(_,_,D,T2),
	A1 = assign(M,J1),
	A2 = assign(M,J2),
	comb2(As,[A1,A2]),
	subseq(T1,T2).

no_subseq_const(Assoc) :-
	assignments(As),
	get_workers(Ws),
	findall(Os,
		bagof(O,Ws^As^(member(W,Ws),subseqs(As,W,O)),Os),
		OOs),
	append(OOs,OOFs),
	maplist(no_overlap_sub(Assoc),OOFs).

no_subseq_sub(Assoc,Os) :-
	assoc_keys_vars(Assoc,Os,Vars),
	sum(Vars,#=,0).
