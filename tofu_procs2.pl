:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- [worker_db].


worker_unavailable(anande,mon,12-16).
worker_unavailable(anande,tue,12-16).
worker_unavailable(anande,wed,12-16).
worker_unavailable(anande,wed,17-23).

worker_unavailable(clementine,fri,3.5-6.5).
worker_unavailable(clementine,sat,4.5-22).
worker_unavailable(clementine,sun,6-6.5).
worker_unavailable(elijah,fri,8-14).
worker_unavailable(elijah,sun,11.5-23).

job(startup, startup, fri, 6-10).
job(ket1, ket, fri, 10-12.5).
job(ket2, ket, fri, 12.5-15.5).
job(ketcleanhonch, ketclean, fri, 15.5-18).
job(ketcleanhelp, ketcleanhelp, fri, 15.5-18).
job(curd1, curd, fri, 7.5-11.5).
job(curd2, curd, fri, 11.5-15.5).
job(trays1, trays, fri, 8-11.5).
job(trays2, trays, fri, 11.5-14.5).
job(trays3, trays, fri, 14.5-17.5).
job(trayscleanhelp, trayscleanhelp, fri, 17.5-19).
job(trayscleanhonch, trayscleanhonch, fri, 17.5-19).
job(packhonch1, packhonch, fri, 9-12).
job(packhonch2, packhonch, fri, 12-16).
job(packhonch3, packhonch, fri, 16-19).
job(packhonch4, packhonch, fri, 19-22).
job(packhelp1, packhelp, fri, 9.5-12.5).
job(packhelp2, packhelp, fri, 12.5-15.5).
job(packhelp3, packhelp, fri, 15.5-18).
job(packhelp4, packhelp, fri, 18-20).
job(packhelp2_1, packhelp, fri, 12.5-15.5).
job(packhelp2_2, packhelp, fri, 15.5-18).


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

get_workers(Workers) :-
	findall(worker(E),worker(E),Workers).

get_jobs(Jobs) :-
	findall(job(JName,JSkill,JDay,JTime), job(JName,JSkill,JDay,JTime), Jobs).

create_assoc_list(Es,Ts,Assoc) :-
	empty_assoc(EmptyAssoc),
	findall(assign(E,T), (member(E,Es), member(T,Ts)), AssignmentPairs),
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

schedule(Schedule) :-
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

	writeln('Assignments = '),
	findall(_,(
		member(assign(W,J),Assignments),
		format('(~w,~w)~n',[W,J])
	),_),
	Schedule = Assignments.

constraints(Assoc, Es, Ts) :-
	core_constraints(Assoc, Es, Ts),
	skill_const(Assoc,Es,Ts),
	no_overlap_const(Assoc),
	max_shifts_const(Assoc,Es,Ts).

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
	sum(Vars,#=,0).

max_shifts_const(Assoc,Es,Ts) :-
	maplist(max_shifts_sub(Assoc,Ts),Es).

max_shifts_sub(Assoc,Ts, W) :-
	findall(assign(W,J), 
	        member(J,Ts),
		As),
	assoc_keys_vars(Assoc,As,Vars),
	sum(Vars, #=<, 3).
	

no_overlap_const(Assoc) :-
	assignments(As),
	get_workers(Ws),
	findall(Os,
		bagof(O,Ws^As^(member(W,Ws),overlapping(As,W,O)),Os),
		OOs),
	append(OOs,OOFs),
	maplist(no_overlap_sub(Assoc),OOFs).

no_overlap_sub(Assoc,Os) :-
	assoc_keys_vars(Assoc,Os,Vars),
	sum(Vars,#=<,1).
