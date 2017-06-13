max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- X < Y.

min(X,Y,X) :- X =< Y.
min(X,Y,Y) :- X > Y.

overlap(block(S1,E1),block(S2,E2)) :- max(S1,S2) < min(E1,E2).

available([],_).
available([B|Bs], S) :-
    not(overlap(B,S)),
    available(Bs,S).

worker(
    keegan, 
    [[block(1,2), block(4,5), block(10,12)],
     [block(3,4), block(4,5), block(10,12)],
     [block(1,2), block(4,5), block(10,12)],
     [block(3,4), block(4,5), block(10,12)],
     [block(7,9), block(4,5), block(10,12)],
     [block(9,10), block(4,5), block(10,12)],
     [block(1,2), block(4,5), block(10,12)]],
    [kettle1,kettle2]).
worker(
    adder, 
    [[block(4,5), block(10,12)],
     [block(4,5), block(10,12)],
     [block(4,5), block(10,12)],
     [block(4,5), block(10,12)],
     [block(4,5), block(10,12)],
     [block(4,5), block(10,12)],
     [block(4,5), block(10,12)]],
    [kettle1,kettle2]).
worker(
    megan,
    [[block(8,12)],
     [block(8,12)],
     [block(8,12)],
     [block(8,12)],
     [block(8,12)],
     [block(8,12)],
     [block(8,12)]],
    [startup,pack1honch,pack2honch]).
worker(
    puck,
    [[block(10,14)],
     [block(10,14)],
     [block(10,14)],
     [block(10,14)],
     [block(10,14)],
     [block(10,14)],
     [block(10,14)]],
    [kettle1,kettle2]).
worker(
    kathryn,
    [[block(4,5), block(7,9)],
     [block(4,5), block(7,9)],
     [block(4,5), block(7,9)],
     [block(4,5), block(7,9)],
     [block(4,5), block(7,9)],
     [block(4,5), block(7,9)],
     [block(4,5), block(7,9)]],
    [kettle1,kettle2,pack1help,pack2help]).
worker(
    rayne,
    [[block(11,16)],
     [block(11,16)],
     [block(11,16)],
     [block(11,16)],
     [block(11,16)],
     [block(11,16)],
     [block(11,16)]],
    [any]).
worker(
    visBro,
    [[],
     [],
     [],
     [],
     [],
     [],
     []],
    [any]).
worker(
    max,
    [[block(11,12), block(14,15)],
     [block(11,12), block(14,15)],
     [block(11,12), block(14,15)],
     [block(11,12), block(14,15)],
     [block(11,12), block(14,15)],
     [block(11,12), block(14,15)],
     [block(11,12), block(14,15)]],
    [kettle1,kettle2]).

works_job(Jobname,Jobs) :-
    member(Jobname,Jobs).
works_job(_,Jobs) :-
    member(any,Jobs).

assign(j(JobName, Day, Block), P) :- 
    worker(P,Avails,Jobs),
    nth1(Day,Avails, Avail),
    works_job(JobName,Jobs),
    available(Avail,Block).

% subseq(b1,b2) holds if b1 is followed immediately by b2 or vice versa
subseq(block(_,T), block(T,_)).
subseq(block(T,_), block(_,T)).

assign_jobs_int([],[]).
assign_jobs_int([j(N,DayOfWeek,B)|Js], [ass(j(N,DayOfWeek,B), P)|As]) :-
    CurJob = j(N, DayOfWeek, B),
    assign(CurJob,P),
    assign_jobs_int(Js,As).
assign_jobs(Js,As) :-
    assign_jobs_int(Js,As),
    not(bad_work_combos(As)).

make_key(PersonName,DayOfWeek,Key) :-
    number_string(DayOfWeek,DayS),
    atom_string(PersonName,PS),
    string_concat(PS,"_",S1),
    string_concat(S1,DayS,KeyS),
    atom_string(Key,KeyS).

% If key exists, append the value
% otherwise, make a new key with the value.
% I'm surprised this doesn't already exist in the library.
dict_add(Key,Value,DictOld,DictNew) :-
    Val = DictOld.get(Key),
    append(Val,[Value],NewVal),
    DictNew = DictOld.put(Key,NewVal).
dict_add(Key,Value,DictOld,DictNew) :-
    DictNew = DictOld.put(Key,[Value]).

map([],P,[]).
map([X|Xs],P,[Y|Ys]) :-
    Goal =.. [P,X,Y],
    call(Goal),
    map(Xs,P,Ys).

% Take the list of assignments and sort it into a dictionary
% where each key is a name/day_of_week pair. These are then
% easy to use for checking overlaps and subseqs
sort_by_names_days_int([],DO,DO).
sort_by_names_days_int([A|As],Dict,DO) :-
    ass(j(_,DayOfWeek,_),P) = A, 
    make_key(P,DayOfWeek,Key),
    dict_add(Key,A,Dict,NewDict),
    sort_by_names_days_int(As,NewDict,DO).
sort_by_names_days(As,DO) :-
    Dict = names_days{},
    sort_by_names_days_int(As,Dict,DO).

permute_assignments(AllAssignments,Res) :-
    sort_by_names_days(AllAssignments,SortedAssignments),
    map(Res, permutations, SortedAssignments).

% True is Cs is a list of all 2-combinations of Xs
comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

% True if As contains any overlapping or subsequent jobs
bad_work_combos(As) :-
    comb2(As,[ass(j(JName1,D1,B1),P),ass(j(JName2,D2,B2),P)]),
    D1 == D2,
    (subseq(B1,B2) ; overlap(B1,B2)).
    
write_list([]).
write_list([X|Xs]) :-
    format("~w~n",[X]),
    write_list(Xs).


assign_day_subseq() :-
    assign_day([j(kettle5, 1, block(9,12)),
                j(kettle5, 1, block(12,13))],
                R),
    bad_work_combos(R).

assign_test_day2(R) :- 
    assign_day([j(kettle1, 1, block(9,12)),
                j(kettle2, 1, block(12,15)),
                j(pack1help, 1, block(12,14)),
                j(trays1, 1, block(9,12.5)),
                j(trays2, 1, block(1,5)),
                j(startup, 1, block(5,9)),
                j(pack1honch, 1, block(12.5,14)),
                j(pack2honch, 1, block(2,5)),
                j(pack2help, 1, block(2,5)),
                j(pack2honch, 1, block(2,5))],
                R),
    write_list(R).
