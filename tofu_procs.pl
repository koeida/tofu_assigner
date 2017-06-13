:- [misc].

overlap(block(S1,E1),block(S2,E2)) :- max(S1,S2) < min(E1,E2).

available([],_).
available([B|Bs], S) :-
    not(overlap(B,S)),
    available(Bs,S).

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
    do_write(assign(CurJob,P)),
    assign_jobs_int(Js,As).
assign_jobs(Js,As) :-
    assign_jobs_int(Js,As),
    not(bad_work_combos(As)).

day_to_string(N,S) :- nth1(N,["Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"],S).

assignment_to_string(ass(j(Jobname,DayOfWeek,Block), Person),AStr) :-
    term_string(Jobname,JobStr),
    term_string(Person,PersonStr),
    day_to_string(DayOfWeek,DayStr),
    block(S,E) = Block,
    term_string(S,StartStr),
    term_string(E,EndStr),
    string_concat("Assign ", PersonStr,S1),
    string_concat(S1, " from ", S2),
    string_concat(S2, StartStr, S3),
    string_concat(S3, " to ", S4),
    string_concat(S4, EndStr, S5),
    string_concat(S5, " on ", S6),
    string_concat(S6, DayStr, S7),
    string_concat(S7, " for job ", S8),
    string_concat(S8, JobStr, AStr).

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

comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

% True if As contains any overlapping or subsequent jobs
bad_work_combos(As) :-
    comb2(As,[ass(j(_,D,B1),P),ass(j(_,D,B2),P)]),
    (subseq(B1,B2) ; overlap(B1,B2)).
