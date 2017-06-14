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

job_key(Person,DayOfWeek,Key) :-
    atom_string(Person,PStr),
    number_string(DayOfWeek,DStr),
    string_concat(PStr,DStr,KeyS),
    atom_string(Key,KeyS).

assign_jobs_int([],[], ADict, ADict).
assign_jobs_int([CurJob|Js], [ass(CurJob,P)|As], ADict, ADictOut) :-
    CurJob = j(_, DayOfWeek, B),
    assign(CurJob,P),
    job_key(P,DayOfWeek,Key),
    append_to_dict(Key, B,  ADict,    ADictNew),
    not(bad_work(ADictNew.get(Key))),
    assign_jobs_int(Js, As, ADictNew, ADictOut).
assign_jobs(Js,As) :-
    assign_jobs_int(Js, As, _{}, _).

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

comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

no_bad_work([]).
no_bad_work(AGs) :- not(comb2(AGs, [block(S1,E1),block(S2,E2)])).
no_bad_work(AGs) :-
    comb2(AGs,[B1,B2]),
    B1 = block(S1,E1),
    B2 = block(S2,E2),
    not(subseq(B1,B2) ; overlap(B1,B2)).

bad_work(AGs) :-
    comb2(AGs,[B1,B2]),
    B1 = block(S1,E1),
    B2 = block(S2,E2),
    (subseq(B1,B2) ; overlap(B1,B2)),!.

