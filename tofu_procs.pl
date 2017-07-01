:- [misc].

overlap(S1-E1,S2-E2) :- max(S1,S2) < min(E1,E2).

available([],_).
available([B|Bs], S) :-
    not(overlap(B,S)),
    available(Bs,S).

works_job(Jobname,Jobs) :-
    member(Jobname,Jobs).
works_job(_,Jobs) :-
    member(any,Jobs).

assign(j(JobName, Day, S-E), P) :- 
    worker(P,Avails,Jobs,_),
    nth1(Day, Avails, Avail),
    works_job(JobName,Jobs),
    available(Avail,S-E).

% subseq(b1,b2) holds if b1 is followed immediately by b2 or vice versa
subseq(_-T, T-_).
subseq(T-_, _-T).

% Glom together a person and day of the week into an atom
job_key(Person,DayOfWeek,Key) :-
    atom_string(Person,PStr),
    number_string(DayOfWeek,DStr),
    string_concat(PStr,DStr,KeyS),
    atom_string(Key,KeyS).

assign_jobs_dict_housekeeping(P, CurJob, ADict,ADictNew,Alljobs,AlljobsNew,Key) :-
    CurJob = j(_, DayOfWeek, B),
    job_key(P,DayOfWeek,Key),
    append_to_dict(Key, B,  ADict,    ADictNew),
    append_to_dict(P, CurJob,  Alljobs, AlljobsNew).

% The actual tofu assigning code.
assign_jobs_int([],[], ADict, ADict, Alljobs).
assign_jobs_int([CurJob|Js],[ass(CurJob,P)|As], ADict, ADict, Alljobs) :-
    CurJob = j(_, DayOfWeek, B),
    format("~w\t~w~n",[CurJob,P]),
    assign(CurJob,P),
    assign_jobs_dict_housekeeping(P, CurJob, ADict, ADictNew, Alljobs, AlljobsNew,Key),
    not(bad_work(ADictNew.get(Key), AlljobsNew,P)),
    assign_jobs_int(Js, As, ADictNew, ADictOut,AlljobsNew).
assign_jobs(Js,As) :-
    not(jobs_unavailable(Js,UAs)),
    assign_jobs_int(Js, As, _{}, _,_{}).

jobs_unavailable([CurJob|Js],[CurJob|UAs]) :-
    CurJob = j(_, DayOfWeek, B),
    findall(X,assign(CurJob,X),AllAssigns),
    length(AllAssigns,LAss),
    LAss == 0,
    format("impossible to schedule: ~w~n",[CurJob]).
jobs_unavailable([CurJob|Js],UAs) :-
    jobs_unavailable(Js,UAs).

day_to_string(N,S) :- nth1(N,["Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"],S).

assignment_to_string(ass(j(Jobname,DayOfWeek,S-E), Person),AStr) :-
    term_string(Jobname,JobStr),
    term_string(Person,PersonStr),
    day_to_string(DayOfWeek,DayStr),
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

assignment_to_string2(ass(j(Jobname,DayOfWeek,S-E), Person),AStr) :-
    term_string(Jobname,JobStr),
    term_string(Person,PersonStr),
    day_to_string(DayOfWeek,DayStr),
    term_string(S,StartStr),
    term_string(E,EndStr),
    string_concat(JobStr, "\t", JobT),
    string_concat(PersonStr, "\t", PersonT),
    string_concat(StartStr, " to ", Range1),
    string_concat(Range1, EndStr, RangeStr),
    string_concat(RangeStr, "\t", RangeT),
    string_concat(JobT,PersonT,JP),
    string_concat(JP, RangeT,AStr).

comb2(_,[]).
comb2([X|T],[X|Comb]):-comb2(T,Comb).
comb2([_|T],[X|Comb]):-comb2(T,[X|Comb]).

list_workers([],[]).
list_workers([ass(j(_,_,_),P)|As],[P|Ws]) :- list_workers(As,Ws).

rating(As,Rating,Comments) :-
    list_workers(As,Ws), list_to_set(Ws,UWs), length(UWs,NumWorkers), 
    Worker_rating is NumWorkers * 10,
    number_string(NumWorkers,UWStr), concat("Number of distinct workers: ",UWStr,Msg),
    C1 = [Msg],
    Comments = C1,

    findall(_,comb2(As, [ass(j(_,Day,_), P), ass(j(_,Day,_), P)]), DoubleShifts),
    length(DoubleShifts,DSs),
    DSPenalty is DSs * -100, 
    Rating is DSPenalty.

rate_all([A:As],A) :-
	rating(A,R,_),
	R > 0,!.
rate_all([A:As],Best) :-
	rating(A,R,_),
	rate_all(As,Best).

best_day([],Max,R-A).
best_day([R-A|Rs],Max,R-A) :- R > Max, best_day(Rs,Max,R).
best_day([R-A|Rs],Max,Res) :- R =< Max, best_day(Rs,Max,Res).

bad_work(AGs,Alljobs,P) :-
    comb2(AGs,[S1-E1,S2-E2]),
    %without the following line, any 2 shifts for one person in one day is "bad work"
    %(subseq(S1-E1,S2-E2) ; overlap(S1-E2,S2-E2)),!.
    overlap(S1-E1,S2-E2),!.
bad_work(AGs,Alljobs,P) :-
    length(Alljobs.get(P), NumShifts),
    worker(P,_,_,MaxShifts),
    NumShifts > MaxShifts.

overlapping_jobs(Js,Ol) :-
   comb2(Js,[j(N,Day,B1), j(N2,Day,B2)]),
   overlap(B1,B2),
   Ol = [j(N,Day,B1), j(N2,Day,B2)].
