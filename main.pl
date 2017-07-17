:- use_module(library(clpfd)).
:- [tofulib].
:- [worker_db].
:- [unavailable].
:- [shifts].

valid_days(5).
valid_days(4).
valid_days(3).
valid_days(2).

go() :-
    sheet_check,
    findall(sched(S,R,Days),gen_sched(S,R,Days),Scheds),
    qsave_program("schedule_output.pls").
gen_sched(Out,Removed,NumDays) :-
        valid_days(NumDays),
        format("~n======ATTEMPTING ~w DAYS======~n",[NumDays]),
        valid_schedule(NumDays,S1),
        format("=====NEW SCHEDULE=====~n~w~n",[S1]),
        schedule_days(S1,S2,0),
        flatten(S2,S3),
        pick_jobs(S3,Removed,Shifts),
        gensym(tess,Tess),
	maplist(recorda(Tess),Shifts),
        format("=====New Attempt=====~n"),
        once(schedule(Tess,Schedule)),
        assert(fillable_schedule(Schedule,Removed)),
        output_schedule(Schedule,0),
        Out = Schedule.
    %findall(Schedule-R,
    %            (
                    %schedule(foofoo,Schedule),
                    %rating(Schedule,R),
                    %curmax(CM),
                    %R >= CM,
                    %abolish(curmax/1),
                    %asserta(curmax(R)),
                    %output_schedule(Schedule,R)
                    %            ),
                    %Schedules),
                    %length(Schedules,SLength),
                    %SLength > 0.
