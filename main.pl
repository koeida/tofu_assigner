:- [tofulib].
:- [worker_db].
:- [unavailable].
:- [shifts].

go() :-
	sheet_check,
        normal_day(fri,D1),
        normal_day(sat,D2),
        normal_day(sun,D3),
        normal_day(mon,D4),
        normal_day(tue,D5),
        normal_day(thu,D6),
        flatten([D4,D3],S1),
        pick_jobs(S1,Shifts),
        gensym(tess,Tess),
	maplist(recorda(Tess),Shifts),
        format("=====New Attempt=====~n"),
        schedule(Tess,Schedule),
        output_schedule(Schedule,0).
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
