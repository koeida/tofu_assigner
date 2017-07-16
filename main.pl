:- [tofulib].
:- [worker_db].
:- [unavailable].
:- [shifts].

go() :-
        normal_day(wed,D3),
        flatten([D3],Shifts),
        format("~w~n",[Shifts]),
	maplist(recorda(tess),Shifts),
	sheet_check,
        schedule(tess,Schedule),
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
