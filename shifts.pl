:- use_module(library(clpfd)).

num_matching([],_,0).
num_matching([X|Xs],X,Num) :- num_matching(Xs,X,Num2), Num #= 1 + Num2.
num_matching([X|Xs],Y,Num) :- X #\= Y, num_matching(Xs,Y,Num).

num_to_day(Num,Day) :-
	Days = [fri,sat,sun,mon,tue,wed,thu],
	nth0(Num,Days,Day).

schedule_day(0,DayOfWeekNum,[]).
schedule_day(1,DayOfWeekNum,Res) :-
	num_to_day(DayOfWeekNum,Day),
	normal_day(Day,Res).
schedule_day(2,DayOfWeekNum,Res) :-
	num_to_day(DayOfWeekNum,Day),
	pounder_day(Day,Res).

schedule_days([],[],_).
schedule_days([T|Ts],[S|Ss],N) :-
	schedule_day(T,N,S),
	N2 is N + 1,
	schedule_days(Ts,Ss,N2).

valid_schedule(NumDays, Result) :-
	Days = [Fri,Sat,Sun,Mon,Tue,Wed,Thu],
	Days ins 0..2,
	Sat #= 0,
	Wed #= 0,
	PounderDays #= 1,
	RegularDays #= NumDays - 1,
	num_matching(Days, 2, PounderDays),
	num_matching(Days, 1, RegularDays),
	label(Days),
	Result = Days.

normal_day(Day,[
		job(startup, startup, Day, 6-10),
		job(ket1, ket, Day, 10-12.5),
		job(ket2, ket, Day, 12.5-15.5),
		job(ketcleanhonch, ketcleanhonch, Day, 15.5-18),
		job(ketcleanhelp, ketcleanhelp, Day, 15.5-18),
		job(curd1, curd, Day, 7.5-11.5),
		job(curd2, curd, Day, 11.5-15.5),
		job(trays1, trays, Day, 8-11.5),
		job(trays2, trays, Day, 11.5-14.5),
		job(trays3, trays, Day, 14.5-17.5),
		job(trayscleanhelp, trayscleanhelp, Day, 17.5-19),
		job(trayscleanhonch, trayscleanhonch, Day, 17.5-19),
		job(packhonch1, packhonch, Day, 9-12),
		job(packhonch2, packhonch, Day, 12-16),
		job(packhonch3, packhonch, Day, 16-19),
		job(packhonch4, packhonch, Day, 19-22),
		job(packhelp1, packhelp, Day, 14-17),
		job(packhelp2, packhelp, Day, 17-20),
		job(packhelp2_1, packhelp, Day, 15-18)
                ]
            ).


remove_jobs([],Result,Result).
remove_jobs([JName|Js],Jobs,Result) :-
    delete(Jobs,job(JName,_,_,_),NewJobs),
    remove_jobs(Js,NewJobs,Result).

pounder_day(Day, [
            job(startup, startup, Day, 6-10),
            job(ket1, ket, Day, 10-12.5),
            job(ket2, ket, Day, 12.5-15.5),
            job(ketcleanhonch, ketcleanhonch, Day, 15.5-18),
            job(ketcleanhelp, ketcleanhelp, Day, 15.5-18),
            job(curd1, curd, Day, 7.5-11.5),
            job(curd2, curd, Day, 11.5-15.5),
            job(trays1, trays, Day, 8-11.5),
            job(trays2, trays, Day, 11.5-14.5),
            job(trays3, trays, Day, 14.5-17.5),
            job(trayscleanhelp, trayscleanhelp, Day, 17.5-18.5),
            job(trayscleanhonch, trayscleanhonch, Day, 17.5-18.5),
            job(packhonch1, packhonch, Day, 9-12),
            job(packhonch2, packhonch, Day, 12-16),
            job(packhonch3, packhonch, Day, 16-19),
            job(packhonch4, packhonch, Day, 19-22),
            job(packhelp1, packhelp, Day, 9.5-12.5),
            job(packhelp2, packhelp, Day, 12.5-15.5),
            job(packhelp3, packhelp, Day, 15.5-18),
            job(packhelp4, packhelp, Day, 18-20),
            job(packhelp2_1, packhelp, Day, 12.5-15.5),
            job(packhelp2_2, packhelp, Day, 15.5-18)
	]).

% pick_jobs(+Jobs,-Removed,-Result)
pick_jobs(Jobs,JobsToRemove,Result) :-
    RemovableJobs = [
        packhelp2_1,
        packhelp2_2,
        trayscleanhelp,
        ketcleanhelp,
        packhelp3,
        packhelp4,
        packhelp1,
        packhelp2,
        packhonch4,
        packhonch3,
        packhonch2,
        trays3,
        curd2,
        ket2,
        ket1,
        startup,
        trays2,
        trays1
        ],
    prefix(JobsToRemove,RemovableJobs),
    length(JobsToRemove,L),
    L > 0,
    format("Trying schedule without:~w~n",[JobsToRemove]), 
    remove_jobs(JobsToRemove,Jobs,Result).
