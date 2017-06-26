:- [misc].
:- [tofu_procs].
:- [workers].

test_day(R) :- 
    assign_jobs([
                 j(startup, 1, 6-10),
                 j(ket1, 1, 10-12.5),
                 j(ket2, 1, 12.5-15.5),
                 j(ketcleanhonch, 1, 15.5-18),
                 j(ketcleanhelp, 1, 15.5-18),
                 j(curd1, 1, 7.5-11.5),
                 j(curd2, 1, 11.5-15.5),
                 j(trays1, 1, 8-11.5),
                 j(trays2, 1, 11.5-14.5),
                 j(trays3, 1, 14.5-17.5),
                 j(trayscleanhelp, 1, 17.5-19),
                 j(trayscleanhonch, 1, 17.5-19),
                 j(packhonch1, 1, 9-12),
                 j(packhonch2, 1, 12-16),
                 j(packhonch3, 1, 16-19),
                 j(packhonch4, 1, 19-22),
                 j(packhelp1, 1, 9.5-12.5),
                 j(packhelp2, 1, 12.5-15.5),
                 j(packhelp3, 1, 15.5-18),
                 j(packhelp4, 1, 18-20),
                 j(packhelp2_1, 1, 12.5-15.5),
                 j(packhelp2_2, 1, 15.5-18)
                ],
                R), 
		rating(R,Rating,_),
		format("~n*******************~n"),
		write_assignment_list(R),
		format("Rating: ~w~n", [Rating]),
		format("~n*******************~n").

get_best_day(_) :-
	findall(_,test_day(Rs),_).
