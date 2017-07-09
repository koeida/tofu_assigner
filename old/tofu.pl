:- [misc].
:- [tofu_procs].
:- [workers].

test_day2(R) :-
    assign_jobs([
    		 j(ket1,1,9-10),
		 j(packhelp1, 1, 9-10),
		 j(packhonch1, 1, 9-10)
		 ],R),
		format("~n*****~n", [Rating]),
		rating(R,Rating,_),
		write_assignment_list(R),
		format("Rating: ~w~n", [Rating]).
test_day(R) :- 
    assign_jobs([
    j(startup, 4, 6-10),
    j(ket1, 4, 10-12.5),
    j(ket2, 4, 12.5-15.5),
    j(ketcleanhonch, 4, 15.5-18),
    j(ketcleanhelp, 4, 15.5-18),
    j(curd1, 4, 7.5-11.5),
    j(curd2, 4, 11.5-15.5),
    j(trays1, 4, 8-11.5),
    j(trays2, 4, 11.5-14.5),
    j(trays3, 4, 14.5-17.5),
    j(trayscleanhelp, 4, 17.5-19),
    j(trayscleanhonch, 4, 17.5-19),
    j(packhonch1, 4, 9-12),
    j(packhonch2, 4, 12-16),
    j(packhonch3, 4, 16-19),
    j(packhonch4, 4, 19-22),
    j(packhelp1, 4, 9.5-12.5),
    j(packhelp2, 4, 12.5-15.5),
    j(packhelp3, 4, 15.5-18),
    j(packhelp4, 4, 18-20),
    j(packhelp2_1, 4, 12.5-15.5),
    j(packhelp2_2, 4, 15.5-18),

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
		 j(packhonch1, 1, 9-10.5),
		 j(packhonch1, 1, 10.5-12),
                 j(packhonch2, 1, 12-16),
                 j(packhonch3, 1, 16-19),
                 j(packhonch4, 1, 19-22),
                 j(packhelp1, 1, 14-17),
                 j(packhelp2, 1, 17-20),
                 j(packhelp2_1, 1, 15-18),

		 j(startup, 3, 6-10),
		 j(ket1, 3, 10-12.5),
		 j(ket2, 3, 12.5-15.5),
		 j(ketcleanhonch, 3, 15.5-18),
		 j(ketcleanhelp, 3, 15.5-18),
		 j(curd1, 3, 7.5-11.5),
		 j(curd2, 3, 11.5-15.5),
		 j(trays1, 3, 8-11.5),
		 j(trays2, 3, 11.5-14.5),
		 j(trays3, 3, 14.5-17.5),
		 j(trayscleanhelp, 3, 17.5-19),
		 j(trayscleanhonch, 3, 17.5-19),
		 j(packhonch1, 3, 9-12),
		 j(packhonch2, 3, 12-16),
                 j(packhonch3, 3, 16-19),
                 j(packhonch4, 3, 19-22),
		 j(packhelp1, 3, 14-17),
		 j(packhelp2, 3, 17-20),
		 j(packhelp2_1, 3, 15-18)
                ],
                R), 
		rating(R,Rating,_),
		write_assignment_list(R),
		format("Rating: ~w~n", [Rating]),
		format("~n*****~n", [Rating]).

get_best_day(_) :-
	findall(_,test_day(_),_).

%! oltest(+Res)
oltest(Res) :-
	overlapping_jobs([
	 j(startup, 3, 6-10),
	 j(ket1, 3, 10-12.5),
	 j(ket2, 3, 12.5-15.5),
	 j(ketcleanhonch, 3, 15.5-18),
	 j(ketcleanhelp, 3, 15.5-18),
	 j(curd1, 3, 7.5-11.5),
	 j(curd2, 3, 11.5-15.5),
	 j(trays1, 3, 8-11.5),
	 j(trays2, 3, 11.5-14.5),
	 j(trays3, 3, 14.5-17.5),
	 j(trayscleanhelp, 3, 17.5-19),
	 j(trayscleanhonch, 3, 17.5-19),
	 j(packhonch1, 3, 9-12),
	 j(packhonch2, 3, 12-16),
	 j(packhonch3, 3, 16-19),
	 j(packhonch4, 3, 19-22),
	 j(packhelp1, 3, 14-17),
	 j(packhelp2, 3, 17-20),
	 j(packhelp2_1, 3, 15-18)],Res).
