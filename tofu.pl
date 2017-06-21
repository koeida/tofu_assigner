:- [misc].
:- [tofu_procs].
:- [workers].

assign_test_day2(R,Ad) :- 
    assign_jobs([
                 j(startup, 1, block(6,10)),
                 j(ket1, 1, block(10,12.5)),
                 j(ket2, 1, block(12.5,15.5)),
                 j(ketcleanhonch, 1, block(15.5,18)),
                 j(ketcleanhelp, 1, block(15.5,18)),
                 j(curd1, 1, block(7.5,11.5)),
                 j(curd2, 1, block(11.5,15.5)),
                 j(trays1, 1, block(8,11.5)),
                 j(trays2, 1, block(11.5,14.5)),
                 j(trays3, 1, block(14.5,17.5)),
                 j(trayscleanhelp, 1, block(17.5,19)),
                 j(trayscleanhonch, 1, block(17.5,19)),
                 j(packhonch1, 1, block(9,12)),
                 j(packhonch2, 1, block(12,16)),
                 j(packhonch3, 1, block(16,19)),
                 j(packhonch4, 1, block(19,22)),
                 j(packhelp1, 1, block(9.5,12.5)),
                 j(packhelp2, 1, block(12.5,15.5)),
                 j(packhelp3, 1, block(15.5,18)),
                 j(packhelp4, 1, block(18,20)),
                 j(packhelp2_1, 1, block(12.5,15.5)),
                 j(packhelp2_2, 1, block(15.5,18))
                ],
                R),
    format("~n~n=======RESULTS=======~n~n",[]),
    write_assignment_list(R).
