:- [misc].
:- [tofu_procs].
:- [workers].

assign_test_day2(R,Ad) :- 
    assign_jobs([
                 j(kettle1, 1, block(1,2)),
                 j(kettle2, 1, block(2,4)),
                 j(pack1help, 1, block(1,3)),
                 j(pack1honch, 1, block(1,3)),
                 j(trays1, 1, block(1,2)),
                 j(trays2, 1, block(2,4)),
                 j(startup, 1, block(4,9)),
                 j(kettle1, 2, block(1,2)),
                 j(kettle2, 2, block(2,4)),
                 j(pack1help, 2, block(1,3)),
                 j(pack1honch, 2, block(1,3)),
                 j(trays1, 2, block(1,2)),
                 j(trays2, 2, block(2,4)),
                 j(startup, 2, block(4,9)),
                 j(kettle1, 3, block(1,2)),
                 j(kettle2, 3, block(2,4)),
                 j(pack1help, 3, block(1,3)),
                 j(pack1honch, 3, block(1,3)),
                 j(trays1, 3, block(1,2)),
                 j(trays2, 3, block(2,4)),
                 j(startup, 3, block(4,9))
                ],
                R),
    format("~n~n=======RESULTS=======~n~n",[]),
    write_assignment_list(R).
