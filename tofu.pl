:- [misc].
:- [tofu_procs].
:- [workers].

assign_test_day2(R) :- 
    assign_jobs([j(kettle1, 1, block(9,12)),
                 j(kettle2, 1, block(12,15)),
                 j(pack1help, 1, block(12,14)),
                 j(trays1, 1, block(9,12.5)),
                 j(trays2, 1, block(1,5)),
                 j(startup, 1, block(5,9)),
                 j(pack1honch, 1, block(12.5,14)),
                 j(pack2honch, 2, block(2,5)),
                 j(pack2help, 2, block(2,5)),
                 j(pack2honch, 2, block(2,5))],
                R),
    format("~n~n=======RESULTS=======~n~n",[]),
    write_assignment_list(R).
