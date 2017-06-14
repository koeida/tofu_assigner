:- [misc].
:- [tofu_procs].
:- [test_workers].

:- begin_tests(tofu).
test(not_available) :-
    not(available([block(1,2), block(4,5), block(10,12)], block(4.5,5.5))).

test(available) :-
    available([block(1,2), block(4,5), block(10,12)], block(13,15)).

% Create a test assignment that forces subsequent shifts.
test(assign_jobs_subseq, [nondet,fail]) :-
    assign_jobs([j(kettle5, 1, block(9,12)),
                j(kettle5, 1, block(12,13))],
                _).

% Create a test day that forces overlap
test(assign_jobs_overlap, [nondet,fail]) :-
    assign_jobs([j(kettle5, 1, block(9,12)),
                 j(kettle5, 1, block(9,12))],
                _).
            
% Create a test day where times, but not days, overlap
test(assign_jobs_overlap, [nondet]) :-
    assign_jobs([j(kettle5, 1, block(9,12)),
                 j(kettle5, 2, block(9,12))],
                _).

% Create a test day where times are subsequent, but
% on different days
test(assign_jobs_overlap, [nondet]) :-
    assign_jobs([j(kettle5, 1, block(9,12)),
                 j(kettle5, 2, block(12,13))],
                _).

test(append_to_dict1) :-
    append_to_dict(a,1, test{}, D),
    D = test{a:[1]}.

test(append_to_dict2, [nondet]) :-
    append_to_dict(a,1, test{a:[0]}, D),
    D = test{a:[0,1]}.

:- end_tests(tofu).
