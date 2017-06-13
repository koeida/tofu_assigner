:- [misc].
:- [tofu_procs].
:- [test_workers].

:- begin_tests(tofu).
test(not_available) :-
    not(available([block(1,2), block(4,5), block(10,12)], block(4.5,5.5))).

test(available) :-
    available([block(1,2), block(4,5), block(10,12)], block(13,15)).

test(sort_by_names_days,[nondet]) :-
    sort_by_names_days([
        ass(j(foo,1,block(1,2)),keeg),
        ass(j(bak,1,block(2,3)),keeg)
        ],
        R),
    R == names_days{keeg_1:[ass(j(foo,1,block(1,2)),keeg),ass(j(bak,1,block(2,3)),keeg)]}.

test(sort_by_names_days2,[nondet]) :-
    sort_by_names_days([
        ass(j(foo,1,block(1,2)),keeg),
        ass(j(bak,1,block(2,3)),mark)
        ],
        R),
    R == names_days{keeg_1:[ass(j(foo,1,block(1,2)),keeg)],
                    mark_1:[ass(j(bak,1,block(2,3)),mark)]}.

test(sort_by_names_days2,[nondet]) :-
    sort_by_names_days([
        ass(j(foo,1,block(1,2)),keeg),
        ass(j(bar,1,block(1,2)),keeg),
        ass(j(bak,1,block(2,3)),mark)
        ],
        R),
    R == names_days{keeg_1:[ass(j(foo,1,block(1,2)),keeg), ass(j(bar,1,block(1,2)),keeg)],
                    mark_1:[ass(j(bak,1,block(2,3)),mark)]}.

test(sort_by_names_days2,[nondet]) :-
    sort_by_names_days([], R),
    R == names_days{}.

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

:- end_tests(tofu).
