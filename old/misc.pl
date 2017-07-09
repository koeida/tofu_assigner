max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- X < Y.

min(X,Y,X) :- X =< Y.
min(X,Y,Y) :- X > Y.

write_list([]).
write_list([X|Xs]) :-
    format("~w~n",[X]),
    write_list(Xs).

write_assignment_list([]).
write_assignment_list([X|Xs]) :-
    assignment_to_string(X,Str),
    format("~w~n",[Str]),
    write_assignment_list(Xs).

do_write(Goal) :- 
    term_string(Goal,GStr),
    format("~w~n", [GStr]),
    Goal*->true ; format("failed").

append_to_dict(Key, E, Dict, NewDict) :-
    Val = Dict.get(Key),
    append(Val,[E],NewVal),
    NewDict = Dict.put(Key, NewVal),!.
append_to_dict(Key, E, Dict, NewDict) :-
    NewDict = Dict.put(Key,[E]).
