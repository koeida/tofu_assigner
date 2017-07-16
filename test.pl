:- [tofulib].

skill(curd).
skill(ket).
skill(ketcleanhelp).
skill(ketcleanhonch).
skill(packhelp).
skill(packhonch).
skill(startup).
skill(trays).
skill(trayscleanhelp).
skill(trayscleanhonch).

make_tmpdb :-
    assert(worker(arlo)),
    assert(worker_unavailable(arlo,tue,10-12)),
    assert(worker_skill(arlo,ket)),
    assert(worker_skill(arlo,ketcleanhonch)),
    assert(worker_skill(arlo,startup)),
    assert(worker_skill(arlo,packhelp)),
    assert(worker_skill_max(arlo, ket, 2)),
    assert(worker_skill_max(arlo, ketcleanhonch, 2)),
    assert(worker_max(arlo,4)).

unmake_tmpdb :-
    retractall(worker/1),
    retractall(worker_skill/2),
    retractall(worker_skill_max/3),
    retractall(worker_max/2).

runshifts(Shifts,Schedule) :-
    gensym(tess,DBId),
    maplist(recorda(DBId),Shifts),
    schedule(DBId,Schedule).

:- begin_tests(misc).

test(consecutive_fail, [fail]) :-
    consecn(job(Arlo,packhelp,mon,10-11), job(Arlo,packhelp,mon,12-14)).

test(consecutive) :-
    consecn(job(Arlo,packhelp,mon,10-12), job(Arlo,packhelp,mon,12-14)).

:- end_tests(misc).

:- begin_tests(constraints).

% Fails because Arlo doesn't have the packhonch skill
test(skill_fail, [setup(make_tmpdb),cleanup(unmake_tmpdb),nondet,fail]) :-
    Shifts = [
              job(p,packhonch,mon,7-14)
             ],
    runshifts(Shifts,_).

% Fails because it violates the availability constraint, i.e: Arlo is 
% unavailable Tuesday 10-12, so this shift can't be filled. 
test(unavailable_fail, [setup(make_tmpdb),cleanup(unmake_tmpdb),nondet,fail]) :-
    Shifts = [
              job(p,packhelp,tue,7-14)
             ],
    runshifts(Shifts,_).

% Fails because the two shifts with overlapping times violates
% the no-overlap constraint
test(overlap_fail, [setup(make_tmpdb),cleanup(unmake_tmpdb),nondet,fail]) :-
    Shifts = [
              job(p,packhelp,mon,6-10),
              job(p,packhelp,mon,7-14)
             ],
    runshifts(Shifts,_).


% fails because it violates the 2-max-shifts-per-day constraint
test(max_per_day_fail, [setup(make_tmpdb),cleanup(unmake_tmpdb),nondet,fail]) :-
    Shifts = [
              job(p,packhelp,mon,6-10),
              job(p,packhelp,mon,11-12),
              job(p,packhelp,mon,13-14)
             ],
    runshifts(Shifts,_).

test(max_per_day_succeed, [setup(make_tmpdb),cleanup(unmake_tmpdb),nondet]) :-
    Shifts = [
              job(p,packhelp,mon,6-10),
              job(p,packhelp,mon,11-12)
             ],
    runshifts(Shifts,_).


% Fails b/c it violates arlo's 2-max-per-week ket constraint
test(max_per_skill, [setup(make_tmpdb),cleanup(unmake_tmpdb),nondet,fail]) :-
    Shifts = [
              job(startup,ket,mon,6-10),
              job(startup,ket,fri,11-12),
              job(startup,ket,wed,13-14)
             ],
    runshifts(Shifts,_).

test(max_per_skill2, [setup(make_tmpdb),cleanup(unmake_tmpdb),nondet]) :-
    Shifts = [
              job(startup,ket,mon,6-10),
              job(startup,ket,mon,13-14)
             ],
    runshifts(Shifts,_).

:- end_tests(constraints).

