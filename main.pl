:- [tofulib].
:- [worker_db].
:- [unavailable].
:- [shifts].

go() :-
	Shifts = [job(startup,startup,tue,6-10),job(ket1,ket,tue,10-12.5),job(ket2,ket,tue,12.5-15.5),job(ketcleanhonch,ketcleanhonch,tue,15.5-18),job(ketcleanhelp,ketcleanhelp,tue,15.5-18),job(curd1,curd,tue,7.5-11.5),job(curd2,curd,tue,11.5-15.5),job(trays1,trays,tue,8-11.5),job(trays2,trays,tue,11.5-14.5),job(trays3,trays,tue,14.5-17.5),job(trayscleanhelp,trayscleanhelp,tue,17.5-18.5),job(trayscleanhonch,trayscleanhonch,tue,17.5-18.5),job(packhonch1,packhonch,tue,9-12),job(packhonch2,packhonch,tue,12-16),job(packhonch3,packhonch,tue,16-19),job(packhonch4,packhonch,tue,19-22),job(packhelp1,packhelp,tue,9.5-12.5),job(packhelp2,packhelp,tue,12.5-15.5),job(packhelp3,packhelp,tue,15.5-18),job(packhelp4,packhelp,tue,18-20),job(packhelp2_1,packhelp,tue,12.5-15.5),job(packhelp2_2,packhelp,tue,15.5-18)],
	maplist(recorda(foofoo),Shifts),
	sheet_check,
	findall(Schedule-R,
		(schedule(foofoo,Schedule),
			rating(Schedule,R),
			curmax(CM),
			R >= CM,
			abolish(curmax/1),
			asserta(curmax(R)),
			output_schedule(Schedule,R)),
		Schedules).
