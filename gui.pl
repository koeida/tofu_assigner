:- use_module(library(pce)).
:- use_module(library('plot/barchart')).
:- use_module(library(autowin)).

thing(foo,10).
thing(bar,90).
thing(sar,110).
thing(tar,15).

barchart :-
	barchart(horizontal).
barchart(HV) :-
	new(W, picture),
	findall(thing(Name,Num),thing(Name,Num),Things),
	length(Things,N),
	send(W, display, new(BC, bar_chart(horizontal, 0, 120, 300, N))),
	forall(member(thing(Name, Val), Things),
		send(BC, append,
			bar_group(Name,
				  bar(val, Val, red)))),
	send(W,open).

schedule :-
	new(@p, picture('Schedule')),
	send(@p, display, new(@bo, box(320,10)), point(0,0)),
	send(@p, display, new(@bo2, box(120,20)), point(10,11)),
	send(@p,open).
