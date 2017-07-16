worker(adder).
worker(anande).
worker(arlo).
worker(becky).
worker(birddog).
worker(brandy).
worker(brittany).
worker(cel).
worker(christian).
worker(cj).
worker(claire).
worker(clementine).
worker(daniel).
worker(danjo).
worker(elijah).
worker(emily).
worker(ezra).
worker(fox).
worker(gordon).
worker(jeli).
worker(jen).
worker(johnny).
worker(kami).
worker(kera).
worker(kete).
worker(lindsey).
worker(madge).
worker(mala).
worker(nina).
worker(paul).
worker(pax).
worker(puck).
worker(reynaldo).
worker(rosie).
worker(rowan).
worker(sansann).
worker(scott).
worker(sky).
worker(stephan).
worker(sunya).
worker(tigger).
worker(tim).
worker(trout).

%visitors:
worker(bobby).
worker(crista).
worker(dan).
worker(danny).
worker(erika).
worker(koerner).
worker(shreya).
worker(ox).

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

% worker_skill_max(WorkerName, Skill, MaxNum)
% How many shifts a given worker wants for work 
% falling under this particular skill per week.
worker_skill_max(arlo, ket, 2).
worker_skill_max(arlo, ketcleanhonch, 2).
worker_skill_max(birddog, packhelp, 2).
worker_skill_max(birddog, trayscleanhelp, 2).
worker_skill_max(elijah, curd, 4).

% worker_max(WorkerName, MaxNum)
% How many shifts a given worke wants in total
% per week.
worker_max(adder,6).
worker_max(becky,3).
worker_max(brandy,4).
worker_max(birddog,2).
worker_max(cel,3).
worker_max(christian,2).
worker_max(cj,4).
worker_max(claire,3).
worker_max(danjo,3).
worker_max(daniel,10).
worker_max(emily, 3).
worker_max(ezra, 2).
worker_max(fox,2).
worker_max(jeli, 5).
worker_max(jen, 2).
worker_max(kami,4).
worker_max(kera,3).
worker_max(kete, 4).
worker_max(lindsey,2).
worker_max(madge,1).
worker_max(mala, 2).
worker_max(paul,4).
worker_max(pax,3).
worker_max(puck,4).
worker_max(reynaldo,3).
worker_max(rosie,2).
worker_max(sansann,4).
worker_max(scott,3).
worker_max(sky,3).
worker_max(stephan,2).
worker_max(sunya,3).
worker_max(tim,2).
worker_max(trout,5).
worker_max(johnny,3).
worker_max(rowan,3).

% visitors
worker_skill(erika,packhelp).
worker_skill(bobby,packhelp).
worker_skill(crista,packhelp).
worker_skill(dan,packhelp).
worker_skill(danny,packhelp).
worker_skill(koerner,packhelp).
worker_skill(ox,packhelp).
worker_skill(shreya,packhelp).

worker_skill(adder,ketcleanhonch).
worker_skill(adder,ketcleanhelp).
worker_skill(adder,curd).
worker_skill(adder,trays).
worker_skill(adder,startup).

worker_skill(anande,packhelp).

worker_skill(arlo,ket).
worker_skill(arlo,ketcleanhonch).
worker_skill(arlo,ketcleanhelp).
worker_skill(arlo,startup).

worker_skill(becky,curd).

worker_skill(birddog,packhelp).
worker_skill(birddog,trayscleanhelp).
worker_skill(birddog,ketcleanhelp).

worker_skill(brandy,trays).
worker_skill(brandy,trayscleanhonch).

worker_skill(brittany,packhonch).
worker_skill(brittany,packhelp).
worker_skill(brittany,trays).

worker_skill(cel, packhelp).
worker_skill(cel, trayscleanhonch).
worker_skill(cel, trayscleanhelp).

worker_skill(christian,trays).
worker_skill(christian,trayscleanhonch).
worker_skill(christian,trayscleanhelp).

%cj only does packhonch1
worker_skill(cj,packhonch).
worker_skill(cj,packhelp).
worker_skill(cj,trayscleanhonch).
worker_skill(cj,trayscleanhelp).

worker_skill(claire,packhonch).
worker_skill(claire,packhelp).

worker_skill(clementine, packhelp).

worker_skill(daniel,any).

worker_skill(danjo,ketcleanhonch).
worker_skill(danjo,ketcleanhelp).

%elijah also does prep and meta
worker_skill(elijah, curd).

worker_skill(emily,trays).

worker_skill(erika,packhelp).

worker_skill(ezra, trays).

worker_skill(fox, packhelp).

worker_skill(gordon,ketcleanhelp).

worker_skill(jeli, packhonch).
worker_skill(jeli, packhelp).

% jen only wants to do early shifts
worker_skill(jen,packhonch).
worker_skill(jen,packhelp).

worker_skill(johnny,ket).

worker_skill(kami,trays).
worker_skill(kami,ket).
worker_skill(kami,startup).

worker_skill(kera,curd).
worker_skill(kera,packhelp).

worker_skill(kete, ket).
worker_skill(kete, curd).

worker_skill(lindsey,trayscleanhelp).

worker_skill(madge,packhelp).

worker_skill(mala,packhelp).
worker_skill(mala,trayscleanhelp).
worker_skill(mala,trayscleanhonch).

worker_skill(nina,trays).

worker_skill(paul,ket).

worker_skill(pax,trays).
worker_skill(pax,curd).

worker_skill(puck,ket).
worker_skill(puck,curd).
worker_skill(puck,trays).
worker_skill(puck,trayscleanhelp).
worker_skill(puck,trayscleanhonch).

worker_skill(reynaldo,trays).

worker_skill(rosie,packhonch).

worker_skill(rowan,packhonch).
worker_skill(rowan,packhelp).

worker_skill(sansann,packhelp).
worker_skill(sansann,trayscleanhelp).
worker_skill(sansann,ketcleanhelp).
worker_skill(sansann,ketcleanhonch).

worker_skill(scott,packhonch).
worker_skill(scott,packhelp).
worker_skill(scott,trays).

worker_skill(sky,ket).
worker_skill(sky,ketcleanhonch).
worker_skill(sky,curd).

worker_skill(stephan,packhonch).
worker_skill(stephan,packhelp).

worker_skill(tigger,trays).

worker_skill(tim,curd).

worker_skill(trout,ket).
