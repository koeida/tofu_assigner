worker(arlo).

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

% worker_max(WorkerName, Skill, MaxNum
% How many shifts a given worker wants for work falling under this particular skill
% per week.
worker_skill_max(arlo, ket, 2).
worker_skill_max(arlo, ketcleanhonch, 2).
worker_skill_max(arlo, startup, 2).

worker_max(arlo,5).

worker_skill(arlo,ket).
worker_skill(arlo,ketcleanhonch).
worker_skill(arlo,ketcleanhelp).
worker_skill(arlo,startup).

worker_unavailable(arlo, sun, 0-0).

