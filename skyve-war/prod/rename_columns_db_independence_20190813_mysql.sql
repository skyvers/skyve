use skyve;

alter table adm_communication add column systemUse bit(1) null;
update adm_communication set systemUse = system;
alter table adm_communication drop column system;
