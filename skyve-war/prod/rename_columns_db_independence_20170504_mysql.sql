use skyve;

alter table adm_audit add column auditDetail longtext null;
update adm_audit set auditDetail = audit;
alter table adm_audit drop column audit;

alter table adm_documentnumber add column documentnumber varchar(500) DEFAULT NULL;
update adm_documentnumber set documentnumber = number;
alter table adm_documentnumber drop column number;