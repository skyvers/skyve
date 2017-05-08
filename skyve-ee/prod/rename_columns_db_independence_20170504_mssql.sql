use skyve;

alter table adm_audit add auditDetail varchar(max) null;
GO
update adm_audit set auditDetail = audit;
GO
alter table adm_audit drop column audit;
GO

alter table adm_documentnumber add documentnumber varchar(500) DEFAULT NULL;
GO
update adm_documentnumber set documentnumber = number;
GO
alter table adm_documentnumber drop column number;
GO