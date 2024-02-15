use skyve;

alter table adm_usermonthlyhits add column hityear int null;
update adm_usermonthlyhits set hityear = year;
alter table adm_usermonthlyhits drop column year;

alter table adm_usermonthlyhits add column hitmonth int null;
update adm_usermonthlyhits set hitmonth = month;
alter table adm_usermonthlyhits drop column month;
