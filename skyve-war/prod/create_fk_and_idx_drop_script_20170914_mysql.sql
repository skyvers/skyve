select concat('alter table ', table_name, ' drop foreign key ', constraint_name, ';') 
from information_schema.key_column_usage u
where u.referenced_table_name is not null
and u.table_schema = 'bizhub'
union
select distinct concat('alter table ', s.table_name, ' drop index ', s.index_name, ';')
from information_schema.statistics s
left outer join information_schema.table_constraints t
on t.table_schema = s.table_schema
and t.table_name = s.table_name
and t.constraint_name = s.index_name
where t.constraint_name is null
and s.table_schema = 'bizhub'
