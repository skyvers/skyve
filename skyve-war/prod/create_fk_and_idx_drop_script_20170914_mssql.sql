select 'alter table [' + u.table_schema + '].[' + u.table_name + '] drop constraint ' + u.constraint_name + ';'
from information_schema.key_column_usage u
inner join information_schema.table_constraints c
on u.constraint_name = c.constraint_name
where c.constraint_type = 'FOREIGN KEY'
union
select 'drop index [' + i.name + '] on [' + s.name + '].[' + o.name + '];'
from sys.indexes i 
inner join sys.objects o on i.object_id = o.object_id
inner join sys.schemas s on o.schema_id = s.schema_id
where o.type != 'S' 
and is_primary_key != 1 
and index_id > 0
and s.name != 'sys' 
and is_unique_constraint = 0
