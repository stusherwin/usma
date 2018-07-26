do $$
declare o1Id integer;
declare o2Id integer;
declare o3Id integer;
declare o4Id integer;
begin

drop table if exists "order" cascade;

create table "order"
( id                     serial  not null  primary key
, date                   date    not null
);

insert into "order" (date)
values ('2018-01-01')
returning id
into o1Id;

insert into "order" (date)
values ('2018-01-02')
returning id
into o2Id;

insert into "order" (date)
values ('2018-01-03')
returning id
into o3Id;

insert into "order" (date)
values ('2018-01-04')
returning id
into o4Id;

end $$