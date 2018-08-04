do $$
declare o1Id integer;
declare o2Id integer;
declare o3Id integer;
declare o4Id integer;
declare p1Id integer;
declare p2Id integer;
declare p3Id integer;
declare p4Id integer;
declare h1Id integer;
declare h2Id integer;
declare h3Id integer;
declare h4Id integer;
begin

drop table if exists "houseold_order_item" cascade;
drop table if exists "houseold_order" cascade;
drop table if exists "order" cascade;
drop table if exists "houseold" cascade;
drop table if exists "product" cascade;

---

create table "order"
( id           serial  not null  primary key
, created_date date    not null
, complete     boolean not null
);

insert into "order" (created_date, complete)
values ('2018-01-01', false)
returning id
into o1Id;

insert into "order" (created_date, complete)
values ('2018-01-02', true)
returning id
into o2Id;

insert into "order" (created_date, complete)
values ('2018-01-03', true)
returning id
into o3Id;

insert into "order" (created_date, complete)
values ('2018-01-04', true)
returning id
into o4Id;

---

create table household
( id     serial not null primary key
, "name" text
);

insert into household ("name") values ('123 Front Road')  returning id into h1Id;
insert into household ("name") values ('1 Main Terrace')  returning id into h2Id;
insert into household ("name") values ('24 The Street')   returning id into h3Id;
insert into household ("name") values ('3 Bowling Alley') returning id into h4Id;

---

create table product
( id     serial not null primary key
, "name" text
, price  int not null
);

insert into product ("name", price) values ('Jam', 1240)     returning id into p1Id;
insert into product ("name", price) values ('Butter', 9523)  returning id into p2Id;
insert into product ("name", price) values ('Milk', 5210)    returning id into p3Id;
insert into product ("name", price) values ('Bananas', 1200) returning id into p4Id;

---

create table household_order
( order_id     int     not null
, household_id int     not null
, "status"     char(1) not null
, primary key (order_id, household_id)
, foreign key (order_id) references "order" (id)
, foreign key (household_id) references household (id)
);

insert into household_order (order_id, household_id, "status") values (o1Id, h1Id, 'U');
insert into household_order (order_id, household_id, "status") values (o1Id, h2Id, 'P');
insert into household_order (order_id, household_id, "status") values (o2Id, h3Id, 'U');
insert into household_order (order_id, household_id, "status") values (o2Id, h4Id, 'C');

---

create table household_order_item
( order_id     int not null
, household_id int not null
, product_id   int not null
, quantity     int not null
, primary key (order_id, household_id, product_id)
, foreign key (order_id, household_id) references household_order (order_id, household_id)
, foreign key (product_id) references product (id)
);

insert into household_order_item (order_id, household_id, product_id, quantity) values (o1Id, h1Id, p1Id, 1);
insert into household_order_item (order_id, household_id, product_id, quantity) values (o1Id, h2Id, p2Id, 2);
insert into household_order_item (order_id, household_id, product_id, quantity) values (o2Id, h3Id, p3Id, 1);
insert into household_order_item (order_id, household_id, product_id, quantity) values (o2Id, h4Id, p4Id, 5);

end $$