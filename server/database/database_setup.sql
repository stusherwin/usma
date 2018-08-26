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
declare hp1Id integer;
declare hp2Id integer;
declare hp3Id integer;
declare hp4Id integer;
begin

drop table if exists "household_payment" cascade;
drop table if exists "household_order_item" cascade;
drop table if exists "household_order" cascade;
drop table if exists "order" cascade;
drop table if exists "household" cascade;
drop table if exists "product" cascade;

---

create table "order"
( id           serial  not null  primary key
, created_date date    not null
, placed       boolean not null
, archived     boolean not null
);

insert into "order" (created_date, placed, archived)
values ('2018-01-01', true, false)
returning id
into o1Id;

insert into "order" (created_date, placed, archived)
values ('2018-01-02', true, false)
returning id
into o2Id;

insert into "order" (created_date, placed, archived)
values ('2018-01-03', true, false)
returning id
into o3Id;

insert into "order" (created_date, placed, archived)
values ('2018-01-04', false, false)
returning id
into o4Id;

---

create table household
( id       serial  not null primary key
, "name"   text    not null
, archived boolean not null
);

insert into household ("name", archived) values ('123 Front Road', false)  returning id into h1Id;
insert into household ("name", archived) values ('1 Main Terrace', false)  returning id into h2Id;
insert into household ("name", archived) values ('24 The Street', false)   returning id into h3Id;
insert into household ("name", archived) values ('3 Bowling Alley', false) returning id into h4Id;

---

create table product
( id       serial not null primary key
, "name"   text   not null
, price    int    not null
, archived boolean not null
);

insert into product ("name", price, archived) values ('Jam', 1240, false)     returning id into p1Id;
insert into product ("name", price, archived) values ('Butter', 9523, false)  returning id into p2Id;
insert into product ("name", price, archived) values ('Milk', 5210, false)    returning id into p3Id;
insert into product ("name", price, archived) values ('Bananas', 1200, false) returning id into p4Id;

---

create table household_order
( order_id     int     not null
, household_id int     not null
, complete     boolean not null
, cancelled    boolean not null
, primary key (order_id, household_id)
, foreign key (order_id) references "order" (id)
, foreign key (household_id) references household (id)
);

insert into household_order (order_id, household_id, complete, cancelled) values (o1Id, h1Id, true, false);
insert into household_order (order_id, household_id, complete, cancelled) values (o1Id, h2Id, true, false);
insert into household_order (order_id, household_id, complete, cancelled) values (o2Id, h3Id, false, true);
insert into household_order (order_id, household_id, complete, cancelled) values (o2Id, h4Id, false, true);

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

---

create table household_payment
( id           serial  not null primary key
, household_id int     not null
, "date"       date    not null
, amount       int     not null
, archived     boolean not null
, foreign key (household_id) references household (id)
);

insert into household_payment (household_id, "date", amount, archived) values (1, '2018-01-01', 10000, false) returning id into hp1Id;
insert into household_payment (household_id, "date", amount, archived) values (1, '2018-01-02', 20000, false) returning id into hp2Id;
insert into household_payment (household_id, "date", amount, archived) values (3, '2018-01-01', 30000, false) returning id into hp3Id;
insert into household_payment (household_id, "date", amount, archived) values (4, '2018-01-01', 40000, false) returning id into hp4Id;

end $$