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
drop table if exists "catalogue_entry" cascade;
drop table if exists "vat_rate" cascade;
drop table if exists past_household_order_item cascade;
drop table if exists past_household_order cascade;
drop table if exists past_order cascade;

---

create table household
( id       serial  not null primary key
, "name"   text    not null
, archived boolean not null
);

insert into household ("name", archived) values ('123 Front Road',  false) returning id into h1Id;
insert into household ("name", archived) values ('1 Main Terrace',  false) returning id into h2Id;
insert into household ("name", archived) values ('24 The Street',   false) returning id into h3Id;
insert into household ("name", archived) values ('3 Bowling Alley', false) returning id into h4Id;

---

create table "order"
( id            serial    not null  primary key
, created_date  timestamptz not null
, created_by_id int       not null
);

insert into "order" (created_date, created_by_id) values ('2018-01-01', h1Id) returning id into o1Id;
insert into "order" (created_date, created_by_id) values ('2018-01-02', h1Id) returning id into o2Id;
insert into "order" (created_date, created_by_id) values ('2018-01-03', h1Id) returning id into o3Id;
insert into "order" (created_date, created_by_id) values ('2018-01-04', h1Id) returning id into o4Id;

---

create table vat_rate
( code       char          not null primary key
, multiplier numeric(3, 2) not null
);

insert into vat_rate (code, multiplier) values('Z', 1.0);
insert into vat_rate (code, multiplier) values('S', 1.2);
insert into vat_rate (code, multiplier) values('R', 1.05);

---

create table product
( id           serial    not null primary key
, "code"       text      not null
, "name"       text      not null
, price        int       not null
, vat_rate     char      not null
, old_price    int       null
, discontinued boolean   not null
, updated      timestamptz not null
, foreign key (vat_rate) references vat_rate (code)
);

insert into product (code, "name", price, vat_rate, discontinued, updated) values ('FX109', 'Jam',     1240, 'Z', false, current_date) returning id into p1Id;
insert into product (code, "name", price, vat_rate, discontinued, updated) values ('CV308', 'Butter',  9523, 'S', false, current_date) returning id into p2Id;
insert into product (code, "name", price, vat_rate, discontinued, updated) values ('M0043', 'Milk',    5210, 'R', false, current_date) returning id into p3Id;
insert into product (code, "name", price, vat_rate, discontinued, updated) values ('BN995', 'Bananas', 1200, 'Z', false, current_date) returning id into p4Id;

---

create table household_order
( order_id         int       not null
, household_id     int       not null
, updated          timestamptz not null
, complete         boolean   not null
, cancelled        boolean   not null
, primary key (order_id, household_id)
, foreign key (order_id) references "order" (id)
, foreign key (household_id) references household (id)
);

---

create table household_order_item
( order_id              int    not null
, household_id          int    not null
, product_id            int    not null
, product_price_exc_vat int    not null
, product_price_inc_vat int    not null
, quantity              int    not null
, item_total_exc_vat    int    not null
, item_total_inc_vat    int    not null
, ix                    serial not null
, primary key (order_id, household_id, product_id)
, foreign key (order_id, household_id) references household_order (order_id, household_id)
, foreign key (product_id) references product (id)
);

---

create table household_payment
( id           serial    not null primary key
, household_id int       not null
, "date"       timestamptz not null
, amount       int       not null
, archived     boolean   not null
, foreign key (household_id) references household (id)
);

insert into household_payment (household_id, "date", amount, archived) values (1, '2018-01-01', 10000, false) returning id into hp1Id;
insert into household_payment (household_id, "date", amount, archived) values (1, '2018-01-02', 20000, false) returning id into hp2Id;
insert into household_payment (household_id, "date", amount, archived) values (3, '2018-01-01', 30000, false) returning id into hp3Id;
insert into household_payment (household_id, "date", amount, archived) values (4, '2018-01-01', 40000, false) returning id into hp4Id;

---

create table catalogue_entry
( code           text    not null primary key
, category       text    not null
, brand          text    not null
, "description"  text    not null
, "text"         text    not null
, size           text    not null
, price          int     not null
, vat_rate       char    not null
, rrp            int     null
, biodynamic     boolean not null
, fair_trade     boolean not null
, gluten_free    boolean not null
, organic        boolean not null
, added_sugar    boolean not null
, vegan          boolean not null
, price_changed  date    null
);

---

create table past_order
( id              int       not null  primary key
, created_date    timestamptz not null
, created_by_id   int       not null
, created_by_name text      not null
, cancelled       boolean   not null
);

create table past_household_order
( order_id       int     not null
, household_id   int     not null
, household_name text    not null
, cancelled      boolean not null
, primary key (order_id, household_id)
, foreign key (order_id) references past_order (id)
, foreign key (household_id) references household (id)
);

create table past_household_order_item
( order_id              int  not null
, household_id          int  not null
, product_id            int  not null
, product_code          text not null
, product_name          text not null
, product_price_exc_vat int  not null
, product_price_inc_vat int  not null
, product_vat_rate      char not null
, quantity              int  not null
, item_total_exc_vat    int  not null
, item_total_inc_vat    int  not null
, primary key (order_id, household_id, product_id)
, foreign key (order_id, household_id) references past_household_order (order_id, household_id)
);

delete from "order" where id = o1Id;
delete from "order" where id = o2Id;
delete from "order" where id = o3Id;

insert into past_order (id, created_date, created_by_id, created_by_name, cancelled) values (o1Id, '2018-01-01', h1Id, '123 Front Road', false);
insert into past_order (id, created_date, created_by_id, created_by_name, cancelled) values (o2Id, '2018-01-02', h1Id, '123 Front Road', true);
insert into past_order (id, created_date, created_by_id, created_by_name, cancelled) values (o3Id, '2018-01-03', h1Id, '123 Front Road', false);

insert into past_household_order (order_id, household_id, household_name, cancelled) values (o1Id, h1Id, '123 Front Road',  false);
insert into past_household_order (order_id, household_id, household_name, cancelled) values (o1Id, h2Id, '1 Main Terrace',  false);
insert into past_household_order (order_id, household_id, household_name, cancelled) values (o2Id, h3Id, '24 The Street',   true);
insert into past_household_order (order_id, household_id, household_name, cancelled) values (o2Id, h4Id, '3 Bowling Alley', true);
insert into past_household_order (order_id, household_id, household_name, cancelled) values (o3Id, h1Id, '123 Front Road',  false);
insert into past_household_order (order_id, household_id, household_name, cancelled) values (o3Id, h4Id, '3 Bowling Alley', false);

insert into past_household_order_item (order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (o1Id, h1Id, p1Id, 'FX109p', 'Jam p',     100, 120, 'Z', 1, 100,  120);
insert into past_household_order_item (order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (o1Id, h2Id, p2Id, 'CV308p', 'Butter p',  200, 240, 'S', 2, 400,  480);
insert into past_household_order_item (order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (o2Id, h3Id, p3Id, 'M0043p', 'Milk p',    300, 315, 'R', 1, 300,  315);
insert into past_household_order_item (order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (o2Id, h4Id, p4Id, 'BN995p', 'Bananas p', 400, 400, 'Z', 5, 2000, 2000);

end $$