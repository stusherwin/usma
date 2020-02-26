begin;
do $$
declare og1Id integer;
declare og2Id integer;
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
  if exists(with t as (select to_regclass('order_group') as x) select x from t where x is not null) then
    raise exception 'Database setup already run. Aborting...';
  end if;

  create table order_group
  ( id     serial not null primary key
  , "name" text   not null
  , "key"  text   not null
  );

  insert into order_group ("name", "key") values ('South Brum Co-ops', 'ABC') returning id into og1Id;
  insert into order_group ("name", "key") values ('Rachel''s Friends', 'DEF') returning id into og2Id;

  ---

  create table household
  ( id           serial  not null primary key
  , order_group_id int   not null
  , "name"       text    not null
  , contact_name  text   null
  , contact_email text   null
  , contact_phone text   null
  , archived     boolean not null
  , foreign key (order_group_id) references order_group (id)
  );

  insert into household (order_group_id, "name", archived) values (og1Id, '123 Front Road',  false) returning id into h1Id;
  insert into household (order_group_id, "name", archived) values (og1Id, '1 Main Terrace',  false) returning id into h2Id;
  insert into household (order_group_id, "name", archived) values (og1Id, '24 The Street',   false) returning id into h3Id;
  insert into household (order_group_id, "name", archived) values (og1Id, '3 Bowling Alley', false) returning id into h4Id;

  ---

  create table "order"
  ( id             serial      not null  primary key
  , order_group_id int         not null
  , created_date   timestamptz not null
  , created_by_id  int         not null
  );

  alter sequence order_id_seq restart with 4;

  insert into "order" (order_group_id, created_date, created_by_id) values (og1Id, '2018-01-04', h1Id) returning id into o4Id;

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
  ( order_id         int         not null
  , household_id     int         not null
  , order_group_id   int         not null
  , updated          timestamptz not null
  , complete         boolean     not null
  , cancelled        boolean     not null
  , primary key (order_id, household_id)
  , foreign key (order_id) references "order" (id)
  , foreign key (household_id) references household (id)
  , foreign key (order_group_id) references order_group (id)
  );

  ---

  create table household_order_item
  ( order_id              int    not null
  , household_id          int    not null
  , order_group_id        int    not null
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
  , foreign key (order_group_id) references order_group (id)
  );

  ---

  create table household_payment
  ( id             serial      not null primary key
  , order_group_id int         not null
  , household_id   int         not null
  , "date"         timestamptz not null
  , amount         int         not null
  , archived       boolean     not null
  , foreign key (household_id) references household (id)
  , foreign key (order_group_id) references order_group (id)
  );

  insert into household_payment (order_group_id, household_id, "date", amount, archived) values (og1Id, 1, '2018-01-01', 10000, false) returning id into hp1Id;
  insert into household_payment (order_group_id, household_id, "date", amount, archived) values (og1Id, 1, '2018-01-02', 20000, false) returning id into hp2Id;
  insert into household_payment (order_group_id, household_id, "date", amount, archived) values (og1Id, 3, '2018-01-01', 30000, false) returning id into hp3Id;
  insert into household_payment (order_group_id, household_id, "date", amount, archived) values (og1Id, 4, '2018-01-01', 40000, false) returning id into hp4Id;

  ---

  create table catalogue_entry
  ( code           text        not null primary key
  , category       text        not null
  , brand          text        not null
  , "description"  text        not null
  , "text"         text        not null
  , size           text        not null
  , price          int         not null
  , vat_rate       char        not null
  , rrp            int         null
  , biodynamic     boolean     not null
  , fair_trade     boolean     not null
  , gluten_free    boolean     not null
  , organic        boolean     not null
  , added_sugar    boolean     not null
  , vegan          boolean     not null
  , updated        timestamptz not null
  );

  ---

  create table past_order
  ( id              int         not null  primary key
  , order_group_id  int         not null
  , created_date    timestamptz not null
  , created_by_id   int         not null
  , created_by_name text        not null
  , cancelled       boolean     not null
  , foreign key (order_group_id) references order_group (id)
  );

  create table past_household_order
  ( order_id       int     not null
  , household_id   int     not null
  , order_group_id int     not null
  , household_name text    not null
  , cancelled      boolean not null
  , primary key (order_id, household_id)
  , foreign key (order_id) references past_order (id)
  , foreign key (household_id) references household (id)
  , foreign key (order_group_id) references order_group (id)
  );

  create table past_household_order_item
  ( order_id              int  not null
  , household_id          int  not null
  , product_id            int  not null
  , order_group_id        int  not null
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
  , foreign key (order_group_id) references order_group (id)
  );

  insert into past_order (order_group_id, id, created_date, created_by_id, created_by_name, cancelled) values (og1Id, 1, '2018-01-01', h1Id, '123 Front Road', false);
  insert into past_order (order_group_id, id, created_date, created_by_id, created_by_name, cancelled) values (og1Id, 2, '2018-01-02', h1Id, '123 Front Road', true);
  insert into past_order (order_group_id, id, created_date, created_by_id, created_by_name, cancelled) values (og1Id, 3, '2018-01-03', h1Id, '123 Front Road', false);

  insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled) values (og1Id, 1, h1Id, '123 Front Road',  false);
  insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled) values (og1Id, 1, h2Id, '1 Main Terrace',  false);
  insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled) values (og1Id, 2, h3Id, '24 The Street',   true);
  insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled) values (og1Id, 2, h4Id, '3 Bowling Alley', true);
  insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled) values (og1Id, 3, h1Id, '123 Front Road',  false);
  insert into past_household_order (order_group_id, order_id, household_id, household_name, cancelled) values (og1Id, 3, h4Id, '3 Bowling Alley', false);

  insert into past_household_order_item (order_group_id, order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (og1Id, 1, h1Id, p1Id, 'FX109p', 'Jam p',     100, 120, 'Z', 1, 100,  120);
  insert into past_household_order_item (order_group_id, order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (og1Id, 1, h2Id, p2Id, 'CV308p', 'Butter p',  200, 240, 'S', 2, 400,  480);
  insert into past_household_order_item (order_group_id, order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (og1Id, 2, h3Id, p3Id, 'M0043p', 'Milk p',    300, 315, 'R', 1, 300,  315);
  insert into past_household_order_item (order_group_id, order_id, household_id, product_id, product_code, product_name, product_price_exc_vat, product_price_inc_vat, product_vat_rate, quantity, item_total_exc_vat, item_total_inc_vat) values (og1Id, 2, h4Id, p4Id, 'BN995p', 'Bananas p', 400, 400, 'Z', 5, 2000, 2000);
end $$ language plpgsql;
commit;