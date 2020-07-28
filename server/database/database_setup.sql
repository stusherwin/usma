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

  ---

  create table "order"
  ( id             serial      not null  primary key
  , order_group_id int         not null
  , created_date   timestamptz not null
  , created_by_id  int         not null
  );

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
end $$ language plpgsql;
commit;