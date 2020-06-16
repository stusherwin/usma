begin;
do $$
begin
  perform upgrade_to_version(7);

  drop schema if exists v2 cascade;
  create schema v2;

  CREATE TABLE v2.vat_rate 
  ( code character(1) NOT NULL
  , multiplier numeric(3,2) NOT NULL
  , PRIMARY KEY (code)
  );

  create table v2.catalogue_entry
  ( code           char(10)    not null
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
  , PRIMARY KEY (code)
  );

  CREATE TABLE v2.product 
  ( id SERIAL NOT NULL
  , code char(10) NOT NULL
  , PRIMARY KEY (id)
  , FOREIGN KEY (vat_rate) REFERENCES v2.vat_rate(code)
  );

  CREATE TABLE v2.order_group 
  ( id SERIAL NOT NULL
  , name text NOT NULL
  , key text NOT NULL
  , is_payments_enabled boolean DEFAULT true NOT NULL
  , PRIMARY KEY (id)
  );

  CREATE TABLE v2.household 
  ( order_group_id integer NOT NULL
  , id SERIAL NOT NULL
  , name text NOT NULL
  , contact_name text
  , contact_email text
  , contact_phone text
  , is_archived boolean NOT NULL
  , PRIMARY KEY (id)
  , FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id)
  );

  CREATE TABLE v2."order" 
  ( order_group_id integer NOT NULL
  , id SERIAL     NOT NULL
  , created        timestamp with time zone NOT NULL
  , created_by_id  integer
  , is_placed      boolean NOT NULL
  , is_abandoned   boolean NOT NULL
  , PRIMARY KEY (id)
  , FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id)
  , FOREIGN KEY (created_by_id) REFERENCES v2.household(id)
  );

  CREATE TABLE v2.household_order 
  ( order_group_id integer NOT NULL
  , order_id integer NOT NULL
  , household_id integer NOT NULL
  , updated timestamp with time zone NOT NULL
  , is_complete boolean NOT NULL
  , is_abandoned boolean NOT NULL
  , PRIMARY KEY (order_id, household_id)
  , FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id)
  , FOREIGN KEY (order_id) REFERENCES v2."order"(id)
  , FOREIGN KEY (household_id) REFERENCES v2.household(id)
  );

  CREATE TABLE v2.order_item 
  ( order_group_id integer NOT NULL
  , order_id integer NOT NULL
  , household_id integer NOT NULL
  , product_code char(10) NOT NULL
  , product_name text NOT NULL
  , product_price integer NOT NULL
  , product_vat_rate character(1) NOT NULL
  , product_vat_rate_multiplier numeric(3,2) NOT NULL
  , product_is_biodynamic boolean DEFAULT false NOT NULL
  , product_is_fair_trade boolean DEFAULT false NOT NULL
  , product_is_gluten_free boolean DEFAULT false NOT NULL
  , product_is_organic boolean DEFAULT false NOT NULL
  , product_is_added_sugar boolean DEFAULT false NOT NULL
  , product_is_vegan boolean DEFAULT false NOT NULL
  , quantity integer NOT NULL
  , PRIMARY KEY (order_id, household_id, product_code)
  , FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id)
  , FOREIGN KEY (order_id) REFERENCES v2."order"(id)
  , FOREIGN KEY (household_id) REFERENCES v2.household(id)
  , FOREIGN KEY (order_id, household_id) REFERENCES v2.household_order(order_id, household_id)
  );

  CREATE TABLE v2.order_item_adjustment 
  ( order_group_id integer NOT NULL
  , order_id integer NOT NULL
  , household_id integer NOT NULL
  , product_code char(10) NOT NULL
  , new_vat_rate character(1) NOT NULL
  , new_price integer NOT NULL
  , new_quantity integer NOT NULL
  , is_discontinued boolean NOT NULL
  , date timestamp with time zone NOT NULL
  , PRIMARY KEY (order_id, household_id, product_code)
  , FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id)
  , FOREIGN KEY (order_id) REFERENCES v2."order"(id)
  , FOREIGN KEY (household_id) REFERENCES v2.household(id)
  , FOREIGN KEY (order_id, household_id) REFERENCES v2.household_order(order_id, household_id)
  , FOREIGN KEY (order_id, household_id, product_code) REFERENCES v2.order_item(order_id, household_id, product_code)
  , FOREIGN KEY (new_vat_rate) REFERENCES v2.vat_rate(code)
  );

  create table v2.payment
  ( id             serial      not null
  , order_group_id int         not null
  , household_id   int         not null
  , "date"         timestamptz not null
  , amount         int         not null
  , archived       boolean     not null
  , primary key (id)
  , foreign key (household_id) references v2.household (id)
  , foreign key (order_group_id) references v2.order_group (id)
  );


end $$ language plpgsql;
commit;
