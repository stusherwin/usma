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

  CREATE TABLE v2.product 
  ( id SERIAL NOT NULL
  , code text NOT NULL
  , name text NOT NULL
  , price integer NOT NULL
  , vat_rate character(1) NOT NULL
  , is_biodynamic boolean DEFAULT false NOT NULL
  , is_fair_trade boolean DEFAULT false NOT NULL
  , is_gluten_free boolean DEFAULT false NOT NULL
  , is_organic boolean DEFAULT false NOT NULL
  , is_added_sugar boolean DEFAULT false NOT NULL
  , is_vegan boolean DEFAULT false NOT NULL
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
  , product_id integer NOT NULL
  , product_vat_rate character(1) NOT NULL
  , product_vat_rate_multiplier numeric(3,2) NOT NULL
  , product_price integer NOT NULL
  , quantity integer NOT NULL
  , ix SERIAL NOT NULL
  , PRIMARY KEY (order_id, household_id, product_id)
  , FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id)
  , FOREIGN KEY (order_id, household_id) REFERENCES v2.household_order(order_id, household_id)
  , FOREIGN KEY (order_id) REFERENCES v2."order"(id)
  , FOREIGN KEY (household_id) REFERENCES v2.household(id)
  , FOREIGN KEY (product_id) REFERENCES v2.product(id)
  );

  CREATE TABLE v2.order_item_adjustment 
  ( order_group_id integer NOT NULL
  , order_id integer NOT NULL
  , household_id integer NOT NULL
  , product_id integer NOT NULL
  , new_vat_rate character(1) NOT NULL
  , new_price integer NOT NULL
  , new_quantity integer NOT NULL
  , is_discontinued boolean NOT NULL
  , date timestamp with time zone NOT NULL
  , PRIMARY KEY (order_id, household_id, product_id)
  , FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id)
  , FOREIGN KEY (order_id, household_id, product_id) REFERENCES v2.order_item(order_id, household_id, product_id)
  , FOREIGN KEY (order_id, household_id) REFERENCES v2.household_order(order_id, household_id)
  , FOREIGN KEY (order_id) REFERENCES v2."order"(id)
  , FOREIGN KEY (household_id) REFERENCES v2.household(id)
  , FOREIGN KEY (product_id) REFERENCES v2.product(id)
  , FOREIGN KEY (new_vat_rate) REFERENCES v2.vat_rate(code)
  );

end $$ language plpgsql;
commit;
