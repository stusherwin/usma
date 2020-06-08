begin;
do $$
begin
  perform upgrade_to_version(7);

  drop schema if exists v2 cascade;
  create schema v2;

  CREATE TABLE v2.vat_rate (
    code character(1) NOT NULL,
    multiplier numeric(3,2) NOT NULL
  );
  ALTER TABLE ONLY v2.vat_rate
    ADD CONSTRAINT v2_vat_rate_pkey PRIMARY KEY (code);


  CREATE TABLE v2.product (
    id integer NOT NULL,
    code text NOT NULL,
    name text NOT NULL,
    price integer NOT NULL,
    vat_rate character(1) NOT NULL,
    is_discontinued boolean NOT NULL,
    updated timestamp with time zone NOT NULL,
    is_biodynamic boolean DEFAULT false NOT NULL,
    is_fair_trade boolean DEFAULT false NOT NULL,
    is_gluten_free boolean DEFAULT false NOT NULL,
    is_organic boolean DEFAULT false NOT NULL,
    is_added_sugar boolean DEFAULT false NOT NULL,
    is_vegan boolean DEFAULT false NOT NULL
  );
  CREATE SEQUENCE v2.product_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
  ALTER SEQUENCE v2.product_id_seq OWNED BY v2.product.id;
  ALTER TABLE ONLY v2.product ALTER COLUMN id SET DEFAULT nextval('v2.product_id_seq'::regclass);
  ALTER TABLE ONLY v2.product
    ADD CONSTRAINT v2_product_pkey PRIMARY KEY (id);
  ALTER TABLE ONLY v2.product
    ADD CONSTRAINT v2_product_vat_rate_fkey FOREIGN KEY (vat_rate) REFERENCES v2.vat_rate(code);


  CREATE TABLE v2.order_group (
    id integer NOT NULL,
    name text NOT NULL,
    key text NOT NULL,
    enable_payments boolean DEFAULT true NOT NULL
  );
  CREATE SEQUENCE v2.order_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
  ALTER SEQUENCE v2.order_group_id_seq OWNED BY v2.order_group.id;
  ALTER TABLE ONLY v2.order_group ALTER COLUMN id SET DEFAULT nextval('v2.order_group_id_seq'::regclass);
  ALTER TABLE ONLY v2.order_group
    ADD CONSTRAINT v2_order_group_pkey PRIMARY KEY (id);


  CREATE TABLE v2.household (
    order_group_id integer NOT NULL,
    id integer NOT NULL,
    name text NOT NULL,
    contact_name text,
    contact_email text,
    contact_phone text,
    archived boolean NOT NULL
  );
  CREATE SEQUENCE v2.household_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
  ALTER SEQUENCE v2.household_id_seq OWNED BY v2.household.id;
  ALTER TABLE ONLY v2.household ALTER COLUMN id SET DEFAULT nextval('v2.household_id_seq'::regclass);
  ALTER TABLE ONLY v2.household
    ADD CONSTRAINT v2_household_pkey PRIMARY KEY (id);
  ALTER TABLE ONLY v2.household
    ADD CONSTRAINT v2_household_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id);


  CREATE TABLE v2."order" (
    order_group_id integer NOT NULL,
    id integer     NOT NULL,
    created        timestamp with time zone NOT NULL,
    created_by_id  integer,
    is_placed      boolean NOT NULL,
    is_abandoned   boolean NOT NULL
  );
  CREATE SEQUENCE v2.order_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
  ALTER SEQUENCE v2.order_id_seq OWNED BY v2."order".id;
  ALTER TABLE ONLY v2."order" ALTER COLUMN id SET DEFAULT nextval('v2.order_id_seq'::regclass);
  ALTER TABLE ONLY v2."order"
    ADD CONSTRAINT v2_order_pkey PRIMARY KEY (id);
  ALTER TABLE ONLY v2."order"
    ADD CONSTRAINT v2_order_created_by_id_fkey FOREIGN KEY (created_by_id) REFERENCES v2.household(id);


  CREATE TABLE v2.household_order (
    order_group_id integer NOT NULL,
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    updated timestamp with time zone NOT NULL,
    is_complete boolean NOT NULL,
    is_abandoned boolean NOT NULL
  );
  ALTER TABLE ONLY v2.household_order
    ADD CONSTRAINT v2_household_order_pkey PRIMARY KEY (order_id, household_id);
  ALTER TABLE ONLY v2.household_order
    ADD CONSTRAINT v2_household_order_household_id_fkey FOREIGN KEY (household_id) REFERENCES v2.household(id);
  ALTER TABLE ONLY v2.household_order
    ADD CONSTRAINT v2_household_order_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id);
  ALTER TABLE ONLY v2.household_order
    ADD CONSTRAINT v2_household_order_order_id_fkey FOREIGN KEY (order_id) REFERENCES v2."order"(id);


  CREATE TABLE v2.household_order_item (
    order_group_id integer NOT NULL,
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    product_id integer NOT NULL,
    product_price_exc_vat integer NOT NULL,
    product_price_inc_vat integer NOT NULL,
    quantity integer NOT NULL,
    ix integer NOT NULL
  );
  CREATE SEQUENCE v2.household_order_item_ix_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
  ALTER SEQUENCE v2.household_order_item_ix_seq OWNED BY v2.household_order_item.ix;
  ALTER TABLE ONLY v2.household_order_item ALTER COLUMN ix SET DEFAULT nextval('v2.household_order_item_ix_seq'::regclass);
  ALTER TABLE ONLY v2.household_order_item
    ADD CONSTRAINT v2_household_order_item_pkey PRIMARY KEY (order_id, household_id, product_id);
  ALTER TABLE ONLY v2.household_order_item
    ADD CONSTRAINT v2_household_order_item_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id);
  ALTER TABLE ONLY v2.household_order_item
    ADD CONSTRAINT v2_household_order_item_order_id_fkey FOREIGN KEY (order_id, household_id) REFERENCES v2.household_order(order_id, household_id);
  ALTER TABLE ONLY v2.household_order_item
    ADD CONSTRAINT v2_household_order_item_product_id_fkey FOREIGN KEY (product_id) REFERENCES v2.product(id);


  CREATE TABLE v2.order_item_adjustment (
    order_group_id integer NOT NULL,
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    product_id integer NOT NULL,
    new_vat_rate character(1) NOT NULL,
    new_product_price_exc_vat integer NOT NULL,
    new_product_price_inc_vat integer NOT NULL,
    new_quantity integer NOT NULL,
    new_item_total_exc_vat integer NOT NULL,
    new_item_total_inc_vat integer NOT NULL,
    date timestamp with time zone NOT NULL,
  );
  ALTER TABLE ONLY v2.order_item_adjustment
    ADD CONSTRAINT v2_order_item_adjustment_pkey PRIMARY KEY (order_id, household_id, product_id);
  ALTER TABLE ONLY v2.order_item_adjustment
    ADD CONSTRAINT v2_order_item_adjustment_order_id_fkey FOREIGN KEY (order_id, household_id, product_id) REFERENCES v2.household_order_item(order_id, household_id, product_id);
  ALTER TABLE ONLY v2.order_item_adjustment
    ADD CONSTRAINT v2_order_item_adjustment_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES v2.order_group(id);
  ALTER TABLE ONLY v2.order_item_adjustment
    ADD CONSTRAINT v2_order_item_adjustment_vat_rate_fkey FOREIGN KEY (new_vat_rate) REFERENCES v2.vat_rate(code);


end $$ language plpgsql;
commit;
