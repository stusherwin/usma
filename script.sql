--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.1
-- Dumped by pg_dump version 9.6.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: can_upgrade(integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION can_upgrade(to_version integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
  declare curr_version integer;
  begin

  if not exists(with t as (select to_regclass('db_upgrade') as x) select x from t where x is not null) then
    raise exception 'Upgrade failed: db_upgrade table doesn''t exist. Run upgrade_setup.sql first.';
  end if;

  select version from db_upgrade limit 1 into curr_version;

  if curr_version < to_version - 1 then
    raise exception 'Upgrade failed: only at version %. Run upgrade_%.sql first.', curr_version, lpad(cast(curr_version + 1 as text), 3, '0');
  end if;

  if curr_version >= to_version then
    raise exception 'Upgrade not run. Already at version %', curr_version;
  end if;

  raise notice 'Upgrading from version % to version %...', curr_version, to_version;
  
  return 1;
  
  end;
  $$;


ALTER FUNCTION public.can_upgrade(to_version integer) OWNER TO postgres;

--
-- Name: can_upgrade_to_version(integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION can_upgrade_to_version(to_version integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
  declare curr_version integer;
  begin

  if not exists(with t as (select to_regclass('db_upgrade') as x) select x from t where x is not null) then
    raise exception 'Upgrade failed: db_upgrade table doesn''t exist. Run database_setup.sql first.';
  end if;

  select version from db_upgrade limit 1 into curr_version;

  if curr_version < to_version - 1 then
    raise exception 'Upgrade failed: only at version %. Run upgrade_%.sql first.', curr_version, lpad(cast(curr_version + 1 as text), 3, '0');
  end if;

  if curr_version >= to_version - 1 then
    raise exception 'Upgrade not run. Already at version %', curr_version;
  end if;

  raise notice 'Upgrading from version % to version %...', curr_version, to_version;
  return t;
  
  end;
  $$;


ALTER FUNCTION public.can_upgrade_to_version(to_version integer) OWNER TO postgres;

--
-- Name: delete_order(integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION delete_order(delete_id integer) RETURNS integer
    LANGUAGE sql
    AS $$

delete from past_household_order_item where order_id = delete_id;
delete from past_household_order where order_id = delete_id;
delete from past_order where id = delete_id returning id;

$$;


ALTER FUNCTION public.delete_order(delete_id integer) OWNER TO postgres;

--
-- Name: upgrade_to_version(integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION upgrade_to_version(to_version integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
      declare curr_version integer;
      begin
        if not exists(with t as (select to_regclass('db_upgrade') as x) select x from t where x is not null) then
          raise exception 'Upgrade failed: db_upgrade table doesn''t exist. Run upgrade_setup.sql first.';
        end if;

        select version from db_upgrade limit 1 into curr_version;

        if curr_version < to_version - 1 then
          raise exception 'Upgrade failed: only at version %. Run upgrade_%.sql first.', curr_version, lpad(cast(curr_version + 1 as text), 3, '0');
        end if;

        if curr_version >= to_version then
          raise exception 'Upgrade not run. Already at version %', curr_version;
        end if;

        raise notice 'Upgrading from version % to version %...', curr_version, to_version;
        
        update db_upgrade set version = to_version;
      end;
    $$;


ALTER FUNCTION public.upgrade_to_version(to_version integer) OWNER TO postgres;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: catalogue_entry; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE catalogue_entry (
    code text NOT NULL,
    category text NOT NULL,
    brand text NOT NULL,
    description text NOT NULL,
    text text NOT NULL,
    size text NOT NULL,
    price integer NOT NULL,
    vat_rate character(1) NOT NULL,
    rrp integer,
    biodynamic boolean NOT NULL,
    fair_trade boolean NOT NULL,
    gluten_free boolean NOT NULL,
    organic boolean NOT NULL,
    added_sugar boolean NOT NULL,
    vegan boolean NOT NULL,
    updated timestamp with time zone NOT NULL
);


ALTER TABLE catalogue_entry OWNER TO postgres;

--
-- Name: db_upgrade; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE db_upgrade (
    version integer NOT NULL
);


ALTER TABLE db_upgrade OWNER TO postgres;

--
-- Name: household; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE household (
    id integer NOT NULL,
    order_group_id integer NOT NULL,
    name text NOT NULL,
    contact_name text,
    contact_email text,
    contact_phone text,
    archived boolean NOT NULL
);


ALTER TABLE household OWNER TO postgres;

--
-- Name: household_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE household_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE household_id_seq OWNER TO postgres;

--
-- Name: household_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE household_id_seq OWNED BY household.id;


--
-- Name: household_order; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE household_order (
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    order_group_id integer NOT NULL,
    updated timestamp with time zone NOT NULL,
    complete boolean NOT NULL,
    cancelled boolean NOT NULL
);


ALTER TABLE household_order OWNER TO postgres;

--
-- Name: household_order_item; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE household_order_item (
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    order_group_id integer NOT NULL,
    product_id integer NOT NULL,
    product_price_exc_vat integer NOT NULL,
    product_price_inc_vat integer NOT NULL,
    quantity integer NOT NULL,
    item_total_exc_vat integer NOT NULL,
    item_total_inc_vat integer NOT NULL,
    ix integer NOT NULL
);


ALTER TABLE household_order_item OWNER TO postgres;

--
-- Name: household_order_item_ix_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE household_order_item_ix_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE household_order_item_ix_seq OWNER TO postgres;

--
-- Name: household_order_item_ix_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE household_order_item_ix_seq OWNED BY household_order_item.ix;


--
-- Name: household_payment; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE household_payment (
    id integer NOT NULL,
    order_group_id integer NOT NULL,
    household_id integer NOT NULL,
    date timestamp with time zone NOT NULL,
    amount integer NOT NULL,
    archived boolean NOT NULL
);


ALTER TABLE household_payment OWNER TO postgres;

--
-- Name: household_payment_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE household_payment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE household_payment_id_seq OWNER TO postgres;

--
-- Name: household_payment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE household_payment_id_seq OWNED BY household_payment.id;


--
-- Name: order; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE "order" (
    id integer NOT NULL,
    order_group_id integer NOT NULL,
    created_date timestamp with time zone NOT NULL,
    created_by_id integer
);


ALTER TABLE "order" OWNER TO postgres;

--
-- Name: order_group; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE order_group (
    id integer NOT NULL,
    name text NOT NULL,
    key text NOT NULL,
    enable_payments boolean DEFAULT true NOT NULL
);


ALTER TABLE order_group OWNER TO postgres;

--
-- Name: order_group_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE order_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE order_group_id_seq OWNER TO postgres;

--
-- Name: order_group_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE order_group_id_seq OWNED BY order_group.id;


--
-- Name: order_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE order_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE order_id_seq OWNER TO postgres;

--
-- Name: order_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE order_id_seq OWNED BY "order".id;


--
-- Name: order_item_adjustment; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE order_item_adjustment (
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    product_id integer NOT NULL,
    order_group_id integer NOT NULL,
    old_product_price_exc_vat integer NOT NULL,
    old_product_price_inc_vat integer NOT NULL,
    old_quantity integer NOT NULL,
    old_item_total_exc_vat integer NOT NULL,
    old_item_total_inc_vat integer NOT NULL
);


ALTER TABLE order_item_adjustment OWNER TO postgres;

--
-- Name: past_household_order; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE past_household_order (
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    order_group_id integer NOT NULL,
    household_name text NOT NULL,
    cancelled boolean NOT NULL
);


ALTER TABLE past_household_order OWNER TO postgres;

--
-- Name: past_household_order_item; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE past_household_order_item (
    order_id integer NOT NULL,
    household_id integer NOT NULL,
    product_id integer NOT NULL,
    order_group_id integer NOT NULL,
    product_code text NOT NULL,
    product_name text NOT NULL,
    product_price_exc_vat integer NOT NULL,
    product_price_inc_vat integer NOT NULL,
    product_vat_rate character(1) NOT NULL,
    quantity integer NOT NULL,
    item_total_exc_vat integer NOT NULL,
    item_total_inc_vat integer NOT NULL,
    product_biodynamic boolean DEFAULT false NOT NULL,
    product_fair_trade boolean DEFAULT false NOT NULL,
    product_gluten_free boolean DEFAULT false NOT NULL,
    product_organic boolean DEFAULT false NOT NULL,
    product_added_sugar boolean DEFAULT false NOT NULL,
    product_vegan boolean DEFAULT false NOT NULL
);


ALTER TABLE past_household_order_item OWNER TO postgres;

--
-- Name: past_order; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE past_order (
    id integer NOT NULL,
    order_group_id integer NOT NULL,
    created_date timestamp with time zone NOT NULL,
    created_by_id integer,
    created_by_name text,
    cancelled boolean NOT NULL
);


ALTER TABLE past_order OWNER TO postgres;

--
-- Name: product; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE product (
    id integer NOT NULL,
    code text NOT NULL,
    name text NOT NULL,
    price integer NOT NULL,
    vat_rate character(1) NOT NULL,
    old_price integer,
    discontinued boolean NOT NULL,
    updated timestamp with time zone NOT NULL,
    biodynamic boolean DEFAULT false NOT NULL,
    fair_trade boolean DEFAULT false NOT NULL,
    gluten_free boolean DEFAULT false NOT NULL,
    organic boolean DEFAULT false NOT NULL,
    added_sugar boolean DEFAULT false NOT NULL,
    vegan boolean DEFAULT false NOT NULL
);


ALTER TABLE product OWNER TO postgres;

--
-- Name: product_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE product_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE product_id_seq OWNER TO postgres;

--
-- Name: product_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE product_id_seq OWNED BY product.id;


--
-- Name: product_image; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE product_image (
    code text NOT NULL,
    image bytea NOT NULL
);


ALTER TABLE product_image OWNER TO postgres;

--
-- Name: vat_rate; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE vat_rate (
    code character(1) NOT NULL,
    multiplier numeric(3,2) NOT NULL
);


ALTER TABLE vat_rate OWNER TO postgres;

--
-- Name: x; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE x (
    a integer
);


ALTER TABLE x OWNER TO postgres;

--
-- Name: household id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household ALTER COLUMN id SET DEFAULT nextval('household_id_seq'::regclass);


--
-- Name: household_order_item ix; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order_item ALTER COLUMN ix SET DEFAULT nextval('household_order_item_ix_seq'::regclass);


--
-- Name: household_payment id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_payment ALTER COLUMN id SET DEFAULT nextval('household_payment_id_seq'::regclass);


--
-- Name: order id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "order" ALTER COLUMN id SET DEFAULT nextval('order_id_seq'::regclass);


--
-- Name: order_group id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY order_group ALTER COLUMN id SET DEFAULT nextval('order_group_id_seq'::regclass);


--
-- Name: product id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY product ALTER COLUMN id SET DEFAULT nextval('product_id_seq'::regclass);


--
-- Name: catalogue_entry catalogue_entry_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY catalogue_entry
    ADD CONSTRAINT catalogue_entry_pkey PRIMARY KEY (code);


--
-- Name: household_order_item household_order_item_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order_item
    ADD CONSTRAINT household_order_item_pkey PRIMARY KEY (order_id, household_id, product_id);


--
-- Name: household_order household_order_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order
    ADD CONSTRAINT household_order_pkey PRIMARY KEY (order_id, household_id);


--
-- Name: household_payment household_payment_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_payment
    ADD CONSTRAINT household_payment_pkey PRIMARY KEY (id);


--
-- Name: household household_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household
    ADD CONSTRAINT household_pkey PRIMARY KEY (id);


--
-- Name: order_group order_group_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY order_group
    ADD CONSTRAINT order_group_pkey PRIMARY KEY (id);


--
-- Name: order_item_adjustment order_item_adjustment_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY order_item_adjustment
    ADD CONSTRAINT order_item_adjustment_pkey PRIMARY KEY (order_id, household_id, product_id);


--
-- Name: order order_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY "order"
    ADD CONSTRAINT order_pkey PRIMARY KEY (id);


--
-- Name: past_household_order_item past_household_order_item_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_household_order_item
    ADD CONSTRAINT past_household_order_item_pkey PRIMARY KEY (order_id, household_id, product_id);


--
-- Name: past_household_order past_household_order_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_household_order
    ADD CONSTRAINT past_household_order_pkey PRIMARY KEY (order_id, household_id);


--
-- Name: past_order past_order_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_order
    ADD CONSTRAINT past_order_pkey PRIMARY KEY (id);


--
-- Name: product_image product_image_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY product_image
    ADD CONSTRAINT product_image_pkey PRIMARY KEY (code);


--
-- Name: product product_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY product
    ADD CONSTRAINT product_pkey PRIMARY KEY (id);


--
-- Name: vat_rate vat_rate_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY vat_rate
    ADD CONSTRAINT vat_rate_pkey PRIMARY KEY (code);


--
-- Name: household household_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household
    ADD CONSTRAINT household_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: household_order household_order_household_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order
    ADD CONSTRAINT household_order_household_id_fkey FOREIGN KEY (household_id) REFERENCES household(id);


--
-- Name: household_order_item household_order_item_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order_item
    ADD CONSTRAINT household_order_item_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: household_order_item household_order_item_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order_item
    ADD CONSTRAINT household_order_item_order_id_fkey FOREIGN KEY (order_id, household_id) REFERENCES household_order(order_id, household_id);


--
-- Name: household_order_item household_order_item_product_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order_item
    ADD CONSTRAINT household_order_item_product_id_fkey FOREIGN KEY (product_id) REFERENCES product(id);


--
-- Name: household_order household_order_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order
    ADD CONSTRAINT household_order_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: household_order household_order_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_order
    ADD CONSTRAINT household_order_order_id_fkey FOREIGN KEY (order_id) REFERENCES "order"(id);


--
-- Name: household_payment household_payment_household_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_payment
    ADD CONSTRAINT household_payment_household_id_fkey FOREIGN KEY (household_id) REFERENCES household(id);


--
-- Name: household_payment household_payment_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY household_payment
    ADD CONSTRAINT household_payment_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: order_item_adjustment order_item_adjustment_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY order_item_adjustment
    ADD CONSTRAINT order_item_adjustment_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: order_item_adjustment order_item_adjustment_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY order_item_adjustment
    ADD CONSTRAINT order_item_adjustment_order_id_fkey FOREIGN KEY (order_id, household_id, product_id) REFERENCES past_household_order_item(order_id, household_id, product_id);


--
-- Name: past_household_order past_household_order_household_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_household_order
    ADD CONSTRAINT past_household_order_household_id_fkey FOREIGN KEY (household_id) REFERENCES household(id);


--
-- Name: past_household_order_item past_household_order_item_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_household_order_item
    ADD CONSTRAINT past_household_order_item_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: past_household_order_item past_household_order_item_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_household_order_item
    ADD CONSTRAINT past_household_order_item_order_id_fkey FOREIGN KEY (order_id, household_id) REFERENCES past_household_order(order_id, household_id);


--
-- Name: past_household_order past_household_order_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_household_order
    ADD CONSTRAINT past_household_order_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: past_household_order past_household_order_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_household_order
    ADD CONSTRAINT past_household_order_order_id_fkey FOREIGN KEY (order_id) REFERENCES past_order(id);


--
-- Name: past_order past_order_order_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY past_order
    ADD CONSTRAINT past_order_order_group_id_fkey FOREIGN KEY (order_group_id) REFERENCES order_group(id);


--
-- Name: product product_vat_rate_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY product
    ADD CONSTRAINT product_vat_rate_fkey FOREIGN KEY (vat_rate) REFERENCES vat_rate(code);


--
-- Name: catalogue_entry; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE catalogue_entry TO col_ord_user;


--
-- Name: db_upgrade; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE db_upgrade TO col_ord_user;


--
-- Name: household; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE household TO col_ord_user;


--
-- Name: household_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE household_id_seq TO col_ord_user;


--
-- Name: household_order; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE household_order TO col_ord_user;


--
-- Name: household_order_item; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE household_order_item TO col_ord_user;


--
-- Name: household_order_item_ix_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE household_order_item_ix_seq TO col_ord_user;


--
-- Name: household_payment; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE household_payment TO col_ord_user;


--
-- Name: household_payment_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE household_payment_id_seq TO col_ord_user;


--
-- Name: order; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE "order" TO col_ord_user;


--
-- Name: order_group; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE order_group TO col_ord_user;


--
-- Name: order_group_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE order_group_id_seq TO col_ord_user;


--
-- Name: order_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE order_id_seq TO col_ord_user;


--
-- Name: order_item_adjustment; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE order_item_adjustment TO col_ord_user;


--
-- Name: past_household_order; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE past_household_order TO col_ord_user;


--
-- Name: past_household_order_item; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE past_household_order_item TO col_ord_user;


--
-- Name: past_order; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE past_order TO col_ord_user;


--
-- Name: product; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE product TO col_ord_user;


--
-- Name: product_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE product_id_seq TO col_ord_user;


--
-- Name: product_image; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE product_image TO col_ord_user;


--
-- Name: vat_rate; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE vat_rate TO col_ord_user;


--
-- Name: x; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON TABLE x TO col_ord_user;


--
-- PostgreSQL database dump complete
--

