begin;
do $$
begin
  perform upgrade_to_version(9);

  drop schema if exists v2 cascade;
  create schema v2;

  create table v2.vat_rate 
  ( code character(1) not null
  , multiplier numeric(3,2) not null
  , primary key (code)
  );

  insert into v2.vat_rate 
  ( code
  , multiplier
  )
  select 
    code
  , multiplier
  from public.vat_rate;

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
  , primary key (code)
  , foreign key (vat_rate) references v2.vat_rate(code)
  );

  insert into v2.catalogue_entry
  ( code
  , category
  , brand
  , "description"
  , "text"
  , size
  , price
  , vat_rate
  , rrp
  , biodynamic
  , fair_trade
  , gluten_free
  , organic
  , added_sugar
  , vegan
  , updated
  )
  select 
    code
  , category
  , brand
  , description
  , text
  , size
  , price
  , vat_rate
  , rrp
  , biodynamic
  , fair_trade
  , gluten_free
  , organic
  , added_sugar
  , vegan
  , updated
  from public.catalogue_entry;

  create table v2.product 
  ( id SERIAL not null
  , code char(10) not null
  , primary key (id)
  );

  insert into v2.product 
  ( id
  , code
  )
  select
    id
  , code
  from public.product;

  create table v2.product_image (
    code char(10) not null
  , image bytea not null
  , primary key (code)
  );

  insert into v2.product_image
  ( code
  , image
  )
  select
    code
  , image
  from public.product_image;

  create table v2.order_group 
  ( id SERIAL not null
  , name text not null
  , key text not null
  , is_payments_enabled boolean default true not null
  , primary key (id)
  );

  insert into v2.order_group 
  ( id
  , name
  , key
  , is_payments_enabled
  )
  select 
    id
  , name
  , key
  , enable_payments
  from public.order_group;

  create table v2.household 
  ( order_group_id integer not null
  , id SERIAL not null
  , name text not null
  , contact_name text
  , contact_email text
  , contact_phone text
  , is_archived boolean not null
  , primary key (id)
  , foreign key (order_group_id) references v2.order_group(id)
  );

  insert into v2.household 
  ( order_group_id
  , id
  , name
  , contact_name
  , contact_email
  , contact_phone
  , is_archived
  )
  select
    order_group_id
  , id
  , name
  , contact_name
  , contact_email
  , contact_phone
  , archived
  from public.household;

  create table v2."order" 
  ( order_group_id integer not null
  , id SERIAL     not null
  , created        timestamp with time zone not null
  , created_by_id  integer
  , is_placed      boolean not null
  , is_abandoned   boolean not null
  , primary key (id)
  , foreign key (order_group_id) references v2.order_group(id)
  , foreign key (created_by_id) references v2.household(id)
  );

  insert into v2."order" 
  ( order_group_id
  , id
  , created
  , created_by_id
  , is_placed
  , is_abandoned
  )
  select
    order_group_id
  , id
  , created_date
  , created_by_id
  , false
  , false
  from public."order"
  union all
  select
    order_group_id
  , id
  , created_date
  , created_by_id
  , not cancelled
  , cancelled
  from public.past_order;

  create table v2.household_order 
  ( order_group_id integer not null
  , order_id integer not null
  , household_id integer not null
  , is_complete boolean not null
  , is_placed boolean not null
  , is_abandoned boolean not null
  , primary key (order_id, household_id)
  , foreign key (order_group_id) references v2.order_group(id)
  , foreign key (order_id) references v2."order"(id)
  , foreign key (household_id) references v2.household(id)
  );

  insert into v2.household_order 
  ( order_group_id
  , order_id
  , household_id
  , is_complete
  , is_placed
  , is_abandoned
  )
  select 
    ho.order_group_id
  , ho.order_id
  , ho.household_id
  , ho.complete
  , false
  , ho.cancelled
  from public.household_order ho
  union all
  select 
    ho.order_group_id
  , ho.order_id
  , ho.household_id
  , not ho.cancelled
  , case 
      when ho.cancelled then false
      else not o.cancelled
    end
  , case 
      when ho.cancelled then true
      else o.cancelled
    end
  from public.past_household_order ho
  inner join public.past_order o 
    on o.id = ho.order_id;

  create table v2.order_item 
  ( order_group_id integer not null
  , order_id integer not null
  , household_id integer not null
  , product_code char(10) not null
  , product_name text not null
  , product_price integer not null
  , product_vat_rate character(1) not null
  , product_vat_rate_multiplier numeric(3,2) not null
  , product_is_biodynamic boolean default false not null
  , product_is_fair_trade boolean default false not null
  , product_is_gluten_free boolean default false not null
  , product_is_organic boolean default false not null
  , product_is_added_sugar boolean default false not null
  , product_is_vegan boolean default false not null
  , quantity integer not null
  , primary key (order_id, household_id, product_code)
  , foreign key (order_group_id) references v2.order_group(id)
  , foreign key (order_id) references v2."order"(id)
  , foreign key (household_id) references v2.household(id)
  , foreign key (order_id, household_id) references v2.household_order(order_id, household_id)
  );

  insert into v2.order_item 
  ( order_group_id
  , order_id
  , household_id
  , product_code
  , product_name
  , product_price
  , product_vat_rate
  , product_vat_rate_multiplier
  , product_is_biodynamic
  , product_is_fair_trade
  , product_is_gluten_free
  , product_is_organic
  , product_is_added_sugar
  , product_is_vegan
  , quantity
  )
  select
    hoi.order_group_id
  , hoi.order_id
  , hoi.household_id
  , p.code
  , p.name
  , hoi.product_price_exc_vat
  , p.vat_rate
  , v.multiplier
  , p.biodynamic
  , p.fair_trade
  , p.gluten_free
  , p.organic
  , p.added_sugar
  , p.vegan
  , hoi.quantity
  from public.household_order_item hoi
  inner join public.product p
    on p.id = hoi.product_id
  inner join public.vat_rate v
    on p.vat_rate = v.code
  union all
  select
    hoi.order_group_id
  , hoi.order_id
  , hoi.household_id
  , hoi.product_code
  , hoi.product_name
  , case 
      when adj.old_product_price_exc_vat is not null then adj.old_product_price_exc_vat
      else hoi.product_price_exc_vat 
    end
  , hoi.product_vat_rate
  , v.multiplier
  , hoi.product_biodynamic
  , hoi.product_fair_trade
  , hoi.product_gluten_free
  , hoi.product_organic
  , hoi.product_added_sugar
  , hoi.product_vegan
  , case 
      when adj.old_quantity is not null then adj.old_quantity
      else hoi.quantity
    end
  from public.past_household_order_item hoi
  left join public.order_item_adjustment adj 
    on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id
  inner join public.product p
    on p.id = hoi.product_id
  inner join public.vat_rate v
    on p.vat_rate = v.code;

  create table v2.order_item_adjustment 
  ( order_group_id integer not null
  , order_id integer not null
  , household_id integer not null
  , product_code char(10) not null
  , new_vat_rate character(1) not null
  , new_price integer not null
  , new_quantity integer not null
  , is_discontinued boolean not null
  , date timestamp with time zone not null
  , primary key (order_id, household_id, product_code)
  , foreign key (order_group_id) references v2.order_group(id)
  , foreign key (order_id) references v2."order"(id)
  , foreign key (household_id) references v2.household(id)
  , foreign key (order_id, household_id) references v2.household_order(order_id, household_id)
  , foreign key (order_id, household_id, product_code) references v2.order_item(order_id, household_id, product_code)
  , foreign key (new_vat_rate) references v2.vat_rate(code)
  );

  insert into v2.order_item_adjustment 
  ( order_group_id
  , order_id
  , household_id
  , product_code
  , new_vat_rate
  , new_price
  , new_quantity
  , is_discontinued
  , date
  )
  select
    hoi.order_group_id
  , hoi.order_id
  , hoi.household_id
  , p.code
  , p.vat_rate
  , case when p.discontinued then 0 
         else p.price 
    end
  , hoi.quantity
  , p.discontinued
  , p.updated
    from public.household_order_item hoi
    inner join public.household_order ho 
      on ho.order_id = hoi.order_id and ho.household_id = hoi.household_id
    inner join public.product p 
      on p.id = hoi.product_id
    where p.updated > ho.updated 
  union all
  select 
    hoi.order_group_id
  , hoi.order_id
  , hoi.household_id
  , hoi.product_code
  , hoi.product_vat_rate
  , hoi.product_price_exc_vat
  , hoi.quantity
  , false
  , now()
  from public.past_household_order_item hoi
  inner join public.order_item_adjustment adj 
    on hoi.order_id = adj.order_id and hoi.household_id = adj.household_id and hoi.product_id = adj.product_id;

  create table v2.payment
  ( id             serial      not null
  , order_group_id int         not null
  , household_id   int         not null
  , "date"         timestamptz not null
  , amount         int         not null
  , is_archived    boolean     not null
  , primary key (id)
  , foreign key (household_id) references v2.household (id)
  , foreign key (order_group_id) references v2.order_group (id)
  );

  insert into v2.payment
  ( id
  , order_group_id
  , household_id
  , "date"
  , amount
  , is_archived
  )
  select
    id
  , order_group_id
  , household_id
  , date
  , amount
  , archived
  from public.household_payment;

  create table v2.file_upload
  ( id             text  not null
  , order_group_id int   not null
  , contents       bytea not null
  , primary key (id)
  , foreign key (order_group_id) references v2.order_group (id)
  );

end $$ language plpgsql;
commit;