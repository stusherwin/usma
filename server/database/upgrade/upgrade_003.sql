CREATE TABLE
GRANT
GRANT
stment
( order_id                  int  not null
, household_id              int  not null
, product_id                int  not null
, order_group_id            int  not null
, old_product_price_exc_vat int  not null
, old_product_price_inc_vat int  not null
, old_quantity              int  not null
, old_item_total_exc_vat    int  not null
, old_item_total_inc_vat    int  not null
, primary key (order_id, household_id, product_id)
, foreign key (order_id, household_id, product_id) references past_household_order_item (order_id, household_id, product_id)
, foreign key (order_group_id) references order_group (id)
)

-- create function int_sum_null(int, int)
-- returns int language sql as $$
--     select $1 + $2
-- $$;

-- create aggregate sumnull(integer) (
--     sfunc = int_sum_null,
--     stype = int
-- );