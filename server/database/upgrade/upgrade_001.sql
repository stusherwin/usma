do $$
begin
  if can_upgrade(1) then
    alter table product add biodynamic   boolean   not null default false;
    alter table product add fair_trade   boolean   not null default false;
    alter table product add gluten_free  boolean   not null default false;
    alter table product add organic      boolean   not null default false;
    alter table product add added_sugar  boolean   not null default false;
    alter table product add vegan        boolean   not null default false;

    alter table past_household_order_item add product_biodynamic   boolean   not null default false;
    alter table past_household_order_item add product_fair_trade   boolean   not null default false;
    alter table past_household_order_item add product_gluten_free  boolean   not null default false;
    alter table past_household_order_item add product_organic      boolean   not null default false;
    alter table past_household_order_item add product_added_sugar  boolean   not null default false;
    alter table past_household_order_item add product_vegan        boolean   not null default false;

    update product p set
        biodynamic = c.biodynamic
      , fair_trade = c.fair_trade
      , gluten_free = c.gluten_free
      , organic = c.organic
      , added_sugar = c.added_sugar
      , vegan = c.vegan
    from catalogue_entry c
    where c.code = p.code;

    update past_household_order_item p set
        product_biodynamic = c.biodynamic
      , product_fair_trade = c.fair_trade
      , product_gluten_free = c.gluten_free
      , product_organic = c.organic
      , product_added_sugar = c.added_sugar
      , product_vegan = c.vegan
    from catalogue_entry c
    where c.code = p.product_code;

    update db_upgrade set version = 1;
  end if;
end $$