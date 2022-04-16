begin;
do $$
begin
  perform upgrade_to_version(14);

  drop table household_order_item;
  drop table household_order;
  drop table household_payment;
  drop table order_item_adjustment;
  drop table past_household_order_item;
  drop table past_household_order;
  drop table past_order;
  drop table product_image;
  drop table order_file_upload;
  drop table "order";
  drop table household;
  drop table product;
  drop table vat_rate;
  drop table order_group;

end $$ language plpgsql;
commit;