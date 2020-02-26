begin;
do $$
begin
  perform upgrade_to_version(4);

  create function delete_order(delete_id int) 
  returns integer
  as $func$
    delete from past_household_order_item where order_id = delete_id;
    delete from past_household_order where order_id = delete_id;
    delete from past_order where id = delete_id returning id;
  $func$ language sql;
end $$ language plpgsql;
commit;
