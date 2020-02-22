do $$
begin
  if can_upgrade(4) then
    create function delete_order(delete_id int) 
    returns integer
    as $func$

    delete from past_household_order_item where order_id = delete_id;
    delete from past_household_order where order_id = delete_id;
    delete from past_order where id = delete_id returning id;

    $func$
    language sql;

    update db_upgrade set version = 4;
  end if;
end $$