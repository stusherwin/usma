create function delete_order(delete_id int) 
returns integer
as $$

delete from past_household_order_item where order_id = delete_id;
delete from past_household_order where order_id = delete_id;
delete from past_order where id = delete_id returning id;

$$
language sql;