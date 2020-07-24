begin;
do $$
begin
  grant all on schema public to test_user;
  grant all on all tables in schema public to test_user;
  grant all on all sequences in schema public to test_user;
  grant all on schema v2 to test_user;
  grant all on all tables in schema v2 to test_user;
  grant all on all sequences in schema v2 to test_user;

  insert into public.order_group ("name", "key", enable_payments) values ('Test Group', 'TEST', true);
  insert into v2.order_group ("name", "key", is_payments_enabled) values ('Test Group', 'TEST', true);
end $$ language plpgsql;
commit;