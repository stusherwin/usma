begin;
do $$
begin
  perform upgrade_to_version(5);

  alter table order_group add enable_payments   boolean   not null default true;

end $$ language plpgsql;
commit;
