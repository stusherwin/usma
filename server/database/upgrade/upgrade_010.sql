begin;
do $$
begin
  perform upgrade_to_version(10);

  alter table v2.order_item add is_packed   boolean   not null default false;

end $$ language plpgsql;
commit;
