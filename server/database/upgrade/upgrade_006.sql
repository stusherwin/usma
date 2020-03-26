begin;
do $$
begin
  perform upgrade_to_version(6);

  alter table "order"    alter column created_by_id   drop not null;
  alter table past_order alter column created_by_id   drop not null
                       , alter column created_by_name drop not null;

end $$ language plpgsql;
commit;
