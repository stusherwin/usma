begin;
do $$
begin
  perform upgrade_to_version(13);

  drop table v2.catalogue_entry;
end $$ language plpgsql;
commit;