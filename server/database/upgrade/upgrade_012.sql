begin;
do $$
begin
  perform upgrade_to_version(12);

  alter table v2.catalogue_file add uploaded_date   timestamptz   not null default '2022-01-01';
end $$ language plpgsql;
commit;