begin;
do $$
begin
  perform upgrade_to_version(11);

  create table v2.catalogue_file
  ( id           serial  not null primary key
  , "file"       text    not null
  );
end $$ language plpgsql;
commit;
