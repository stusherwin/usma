begin;
do $$
begin
  perform upgrade_to_version(7);

  create table order_file_upload
  ( id       text        not null primary key
  , contents bytea       not null
  );
end $$ language plpgsql;
commit;