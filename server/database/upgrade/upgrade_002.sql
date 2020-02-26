begin;
do $$
begin
  perform upgrade_to_version(2);

  create table product_image
  ( code           text        not null primary key
  , image          bytea       not null
  );
end $$ language plpgsql;
commit;
