begin;
do $$
begin
  perform upgrade_to_version(15);

  drop table v2.product_image;

  create table v2.product_data
  ( code      text  not null primary key
  , image     bytea null
  , url       text  null
  , title     text  null
  , imageUrl  text  null
  , size      int   null
  );
    
end $$ language plpgsql;
commit;