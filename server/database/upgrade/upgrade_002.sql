do $$
begin
  if can_upgrade(2) then
    create table product_image
    ( code           text        not null primary key
    , image          bytea       not null
    );

    update db_upgrade set version = 2;
  end if;
end $$