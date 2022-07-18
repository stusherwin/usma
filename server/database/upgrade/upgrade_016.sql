begin;
do $$
begin
  perform upgrade_to_version(16);

  delete from v2.product_image;

end $$ language plpgsql;
commit;