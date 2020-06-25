begin;
do $$
begin
  perform upgrade_to_version(8);

end $$ language plpgsql;
commit;