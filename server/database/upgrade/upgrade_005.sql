begin;
do $$
begin
  perform upgrade_to_version(5);

  -- do upgrade
end $$ language plpgsql;
rollback; -- commit;