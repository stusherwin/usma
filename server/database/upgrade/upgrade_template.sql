begin;
do $$
begin
  perform upgrade_to_version(?);

  -- do upgrade
end $$ language plpgsql;
rollback; -- commit;