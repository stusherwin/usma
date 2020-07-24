begin;
do $$
begin
  drop schema public cascade;
  drop schema if exists v2 cascade;
  create schema public;
  GRANT ALL ON SCHEMA public TO postgres;
  GRANT ALL ON SCHEMA public TO public;
  COMMENT ON SCHEMA public IS 'standard public schema';
  grant all on schema public to test_user;
end $$ language plpgsql;
commit;