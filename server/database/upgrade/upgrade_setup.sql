begin;
do $$
begin
  if not exists(with t as (select to_regclass('db_upgrade') as x) select x from t where x is not null) then
    create table db_upgrade
    ( version int not null
    );
  end if;

  if not exists(with t as (select to_regproc('upgrade_to_version') as x) select x from t where x is not null) then
    create function upgrade_to_version(to_version int) 
    returns void
    as $func$
      declare curr_version integer;
      begin
        if not exists(with t as (select to_regclass('db_upgrade') as x) select x from t where x is not null) then
          raise exception 'Upgrade failed: db_upgrade table doesn''t exist. Run upgrade_setup.sql first.';
        end if;

        select version from db_upgrade limit 1 into curr_version;

        if curr_version < to_version - 1 then
          raise exception 'Upgrade failed: only at version %. Run upgrade_%.sql first.', curr_version, lpad(cast(curr_version + 1 as text), 3, '0');
        end if;

        if curr_version >= to_version then
          raise exception 'Upgrade not run. Already at version %', curr_version;
        end if;

        raise notice 'Upgrading from version % to version %...', curr_version, to_version;
        
        update db_upgrade set version = to_version;
      end;
    $func$ language plpgsql;
  end if;
end $$ language plpgsql;
commit;