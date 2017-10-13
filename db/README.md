Mystic RecFlag - Database
===================================

## Configuration

See `/config/index.template.js`

```js
{
  db: {
    host: '',
    port: 5432,
    database: '',
    user: '',
    password: ''
  },
  ...
}
```

## Users

Assumes two users have been created

```bash
myrwa_admin # superuser
myrwa_www   # primary web user
```

## Set Up

```bash
# manually create myrwa_www role
psql -h <hostname> -p 5432 -U myrwa_admin -d recflag -f db/permissions.sql

# install schema
psql -h <hostname> -p 5432 -U myrwa_admin -d recflag -f schema.sql
```

## Copy Data

To copy data from one server to another, use `pg_dump`. May need to truncate table on target server.

```bash
pg_dump --host [SERVER 1] --username=[USER] --encoding=utf8 --no-owner -a -t [TABLE] recflag > server1-table.sql
pgsql --host [SERVER 2] --username=[USER] -d recflag -f server1-table.sql
```
