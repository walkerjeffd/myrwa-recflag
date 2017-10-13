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
