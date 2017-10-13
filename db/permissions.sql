-- create new user manually in psql
-- CREATE ROLE myrwa_www WITH LOGIN PASSWORD '<PASSWORD>'

GRANT CONNECT ON DATABASE recflag TO myrwa_www;

GRANT SELECT ON ALL TABLES IN SCHEMA public TO myrwa_www;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO myrwa_www;
