-- MyRWA Recreational Flagging Database

CREATE TABLE locations (
  id TEXT PRIMARY KEY,
  location_description TEXT,
  latitude REAL,
  longitude REAL
);

CREATE TABLE streamflow (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now(),
  station_id TEXT,
  datetime TIMESTAMP WITH TIME ZONE,
  flow REAL
);

CREATE TABLE darksky (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now(),
  datetime TIMESTAMP WITH TIME ZONE,
  values JSON
);

