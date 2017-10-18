-- MyRWA Recreational Flagging Database

CREATE TABLE streamflow (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now(),
  station_id TEXT,
  date DATE,
  flow REAL
);
CREATE UNIQUE INDEX streamflow_idx_date_flow ON streamflow (station_id, date);

CREATE TABLE wunderground (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now(),
  date DATE,
  json JSON
);
CREATE UNIQUE INDEX wunderground_idx_date ON wunderground (date);

CREATE TABLE predictions (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now(),
  name TEXT,
  uuid TEXT,
  predictors JSON,
  prob REAL,
  exceedance BOOLEAN,
  timestamp TIMESTAMP WITH TIME ZONE
);
CREATE UNIQUE INDEX predictions_idx_name_timestamp ON predictions (name, timestamp);

CREATE TABLE flags (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now(),
  location_id TEXT NOT NULL,
  start_timestamp TIMESTAMP WITH TIME ZONE NOT NULL,
  end_timestamp TIMESTAMP WITH TIME ZONE NOT NULL,
  type TEXT NOT NULL,
  level TEXT NOT NULL,
  description TEXT
);
