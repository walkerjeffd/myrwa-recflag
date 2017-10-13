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
