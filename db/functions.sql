-- extract and parse hourly metar data
CREATE OR REPLACE FUNCTION f_metar_hourly (start_date TEXT, end_date TEXT)
RETURNS TABLE(
  id INT,
  date TEXT,
  observation_time TIMESTAMP WITH TIME ZONE,
  temp_c REAL,
  dewpoint_c REAL,
  wind_speed_kt REAL,
  precip_in REAL,
  sea_level_pressure_mb REAL
) AS $$
  WITH t AS (
    SELECT
      id,
      date,
      json_array_elements(json)->>'metar_type' AS metar_type,
      (json_array_elements(json)->>'observation_time')::TIMESTAMP WITH TIME ZONE AS observation_time,
      (json_array_elements(json)->>'temp_c') AS temp_c,
      (json_array_elements(json)->>'dewpoint_c') AS dewpoint_c,
      (json_array_elements(json)->>'wind_speed_kt') AS wind_speed_kt,
      (json_array_elements(json)->>'precip_in') AS precip_in,
      (json_array_elements(json)->>'sea_level_pressure_mb') AS sea_level_pressure_mb
    FROM metar
    WHERE date >= start_date::date AND date <= end_date::date
  )
  SELECT
    id,
    date::TEXT,
    observation_time,
    NULLIF(temp_c, '')::REAL,
    NULLIF(dewpoint_c, '')::REAL,
    NULLIF(wind_speed_kt, '')::REAL,
    COALESCE(NULLIF(precip_in, '')::REAL, 0),
    NULLIF(sea_level_pressure_mb, '')::REAL
  FROM t
  WHERE metar_type='METAR'
  ORDER BY observation_time;
$$ LANGUAGE SQL;
