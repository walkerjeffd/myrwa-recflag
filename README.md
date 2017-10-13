MyRWA Recreational Flagging Project
===================================

Jeffrey D Walker, PhD  
[Walker Environmental Research, LLC](https//walkerenvres.com)

Prepared for: [Mystic River Watershed Association](https://mysticriver.org)

## About

The MyRWA Recreational Flagging Project provides near real-time predictions of bacteria exceedences at locations within the Mystic River Watershed. This repo contains the source code for the database, models, and web applications.

## Configuration

```bash
cp config/index.template.json config/index.json
nano config/index.json
```

## Database

### Set Up

```bash
createdb recflag
psql -d recflag -f db/schema.sql
```

## Data Fetching

The `fetch.js` command line utility can be used to fetch and save new streamflow and weather data to the database.

Note you must be in the `fetch/` directory to run the following commands.

```bash
cd fetch
```

### Streamflow Data

Streamflow data are fetched using the Daily Values (DV) service from the [USGS Water Services API](https://waterservices.usgs.gov). Time periods are defined using either the [ISO 8601 Durations Standard](https://en.wikipedia.org/wiki/ISO_8601#Durations) (`P1D` = previous 1 day, `PT6H` = previous 6 hours) or start/end dates.

```bash
node fetch.js streamflow --help
node fetch.js streamflow <stationId> -P [period] -S [start date, YYYY-MM-DD] -E [end date, YYYY-MM-DD]
# example for 5 days of data at Aberjona River
node fetch.js streamflow 01102500 -P P5D
# example for specific period at Aberjona River
node fetch.js streamflow 01102500 -S 2017-10-01 -E 2017-10-15
```

### Wunderground Weather Data

Hourly weather data are fetched from the [Wunderground API](https://www.wunderground.com/weather/api/). The `config` file must contain `wundergound.key` (API key).

```bash
node fetch.js wunderground --help
node fetch.js wunderground -D [date]
# example
node fetch.js wunderground -D 2017-10-13
node fetch.js wunderground -Y # yesterday
node fetch.js wunderground -T # today
```

To download wunderground data in batch mode with delay between requests.

```bash
node fetch.js wunderground-batch -S [start date] -E [end date] -D [delay in seconds]
# example
node fetch.js wunderground-batch -S 2017-10-10 -E 2017-10-11 -D 15
```

### Daily Auto-Update

Each morning, update yesterday's streamflow and wunderground data.

```bash
node fetch.js streamflow 01102500 -P P1D
node fetch.js wunderground -Y
```

## Prediction Models

### Model Development

Logistic regression models are developed using the `r/models.R` script. The results are saved to `r/models.rds`.

### Model Execution

To generate and save predictions for each model at the most recent timestamp, first update wunderground dataset for today.

```bash
cd fetch
node fetch.js wunderground -T
```

Then run predict.R script with no arguments to use the latest available timestamp:

```bash
cd ../r
Rscript predict.R
```

To generate and save predictions for a specific timestamp, pass in one argument:

```bash
Rscript predict.R "YYYY-mm-dd HH:MM"
# or
Rscript predict.R YYYYmmddHHMM
# example
Rscript predict.R 201710010700
```

To generate and save predictions for a range of timestamps, pass in three arguments (start, end, interval):

```bash
Rscript predict.R "YYYY-mm-dd HH:MM" "YYYY-mm-dd HH:MM" "interval"
# or
Rscript predict.R YYYYmmddHHMM YYYYmmddHHMM interval
# example
Rscript predict.R 201705010700 201710010700 day
```

## Web Applications

### Map

Marker Icon: https://www.iconfinder.com/icons/299087/map_marker_icon

```bash
npm run serve
npm run dev
npm run build
```
