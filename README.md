MyRWA Recreational Flagging Project
===================================

Jeffrey D Walker, PhD  
[https//walkerenvres.com]()

Prepared for: [Mystic River Watershed Association](https://mysticriver.org)

## About

The MyRWA Recreational Flagging Project provides near real-time predictions of bacteria exceedences at locations within the Mystic River Watershed. This repo contains the source code for the database, models, and web applications.

## Database

### Set Up

```sh
createdb recflag
psql -d recflag -f db/schema.sql
```

## Data Fetching

The `fetch.js` command line utility can be used to fetch and save new streamflow and weather data to the database.

Note you must be in the `fetch/` directory to run the following commands.

```sh
cd fetch
```

### Streamflow Data

Streamflow data are fetched using the Instantaneous Values (IV) service from the [USGS Water Services API](https://waterservices.usgs.gov). Time periods are defined using the [ISO 8601 Durations Standard](https://en.wikipedia.org/wiki/ISO_8601#Durations) (`P1D` = previous 1 day, `PT6H` = previous 6 hours).

```sh
node fetch.js streamflow --help
node fetch.js streamflow <stationId> -P [period, default=P1D]
# example for 5 days of data at Aberjona River
node fetch.js streamflow 01102500 -P P5D
```

### DarkSky Weather Data

Hourly weather data are fetched from the [Dark Sky API](https://darksky.net/dev/). The `config` file must contain the `darksky.key` (API key) and `darksky.location = {latitude, longitude}` options.

```sh
node fetch.js darksky --help
node fetch.js darksky -D [days, default=1]
# example for 5 past days (include today up to previous hour)
node fetch.js darksky -D 5
```

## Web Applications