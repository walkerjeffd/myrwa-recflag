const axios = require('axios');
const logger = require('./logger');
const config = require('../config');

const knex = require('knex')({
  client: 'pg',
  connection: config.db
});

// functions ------------------------------------------------------------------

function parseNwisResponse(response) {
  if (response.status !== 200) {
    throw new Error(`Response status is not 200 (${response.status})`);
  }

  if (!response.data) {
    throw new Error('Response missing data');
  }

  if (!response.data.value) {
    throw new Error('Response data missing value object');
  }

  const stationId = response.data.value.timeSeries[0].sourceInfo.siteCode[0].value;
  const values = response.data.value.timeSeries[0].values[0].value;

  return {
    stationId,
    values
  };
}

function convertValueObjects(rawData) {
  const stationId = rawData.stationId;

  const values = rawData.values.map((d) => {
    return {
      station_id: stationId,
      datetime: new Date(d.dateTime),
      flow: +d.value
    };
  });

  if (values.length > 0) {
    logger.info(`received ${values.length} values (start = ${values[0].datetime.toISOString()}, end = ${values[values.length - 1].datetime.toISOString()})`);
  } else {
    logger.info('received no raw values');
  }

  return {
    stationId,
    values
  };
}

function filterByTimestamp(data) {
  // get latest timestamp in database
  return knex('streamflow')
    .select()
    .where('station_id', data.stationId)
    .orderBy('datetime', 'desc')
    .limit(1)
    .then((rows) => {
      let values = data.values;

      if (rows.length > 0) {
        const lastTimestamp = rows[0].datetime;
        logger.info(`last database timestamp = ${lastTimestamp.toISOString()}`);
        values = values.filter(d => d.datetime > lastTimestamp);
      } else {
        logger.info('no existing data in database');
      }

      return {
        stationId: data.stationId,
        values
      };
    });
}

function saveToDatabase(data) {
  if (data.length === 0) {
    logger.info('no new values to save');
  }

  return knex('streamflow')
    .insert(data.values)
    .then((result) => {
      if (result.rowCount) {
        logger.info(`saved ${result.rowCount} new records to database`);
      } else {
        logger.info('no data saved to database');
      }
      return result;
    });
}

function fetch(params) {
  const request = {
    method: 'get',
    url: 'https://waterservices.usgs.gov/nwis/iv/',
    params: {
      format: 'json',
      sites: params.stationId,
      period: params.period,
      parameterCd: '00060',
      siteStatus: 'all'
    }
  };

  return axios.request(request)
    .then(parseNwisResponse)
    .then(convertValueObjects)
    .then(filterByTimestamp)
    .then(saveToDatabase);
}

// export ---------------------------------------------------------------------
module.exports = {
  fetch
};
