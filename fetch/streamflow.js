const axios = require('axios');
const logger = require('./logger');
const config = require('../config');

const knex = require('knex')({
  client: 'pg',
  connection: config.db
});

// functions ------------------------------------------------------------------

function parseResponse(response) {
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
      date: d.dateTime.substr(0, 10),
      flow: +d.value
    };
  });

  if (values.length > 0) {
    logger.info(`received ${values.length} values (start = ${values[0].date}, end = ${values[values.length - 1].date})`);
  } else {
    logger.info('received no raw values');
  }

  return {
    stationId,
    values
  };
}

// function filterByTimestamp(data) {
//   // get latest timestamp in database
//   return knex('streamflow')
//     .select()
//     .where('station_id', data.stationId)
//     .orderBy('datetime', 'desc')
//     .limit(1)
//     .then((rows) => {
//       let values = data.values;

//       if (rows.length > 0) {
//         const lastTimestamp = rows[0].datetime;
//         logger.info(`last database timestamp = ${lastTimestamp.toISOString()}`);
//         values = values.filter(d => d.datetime > lastTimestamp);
//       } else {
//         logger.info('no existing data in database');
//       }

//       return {
//         stationId: data.stationId,
//         values
//       };
//     });
// }

function saveToDatabase(data) {
  if (data.values.length === 0) {
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
  const urlParams = {
    format: 'json',
    sites: params.stationId,
    parameterCd: '00060'
  };

  if (params.period) {
    urlParams.period = params.period;
  } else if (params.start && params.end) {
    urlParams.startDT = params.start;
    urlParams.endDT = params.end;
  } else {
    return Promise.reject('Missing period or start/end parameters');
  }

  const request = {
    method: 'get',
    url: 'https://waterservices.usgs.gov/nwis/dv/',
    params: urlParams
  };

  return axios.request(request)
    .then(parseResponse)
    .then(convertValueObjects)
    // .then(filterByTimestamp)
    .then(saveToDatabase);
}

// export ---------------------------------------------------------------------
module.exports = {
  fetch
};
