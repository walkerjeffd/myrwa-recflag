const axios = require('axios');
const moment = require('moment-timezone');
const Promise = require('bluebird');

const logger = require('./logger');
const config = require('../config');

const knex = require('knex')({
  client: 'pg',
  connection: config.db
});


// functions ------------------------------------------------------------------

function parseResponse(response, i) {
  if (response.status !== 200) {
    throw new Error(`Response status is not 200 (${response.status})`);
  }

  if (!response.data) {
    throw new Error('Response missing data');
  }

  if (!response.data.hourly) {
    throw new Error('Response data missing hourly object');
  }

  // const meta = {
  //   latitude: response.data.latitude,
  //   longitude: response.data.longitude,
  //   timezone: response.data.timezone,
  //   offset: response.data.offset,
  // };

  const values = response.data.hourly.data.map((d) => {
    return {
      datetime: new Date(d.time * 1000),
      values: d
    };
  });

  logger.info(`parsed ${values.length} values (request ${i})`);

  return values;
}

function filterByTimestamp(data) {
  // darksky timestamps are at top of the hour
  const now = moment().tz('UTC').subtract(1, 'hour').toDate();

  let values = data.filter(d => d.datetime < now);

  // get latest timestamp in database
  return knex('darksky')
    .select()
    .orderBy('datetime', 'desc')
    .limit(1)
    .then((rows) => {
      if (rows.length > 0) {
        const lastTimestamp = rows[0].datetime;

        logger.info(`last database timestamp = ${lastTimestamp.toISOString()}`);

        values = values.filter(d => d.datetime > lastTimestamp);
      } else {
        logger.info('no existing data in database');
      }

      return values;
    });
}

function saveToDatabase(data) {
  if (data.length === 0) {
    logger.info('no new values to save');
  }

  return knex('darksky')
    .insert(data)
    .then((result) => {
      if (result.rowCount) {
        logger.info(`saved ${result.rowCount} new records to database`);
      } else {
        logger.info('no data saved to database');
      }
      return result;
    });
}

function timesArray(days) {
  // create array of formatted dates starting from today
  // days: number of days back in time (1 = [today], 2 = [yesterday, today], ...)
  const currentDate = moment.tz('America/New_York').endOf('day');

  const daysArray = [];
  for (let i = days - 1; i >= 0; i -= 1) {
    daysArray.push(i);
  }

  return daysArray.map(d => currentDate.clone().subtract(d, 'days').format('YYYY-MM-DDTHH:mm:ssZZ'));
}

function createRequest(key, location, time) {
  const url = `https://api.darksky.net/forecast/${key}/${location.latitude},${location.longitude},${time}`;
  logger.info('request', { url });

  const request = {
    method: 'get',
    url,
    params: {
      exclude: ['currently', 'flags', 'alerts', 'daily', 'currently', 'minutely'].join(',')
    }
  };

  return axios.request(request);
}

function mergeResponses(responses) {
  const data = responses.map(parseResponse);
  const merged = [].concat.apply([], data);
  logger.info(`merged ${merged.length} values`);
  return merged;
}

function fetch(params) {
  const location = config.darkSky.location;
  const key = config.darkSky.key;

  const times = timesArray(params.days);

  const requests = times.map(time => createRequest(key, location, time));

  return Promise.all(requests)
    .then(mergeResponses)
    .then(filterByTimestamp)
    .then(saveToDatabase);
}

// export ---------------------------------------------------------------------
module.exports = {
  fetch
};
