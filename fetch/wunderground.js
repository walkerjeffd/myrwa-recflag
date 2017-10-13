const axios = require('axios');
const Moment = require('moment-timezone');
const MomentRange = require('moment-range');

const moment = MomentRange.extendMoment(Moment);

const logger = require('./logger');
const config = require('../config');

const knex = require('knex')({
  client: 'pg',
  connection: config.db
});


// functions ------------------------------------------------------------------

function insertDatabase(data) {
  if (!data) {
    logger.info('no new data to save');
  }

  return knex('wunderground')
    .insert(data)
    .returning(['id', 'date'])
    .then((result) => {
      // logger.info(result);

      if (result.length > 0) {
        logger.info(`saved ${result.length} record to database (id=${result[0].id}, date=${result[0].date.toISOString().substr(0, 10)})`);
      } else {
        logger.info('no data saved to database');
      }
      return result;
    });
}

function updateDatabase(data) {
  return knex('wunderground')
    .where('date', data.date)
    .update('json', data.json)
    .returning('*')
    .then((result) => {
      if (result.length > 0) {
        logger.info(`saved ${result.length} record to database (id=${result[0].id}, date=${result[0].date.toISOString().substr(0, 10)})`);
      } else {
        logger.info('no data saved to database');
      }
      return result;
    });
}

function upsertDatabase(data) {
  if (!data) {
    logger.info('missing data for upsert');
    return Promise.reject('data is undefined');
  }

  return knex('wunderground')
    .where('date', data.date)
    .count('*')
    .then((results) => {
      const count = +results[0].count;
      let promise;
      if (count > 0) {
        logger.info(`row with date=${data.date} already exists, updating`);
        promise = updateDatabase(data);
      } else {
        logger.info(`row with date=${data.date} does not already exist, inserting`);
        promise = insertDatabase(data);
      }
      return promise;
    });
}

function createRequest(key, date) {
  const dateString = date.replace(/-/g, '');
  const url = `http://api.wunderground.com/api/${key}/history_${dateString}/q/MA/Boston.json`;

  logger.info('request', { url });

  const request = {
    method: 'get',
    url
  };

  return request;
}

function handleResponse(response) {
  if (response.status !== 200) {
    throw new Error(`Response status is not 200 (${response.status})`);
  }

  if (!response.data.history) {
    throw new Error('Response data missing history object');
  }

  return response.data;
}

function fetch(params) {
  const key = config.wunderground.key;
  const date = params.date;
  const request = createRequest(key, date);

  return axios.request(request)
    .then(handleResponse)
    .then((data) => {
      return {
        date,
        json: data
      };
    })
    .then(upsertDatabase);
}

// export ---------------------------------------------------------------------
module.exports = {
  fetch
};
