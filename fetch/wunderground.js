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

function saveToDatabase(data) {
  if (!data) {
    logger.info('no new data to save');
  }

  return knex('wunderground')
    .insert(data)
    .then((result) => {
      // logger.info(result);

      if (result.rowCount) {
        logger.info(`saved ${result.rowCount} new records to database`);
      } else {
        logger.info('no data saved to database');
      }
      return result;
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

  return axios.request(request);
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

  return request
    .then(handleResponse)
    .then((data) => {
      return {
        date,
        data
      };
    })
    .then(saveToDatabase);
}

// fetch({ date: '2017-09-22' })
//   .then(() => {
//     logger.info('done');
//     process.exit(0);
//   })
//   .catch((err) => {
//     logger.error(err);
//     process.exit(1);
//   });

// export ---------------------------------------------------------------------
module.exports = {
  fetch
};
