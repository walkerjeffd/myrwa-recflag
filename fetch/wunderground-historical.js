const Moment = require('moment-timezone');
const MomentRange = require('moment-range');

const moment = MomentRange.extendMoment(Moment);

const config = require('../config');
const logger = require('./logger');
const fetch = require('./wunderground').fetch;

const range = moment.range(config.wunderground.history.start, config.wunderground.history.end);

const dateArray = Array.from(range.by('day')).map(d => d.format('YYYY-MM-DD'));

const delay = 3 * 60 * 10000; // 3 minutes

function run(i) {
  if (i < 0) {
    process.exit(0);
  }

  const date = dateArray[i];

  logger.info(`fetching "${date}"`);
  fetch({ date })
    .then(() => {
      logger.info(`done "${date}", waiting ${delay} ms`);
      setTimeout(() => {
        run(i - 1);
      }, delay);
    })
    .catch((err) => {
      logger.error(`error occurred for "${date}"`);
      logger.error(err);
      process.exit(1);
    });
}

run(dateArray.length - 1);
