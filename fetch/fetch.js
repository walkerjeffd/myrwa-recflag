const logger = require('./logger');
const program = require('commander');
const moment = require('moment-timezone');
const fetchStreamflow = require('./streamflow').fetch;
const fetchWunderground = require('./wunderground').fetch;

const startTime = new Date();

program
  .version('0.1.0');

program
  .command('streamflow <stationId>')
  .description('Fetch daily streamflow observations for a single station from NWIS and save new data to database')
  .option('-P, --period [period]', 'USGS Time Period [P1D], overrides start and end dates')
  .option('-S, --start [start date]', 'Start Date (YYYY-MM-DD)')
  .option('-E, --end [end date]', 'End Date (YYYY-MM-DD)')
  .action((stationId, command) => {
    logger.info('fetching streamflow', { stationId, period: command.period, start: command.start, end: command.end });

    fetchStreamflow({ stationId, period: command.period, start: command.start, end: command.end })
      .then(() => {
        const endTime = new Date();
        logger.info('done (duration = %d sec)', (endTime - startTime) / 1000);
        process.exit(0);
      })
      .catch((error) => {
        logger.error(error.toString());
        process.exit(1);
      });
  });

program
  .command('wunderground')
  .description('Fetch one day of weather data from Wunderground API and save to database')
  .option('-D, --date [YYYY-MM-DD]', 'Date to fetch')
  .option('-Y, --yesterday', 'Yesterday')
  .option('-T, --today', 'Today')
  .action((command) => {
    logger.info('fetching wunderground');

    let date = command.date;
    if (command.yesterday) {
      date = moment.tz(new Date(), 'US/Eastern').subtract(1, 'day').format('YYYY-MM-DD');
    }
    if (command.today) {
      date = moment.tz(new Date(), 'US/Eastern').format('YYYY-MM-DD');
    }

    fetchWunderground({ date })
      .then(() => {
        const endTime = new Date();
        logger.info('done (duration = %d sec)', (endTime - startTime) / 1000);
        process.exit(0);
      })
      .catch((error) => {
        logger.error(error.toString());
        process.exit(1);
      });
  });

program
  .command('wunderground-batch')
  .description('Fetch daily weather data from Wunderground API in batch mode and save to database')
  .option('-S, --start [YYYY-MM-DD]', 'Start date')
  .option('-E, --end [YYYY-MM-DD]', 'End date')
  .option('-D, --delay [seconds]', 'Delay between requests in seconds')
  .action((command) => {
    logger.info('fetching wunderground-batch');

    const range = moment.range(command.start, command.end);
    const dateArray = Array.from(range.by('day')).map(d => d.format('YYYY-MM-DD'));
    const delay = command.delay * 1000;

    function run(i) {
      if (i < 0) {
        process.exit(0);
      }

      const date = dateArray[i];

      logger.info(`fetching "${date}"`);
      fetchWunderground({ date })
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
  });


program.parse(process.argv);
