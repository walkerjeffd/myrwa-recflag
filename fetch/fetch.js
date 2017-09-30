const logger = require('./logger');
const program = require('commander');
const fetchStreamflow = require('./streamflow').fetch;
const fetchDarkSky = require('./darksky').fetch;
const fetchWunderground = require('./wunderground').fetch;

// main program ---------------------------------------------------------------
const startTime = new Date();

program
  .version('0.1.0');

program
  .command('streamflow <stationId>')
  .description('Fetch instantaneous streamflow observations for a single station from NWIS and save new data to database')
  .option('-P, --period [period]', 'USGS Time Period [P1D]', 'P1D')
  .action((stationId, command) => {
    logger.info('fetching streamflow', { stationId, period: command.period });
    fetchStreamflow({ stationId, period: command.period })
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
  .command('darksky')
  .description('Fetch hourly weather data from DarkSky API and save new data to database')
  .option('-D, --days [no. of days]', 'Number of days to fetch [1=today only]', 1)
  .action((command) => {
    logger.info('fetching darksky');
    fetchDarkSky({ days: command.days })
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
  .description('Fetch hourly weather data from Wunderground API and save new data to database')
  .option('-D, --date [YYYY-MM-DD]', 'Date to fetch')
  .action((command) => {
    logger.info('fetching darksky');
    fetchWunderground({ date: command.date })
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

program.parse(process.argv);
