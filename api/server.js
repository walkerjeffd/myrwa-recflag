/* eslint no-console: "off" */

const fs = require('fs');
const express = require('express');
const morgan = require('morgan');
const bodyParser = require('body-parser');
const _ = require('lodash');
const moment = require('moment-timezone');

const config = require('../config');
const db = require('./db');

const app = express();

console.log('booting');

// access logging
morgan.token('real-ip', req => req.headers['x-real-ip'] || req.connection.remoteAddress);
const logFormat = ':date[iso] :real-ip :remote-user :method :url HTTP/:http-version :status :res[content-length] - :response-time ms';
const accessLogStream = fs.createWriteStream(config.api.logFile, { flags: 'a' });
app.use(morgan(logFormat, { stream: accessLogStream }));

// body parser (json only)
app.use(bodyParser.json());


// allow CORS
const allowCrossDomain = (req, res, next) => {
  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE');
  res.header('Access-Control-Allow-Headers', 'Content-Type');
  next();
};
app.use(allowCrossDomain);


// paths to app builds
app.use('/static/map', express.static('../apps/map/dist'));
app.use('/reports', express.static('../r/pdf'));

// pages
// app.use('/www/', express.static('./www/'));

// endpoints
app.get('/', (req, res) => {
  res.status(200).json({ status: 'ok', data: [] });
});

function assignStatus(prediction) {
  let type = 'unknown';
  let label = 'Not Available';
  let reason = 'Data unavailable or system error';

  if (_.isNull(prediction.exceedance) || _.isUndefined(prediction.exceedance)) {
    type = 'unknown';
    label = 'Not Available';
    reason = 'Data unavailable or system error.';
  } else if (prediction.exceedance) {
    type = 'advisory';
    label = 'Advisory';
    reason = 'High probability of elevated bacteria levels.';
  } else {
    type = 'good';
    label = 'Good';
    reason = 'Low probability of elevated bacteria levels.';
  }

  return { type, label, reason };
}

const sites = {
  MYSTIC_ECOLI: {
    name: 'Mystic River',
    description: 'Mystic Valley Parkway (Rt 16)',
    latitude: 42.405722,
    longitude: -71.096351
  },
  MALDENLOWER_ECOLI: {
    name: 'Malden River',
    description: 'Revere Beach Parkway (Rt 16)',
    latitude: 42.4053,
    longitude: -71.07191
  },
  SHANNON_ENT: {
    name: 'Upper Mystic Lake',
    description: 'Shannon Beach',
    latitude: 42.439892,
    longitude: -71.146153
  }
};

app.get('/predictions/', (req, res, next) => {
  db.getPredictions()
    .then((results) => {
      // randomize exceedances
      results.forEach((d) => {
        d.prob = Math.random();
        d.exceedance = d.prob > 0.45;
        if (d.prob > 0.9) {
          d.prob = null;
          d.exceedance = null;
        }
      });

      // add local timestamp string
      results.forEach((d) => {
        d.timestamp_local = moment.tz(d.timestamp, 'US/Eastern').format('MMM D YYYY H:mm a z');
      });

      // create response data
      const names = _.uniq(results.map(d => d.name));
      const data = names.map((name) => {
        const d = {
          name,
          site: sites[name],
          current: results.filter(p => p.name === name)[0],
          history: results.filter(p => p.name === name),
          flags: []
        };

        // assign status
        d.current.status = assignStatus(d.current, d.flags);
        d.history.forEach((p) => {
          p.status = assignStatus(p, d.flags);
        });
        return d;
      });

      return data;
    })
    .then(data => res.status(200).json({ status: 'ok', data }))
    .catch(next);
});

// error handler
function errorHandler(err, req, res, next) { // eslint-disable-line no-unused-vars
  console.error(err.toString());
  return res.status(500).json({
    status: 'error',
    error: {
      data: err,
      message: err.toString(),
    },
  });
}
app.use(errorHandler);

// start server
app.listen(config.api.port, () => {
  console.log('listening port=%d', config.api.port);
});
