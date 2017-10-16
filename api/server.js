/* eslint no-console: "off" */

const fs = require('fs');
const express = require('express');
const morgan = require('morgan');
const bodyParser = require('body-parser');
const _ = require('lodash');

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

app.get('/predictions/', (req, res, next) => {
  db.getPredictions()
    .then((result) => {
      const names = _.uniq(result.map(d => d.name));

      const data = names.map((name) => {
        return {
          name,
          current: result.filter(d => d.name === name)[0],
          history: result.filter(d => d.name === name)
        };
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
