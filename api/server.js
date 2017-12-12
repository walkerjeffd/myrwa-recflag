const fs = require('fs');
const express = require('express');
const morgan = require('morgan');
const bodyParser = require('body-parser');
const _ = require('lodash');
const moment = require('moment-timezone');
const jwt = require('jsonwebtoken');

const config = require('../config');
const db = require('./db');
const utils = require('../utils');

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
  res.header('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE,OPTIONS');
  res.header('Access-Control-Allow-Headers', 'Content-Type,Authorization');
  next();
};
app.use(allowCrossDomain);


// paths to app builds
app.use('/static/map', express.static('../apps/map/dist'));
app.use('/admin', express.static('../apps/admin/dist'));
app.use('/reports', express.static('../r/pdf'));
app.use('/rmd', express.static('../r/rmd'));
app.use('/display', express.static('../apps/display'));

// pages
// app.use('/www/', express.static('./www/'));


// middleware
function getTokenFromRequest(req) {
  let token;
  if (req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Bearer') {
    token = req.headers.authorization.split(' ')[1];
  }
  return token;
}

function requireAuth(req, res, next) {
  const token = getTokenFromRequest(req);
  jwt.verify(token, config.admin.secret, (err) => {
    if (err) {
      return res.status(401).json({ status: 'error', data: { message: 'Invalid or expired token' } });
    }
    return next();
  });
}

// endpoints
app.get('/', (req, res) => {
  res.status(200).json({ status: 'ok', data: [] });
});

app.get('/predictions/', (req, res, next) => {
  Promise.all([db.getPredictions(), db.getFlags()])
    .then((results) => {
      const predictions = results[0];
      const flags = results[1];

      if (config.api.randomize) {
        // randomize exceedances
        predictions.forEach((d) => {
          d.prob = Math.random();
          d.exceedance = d.prob > 0.45;
          if (d.prob > 0.9) {
            d.prob = null;
            d.exceedance = null;
          }
        });
      }

      // add local timestamp string
      predictions.forEach((d) => {
        d.timestamp_local = moment.tz(d.timestamp, 'US/Eastern').format('MMM D YYYY H:mm a z');
        d.flags = flags.filter((flag) => {
          const start = new Date(flag.start_timestamp);
          const end = new Date(flag.end_timestamp);
          const predictionTimestamp = new Date(d.timestamp);

          return flag.location_id === d.name &&
            start <= predictionTimestamp &&
            predictionTimestamp <= end;
        });
        d.status = utils.assignStatus(d, d.flags);
      });

      // create response data
      const names = _.uniq(predictions.map(d => d.name));
      const data = names.map((name) => {
        const d = {
          name,
          site: utils.sites[name],
          current: predictions.filter(p => p.name === name)[0],
          history: predictions.filter(p => p.name === name),
          flags: flags.filter(f => f.location_id === name)
        };

        // assign status
        d.current.status = utils.assignStatus(d.current, d.flags.filter(f => f.status === 'ACTIVE'));

        return d;
      });

      return data;
    })
    .then(data => res.status(200).json({ status: 'ok', data }))
    .catch(next);
});

app.get('/flags', (req, res, next) => {
  db.getFlags()
    .then(data => res.status(200).json({ status: 'ok', data }))
    .catch(next);
});

app.get('/flags/:id', (req, res, next) => {
  db.getFlag(+req.params.id)
    .then(data => res.status(200).json({ status: 'ok', data }))
    .catch(next);
});

app.post('/flags', requireAuth, (req, res, next) => {
  if (!req.body) {
    res.status(400).json({ status: 'error', error: { message: 'Missing flag data' } });
  }

  db.createFlag(req.body)
    .then(data => res.status(201).json({ status: 'ok', data }))
    .catch(next);
});

app.post('/flags/:id', requireAuth, (req, res, next) => {
  if (!req.body) {
    res.status(400).json({ status: 'error', error: { message: 'Missing flag data' } });
  }

  db.updateFlag(req.body)
    .then(data => res.status(201).json({ status: 'ok', data }))
    .catch(next);
});

app.delete('/flags/:id', requireAuth, (req, res, next) => {
  db.deleteFlag(req.params.id)
    .then(() => res.status(201).json({ status: 'ok', data: {} }))
    .catch(next);
});

app.post('/auth/login', (req, res) => {
  if (!req.body || !req.body.username || !req.body.password) {
    return res.status(401).json({ status: 'error', error: { message: 'Missing username and/or password' } });
  }

  if (!(req.body.username === config.admin.username &&
        req.body.password === config.admin.password)) {
    return res.status(401).json({ status: 'error', error: { message: 'Invalid username or password' } });
  }

  const token = jwt.sign({ username: req.body.username }, config.admin.secret, { expiresIn: '1d' });
  return res.status(200).json({ status: 'ok', data: { access_token: token } });
});

app.post('/auth/check', (req, res) => {
  if (!req.body || !req.body.access_token) {
    return res.status(401).json({ status: 'error', data: { message: 'Missing token' } });
  }

  jwt.verify(req.body.access_token, config.admin.secret, (err) => {
    if (err) {
      return res.status(401).json({ status: 'error', data: { message: 'Invalid or expired token' } });
    }
    return res.status(200).json({ status: 'ok', data: { access_token: req.body.access_token } });
  });
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
