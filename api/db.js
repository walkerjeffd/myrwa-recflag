const config = require('../config');

const knex = require('knex')({
  client: 'pg',
  connection: config.db,
});

function getPredictions() {
  const count = knex('predictions')
    .select()
    .orderBy('timestamp', 'desc')
    .whereRaw("date_trunc('day', timestamp at time zone 'US/Eastern') >= (date_trunc('day', now() at time zone 'US/Eastern') - interval '6 days')");
  return count;
}

module.exports = {
  getPredictions
};
