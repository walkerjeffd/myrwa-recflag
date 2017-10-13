const config = require('../config');

const knex = require('knex')({
  client: 'pg',
  connection: config.db,
});

function getPredictions() {
  const count = knex('predictions')
    .select()
    .orderBy('timestamp', 'desc')
    .limit(3);

  return count;
}

module.exports = {
  getPredictions
};
