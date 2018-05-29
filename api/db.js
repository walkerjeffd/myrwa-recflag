const config = require('../config');

const knex = require('knex')({
  client: 'pg',
  connection: config.db,
});

module.exports = {
  getPredictions() {
    return knex('predictions')
      .select('*', knex.raw('to_char(date_trunc(\'day\', timestamp at time zone \'US/Eastern\'), \'Mon DD YYYY\') as day_label'))
      .orderBy('timestamp', 'desc')
      .whereRaw("date_trunc('day', timestamp at time zone 'US/Eastern') >= (date_trunc('day', now() at time zone 'US/Eastern') - interval '6 days')");
  },
  getFlags() {
    return knex('flags')
      .select(
        '*',
        knex.raw("CASE WHEN end_timestamp < now() THEN 'EXPIRED' WHEN start_timestamp > now() THEN 'PENDING' ELSE 'ACTIVE' END AS status")
      )
      .orderBy('end_timestamp', 'desc');
  },
  getFlag(id) {
    return knex('flags')
      .select()
      .where('id', id)
      .then(results => results[0]);
  },
  createFlag(data) {
    return knex('flags')
      .insert(data)
      .returning('*');
  },
  updateFlag(data) {
    return knex('flags')
      .where('id', data.id)
      .update({
        location_id: data.location_id,
        start_timestamp: data.start_timestamp,
        end_timestamp: data.end_timestamp,
        type: data.type,
        level: data.level,
        description: data.description
      })
      .returning('*');
  },
  deleteFlag(id) {
    return knex('flags')
      .where('id', id)
      .delete();
  }
};
