const Twitter = require('twitter');
const _ = require('lodash');

const config = require('../config');
const utils = require('../utils');

const knex = require('knex')({
  client: 'pg',
  connection: config.db,
});

const client = new Twitter({
  consumer_key: config.twitter.consumerKey,
  consumer_secret: config.twitter.consumerSecret,
  access_token_key: config.twitter.accessKey,
  access_token_secret: config.twitter.accessSecret
});

function sendTweet(text) {
  return client.post('statuses/update', { status: text });
}

// add row number by name
const cte = knex('predictions')
  .select(
    knex.raw('*, (ROW_NUMBER() OVER (PARTITION BY name ORDER BY timestamp DESC))::int AS rn')
  );

const query = knex.raw('with t1 as (?) ?', [
  cte,
  knex
    .select('*')
    .from('t1')
    .where('rn', '=', 1)
]);

query
  .then((results) => {
    const names = _.sortedUniq(results.rows.map(d => d.name));

    const data = names.map((name) => {
      return {
        name,
        site: utils.sites[name],
        prediction: results.rows.filter(d => d.name === name)[0]
      };
    });

    data.forEach((d) => {
      d.status = utils.assignStatus(d.prediction);
    });

    let text = '';
    data.forEach((d) => {
      text += d.site.tweet_name + ': ' + d.status.label;
      text += '\n';
    });

    text += '\nMore Info: https://mysticriver.org/boatingadvisory';

    return text;
  })
  .then(sendTweet)
  .then((tweet) => {
    console.log('done');
    console.log(tweet.text.length);
    process.exit(0);
  })
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
