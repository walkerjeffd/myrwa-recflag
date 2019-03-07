const _ = require('lodash');

function assignStatus(prediction, flags) {
  let type = 'unknown';
  let label = 'Not Available';
  let reason = 'Data unavailable or system error';

  if (flags && flags.length > 0) {
    const flagsOffline = flags.filter(f => f.level === 'OFFLINE');
    const flagsAdvisory = flags.filter(f => f.level === 'ADVISORY');
    const flagsUncertain = flags.filter(f => f.level === 'UNCERTAIN');

    if (flagsAdvisory.length > 0) {
      type = 'advisory';
      label = 'Advisory';
    } else if (flagsUncertain.length > 0) {
      type = 'uncertain';
      label = 'Uncertain';
    }

    reason = flags.map(d => d.description).join(' ');

    if (prediction && prediction.exceedance && flagsOffline.length === 0) {
      type = 'advisory';
      label = 'Advisory';
      reason = `High probability of elevated bacteria levels. ${reason}`;
    }
  } else if (_.isNull(prediction.exceedance) || _.isUndefined(prediction.exceedance)) {
    type = 'unknown';
    label = 'Not Available';
    reason = 'Data unavailable or system error.';
  } else if (prediction.exceedance) {
    type = 'advisory';
    label = 'Advisory';
    reason = 'High probability of elevated bacteria levels.';
  } else if (!flags || flags.length === 0) {
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
    longitude: -71.096351,
    tweet_name: 'Mystic River'
  },
  MALDENLOWER_ECOLI: {
    name: 'Malden River',
    description: 'Revere Beach Parkway (Rt 16)',
    latitude: 42.4053,
    longitude: -71.07191,
    tweet_name: 'Malden River'
  },
  SHANNON_ENT: {
    name: 'Upper Mystic Lake',
    description: 'Shannon Beach',
    latitude: 42.439892,
    longitude: -71.146153,
    tweet_name: 'Upper Mystic Lake @ Shannon Beach'
  }
};

module.exports = {
  assignStatus,
  sites
};
