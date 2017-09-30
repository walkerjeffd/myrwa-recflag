module.exports = {
  db: {
    host: '',
    port: 5432,
    database: '',
    user: '',
    password: ''
  },
  darkSky: {
    key: '',
    location: {
      // Medford, MA
      latitude: 42.418333,
      longitude: -71.106667
    }
  },
  wunderground: {
    key: '',
    history: {
      start: '2010-01-01',
      end: '2016-12-31',
      delay: 3 * 60 * 10000 // 3 min
    }
  },
  api: {
    port: 8000,
    logFile: '/path/to/api-access.log'
  }
};
