const tmpl = require('blueimp-tmpl');
const L = require('leaflet');

L.Icon.Default.imagePath = 'dist/img/';

require('leaflet-providers');
require('leaflet-sidebar');

require('leaflet/dist/leaflet.css');
require('leaflet-sidebar/src/L.Control.Sidebar.css');

const config = require('../../config');

const sidebarTemplate = `
<h3>{%=o.description%}</h3>
<p>
  Risk Level: {%=o.risk%}<br>
  Reason: {%=o.reason%}<br>
  Recreation: {%=o.standard%}
</p>
<h4>Risk History</h4>
<img src="${config.api.url}static/map/img/risk-timeseries.png" width=400 height=50></img>
<p>Risk levels over the past 5 days</p>
<h4>Station Photo</h4>
<img src="${config.api.url}static/map/img/stn-image.png" width=400 height=250></img>
`;

const icons = {
  green: L.icon({
    iconUrl: `${config.api.url}static/map/img/marker-green-2x.png`,
    iconSize: [64, 64],    // size of the icon
    iconAnchor: [32, 64],  // point of the icon which will correspond to marker's location
    popupAnchor: [-3, -76] // point from which the popup should open relative to the iconAnchor
  }),
  yellow: L.icon({
    iconUrl: `${config.api.url}static/map/img/marker-yellow-2x.png`,
    iconSize: [64, 64],
    iconAnchor: [32, 64],
    popupAnchor: [-3, -76]
  }),
  red: L.icon({
    iconUrl: `${config.api.url}static/map/img/marker-red-2x.png`,
    iconSize: [64, 64],
    iconAnchor: [32, 64],
    popupAnchor: [-3, -76]
  }),
  gray: L.icon({
    iconUrl: `${config.api.url}static/map/img/marker-gray-2x.png`,
    iconSize: [64, 64],
    iconAnchor: [32, 64],
    popupAnchor: [-3, -76]
  })
};

const sites = [
  {
    id: 'MAR036',
    description: 'Malden River at Medford St Bridge',
    latitude: 42.4175,
    longitude: -71.073283,
    waterbody: 'Malden River',
    parameter: 'ECOLI',
    risk: 'Low',
    standard: 'Boating',
    reason: 'No expected bacteria exceedances',
    updated: ''
  }, {
    id: 'MWRA176',
    description: 'Malden River at Rt 16 Bridge',
    latitude: 42.4053,
    longitude: -71.07191,
    WaterBodyID: 'Malden River',
    parameter: 'ECOLI',
    risk: 'Low',
    standard: 'Boating',
    reason: 'No expected bacteria exceedances',
    updated: ''
  }, {
    id: 'MYR0435',
    description: 'Mystic River at Rt 16 Bridge',
    latitude: 42.405722,
    longitude: -71.096351,
    WaterBodyID: 'Mystic River (Fresh)',
    parameter: 'ECOLI',
    risk: 'High',
    standard: 'Boating',
    reason: 'High chance of bacteria levels above the boating standard',
    updated: ''
  }, {
    id: 'MYRBOBDOCK',
    description: 'Blessing of the Bay Boathouse',
    latitude: 42.3987,
    longitude: -71.090461,
    WaterBodyID: 'Mystic River (Fresh)',
    parameter: 'ECOLI',
    risk: 'High',
    standard: 'Boating',
    reason: 'High chance of bacteria levels above the boating standard',
    updated: ''
  }, {
    id: 'WEPBCHC',
    description: 'Wedge Pond Beach',
    latitude: 42.453883,
    longitude: -71.142985,
    WaterBodyID: 'Wedge Pond',
    parameter: 'ECOLI',
    risk: 'Medium',
    standard: 'Swimming',
    reason: 'Unconfirmed cyanobacteria bloom',
    updated: ''
  }, {
    id: 'UPLSHBM',
    description: 'Shannon Beach',
    latitude: 42.439892,
    longitude: -71.146153,
    WaterBodyID: 'Upper Mystic Lake',
    parameter: 'ECOLI',
    risk: 'Unknown',
    standard: 'Swimming',
    reason: 'Rainfall data not available',
    updated: ''
  }
];

window.onload = () => {
  const map = L.map('recflag-map').setView([42.42624, -71.09630], 13);

  L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
    attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
  }).addTo(map);

  // L.tileLayer.provider('OpenStreetMap.Mapnik').addTo(map);

  const sidebar = L.control.sidebar('recflag-sidebar', {
    closeButton: true,
    position: 'right',
    autoPan: false
  });
  map.addControl(sidebar);


  sites.forEach((site) => {
    let icon = icons.gray;
    switch (site.risk) {
      case 'Low':
        icon = icons.green;
        break;
      case 'Medium':
        icon = icons.yellow;
        break;
      case 'High':
        icon = icons.red;
        break;
      default:
        icon = icons.gray;
        break;
    }
    L.marker([site.latitude, site.longitude], { icon }).addTo(map)
      .on('click', () => {
        console.log('clicked: ', site);
        if (sidebar.isVisible()) {
          sidebar.hide();
        }

        setTimeout(() => {
          sidebar.setContent(tmpl(sidebarTemplate, site));
          sidebar.show();
        }, 200);
      });
  });
};
