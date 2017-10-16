const tmpl = require('blueimp-tmpl');
const L = require('leaflet');

L.Icon.Default.imagePath = 'dist/img/';

require('leaflet-providers');
require('leaflet-sidebar');

require('leaflet/dist/leaflet.css');
require('leaflet-sidebar/src/L.Control.Sidebar.css');

const config = require('../../config');

const sidebarTemplate = `
<h2>{%=o.name%}</h2>
<h3 style="margin-top:0">{%=o.description%}</h3>

<p><strong>Status</strong>: <strong class="{%=o.status_class%}" style="font-size:1.4em;padding:4px">{%=o.status%}</strong></p>
<p><strong>Reason</strong>: {%=o.reason%}</p>
<p><strong>Last Updated</strong>: {%=o.timestamp%}</p>

<h4>Past 7 Days</h4>
<table class="pure-table">
  <thead>
    <tr>
      <th>Date/Time</th>
      <th>Status</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>10/16/2017 07:00 EDT</td>
      <td class="recflag-status-good">Good</td>
    </tr>
    <tr>
      <td>10/15/2017 07:00 EDT</td>
      <td class="recflag-status-good">Good</td>
    </tr>
    <tr>
      <td>10/14/2017 07:00 EDT</td>
      <td class="recflag-status-advisory">Advisory</td>
    </tr>
    <tr>
      <td>10/13/2017 07:00 EDT</td>
      <td class="recflag-status-advisory">Advisory</td>
    </tr>
    <tr>
      <td>10/12/2017 07:00 EDT</td>
      <td class="recflag-status-uncertain">Uncertain</td>
    </tr>
    <tr>
      <td>10/11/2017 07:00 EDT</td>
      <td class="recflag-status-unknown">Not Available</td>
    </tr>
    <tr>
      <td>10/10/2017 07:00 EDT</td>
      <td class="recflag-status-good">Good</td>
    </tr>
  </tbody>
</table>
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
    id: 'MALDENLOWER_ECOLI',
    name: 'Malden River',
    description: 'Revere Beach Parkway (Rt 16)',
    latitude: 42.4053,
    longitude: -71.07191,
    WaterBodyID: 'Malden River',
    parameter: 'ECOLI',
    status: 'Good',
    status_class: 'recflag-status-good',
    standard: 'Boating',
    reason: 'No expected bacteria exceedances',
    timestamp: 'October 16, 2017 07:00 (EDT)'
  }, {
    id: 'MYSTIC_ECOLI',
    name: 'Mystic River',
    description: 'Mystic Valley Parkway (Rt 16)',
    latitude: 42.405722,
    longitude: -71.096351,
    WaterBodyID: 'Mystic River (Fresh)',
    parameter: 'ECOLI',
    status: 'Advisory',
    status_class: 'recflag-status-advisory',
    standard: 'Boating',
    reason: 'High chance of bacteria levels above the boating standard',
    timestamp: 'October 16, 2017 07:00 (EDT)'
  }, {
    id: 'SHANNON_ENT',
    name: 'Upper Mystic Lake',
    description: 'Shannon Beach',
    latitude: 42.439892,
    longitude: -71.146153,
    WaterBodyID: 'Upper Mystic Lake',
    parameter: 'ECOLI',
    status: 'Uncertain',
    status_class: 'recflag-status-uncertain',
    standard: 'Swimming',
    reason: 'Possible cyanobacteria bloom (unconfirmed)',
    timestamp: 'October 16, 2017 07:00 (EDT)'
  }
];

window.onload = () => {
  const map = L.map('recflag-map').setView([42.42624, -71.09630], 12);

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
    switch (site.status) {
      case 'Good':
        icon = icons.green;
        break;
      case 'Uncertain':
        icon = icons.yellow;
        break;
      case 'Advisory':
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
