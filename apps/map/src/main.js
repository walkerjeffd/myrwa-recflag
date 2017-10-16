const tmpl = require('blueimp-tmpl');
const L = require('leaflet');
const $ = require('jquery');

L.Icon.Default.imagePath = 'dist/img/';

require('leaflet-providers');
require('leaflet-sidebar');

require('leaflet/dist/leaflet.css');
require('leaflet-sidebar/src/L.Control.Sidebar.css');

const config = require('../../config');

const siteInfo = {
  MYSTIC_ECOLI: {
    name: 'Mystic River',
    description: 'Mystic Valley Parkway (Rt 16)'
  },
  MALDENLOWER_ECOLI: {
    name: 'Malden River',
    description: 'Revere Beach Parkway (Rt 16)'
  },
  SHANNON_ENT: {
    name: 'Upper Mystic Lake',
    description: 'Shannon Beach'
  }
};

const cardTemplate = `
<div class="recflag-card-status recflag-status-{%=o.current.status.type%}">
  <div class="recflag-card-status-title">Status: <span class="recflag-card-status-title-type">{%=o.current.status.label%}</span></div>
  <div class="recflag-card-status-reason">{%=o.current.status.reason%}</div>
</div>
`;

const sidebarTemplate = `
<h2>{%=o.site.name%}</h2>
<h3 style="margin-top:0">{%=o.site.description%}</h3>

<p><strong>Status</strong>: <strong class="recflag-status-{%=o.current.status.type%}" style="font-size:1.4em;padding:4px">{%=o.current.status.label%}</strong></p>
<p><strong>Reason</strong>: {%=o.current.status.reason%}</p>
<p><strong>Last Updated</strong>: {%=o.current.timestamp_local%}</p>

<h4>Past 7 Days</h4>
<table class="pure-table">
  <thead>
    <tr>
      <th>Date/Time</th>
      <th>Status</th>
    </tr>
  </thead>
  <tbody>
    {% for (var i=0; i<o.history.length; i++) { %}
      <tr>
      <td>{%=o.history[i].timestamp_local%}</td>
      <td class="recflag-status-{%=o.history[i].status.type%}">{%=o.history[i].status.label%}</td>
    </tr>
    {% } %}
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

  $.get(config.api.url + 'predictions/', (response) => {
    const data = response.data;
    console.log(data);

    data.forEach((d) => {
      // update cards
      $(`#recflag-card-${d.name}`).html(tmpl(cardTemplate, d));

      // add map markers
      let icon = icons.gray;
      switch (d.current.status.type) {
        case 'good':
          icon = icons.green;
          break;
        case 'uncertain':
          icon = icons.yellow;
          break;
        case 'advisory':
          icon = icons.red;
          break;
        default:
          icon = icons.gray;
          break;
      }
      L.marker([d.site.latitude, d.site.longitude], { icon }).addTo(map)
        .on('click', () => {
          console.log('clicked: ', d.id);
          if (sidebar.isVisible()) {
            sidebar.hide();
          }

          setTimeout(() => {
            sidebar.setContent(tmpl(sidebarTemplate, d));
            sidebar.show();
          }, 200);
        });
    });
  });
};
