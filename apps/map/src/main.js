const tmpl = require('blueimp-tmpl');
const L = require('leaflet');
const $ = require('jquery');

L.Icon.Default.imagePath = 'dist/img/';

require('leaflet-providers');
require('leaflet-sidebar');

require('leaflet/dist/leaflet.css');
require('leaflet-sidebar/src/L.Control.Sidebar.css');
require('./main.css');

const config = require('../../config');

// templates
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
      <th>Date</th>
      <th>Status</th>
    </tr>
  </thead>
  <tbody>
    {% for (var i=0; i<o.history.length; i++) { %}
      <tr>
      <td>{%=o.history[i].day_label%}</td>
      <td class="recflag-status-{%=o.history[i].status.type%}">{%=o.history[i].status.label%}</td>
    </tr>
    {% } %}
  </tbody>
</table>
`;

// map icons
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
  // set up map
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

  const sidebarCloseButton = sidebar.getCloseButton();

  // workaround for non-firing "click" events
  sidebarCloseButton.addEventListener('mousedown', () => {
    sidebar.hide();
  });

  // fetch data
  $.get(config.api.url + 'predictions/', (response) => {
    const data = response.data;

    // for each site
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
      L.marker([d.site.latitude, d.site.longitude], {
        icon
      }).addTo(map)
        .on('mousedown', () => {
          // workaround for non-firing "click" events
          if (sidebar.isVisible()) {
            sidebar.hide();
          }

          setTimeout(() => {
            sidebar.setContent(tmpl(sidebarTemplate, d));
            sidebar.show();
          }, 200);
        });
    });
  })
    .fail((err) => {
      console.log(err);
      // unable to get data from server
      const data = [
        {
          name: 'MYSTIC_ECOLI',
          site: {
            name: 'Mystic River',
            description: 'Mystic Valley Parkway (Rt 16)',
            latitude: 42.405722,
            longitude: -71.096351
          },
          current: {
            timestamp_local: 'Not Available',
            status: {
              type: 'unknown',
              label: 'Not Available',
              reason: 'Unable to get data from the server.'
            }
          },
          history: []
        },
        {
          name: 'MALDENLOWER_ECOLI',
          site: {
            name: 'Malden River',
            description: 'Revere Beach Parkway (Rt 16)',
            latitude: 42.4053,
            longitude: -71.07191
          },
          current: {
            timestamp_local: 'Not Available',
            status: {
              type: 'unknown',
              label: 'Not Available',
              reason: 'Unable to get data from the server.'
            }
          },
          history: []
        },
        {
          name: 'SHANNON_ENT',
          site: {
            name: 'Upper Mystic Lake',
            description: 'Shannon Beach',
            latitude: 42.439892,
            longitude: -71.146153
          },
          current: {
            timestamp_local: 'Not Available',
            status: {
              type: 'unknown',
              label: 'Not Available',
              reason: 'Unable to get data from the server.'
            }
          },
          history: []
        }
      ];

      data.forEach((d) => {
        // update cards
        $(`#recflag-card-${d.name}`).html(tmpl(cardTemplate, d));
      });

      setTimeout(() => {
        alert('System error occurred. Unable to retrieve current data. Please contact us at contact@mysticriver.org if the problem persists.');
      }, 500);
    });
};
