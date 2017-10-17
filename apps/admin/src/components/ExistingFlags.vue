<template>
  <div>
    <b-row>
      <b-col>
        <h3>Existing Flags</h3>
      </b-col>
    </b-row>
    <b-row>
      <b-col>
        <b-alert :show="error" variant="danger">
          <h4 class="alert-heading">Error!</h4>
          <p>{{ error }}</p>
        </b-alert>
        <p v-if="loading">Loading...</p>
      </b-col>
    </b-row>
    <b-row v-if="!loading">
      <b-col>
        <p v-if="flags.length === 0">
          No existing flags. Click <router-link to="/new">Create New Flag</router-link> to make one.
        </p>
        <div v-if="flags.length > 0">
          <b-table striped sortable :items="flags" :fields="fields">
            <template slot="edit" scope="data">
              <b-button variant="primary" :to="{ name: 'edit', params: { id: data.item.id } }">Edit</b-button>
              <b-button variant="danger" @click="deleteFlag(data.item.id)">Delete</b-button>
            </template>
          </b-table>
        </div>
      </b-col>
    </b-row>
  </div>
</template>

<script>
export default {
  name: 'existing',
  data () {
    return {
      loading: false,
      error: null,
      flags: [],
      fields: [
        {
          key: 'status',
          label: 'Status',
          sortable: true,
          variant: () => {
            return 'danger';
          }
        },
        {
          key: 'level',
          label: 'Level',
          sortable: true
        },
        {
          key: 'type',
          label: 'Flag Type',
          sortable: true
        },
        {
          key: 'start',
          label: 'Start',
          sortable: true
        },
        {
          key: 'end',
          label: 'End',
          sortable: true
        },
        {
          key: 'location_shortname',
          label: 'Location',
          sortable: true,
          class: 'rf-table-col'
        },
        {
          key: 'description',
          label: 'Description',
          sortable: false,
          class: 'rf-table-col'
        },
        {
          key: 'edit',
          label: ''
        }
      ]
    }
  },
  created () {
    this.fetchData();
  },
  methods: {
    fetchData () {
      this.loading = true;
      setTimeout(() => {
        const flags = [
          {
            id: 6,
            start: '2017-10-17 17:15',
            end: '2017-10-19 22:50',
            location_id: 'SHANNON_ENT',
            location_shortname: 'Shannon Beach',
            type: 'CSO',
            level: 'ADVISORY',
            description: 'CSO discharge',
            status: 'ACTIVE'
          },
          {
            id: 5,
            start: '2017-10-16 08:25',
            end: '2017-10-18 12:00',
            location_id: 'MALDENLOWER_ECOLI',
            location_shortname: 'Malden River (Rt 16)',
            type: 'CYANO',
            level: 'UNCERTAIN',
            description: 'Cyanobacteria unconfirmed and a very long message',
            status: 'ACTIVE'
          },
          {
            id: 4,
            start: '2017-10-15 12:15',
            end: '2017-10-15 18:25',
            location_id: 'MYSTIC_ECOLI',
            location_shortname: 'Mystic River (Rt 16)',
            type: 'CYANO',
            level: 'ADVISORY',
            description: 'Cyanobacteria',
            status: 'EXPIRED'
          },
          {
            id: 3,
            start: '2017-10-10 14:15',
            end: '2017-10-11 10:25',
            location_id: 'SHANNON_ENT',
            location_shortname: 'Shannon Beach',
            type: 'CYANO',
            level: 'UNCERTAIN',
            description: 'Cyanobacteria',
            status: 'EXPIRED'
          },
          {
            id: 2,
            start: '2017-10-7 12:15',
            end: '2017-10-10 18:25',
            location_id: 'MYSTIC_ECOLI',
            location_shortname: 'Mystic River (Rt 16)',
            type: 'CYANO',
            level: 'ADVISORY',
            description: 'Cyanobacteria',
            status: 'EXPIRED'
          },
          {
            id: 1,
            start: '2017-10-01 07:45',
            end: '2017-10-05 19:30',
            location_id: 'MALDENLOWER_ECOLI',
            location_shortname: 'Malden River (Rt 16)',
            type: 'CSO',
            level: 'ADVISORY',
            description: 'Major CSO Discharge',
            status: 'EXPIRED'
          }
        ];

        flags.forEach((d) => {
          if (d.status == 'ACTIVE') {
            d._rowVariant = 'danger';
          }
        });

        this.flags = flags;
        this.loading = false;
      }, 1000);
    },
    deleteFlag (id) {
      this.flags = this.flags.filter(d => d.id !== id);
    }
  }
}
</script>

<style>
.rf-table-col {
  max-width: 200px;
}
</style>
