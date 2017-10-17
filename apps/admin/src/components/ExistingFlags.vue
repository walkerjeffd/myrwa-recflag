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
          <p><em>Note: Click on a column header to sort the table</em></p>
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
          key: 'site',
          label: 'Site',
          sortable: true,
          formatter (x) {
            const sites = {
              MYSTIC_ECOLI: 'Mystic River (Rt 16)',
              MALDENLOWER_ECOLI: 'Malden River (Rt 16)',
              SHANNON_ENT: 'Upper Mystic Lake (Shannon Beach)'
            }
            return sites[x];
          },
          class: 'rf-table-col'
        },
        {
          key: 'type',
          label: 'Flag Type',
          sortable: true,
          formatter (x) {
            const types = {
              CYANO_UNCERTAIN: 'Unconfirmed Cyanobacteria (Uncertain)',
              CYANO_ADVISORY: 'Confirmed Cyanobacteria (Advisory)',
              CSO_ADVISORY: 'Combined Sewer Overflow (Advisory)',
              OTHER_ADVISORY: 'Other (Advisory)',
              OTHER_UNCERTAIN: 'Other (Uncertain)'
            }
            console.log(x);
            return types[x];
          },
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
          { id: 1, start: '2017-10-15 12:15', end: '2017-10-17 18:25', 'site': 'MYSTIC_ECOLI', 'type': 'CYANO_ADVISORY', 'description': 'Cyanobacteria' },
          { id: 2, start: '2017-10-16 08:25', end: '2017-10-18 12:00', 'site': 'MALDENLOWER_ECOLI', 'type': 'CYANO_UNCERTAIN', 'description': 'Cyanobacteria unconfirmed and a very long message' },
          { id: 3, start: '2017-10-17 17:15', end: '2017-10-19 22:50', 'site': 'SHANNON_ENT', 'type': 'CSO_ADVISORY', 'description': 'CSO discharge' }
        ];
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
