<template>
  <div>
    <b-row>
      <b-col>
        <h3>Existing Flags</h3>
      </b-col>
    </b-row>
    <b-row>
      <b-col>
        <b-alert :show="!!error" variant="danger">
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
          <b-table sortable :items="flags" :fields="fields">
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
import moment from 'moment';
import auth from '../auth';
import fixtures from '../fixtures';
import config from '../../../config';

export default {
  name: 'existing',
  data () {
    return {
      loading: true,
      error: '',
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
          key: 'start_timestamp',
          label: 'Start',
          sortable: true,
          formatter: t => moment(t).format('MMM D YYYY, h:mm a')
        },
        {
          key: 'end_timestamp',
          label: 'End',
          sortable: true,
          formatter: t => moment(t).format('MMM D YYYY, h:mm a')
        },
        {
          key: 'location_id',
          label: 'Location',
          sortable: true,
          class: 'rf-table-col',
          formatter: id => fixtures.locations[id] || 'Unknown'
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
      this.$http.get(`${config.api.url}/flags/`)
        .then((response) => {
          const flags = response.data.data;

          flags.forEach((d) => {
            if (d.status == 'ACTIVE') {
              d._rowVariant = 'danger';
            } else if (d.status == 'PENDING') {
              d._rowVariant = 'info';
            }
          });

          this.flags = flags;
          this.loading = false;
        })
        .catch((response) => {
          console.log(response);
          this.error = 'Failed to get existing flags, see console';
          this.loading = false;
        })
    },
    deleteFlag (id) {
      this.$http.delete(`${config.api.url}/flags/${id}`, {
          headers: auth.getAuthHeader()
        })
        .then((response) => {
          this.flags = this.flags.filter(d => d.id !== id);
        })
        .catch((response) => {
          if (response.status === 401) {
            this.error = 'Invalid login credentials, try logging out and then back in';
          } else {
            console.log(response);
            this.error = `Failed to delete flag (id=${id}), see console`;
          }
        })
    }
  }
}
</script>

<style>
.rf-table-col {
  max-width: 200px;
}
</style>
