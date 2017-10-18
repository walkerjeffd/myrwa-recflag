<template>
  <div>
    <h2>Edit Existing Flag</h2>
    <b-row>
      <b-col>
        <b-alert :show="error" variant="danger">
          <h4 class="alert-heading">Error!</h4>
          <p>{{ error }}</p>
        </b-alert>
        <p v-if="loading">Loading...</p>
      </b-col>
    </b-row>
    <b-row v-if="!loading && !error">
      <b-col cols="6">
        <b-form @submit="onSubmit">
          <b-form-group id="labelLocation" label="Location:" label-for="inputLocation">
            <b-form-row>
              <b-col cols="6"><b-form-select id="inputLocation" :options="locations" required v-model="form.location_id"></b-form-select></b-col>
            </b-form-row>
          </b-form-group>
          <b-form-row>
            <b-col>
              <b-form-group id="labelStartDate" label="Start Date:" label-for="inputStartDate">
                <b-form-input id="inputStartDate" type="date" required v-model="form.start.date"></b-form-input>
              </b-form-group>
            </b-col>
            <b-col>
              <b-form-group id="labelStartTime" label="Start Time:" label-for="inputStartTime">
                <b-form-input id="inputStartTime" type="time" required v-model="form.start.time"></b-form-input>
              </b-form-group>
            </b-col>
          </b-form-row>
          <b-form-row>
            <b-col>
              <b-form-group id="labelEndDate" label="End Date:" label-for="inputEndDate">
                <b-form-input id="inputEndDate" type="date" required v-model="form.end.date"></b-form-input>
              </b-form-group>
            </b-col>
            <b-col>
              <b-form-group id="labelEndTime" label="End Time:" label-for="inputEndTime">
                <b-form-input id="inputEndTime" type="time" required v-model="form.end.time"></b-form-input>
              </b-form-group>
            </b-col>
          </b-form-row>
          <b-form-row>
            <b-col>
              <b-form-group id="labelType" label="Flag Type:" label-for="inputType">
                <b-form-select id="inputType" :options="types" required v-model="form.type"></b-form-select>
              </b-form-group>
            </b-col>
            <b-col>
              <b-form-group id="labelLevel" label="Flag Level:" label-for="inputLevel">
                <b-form-select id="inputLevel" :options="levels" required v-model="form.level"></b-form-select>
              </b-form-group>
            </b-col>
          </b-form-row>
          <b-form-group id="labelDescription" label="Description:" label-for="inputDescription">
            <b-form-input id="inputDescription" type="text" placeholder="Custom message that will appear on website" required v-model="form.description"></b-form-input>
          </b-form-group>
          <b-button type="submit" variant="primary">Submit</b-button>
          <b-button variant="danger" to="/list">Cancel</b-button>
        </b-form>
      </b-col>
    </b-row>
  </div>
</template>

<script>
import moment from 'moment';
import auth from '../auth';
import config from '../../../config';

export default {
  name: 'edit',
  data () {
    return {
      loading: true,
      form: {
        location_id: null,
        start: {
          date: '',
          time: ''
        },
        end: {
          date: '',
          time: ''
        },
        type: null,
        level: null,
        description: ''
      },
      types: [
        { value: 'CYANO', text: 'Cyanobacteria'},
        { value: 'CSO', text: 'Combined Sewer Overflow'},
        { value: 'OTHER', text: 'Other'}
      ],
      levels: [
        { value: 'UNCERTAIN', text: 'Uncertain'},
        { value: 'ADVISORY', text: 'Advisory'}
      ],
      locations: [
        { value: 'MYSTIC_ECOLI', text: 'Mystic River (Rt 16)' },
        { value: 'MALDENLOWER_ECOLI', text: 'Malden River (Rt 16)' },
        { value: 'SHANNON_ENT', text: 'Upper Mystic Lake (Shannon Beach)' }
      ]
    }
  },
  created () {
    const id = +this.$route.params.id;
    this.$http.get(`${config.api.url}/flags/${id}`)
      .then((response) => {
        const data = response.data.data;

        const start = moment(data.start_timestamp);
        data.start = {
          date: start.format('YYYY-MM-DD'),
          time: start.format('HH:mm')
        };
        const end = moment(data.end_timestamp);
        data.end = {
          date: end.format('YYYY-MM-DD'),
          time: end.format('HH:mm')
        };

        this.form = data;

        this.loading = false;
      })
      .catch((response) => {
        console.log(response);
        this.error = `Failed to get flag (id=${this.$route.params.id})`;
      })
  },
  methods: {
    onSubmit(evt) {
      evt.preventDefault();
      const id = +this.$route.params.id;

      const start_timestamp = moment(`${this.form.start.date} ${this.form.start.time}`).toISOString();
      const end_timestamp = moment(`${this.form.end.date} ${this.form.end.time}`).toISOString();

      const flag = {
        id: id,
        location_id: this.form.location_id,
        start_timestamp: start_timestamp,
        end_timestamp: end_timestamp,
        type: this.form.type,
        level: this.form.level,
        description: this.form.description
      };

      this.$http.post(`${config.api.url}/flags/${id}`, flag, {
          headers: auth.getAuthHeader()
        })
        .then((response) => {
          this.$router.push('/list');
        })
        .catch((response) => {
          if (response.status === 401) {
            this.error = 'Invalid login credentials, try logging out and then back in';
          } else {
            console.log(response);
            this.error = `Failed to update flag (id=${id}), see console`;
          }
        })
    }
  }
}
</script>

<style scoped>
</style>
