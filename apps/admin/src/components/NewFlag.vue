<template>
  <div>
    <h2>Create New Flag</h2>
    <b-row>
      <b-col cols="6">
        <b-form @submit="onSubmit">
          <b-form-group id="labelSite" label="Site:" label-for="inputSite">
            <b-form-select id="inputSite" :options="sites" required v-model="form.site"></b-form-select>
          </b-form-group>
          <b-form-group id="labelStart" label="Start Date/Time:" label-for="inputStartDate">
            <b-form-row>
              <b-col><b-form-input id="inputStartDate" type="date" required v-model="form.start.date"></b-form-input></b-col>
              <b-col><b-form-input id="inputStartTime" type="time" required v-model="form.start.time"></b-form-input></b-col>
            </b-form-row>
          </b-form-group>
          <b-form-group id="labelEnd" label="End Date/Time:" label-for="inputEndDate">
            <b-form-row>
              <b-col><b-form-input id="inputEndDate" type="date" required v-model="form.end.date"></b-form-input></b-col>
              <b-col><b-form-input id="inputEndTime" type="time" required v-model="form.end.time"></b-form-input></b-col>
            </b-form-row>
          </b-form-group>
          <b-form-group id="labelType" label="Flag Type:" label-for="inputType">
            <b-form-select id="inputType" :options="types" required v-model="form.type"></b-form-select>
          </b-form-group>
          <b-form-group id="labelDescription" label="Description:" label-for="inputDescription">
            <b-form-input id="inputDescription" type="text" required v-model="form.description"></b-form-input>
          </b-form-group>
          <b-button type="submit" variant="primary">Submit</b-button>
          <b-button type="reset" variant="secondary">Reset</b-button>
          <b-button variant="danger" to="/">Cancel</b-button>
        </b-form>
      </b-col>
    </b-row>
  </div>
</template>

<script>
export default {
  name: 'new',
  data () {
    return {
      // form: {
      //   site: null,
      //   start: {
      //     date: '',
      //     time: ''
      //   },
      //   end: {
      //     date: '',
      //     time: ''
      //   },
      //   type: null,
      //   description: ''
      // },
      form: {
        site: 'MYSTIC_ECOLI',
        start: {
          date: '2017-10-15',
          time: '00:00'
        },
        end: {
          date: '2017-10-17',
          time: '12:00'
        },
        type: 'CYANO_UNCERTAIN',
        description: 'Unconfirmed cyanobacteria'
      },
      types: [
        { value: 'CYANO_UNCERTAIN', text: 'Unconfirmed Cyanobacteria (Uncertain)'},
        { value: 'CYANO_ADVISORY', text: 'Confirmed Cyanobacteria (Advisory)'},
        { value: 'CSO_ADVISORY', text: 'Combined Sewer Overflow (Advisory)'},
        { value: 'OTHER_ADVISORY', text: 'Other (Advisory)'},
        { value: 'OTHER_UNCERTAIN', text: 'Other (Uncertain)'}
      ],
      sites: [
        { value: 'MYSTIC_ECOLI', text: 'Mystic River (Rt 16)' },
        { value: 'MALDENLOWER_ECOLI', text: 'Malden River (Rt 16)' },
        { value: 'SHANNON_ENT', text: 'Upper Mystic Lake (Shannon Beach)' }
      ]
    }
  },
  methods: {
    onSubmit(evt) {
      evt.preventDefault();
      console.log('submitted');
      console.log(this.form);
      this.$router.push('/');
    }
  }
}
</script>

<style scoped>
</style>
