<template>
  <div>
    <h2>Create New Flag</h2>
    <b-row>
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
