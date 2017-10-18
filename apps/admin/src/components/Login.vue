<template>
  <div>
    <h2>Log In</h2>
    <b-row>
      <b-col cols="6">
        <b-form @submit="submit">
          <b-form-group id="labelPassword" label="Password:" label-for="inputPassword">
            <b-form-row>
              <b-col cols="6">
                <b-form-input id="inputPassword" type="password" required v-model="form.password"></b-form-input>
              </b-col>
            </b-form-row>
          </b-form-group>
          <b-button type="submit" variant="primary">Submit</b-button>
        </b-form>
      </b-col>
    </b-row>
    <b-row>
      <b-col cols="3">
        <b-alert :show="!!error" variant="danger" class="mt-4">
          <h4 class="alert-heading">Error!</h4>
          <p>{{ error }}</p>
        </b-alert>
      </b-col>
    </b-row>
    <b-row class="mt-4">
      <b-col>
        <p><em>For testing, password: myrwa</em></p>
      </b-col>
    </b-row>
  </div>
</template>

<script>
import auth from '../auth';

export default {
  name: 'login',
  data () {
    return {
      form: {
        password: '',
      },
      error: ''
    }
  },
  methods: {
    submit(evt) {
      evt.preventDefault();
      const credentials = {
        username: 'myrwa',
        password: this.form.password
      }
      auth.login(this, credentials, 'list');
    }
  }
}
</script>

<style scoped>
</style>
