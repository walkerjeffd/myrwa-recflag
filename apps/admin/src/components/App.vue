<template>
  <div id="app">
    <b-container fluid>
      <b-row>
        <b-col>
          <h1 class="display-4">MyRWA Recreational Flagging Project</h1>
          <h3 class="text-muted">Manual Advisory Override Control Center</h3>
        </b-col>
      </b-row>
      <b-row v-if="user.authenticated" class="mt-3">
        <b-col>
          <b-button to="/list">Existing Flags</b-button>
          <b-button to="/new" variant="success">Create New Flag</b-button>
          <b-button @click="logout" variant="info">Log Out</b-button>
        </b-col>
      </b-row>
      <b-row class="mt-4">
        <b-col>
          <router-view></router-view>
        </b-col>
      </b-row>
    </b-container>
  </div>
</template>

<script>
import auth from '../auth';

export default {
  name: 'app',
  data() {
    return {
      user: auth.user
    };
  },
  methods: {
    logout() {
      auth.logout();
      this.$router.push('/login');
    }
  }
}
</script>

<style scoped>
</style>
