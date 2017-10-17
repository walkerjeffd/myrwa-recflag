<template>
  <div id="app">
    <b-container fluid>
      <b-row>
        <b-col>
          <h1 class="display-4">MyRWA Recreational Flagging Project</h1>
          <h3 class="text-muted">Manual Advisory Override Control Center</h3>
        </b-col>
      </b-row>
      <hr>
      <b-row>
        <b-col>
          <b-button to="/">Existing Flags</b-button>
          <b-button variant="success" to="/new">Create New Flag</b-button>
        </b-col>
      </b-row>
      <hr>
      <b-row>
        <b-col>
          <router-view></router-view>
        </b-col>
      </b-row>
    </b-container>
  </div>
</template>

<script>
export default {
  name: 'app'
}
</script>

<style scoped>
</style>
