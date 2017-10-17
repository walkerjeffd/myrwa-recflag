<template>
  <div id="app">
    <b-container>
      <b-row>
        <b-col>
          <h1>MyRWA Recreational Flagging Admin Page</h1>
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
  name: 'app',
  data () {
    return {
    }
  },
  methods: {
    clickBtn () {
      console.log('clicked')
    }
  }
}
</script>

<style scoped>
</style>
