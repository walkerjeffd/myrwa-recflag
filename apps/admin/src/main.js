import Vue from 'vue';
import BootstrapVue from 'bootstrap-vue';
import VueRouter from 'vue-router';
import VueResource from 'vue-resource';

require('./css/app.css');

Vue.use(VueRouter);
Vue.use(VueResource);
Vue.use(BootstrapVue);

import App from './components/App.vue';
import NewFlag from './components/NewFlag.vue';
import EditFlag from './components/EditFlag.vue';
import Existing from './components/ExistingFlags.vue';

const routes = [
  { path: '/', component: Existing, name: 'home' },
  { path: '/new', component: NewFlag, name: 'new' },
  { path: '/edit/:id', component: EditFlag, name: 'edit' }
];

const router = new VueRouter({
  routes
});

const app = new Vue({
  el: '#app',
  router,
  http: {
    // root: '/',
    // headers: {
    //   Authorization: 'Basic YXBpOnBhc3N3b3Jk'
    // }
  },
  render: h => h(App)
});
