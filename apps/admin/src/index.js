import Vue from 'vue';
import BootstrapVue from 'bootstrap-vue';
import VueRouter from 'vue-router';
import VueResource from 'vue-resource';

import App from './components/App.vue';
import NewFlag from './components/NewFlag.vue';
import EditFlag from './components/EditFlag.vue';
import Existing from './components/ExistingFlags.vue';
import Login from './components/Login.vue';
import auth from './auth';
import config from '../../config';

require('./css/app.css');

Vue.use(VueRouter);
Vue.use(VueResource);
Vue.use(BootstrapVue);

const routes = [
  {
    path: '/',
    redirect: '/list'
  },
  {
    path: '/list',
    component: Existing,
    name: 'list',
    meta: { requireAuth: true }
  },
  {
    path: '/new',
    component: NewFlag,
    name: 'new',
    meta: { requireAuth: true }
  },
  {
    path: '/edit/:id',
    component: EditFlag,
    name: 'edit',
    meta: { requireAuth: true }
  },
  {
    path: '/login',
    component: Login,
    name: 'login'
  }
];

export const router = new VueRouter({
  routes
});


router.beforeEach((to, from, next) => {
  if (!to.meta.requireAuth) {
    console.log('no auth');
    next();
  } else if (!auth.user.initialized) {
    console.log('not initialized');
    auth.checkAuth(router.app)
      .then(() => next())
      .catch(() => next('/login'));
  } else if (!auth.user.authenticated) {
    console.log('initialized, not authenticated');
    next('/login');
  } else {
    console.log('initialized, authenticated');
    next();
  }
});

export const app = new Vue({
  el: '#app',
  router,
  render: h => h(App)
});
