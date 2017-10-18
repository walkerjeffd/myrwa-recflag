import { router } from '../index';
import * as config from '../../../config';

const LOGIN_URL = `${config.api.url}/auth/login`;
const CHECK_URL = `${config.api.url}/auth/check`;

export default {
  user: {
    initialized: false,
    authenticated: false
  },

  login(context, creds, redirect) {
    context.$http.post(LOGIN_URL, creds)
      .then((response) => {
        localStorage.setItem('recflag_token', response.data.data.access_token);

        this.user.authenticated = true;

        if (redirect) {
          router.push(redirect);
        }
      })
      .catch((response) => {
        context.error = response.data.error.message;
      });
  },

  logout(redirect) {
    localStorage.removeItem('recflag_token');
    this.user.authenticated = false;

    if (redirect) {
      router.push(redirect);
    }
  },

  checkAuth(context) {
    const jwt = localStorage.getItem('recflag_token');

    if (!jwt) {
      this.logout();
      return Promise.reject('No token');
    }

    return context.$http.post(CHECK_URL, { access_token: jwt })
      .then((response) => {
        this.user.initialized = true;
        if (response.status === 200) {
          this.user.authenticated = true;
        } else {
          this.logout();
        }
      })
      .catch(() => {
        this.user.initialized = true;
        this.logout();
        throw new Error('Unauthorized');
      });
  },

  getAuthHeader() {
    return {
      Authorization: `Bearer ${localStorage.getItem('recflag_token')}`
    };
  }
};
