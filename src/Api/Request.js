exports.googleAuth_ = () =>
  gapi.auth2
    .getAuthInstance()
    .signIn()
    .then((user) => ({
      success: true,
      token: user.getAuthResponse().id_token || null,
      image_url: user.getBasicProfile().getImageUrl() || null,
    }))
    .catch(() => ({ success: false, token: null, image_url: null }));

exports.initAuth_ = (clientId) => () => new Promise(res => {
  gapi.load('auth2', () => {
    gapi.auth2.init({
      client_id: `${clientId}.apps.googleusercontent.com`,
      cookiepolicy: 'single_host_origin',
    });

    res()
  })
})
