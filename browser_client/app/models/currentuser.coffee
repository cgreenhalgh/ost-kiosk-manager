# Current user (in browser) 
module.exports = class CurrentUser extends Backbone.Model
  defaults:
    email: ''
    password: ''
    authenticated: false
    authenticating: false

  # may need to override sync(method, model, [options])
