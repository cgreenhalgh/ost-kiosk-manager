# login form view - actually the form is already made; just handle some feedback...

module.exports = class LoginFormView extends Backbone.View

  initialize: ->
    @model.bind 'change', @render

  render: =>
    $('input[name="password"]',@el).val('')
    @

  login: =>
    email = $('input[name="email"]',@el).val()
    password = $('input[name="password"]',@el).val()
    console.log "login #{email}..."

    # TODO real login
    @model.set 'authenticating',true
    @model.set 'email',email
    @model.set 'password',password
    @model.set 'authenticated',true
    @model.set 'authenticating',false

    router.navigate("groups", {trigger:true})


  events: 'submit': 'login'
  
#click input[type="submit"]
