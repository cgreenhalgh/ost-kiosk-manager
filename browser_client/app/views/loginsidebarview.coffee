# login sidebar view - show current user status & logout if applicable

module.exports = class LoginSidebarView extends Backbone.View

  initialize: ->
    @model.bind 'change', @render

  render: =>
    $(@el).html """
      <p>#{@model.get 'email'}</p>
      <!-- Authenticated: #{@model.get 'authenticated'}
           Authenticating: #{@model.get 'authenticating'} -->
      <button class="small secondary logout">Logout</button>
    """
    $('.logout',@el).toggleClass('disabled', not @model.get 'authenticated')
    @

  logout: => 
    # 'real' logout?
    router.navigate 'login',{trigger:true}

  events:
      #'click .swap': 'swap'
      'click .logout': 'logout'
