# test example from http://adamjspooner.github.io/coffeescript-meet-backbonejs/01/docs/script.html

# need jQuery, Backbone and Underscore (jQuery ->)

module.exports = class ListView extends Backbone.View
  
  el: $ 'body'

  initialize: ->
    # alt. to => for methods used as event handlers
    _.bindAll @,'render'
    @render()

  render: ->
    $(@el).append '<ul><li>Hello, Backbone!</li></ul>'

#list_view = new ListView

