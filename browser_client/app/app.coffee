User = require("models/user")
ListView = require 'backbonetest'

App =
  init: ->
    template = require("views/users/show")
    view     = template(new User("Brian"))

    # Obviously this could be spruced up by jQuery
    element = document.createElement("div")
    element.innerHTML = view
    document.body.appendChild(element)

    # backbonetest
    list_view = new ListView

module.exports = App
