User = require("models/user")

App =
  init: ->
    template = require("views/users/show")
    view     = template(new User("Brian"))

    # Obviously this could be spruced up by jQuery
    element = document.createElement("div")
    element.innerHTML = view
    document.body.appendChild(element)

module.exports = App
