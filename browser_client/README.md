# Browser client notes

Trying out lots of new JS technologies here - coffee script, stitch,
backbone, zurb foundation...

For initial project set-up I'm following [The Little Book on Coffee Script, chapter 6](http://arcturo.github.io/library/coffeescript/06_applications.html).

You'll need npm, node and coffescript, e.g. (ubuntu 10.x)
```
sudo apt-get install npm
sudo apt-get install nodejs-legacy
sudo npm install -g coffee-script
```

Get dependencies:
```
npm install coffee-script stitch express eco
```

Build/run:
```
coffee index.coffee
```

Runs as server on [port 9294](http://127.0.0.1:9294) by default.

Export the assembled application.ks:

