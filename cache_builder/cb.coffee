# coffee-script version of cache builder
https = require 'https'
fs = require 'fs'
xml2js = require 'xml2js'

if process.argv.length<3 
  console.log 'usage: coffee cb.coffee <KIOSK-ATOM-FILE>'
  process.exit -1

atomfn = process.argv[2]


parser = new xml2js.Parser()

# is file hidden
is_hidden = (entry) ->
  #console.dir entry
  for cs in entry.category ? [] when cs.$.scheme == 'visibility' and cs.$.term == 'hidden'
    return true
  return false

# read main atom file 
console.log 'read '+atomfn
data = fs.readFileSync atomfn, 'utf8'
console.log 'read '+data.length+' bytes'

# try reading shorturls.js
shorturls = []
shorturlsfn = 'shorturls.js'
try 
  shorturlsdata = fs.readFileSync shorturlsfn,'utf8'
  shorturls = JSON.parse shorturlsdata
catch e
  console.log 'could not read '+shorturlsfn+': '+e

# try reading cache.js
cache = {}
cachefn = 'cache.js'
try 
  cachedata = fs.readFileSync cachefn,'utf8'
  cache = JSON.parse cachedata
catch e
  console.log 'could not read '+cachefn+': '+e

# make one shorturl
add_shorturl = (shorturls,url) ->
  #console.log 'shorturl for '+url
  sus = for su in shorturls when su.url == url
    su.shorturl ? ''

  if sus.length == 0
    # work out shorturl later
    shorturls.push { url: url } 

get_baseurl = (feed) ->
  baseurl = for link in feed.link when link.$.rel == 'self'
    link.$.href
  if baseurl.length < 1
    console.log 'No self link found - cannot work out shorturls'
    process.exit -1
  baseurl = baseurl[0]
  ix = baseurl.lastIndexOf '/'
  baseurl = baseurl.slice 0,ix+1
  console.log 'Base URL = '+baseurl
  return baseurl

# shorturl for each 
make_shorturls = (feed,shorturls) ->
  console.log 'make_shorturls...'
  # TODO

  # atom url -> get url
  baseurl = get_baseurl feed
  geturl = baseurl+'get.html'

  # work out URLs to be shortened
  # get.html?u=URL&t=TITLE&a=HELPERURL...
  # each entry...
  for entry in feed.entry when not is_hidden entry
    title = entry.title[0]
    # each enclosure    
    for link in entry.link when link.$.rel == 'enclosure'
      fileurl = link.$.href
      mime = link.$.type

      url = geturl+'?u='+encodeURIComponent(fileurl)+'&t='+encodeURIComponent(title) 

      add_shorturl shorturls,url

      # each helper app
      for appentry in feed.entry 
        supports = for cat in appentry.category ? [] when cat.$.scheme == 'supports-mime-type' and cat.$.term == mime
          cat.$.label ? cat.$.term 
        if supports?.length > 0
          appurls = for link in appentry.link ? [] when link.$.rel == 'enclosure'
            link.$.href
          if appurls?.length > 0
            add_shorturl shorturls,url+'&a='+encodeURIComponent(appurls[0])

# cache entry for each
make_cache = (feed,cache) ->

  fileurls = []
  for entry in feed.entry 
    hidden = is_hidden entry
    for link in entry.link when link.$.href? and (link.$.rel == 'alternate' or not is_hidden)
      url = link.$.href
      # not sure why indexOf doesn't seem to match it
      us = for u in fileurls when u == url
        u
      if us.length == 0
        #console.log 'add '+url+' to '+fileurls
        fileurls.push url 
  #console.dir fileurls
 
  oldfiles = cache.files ? []
  cache.files = []

  for file in oldfiles
    file.needed = false
    # does the file exist?
    if file.path?
      fileok = false
      try 
        st = fs.statSync file.path
        if st.isFile() 
          fileok = true
        else
          console.log 'old cache path not file: '+file.path
      catch e
        console.log 'old cache file not found: '+file.path
      if not fileok
        # doesn't exist, presumably
        delete file.path
        delete file.size
        delete file.mtime

  for fileurl in fileurls
    file = { url: fileurl }  
    oldfile = for file in oldfiles when file.url == fileurl
      file
    if oldfile.length > 0
      file = oldfile[0]
    
    file.needed = true
    cache.files.push file

  # un-needed files in cache?
  for oldfile in oldfiles when oldfile.path? and not oldfile.needed 
    cache.files.push oldfile

# parse file(s) and call worker(s)
parser.parseString data,(err,result) ->
  if err 
    console.log 'Error parsing '+atomfn+': '+err
    process.exit -1

  feed = result.feed
  console.log 'Feed '+feed.title+' ('+feed.id+')'

  make_shorturls feed,shorturls

  fix_shorturls = (shorturls,i) ->
    if i >= shorturls.length
      # done
      console.log 'write shorturls.js'
      fs.writeFileSync shorturlsfn,JSON.stringify shorturls
    else
      su = shorturls[i]
      if su.shorturl == undefined
        # see https://developers.google.com/url-shortener/v1/getting_started
        # POST application/json {"longUrl":"XXX"} ->
        #   https://www.googleapis.com/urlshortener/v1/url
        # { "kind": "urlshortener#url", "id": "http://goo.gl/XXXX",
        #  "longUrl": "XXX" }
        console.log 'shorten '+su.url
        req = {longUrl: su.url}
        reqs = JSON.stringify req
        options =
          hostname: 'www.googleapis.com'
          path: '/urlshortener/v1/url'
          method: 'POST'
          headers: { 'content-type': 'application/json' }

        hreq = https.request options, (res) ->
          if res.statusCode != 200
            console.log 'got shortener response '+res.statusCode
            process.exit -1

          res.setEncoding 'utf8'
          res.on 'data',(chunk) ->
            console.log 'shortener response: '+chunk
            jres = JSON.parse chunk
            su.shorturl = jres.id
            # recurse
            fix_shorturls shorturls,i+1

        hreq.on 'error',(e) ->
          console.log 'Error shortening url: '+e
          process.exit -1

        hreq.end reqs  
        console.log 'sent '+reqs
  
  fix_shorturls shorturls,0

  # download icons and visible enclosures, populating cache.json
  baseurl = get_baseurl feed
  cache.baseurl = baseurl

  make_cache feed,cache

  # TODO
  # if the local file exists and we have size and server last-modified,
  #   try a head on the remote file with if-modified-since;
  #   (prepare to) dump local copy if out of date
  #
  # new local path = url mapped to folder hierarchy - domain name
  #   in reverse order (ip forwards), port, path elements, 
  #   final filename+fragment+query
  #
  # if updated or missing attempt download, initially to temp file
  #   and stash header last-modified and content-length  
  # 
  # on success remove old file if present and link/rename new file

  console.log 'write cache.js'
  fs.writeFileSync cachefn,JSON.stringify cache


# TODO download icons and non-hidden files

#req = http.get "http://www.google.com/index.html", (res) ->
#  console.log "Got response: " + res.statusCode
#req.on 'error', (e) ->
#  console.log "Got error: " + e.message

