# mirage_server design/implementation notes

For version 1 the goal is to output:

- atom feed file containing selected items
- shorturls Json file with short urls for item helper pages
- cache versions of content files (e.g. SHA-1 hashes of content as name)

So a user has 

- an email address
- a password (or other means of authentication)
- a public name (title)
- some content sets...
- some website deployments...
- some device deployments...

`/user:EMAIL`

A content group (=> feed) has

- an internal name (ID)
- a title
- a version / subtitle
- a last modified date/time
- some content items...

`/user/group:EMAIL,GROUPNAME`

A content item (=> entry) has

- a file url 
- a title
- a summary
- an icon URL
- a file mime type
- a browser visibility, e.g. hidden or not
- a (possibly empty) list of required device (types)
- a (possibly empty) list of mime types supported
- a (general) visibility, e.g. hidden or public
- a last modified date/time

`/user/group/item:USERNAME,GROUPID,ITEMID`
`/user/group/nextitemid:USERNAME,GROUPID`

A website deployment has

- a base url
- a set of groups (IDs)
- a set of short url mappings

A device deployment has

- a device id?
- a set of groups (IDs)

## Client API

in v1 I'll assume it gets userid from session or basic authentication, i.e. via headers. So top-level will be groups.

```
curl ...
  --basic
  --data DATA or --data-ascii DATA - POST
  --header LINE
  --request COMMAND
  --user USER:PASS
  --verbose
'''

'''
curl -v --request GET --user admin:password http://127.0.0.1:8080/api/group/

curl -v --request GET --user admin:password http://127.0.0.1:8080/api/group/testgp1

curl -v --request POST --user admin:password \
 --data-ascii '{"gid":"testgp1", "gtitle":"Test Group 1", "gver":"1", "gdate":"2013-11-07T21:31:00Z"}' \
 http://127.0.0.1:8080/api/group/

curl -v --request GET --user admin:password http://127.0.0.1:8080/api/group/testgp1

curl -v --request PUT --user admin:password \
 --data-ascii '{"gid":"testgp1", "gtitle":"Test Group 1b", "gver":"2", "gdate":"2013-11-07T21:33:00Z"}' \
 http://127.0.0.1:8080/api/group/testgp1

curl -v --request PUT --user admin:password \
 --data-ascii '{"gid":"testgp1", "gtitle":"Test Group 1b", "gver":"3"}' \
 http://127.0.0.1:8080/api/group/testgp1
ERROR:
curl -v --request PUT --user admin:password \
 --data-ascii '{"gid":"testgp11", "gtitle":"Test Group 1b", "gver":"3"}' \
 http://127.0.0.1:8080/api/group/testgp1
ERROR:
curl -v --request PUT --user admin:password \
 --data-ascii '{"gid":"testgp1", "gtitle":"Test Group 1b", "gver":"3", "gtestimmutable":"123"}' \
 http://127.0.0.1:8080/api/group/testgp1

curl -v --request GET --user admin:password http://127.0.0.1:8080/api/group/testgp1

curl -v --request DELETE --user admin:password http://127.0.0.1:8080/api/group/testgp1

curl -v --request GET --user admin:password http://127.0.0.1:8080/api/group/testgp1
```

