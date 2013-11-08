ost-kiosk-manager
=================

OpenSharingToolkit web-based manager/configuration tool for Kiosks.

The server is a [Mirage](http://openmirage.org) web application. To build it you need to install [OPAM](http://opam.ocaml.org) and, as a minumum, Mirari, the Mirage configuration/build tool:

```
opam install mirari
```
You will need to ensure the necessary opam packages are in your opam repository(s); as of 2013-10-28 you would need to add my dev repository to get `unix-simple-blkdev`, `mirage-baardskeerder` and `ocaml-btree`:
```
opam repo add dev https://github.com/cgreenhalgh/opam-dev-repository.git
```

Mirage will build applications to run under Unix or directly on the [Xen](http://www.xenproject.org) Hypervisor. To build for unix try:
```
cd browser_client
coffee build.coffee
cd mirage_server
mirari configure --unix --socket
mirari build --unix --socket
```
The server uses a virtual disk for persistence; you can create an (empty) bootstrap file with 
```
dd if=/dev/zero of=2049 bs=4096 count=256
```
Or (Unix only) just `touch 2049`. (2049 is the decimal major/minor device number for /dev/sda1 on Linux.)

You should then be able to run the server with:
```
./main.native
```
or, most likely if trying direct sockets, 
```
sudo `which mirari` run --unix
```
Build for Xen is similar:
```
cd mirage_server
mirari configure --xen
mirari build --xen
```
As of 2013-10-28 Mirari won't correctly configure the Xen VM with Xen 4.1+, so you can run it (assuming you are using the Xen xl toolchain) with:
```
sudo xl create -c mirage_server.myxl
```

Status: initial test application with users and groups in database, and admin DB forms interface. Browser app skeleton only.
