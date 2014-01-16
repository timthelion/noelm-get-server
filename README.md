# The noelm-get-server

This package contains everything you need to run your own noelm-get server for sharing projects and their documentation with your friends and familly.

## Install the server on a local development machine

Clone this repo and run cabal install:

````
$ git clone https://github.com/timthelion/noelm-get-server.git
$ cd noelm-get-server
$ cabal install
````

## Run the server on a local development machine

Move to the website directory and run `noelm-get-server`.

````
$ cd website
$ noelm-get-server -p 8000
````

## Remove packages from the server

While in the `noelm-get-server/website` directory run `noelm-get-server-remove` passing it the name of the package that you want removed as your argument:

````
$ noelm-get-server-remove timthelion/noelm-get-hacker
````

# Set up a public noelm-get server on a VPS running debian wheezy

## Install haskell-platform

````
# apt-get update
# apt-get install haskell-platform
````

## Create a new user for the server

````
# useradd -m noelm-get
# passwd noelm-get
Enter password: *****
Re-enter password: *****
````

## install the server software as the noelm-get user

### Move to the noelm-get user's home dir

````
# su noelm-get
$ cd
````

#### Setup cabal as the user

````
$ cabal update
$ echo PATH=\$PATH:\$HOME/.cabal/bin >> ~/.bashrc
````

#### Dowload and install the noelm-get-server

Note: It is best to follow these instructions exactly.
If you install Noelm first and then noelm-get, then you will
run into cabal conflicts.  You must install all of the packages
at the same time!

````
$ git clone https://github.com/timthelion/Noelm.git
$ git clone https://github.com/timthelion/noelm-get.git
$ git clone https://github.com/timthelion/noelm-get-server.git
$ cabal install Noelm/ noelm-get/ noelm-get-server/
````

#### Make the server accessable to the world:

You have two options.

** Stand alone server **

You can make the server accessible to the world by giving it access to low numbered ports as so:

````
$ su
# apt-get install libcap2-bin
# setcap 'cap_net_bind_service=+ep' /home/noelm-get/.cabal/bin/noelm-get-server
# exit
````

** Server as a subdomain with lighttpd **

I run my server as under the [noelm-get.thobbs.cz](http://noelm-get.thobbs.cz) subdomain of [thobbs.cz](http://thobbs.cz) by adding the following lines to my lighttpd.conf file:

````
server.modules +=  ("mod_proxy")
````

````
$HTTP["host"] =~ "noelm-get.thobbs." {
        proxy.balance = "fair"
        proxy.server  = ( "" => (
        ( "host" => "127.0.0.1", "port" => 8000 ))
    )
}
````
  
And setting noelm-get-server's port to 8000.

#### Install the init.d service file

##### Set the port you want your server to run with:

````
$ cd
$ cd noelm-get-server/etc/init.c/
$ vi noelm-get-server
Change the NOELM_GET_SERVER_PORT variable
````

##### Put the service file in its proper place

````
$ su
# cp /home/noelm-get/noelm-get-server/etc/init.d/noelm-get-server /etc/init.d/noelm-get-server
````

##### Make the file executable

````
# chmod +x /etc/init.d/noelm-get-server
````

##### Start the service

````
# /etc/init.d/noelm-get-server start
````