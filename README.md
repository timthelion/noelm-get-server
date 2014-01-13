# The noelm-get-server

This package contains everything you need to run your own noelm-get server for sharing projects and their documentation with your friends and familly.

## Install the server

Clone this repo and run cabal install:

````
$git clone https://github.com/timthelion/noelm-get-server.git
$cd noelm-get-server
$cabal install
````

## Run the server

Move to the website directory and run `noelm-get-server`.

````
$cd website
$noelm-get-server -p 8000
````

## Remove packages from the server

While in the `noelm-get-server/website` directory run `noelm-get-server-remove` passing it the name of the package that you want removed as your argument:

````
$noelm-get-server-remove timthelion/noelm-get-hacker
````