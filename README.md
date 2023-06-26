# cs-3110-final

## Team
Sydney Tran - sdt35  
Benjamin Tang - bt283  
Victoria Zhang - vz36  
Jennifer Gu - jg2368


## Install Instructions

Follow the steps below in terminal to install/use the melody generator:

1. To install OPAM packages:  
   `$ opam install ANSITerminal`  
   `$ opam install ao`
   `$ opam install mm`
2. To build the melody generator:  
   `$ make build`
3. To play the melody generator:  
   `$ make play`
4. To remove build files when done:  
   `$ make clean`
5. To view documentation html:  
   `$ dune build @doc`  
   `$ make doc`  
   `$ make opendoc`

Note that you will need OCaml 4.14.0 installed such as from [this guide](https://cs3110.github.io/textbook/chapters/preface/install.html).
