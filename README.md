# DCL

CircleCI: 
[![CircleCI](https://circleci.com/gh/CraigRhodes/dcl.svg?style=svg&circle-token=126d40f16064f154caa60d88a21304ccbd157a17)](https://circleci.com/gh/CraigRhodes/dcl)  


This is the home for DCL - a new callback-based programming language.

## Generate compiler

To generate the compiler executable:  
  `cd docker`
  `make compile`

## Run tests  

To run tests:  
  `cd docker`
  `make test`

## Docker

To compile:  
  `cd docker`  
  `make # This puts you inside the docker container ` 
  `make # This makes the executable`
  `./microc.native < <filename>`
OR  
Run the following command inside the docker environment:  
  `compile <filename>`  

To run:  
  `lli <compiled_file>`

To compile and run inside docker container:  
  `run <filename>`  
