.PHONY: default docker push commit modify test stop compile build clean

default: docker 

docker:
	docker run --rm --name DCL -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl /bin/bash

commit:
	docker commit DCL craigrhodes/dcl

compile:
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" --entrypoint=make -it craigrhodes/dcl

modify:
	docker run --name DCL -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl /bin/bash

build:
	docker build -t craigrhodes/dcl .

remove:
	docker stop DCL
	docker rm DCL

push: 
	docker push craigrhodes/dcl

test: clean compile
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=./testall.sh craigrhodes/dcl

clean:
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=make craigrhodes/dcl clean
