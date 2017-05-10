.PHONY: default docker push commit modify test stop compile build clean test-ci compile-ci

default: docker 

docker:
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl 

commit:
	docker commit DCL craigrhodes/dcl

compile:
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" --entrypoint=make -it craigrhodes/dcl

modify:
	docker run --name DCL -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl 

build:
	docker build -t craigrhodes/dcl .

remove:
	docker stop DCL
	docker rm DCL

push: 
	docker push craigrhodes/dcl

test: clean compile
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=./testall.sh craigrhodes/dcl

test-ci: compile-ci
	docker run -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=./testall.sh craigrhodes/dcl

compile-ci:
	docker run -it -v "`pwd`/../src:/home/dcl-user/dcl" --entrypoint=make craigrhodes/dcl

clean:
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=make craigrhodes/dcl clean
