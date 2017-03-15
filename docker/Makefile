.PHONY: default compile docker push

default: docker 

docker:
	docker run --rm --name DCL -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl /bin/bash

commit:
	docker commit `docker ps -q -l` craigrhodes/dcl

push: 
	docker push craigrhodes/dcl

test:
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=./testall.sh craigrhodes/dcl
