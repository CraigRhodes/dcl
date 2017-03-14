.PHONY: default compile docker push

default: docker 

docker:
	docker run -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl /bin/bash

commit:
	docker commit `docker ps -q -l` craigrhodes/dcl

push: 
	docker push craigrhodes/dcl

test:
	docker run -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=./testall.sh craigrhodes/dcl
