.PHONY: default docker push commit modify test stop

default: docker 

docker:
	docker run --rm --name DCL -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl /bin/bash

commit:
	docker commit DCL craigrhodes/dcl

modify:
	docker run --name DCL -v "`pwd`/../src:/home/dcl-user/dcl" -it craigrhodes/dcl /bin/bash

remove:
	docker stop DCL
	docker rm DCL

push: 
	docker push craigrhodes/dcl

test:
	docker run --rm -v "`pwd`/../src:/home/dcl-user/dcl" -it --entrypoint=./testall.sh craigrhodes/dcl
