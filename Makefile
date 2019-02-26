.PHONY: deps compile test

REBAR=./rebar

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

test: compile fakes3
	$(REBAR) eunit skip_deps=true

fakes3:
ifeq ($(shell docker inspect -f {{.State.Running}} fake_s3),true)
	@echo "Fake S3 is already running."
else
	docker run --rm --name fake_s3 --network=$(DOCKER_NETWORK) -p 4569:4569 -d lphoward/fake-s3
endif

fakes3-stop:
	docker stop fake_s3

docker-deps: docker-network fakes3

docker-network:
	docker network inspect $(DOCKER_NETWORK) && \
	echo "Docker network $(DOCKER_NETWORK) already exists." || \
	docker network create $(DOCKER_NETWORK)
