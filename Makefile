PROJECT_NAME := $(notdir $(CURDIR))
IMAGE ?= ghcr.io/theeconomist/$(PROJECT_NAME):latest

.PHONY: docker-run-interactive

docker-run:
	docker run -ti --rm -v "$(CURDIR)":/$(PROJECT_NAME) --workdir=/$(PROJECT_NAME) $(IMAGE) /bin/bash
