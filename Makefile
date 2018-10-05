#!make
include docker.env
export $(shell sed 's/=.*//' docker.env)

$(PROJECT_BIN_DIR)/$(PROJECT_BIN):
	stack build --local-bin-path $(PROJECT_BIN_DIR) --copy-bins

build: $(PROJECT_BIN_DIR)/$(PROJECT_BIN)
.PHONY: build

docker-build:
	docker-compose run --rm builder $(SRC_DIR)/docker-builder.sh

image:
	docker build -t $(IMAGE_NAME):$(IMAGE_VERSION) .
	docker tag $(IMAGE_NAME):$(IMAGE_VERSION) $(IMAGE_NAME):latest

clean:
	rm -f $(PROJECT_BIN_DIR)/* || true
	rm *.cabal # remove hpack generated cabal file
	stack clean
