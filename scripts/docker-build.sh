#!/bin/bash

name=$(basename ${PWD})
image=ghcr.io/theeconomist/${name}:latest

push() {
  docker login ghcr.io
  docker buildx build --push --platform linux/amd64 -t $image .
}

run() {
  docker pull $image
  docker run -ti --rm -v $PWD:"/${name}" --workdir="/${name}" $image /bin/bash
}

$1