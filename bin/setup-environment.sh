#!/bin/bash

source .project-env

docker build -f Dockerfile-setup -t $DOCKER_IMAGE .
