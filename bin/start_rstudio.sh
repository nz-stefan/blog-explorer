#!/bin/bash

source .project-env

# docker run -d -p $DOCKER_PORT:8787 -v $(pwd):/home/rstudio -e DISABLE_AUTH=true $DOCKER_IMAGE

# set path to local renv cache on Docker host and container
RENV_PATHS_CACHE_HOST=/opt/local/renv/cache
RENV_PATHS_CACHE_CONTAINER=/renv/cache

# spin up the container
docker run -d \
    -e "RENV_PATHS_CACHE=${RENV_PATHS_CACHE_CONTAINER}" \
    -e DISABLE_AUTH=true \
    -v "${RENV_PATHS_CACHE_HOST}:${RENV_PATHS_CACHE_CONTAINER}" \
    -v $(pwd):/home/rstudio \
    -p $DOCKER_PORT:8787 \
    $DOCKER_IMAGE && \
echo "Rstudio Server listens on http://localhost:$DOCKER_PORT"
