#!/bin/bash

###############################################################################
# Deploy the r-bloggers-rssfeed container into AWS container repository.  
# Instructions taken from AWS push commands
#
# Author: Stefan Schliebs
# Created: 2020-03-26
###############################################################################

# retrieve the login command to use to authenticate your Docker client to your registry
$(aws ecr get-login --no-include-email --region us-east-1)

# tag your image so you can push the image to this repository
docker tag project/r-bloggers-rssfeed:latest 456636380153.dkr.ecr.us-east-1.amazonaws.com/r-bloggers-rssfeed:latest

# push this image to your newly created AWS repository
docker push 456636380153.dkr.ecr.us-east-1.amazonaws.com/r-bloggers-rssfeed:latest

