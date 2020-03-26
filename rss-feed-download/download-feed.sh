#!/bin/bash

# r-blogger XML feed
URL_FEED="http://feeds.feedburner.com/RBloggers?format=xml"

# output file name is based on datetime
F_NAME=$(date +"%Y%m%d-%H%M%S").xml

# create year and month directories on the S3 bucket
S3_PATH=$(date +"%Y/%m")

# download feed from r-bloggers
echo "Downloading feed from '$URL_FEED' into '$F_NAME'"
curl "$URL_FEED" > $F_NAME

# upload data to AWS S3
echo "Uploading file to 's3://blog-explorer/${S3_PATH}/${F_NAME}'"
aws s3 cp $F_NAME s3://blog-explorer/${S3_PATH}/${F_NAME}

echo "Done"
