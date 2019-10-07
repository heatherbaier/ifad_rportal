# install.packages("aws.s3")

library(aws.s3)

# Set up your keys here - you need to sign up for an aws account which i'll send directions for separatley
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAVT2CNKEKE4KUXQPB",
           "AWS_SECRET_ACCESS_KEY" = "EMfEzTgJsZeLmNUxK02J8+Yz3YS+mOkYBijpf50+")

# The region is important here! Make sure to check it on the open data site
# This will print a list of the objects in the bucket to your console
get_bucket(bucket = 'dataforgood-fb-data',
           key = 'AKIAVT2CNKEKE4KUXQPB',
           secret = "EMfEzTgJsZeLmNUxK02J8+Yz3YS+mOkYBijpf50+",
           region = "us-east-1",
           check_region = FALSE)

# Find the content name you want and that is the first argumne there - again, region is important
save_object("tif/month=2019-06/country=ERI/type=youth_15_24/ERI_youth_15_24.tif", 
            bucket = "dataforgood-fb-data",
            region = "us-east-1",
            check_region = FALSE,
            '~/Desktop/fb_try.tif')