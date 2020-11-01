#!/usr/bin/env bash

# Commands used to create Docker images and push them to repositories
# Author: Andrew Jarombek
# Date: 5/2/2019

# Build the image in the repository root directory based on the Dockerfile
docker image build -t jarombek-com-database:latest .

# Push image to Docker Hub with tag 'latest'
docker image tag jarombek-com-database:latest ajarombek/jarombek-com-database:latest
docker push ajarombek/jarombek-com-database:latest

# Push image to Docker Hub with version tag
docker image tag jarombek-com-database:latest ajarombek/jarombek-com-database:1.1.8
docker push ajarombek/jarombek-com-database:1.1.8

# Run the container
docker container run -d --name jarombek-com-database -p 27018:27017 jarombek-com-database:latest

# Execute commands on the running container
docker exec -it jarombek-com-database /bin/bash

# Remove existing containers
docker container stop jarombek-com-database
docker container rm jarombek-com-database
