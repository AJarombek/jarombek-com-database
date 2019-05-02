# Dockerfile for the MongoDB instance of jarombek-com-database
# Author: Andrew Jarombek
# Date: 5/2/2019

FROM mongo:4.0

# Make a new directory that MongoDB uses to save data
RUN mkdir -p /data/db

# Copy the jarombek-com-database source code into /src and use it as a working directory
COPY . /src
WORKDIR /src

# Start mongodb, execute the base database scripts, and shutdown mongodb
RUN mongod --fork --logpath /var/log/mongodb.log \
    && mongo --quiet loadScripts.js \
    && mongod --shutdown

# Create a volume out of the MongoDB data directory to persist it (https://stackoverflow.com/a/33601894)
VOLUME /data/db

# Expose the database port and start MongoDB in the container
EXPOSE 27017
ENTRYPOINT ["mongod"]