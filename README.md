# jarombek-com-database

### Overview

Scripts for populating the Jarombek.com MongoDB database.

### Commands

**Run the database locally with Docker**

```bash
# Create the database
docker image build -t jarombek-com-database:latest .
docker container run -d --name jarombek-com-database -p 27017:27017 jarombek-com-database:latest

# Destroy the database
docker container stop jarombek-com-database
docker container rm jarombek-com-database
```

**Query the database running locally with Docker**

```bash
# Host machine
docker exec -it jarombek-com-database /bin/bash 

# Container
mongo

# MongoDB Shell
use jarombekcom
db.posts.count()
```

**Update the database running locally with Docker**

```bash
# Host machine
docker cp . jarombek-com-database:/src/
docker exec -it jarombek-com-database /bin/bash

# Container
mongo

# MongoDB Shell
use jarombekcom
load('./loadScripts.js')
```

### Files

| Filename             | Description                                                                                              |
|----------------------|----------------------------------------------------------------------------------------------------------|
| `posts/`             | All the tokenized articles on the website.                                                               |
| `docker.sh`          | Bash commands to create a Docker image and push it to Docker Hub.                                        |
| `Dockerfile`         | Blueprint for a Docker image running MongoDB.                                                            |
| `loadScripts.js`     | Main entrypoint for the database scripts.  Calling this script will properly set up the entire database. |
| `maintenance.js`     | Common MongoDB shell commands used when working with the database & debugging.                           |
| `template.js`        | A base template for each post in the database.                                                           |