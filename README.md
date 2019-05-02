# jarombek-com-database

### Overview

Scripts for populating the Jarombek.com MongoDB database.

### Files

| Filename             | Description                                                                                              |
|----------------------|----------------------------------------------------------------------------------------------------------|
| `posts/`             | All the tokenized articles on the website.                                                               |
| `docker.sh`          | Bash commands to create a Docker image and push it to Docker Hub.                                        |
| `Dockerfile`         | Blueprint for a Docker image running MongoDB.                                                            |
| `loadScripts.js`     | Main entrypoint for the database scripts.  Calling this script will properly set up the entire database. |
| `maintenance.js`     | Common MongoDB shell commands used when working with the database & debugging.                           |
| `template.js`        | A base template for each post in the database.                                                           |