/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/13/2018
 */

const connection = new Mongo();
const db = connection.getDB("jarombekcom");

db.posts.insertMany([
    {
        title: "Creating a Simple Geographical Map with Neo4j and Cypher",
        date: new Date('2017-11-06'),
        type: "Discovery",
        tags: [
            {
                name: "Neo4j",
                picture: "./assets/neo4j.png",
                color: "neo4j"
            },
            {
                name: "Graph Databases"
            },
            {
                name: "Cypher Query Language"
            },
            {
                name: "NoSQL"
            }
        ],
        content: "",
        sources: []
    }
]);