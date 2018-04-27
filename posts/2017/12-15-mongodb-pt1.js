/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/26/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [];

db.posts.remove({name: "dec-15-2017-mongodb-pt1"});

db.posts.insertOne({
    name: "dec-15-2017-mongodb-pt1",
    title: "Learning MongoDB Part I: Creating the Database",
    date: new Date('2017-12-15T12:00:00'),
    type: "Discovery",
    tags: [
        {
            name: "MongoDB",
            picture: "./assets/mongodb.png",
            color: "mongodb"
        },
        {
            name: "JavaScript",
            picture: "./assets/js.png",
            color: "javascript"
        },
        {
            name: "Vim",
            picture: "./assets/vim.png",
            color: "vim"
        },
        {
            name: "Relational Database"
        },
        {
            name: "NoSQL"
        },
        {
            name: "Document Database"
        }
    ],
    content,
    sources: [
        {
            startName: "\"BSON\", ",
            endName: "",
            linkName: "http://bsonspec.org",
            link: "http://bsonspec.org"
        },
        {
            startName: "\"Atomicity and Transactions\", ",
            endName: "",
            linkName: "https://docs.mongodb.com/manual/core/write-operations-atomicity/",
            link: "https://docs.mongodb.com/manual/core/write-operations-atomicity/"
        },
        {
            startName: "\"Atomicity (database systems)\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Atomicity_(database_systems)",
            link: "https://en.wikipedia.org/wiki/Atomicity_(database_systems)"
        },
        {
            startName: "Kyle Banker, Peter Bakkum, Shaun Verch, Douglas Garrett &amp; Tom Hawkins, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2016), 31",
            linkName: "MongoDB In Action",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "",
            endName: "., 34",
            linkName: "Ibid",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        }
    ]
});