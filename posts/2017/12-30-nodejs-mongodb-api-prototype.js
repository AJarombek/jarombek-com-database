/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/28/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [];

db.posts.remove({name: "dec-30-2017-nodejs-mongodb-api-prototype"});

db.posts.insertOne({
    name: "dec-30-2017-nodejs-mongodb-api-prototype",
    title: "Creating a Node.js and MongoDB REST API Prototype",
    date: new Date('2017-12-30T12:00:00'),
    type: "Blog",
    tags: [
        {
            name: "MongoDB",
            picture: "./assets/mongodb.png",
            color: "mongodb"
        },
        {
            name: "Node.js",
            picture: "./assets/nodejs.png",
            color: "nodejs"
        },
        {
            name: "Express",
            picture: "./assets/express.png",
            color: "express"
        },
        {
            name: "JavaScript",
            picture: "./assets/js.png",
            color: "javascript"
        },
        {
            name: "ECMAScript 6",
            picture: "./assets/es6.png",
            color: "ecmascript6"
        },
        {
            name: "NoSQL"
        },
        {
            name: "Document Database"
        },
        {
            name: "Mongoose",
            picture: "./assets/mongoose.png",
            color: "mongoose"
        },
        {
            name: "Babel",
            picture: "./assets/babel.png",
            color: "babel"
        },
        {
            name: "Gulp",
            picture: "./assets/gulp.svg",
            color: "gulp"
        },
        {
            name: "REST"
        }
    ],
    content,
    sources: [
        {
            startName: "Amos Haviv, ",
            endName: ", 2nd ed (Birmingham, UK: Packt, 2016), 110",
            linkName: "MEAN Web Development",
            link: "https://www.packtpub.com/mapt/book/web_development/9781783983285"
        },
        {
            startName: "\"Built-in Promises\", ",
            endName: "",
            linkName: "http://mongoosejs.com/docs/promises.html",
            link: "http://mongoosejs.com/docs/promises.html"
        },
        {
            startName: "Alex Young, Bradley Meck, Mike Cantelon, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2017), 156",
            linkName: "Node.js In Action",
            link: "https://www.manning.com/books/node-js-in-action-second-edition"
        },
        {
            startName: "",
            endName: "., 157",
            linkName: "Young",
            link: "https://www.manning.com/books/node-js-in-action-second-edition"
        },
        {
            startName: "",
            endName: "., 158",
            linkName: "Young",
            link: "https://www.manning.com/books/node-js-in-action-second-edition"
        },
        {
            startName: "Kyle Banker, Peter Bakkum, Shaun Verch, Douglas Garrett &amp; Tom Hawkins, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2016), 252",
            linkName: "MongoDB In Action",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "",
            endName: "., 255",
            linkName: "Banker",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "\"Mongoose - Search for text in three fields based on score or weightage\", ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/32063998/mongoose-search-for-text-in-three-fields-based-on-score-or-weightage",
            link: "https://stackoverflow.com/questions/32063998/mongoose-search-for-text-in-three-fields-based-on-score-or-weightage"
        },
        {
            startName: "\"The 'Basic' HTTP Authentication Scheme\", ",
            endName: "",
            linkName: "https://tools.ietf.org/html/rfc7617",
            link: "https://tools.ietf.org/html/rfc7617"
        },
        {
            startName: "\"express-basic-auth\", ",
            endName: "",
            linkName: "https://www.npmjs.com/package/express-basic-auth",
            link: "https://www.npmjs.com/package/express-basic-auth"
        },
        {
            startName: "",
            endName: "., 73",
            linkName: "Young",
            link: "https://www.manning.com/books/node-js-in-action-second-edition"
        }
    ]
});