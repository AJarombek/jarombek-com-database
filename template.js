/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/29/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [];

existingPost = db.posts.findOne({name: "jun-4-2018-react-seed"});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: "mar-6-2018-angular-5-first-impressions"});

db.posts.insertOne({
    name: "mar-6-2018-angular-5-first-impressions",
    title: "Angular 5 First Impressions",
    description: `There are a lot of different complexities with generics and how 
        they differ from arrays.  This is my journey to understand Java’s Generics in depth.`,
    date: new Date('2018-06-06T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        }
    ],
    content,
    sources: [
        {
            startName: "\"this\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this",
            link: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this"
        },
        {
            startName: "Kyle Simpson, ",
            endName: " (Beijing: O'Reilly, 2014), 14",
            linkName: "You Don't Know JavaScript: this & Object Prototypes",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        },
        {
            startName: "\"Object.prototype\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/prototype",
            link: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/prototype"
        },
        {
            startName: "",
            endName: ", 22",
            linkName: "Simpson.",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        },
        {
            startName: "",
            endName: ", 33",
            linkName: "Simpson.",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        }
    ]
});