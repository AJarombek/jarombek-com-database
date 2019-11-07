/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 11/6/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "nov-10-2019-garbage-collection";
postDate = new Date('2019-11-10T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "The Basics of Programming Language Garbage Collection",
    description: `This article gives a high-level overview of garbage collectors and the APIs 
        available to interact with them in Java and C#.`,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Garbage Collection"
        },
        {
            name: "Java",
            picture: "https://asset.jarombek.com/logos/java.png",
            color: "java"
        },
        {
            name: "C#",
            picture: "https://asset.jarombek.com/logos/csharp.png",
            color: "csharp"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Garbage collection (computer science)\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)",
            link: "https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)"
        },
        {
            startName: "\"Reference counting: Garbage collection\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Reference_counting#Garbage_collection",
            link: "https://en.wikipedia.org/wiki/Reference_counting#Garbage_collection"
        },
        {
            startName: "\"Garbage collection (computer science): Reference counting\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)#Reference_counting",
            link: "https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)#Reference_counting"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});