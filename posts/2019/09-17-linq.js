/**
 * Script for the MongoDB Shell.  In case I'm your one, don't worry.
 * I'm here once you know what to say.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "sep-15-2019-linq";
postDate = new Date('2019-09-15T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Using LINQ in C# and .NET",
    description: `I'll save my exploration of integrating LINQ with a remote database for a future 
        article.  Today, I'm focusing on LINQ basics with local data structures.`,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "C#",
            picture: "https://asset.jarombek.com/logos/csharp.png",
            color: "csharp"
        },
        {
            name: ".NET Core",
            picture: "https://asset.jarombek.com/logos/dotnetcore.png",
            color: "dotnetcore"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Language Integrated Query (LINQ)\", ",
            endName: "",
            linkName: "https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/",
            link: "https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/"
        },
        {
            startName: "Joseph Albahari & Ben Albahari, ",
            endName: " (Beijing: O'Reilly, 2018), 355",
            linkName: "C# 7.0 in a Nutshell",
            link: "http://shop.oreilly.com/product/0636920083634.do"
        },
        {
            startName: "",
            endName: ", 364",
            linkName: "Ibid.",
            link: "http://shop.oreilly.com/product/0636920083634.do"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});