/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 12/31/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "dec-31-2019-react-16-3";
postDate = new Date('2019-12-31T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Features of React 16.3",
    description: ``,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Context\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/context.html",
            link: "https://reactjs.org/docs/context.html"
        },
        {
            startName: "\"When to Use Context\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/context.html#when-to-use-context",
            link: "https://reactjs.org/docs/context.html#when-to-use-context"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
