/**
 * Script for the MongoDB Shell.
 * You are amazing and deserve all the best.
 * @author Andrew Jarombek
 * @since 11/2/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [];

preview = content.slice(0, 2);

postName = "nov-05-2020-react-component-library";
postDate = new Date('2020-11-05T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Building a Reusable React Component Library",
    description: `Besides the obvious benefit of code reuse, component libraries help to enforce organizational style 
        guides and allow developers to easily iterate on components used in all their applications.  Setup of a 
        component library is easy and can save a lot of front-end development time in the long run.`,
    date: postDate,
    type: "Retrospective",
    views: postViews,
    tags: [
        {
            name: "React",
            picture: "https://asset.jarombek.com/logos/react.png",
            color: "react"
        },
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        },
        {
            name: "JSS",
            picture: "https://asset.jarombek.com/logos/jss.png",
            color: "jss"
        },
        {
            name: "TypeScript",
            picture: "https://asset.jarombek.com/logos/ts.png",
            color: "typescript"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"JSS integration with React\", ",
            endName: "",
            linkName: "https://cssinjs.org/react-jss/?v=v10.4.0",
            link: "https://cssinjs.org/react-jss/?v=v10.4.0"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
