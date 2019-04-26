/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "apr-15-2019-docker-pt3";
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Docker Part III: Containerizing an Application",
    description: ``,
    date: new Date('2019-04-28T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Docker",
            picture: "https://asset.jarombek.com/logos/docker.png",
            color: "docker"
        },
        {
            name: "Bash",
            picture: "https://asset.jarombek.com/logos/bash.png",
            color: "bash"
        },
        {
            name: "Node.js",
            picture: "https://asset.jarombek.com/logos/nodejs.png",
            color: "nodejs"
        },
        {
            name: "Container"
        },
        {
            name: "Containerized Application"
        },
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
            startName: "\"this\", ",
            endName: "",
            linkName: "http",
            link: "http"
        },
        {
            startName: "Kyle Simpson, ",
            endName: " (Beijing: O'Reilly, 2014), 14",
            linkName: "You Don't Know JavaScript: this & Object Prototypes",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        },
        {
            startName: "",
            endName: ", 22",
            linkName: "Simpson.",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content,
    contentString: JSON.stringify(content)
});