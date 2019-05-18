/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 5/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "may-20-2019-kubernetes-pt2";
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Orchestrating Containers with Kubernetes Part II: Single Node Cluster",
    description: `In this article, I’m building a single node Kubernetes cluster running a Node.js 
        application.`,
    date: new Date('2019-05-20T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Kubernetes",
            picture: "https://asset.jarombek.com/logos/k8s.png",
            color: "k8s"
        },
        {
            name: "Docker",
            picture: "https://asset.jarombek.com/logos/docker.png",
            color: "docker"
        },
        {
            name: "Container"
        },
        {
            name: "Container Orchestrator"
        },
        {
            name: "AWS",
            picture: "https://asset.jarombek.com/logos/aws.png",
            color: "aws"
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