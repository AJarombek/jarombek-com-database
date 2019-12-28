/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 12/28/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "dec-28-2019-elasticsearch-queries";
postDate = new Date('2019-12-28T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Basic Elasticsearch Queries",
    description: `This article focuses on querying Elasticsearch documents`,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Elasticsearch",
            picture: "https://asset.jarombek.com/logos/elasticsearch.png",
            color: "elasticsearch"
        },
        {
            name: "JSON",
            picture: "https://asset.jarombek.com/logos/json.png",
            color: "json"
        },
        {
            name: "Search Engine"
        },
        {
            name: "Text Search"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "Pranav Shukla & Sharath Kumar M N, ",
            endName: " (Birmingham: Packt, 2017), 75",
            linkName: "Learning Elastic Stack 6.0",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        },
        {
            startName: "",
            endName: ", 84",
            linkName: "Shukla.",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        },
        {
            startName: "",
            endName: ", 90",
            linkName: "Shukla.",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
