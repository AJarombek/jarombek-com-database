/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 10/3/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "oct-8-2019-elasticsearch-analyzer";
postDate = new Date('2019-10-08T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Writing Elasticsearch Analyzers",
    description: ``,
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
            name: "ELK Stack",
            picture: "https://asset.jarombek.com/logos/elk.png",
            color: "elk"
        },
        {
            name: "JSON",
            picture: "https://asset.jarombek.com/logos/json.png",
            color: "json"
        },
        {
            name: "Bash",
            picture: "https://asset.jarombek.com/logos/bash.png",
            color: "bash"
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
    sources: []
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});