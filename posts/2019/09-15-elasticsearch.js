/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 9/14/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "sep-15-2019-elasticsearch";
postDate = new Date('2019-09-15T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Introduction to Elasticsearch",
    description: `In this article I discuss Elasticsearch, which is the core technology of ELK 
        Stack.`,
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
            name: "Kibana",
            picture: "https://asset.jarombek.com/logos/kibana.png",
            color: "kibana"
        },
        {
            name: "ELK Stack",
            picture: "https://asset.jarombek.com/logos/elk.png",
            color: "elk"
        },
        {
            name: "Search Engine"
        },
        {
            name: "Text Search"
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
            name: "Terraform",
            picture: "https://asset.jarombek.com/logos/terraform.png",
            color: "terraform"
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
            startName: "Pranav Shukla & Sharath Kumar M N, ",
            endName: " (Birmingham: Packt, 2017), 8",
            linkName: "Learning Elastic Stack 6.0",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        },
        {
            startName: "",
            endName: ", 26",
            linkName: "Ibid.",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        },
        {
            startName: "",
            endName: ", 10-12",
            linkName: "Ibid.",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        },
        {
            startName: "",
            endName: ", 30",
            linkName: "Ibid.",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        },
        {
            startName: "",
            endName: ", 32",
            linkName: "Ibid.",
            link: "https://www.packtpub.com/big-data-and-business-intelligence/learning-elastic-stack-60"
        },
        {
            startName: "\"Difference between keyword and text in ElasticSearch\", ",
            endName: "",
            linkName: "https://stackoverflow.com/a/52845279",
            link: "https://stackoverflow.com/a/52845279"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});