/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 9/29/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "sep-29-2019-linq-sqlserver";
postDate = new Date('2019-09-29T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Integrated Queries with LINQ and SQL Server",
    description: `In this article, I’m exploring integrated queries with a SQL Server database.  
        First I’ll create the SQL Server database instance with Docker and then query it 
        with LINQ.`,
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
            name: "SQL Server",
            picture: "https://asset.jarombek.com/logos/sqlserver.png",
            color: "sqlserver"
        },
        {
            name: ".NET Core",
            picture: "https://asset.jarombek.com/logos/dotnetcore.png",
            color: "dotnetcore"
        },
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
            name: "Relational Database"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Microsoft SQL Server\", ",
            endName: "",
            linkName: "https://hub.docker.com/_/microsoft-mssql-server?tab=description",
            link: "https://hub.docker.com/_/microsoft-mssql-server?tab=description"
        },
        {
            startName: "\"mssql-node-docker-demo-app\", ",
            endName: "",
            linkName: "https://github.com/twright-msft/mssql-node-docker-demo-app",
            link: "https://github.com/twright-msft/mssql-node-docker-demo-app"
        },
        {
            startName: "\"Basic Queries\", ",
            endName: "",
            linkName: "https://docs.microsoft.com/en-us/ef/core/querying/basic",
            link: "https://docs.microsoft.com/en-us/ef/core/querying/basic"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});