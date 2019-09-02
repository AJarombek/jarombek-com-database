/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 9/2/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "sep-3-2019-rds-backups-pt1";
postDate = new Date('2019-09-03T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "AWS Lambda Function to Backup a MySQL RDS Instance Part I: VPC Infrastructure",
    description: `In this series of articles, I’m going to create an automated mechanism for backing 
        up RDS instances.  My approach uses an AWS Lambda function and the mysqldump command line 
        utility to store MySQL database backup files on Amazon S3.`,
    date: postDate,
    type: "Retrospective",
    views: postViews,
    tags: [
        {
            name: "AWS Lambda",
            picture: "https://asset.jarombek.com/logos/awslambda.png",
            color: "awslambda"
        },
        {
            name: "AWS RDS",
            picture: "https://asset.jarombek.com/logos/awsrds.png",
            color: "awsrds"
        },
        {
            name: "MySQL",
            picture: "https://asset.jarombek.com/logos/mysql.png",
            color: "mysql"
        },
        {
            name: "AWS S3",
            picture: "https://asset.jarombek.com/logos/awss3.png",
            color: "awss3"
        },
        {
            name: "AWS Secrets Manager",
            picture: "https://asset.jarombek.com/logos/awssm.png",
            color: "awssm"
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
            startName: "\"Creating a DB Snapshot\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CreateSnapshot.html",
            link: "https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CreateSnapshot.html"
        },
        {
            startName: "\"How to copy/move RDS snapshots to S3 service?\", ",
            endName: "",
            linkName: "https://forums.aws.amazon.com/message.jspa?messageID=607366",
            link: "https://forums.aws.amazon.com/message.jspa?messageID=607366"
        },
        {
            startName: "\"Virtual private cloud\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Virtual_private_cloud",
            link: "https://en.wikipedia.org/wiki/Virtual_private_cloud"
        },
        {
            startName: "\"Internet Access for Lambda Functions\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/lambda/latest/dg/vpc.html#vpc-internet",
            link: "https://docs.aws.amazon.com/lambda/latest/dg/vpc.html#vpc-internet"
        },
        {
            startName: "\"VPC Endpoints\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html",
            link: "https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});