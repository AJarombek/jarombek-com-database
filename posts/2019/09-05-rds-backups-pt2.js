/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 9/4/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "sep-5-2019-rds-backups-pt2";
postDate = new Date('2019-09-05T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "AWS Lambda Function to Backup a MySQL RDS Instance Part II: Building the Function",
    description: `In this article I create additional AWS resources and further explain my design 
        decisions.  Finally, I test the lambda function and show the backup file in my S3 bucket.`,
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
            name: "Terraform",
            picture: "https://asset.jarombek.com/logos/terraform.png",
            color: "terraform"
        },
        {
            name: "MySQL",
            picture: "https://asset.jarombek.com/logos/mysql.png",
            color: "mysql"
        },
        {
            name: "AWS S3",
            picture: "https://asset.jarombek.com/logos/awss3.svg",
            color: "awss3"
        },
        {
            name: "AWS Secrets Manager",
            picture: "https://asset.jarombek.com/logos/aws-secrets-manager.png",
            color: "awssm"
        },
        {
            name: "AWS VPC Endpoint",
            picture: "https://asset.jarombek.com/logos/aws-vpc-endpoint.png",
            color: "awsvpcendpoint"
        },
        {
            name: "AWS IAM",
            picture: "https://asset.jarombek.com/logos/awsiam.png",
            color: "awsiam"
        },
        {
            name: "AWS",
            picture: "https://asset.jarombek.com/logos/aws.png",
            color: "aws"
        },
        {
            name: "HCL"
        },
        {
            name: "Python",
            picture: "https://asset.jarombek.com/logos/python.png",
            color: "python"
        },
        {
            name: "Bash",
            picture: "https://asset.jarombek.com/logos/bash.png",
            color: "bash"
        },
        {
            name: "Infrastructure as Code"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"AWS Lambda Runtimes\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html",
            link: "https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html"
        },
        {
            startName: "\"AWS Lambda: Internet and Service Access for VPC-Connected Functions\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html#vpc-internet",
            link: "https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html#vpc-internet"
        },
        {
            startName: "\"VPC Endpoints\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html",
            link: "https://docs.aws.amazon.com/vpc/latest/userguide/vpc-endpoints.html"
        },
        {
            startName: "\"AWS Lambda: Execution Role and User Permissions\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html#vpc-permissions",
            link: "https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html#vpc-permissions"
        },
        {
            startName: "\"Use terraform to set up a lambda function triggered by a scheduled event source\", ",
            endName: "",
            linkName: "https://stackoverflow.com/a/35895316",
            link: "https://stackoverflow.com/a/35895316"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});