/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 6/13/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [];

preview = content.slice(0, 2);

postName = "jun-15-2021-saints-xctf-v2-aws-infrastructure";
postDate = new Date('2021-06-15T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "SaintsXCTF Version 2.0: AWS Infrastructure",
    description: ``,
    date: postDate,
    type: "Retrospective",
    views: postViews,
    tags: [
        {
            name: "AWS",
            picture: "https://asset.jarombek.com/logos/aws.png",
            color: "aws"
        },
        {
            name: "Terraform",
            picture: "https://asset.jarombek.com/logos/terraform.png",
            color: "terraform"
        },
        {
            name: "HCL"
        },
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        },
        {
            name: "Python",
            picture: "https://asset.jarombek.com/logos/python.png",
            color: "python"
        },
        {
            name: "AWS EC2",
            picture: "https://asset.jarombek.com/logos/ec2.png",
            color: "ec2"
        },
        {
            name: "AWS Lambda",
            picture: "https://asset.jarombek.com/logos/awslambda.png",
            color: "awslambda"
        },
        {
            name: "AWS S3",
            picture: "https://asset.jarombek.com/logos/awss3.svg",
            color: "awss3"
        },
        {
            name: "AWS RDS",
            picture: "https://asset.jarombek.com/logos/awsrds.png",
            color: "awsrds"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [

    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
