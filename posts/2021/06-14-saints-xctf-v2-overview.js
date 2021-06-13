/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 6/12/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [];

preview = content.slice(0, 2);

postName = "jun-14-2021-saints-xctf-v2-overview";
postDate = new Date('2021-06-14T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "SaintsXCTF Version 2.0: Architectural Overview",
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
            name: "Terraform",
            picture: "https://asset.jarombek.com/logos/terraform.png",
            color: "terraform"
        },
        {
            name: "TypeScript",
            picture: "https://asset.jarombek.com/logos/ts.png",
            color: "typescript"
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
            name: "PHP",
            picture: "https://asset.jarombek.com/logos/php.svg",
            color: "php"
        },
        {
            name: "iOS",
            picture: "https://asset.jarombek.com/logos/ios.png",
            color: "ios"
        },
        {
            name: "Android",
            picture: "https://asset.jarombek.com/logos/android.png",
            color: "android"
        },
        {
            name: "Flask",
            picture: "https://asset.jarombek.com/logos/flask.png",
            color: "flask"
        },
        {
            name: "React",
            picture: "https://asset.jarombek.com/logos/react.png",
            color: "react"
        },
        {
            name: "Redux",
            picture: "https://asset.jarombek.com/logos/redux.png",
            color: "redux"
        },
        {
            name: "JSS",
            picture: "https://asset.jarombek.com/logos/jss.png",
            color: "jss"
        },
        {
            name: "MySQL",
            picture: "https://asset.jarombek.com/logos/mysql.png",
            color: "mysql"
        },
        {
            name: "JQuery",
            picture: "https://asset.jarombek.com/logos/jquery.png",
            color: "jquery"
        },
        {
            name: "AWS API Gateway",
            picture: "https://asset.jarombek.com/logos/apigateway.svg",
            color: "apigateway"
        },
        {
            name: "AWS EKS",
            picture: "https://asset.jarombek.com/logos/eks.png",
            color: "eks"
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
        {
            startName: "\"Amazon RDS Service Level Agreement\", ",
            endName: "",
            linkName: "https://aws.amazon.com/rds/sla/",
            link: "https://aws.amazon.com/rds/sla/"
        },
        {
            startName: "\"Amazon Compute Service Level Agreement\", ",
            endName: "",
            linkName: "https://aws.amazon.com/compute/sla/",
            link: "https://aws.amazon.com/compute/sla/"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
