/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 12/13/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "feb-15-2020-s3-react";
postDate = new Date('2020-02-15T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Hosting a Static React Application on Amazon S3",
    description: `In this article I discuss the process of hosting my demo application on Amazon 
        S3 along with the challenges I faced.`,
    date: postDate,
    type: "Retrospective",
    views: postViews,
    tags: [
        {
            name: "AWS S3",
            picture: "https://asset.jarombek.com/logos/awss3.svg",
            color: "awss3"
        },
        {
            name: "AWS CloudFront",
            picture: "https://asset.jarombek.com/logos/aws-cloudfront.svg",
            color: "awscloudfront"
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
        },
        {
            name: "React",
            picture: "https://asset.jarombek.com/logos/react.png",
            color: "react"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Disadvantages of a static website\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Static_web_page#Disadvantages_of_a_static_website",
            link: "https://en.wikipedia.org/wiki/Static_web_page#Disadvantages_of_a_static_website"
        },
        {
            startName: "\"Dynamic web page\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Dynamic_web_page",
            link: "https://en.wikipedia.org/wiki/Dynamic_web_page"
        },
        {
            startName: "\"What Is Amazon CloudFront?\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Introduction.html",
            link: "https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Introduction.html"
        },
        {
            startName: "\"Example: Setting Up a Static Website Using a Custom Domain\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/AmazonS3/latest/dev/website-hosting-custom-domain-walkthrough.html",
            link: "https://docs.aws.amazon.com/AmazonS3/latest/dev/website-hosting-custom-domain-walkthrough.html"
        },
        {
            startName: "\"(Optional) Configuring a Webpage Redirect\", ",
            endName: "",
            linkName: "https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html",
            link: "https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
