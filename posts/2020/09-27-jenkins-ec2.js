/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 9/26/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "sep-27-2020-jenkins-ec2";
postDate = new Date('2020-09-26T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Jenkins Server Legacy Infrastructure on EC2 and EFS",
    description: `In this article, I discuss my original EC2 Jenkins server and its creation 
        process with Terraform.  In an upcoming article, I’ll discuss the Kubernetes Jenkins 
        server infrastructure.`,
    date: postDate,
    type: "Retrospective",
    views: postViews,
    tags: [
        {
            name: "Jenkins",
            picture: "https://asset.jarombek.com/logos/jenkins.png",
            color: "jenkins"
        },
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
            name: "Amazon EC2",
            picture: "https://asset.jarombek.com/logos/ec2.png",
            color: "ec2"
        },
        {
            name: "Amazon EFS",
            picture: "https://asset.jarombek.com/logos/aws-efs.png",
            color: "efs"
        },
        {
            name: "Packer",
            picture: "https://asset.jarombek.com/logos/packer.png",
            color: "packer"
        },
        {
            name: "Ansible",
            picture: "https://asset.jarombek.com/logos/ansible.png",
            color: "ansible"
        },
        {
            name: "DevOps"
        },
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Amazon Elastic File System\", ",
            endName: "",
            linkName: "https://aws.amazon.com/efs/",
            link: "https://aws.amazon.com/efs/"
        },
        {
            startName: "Michael Wittig & Andreas Wittig, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2019), 275",
            linkName: "Amazon Web Services In Action",
            link: "https://www.manning.com/books/amazon-web-services-in-action-second-edition"
        },
        {
            startName: "\"Network File System\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Network_File_System",
            link: "https://en.wikipedia.org/wiki/Network_File_System"
        },
        {
            startName: "\"What is Packer?\", ",
            endName: "",
            linkName: "https://www.packer.io/intro#what-is-packer",
            link: "https://www.packer.io/intro#what-is-packer"
        },
        {
            startName: "\"Packer Terminology: Builders\", ",
            endName: "",
            linkName: "https://www.packer.io/docs/terminology#builders",
            link: "https://www.packer.io/docs/terminology#builders"
        },
        {
            startName: "\"AMI Builder (EBS backed)\", ",
            endName: "",
            linkName: "https://www.packer.io/docs/builders/amazon/ebs",
            link: "https://www.packer.io/docs/builders/amazon/ebs"
        },
        {
            startName: "\"Jenkins Configuration as Code (a.k.a. JCasC) Plugin\", ",
            endName: "",
            linkName: "https://github.com/jenkinsci/configuration-as-code-plugin/blob/master/README.md",
            link: "https://github.com/jenkinsci/configuration-as-code-plugin/blob/master/README.md"
        },
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
