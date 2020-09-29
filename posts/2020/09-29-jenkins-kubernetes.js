/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 9/27/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "sep-29-2020-jenkins-kubernetes";
postDate = new Date('2020-09-29T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Jenkins Server Modern Infrastructure with Kubernetes on EKS",
    description: `In this article I'll discuss the second generation of that infrastructure, which 
        uses Docker containers orchestrated by Kubernetes on an EKS cluster.`,
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
            name: "Kubernetes",
            picture: "https://asset.jarombek.com/logos/k8s.png",
            color: "k8s"
        },
        {
            name: "AWS EKS",
            picture: "https://asset.jarombek.com/logos/eks.png",
            color: "eks"
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
            name: "Docker",
            picture: "https://asset.jarombek.com/logos/docker.png",
            color: "docker"
        },
        {
            name: "DevOps"
        },
        {
            name: "HCL"
        },
        {
            name: "YAML",
            picture: "https://asset.jarombek.com/logos/yaml.png",
            color: "yaml"
        },
        {
            name: "Groovy",
            picture: "https://asset.jarombek.com/logos/groovy.png",
            color: "groovy"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Using Docker-in-Docker for your CI or testing environment? Think twice.\", ",
            endName: "",
            linkName: "https://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/",
            link: "https://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/"
        },
        {
            startName: "\"Kubernetes plugin for Jenkins\", ",
            endName: "",
            linkName: "https://github.com/jenkinsci/kubernetes-plugin/blob/master/README.md",
            link: "https://github.com/jenkinsci/kubernetes-plugin/blob/master/README.md"
        },
        {
            startName: "\"Jenkins Configuration as Code\", ",
            endName: "",
            linkName: "https://www.jenkins.io/projects/jcasc/",
            link: "https://www.jenkins.io/projects/jcasc/"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
