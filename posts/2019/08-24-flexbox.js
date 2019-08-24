/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 8/24/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "aug-24-2019-flexbox";
postDate = new Date('2019-08-24T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Introducing Flexbox",
    description: `While CSS Grid is a two-dimensional system used to create rigid layouts, Flexbox 
        is a one-dimensional system used to create flexible layouts with dynamic resizing of 
        elements.  Let’s go over the basics!`,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Sass",
            picture: "https://asset.jarombek.com/logos/sass.png",
            color: "sass"
        },
        {
            name: "CSS",
            picture: "https://asset.jarombek.com/logos/css.png",
            color: "css"
        },
        {
            name: "Flexbox"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Flexbox Background\", ",
            endName: "",
            linkName: "https://css-tricks.com/snippets/css/a-guide-to-flexbox/#flexbox-background",
            link: "https://css-tricks.com/snippets/css/a-guide-to-flexbox/#flexbox-background"
        },
        {
            startName: "Rachel Andrew, ",
            endName: " (New York: A Book Apart, 2017), 37",
            linkName: "The New CSS Layout",
            link: "https://abookapart.com/products/the-new-css-layout"
        },
        {
            startName: "\"Can I Use Flexbox?\", ",
            endName: "",
            linkName: "https://caniuse.com/#feat=flexbox",
            link: "https://caniuse.com/#feat=flexbox"
        },
        {
            startName: "\"Backwards Compatibility of Flexbox: Status in Browsers\", ",
            endName: "",
            linkName: "https://mzl.la/2HiQJj9",
            link: "https://mzl.la/2HiQJj9"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});