/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 3/30/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "mar-30-2020-numpy";
postDate = new Date('2020-03-30T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Interesting Aspects of Numpy",
    description: `This article isn’t meant to teach numpy to beginners, instead discussing aspects 
        of the library I found most interesting.`,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Python",
            picture: "https://asset.jarombek.com/logos/python.png",
            color: "python"
        },
        {
            name: "Numpy",
            picture: "https://asset.jarombek.com/logos/numpy.png",
            color: "numpy"
        },
        {
            name: "Data Analysis"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "Wes McKinney, ",
            endName: ", 2nd ed (Beijing: O'Reilly, 2017), 88",
            linkName: "Python for Data Analysis: ",
            link: "http://shop.oreilly.com/product/0636920050896.do"
        },
        {
            startName: "\"What is Vectorization?\", ",
            endName: "",
            linkName: "https://realpython.com/numpy-array-programming/#what-is-vectorization",
            link: "https://realpython.com/numpy-array-programming/#what-is-vectorization"
        },
        {
            startName: "\"What is Vectorization?\", ",
            endName: "",
            linkName: "https://stackoverflow.com/a/47755634",
            link: "https://stackoverflow.com/a/47755634"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
