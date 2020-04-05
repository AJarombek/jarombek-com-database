/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/5/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "apr-9-2020-pandas";
postDate = new Date('2020-04-09T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Interesting Aspects of Pandas",
    description: ``,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Pandas",
            picture: "https://asset.jarombek.com/logos/pandas.png",
            color: "pandas"
        },
        {
            name: "Numpy",
            picture: "https://asset.jarombek.com/logos/numpy.png",
            color: "numpy"
        },
        {
            name: "Python",
            picture: "https://asset.jarombek.com/logos/python.png",
            color: "python"
        },
        {
            name: "Data Analysis"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"pandas (software)\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Pandas_(software)",
            link: "https://en.wikipedia.org/wiki/Pandas_(software)"
        },
        {
            startName: "\"pandas (software): History\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Pandas_(software)#History",
            link: "https://en.wikipedia.org/wiki/Pandas_(software)#History"
        },
        {
            startName: "\"Applications of Pandas\", ",
            endName: "",
            linkName: "https://data-flair.training/blogs/applications-of-pandas/",
            link: "https://data-flair.training/blogs/applications-of-pandas/"
        },
        {
            startName: "Wes McKinney, ",
            endName: ", 2nd ed (Beijing: O'Reilly, 2017), 126",
            linkName: "Python for Data Analysis: ",
            link: "http://shop.oreilly.com/product/0636920050896.do"
        },
        {
            startName: "",
            endName: ", 130",
            linkName: "McKinney.",
            link: "http://shop.oreilly.com/product/0636920050896.do"
        },
        {
            startName: "",
            endName: ", 5",
            linkName: "McKinney.",
            link: "http://shop.oreilly.com/product/0636920050896.do"
        },
        {
            startName: "\"data.frame function\", ",
            endName: "",
            linkName: "https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame",
            link: "https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/data.frame"
        },
        {
            startName: "\"Iterable\", ",
            endName: "",
            linkName: "https://docs.oracle.com/javase/8/docs/api/java/lang/Iterable.html",
            link: "https://docs.oracle.com/javase/8/docs/api/java/lang/Iterable.html"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
