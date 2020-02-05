/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 2/4/2020
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "feb-5-2020-react-jest-enzyme";
postDate = new Date('2020-02-05T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Unit, Integration, and Snapshot Testing in React with Jest and Enzyme",
    description: `This article walks through a React testing suite, providing insights about Jest 
        and Enzyme in the process.`,
    date: postDate,
    type: "Retrospective",
    views: postViews,
    tags: [
        {
            name: "Jest",
            picture: "https://asset.jarombek.com/logos/jest.png",
            color: "jest"
        },
        {
            name: "Enzyme",
            picture: "https://asset.jarombek.com/logos/enzyme.png",
            color: "enzyme"
        },
        {
            name: "React",
            picture: "https://asset.jarombek.com/logos/react.png",
            color: "react"
        },
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        },
        {
            name: "NPM",
            picture: "https://asset.jarombek.com/logos/npm.png",
            color: "npm"
        },
        {
            name: "Unit Test"
        },
        {
            name: "Integration Test"
        },
        {
            name: "Snapshot Test"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Configuring your Adapter\", ",
            endName: "",
            linkName: "https://github.com/airbnb/enzyme/blob/HEAD/docs/guides/migration-from-2-to-3.md#configuring-your-adapter",
            link: "https://github.com/airbnb/enzyme/blob/HEAD/docs/guides/migration-from-2-to-3.md#configuring-your-adapter"
        },
        {
            startName: "\"react-test-renderer\", ",
            endName: "",
            linkName: "https://www.npmjs.com/package/react-test-renderer",
            link: "https://www.npmjs.com/package/react-test-renderer"
        },
        {
            startName: "\"Snapshot Testing\", ",
            endName: "",
            linkName: "https://jestjs.io/docs/en/snapshot-testing",
            link: "https://jestjs.io/docs/en/snapshot-testing"
        },
        {
            startName: "\"shallow([options])\", ",
            endName: "",
            linkName: "https://airbnb.io/enzyme/docs/api/ShallowWrapper/shallow.html",
            link: "https://airbnb.io/enzyme/docs/api/ShallowWrapper/shallow.html"
        },
        {
            startName: "\"exists([selector])\", ",
            endName: "",
            linkName: "https://airbnb.io/enzyme/docs/api/ShallowWrapper/exists.html",
            link: "https://airbnb.io/enzyme/docs/api/ShallowWrapper/exists.html"
        },
        {
            startName: "\"getDOMNode()\", ",
            endName: "",
            linkName: "https://airbnb.io/enzyme/docs/api/ReactWrapper/getDOMNode.html",
            link: "https://airbnb.io/enzyme/docs/api/ReactWrapper/getDOMNode.html"
        },
        {
            startName: "\"jest.spyOn(object, methodName)\", ",
            endName: "",
            linkName: "https://jestjs.io/docs/en/jest-object#jestspyonobject-methodname",
            link: "https://jestjs.io/docs/en/jest-object#jestspyonobject-methodname"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
