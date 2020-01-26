/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 12/31/2019
 */

// I'm so happy that you are part of this world and with us all.

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "dec-31-2019-react-16-3";
postDate = new Date('2019-12-31T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Features of React 16.3",
    description: ``,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "React",
            picture: "https://asset.jarombek.com/logos/react.png",
            color: "react"
        },
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Context\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/context.html",
            link: "https://reactjs.org/docs/context.html"
        },
        {
            startName: "\"When to Use Context\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/context.html#when-to-use-context",
            link: "https://reactjs.org/docs/context.html#when-to-use-context"
        },
        {
            startName: "\"Using Context API in React (Hooks and Classes)\", ",
            endName: "",
            linkName: "https://www.taniarascia.com/using-context-api-in-react/",
            link: "https://www.taniarascia.com/using-context-api-in-react/"
        },
        {
            startName: "\"Context.Consumer\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/context.html#contextconsumer",
            link: "https://reactjs.org/docs/context.html#contextconsumer"
        },
        {
            startName: "\"Hooks API Reference: useContext\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/hooks-reference.html#usecontext",
            link: "https://reactjs.org/docs/hooks-reference.html#usecontext"
        },
        {
            startName: "\"createRef API\", ",
            endName: "",
            linkName: "https://reactjs.org/blog/2018/03/29/react-v-16-3.html#createref-api",
            link: "https://reactjs.org/blog/2018/03/29/react-v-16-3.html#createref-api"
        },
        {
            startName: "\"forwardRef API\", ",
            endName: "",
            linkName: "https://reactjs.org/blog/2018/03/29/react-v-16-3.html#forwardref-api",
            link: "https://reactjs.org/blog/2018/03/29/react-v-16-3.html#forwardref-api"
        },
        {
            startName: "\"Use componentWillMount or componentDidMount lifecycle functions for async request in React\", ",
            endName: "",
            linkName: "https://stackoverflow.com/a/47393005",
            link: "https://stackoverflow.com/a/47393005"
        },
        {
            startName: "\"Using Derived State in React: getDerivedStateFromProps\", ",
            endName: "",
            linkName: "https://alligator.io/react/get-derived-state/#getderivedstatefromprops",
            link: "https://alligator.io/react/get-derived-state/#getderivedstatefromprops"
        },
        {
            startName: "\"React.Component: static getDerivedStateFromProps()\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/react-component.html#static-getderivedstatefromprops",
            link: "https://reactjs.org/docs/react-component.html#static-getderivedstatefromprops"
        },
        {
            startName: "\"getSnapshotBeforeUpdate() in ReactJS\", ",
            endName: "",
            linkName: "https://www.learnperday.com/react/course/getsnapshotbeforeupdate.php#defination",
            link: "https://www.learnperday.com/react/course/getsnapshotbeforeupdate.php#defination"
        },
        {
            startName: "\"React.Component: getSnapshotBeforeUpdate()\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/react-component.html#getsnapshotbeforeupdate",
            link: "https://reactjs.org/docs/react-component.html#getsnapshotbeforeupdate"
        },
        {
            startName: "\"Strict Mode\", ",
            endName: "",
            linkName: "https://reactjs.org/docs/strict-mode.html",
            link: "https://reactjs.org/docs/strict-mode.html"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});
