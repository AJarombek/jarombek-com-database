/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 8/18/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [];

content = [];

postName = "aug-18-2019-type-equality";
postDate = new Date('2019-08-18T12:00:00');
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Revisiting Type Equality",
    description: `In my time spent re-learning type equality in 13 different languages I’ve 
        reaffirmed my knowledge and gained new insights.  The rest of the article discusses my 
        findings.`,
    date: postDate,
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Java",
            picture: "https://asset.jarombek.com/logos/java.png",
            color: "java"
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
            name: "Bash",
            picture: "https://asset.jarombek.com/logos/bash.png",
            color: "bash"
        },
        {
            name: "C",
            picture: "https://asset.jarombek.com/logos/c.png",
            color: "c"
        },
        {
            name: "C++",
            picture: "https://asset.jarombek.com/logos/cpp.png",
            color: "cpp"
        },
        {
            name: "C#",
            picture: "https://asset.jarombek.com/logos/csharp.png",
            color: "csharp"
        },
        {
            name: "Groovy",
            picture: "https://asset.jarombek.com/logos/groovy.png",
            color: "groovy"
        },
        {
            name: "Haskell",
            picture: "https://asset.jarombek.com/logos/haskell.png",
            color: "haskell"
        },
        {
            name: "PHP",
            picture: "https://asset.jarombek.com/logos/php.svg",
            color: "php"
        },
        {
            name: "PowerShell",
            picture: "https://asset.jarombek.com/logos/powershell.png",
            color: "powershell"
        },
        {
            name: "Swift",
            picture: "https://asset.jarombek.com/logos/swift.png",
            color: "swift"
        },
        {
            name: "TypeScript",
            picture: "https://asset.jarombek.com/logos/ts.png",
            color: "typescript"
        },
        {
            name: "Object Oriented Programming"
        },
        {
            name: "Imperative Programming"
        },
        {
            name: "Functional Programming"
        },
        {
            name: "Command Line Scripting"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "Kyle Simpson, ",
            endName: " (Beijing: O'Reilly, 2014), 1",
            linkName: "You Don't Know JavaScript: Types & Grammar",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/types%20%26%20grammar"
        },
        {
            startName: "\"how equal operator works with primitive and object type data\", ",
            endName: "",
            linkName: "https://stackoverflow.com/a/29139595",
            link: "https://stackoverflow.com/a/29139595"
        },
        {
            startName: "\"Object Equality in JavaScript\", ",
            endName: "",
            linkName: "http://adripofjavascript.com/blog/drips/object-equality-in-javascript.html",
            link: "http://adripofjavascript.com/blog/drips/object-equality-in-javascript.html"
        },
        {
            startName: "\"How to check if two arrays are equal with vanilla JS\", ",
            endName: "",
            linkName: "https://gomakethings.com/how-to-check-if-two-arrays-are-equal-with-vanilla-js/",
            link: "https://gomakethings.com/how-to-check-if-two-arrays-are-equal-with-vanilla-js/"
        },
        {
            startName: "\"How do you compare structs for equality in C?\", ",
            endName: "",
            linkName: "https://stackoverflow.com/q/141720",
            link: "https://stackoverflow.com/q/141720"
        },
        {
            startName: "\"memcmp\", ",
            endName: "",
            linkName: "http://www.cplusplus.com/reference/cstring/memcmp/",
            link: "http://www.cplusplus.com/reference/cstring/memcmp/"
        },
        {
            startName: "\"r/haskell: Why no reference equality?\", ",
            endName: "",
            linkName: "https://www.reddit.com/r/haskell/comments/4ivvge/why_no_reference_equality/",
            link: "https://www.reddit.com/r/haskell/comments/4ivvge/why_no_reference_equality/"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    date: postDate,
    content,
    contentString: JSON.stringify(content)
});