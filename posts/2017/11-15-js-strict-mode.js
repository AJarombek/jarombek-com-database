/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/26/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" One JavaScript feature I have used before but never fully known the details of is strict mode. Strict mode restricts certain features and leniencies in the language",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"1",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  In a way it protects the developer from things that happen implicitly in JavaScript (that only an experienced dev would be aware of).  In a ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/nov-11-2017-js-this"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" previous discovery",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" I looked at how strict mode disallows ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"this",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" to be set to the global scope when invoking a function.  What else does strict mode have to offer? ",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" Certain things that silently fail in JavaScript will now throw errors under strict mode.  This includes setting properties to primitives and assigning values to keywords. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"try {\n    undefined = \"defined!\";\n} catch(e) {\n    console.error('Strict mode disallows assigning values to keywords');\n    console.error(e);\n}\n\ntry {\n    false.not = true;\n} catch(e) {\n    console.error('Strict mode disallows setting properties for primitive values');\n    console.error(e);\n}\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" Strict mode also disallows duplicate parameter names for functions.  It also used to restrict duplicate property names on an object, but this restriction has been lifted as of ES6 (either a bug or poor design choice). ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"// Strict mode does not throw an error for duplicate property names\nvar andy = {\n    name: \"Andy J\",\n    name: \"whoops\",\n    /* Strict mode disallows duplicate parameter names for functions\n    func: function(a, b, a) {\n        console.info(a + b + c);\n    }*/\n};\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" One final feature we will look at is the restriction of creating a new variable when assigning a value to a variable without the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"var",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"let",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", or ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"const",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keyword. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"try {\n    hey = \"hello\"\n} catch(e) {\n    console.error('Strict mode disallows assigning to undeclared variables');\n    console.error(e);\n}\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" Without strict mode forgetting the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"var",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" or ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"let",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable is “forgiving.”  If the variable has not yet been defined, JavaScript creates a new global variable with the name provided. In my opinion this should be considered a syntax mistake by the developer and throw an error.  That is exactly what happens in strict mode. ",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" Strict mode has other features as well that can come in useful (such as throwing errors when trying to assign values to immutable properties) but the ones I went over have the largest impact on my code. You can find the code used in this discovery ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/\nblob/master/2017/11-Nov/11-15-JS-Strict-Mode/strict-mode.js"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"HERE",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". ",
                "children":null
            }
        ]
    }
];

postViews = db.posts.findOne({name: "nov-15-2017-js-strict-mode"}).views;

db.posts.remove({name: "nov-15-2017-js-strict-mode"});

db.posts.insertOne({
    name: "nov-15-2017-js-strict-mode",
    title: "JavaScript Strict Mode",
    date: new Date('2017-11-15T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "JavaScript",
            picture: "./assets/js.png",
            color: "javascript"
        },
        {
            name: "ECMAScript 6",
            picture: "./assets/es6.png",
            color: "javascript"
        },
    ],
    content,
    sources: [
        {
            startName: "\"Strict mode\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode",
            link: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode"
        }
    ]
});