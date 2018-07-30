/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/26/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" In a previous posts I looked at Promises and Generators in JavaScript.  I mentioned that these two new ES6 features can be especially powerful when combined.  I do think that Promises on their own are very useful, however I am not yet sold on Generators.  Now we can see how Generators can potentially be used in production level code. ",
                "children":null
            }
        ]
    }
];

content = [
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" In a previous posts I looked at Promises and Generators in JavaScript.  I mentioned that these two new ES6 features can be especially powerful when combined.  I do think that Promises on their own are very useful, however I am not yet sold on Generators.  Now we can see how Generators can potentially be used in production level code. ",
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
                "value":" When combining Promises and Generators, we can create a function that handles asynchronous tasks (let us call it ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":").  This function takes one parameter - a generator function.  Each ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"yield",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" statement in the generator passes a promise, which when resolved with a value calls the generators iterator.  Therefore each asynchronous task in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function moves on to the next task in sequential order when the promise is completed.  Let's look at some pseudocode: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"function async(generator) {\n    var iterator = generator();\n    await(iterator.next())\n\n    function await(promise) {\n        promise.then(function() {\n            await(iterator.next());\n        })\n    }\n}\n",
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
                "value":" This code lacks all the error handling and other technicalities that would be seen in production code, but displays the basic ideas of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function.  Now lets look at some pseudocode of how ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" would be called using some made up functions that execute HTTP GET calls. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"async(function *tasks() {\n    yield httpGet(\"www.example.com/search/1\");\n    yield httpGet(\"www.example.com/search/2\");\n    yield httpGet(\"www.example.com/search/3\");\n})\n",
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
                "value":" With our reusable ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function, this is how simple our asynchronous code can be!  Even better, it is laid out in a synchronous manner, no promises or dreaded callbacks to deal with!  All of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Promise",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" code is extracted out. So why am I only showing pseudocode and not the real thing?  This is because in ECMAScript 2017 (or following the old naming convention ES8) this pattern has gotten even easier with a standardized ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function",
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
                "value":" which is created using the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"await",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keywords. ",
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
                "value":" In my discovery on promises I created a google search API for cat posts.  I am going to use this example again except this time using an ES2017 ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function.  I will use Node 9 so that I won't have to transpile my code with Babel.  One thing that Node 9 (the newest version of Node.js as of November 2017) does not currently support is ES6 modules by default, so I have to use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"--experimental-modules",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" flag when running my code to enable them",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"2",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  I also have to use the .mjs extension for my JavaScript files so Node.js knows that they are modules.  These files are hilariously referred to as Micheal Jackson Scripts.  Hopefully the .mjs files are just for the experimental version and not here to stay - I would miss those .js files (and it doesn't seem like an elegant solution)! ",
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
                "value":" Now let's get to the code.  The first piece is the meowHttp module which creates a promise for our google search",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"3",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  This module will be imported into the main JavaScript file and used in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"import {default as https} from 'https';\n\nexport function search(query = 'cat') {\n\nconst url = 'https://www.googleapis.com/customsearch/v1?key=AIzaSyA2QIPJoGYMx_DuQH6wDqNG3AHXG7bcb94' +\n            '&cx=005720647135116730303:chkg-pnyocg&q=' + query + '&prettyPrint=false';\n\nreturn new Promise(function (resolve, reject) {\n\n    https.get(url, res => {\n        res.setEncoding('utf-8');\n        let response = '';\n\n        res.on('data', data => {\n            response += data;\n        });\n\n        res.on('end', () => {\n            resolve(JSON.parse(response));\n        });\n        });\n    });\n}\n",
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
                "value":" Let's put this module to work with our ES2017 ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"import * as http from './meowHttp';\n\n(async function search() {\n    let catResult = await http.search();\n    print(catResult);\n\n    let meowResult = await http.search('meow');\n    print(meowResult);\n})();\n",
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
                "value":" If you compare this code with the previous Promise and Generator code you can see that the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"await",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keywords and simply syntactic sugar for our ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"await()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" functions.  Now this is a truly elegant solution that allows asynchronous code to be written in an easy to understand synchronous manner.  All of the synchronous code is separated out in our imported modules ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"search()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function.  In this code the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"print()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function displays the names of the articles from the API, and the result of running the code is as follows: ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"img",
                "attributes":{
                    "class":"jarombek-blog-image",
                    "src":"https://asset.jarombek.com/posts/11-26-17-results.png"
                },
                "value":null,
                "children":[

                ]
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
                "value":" Now you can see how powerful the ES2017 ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"async",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function is! Although you don't actually work with the details of combined Promises and Generators in ES2017, it is still good to know what is going on behind the scenes!  You can find all the code from this discovery ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/\nmaster/2017/11-Nov/11-26-JS-Async-Function"
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

postName = "nov-26-2017-js-async-function";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "JavaScript Async Functions: Combining Promises and Generators",
    date: new Date('2017-11-26T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        },
        {
            name: "ECMAScript 6",
            picture: "https://asset.jarombek.com/logos/es6.png",
            color: "ecmascript6"
        },
        {
            name: "ECMAScript 2017",
            picture: "https://asset.jarombek.com/logos/es2017.png",
            color: "javascript"
        },
        {
            name: "Asynchronous Programming"
        },
        {
            name: "Promises"
        },
        {
            name: "Generators"
        },
        {
            name: "Async Function"
        },
        {
            name: "Node.js",
            picture: "https://asset.jarombek.com/logos/nodejs.png",
            color: "nodejs"
        }
    ],
    preview,
    sources: [
        {
            startName: "\"async function\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function",
            link: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function"
        },
        {
            startName: "\"Node.js v9.2.0 Documentation: ECMAScript Modules\", ",
            endName: "",
            linkName: "https://nodejs.org/api/esm.html#esm_enabling",
            link: "https://nodejs.org/api/esm.html#esm_enabling"
        },
        {
            startName: "\"Node.js v9.2.0 Documentation: HTTPS\", ",
            endName: "",
            linkName: "https://nodejs.org/api/https.html",
            link: "https://nodejs.org/api/https.html"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});