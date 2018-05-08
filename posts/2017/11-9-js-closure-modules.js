/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/24/2018
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
                "value":" In JavaScript there are many module patterns that can be used to create APIs and separate concerns in code (as of ES6 there is actually syntax for modules in the spec). In the following code I created an API using the revealing module pattern.  The name comes from the return statement at the end of the module - it 'reveals' functions to outside code. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var taylorSwiftApi = function() {\n    var lyric = {\n        sparks: \"I see sparks fly whenever you smile\",\n        mine: \"You are the best thing thats ever been mine\",\n        dec: \"I go back to Decemeber all the time\",\n        other: \"I'm sorry, taylor can't pick up the phone right now\"\n    }\n\n    function lyrics(song) {\n        switch(song.toLowerCase()) {\n            case \"sparks fly\":\n            return lyric.sparks;\n            case \"mine\":\n            return lyric.mine;\n            case \"back to december\":\n            return lyric.dec;\n            default:\n            return lyric.other;\n        }\n    }\n\n    return {\n        lyrics: lyrics\n    }\n}();\n",
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
                "value":" This module shows Taylor Swift lyrics for songs that the user enters (because who doesn’t enjoy some T-Swift!)  The return statement is the public API revealed to outside code.  All interior details, such as the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"lyric",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable, are hidden. This pattern harnesses the power of closure in JavaScript! ",
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
                "value":" Closure has been defined as “when a function is able to remember and access its lexical scope even when that function is executing outside its lexical scope",
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
                "value":".” ",
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
                "value":" Okay cool, so even when we call ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"taylorSwiftApi.lyrics()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" later in the code it will be aware of the interior ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"lyric",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable! But what is this 'lexical scope' I see in the definition? ",
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
                "value":" Lexical scope is when the physical structure of the program you write determines the scope.  Because of this the scope in which you define a function in JavaScript will always be that functions scope.  This scope is immutable and can’t be changed. ",
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
                "value":" Maybe lexical scope and closure will be easier to understand with an example of us calling the Taylor Swift API: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var lyric = {\n    sparks: \"I was enchanted to meet you\"\n};\n\nlet result = taylorSwiftApi.lyrics(\"Sparks Fly\");\nconsole.info(result); // I see sparks fly whenever you smile\n",
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
                "value":" You can see that in the global scope we define another variable named lyric with a different (and as Swift fans know – incorrect) value.  The question is when we call ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"taylorSwiftApi.lyrics()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" which ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"lyric.sparks",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable will the API find?  In this case it will be the one defined in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"taylorSwiftApi",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" because of a closure that was created over the lexical scope of the API.  The function ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"taylorSwiftApi.lyrics()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" was defined in the API functions lexical scope, so that scope can’t change! ",
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
                "value":" As I briefly mentioned in the beginning of this discovery there is support for modules in the JavaScript spec as of ES6.  I will look at this Taylor Swift API using these modules in the future.  You can find all the code for this discovery ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/2017/\n11-Nov/11-9-JS-Closure-Modules/module.js"
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
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" A final note: there is another type of scoping called dynamic scope.  This is used in shell scripts such as Bash and PowerShell.  In these scripting languages the scope is determined by the executing programs location on the call stack",
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
                "value":".  This may be a fun topic to explore in the future! ",
                "children":null
            }
        ]
    }
];

db.posts.remove({name: "nov-9-2017-js-closure-modules"});

db.posts.insertOne({
    name: "nov-9-2017-js-closure-modules",
    title: "Closure & Lexical Scope in JavaScript Modules",
    date: new Date('2017-11-09T12:00:00'),
    type: "Discovery",
    tags: [
        {
            name: "JavaScript",
            picture: "./assets/js.png",
            color: "javascript"
        },
        {
            name: "API"
        }
    ],
    content,
    sources: [
        {
            startName: "Kyle Simpson, ",
            endName: " (Beijing: O'Reilly, 2014), 48",
            linkName: "You Don't Know JavaScript: Scope & Closures",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/scope%20%26%20closures"
        },
        {
            startName: "\"Static (Lexical) Scoping vs Dynamic Scoping (Pseudocode)\", March 14th, 2014, ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/22394089/static-lexical-scoping-vs-dynamic-scoping-pseudocode",
            link: "https://stackoverflow.com/questions/22394089/static-lexical-scoping-vs-dynamic-scoping-pseudocode"
        }
    ]
});