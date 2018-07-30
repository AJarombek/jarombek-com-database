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
                "value":" The ",
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
                "value":" keyword in JavaScript is a piece of the language that has always confused me.  It doesn't work like ",
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
                "value":" in Java where it refers to the method or constructors object.  Often in the past when using ",
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
                "value":" I have taken shortcuts such as the popular ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"var self = this;",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" so I wouldn't have to worry about what value ",
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
                "value":" holds in nested functions. ",
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
                "value":" The ",
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
                "value":" keyword in JavaScript is a piece of the language that has always confused me.  It doesn't work like ",
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
                "value":" in Java where it refers to the method or constructors object.  Often in the past when using ",
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
                "value":" I have taken shortcuts such as the popular ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"var self = this;",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" so I wouldn't have to worry about what value ",
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
                "value":" holds in nested functions. ",
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
                "value":" What makes ",
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
                "value":" so confusing is that it is set during program runtime instead of defined at author time",
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
                "value":".  In other words it does not follow the rules of ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/nov-8-2017-scope-hoisting"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"lexical scope",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" that I am comfortable with.  It also is incredibly daunting for new programmers since the value of ",
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
                "value":" can potentially be different when calling the same function at different instances. ",
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
                "value":" So what are the rules for ",
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
                "value":"?  If you are in the global scope it refers to the global object, which will be different depending on the environment you are running JavaScript in.  For example if you are running JavaScript in your browser, this will be the window object. ",
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
                "value":" There is an exception to this rule though.  If you are using strict mode, a function won't default its ",
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
                "value":" binding to the global object and instead will return ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"undefined",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  The following example shows an IIFE (Immediately Invoked Function Expression) that is set to strict mode so ",
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
                "value":" is set to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"undefined",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". However, if we removed ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"'use strict';",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" it would return the global object. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"(function() {\n    'use strict';\n\n    console.info(this);\n})();\n",
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
                "value":" You can imagine how confusing it would be dealing with ",
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
                "value":" if some parts of the code were in strict mode and others weren't.  Beacuse of this (and potentially more confusing cases I haven't discovered yet) having different pieces of a codebase be strict while others are not is considered bad practice",
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
                "value":" You can also explicity define the value of ",
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
                "value":" when calling a function with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"call()",
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
                "value":"apply()",
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
                "value":"bind()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" functions.  All objects that have Object.prototype on their prototype chain will have these functions ",
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
                "value":".  Both functions take a parameter which is the value you want to set to ",
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
                "value":".  The difference is that while ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"call()",
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
                "value":"apply()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" bind the ",
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
                "value":" value for this one call, ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"bind()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" creates an immutable bind for all future calls. First lets look at an example of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"call()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":": ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"function name() {\n    console.info(this.username);\n}\n\nvar user = {\n    username: \"Andy\"\n}\n\nname(); // undefined\nname.call(user); // Andy\n",
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
                "value":" In is example instead of the function call defaulting to the global scope for ",
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
                "value":", we manipulate it and pass in an object ",
                "children":null
            },
            {
                "el":"code",
                "attributes":null,
                "value":"user",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" to bind to ",
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
                "value":".  Now lets look at ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"bind()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":": ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var n = name.bind(user); // Andy\n",
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
                "value":" The result we get back from the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"bind()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function is a function itself.  The above code is the equivalent of: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var n = function _n() {\n    name.call(user);\n}\n",
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
                "value":"     When we call the function created by ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"bind()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" the     ",
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
                "value":" value we passed to it is used.  You can also see     if we try and change the value of ",
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
                "value":", we are unable to: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var secondUser = {\n    username: \"Joe\"\n}\n\nn.call(secondUser);\nn.bind(secondUser);\nn(); // Andy\n",
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
                "value":"     Another way to manipulate ",
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
                "value":" is by performing a constructor     call on an object with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"new",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keyword.  In this     case ",
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
                "value":" is set to the newly created object",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"4",
                "children":null
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
                "value":"     Finally if you call a function in the context of an object, that object is used as the value for     ",
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
                "value":".  Here is an example: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var n = name.bind(user); // Andy\n",
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
                "value":" You can see that we make the call ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"user.name();",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in the context of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"user",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object, so the value of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"this.username",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is the one defined in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"user",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Of course you can overwrite this rule if you explicitly bind ",
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
                "value":" using the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"call()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function ",
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
                "value":" In my opinion if you have a team of developers that understand how ",
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
                "value":" works then by all means use it!  However if you are on a team of people who are new to JavaScript it may be smart to avoid it.  I do agree with Kyle Simpson that ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"self = this",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is a confusing hack around practice that shouldn't be used ",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"5",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  You can find the source code for the examples I used ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/2017/11-Nov/11-11-JS-This/this.js"
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

postName = "nov-11-2017-js-this";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Understanding \"this\" in JavaScript",
    date: new Date('2017-11-11T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        }
    ],
    preview,
    sources: [
        {
            startName: "\"this\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this",
            link: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this"
        },
        {
            startName: "Kyle Simpson, ",
            endName: " (Beijing: O'Reilly, 2014), 14",
            linkName: "You Don't Know JavaScript: this & Object Prototypes",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        },
        {
            startName: "\"Object.prototype\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/prototype",
            link: "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/prototype"
        },
        {
            startName: "",
            endName: ", 22",
            linkName: "Simpson.",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        },
        {
            startName: "",
            endName: ", 33",
            linkName: "Simpson.",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});