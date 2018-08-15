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
                "value":" In many programming languages arrow functions are added to allow for concise one liners and decreased verbosity.  Less lines of code is one of the things that made the Java 8 release with lambdas so appealing.  In ES6 JavaScript also added arrow functions to write shorter, more readable code.  In JavaScript however these new functions aren’t as universally praised due to some quirks. ",
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
                "value":" In JavaScript I have ",
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
                        "value":"looked at",
                        "children":null
                    }
                ]
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
                "value":" and how it is set dynamically at runtime instead of lexically depending on the scope code is written in.  I also went over how many JavaScript users (including myself!) are easily confused by how ",
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
                "value":" works in the language.  Arrow functions look to ‘fix’ these confusions by implementing a lexical ",
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
                "value":".  Now the value of ",
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
                "value":" in an arrow function depends on what ",
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
                "value":" is equal to at the time it was written (instead of where it is called).  This leads to confusing behavior if you are expecting arrow functions to act like normal function definitions.  Let’s look at an example. ",
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
                "value":" In many programming languages arrow functions are added to allow for concise one liners and decreased verbosity.  Less lines of code is one of the things that made the Java 8 release with lambdas so appealing.  In ES6 JavaScript also added arrow functions to write shorter, more readable code.  In JavaScript however these new functions aren’t as universally praised due to some quirks. ",
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
                "value":" In JavaScript I have ",
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
                        "value":"looked at",
                        "children":null
                    }
                ]
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
                "value":" and how it is set dynamically at runtime instead of lexically depending on the scope code is written in.  I also went over how many JavaScript users (including myself!) are easily confused by how ",
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
                "value":" works in the language.  Arrow functions look to ‘fix’ these confusions by implementing a lexical ",
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
                "value":".  Now the value of ",
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
                "value":" in an arrow function depends on what ",
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
                "value":" is equal to at the time it was written (instead of where it is called).  This leads to confusing behavior if you are expecting arrow functions to act like normal function definitions.  Let’s look at an example. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"'use strict';\n\nfunction mult() {\n    this.x = 50;\n\n    var x2Arrow = () => {\n        console.info(this);\n        return this.x * 2;\n    };\n\n    var x2 = function times2() {\n        console.info(this);\n        return this.x * 2;\n    };\n\n    return {x2, x2Arrow};\n}\n\nvar m = new mult();\n",
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
                "value":" In this example we have a function ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"mult()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" which creates an API for multiplying the ",
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
                "value":" value by two.  Since we use the constructor call (with the ",
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
                "value":" keyword) ",
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
                "value":" is assigned with the newly created object, in this case the returned object.  Now let’s look at what happens when we call these functions: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"m.x2Arrow(); // this -> mult {x: 50}, return -> 100\nm.x2(); // this -> {x2: function(), x2Arrow: function()}, return -> NaN\n",
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
                "value":" Why does the regular function definition return ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"NaN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"? Remember that ",
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
                "value":" is defined as the return object of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"mult()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", which contains two properties: ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"x2",
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
                "value":"x2Arrow",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". Notice there is no ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"x",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property defined, so ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"this.x",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" equals ",
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
                "value":".  Multiplying a number by ",
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
                "value":" in JavaScript equals ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"NaN",
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
                "value":" But why does the arrow function maintain the correct ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"this.x",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" value and return the intended result?  Because of lexical ",
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
                "value":" , the ",
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
                "value":" reference in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"x2Arrow",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is the value that was held by ",
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
                "value":" in the function definitions scope (",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"this.x = 50",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"). Lexical scoped ",
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
                "value":" values cannot be changed even when called later in the code in different scopes.  Let’s put this claim to the test by trying to explicitly set ",
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
        "value":"m.x2Arrow.call({x: 5}); // this -> mult {x: 50}, return -> 100\nm.x2.call({x: 5}); // this => {x: 5}, return -> 10\n",
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
                "value":" As you can see the arrow function was not effected by our explicit ",
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
                "value":" binding!  Very cool (and a bit confusing).  Some claim that arrow functions fix some of JavaScript’s flaws",
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
                "value":" (in this case confusing ",
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
                "value":" rules).  Others say it breaks expected behavior in the language",
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
                "value":".  I think if you aren’t using ",
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
                "value":" in your code arrow functions can be a great way to increase readability.  Just be careful if the code gets in the hands of developers who aren’t aware of JavaScript’s quirks! ",
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
                "value":" You can find the full example code ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/2017/11-Nov/11-20-JS-Arrow-Function/\narrow-functions.js"
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

postName = "nov-20-2017-js-arrow-functions";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Understanding \"this\" in JavaScript",
    date: new Date('2017-11-20T12:00:00'),
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
            color: "javascript"
        },
        {
            name: "Lambda Functions"
        }
    ],
    preview,
    sources: [
        {
            startName: "John Resig, Bear Bibeault, & Josip Maras, ",
            endName: " (Shelter Island, NY: Manning, 2016), 83",
            linkName: "Secrets of the JavaScript Ninja",
            link: "https://www.manning.com/books/secrets-of-the-javascript-ninja-second-edition"
        },
        {
            startName: "Kyle Simpson, ",
            endName: " (Beijing: O'Reilly, 2014), 33",
            linkName: "You Don't Know JavaScript: this & Object Prototypes",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/this%20%26%20object%20prototypes"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});