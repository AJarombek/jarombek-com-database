/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/24/2018
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
                "value":" JavaScript has quickly become one of the languages that I use the most (probably second behind Java). Many people use JavaScript along with one of its many frontend frameworks (JQuery, AngularJS, etc.) without really knowing how the core language operates.  I don’t want to be one of those people! ",
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
                "value":" This will be the first of many discovery posts on JavaScript.  Let’s look at one of the basic concepts of the language: how variables interact with scope.  By scope I mean the area of a program you can access a variable (e.g. a variable declared in a function can only be accessed within that function). You can also think of scope as the execution environment for a line in a program.  The scope is all the other variables and functions this program line is aware of.  In JavaScript scope can get a bit tricky. ",
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
                "value":" JavaScript has quickly become one of the languages that I use the most (probably second behind Java). Many people use JavaScript along with one of its many frontend frameworks (JQuery, AngularJS, etc.) without really knowing how the core language operates.  I don’t want to be one of those people! ",
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
                "value":" This will be the first of many discovery posts on JavaScript.  Let’s look at one of the basic concepts of the language: how variables interact with scope.  By scope I mean the area of a program you can access a variable (e.g. a variable declared in a function can only be accessed within that function). You can also think of scope as the execution environment for a line in a program.  The scope is all the other variables and functions this program line is aware of.  In JavaScript scope can get a bit tricky. ",
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
                "value":" One tricky part is that declared variables in JavaScript get hoisted to the top of their scope.  Let’s look at a quick code snippet. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"console.info(x);  // Error??\n\nvar x = 10;\n",
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
                "value":" For a Java developer like myself when first starting JavaScript I expected an error.  Variable x hasn’t been declared yet!  However, JavaScript returns ",
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
                "value":", which means the variable has been declared but not initialized a variable. ",
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
                "value":" What’s happening is JavaScript splits the statement ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"var x = 10",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" into its declaration and assignment.  It then ‘hoists’ the declaration to the top of the scope (in this case the global program scope).  Before the call to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"console.info()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" there is a line created that contains",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"var x;",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". Tricky JavaScript! ",
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
                "value":" Now if you are using ES6+ and don’t want hoisting you can use the ",
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
                "value":" keyword (replace ",
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
                "value":" with ",
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
                "value":" in the previous example).  Now if you run the code the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"console.info(x)",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" call will throw an error. ",
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
                "value":" Here is one more basic example: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"{\n    let firstName = \"Andy\";\n    var lastName = \"Jarombek\";\n\n    console.info(firstName); // Andrew\n    console.info(lastName); // Jarombek\n}\n\nconsole.info(lastName); // Jarombek\nconsole.info(firstName); // Reference Error\n",
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
                "value":" In JavaScript you can define a scope with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"{ }",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" block. You can see that the ",
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
                "value":" statement is only available within the block scope while the variable defined with ",
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
                "value":" is exposed to the global scope",
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
                "value":".  This can be useful when you want to constrain a variable to one small piece of code. ",
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
                "value":" One thing that the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"{ }",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" block behavior made me question was ‘are variables defined in functions available globally?’  The answer is NO (thankfully) so your internally defined function variables are local and safe. ",
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
                "value":" You can check out the code for these examples ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/2017/11-Nov/11-8-JS-Scope-Hoisting/\nscope.js"
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
                "value":" along with a look at how variables in for loops leak into the global scope in JavaScript (Yuck! – but there is a solution with ",
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
                "value":"). ",
                "children":null
            }
        ]
    }
];

postName = "nov-8-2017-js-scope-hoisting";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Scope & Hoisting in JavaScript",
    date: new Date('2017-11-08T12:00:00'),
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
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "Kyle Simpson, ",
            endName: " (Beijing: O'Reilly, 2014), 36",
            linkName: "You Don't Know JavaScript: Scope & Closures",
            link: "https://github.com/getify/You-Dont-Know-JS/tree/master/scope%20%26%20closures"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content,
    contentString: JSON.stringify(content)
});