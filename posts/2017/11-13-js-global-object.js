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
                "value":" When you run code in JavaScript there is a global object created in the global scope.  What this object is depends on the environment JavaScript is running in.  If you are running JavaScript code in your web browser the global object will be ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", which represents the browser window.  Window exposes an API that allows interaction with the Document Object Model (DOM), click listeners, and other information about the browser window",
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
                "value":" In the past I have used with ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object to check the current URL and previous page visited by the user (to create a ‘back’ button).  The most common use of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" however is to manipulate the DOM and set click listeners (I used frameworks such as JQuery to perform these tasks in the past). ",
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
                "value":" Before we look at some of the uses for the window object, it is important to note that when we run code in the global scope ",
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
                "value":" will contain the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"console.info(this); // window object\n\nconsole.info(window); // window object\n",
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
                "value":" Because of this fact we can use ",
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
                "value":" in the global scope (and any ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-submittions/blob/master/Discoveries/\n2017/11-Nov/11-11-JS-This/View/js-this.html"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"other scopes",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" where the assigned value of ",
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
                "value":" is ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":") to refer to window.  One function that the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" API exposes is ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"prompt()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" which displays a field for the user to enter a value.  In the global scope you can call ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"prompt()",
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
                "value":"this",
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
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"this.prompt(\"What is your name?\");\n",
        "children":null
    },
    {
        "el":"figure",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"     ",
                "children":null
            },
            {
                "el":"img",
                "attributes":{
                    "src":"prompt.png"
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
                "value":" Another ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function is ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"open()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" which creates and returns a new window.  We will use ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"open()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" along with windows ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"document.write()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function which will write a new element to the DOM. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var newWindow = window.open();\nnewWindow.document.write(\"",
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
                "value":" In this code we open a new window and assign the newly created ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"newWindow",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"document",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"document.write()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is the API revealed for the DOM.  It is in the document API where we can traverse and manipulate the DOM to create dynamic web pages. ",
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
                "value":" Finally we can utilize the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object's event listeners. Let’s create a function that will be called when the user clicks on the new window.  Once clicked, we want the new window to close. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"newWindow.onclick = function click() {\n    newWindow.close();\n};\n",
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
                "value":"     The problem with this approach is that we can only assign one click listener to the window",
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
                "value":".     If we wanted to create more, we could utilize the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"addEventListener()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"     function on the target object (in this case the window)",
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
                "value":"     This look into the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object and DOM API only scratched     the surface of all the features.  I think understanding both of them can     help a programmer to build better web pages (even if you aren’t contacting them directly.  In newer code,     you are likely to be using higher level frameworks such as JQuery, AngularJS, or ReactJS instead). ",
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
                "value":"     So what about code running in Node.js?  In Node there is also a global object called     ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"global",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  I haven’t yet found anything too useful     in this global object (although my knowledge of Node is limited) but you can get information on the     environment Node is running in by reading global properties. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"// The following results are from my PC\nglobal.process.arch; // x64\nglobal.process.platform; // win32\nglobal.process.env.OS; // Windows_NT\nglobal.process.env.PROCESSOR_ARCHITECTURE; // AMD64\nglobal.process.env.USERNAME; // Andy\nglobal.process.pid; // 15088\n",
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
                "value":"     One final note on the global object in Node.  You may expect the assigned value of     ",
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
                "value":" to be ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"global",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in the     global scope (just like ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"window",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in the browser).  Let’s try it out: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"console.info(this); // { }\n",
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
                "value":" If you run this code, the returned value is the empty object ",
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
                "value":" instead.  Why is this?  It turns out when we debug the file, this code is actually executed like so: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"(function (exports, require, module, __filename, __dirname) {\n    console.info(this);\n});\n",
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
                "value":" It turns out ",
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
                "value":" is assigned to the module itself, which here is empty.  As I mentioned my Node.js experience is limited, but I will be fixing that soon! ",
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
                "value":" If you want to look at everything on the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"global",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object, you can do so while debugging.  You can debug Node.js by running the following command and navigating to the link given: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"node --inspect --debug-brk test.js\n",
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
                "value":" All the code from this discovery can be found ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-submittions/blob/master/Discoveries/2017/\n11-Nov/11-13-JS-Global-Object/Source"
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

db.posts.remove({name: "nov-13-2017-js-global-object"});

db.posts.insertOne({
    name: "nov-13-2017-js-global-object",
    title: "Global Objects in JavaScript",
    date: new Date('2017-11-13T12:00:00'),
    type: "Discovery",
    tags: [
        {
            name: "JavaScript",
            picture: "./assets/js.png",
            color: "javascript"
        },
        {
            name: "Node.js",
            picture: "./assets/nodejs.png",
            color: "nodejs"
        },
        {
            name: "Document Object Model"
        }
    ],
    content,
    sources: [
        {
            startName: "\"Window\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/API/Window",
            link: "https://developer.mozilla.org/en-US/docs/Web/API/Window"
        },
        {
            startName: "John Resig, Bear Bibeault, & Josip Maras, ",
            endName: " (Shelter Island, NY: Manning, 2016), 26",
            linkName: "Secrets of the JavaScript Ninja",
            link: "https://www.manning.com/books/secrets-of-the-javascript-ninja-second-edition"
        },
        {
            startName: "\"EventTarget.addEventListener()\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener",
            link: "https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener"
        }
    ]
});