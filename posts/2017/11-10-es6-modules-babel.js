/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/25/2018
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
                "value":" In my last discovery I went over the ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/nov-9-2017-closure-modules"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"revealing module pattern in JavaScript.",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" Now I will recreate the same API with ES6 modules.  The API code will look familiar: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"export function lyrics(song) {\n    switch(song.toLowerCase()) {\n    case \"sparks fly\":\n        return lyric.sparks;\n        case \"mine\":\n        return lyric.mine;\n    case \"back to december\":\n        return lyric.dec;\n    default:\n        return lyric.other;\n    }\n}\n\nvar lyric = {\n    sparks: \"I see sparks fly whenever you smile\",\n    mine: \"You are the best thing thats ever been mine\",\n    dec: \"I go back to Decemeber all the time\",\n    other: \"I'm sorry, taylor can't pick up the phone right now\"\n}\n",
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
                "value":" The only difference with our API is the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"export",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keyword. This new keyword in ES6 will reveal the function to other JavaScript code that imports this module.  Each JavaScript file can be a module if it exports functions/variables, but there can’t be more than one module in a file.  You can however have multiple exports.  Since one file is one and only one module, the name of the module is the filename. Now let’s check out the module import code: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"import * as ts from './taylorSwift';\n",
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
                "value":" Here we ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"import *",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" (all variables/functions in the module) from the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"taylorSwift",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" module and give it the alias ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ts",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". We can then call the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ts.lyrics()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"console.info(ts.lyrics(\"Sparks Fly\"));\n",
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
                "value":" If you tried running this code in many environments it would fail.  Many JavaScript engines are not ES6 compliant (including my version of Node.js!).  Therefore when I try to run my main.js file (containing the input code) I get an error: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"node main.js\n#import * as ts from 'taylorSwift';\n#^^^^^^\n#SyntaxError: Unexpected token import\n",
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
                "value":" Okay so that didn’t work.  We can however get this code to work by jumping through some hoops. The tool to get the job done is Babel, and it is a JavaScript transpiler. ",
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
                "value":" The way we are going to get our ES6 code to work in all environments is by converting the code to its ES5 equivalent (or as close as possible).  This would be quite the task to do manually for all our code, so we let Babel do the work for us.  Transpile is a combination of the words translate and compile.  So Babel (the compiler) converts (translates) our ES6 code to ES5. ",
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
                "value":" Let’s get to work.  Since I’m using Node.js I need to set up my dependency manager and install Babel",
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
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"npm install --save-dev babel-cli\nnpm install babel-preset-env --save-dev\n",
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
                "value":" I then need to configure Babel by creating a .babelrc file.  In this JSON file I set “presets” to “env” which lets Babel automatically determine the Babel plugins I need to perform the transpiling based on my environment",
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
        "el":"codesnippet",
        "attributes":{
            "language":"JSON"
        },
        "value":"{\n  \"presets\": [\"env\"]\n}\n",
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
                "value":" Now when I got to this point I thought “great I’m done! Let’s run the code!”  Unfortunately I forgot to actually tell Babel which files to convert to ES5.  One way to do this is like so: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"npx babel main.js --out-file es5/main.js\n",
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
                "value":" I am telling Babel to take main.js and transpile it to a file of the same name in a folder called es5 (note: ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"npx",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is not a typo – install it with ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"npm install –g npx",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" to simplify Babel commands). ",
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
                "value":" There is however a better way of doing this.  You can actually ask Babel to watch your ES6 files for changes. When a change is made, the code is automatically transpiled to the output file. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"npx babel main.js --watch --out-file es5/main.js &\nnpx babel taylorSwift.js --watch --out-file es5/taylorSwift.js &\n",
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
                "value":" You can see I added the ampersand to the end of these commands to place these tasks in the background. This is so the bash shell will not be blocked and I can continue to execute commands in one window.  You can kill these background tasks at any time with their job number",
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
                "value":": ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"# Kill the task by job number (in this case 1)\nkill %1\n",
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
                "value":" Now we finally did it! ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"node es5/main.js\n# I see sparks fly whenever you smile\n",
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
                "value":" If we look at the transpiled ES5 code you can see that Babel is using CommonJS for the modules.  This is the library that Node.js uses for modules in pre-ES6 code. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"var _taylorSwift = require(\"./taylorSwift\");\n",
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
                "value":" Whew that was a lot of work to just see some sparks fly using ES6!  Obviously it is not practical to transpile ES6 code for such a small codebase, but for larger projects you don’t want to be held back from newer JavaScript versions because of the environment you are using.  I will be using Babel for many projects in the future.  You can find all the code in this demo ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/\njarombek-com-sources/tree/master/2017/11-Nov/11-10-ES6-Modules-Babel"
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

postViews = db.posts.findOne({name: "nov-10-2017-es6-modules-babel"}).views;

db.posts.remove({name: "nov-10-2017-es6-modules-babel"});

db.posts.insertOne({
    name: "nov-10-2017-es6-modules-babel",
    title: "ES6 Modules Run with Babel",
    date: new Date('2017-11-10T12:00:00'),
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
        {
            name: "Node.js",
            picture: "./assets/nodejs.png",
            color: "nodejs"
        },
        {
            name: "Babel",
            picture: "./assets/babel.png",
            color: "babel"
        },
        {
            name: "Transpiler"
        }
    ],
    content,
    sources: [
        {
            startName: "\"Using Babel\", ",
            endName: "",
            linkName: "https://babeljs.io/docs/setup/#installation",
            link: "https://babeljs.io/docs/setup/#installation"
        },
        {
            startName: "\"Env preset\", ",
            endName: "",
            linkName: "https://babeljs.io/docs/plugins/preset-env/",
            link: "https://babeljs.io/docs/plugins/preset-env/"
        },
        {
            startName: "\"linux: kill background task\", October 26th, 2009, ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/1624691/linux-kill-background-task",
            link: "https://stackoverflow.com/questions/1624691/linux-kill-background-task"
        }
    ]
});