/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 5/26/2018
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
                "value":" Recently while working on my website I made a command line tool in Node.js.  The tool took in an HTML file and tokenized its contents into a JSON file.  Writing command line tools with Node.js sounded complicated at first but is actually incredibly simple!  This discovery post will introduce a simple command line tool for generating random numbers - written in Node.js. ",
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
                "value":" One of the great things about the Node.js and the npm ecosystem is all the community built npm modules at a developers disposal.  For command line tools there are many different modules to choose from.  In this discovery post I will be using ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"“https://github.com/tj/commander.js/”"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"commander.js",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  The reason I chose this module over all the others it is has no dependencies!  Less dependencies generally means more reliable code and less unused code. ",
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
                "value":" Recently while working on my website I made a command line tool in Node.js.  The tool took in an HTML file and tokenized its contents into a JSON file.  Writing command line tools with Node.js sounded complicated at first but is actually incredibly simple!  This discovery post will introduce a simple command line tool for generating random numbers - written in Node.js. ",
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
                "value":" One of the great things about the Node.js and the npm ecosystem is all the community built npm modules at a developers disposal.  For command line tools there are many different modules to choose from.  In this discovery post I will be using ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"“https://github.com/tj/commander.js/”"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"commander.js",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  The reason I chose this module over all the others it is has no dependencies!  Less dependencies generally means more reliable code and less unused code. ",
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
                "value":" The first step for building the application is initializing the projects package.json file and setting up the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"bin",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" field. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"yarn init\n",
        "children":null
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JSON"
        },
        "value":"{\n    \"name\": \"5-26-NodeJS-Command-Line\",\n    \"version\": \"1.0.0\",\n    \"description\": \"Node.JS Command Line Prototype Application\",\n    \"main\": \"index.js\",\n    \"author\": \"Andrew Jarombek\",\n    \"license\": \"MIT\",\n    \"bin\": {\n        \"random\": \"./index.js\"\n    }\n}\n",
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
                "value":" The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"bin",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" field is an object of key value pairs.  The key is the name of a command and the value is the file that will be executed when the command is called.  Npm takes the key value pairs in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"bin",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and adds the executable files (the values) to the systems ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"PATH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable",
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
                "value":".  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"PATH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable in Unix systems declares a list of directories for executable programs",
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
                "value":".  In order to install this executable globally the following command must be run: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"npm install -g\n",
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
                "value":" Now the executable can be referenced globally throughout the filesystem by running ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"random",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in Bash.  Finally the commander.js dependency is added and the JavaScript development can start! ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"yarn add commander\n",
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
                "value":" Commander.js introduces a really nice API for building command line apps.  The API allows you to chain together methods to declare the applications functionality.  Let’s go through the Node.js application piece by piece before displaying the entire application in full. ",
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
                "value":" The first line of the script must be a shebang line in order for the command line application to work. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"#!/usr/bin/env node --harmony\n",
        "children":null
    },
    {
        "el":"definition",
        "attributes":{
            "word":"Shebang Line"
        },
        "value":null,
        "children":[
            {
                "el":"p",
                "attributes":null,
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" A shebang line is written to determine which interpreter will be used on an executable file",
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
                        "value":".  The syntax is the ",
                        "children":null
                    },
                    {
                        "el":"code",
                        "attributes":{
                            "class":"jarombek-inline-code"
                        },
                        "value":"#!",
                        "children":null
                    },
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" symbols followed by the path to the interpreter script.  The shebang line is used on Unix operating systems, but in different OS environments other methods are used.  For example, on Windows the interpreter is simply implied by the file name extension. ",
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
                        "value":" Shebang lines are commonly used with the format ",
                        "children":null
                    },
                    {
                        "el":"code",
                        "attributes":{
                            "class":"jarombek-inline-code"
                        },
                        "value":"#!/usr/bin/env",
                        "children":null
                    },
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" followed by the interpreter name.  This sequence will use the Unix ",
                        "children":null
                    },
                    {
                        "el":"code",
                        "attributes":{
                            "class":"jarombek-inline-code"
                        },
                        "value":"env",
                        "children":null
                    },
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" command to look for the interpreter name in the systems ",
                        "children":null
                    },
                    {
                        "el":"code",
                        "attributes":{
                            "class":"jarombek-inline-code"
                        },
                        "value":"PATH",
                        "children":null
                    },
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" variable",
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
                "value":" While the shebang line is only needed for unix systems, it will always be included for cross compatibility. Windows OS will simply ignore the line",
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
                "value":".  Also, note that this line isn’t actually valid JavaScript - ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"#",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" are not comments in JavaScript.  The JavaScript interpreter will actually ignore this line when executing the program. ",
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
                "value":" Also note that I used the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"--harmony",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" flag when specifying the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"node",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" interpreter.  This will enable the use of ES6+ features. ",
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
                "value":" Now comes the main body of the application.  I import the commander.js module and declare the command line program: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"const program = require('commander');\n\n// Help message to be displayed when using the --help option\nconst helpMessage = `\n  Examples:\n    \"Return a random floating point number between 0 and 1\"\n    random 0 1 -t float\n\n    \"Return a random integer number between 1 and 10\"\n    random 1 10 -t int\n`;\n\n// Declare the command line program\nprogram\n    .version('1.0')\n    .arguments('<start> <end>')\n    .option('-t, --type <type>', 'Type of Number')\n    .action((start, end) => {\n        ...\n    })\n    .on('--help', () => {\n        console.log(helpMessage)\n    })\n    .parse(process.argv);\n",
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
                "value":" The application is broken down into pieces - each piece being a chained method call.  The first method call is to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"version()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", which specifies a version number that will be displayed when using the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"random --version",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" command. ",
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
                "value":" The seconds method call is to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"arguments()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", which specifies mandatory arguments users must enter when writing a command.  Two arguments are specified here - ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<start>",
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
                "value":"<end>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". Remember that the command line app is for generating random numbers.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"start",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" argument specifies the smallest number that the random number can be, and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"end",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" specifies the largest number it can be.  Later in the method chain we can access these two arguments with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"start",
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
                "value":"end",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variables respectively. ",
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
                "value":" Unlike ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"arguments()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" which specifies mandatory parameters, ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"option()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" specifies an optional flag and value that can be added to the command.  This will provide additional functionality to the application.  The code above specifies an option that can be declared with either the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"-t",
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
                "value":"--type",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" flags followed by a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<type>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" argument.  This option can be sued to specify if the random number should be an integer or a floating point number. ",
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
                "value":" Options can also be accessed later on as variables in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"action()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method call.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<type>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" option is stored in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"program.type",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable. I will go over ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"action()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in just a bit. ",
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
                "value":" The final two methods in the chain specify what text to display when calling ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"random --help",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" along with the fact that the programs arguments are coming from the command line arguments (",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"process.argv",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"). ",
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
                "value":" The final piece is the core of the application - what logic to perform based on the command line arguments.  This business logic is executed in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"action()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":".action((start, end) => {\n\n    // Log out the arguments and optional options\n    console.log(`Start: ${start}`);\n    console.log(`End: ${end}`);\n    console.log(`Type: ${program.type}`);\n\n    // Generate the random number\n    const range = end - start;\n\n    const random = Math.random();\n    const randomInRange = random * range;\n    const randomInRangeFromStart = randomInRange + start;\n\n    // Round the random number if the int option is specified\n    if (program.type && (program.type.toLowerCase() === 'int' ||\n        program.type.toLowerCase() === 'integer')) {\n\n        console.log(Math.round(randomInRangeFromStart));\n    } else {\n        console.log(randomInRangeFromStart);\n    }\n\n})\n",
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
                "value":" The code here is straightforward.  Return either an integer or floating point random number within the bounds specified in the command line arguments.  That is all! ",
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
                "value":" Now the program can be executed like so: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"random --help\n\nrandom 0 1\n\n# Start: 0\n# End: 1\n# Type: undefined\n# 0.407353929118857040\n\nrandom 1 10 -t int\n\n# Start: 0\n# End: 10\n# Type: int\n# 4\n",
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
                "value":" Easy!  The command line application I built for my website has a bit more complexity, but even that is simple and easy to understand.  You can check out the code for that app on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/html-tokenizer"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"GitHub",
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
                "value":" There are many different languages you can build simple command line applications in.  Now you know that JavaScript running in the Node.js environment is one of them! Here is the full code for the application: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"#!/usr/bin/env node --harmony\n\nconst program = require('commander');\n\n// Help message to be displayed when using the --help option\nconst helpMessage = `\n  Examples:\n    \"Return a random floating point number between 0 and 1\"\n    random 0 1 -t float\n\n    \"Return a random integer number between 1 and 10\"\n    random 1 10 -t int\n`;\n\n// Declare the command line program\nprogram\n    .version('1.0')\n    .arguments('<start> <end>')\n    .option('-t, --type <type>', 'Type of Number')\n    .action((start, end) => {\n\n    // Log out the arguments and optional options\n    console.log(`Start: ${start}`);\n    console.log(`End: ${end}`);\n    console.log(`Type: ${program.type}`);\n\n    // Generate the random number\n    const range = end - start;\n\n    const random = Math.random();\n    const randomInRange = random * range;\n    const randomInRangeFromStart = randomInRange + start;\n\n    // Round the random number if the int option is specified\n    if (program.type && (program.type.toLowerCase() === 'int' ||\n        program.type.toLowerCase() === 'integer')) {\n\n        console.log(Math.round(randomInRangeFromStart));\n    } else {\n        console.log(randomInRangeFromStart);\n    }\n\n    })\n    .on('--help', () => {\n        console.log(helpMessage)\n    })\n    .parse(process.argv);\n",
        "children":null
    }
];

postName = "may-26-2018-nodejs-command-line";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "A Simple Node.js Command Line Application",
    date: new Date('2018-05-26T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Node.js",
            picture: "https://asset.jarombek.com/logos/nodejs.png",
            color: "nodejs"
        },
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        },
        {
            name: "Command Line Application"
        }
    ], 
    preview,
    sources: [
        {
            startName: "\"bin\", ",
            endName: "",
            linkName: "https://docs.npmjs.com/files/package.json#bin",
            link: "https://docs.npmjs.com/files/package.json#bin"
        },
        {
            startName: "\"PATH (variable)\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/PATH_(variable)",
            link: "https://en.wikipedia.org/wiki/PATH_(variable)"
        },
        {
            startName: "\"Shebang (Unix)\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Shebang_(Unix)",
            link: "https://en.wikipedia.org/wiki/Shebang_(Unix)"
        },
        {
            startName: "\"env\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Env",
            link: "https://en.wikipedia.org/wiki/Env"
        },
        {
            startName: "\"What exactly does “/usr/bin/env node” do at the beginning of node files?\", ",
            endName: "",
            linkName: "https://stackoverflow.com/a/33510581",
            link: "https://stackoverflow.com/a/33510581"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});