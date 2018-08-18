/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/29/2018
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
                "value":" My MEAN stack prototype was the first personal project that used Continuous Integration (CI). Continuous Integration is great and has made me realize that I really should use it on all future projects. ",
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
                "value":" So what is CI?  Continuous Integration is the practice of merging code being developed into a main repository often.  On each of these merges, testing suites and builds should be executed automatically by a tool separate from the main codebase",
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
                "value":".  This allows for early detection of bugs and helps avoid the nightmare of backtracking through many commits to see where some functionality broke.  Now each time a commit is done with CI the developers will know if any existing code breaks.  Of course this requires some thorough testing suites to be created (something I often cut corners on admittedly). ",
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
                "value":" My MEAN stack prototype was the first personal project that used Continuous Integration (CI). Continuous Integration is great and has made me realize that I really should use it on all future projects. ",
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
                "value":" So what is CI?  Continuous Integration is the practice of merging code being developed into a main repository often.  On each of these merges, testing suites and builds should be executed automatically by a tool separate from the main codebase",
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
                "value":".  This allows for early detection of bugs and helps avoid the nightmare of backtracking through many commits to see where some functionality broke.  Now each time a commit is done with CI the developers will know if any existing code breaks.  Of course this requires some thorough testing suites to be created (something I often cut corners on admittedly). ",
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
                "value":" So yes, I have a bad habit of thinking about unit/integration testing as an afterthought.  Like any bad habit, it is time to start breaking it with small baby steps!  The first of those steps includes setting up each project with a CI service, and in this case I chose TravisCI because of how easily it integrates with GitHub repositories! ",
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
                "value":" After you have signed up for ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://travis-ci.org/"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"TravisCI",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and enabled its support for certain repositories, GitHub will notify Travis each time new commits are pushed",
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
                "value":"!  I set up TravisCI for both the client side and server side of my MEAN stack prototype. Let’s take a look at the configuration file (named .travis.yml) and what it does for each project! ",
                "children":null
            }
        ]
    },
    {
        "el":"h5",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"Angular 5 Client",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"YAML"
        },
        "value":"sudo: required\ndist: trusty\n\naddons:\n  apt:\n    source:\n        - google-chrome\n    packages:\n        - google-chrome-stable\n\nlanguage: node_js\nnode_js:\n    - '9.2'\n    - '8.0'\n\nbefore_install:\n    - export CHROME_BIN=chromium-browser\n    - export DISPLAY=:99.0\n    - sh -e /etc/init.d/xvfb start\n\nbefore_script:\n    - npm install -g @angular/cli\n\nscript:\n    - cd web-app\n    - npm install\n    - ng test --single-run --watch=false --code-coverage\n    - npm run build\n\nafter_script:\n    - cat ./coverage/lcov.info | ./node_modules/coveralls/bin/coveralls.js\n\nnotifications:\n  email:\n    recipients:\n        - ajarombek95@gmail.com\n    on_success: always\n    on_failure: always\n",
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
                "value":" So let’s break this file down.  You can see that we set the language being used to a Node.js flavor of JavaScript ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"language: node_js",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and we will run the tests on three different Node.js versions.  I did this to test out how backwards compatible my project was!  Even though the project is an Angular frontend and not Node, I guess specifying the language as Node.js basically tells Travis \"This project uses npm.\" ",
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
                "value":" The Angular configuration was a bit more complex than the server side config because of some complexities thanks to Angular CLI.  For whatever reason, you have to install Chrome in the build in order to get the Karma tests to run properly",
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
                "value":" Accomplishing this requires you to use the apt add-on.  This allows us to provide commands to the apt-get package handler for installation",
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
                "value":".  In this case I specify that I want to install ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"google-chrome",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" from the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"google-chrome-stable",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" package. ",
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
                "value":" Before the script is run I tell TravisCI to install AngularCLI (",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"npm install -g @angular/cli",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":").  This will allow TravisCI to use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ng",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" commands in the scripts.  Then in my scripts, I specify that the tests should be run and that a build should be created.  Both of these scripts succeeding defines the success criteria.  Finally, code coverage statistics are sent to ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://coveralls.io/"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"Coveralls",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" , another really cool tool that easily integrates with GitHub. ",
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
                "value":" After TravisCI finishes our scripts, there is an option to send notifications about the build details.  I decided to have emails sent out each time a build finished, no matter if it succeeded or failed.  Notifications are very easily configurable to fit your needs. ",
                "children":null
            }
        ]
    },
    {
        "el":"h5",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"Node.js & Express Server",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"YAML"
        },
        "value":"language: node_js\nnode_js:\n    - '9.2'\n    - '8.0'\n\nservices:\n    - mongodb\n\n# Put some mock data into MongoDB for testing purposes\n# https://docs.travis-ci.com/user/database-setup/#MongoDB\nbefore_script:\n    - mongo meowcat --eval 'db.user.insert({_id: '5a9616e6c5631b2e8bd0bd5e',\n                                            username: \"andy\",\n                                            first: \"Andrew\",\n                                            last: \"Jarombek\",\n                                            password: \"pw\",\n                                            postCount: 4});'\n\nscript:\n    - npm run test\n\nafter_success:\n    - npm run coveralls\n\nnotifications:\n  email:\n    recipients:\n        - ajarombek95@gmail.com\n    on_success: always\n    on_failure: always\n",
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
                "value":" Just by glancing at this code you will see that not much has changed from the Angular frontend TravisCI configuration.  Node.js is used as the language, a test script is run, and email notifications are sent out on success and failure. ",
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
                "value":" One big difference is the use of a service specification.  I tell TravisCI to create a new MongoDB service and then I give it an insert statement to execute before running my tests",
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
                "value":".  This is incredibly cool!  Since the Express app consists of REST endpoints that make calls to MongoDB, we can actually expect endpoints to return exact mock data based on the TravisCI config!  Here is some code in Node.js that runs a test based on the insert statement specified in the TravisCI configuration: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"const request = require('supertest');\nconst app = require('../src/app');\n\n// Tests for the user api endpoint '/api/user'\ndescribe(\"GET '/api/user'\", () => {\n\n    /**\n     * This test will be successful when run in TravisCI since it is based off mocked data\n     * used there.  It will not (most likely) work in a development environment where the\n     * database has different user documents.\n     */\n    it('responded with correct JSON from database', () => {\n        return request(app).get('/api/user')\n            .expect(200, [\n                {\n                    _id: '5a9616e6c5631b2e8bd0bd5e',\n                    username: \"andy\",\n                    first: \"Andrew\",\n                    last: \"Jarombek\",\n                    password: \"pw\",\n                    postCount: 4\n                }]);\n    });\n});\n",
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
                "value":" Now that is powerful stuff!  This is the kinda stuff that makes me realize I need more unit testing in my applications.  The first baby step has been taken, by my next application hopefully I will have a full fledged test suite with near complete code coverage!  I will be using TravisCI in my projects for the foreseeable future. ",
                "children":null
            }
        ]
    }
];

postName = "mar-9-2018-travisci";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "What I have Learned About TravisCI",
    date: new Date('2018-03-09T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "TravisCI",
            picture: "https://asset.jarombek.com/travisci.png",
            color: "travisci"
        },
        {
            name: "Continuous Integration"
        },
        {
            name: "YAML",
            picture: "https://asset.jarombek.com/logos/yaml.png",
            color: "yaml"
        },
        {
            name: "Angular",
            picture: "https://asset.jarombek.com/logos/angular.png",
            color: "angular"
        },
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
            name: "MongoDB",
            picture: "https://asset.jarombek.com/logos/mongodb.png",
            color: "mongodb"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"Continuous Integration\", ",
            endName: "",
            linkName: "https://www.thoughtworks.com/continuous-integration",
            link: "https://www.thoughtworks.com/continuous-integration"
        },
        {
            startName: "\"Travis CI\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Travis_CI",
            link: "https://en.wikipedia.org/wiki/Travis_CI"
        },
        {
            startName: "\"Angular 2 with Travis CI\", ",
            endName: "",
            linkName: "https://medium.com/from-the-couch/angular-2-with-travis-ci-922040e01937",
            link: "https://medium.com/from-the-couch/angular-2-with-travis-ci-922040e01937"
        },
        {
            startName: "\"Installing Packages with the APT Addon\", ",
            endName: "",
            linkName: "https://docs.travis-ci.com/user/installing-dependencies/#Installing-Packages-with-the-APT-Addon",
            link: "https://docs.travis-ci.com/user/installing-dependencies/#Installing-Packages-with-the-APT-Addon"
        },
        {
            startName: "\"Setting up Databases: MongoDB\", ",
            endName: "",
            linkName: "https://docs.travis-ci.com/user/database-setup/#MongoDB",
            link: "https://docs.travis-ci.com/user/database-setup/#MongoDB"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content, 
    contentString: JSON.stringify(content) 
});