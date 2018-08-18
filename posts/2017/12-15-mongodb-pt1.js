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
                "value":" Continuing the trend of learning more about JavaScript, today I will be looking at MongoDB. Mongo is a NoSQL database which stores data in documents.  Before we get into the code, it is important to question why to even use MongoDB in the first place. ",
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
                "value":" First lets look at some of the positive aspects of MongoDB.  The database stores its data as JSON (and internally as BSON - short for Binary JSON) which in my personal opinion is the best structure for transferring data.  In MongoDB the BSON implementation makes this data extremely lightweight",
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
                "value":". If you are using a JavaScript web stack, MongoDB allows you to even use JavaScript in your database instead of SQL.  The query language of MongoDB is also JavaScript, which means if you have knowledge in JavaScript using MongoDB will be an easy transition.  You can even use JavaScript functions and variables to perform complex queries and database updates! ",
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
                "value":" Continuing the trend of learning more about JavaScript, today I will be looking at MongoDB. Mongo is a NoSQL database which stores data in documents.  Before we get into the code, it is important to question why to even use MongoDB in the first place. ",
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
                "value":" First lets look at some of the positive aspects of MongoDB.  The database stores its data as JSON (and internally as BSON - short for Binary JSON) which in my personal opinion is the best structure for transferring data.  In MongoDB the BSON implementation makes this data extremely lightweight",
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
                "value":". If you are using a JavaScript web stack, MongoDB allows you to even use JavaScript in your database instead of SQL.  The query language of MongoDB is also JavaScript, which means if you have knowledge in JavaScript using MongoDB will be an easy transition.  You can even use JavaScript functions and variables to perform complex queries and database updates! ",
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
                "value":" As far as negatives are concerned, MongoDB does not have transaction support (similarly to many NoSQL databases).  MongoDB does support atomic writes for a single document (JavaScript object equivalent of a row of a table in a RDBMS)",
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
                "value":".  In other words, while you are updating a document either the entire update takes place, or no changes occur.  Other database connections will never see an update in progress, it will either have not started or be fully completed",
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
                "value":".  This is however where MongoDB's ACID transaction properties end.  This can cause issues if you need true transactions in your database.  Another drawback (in my opinion) for MongoDB is the lack of SQL.  Using JavaScript for querying is nice, but SQL is at the top for query languages.  Luckily if you know SQL and JavaScript the learning curve for querying MongoDB is small. ",
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
                "value":" One thing about MongoDB that can be seen as a positive or negative is its schema-less design.  This allows you to have a variable number of properties in a document and to add and remove properties whenever you like.  This allows for a lot of flexibility with changing requirements and evolving object state.  It also debatably makes it harder to create a well structured database.  With more power comes more responsibility! ",
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
                "value":" Now let's get started creating a MongoDB database that represents different Christmas trees.  Once you have MongoDB installed, you can run the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"mongo",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" command in bash to start up the database and CLI.  It is important to know that in MongoDB there are three main logical groupings; databases, collections, and documents.  Databases are not the same as a traditional RDBMS database connection - they are simply groupings (namespaces) for collections and documents",
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
                "value":". Collections are the way we group documents together in MongoDB, similar to tables in an RDBMS. ",
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
                "value":" To start, let's make a new database for our Christmas trees.  To change databases we can use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"use <database>",
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
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"use xmas\n",
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
                "value":" This command does not actually create the database yet, first we need to add some collections and documents. Let's add a document to a collection we will call tree: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.insert({type: \"balsam\"})\n",
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
                "value":" You can see the document JSON is passed into the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"insert()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function on the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"tree",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" collection object.  If we want to view all the documents in the tree collection, we can use the collections ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"find()",
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
        "value":"db.tree.find().pretty()\n",
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
                "value":" This code sample also shows that we can chain functions.  The second function ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"pretty()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" will format the JSON in an easily readable fashion. ",
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
                "value":" Let's say we want to add more properties to our new tree document.  Since MongoDB collections have no schema, we can add any properties that we want! ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.update({type: \"balsam\"}, {\n    $set: {\n        height: \"6'11\\\"\",\n        source_price: 10.50,\n        sell_price: 45.00,\n        grade: \"6-7ft\"\n    }\n})\n",
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
                "value":" We use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"update()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function on the collection to edit a document.  The first parameter will query the collection and match with a document, and the second parameter defines what to update.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$set",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property will set the specified properties in the document to the given values",
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
                "value":" With these code samples you can start to imagine the possibilities for using JavaScript to query MongoDB. However performing complex JavaScript code in the CLI gets quite difficult.  Thankfully MongoDB allows us to edit any variable or function in our favorite text editor.  In my case I will be using Vim.  To get this to work we first have to change the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"EDITOR",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"vim",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in bash. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"export EDITOR=vim\n",
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
                "value":" Now in the MongoDB shell we can define a function that we will edit in vim: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"function createTree() {}\n",
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
                "value":" Using the name of this function, we can begin editing it in vim with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"edit",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" command: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"edit createTree\n",
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
                "value":" Since MongoDB queries use JavaScript, we can use everything in the JavaScript language (even ES6+ features!) in our function.  I will edit the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"createTree",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function so that it inserts another document into the tree collection.  When the function is edited to our liking, write and quit vim. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"function createTree() {\n    db.tree.insert({\n    type: \"frazier\",\n    height: \"6'1\\\"\",\n    source_price: 14.00,\n    sell_price: 45.00,\n    grade: \"6-7ft\"\n    })\n}\n",
        "children":null
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":":wq\n",
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
                "value":" Now back in the MongoDB CLI we can view the functions contents by typing ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"createTree",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and run the code in the function by typing ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"createTree()",
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
                "value":" Now our tree collection contains two documents.  Let's say we want to add two properties to our existing documents, ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"sold",
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
                "value":"buyer_id",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  We could use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"update()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function, however this function only matches with the first document it finds.  To match all the documents that fit the query conditions, we must use ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"updateMany()",
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
        "value":"db.tree.updateMany({}, {\n    $set: {\n        \"sold\": false,\n        \"buyer_id\": undefined\n    }\n})\n",
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
                "value":" Finally to show the power of using JavaScript functions in MongoDB, I will create a function that bulk inserts random trees into our database. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"function bulkTreeInsert(number=1) {\n    let types = [\n        {type: \"frazier\", grade: \"3-4ft\", feet: 3, source: 7, sell: 25},\n        {type: \"frazier\", grade: \"4-5ft\", feet: 4, source: 7.50, sell: 30},\n        {type: \"frazier\", grade: \"5-6ft\", feet: 5, source: 8, sell: 45},\n        {type: \"frazier\", grade: \"6-7ft\", feet: 6, source: 9.50, sell: 55},\n        {type: \"frazier\", grade: \"7-8ft\", feet: 7, source: 10.50, sell: 65},\n        {type: \"frazier\", grade: \"8-9ft\", feet: 8, source: 12, sell: 85},\n        {type: \"frazier\", grade: \"9-10ft\", feet: 9, source: 15, sell: 115},\n        {type: \"frazier\", grade: \"10+ft\", feet: 10, source: 20, sell: 140},\n        {type: \"balsam\", grade: \"5-6ft\", feet: 5, source: 7, sell: 30},\n        {type: \"balsam\", grade: \"6-7ft\", feet: 6, source: 8, sell: 40},\n        {type: \"balsam\", grade: \"7-8ft\", feet: 7, source: 9, sell: 50},\n        {type: \"balsam\", grade: \"8-9ft\", feet: 8, source: 10, sell: 65},\n        {type: \"balsam\", grade: \"9-10ft\", feet: 9, source: 11.50, sell: 80},\n        {type: \"douglas\", grade: \"5-6ft\", feet: 5, source: 7.50, sell: 40},\n        {type: \"douglas\", grade: \"6-7ft\", feet: 6, source: 8.50, sell: 50},\n        {type: \"douglas\", grade: \"7-8ft\", feet: 7, source: 10, sell: 60}\n    ];\n\n    for (let i = 0; i < number; i++) {\n\n        // Get a random index in the types array\n        let random = Math.floor(Math.random() * 16);\n\n        // Get a random number for inches between 0-11\n        let inches = Math.floor(Math.random() * 11);\n\n        let selected = types[random];\n        let tree = {\n            type: selected[\"type\"],\n            height: `${selected[\"feet\"]}' ${inches}\"`,\n            source_price: selected[\"source\"],\n            sell_price: selected[\"sell\"],\n            grade: selected[\"grade\"],\n            sold: false,\n            buyer_id: undefined\n        };\n\n        db.tree.insert(tree)\n    }\n}\n",
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
                "value":" We can pass ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"1000",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" as a parameter to this function, effectively calling it one thousand times.  When we check to see how many trees are in our tree collection, we will get 1003 as expected. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"bd.tree.count() // 1003\n",
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
                "value":" Since the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":null,
                "value":"find()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function also accepts JSON, we can count all trees of type Frazier and height between 6-7 feet: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.find({type:\"frazier\", grade:\"6-7ft\"}).count()\n",
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
                "value":" With knowledge of JavaScript and query languages, MongoDB can be an extremely powerful database.  I have only scratched the surface here, but there we will many more discoveries to come!  You can find all the code from this discovery ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/\nmaster/2017/12-Dec/12-15-MongoDB-Pt1"
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

postName = "dec-15-2017-mongodb-pt1";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Learning MongoDB Part I: Creating the Database",
    date: new Date('2017-12-15T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "MongoDB",
            picture: "https://asset.jarombek.com/logos/mongodb.png",
            color: "mongodb"
        },
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        },
        {
            name: "Vim",
            picture: "https://asset.jarombek.com/logos/vim.png",
            color: "vim"
        },
        {
            name: "Relational Database"
        },
        {
            name: "NoSQL"
        },
        {
            name: "Document Database"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "\"BSON\", ",
            endName: "",
            linkName: "http://bsonspec.org",
            link: "http://bsonspec.org"
        },
        {
            startName: "\"Atomicity and Transactions\", ",
            endName: "",
            linkName: "https://docs.mongodb.com/manual/core/write-operations-atomicity/",
            link: "https://docs.mongodb.com/manual/core/write-operations-atomicity/"
        },
        {
            startName: "\"Atomicity (database systems)\", ",
            endName: "",
            linkName: "https://en.wikipedia.org/wiki/Atomicity_(database_systems)",
            link: "https://en.wikipedia.org/wiki/Atomicity_(database_systems)"
        },
        {
            startName: "Kyle Banker, Peter Bakkum, Shaun Verch, Douglas Garrett &amp; Tom Hawkins, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2016), 31",
            linkName: "MongoDB In Action",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "",
            endName: "., 34",
            linkName: "Ibid",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content, 
    contentString: JSON.stringify(content) 
});