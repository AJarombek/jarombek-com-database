/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/28/2018
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
                "value":" One of the really powerful things about a document database like MongoDB is the ability to store arrays of values in a document.  This array itself can even store sub-documents, allowing for many levels of nested data. ",
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
                "value":" An example of an array in a document being useful is a user that has multiple addresses.  In a relational database, this address information would have to be stored in a separate table that could be joined on the user table to get all the addresses.  In MongoDB, this can all be stored in one document which makes accessing quick and easy (no expensive ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"JOIN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operations) as well as simplifying updating. ",
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
                "value":" One of the really powerful things about a document database like MongoDB is the ability to store arrays of values in a document.  This array itself can even store sub-documents, allowing for many levels of nested data. ",
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
                "value":" An example of an array in a document being useful is a user that has multiple addresses.  In a relational database, this address information would have to be stored in a separate table that could be joined on the user table to get all the addresses.  In MongoDB, this can all be stored in one document which makes accessing quick and easy (no expensive ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"JOIN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operations) as well as simplifying updating. ",
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
                "value":" Now let's look at some operations on arrays from the MongoDB shell.  I created a document called ",
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
                "value":" which will hold running log information.  This info consists of an array of exercise logs, an array of planned exercises, and the users personal records. ",
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
                "value":" Accessing data from nested objects and arrays is easy.  Lets say we have a plan array that contains exercise plans.  These plans contain two properties - a date and a distance in miles.  If we want to just return the miles and not the date, we can do this using dot notation - ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"plan.miles",
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
        "value":"db.user.find({}, {'plan.miles': 1}).pretty()\n",
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
                "value":" If you wanted to just return the first item in the plan array to see the next run you have to go on, you can use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$slice",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operator.  ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$slice",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" allows you to determine a number of items to return from an array. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.user.find({}, {_id: 1, plan: {$slice: 1}}).pretty()\n",
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
                "value":" We pass ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$slice",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" a value of 1 which says to only return the first element from the array.  You can give ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$slice",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" a negative value to return from the end of the array. ",
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
                "value":"     Like any property, you can put an index on a nested property using dot notation: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.user.createIndex({'plan.date': 1})\n",
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
                "value":" You can update an array with numerous operators including ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$push",
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
                "value":"$pop",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$addToSet",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$pop",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" removes an item from the array, while both ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$push",
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
                "value":"$addToSet",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" add an item to the array.  The difference is with ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$addToSet",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" the new item will only be added if it does not already exist in the array.  This is a safer option if we want to avoid duplicates. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"// Delete the first element of the plan array\ndb.user.update({}, {$pop: {'plan': -1}})\n\n// Add an element to the array if it does not already exist\ndb.user.update({}, {$addToSet: {'plan': {date: new Date(\"2017-12-27\"), miles: 2}}})\n",
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
                "value":" One important operator for updating in MongoDB is ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"findAndModify()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". This command allows you to both update a document and return the new document atomically",
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
                "value":".  All updates in MongoDB are performed atomically (an update is never half completed and no database connection ever sees an update in progress).  However, when you do ",
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
                "value":" and ",
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
                "value":" operations separately, there is a chance that another update is performed in between the execution of both commands.  ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"findAndModify()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" eliminates this problem so you know any changes reflected in the document are from the update just performed. The following query will add another sub-document to the plan array and also return the new updated document. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.user.findAndModify({\n    query: {},\n    update: {\n        $addToSet: {'plan': {date: new Date(\"2017-12-28\"), miles: 4.1}}\n    },\n    'new': true\n})\n",
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
                "value":" These code samples begin to expose the power of arrays and sub-documents in MongoDB.  With them you can combine what would be many tables in a RDBMS into a single document.  All the code samples are found ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/2017/12-Dec/12-23-MongoDB-Pt3/dbscripts.js"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" HERE",
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

postName = "dec-23-2017-mongodb-pt3";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Learning MongoDB Part III: Arrays and Nested Objects",
    date: new Date('2017-12-23T12:00:00'),
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
            startName: "Kyle Banker, Peter Bakkum, Shaun Verch, Douglas Garrett &amp; Tom Hawkins, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2016), 171",
            linkName: "MongoDB In Action",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content, 
    contentString: JSON.stringify(content) 
});