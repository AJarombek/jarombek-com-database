/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/27/2018
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
                "value":" Today I will build on my ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/dec-15-2017-mongodb-pt1"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"first MongoDB discovery",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and look at documents in more depth.  To start, let's look at how we would handle the purchase of a Christmas tree in our database.  The first thing we need to do is pick a tree to buy!  We can search the database for a tree to our liking and use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"findOne()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function to return a single tree. ",
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
                "value":" Today I will build on my ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/dec-15-2017-mongodb-pt1"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"first MongoDB discovery",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and look at documents in more depth.  To start, let's look at how we would handle the purchase of a Christmas tree in our database.  The first thing we need to do is pick a tree to buy!  We can search the database for a tree to our liking and use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"findOne()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function to return a single tree. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.findOne({type:\"douglas\", grade:\"7-8ft\", height:\"7' 3\\\"\"})\n\n/* Result */\n{\n    \"_id\" : ObjectId(\"5a3352702e48ee76cb1fe459\"),\n    \"type\" : \"douglas\",\n    \"height\" : \"7' 3\\\"\",\n    \"source_price\" : 10,\n    \"sell_price\" : 60,\n    \"grade\" : \"7-8ft\",\n    \"sold\" : false,\n    \"buyer_id\" : undefined\n}\n",
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
                "value":" Now we need to create a customer collection to hold all the people who are buying trees: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.customer.insert({\n    username: \"andy\",\n    name: \"Andrew Jarombek\",\n    email: \"andy@jarombek.com\"\n})\n",
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
                "value":" One thing you may have noticed is that each document has a field called ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"_id",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" with an ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ObjectId(...)",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" containing a hex number.  You may think that the hex digits are just randomly generated but they actually hold organized information.  The first eight hex digits (four bytes) of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ObjectId",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" are actually a timestamp of the time when the id was created.  The rest of the id is broken down into three pieces - the machine ID, process ID, and a counter which increments each time an ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ObjectId",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is generated in this particular process.  All these items together create a very reliable unique key (you don't have to worry about collisions - the possibility of that is so small). ",
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
                "value":" So why am I going into the details of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ObjectId",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object?  We can actually use these unique ids to link documents together by having a document property value containing the id of another document.  In the case of our Christmas tree database, we want a collection for purchases.  In this collection, our documents will be linked to both the tree purchased and the customer.  To do this we can take the tree and customer documents ids and put them in the purchase document. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"let tree_id = db.tree.findOne({type:\"douglas\", grade:\"7-8ft\", height:\"7' 3\\\"\"})._id;\nlet user_id = db.customer.findOne()._id;\n\ndb.purchase.insert({\n    type: \"douglas\",\n    grade: \"7-8ft\",\n    price: 60,\n    tree_id: tree_id,\n    username: \"andy\",\n    user_id: user_id,\n    date: Date()\n})\n",
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
                "value":" This code also displays our ability in MongoDB to use JavaScript variables in the query language.  This allows for more readable and structured queries. Now when we look at the purchase document we can see the other documents ids: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.purchase.findOne()\n\n/* Result */\n{\n    \"_id\" : ObjectId(\"5a34a1942e48ee76cb1fe82c\"),\n    \"type\" : \"douglas\",\n    \"grade\" : \"7-8ft\",\n    \"price\" : 60,\n    \"tree_id\" : ObjectId(\"5a3352702e48ee76cb1fe459\"),\n    \"username\" : \"andy\",\n    \"user_id\" : ObjectId(\"5a349e732e48ee76cb1fe82b\"),\n    \"date\" : \"Fri Dec 15 2017 23:31:16 GMT-0500 (EST)\"\n}\n",
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
                "value":" You will also notice some duplicated fields from other collections in the purchase document (such as the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"username",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property). This sort of duplication is frowned upon in a RDBMS, however with no JOINs in MongoDB this duplication is okay",
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
                "value":" Now you have seen how we can link related documents in MongoDB which allows us to find a linked document without a JOIN operation.  Let's take a step back and look at the first query we made picking out a Christmas tree.  We can call the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"explain()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function on our query to find useful information on its execution: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.find({type:\"douglas\", grade:\"7-8ft\", height:\"7' 3\\\"\"}).explain(\"executionStats\")\n\n/* Result */\n{\n    \"executionStats\" : {\n        \"executionSuccess\" : true,\n        \"nReturned\" : 6,\n        \"executionTimeMillis\" : 4,\n        \"totalKeysExamined\" : 0,\n        \"totalDocsExamined\" : 1003,\n    }\n}\n",
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
                "value":" The important property we look at on the returned JSON object is ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"totalDocsExamined",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  You can see that our query looked at every single document in our collection.  Now imagine how slow this could be if we had millions of documents in our collection!  For anyone who has used databases before the answer should come to mind - an index.  Let's add indexes on the fields in tree we are querying against. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.createIndex({type: 1})\ndb.tree.createIndex({grade: 1})\ndb.tree.createIndex({height: 1})\n",
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
                "value":" You may be wondering what the significance is of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"1",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" value. This means that the index is stored in ascending order, while a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"-1",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" would mean descending",
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
                "value":".  Now if we do the same query and call ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"explain()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", only the returned documents are examined. Much better! ",
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
                "value":" We can view all the indexes on a document with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"getIndexes()",
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
        "value":"db.tree.getIndexes()\n",
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
                "value":" Indexes can be used for other purposes other than just speeding up query times.  We can use them to expire documents in what is called a time-to-live (TTL) collection",
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
                "value":".  These collections use indexes to set a date for when a document expires.  To do this, we first need to set a date property on our tree documents.  This date will be Christmas eve, since you won't be able to buy a tree anymore after this date. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"\ndb.tree.updateMany({}, {$set: {\"availableUntil\": new Date(\"2017-12-24\")}})\n",
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
                "value":" Now we can create an index on this ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"availableUntil",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property. The second parameter to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"createIndex()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" supplies additional options, and in this case we want to expire the document zero seconds after the date in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"availableUntil",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property",
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
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"\ndb.tree.createIndex({availableUntil: 1}, {expireAfterSeconds: 0})\n",
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
                "value":" We added a lot of new MongoDB concepts to our tree database.  The power of linked documents and indexes in MongoDB are becoming apparent.  I will look at MongoDB even more in my next discovery.  The code for this discovery can be found ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/\n2017/12-Dec/12-16-MongoDB-Pt2/dbscrips.js"
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

postViews = db.posts.findOne({name: "dec-16-2017-mongodb-pt2"}).views;

db.posts.remove({name: "dec-16-2017-mongodb-pt2"});

db.posts.insertOne({
    name: "dec-16-2017-mongodb-pt2",
    title: "Learning MongoDB Part II: Working with Documents",
    date: new Date('2017-12-16T12:00:00'),
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
    content,
    preview,
    sources: [
        {
            startName: "Kyle Banker, Peter Bakkum, Shaun Verch, Douglas Garrett &amp; Tom Hawkins, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2016), 83",
            linkName: "MongoDB In Action",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "\"db.collection.createIndex()\", ",
            endName: "",
            linkName: "https://docs.mongodb.com/v3.4/reference/method/db.collection.createIndex/",
            link: "https://docs.mongodb.com/v3.4/reference/method/db.collection.createIndex/"
        },
        {
            startName: "",
            endName: "., 90",
            linkName: "Banker",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "\"Atomicity and Transactions\", ",
            endName: "",
            linkName: "https://docs.mongodb.com/manual/tutorial/expire-data/",
            link: "https://docs.mongodb.com/manual/tutorial/expire-data/"
        }
    ]
});