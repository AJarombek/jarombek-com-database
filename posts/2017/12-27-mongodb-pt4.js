/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/28/2018
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
                "value":" For more complex queries and data manipulation in MongoDB, we can use the aggregation framework. First introduced in MongoDB V2.2 (the current version as of this writing is V3.6) the aggregation framework creates a pipeline of operations to perform on documents",
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
                "value":".  Inside the pipeline we can do operations such as sorting, filtering, and grouping documents similar to a SQL ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"GROUP BY",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause.  You can even manipulate grouped data inside the pipeline, creating entirely new collections in the process.  Lets use our tree database used in the ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-submittions/blob/master/Discoveries/2017/12-Dec/\n12-15-MongoDB-Pt1/View/mongodb1.html"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"first",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-submittions/blob/master/Discoveries/2017/12-Dec/\n12-16-MongoDB-Pt2/View/mongodb2.html"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"second",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" MongoDB discoveries to explore this framework. ",
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
                "value":" If you remember each tree has a type and a grade specifying the height of the tree.  Let's say we wanted to find the total number of trees in each grade with type frazier fir.  The aggregation function would be like so: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.aggregate([\n    {$match: {type: 'frazier'}},\n    {$group: {\n        _id: '$grade',\n        count: {$sum:1}\n    }},\n    {$sort: {count: -1}}\n])\n\n/* Result */\n{ \"_id\" : \"9-10ft\", \"count\" : 71 }\n{ \"_id\" : \"10+ft\", \"count\" : 70 }\n{ \"_id\" : \"6-7ft\", \"count\" : 68 }\n{ \"_id\" : \"7-8ft\", \"count\" : 67 }\n{ \"_id\" : \"5-6ft\", \"count\" : 63 }\n{ \"_id\" : \"3-4ft\", \"count\" : 61 }\n{ \"_id\" : \"8-9ft\", \"count\" : 61 }\n{ \"_id\" : \"4-5ft\", \"count\" : 58 }\n",
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
                "value":" You can see that the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"aggregate()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function takes an array of pipeline steps.  So we first match on type frazier, group by the grade and get a count of documents in that group, and finally sort by the count with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$match",
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
                "value":"$group",
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
                "value":"$sort",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operators respectively. ",
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
                "value":" You can also group on more than one property by adding it to the ",
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
                "value":" object.  In the next example we group on both type and grade.  Also since we are matching on the entire collection, we can exclude the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$match",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" step from the pipeline. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.aggregate([\n    {$group: {\n        _id: {type: '$type', grade: '$grade'},\n        count: {$sum:1}\n    }},\n    {$sort: {count: -1}}\n])\n",
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
                "value":" You can also combine multiple ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$group",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operations in an aggregation pipeline.  In the next pipeline we are going to get all the tree statistics - count, expenses, revenue, and profits.  Since we don't want the result of this aggregation to go to waste, we are going to save its result to a collection with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$out",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operator.  We just need to specify the name of the collection to save to, in this case ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"treestat",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  We also use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$project",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operator to structure and pick the properties that we want in the collection.  All properties specified in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$project",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" step are passed to the next step in the pipeline. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.tree.aggregate([\n    {$group: {\n        _id: {\n            type: '$type',\n            grade: '$grade',\n            source: '$source_price',\n            sell: '$sell_price'\n        },\n        count: {$sum:1},\n        expenses: {$sum: '$source_price'},\n        revenue: {$sum: '$sell_price'}\n    }},\n    {$group: {\n        _id: {\n            type: '$_id.type',\n            grade: '$_id.grade',\n            source: '$_id.source',\n            sell: '$_id.sell',\n            count: '$count',\n            expenses: '$expenses',\n            revenue: '$revenue'\n        },\n        profit: {$sum: {$subtract: ['$revenue', '$expenses']}}\n    }},\n    {$sort: {profit: -1}},\n    {$project: {\n        _id: {\n            type: '$_id.type',\n            grade: '$_id.grade'\n        },\n        count: '$_id.count',\n        expenses: '$_id.expenses',\n        revenue: '$_id.revenue',\n        profit: '$profit'\n    }},\n    {$out: 'treestat'}\n])\n",
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
                "value":" One important thing to note about the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"$out",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operator is that it completely replaces the existing collection with the result of our aggregation",
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
                "value":".  This means that you could accidentally wipe out an entire collection such as ",
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
                "value":". Then our database would be ruined! ",
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
                "value":" Let's now get the tree count, expenses, revenue, and profits for all trees using our newly created tree stat collection: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"db.treestat.aggregate([\n    {$group: {\n        _id: 'all_trees',\n        total_trees: {$sum: '$count'},\n        total_expenses: {$sum: '$expenses'},\n        total_revenue: {$sum: '$revenue'},\n        total_profit: {$sum: '$profit'}\n    }}\n])\n\n/* Result */\n{\n    \"_id\" : \"all_trees\",\n    \"total_trees\" : 1001,\n    \"total_expenses\" : 10220.5,\n    \"total_revenue\" : 62245,\n    \"total_profit\" : 52024.5\n}\n",
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
                "value":" Hopefully this has begun to show the power of the aggregation framework.  You can even use the framework to create a kind of pseudo ",
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
                "value":" operation between collections",
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
                "value":"!  You can find the source code for this discovery ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-submittions/blob/master/Discoveries/2017/12-Dec/\n12-27-MongoDB-Pt4/Source/dbscripts.js"
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

db.posts.remove({name: "dec-27-2017-mongodb-pt4"});

db.posts.insertOne({
    name: "dec-27-2017-mongodb-pt4",
    title: "Learning MongoDB Part IV: Aggregation Framework",
    date: new Date('2017-12-27T12:00:00'),
    type: "Discovery",
    tags: [
        {
            name: "MongoDB",
            picture: "./assets/mongodb.png",
            color: "mongodb"
        },
        {
            name: "JavaScript",
            picture: "./assets/js.png",
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
    sources: [
        {
            startName: "Kyle Banker, Peter Bakkum, Shaun Verch, Douglas Garrett &amp; Tom Hawkins, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2016), 121",
            linkName: "MongoDB In Action",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "",
            endName: "., 140",
            linkName: "Ibid",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        },
        {
            startName: "",
            endName: "., 129",
            linkName: "Ibid",
            link: "https://www.manning.com/books/mongodb-in-action-second-edition"
        }
    ]
});