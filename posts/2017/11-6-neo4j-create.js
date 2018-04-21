/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/13/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nLately I have been reading up on graph databases and their place in the NoSQL data storage\nuniverse. The graph database I've worked with is Neo4j, which is very easy to get set up.\nI've found the user interface to view graphs and type in queries to be very enjoyable and\nI highly recommend it if you need a graph database solution.\n",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nWithout going into too much detail, the largest draw to graph databases is storing\nrelationships between data and the speed at which you can query related data points (or in\ngraph terms nodes/vertices). Relationships are first class citizens in graph databases which\nallows you to query related data by traversing the relationship itself.  This is contrasted\nwith a typical relational database solution where you have to find relationships through\nforeign keys or combine two tables of data with a very slow SQL JOIN operation\n",
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
                "value":"\n.  Our same slow query in a RDBMS (Relational DataBase Management System) is extremely quick\nin a graph database.\n",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nOne of the first graphs I made in Neo4j represented the county I grew up in -\n",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://i.pinimg.com/736x/32/10/06/3210060e2e11b84a497e7b56dac7fbb8--connecticut-ancestry.jpg"
                },
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"\nFairfield County CT\n",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"\n.  The first task on my list was to create a vertex to represent a state - in this case Connecticut.\nIn Cypher (the query language used by Neo4j) that is easy!\n",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"\nCREATE (ct:State {name: 'Connecticut'}) RETURN ct\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nWe use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CREATE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" statement to build a vertex and pass it a label\n",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":":State",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and a property ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"name",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"\n. The label is used for grouping, in this case all states will have the label\n",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":":State",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". You can also name the vertex along\nwith supply additional key->value information in the vertices properties.\n",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nYou can also create multiple vertices from a single ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CREATE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"\n statement. I will utilize this to populate the counties towns and cities:\n",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"\nCREATE (:City {name: 'Bridgeport'}),\n  (:City {name: 'Danbury'}),\n  ...\n\nCREATE (:Town {name: 'Bethel'}),\n  (:Town {name: 'Brookfield'}),\n  ...\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nBefore I create any relationships, I want to make life easier and group together cities and\ntowns under one common label. After all they are both considered settlements.\n",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"\nMATCH (s) WHERE s:City OR s:Town SET s:Settlement\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nI introduced some new keywords here. Most important of them is ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"MATCH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"\n which queries the database based on some ASCII Art that I pass it. The\n",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"(...)",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" token represents a node in the database which I assign\nto variable ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"s",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". So this query says \"for each vertex in the database\nthat is a city or town set a new label called Settlement\". In Neo4j a vertex can have multuple labels so this\n",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"SET",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operation will not override the old labels.\n",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nNow it is time for the fun part: relationships. Lets create a relationship between all the\nsettlements and the state of Connecticut:\n",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"\nMATCH (ct:State), (s:Settlement) MERGE (ct)<-[:IN]-(s)\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nAs you likely guessed, the ASCII art for a relationship is ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"{`<-[:IN]-`}",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"\nwhere the arrow shows the direction of the relationship. We also give the relationship a\nlabel, in this case\n",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":":IN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"\n. We could also give a relationship properties just like you would a vertex. This is what I\nmeant by 'relationships are first class entities' - they are treated and can be queried just\nlike a vertex! This is extremely powerful.\n",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nYou may have noticied that in this query we match for multiple vertices. In this case, we\nwant all the vertices where the label is State or Settlement. Then we create the\nrelationship \"settlement is in state\". Since the only state in the database is\nConnecticut, this simple query will give us the intended result.\n",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nFor the final step of this graph we want to create relationships between all the\nneighboring towns. This is a long query so I'll just show a snippet (the full code\nfor this and the other snippets can be found\n",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-submittions/blob/master/Discoveries\n/2017/11-Nov/11-6-Neo4j-Create/Source/neo4j-create.cql"
                },
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
                "value":"):\n",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"\nMATCH (greenwich:Settlement {name: 'Greenwich'}),\n  (stamford:Settlement {name: 'Stamford'}),\n  (newcannan:Settlement {name: 'New Cannan'}),\n  (darien:Settlement {name: 'Darien'}),\n  ...\nCREATE (greenwich)-[:NEIGHBORS_OF]->(stamford),\n  (stamford)-[:NEIGHBORS_OF]->(newcannan),\n  (stamford)-[:NEIGHBORS_OF]->(darien),\n  ...\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nIn this code we first want to give variables for all the settlement nodes by their name.\nThen we want to create neighbors relationships between towns that share borders. One thing\nthat I questioned when writing this code is 'why cant there be bi-directional\nrelationships?' It turns out at the time of this writing Neo4j does not support\nbi-directional relationships. This is because traversing a realtionship takes the same\namount of time (O(1)) regardless of the direction it is pointing\n",
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
                "value":"\n. In a case like this one where we will treat the relationships as bi-directional, you can\njust ignore the arrow in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"MATCH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"\nqueries. Below you can see the output of the settlements in the Neo4j user interface:\n",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":null,
        "children":[
            {
                "el":"img",
                "attributes":{
                    "src":"./assets/jarombek.png"
                },
                "children":[

                ]
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"\nI will look further at Neo4j and build off this graph in future discoveries. I hope this\nshows you just how simple it is to build a graph database!\n",
                "children":null
            }
        ]
    }
];

db.posts.remove({name: "nov-6-2018-neo4j-create"});

db.posts.insertOne({
    name: "nov-6-2018-neo4j-create",
    title: "Creating a Simple Geographical Map with Neo4j and Cypher",
    date: new Date('2017-11-06'),
    type: "Discovery",
    tags: [
        {
            name: "Neo4j",
            picture: "./assets/neo4j.png",
            color: "neo4j"
        },
        {
            name: "Graph Databases"
        },
        {
            name: "Cypher Query Language"
        },
        {
            name: "NoSQL"
        }
    ],
    content,
    sources: [
        {
            startName: "Ian Robinson, Jim Webber & Emil Eifrem, ",
            endName: " (Beijing: O'Reilly, 2015), 6",
            linkName: "Graph Databases",
            link: "http://shop.oreilly.com/product/0636920028246.do"
        },
        {
            startName: "",
            endName: "., 152",
            linkName: "Ibid",
            link: "http://shop.oreilly.com/product/0636920028246.do"
        }
    ]
});