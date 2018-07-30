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
                "value":" In my last discovery post on graph databases and Neo4j I ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/nov-6-2017-neo4j-create"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"created a graph",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" that represented a map of Fairfield County Connecticut.  I created nodes for all the towns/cities and edges between the settlements that shared borders.  In this discovery I will add to this graph and show some of the challenges I faced along the way.  Let’s dive in! ",
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
                "value":" In my last discovery post on graph databases and Neo4j I ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/nov-6-2017-neo4j-create"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"created a graph",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" that represented a map of Fairfield County Connecticut.  I created nodes for all the towns/cities and edges between the settlements that shared borders.  In this discovery I will add to this graph and show some of the challenges I faced along the way.  Let’s dive in! ",
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
                "value":" First let’s populate this graph with some people (after all, a settlement needs to have citizens!). ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"CREATE (aj:Person {name: 'Andy J.'}),\n    (tc:Person {name: 'Tom C.'}),\n    (js:Person {name: 'Joe S.'}),\n    (bf:Person {name: 'Ben F.'})\n",
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
                "value":" Now what if I wanted to undo this action of creating people?  The first option I have is to simply delete all of the people from the graph: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"MATCH (p:Person) DELETE p\n",
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
                "value":" But maybe I only want to delete specific people from the graph.  The first thing that came into to my head was to simply match on a certain name such as ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"MATCH (p:Person {name: ‘Andy J.’})",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  In this case I would delete all nodes with label people and where property name equals ‘Andy J.’. You may be seeing the potential issue here.  What if there are two people with the name ‘Andy J’?  Then Cypher will delete both of the vertices!  In a large graph like this that is a very likely scenario. What you can do instead is delete the vertex by looking up its node id.  Each vertex and edge in the graph has a unique node id.  That means if I delete a vertex by its id I know only one vertex will be deleted.  You can get a nodes id with the Cypher ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"id()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function",
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
            "language":"Cypher"
        },
        "value":"MATCH (p:Person) WHERE id(p) = 62 DETACH DELETE p\n",
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
                "value":" In this example I found that the node I wanted to delete had an id of 62 and then searched all nodes with label people for it.  The pair of keywords here that I haven’t used before are ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"DETACH DELETE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  In order to delete a node in Cypher you also have to delete its relationships, so ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"DETACH DELETE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" removes both of them",
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
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" Now if we have all four people vertices back in the graph, we can create a relationship between them and the settlements they are citizens of: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"MATCH (greenwich:Town {name: 'Greenwich'}),\n    (sherman:Town {name: 'Sherman'}),\n    (fairfield:Town {name: 'Fairfield'}),\n    (easton:Town {name: 'Easton'})\nCREATE (aj:Person {name: 'Andy J.'}),\n    (aj)-[:LIVES_IN]->(greenwich),\n    (tc:Person {name: 'Tom C.'}),\n    (tc)-[:LIVES_IN]->(fairfield),\n    (js:Person {name: 'Joe S.'}),\n    (js)-[:LIVES_IN]->(easton),\n    (bf:Person {name: 'Ben F.'}),\n    (bf)-[:LIVES_IN]->(sherman)\n",
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
                "value":" We now have a populated map!  Fairfield County still seems a bit boring however.  Let’s populate it will some of the top rated pizza places in the county.  I want to create a vertex pizza and give it a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":":LOCATED_IN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" relationship to a settlement: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"CREATE (stan:Pizza {name: 'Stanziato’s Wood Fired Pizza', price: 2, rating: 4.5}),\n    (stan)-[:LOCATED_IN]->(:Settlement {name: 'Danbury'}),\n    (bw:Pizza {name: 'Brick + Wood', price: 2, rating: 4.5}),\n    (bw)-[:LOCATED_IN]->(:Settlement {name: 'Fairfield'}),\n    ...\n",
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
                "value":" When I first ran this code I really thought it would work!  However instead of connecting the newly created pizza places to existing settlements, Cypher created new settlements as well!  Now we have duplicate cities in our graph that we need to delete.  The following query will do the job",
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
            "language":"Cypher"
        },
        "value":"MATCH (s:Settlement)\nWITH s.name as name, collect(s) as instances\nWHERE size(instances) > 1\nUNWIND tail(instances) as dups\nMATCH (dups)-[r:LOCATED_IN]-(pizza)\nDELETE r, dups, pizza\n",
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
                "value":" Let’s walk through this.  First we are going to query the graph for all settlements.  Then we want to basically set up a table with two columns.  The first column will be the name of the settlement and the second column will be a list of all the settlement vertices with that name. The list of names is created when we write ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"WITH s.name as name",
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
                "value":"WITH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause is used to manipulate our data (in this case the settlement vertices information) before we pass it on to the next line in our query",
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
                "value":". The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"AS",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keyword simply creates an alias for the column. We then create the list of all the settlement vertices with a given name with Cypher’s ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"collect()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function.  You can think of this entire line of code as an implicit ",
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
                "value":" statement (like that seen in SQL – Cypher has no explicit ",
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
                "value":" Now the line ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"WHERE size(instances) > 1",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" filters our list for only settlement names that have more than one vertex.  After all the goal of this is to delete all the duplicates! The next line, ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"UNWIND tail(instances) as dups",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" transforms our list back into rows in a table",
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
                "value":". This ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"UNWIND",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operation will happen for each ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"instances",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" list we encounter.  Now we see if the row (which is a settlement) has a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"LOCATED_IN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" relationship to a vertex with label ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":":Pizza",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  If so this is the duplicated data.  In the final line, we delete the settlement, relationship, and pizza place from the graph.  The graph is successfully cleaned up! ",
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
                "value":" Let’s again create pizza places and relate them to a town without creating duplicates: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"CREATE (p1:Pizza {name: 'Stanziato’s Wood Fired Pizza', price: 2, rating: 4.5})\nWITH p1\nMATCH (s1:Settlement {name: 'Danbury'})\nMERGE (p1)-[:LOCATED_IN]->(s1)\n",
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
                "value":" First we create the pizza place and pass its variable to the ",
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
                "value":" statement through the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"WITH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause.  This shows that you can also use ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"WITH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" for strictly variable passing without performing any manipulations on it.  Now instead of creating a new settlement we ",
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
                "value":" an existing one and then ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"MERGE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"LOCATED_IN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" relationship. ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"MERGE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" will only create the new relationship if it does not already exist.  If the relationship does not exist it acts the same as ",
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
                "value":" So we now have a complete graph with settlements, people, and places to eat.  One thing that is important to mention before finishing this discovery is indexes.  In Neo4j, just like in SQL, we can create indexes to speed up vertex lookup speeds",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"6",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  This is important when we are searching for a specific item in the graph, such as a person with name Tom. ",
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
                "value":" Since we are often going to query settlements and pizza places by their names we can create indexes on those labels and properties like so: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Cypher"
        },
        "value":"CREATE INDEX ON :Settlement(name)\nCREATE INDEX ON :Pizza(name)\n",
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
                "value":" Now we are fully ready to query the graph database.  The code used for this discovery can be found ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://\ngithub.com/AJarombek/jarombek-com-sources/blob/master/2017/11-Nov/11-12-Neo4j-Challenges/neo4j-additions.cql"
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

postName = "nov-12-2017-neo4j-challenges";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Challenges with Neo4j Graph Creation",
    date: new Date('2017-11-12T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Neo4j",
            picture: "https://asset.jarombek.com/logos/neo4j.png",
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
        },
        {
            name: "SQL",
            picture: "https://asset.jarombek.com/logos/sql.png",
            color: "sql"
        }
    ],
    preview,
    sources: [
        {
            startName: "\"What's the Cypher script to delete a node by ID?\", September 20th, 2016, ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/28144751/whats-the-cypher-script-to-delete-a-node-by-id",
            link: "https://stackoverflow.com/questions/28144751/whats-the-cypher-script-to-delete-a-node-by-id"
        },
        {
            startName: "\"DETACH DELETE Neo4j 2.3.x/Cypher\", October 15th, 2015, ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/33139903/detach-delete-neo4j-2-3-x-cypher",
            link: "https://stackoverflow.com/questions/33139903/detach-delete-neo4j-2-3-x-cypher"
        },
        {
            startName: "\"How to delete duplicate nodes and their relationships in neo4j with cypher?\", ",
            endName: "",
            linkName: "https://gist.github.com/jruts/fe782ff2531d509784a24b655ad8ae76",
            link: "https://gist.github.com/jruts/fe782ff2531d509784a24b655ad8ae76"
        },
        {
            startName: "\"WITH\", ",
            endName: "",
            linkName: "https://neo4j.com/docs/developer-manual/current/cypher/clauses/with/",
            link: "https://neo4j.com/docs/developer-manual/current/cypher/clauses/with/"
        },
        {
            startName: "\"UNWIND\", ",
            endName: "",
            linkName: "https://neo4j.com/docs/developer-manual/current/cypher/clauses/unwind/",
            link: "https://neo4j.com/docs/developer-manual/current/cypher/clauses/unwind/"
        },
        {
            startName: "Ian Robinson, Jim Webber & Emil Eifrem, ",
            endName: " (Beijing: O'Reilly, 2015), 47",
            linkName: "Graph Databases",
            link: "http://shop.oreilly.com/product/0636920028246.do"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});