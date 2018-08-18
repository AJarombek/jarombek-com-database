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
                "value":" In my previous ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/feb-12-2018-oracle-start"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"Oracle database discovery",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" I got a database instance up and running plus set up my basic table structure.  The database held programming language information and different books that I have read.  In this discovery I continue with that example and perform some interesting queries on the database of intermediate difficulty. ",
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
                "value":" Now what do I mean by intermediate difficulty.  Obviously this definition will vary among developers, but I believe that intermediate queries are ones beyond simple select statements with basic filters.  This discovery isn't really a tutorial on creating SQL queries but more exploring different SQL syntax that was interesting to me!  I will start out pretty simple and then get more complex! ",
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
                "value":" In my previous ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/feb-12-2018-oracle-start"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"Oracle database discovery",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" I got a database instance up and running plus set up my basic table structure.  The database held programming language information and different books that I have read.  In this discovery I continue with that example and perform some interesting queries on the database of intermediate difficulty. ",
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
                "value":" Now what do I mean by intermediate difficulty.  Obviously this definition will vary among developers, but I believe that intermediate queries are ones beyond simple select statements with basic filters.  This discovery isn't really a tutorial on creating SQL queries but more exploring different SQL syntax that was interesting to me!  I will start out pretty simple and then get more complex! ",
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
                "value":"Fun Queries!",
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
                "value":" Often I perform queries with multiple ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"WHERE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" conditions, chained by ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"AND",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clauses.  If I wanted to find whether a column is between two different values, I can use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"BETWEEN",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause.  This will make my intentions more clear and reduce my use of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"AND",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clauses.  The following query looks for books between two dates: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"SELECT * FROM books WHERE released\nBETWEEN to_date('2014-08-01', 'YYYY-mm-dd')\nAND to_date('2014-08-31', 'YYYY-mm-dd');\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"WHERE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clauses can also look for soft varchar matches, where only a portion of a string has to match a columns value.  Soft matches use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"LIKE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause which can be chained with a string value.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"%",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" wildcard is used to match any number of characters.  The following query searches for books with titles containing the word 'Java': ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"SELECT * FROM books WHERE title LIKE '%Java%';\n",
        "children":null
    },
    {
        "el":"span",
        "attributes":null,
        "value":"ISBN           TITLE              RELEASED   STARTED   FINISHED  EDITION TIME_READING\n-------------- ------------------ ---------- --------- --------- ------- ------------\n9781617291999  Java 8 in Action   01-AUG-14  10-OCT-17 08-FEB-18         121\n",
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
                "value":" The current table structure of this database forms a many to many relationship.  Here is the ER diagram: ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":{
            "id":"image"
        },
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
                    "class":"jarombek-blog-image",
                    "src":"https://asset.jarombek.com/Database-ER.png"
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
                "value":" In the following query I perform an inner join across this many to many relationship to get a book, the language that it is about, and the inception date of the language.  Inner joins only return rows when the columns in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"JOIN ON",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause have non-null values",
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
                "value":".  Therefore, no row will be returned for a language that has no corresponding book. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"SELECT b.isbn, b.title, bl.name, l.created\nFROM books b\nINNER JOIN book_languages bl ON b.isbn = bl.isbn\nINNER JOIN languages l ON bl.name = l.name\nORDER BY b.title;\n",
        "children":null
    },
    {
        "el":"span",
        "attributes":null,
        "value":"           ISBN TITLE                      NAME       CREATED\n--------------- -------------------------- ---------- --------------\n  9781491901946 AngularJS: Up and Running  JavaScript 04-DEC-95\n  9781617291999 Java 8 in Action           Java       23-MAY-95\n  9780071799355 Oracle Database 12c SQL    SQL        01-JAN-74\n  9780071799355 Oracle Database 12c SQL    PL/SQL     01-JAN-92\n  9781617291203 Spring In Action           Java       23-MAY-95\n",
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
                "value":" Sometimes you may want to make a SQL query that doesn't actually return the data in a table, but instead different values depending on the underlying structure.  The following query does exactly that. Depending on whether a book has a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"time_reading",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" value, we will return 'Finished Reading' or 'Not Completed' (note: the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"time_reading",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"column is a virtual column that is only generated when both a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"finished",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"started",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"date exist).  This operation is performed with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"nvl2()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function, which for its first argument takes a column.  If this column is not null, the second argument is returned, otherwise the third is returned. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"SELECT title, nvl2(time_reading, 'Finished Reading', 'Not Completed') still_reading,\n    CASE WHEN time_reading > 100 THEN 'Long Read'\n        WHEN time_reading > 25 THEN 'Moderate Read'\n        ELSE 'Short Read' END duration\nFROM books;\n",
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
                "value":" If you want to perform more complex if-else logic then checking for null values, you can use a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CASE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" statement",
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
                "value":". Here I check if the time spent reading the book is greater than certain values, and if so return a different string.  Here is the result of this query: ",
                "children":null
            }
        ]
    },
    {
        "el":"span",
        "attributes":null,
        "value":"TITLE                       STILL_READING     DURATION\n--------------------------- ----------------- -------------\nJava 8 in Action            Finished Reading  Long Read\nOracle Database 12c SQL     Finished Reading  Moderate Read\nAngularJS: Up and Running   Not Completed     Short Read\nJump Start: Bootstrap       Finished Reading  Short Read\nSpring In Action            Finished Reading  Short Read\n",
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
                "value":" The next query is going to look at hierarchical data.  This data forms parent child relationships in a tree structure.  The three tables in the database don't have any hierarchical data. Let's change that by creating a new table called ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"language_hierarchy",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" which stores data about languages and other languages that influenced them. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"-- Both name and influenced_by fields come from the language tables name column\nCREATE TABLE language_hierarchy(\n    name VARCHAR2(63),\n    influnced_by VARCHAR2(63),\n    CONSTRAINT language_hierarchy_name_fk\n    FOREIGN KEY (name) REFERENCES languages(name) ON DELETE CASCADE,\n    CONSTRAINT language_hierarchy_inf_by_fk\n    FOREIGN KEY (influnced_by) REFERENCES languages(name) ON DELETE CASCADE\n);\n",
        "children":null
    },
    {
        "el":"span",
        "attributes":null,
        "value":"NAME          INFLUNCED_BY\n------------- --------------\nJava          C\nC\nJavaScript    Java\nJavaScript    C\nJavaScript    Python\nSQL\nPL/SQL        SQL\nHTML\nXML\nCSS\nSass          CSS\nSwift         Python\nPHP           C\nPHP           Java\nPython        C\nPython        Java\nTypeScript    Java\nTypeScript    JavaScript\nJSON          JavaScript\n",
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
                "value":" Now we can display the hierarchical nature of this data using the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CONNECT BY PRIOR",
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
                "value":"START WITH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clauses. ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"START WITH",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" defines a row to start the hierarchical analysis at (it does not have to be the root node in the graph).  In the following query we start with the language 'C' since so many modern day languages were influenced by it. The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CONNECT BY PRIOR",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" defines the parent child relationship.  Let's say we have two rows where the ",
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
                "value":" column in row #1 equals the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"influenced_by",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" column in row #2.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CONNECT BY PRIOR name = influnced_by",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" statement defines that row #1 is the child node and has a relationship with parent node row #2. ",
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
                "value":" Here is the full query: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"SELECT LEVEL, lpad(' ', 2 * LEVEL -1) || name AS language\nFROM language_hierarchy\nSTART WITH name = 'C'\nCONNECT BY PRIOR name = influnced_by;\n",
        "children":null
    },
    {
        "el":"span",
        "attributes":null,
        "value":"    LEVEL LANGUAGE\n--------- -----------------------\n        1 C\n        2   Java\n        3     JavaScript\n        4       JSON\n        4       TypeScript\n        3     PHP\n        3     Python\n        4       JavaScript\n        5         JSON\n        5         TypeScript\n        4       Swift\n        3     TypeScript\n        2   JavaScript\n        3     JSON\n        3     TypeScript\n        2   PHP\n        2   Python\n        3     JavaScript\n        4       JSON\n        4       TypeScript\n        3     Swift\n",
        "children":null
    },
    {
        "el":"#text",
        "attributes":null,
        "value":"     ",
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
                "value":" You may be wondering about the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"LEVEL",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" column.  This is a pseudo column which indicates the level at which we are at in the tree",
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
                "value":".  We use this ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"LEVEL",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" column along with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"lpad()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function to add tabs to the front of the language name depending on the tree depth.  In this example we started at the root column of the tree and navigated downwards, but you can also traverse the tree upwards from a child node.  You can do this by simply switching the columns in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CONNECT BY",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause",
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
                "value":".  In the following query I started at the language 'Java' and traversed the tree upwards. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"SELECT LEVEL, lpad(' ', 2 * LEVEL -1) || name AS language\nFROM language_hierarchy\nSTART WITH name = 'Java'\nCONNECT BY PRIOR influnced_by = name;\n",
        "children":null
    },
    {
        "el":"span",
        "attributes":null,
        "value":"    LEVEL LANGUAGE\n--------- -----------------------\n        1 Java\n        2   C\n",
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
                "value":" Let say I want to group my time reading by the language I am reading about.  I can do this by joining the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"books",
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
                "value":"book_languages",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" tables on the language name. I also use the aggregate function ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"sum()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" on the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"time_reading",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" column. This is a pretty simple use case of the ",
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
                "value":" clause.  Now let's say I also want a row in the result set that totals all the time spent reading.  This is where the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ROLLUP",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause comes in.  You use ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ROLLUP",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in conjunction with the ",
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
                "value":" clause to return a row with the subtotal for the grouping",
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
                "value":": ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"SQL"
        },
        "value":"-- In this scenario you could also use the CUBE clause to the same effect.\nSELECT l.name, sum(b.time_reading) FROM books b\n    INNER JOIN book_languages l ON b.isbn = l.isbn\nGROUP BY ROLLUP(l.name);\n",
        "children":null
    },
    {
        "el":"span",
        "attributes":null,
        "value":"NAME          SUM(B.TIME_READING)\n------------- -------------------\nJava          134\nJavaScript\nPL/SQL        26\nSQL           26\n              186\n",
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
                "value":" Now if you wanted to make the result table more pretty it would be nice for the name column for the subtotal row to display a name like 'Total Days'.  We can implement this behavior with the help of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"grouping()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function.  This function accepts a column and returns 1 if the column value is null and 0 if a value exists",
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
                "value":".  In this example we can use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"grouping()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function along with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CASE",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause to populate the ",
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
                "value":" column with a value when it would usually be ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"null",
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
            "language":"SQL"
        },
        "value":"    SELECT\n        CASE grouping(l.name)\n            WHEN 1 THEN '*Total Days'\n            ELSE l.name\n            END AS name,\n        sum(b.time_reading) AS total_time_reading\n        FROM books b\n        INNER JOIN book_languages l ON b.isbn = l.isbn\n    GROUP BY ROLLUP(l.name);\n",
        "children":null
    },
    {
        "el":"span",
        "attributes":null,
        "value":"NAME          TOTAL_TIME_READING\n------------- -------------------\nJava          134\nJavaScript\nPL/SQL        26\nSQL           26\n*Total Days   186\n",
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
                "value":" There are a ton more cool queries to explore in Oracle, and next time I plan on exploring analytic functions.  You can check out all the code for this discovery on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/master/2018/03-Mar/3-2-Oracle-Queries"
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
    }
];

postName = "mar-2-2018-oracle-queries";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Oracle: Cool Intermediate Level SQL Queries",
    date: new Date('2018-03-02T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "SQL",
            picture: "https://asset.jarombek.com/logos/sql.png",
            color: "sql"
        },
        {
            name: "Relational Database"
        },
        {
            name: "Oracle Database",
            picture: "https://asset.jarombek.com/logos/oracle.png",
            color: "oracle"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "Jason Price, ",
            endName: " (New York: McGraw-Hill Education, 2013), 51",
            linkName: "Oracle Database 12c SQL",
            link: "https://www.amazon.com/Oracle-Database-12c-Jason-Price/dp/0071799354"
        },
        {
            startName: "",
            endName: ", 206",
            linkName: "Ibid.",
            link: "https://www.amazon.com/Oracle-Database-12c-Jason-Price/dp/0071799354"
        },
        {
            startName: "",
            endName: ", 210",
            linkName: "Ibid.",
            link: "https://www.amazon.com/Oracle-Database-12c-Jason-Price/dp/0071799354"
        },
        {
            startName: "",
            endName: ", 213",
            linkName: "Ibid.",
            link: "https://www.amazon.com/Oracle-Database-12c-Jason-Price/dp/0071799354"
        },
        {
            startName: "",
            endName: ", 222",
            linkName: "Ibid.",
            link: "https://www.amazon.com/Oracle-Database-12c-Jason-Price/dp/0071799354"
        },
        {
            startName: "",
            endName: ", 226",
            linkName: "Ibid.",
            link: "https://www.amazon.com/Oracle-Database-12c-Jason-Price/dp/0071799354"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content,
    contentString: JSON.stringify(content)
});