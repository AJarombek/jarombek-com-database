/**
 * Code written statistics for JavaScript.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "JavaScript"});

db.statistics.insertOne({
    name: "JavaScript",
    first_year: 2016,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 2008,
        /* 2017 */ 6663,
        /* 2018 */ 16414,
        /* 2019 */ 7829
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 4,
        /* 2017 */ 3,
        /* 2018 */ 1,
        /* 2019 */ 2
    ]
});