/**
 * Code written statistics for JSON.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "JSON"});

db.statistics.insertOne({
    name: "JSON",
    first_year: 2015,
    lines: [
        /* 2014 */ null,
        /* 2015 */ 32,
        /* 2016 */ 820,
        /* 2017 */ 1019,
        /* 2018 */ 364,
        /* 2019 */ 804
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ 5,
        /* 2016 */ 8,
        /* 2017 */ 11,
        /* 2018 */ 9,
        /* 2019 */ 14
    ]
});