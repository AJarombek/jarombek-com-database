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
        /* 2019 */ 1068,
        /* 2020 */ null
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ 8,
        /* 2016 */ 9,
        /* 2017 */ 9,
        /* 2018 */ 15,
        /* 2019 */ 14,
        /* 2020 */ null
    ]
});
