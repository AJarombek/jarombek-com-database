/**
 * Code written statistics for CSS.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "CSS"});

db.statistics.insertOne({
    name: "CSS",
    first_year: 2016,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 1223,
        /* 2017 */ 1654,
        /* 2018 */ 594,
        /* 2019 */ 563
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 6,
        /* 2017 */ 7,
        /* 2018 */ 13,
        /* 2019 */ 15
    ]
});