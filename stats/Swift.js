/**
 * Code written statistics for Swift.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "Swift"});

db.statistics.insertOne({
    name: "Swift",
    first_year: 2017,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 10726,
        /* 2018 */ 698,
        /* 2019 */ 2074
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 2,
        /* 2018 */ 12,
        /* 2019 */ 8
    ]
});