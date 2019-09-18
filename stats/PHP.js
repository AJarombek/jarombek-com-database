/**
 * Code written statistics for PHP.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "PHP"});

db.statistics.insertOne({
    name: "PHP",
    first_year: 2016,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 5433,
        /* 2017 */ 3670,
        /* 2018 */ 356,
        /* 2019 */ 357
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 2,
        /* 2017 */ 5,
        /* 2018 */ 16,
        /* 2019 */ 18
    ]
});