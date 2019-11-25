/**
 * Code written statistics for Java.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "Java"});

db.statistics.insertOne({
    name: "Java",
    first_year: 2014,
    lines: [
        /* 2014 */ 4282,
        /* 2015 */ 1585,
        /* 2016 */ 12962,
        /* 2017 */ 12113,
        /* 2018 */ 4769,
        /* 2019 */ 2957,
        /* 2020 */ null
    ],
    rank: [
        /* 2014 */ 1,
        /* 2015 */ 1,
        /* 2016 */ 1,
        /* 2017 */ 1,
        /* 2018 */ 3,
        /* 2019 */ 8,
        /* 2020 */ null
    ]
});
