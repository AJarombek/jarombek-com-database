/**
 * Code written statistics for C#.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "C#"});

db.statistics.insertOne({
    name: "C#",
    first_year: 2018,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ null,
        /* 2018 */ 325,
        /* 2019 */ 4108,
        /* 2020 */ null
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ null,
        /* 2018 */ 18,
        /* 2019 */ 5,
        /* 2020 */ null
    ]
});
