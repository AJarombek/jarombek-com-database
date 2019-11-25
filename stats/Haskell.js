/**
 * Code written statistics for Haskell.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "Haskell"});

db.statistics.insertOne({
    name: "Haskell",
    first_year: 2018,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ null,
        /* 2018 */ 873,
        /* 2019 */ 332,
        /* 2020 */ null
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ null,
        /* 2018 */ 11,
        /* 2019 */ 22,
        /* 2020 */ null
    ]
});
