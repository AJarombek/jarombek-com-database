/**
 * Code written statistics for Sass.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "Sass"});

db.statistics.insertOne({
    name: "Sass",
    first_year: 2017,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 163,
        /* 2018 */ 4198,
        /* 2019 */ 4629,
        /* 2020 */ null
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 15,
        /* 2018 */ 4,
        /* 2019 */ 3,
        /* 2020 */ null
    ]
});
