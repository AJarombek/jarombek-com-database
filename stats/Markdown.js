/**
 * Code written statistics for Markdown.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "Markdown"});

db.statistics.insertOne({
    name: "Markdown",
    first_year: 2016,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 16,
        /* 2017 */ 133,
        /* 2018 */ 439,
        /* 2019 */ 3470,
        /* 2020 */ null
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 18,
        /* 2017 */ 16,
        /* 2018 */ 14,
        /* 2019 */ 7,
        /* 2020 */ null
    ]
});
