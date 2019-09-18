/**
 * Code written statistics for Bash.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "Bash"});

db.statistics.insertOne({
    name: "Bash",
    first_year: 2017,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 129,
        /* 2018 */ 1344,
        /* 2019 */ 1924
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 18,
        /* 2018 */ 10,
        /* 2019 */ 12
    ]
});