/**
 * Code written statistics for JSON.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

db.stats.remove({name: "JSON"});

db.stats.insertOne({
    name: "JSON",
    first_year: 2015,
    lines: [
        /* 2014 */ null,
        /* 2015 */ 32,
        /* 2016 */ 820,
        /* 2017 */ 1019,
        /* 2018 */ 364,
        /* 2019 */ 804
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ 8,
        /* 2016 */ 9,
        /* 2017 */ 9,
        /* 2018 */ 15,
        /* 2019 */ 13
    ]
});