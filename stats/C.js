/**
 * Code written statistics for C.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

db.stats.remove({name: "C"});

db.stats.insertOne({
    name: "C",
    first_year: 2015,
    lines: [
        /* 2014 */ null,
        /* 2015 */ 630,
        /* 2016 */ 379,
        /* 2017 */ 271,
        /* 2018 */ 196,
        /* 2019 */ 125
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ 3,
        /* 2016 */ 10,
        /* 2017 */ 14,
        /* 2018 */ 22,
        /* 2019 */ 24
    ]
});