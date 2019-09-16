/**
 * Code written statistics for Groovy.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

db.stats.remove({name: "Groovy"});

db.stats.insertOne({
    name: "Groovy",
    first_year: 2016,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 179,
        /* 2017 */ 113,
        /* 2018 */ 2164,
        /* 2019 */ 2324
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ 14,
        /* 2017 */ 19,
        /* 2018 */ 7,
        /* 2019 */ 7
    ]
});