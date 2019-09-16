/**
 * Code written statistics for TypeScript.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

db.stats.remove({name: "TypeScript"});

db.stats.insertOne({
    name: "TypeScript",
    first_year: 2017,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 133,
        /* 2018 */ 2375,
        /* 2019 */ 361
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 17,
        /* 2018 */ 6,
        /* 2019 */ 17
    ]
});