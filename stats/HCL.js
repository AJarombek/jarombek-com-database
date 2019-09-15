/**
 * Code written statistics for HCL.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

db.stats.remove({name: "HCL"});

db.stats.insertOne({
    name: "HCL",
    first_year: 2018,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ null,
        /* 2018 */ 3801,
        /* 2019 */ 4067
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ null,
        /* 2018 */ 5,
        /* 2019 */ 3
    ]
});