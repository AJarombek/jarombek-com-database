/**
 * Code written statistics for Python.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

db.stats.remove({name: "Python"});

db.stats.insertOne({
    name: "Python",
    first_year: 2015,
    lines: [
        /* 2014 */ null,
        /* 2015 */ 931,
        /* 2016 */ 1122,
        /* 2017 */ 1288,
        /* 2018 */ 1975,
        /* 2019 */ 8848
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ 2,
        /* 2016 */ 7,
        /* 2017 */ 8,
        /* 2018 */ 8,
        /* 2019 */ 1
    ]
});