/**
 * Code written statistics for YAML.
 * @author Andrew Jarombek
 * @since 9/15/2019
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

db.statistics.remove({name: "YAML"});

db.statistics.insertOne({
    name: "YAML",
    first_year: 2017,
    lines: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 33,
        /* 2018 */ 258,
        /* 2019 */ 2063,
        /* 2020 */ null
    ],
    rank: [
        /* 2014 */ null,
        /* 2015 */ null,
        /* 2016 */ null,
        /* 2017 */ 20,
        /* 2018 */ 19,
        /* 2019 */ 13,
        /* 2020 */ null
    ]
});
