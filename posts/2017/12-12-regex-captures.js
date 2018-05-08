/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/26/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

content = [
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" Regular expressions are very commonly used in my code.  However, their are still certain aspects of them that have remained a mystery to me.  Today I will explore one new aspect of regular expressions - captures.  I will write the code for captures in JavaScript but they apply to many languages with regex capabilities (including my main language Java).  Let's dig in. ",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" Captures allow us to save pieces of a regular expression so we can use them later on.  For example, it we have a regex that matches emails, we can capture certain aspects of the email - the local part and the domain.  We can also do the same thing with dates - saving the day, month, and year.  That is what happens in the following example: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"let date = \"02/26/1995\";\n\nlet pattern = /(\\d{1,2})\\/(\\d{2})\\/(\\d{4})/;\nlet captures = date.match(pattern);\n\nconsole.info(captures); // [\"02/26/1995\", \"02\", \"26\", \"1995\"]\n\n// The captures can be accessed from the array produced by match()\nconsole.info(`Month: ${captures[1]}, Day: ${captures[2]}, Year: ${captures[3]}`);\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" We use the grouping syntax ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"(...)",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" to define a capture.  The value matched inside this capture is then saved into an array when the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"match()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function is called on the string.  We can then access this arrays elements, as shown on the last line. ",
                "children":null
            }
        ]
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" If we want to use grouping but don't need to capture pieces of our regular expression, JavaScript allows us to specify that a group shouldn't create a capture with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"(?: ...)",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" syntax.  This will result in less work for the languages engine to perform",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"1",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"let pattern2 = /(?:\\d{1,2})\\/(?:\\d{2})\\/(?:\\d{4})/;\nlet captures2 = date.match(pattern2);\nconsole.info(captures2); // [\"02/26/1995\"]\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" One final thing to explore with captures is backreferences.  This allows us to refer to previous captures created in the same regular expression",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"2",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  We can use backreferences to check if the month and day in our dates are equal.  In the following code the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"\\1",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" token will refer to the contents of the first capture (the month) which is defined at runtime.  Now our regular expressions are even more dynamic! ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"let date = \"02/26/1995\";\nlet date2 = \"02/02/2017\";\n\nlet sameDayMonthPattern = /(\\d{1,2})\\/\\1\\/\\d{4}/;\n\nlet fails = sameDayMonthPattern.test(date);\nlet succeeds = sameDayMonthPattern.test(date2);\n\nconsole.info(fails); // false\nconsole.info(succeeds); // true\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" You can find the full code from this discovery ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/2017/12-Dec/\n12-12-Regex-Captures/captures.js"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"HERE",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". ",
                "children":null
            }
        ]
    }
];

db.posts.remove({name: "dec-12-2017-regex-captures"});

db.posts.insertOne({
    name: "dec-12-2017-regex-captures",
    title: "Regular Expression Captures",
    date: new Date('2017-12-12T12:00:00'),
    type: "Discovery",
    tags: [
        {
            name: "JavaScript",
            picture: "./assets/js.png",
            color: "javascript"
        },
        {
            name: "Regular Expression"
        }
    ],
    content,
    sources: [
        {
            startName: "John Resig, Bear Bibeault, & Josip Maras, ",
            endName: " (Shelter Island, NY: Manning, 2016), 274",
            linkName: "Secrets of the JavaScript Ninja",
            link: "https://www.manning.com/books/secrets-of-the-javascript-ninja-second-edition"
        },
        {
            startName: "",
            endName: "., 266",
            linkName: "Ibid",
            link: "https://www.manning.com/books/secrets-of-the-javascript-ninja-second-edition"
        }
    ]
});