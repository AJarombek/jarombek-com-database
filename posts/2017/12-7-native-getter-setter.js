/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/26/2018
 */

connection = new Mongo();
db = connection.getDB("jarombekcom");

preview = [
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" In many languages I have worked with creating getters and setters for private class variables is a common practice.  In Java getters and setters are implemented as two functions which can be called directly on an object instance.  They are commonly used in the POJO structure.  In JavaScript getters and setters have native support in the language itself.  You can create these getters and setters with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"get",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"set",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keywords.  They are commonly used for computing properties, as shown in the next example. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"    var person = {\n        _first: 'Andy',\n        _last: 'Jarombek',\n        get full() {\n            console.info('Accessing Full Name');\n            // Use ES6 template literals\n            return `${this._first} ${this._last}`;\n        }\n    };\n",
        "children":null
    }
];

content = [
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" In many languages I have worked with creating getters and setters for private class variables is a common practice.  In Java getters and setters are implemented as two functions which can be called directly on an object instance.  They are commonly used in the POJO structure.  In JavaScript getters and setters have native support in the language itself.  You can create these getters and setters with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"get",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"set",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keywords.  They are commonly used for computing properties, as shown in the next example. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"    var person = {\n        _first: 'Andy',\n        _last: 'Jarombek',\n        get full() {\n            console.info('Accessing Full Name');\n            // Use ES6 template literals\n            return `${this._first} ${this._last}`;\n        }\n    };\n",
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
                "value":" You can also use setters to perform validation on incoming data.  In the dynamically typed JavaScript language we can implement a strictly typed variable",
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
                "value":": ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"var person = {\n    _first: 'Andy',\n    _last: 'Jarombek',\n    set first(str) {\n        console.info('Setting First Name');\n        if (typeof str === 'string' || str instanceof String) {\n            var s = str.toLowerCase();\n            this._first = s.charAt(0).toUpperCase() + s.slice(1);\n        } else {\n            throw new TypeError('First Name Must be a String');\n        }\n    }\n};\n",
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
                "value":" You can see that we can perform any sort of logic inside our getters and setters.  To make things even nicer we don't have to call a function to use our native getters and setters.  We just have to assign a value or access a property just like any normal object property! ",
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
                "value":" Out of all the other languages I use, the only other one with native getter and setter support like JavaScript is Swift",
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
                "value":".  You can check out the full getter and setter code in both languages ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/master/2017/12-Dec/12-7-Native-Getter-Setter"
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

postName = "dec-7-2017-native-getter-setter";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Native Getters & Setters",
    date: new Date('2017-12-07T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        },
        {
            name: "Swift",
            picture: "https://asset.jarombek.com/logos/swift.png",
            color: "swift"
        },
        {
            name: "Getter & Setter"
        },
        {
            name: "Java",
            picture: "https://asset.jarombek.com/logos/java.png",
            color: "java"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "John Resig, Bear Bibeault, & Josip Maras, ",
            endName: " (Shelter Island, NY: Manning, 2016), 207",
            linkName: "Secrets of the JavaScript Ninja",
            link: "https://www.manning.com/books/secrets-of-the-javascript-ninja-second-edition"
        },
        {
            startName: "Matthew Mathias & John Gallagher, ",
            endName: " (Atlanta, GA: Big Nerd Ranch, 2016), 194",
            linkName: "Swift Programming: The Big Nerd Ranch Guide",
            link: "https://www.bignerdranch.com/books/swift-programming/"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content, 
    contentString: JSON.stringify(content) 
});