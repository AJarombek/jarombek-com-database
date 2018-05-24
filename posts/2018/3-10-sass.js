/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/29/2018
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
                "value":" In the client side code for my ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/mean-client-prototype"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" MEAN stack prototype",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" I used Sass for my stylesheets over CSS.  I had experimented with Sass before, but this was my first time using it in a full project.  Until native CSS adds the features that Sass provides, I will continue using Sass for stylesheets in the future. ",
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
                "value":" Sass is a CSS preprocessor with Ruby style syntax.  It is a superset of CSS so any CSS document is also valid Sass",
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
                "value":".  Sass adds lots of new features including variable definitions, mixins, and other syntax that helps create reusable components in your styling.  The biggest problem I had with CSS is that it ends up looking like a jumbled mess with many repeated styles.  With Sass, we now have reusable styles! ",
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
                "value":" In the past I had played around with basic Sass features like inline comments and variables.  For example, you can define variables for a bunch of colors you use throughout the application. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Sass"
        },
        "value":"$baby-blue: #8bb3f4;\n$baby-blue-light: #ccdfff;\n$light-gray: #aaa;\n$off-white: #eee;\n",
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
                "value":" Now when you have a CSS declaration you can reference this variable: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Sass"
        },
        "value":"button {\n    border-color: $baby-blue;\n}\n",
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
                "value":" This time around I experimented with more complex Sass features.  I will discuss these exciting features next! ",
                "children":null
            }
        ]
    },
    {
        "el":"h5",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"Mixins",
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
                "value":" Mixins are an extremely powerful Sass property that allows you to create reusable style blocks. You can create a mixin with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@mixin",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" directive and can use it in a style declaration with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@include",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" directive",
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
                "value":".  For example, in my application I created a mixin for each of my font styles: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Sass"
        },
        "value":"@mixin dyslexie {\n    background-color: transparent !important;\n    font-family: 'Dyslexie', Helvetica, sans-serif !important;\n    font-weight: normal;\n    font-style: normal;\n}\n",
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
                "value":" Then I could apply the styles in the mixin to a style declaration: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Sass"
        },
        "value":"body {\n    @include dyslexie;\n}\n",
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
                "value":" Every element in the website that needed that specific font would simply include the mixin!  I also used mixins to perform more complex operations like transformations.  Another powerful feature of mixins is the ability to pass it arguments.  These arguments can even have default values!  Here is a mixin that I created to apply a transition to a property: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Sass"
        },
        "value":"/**\n* a mixin that simplifies transition generation\n* @param $property - the property of the element that transitions\n* @param $result - the resulting style of the transition\n* @param $time - the time the transition takes to complete (optional)\n*/\n@mixin transition($property, $result, $time: .3s) {\n    transition: #{$property} $time ease;\n\n    // #{} syntax compiles the contents as a property name or selector\n    &:hover {\n        #{$property}: $result;\n    }\n}\n",
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
                "value":" This mixin creates a transition style on the property parameter.  Also if no time is specified as a parameter, the default argument of 0.3 seconds is used. ",
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
                "value":" Another cool piece of this code is the ampersand (",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"&",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":") placeholder.  This placeholder will be a reference to the parent selector once Sass is converted to native CSS",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"3",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":":hover",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" pseudo-class in the above example will be applied to the parent selector that the mixin is included in! ",
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
                "value":" Here is some code where I include the transition mixin: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Sass"
        },
        "value":"#login-button {\n    @include transition(\"background-color\", $light-gray);\n}\n",
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
                "value":" Mixins allow for reduced code and less repetition in Sass files! ",
                "children":null
            }
        ]
    },
    {
        "el":"h5",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"Extends",
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
                "value":" If you have multiple selectors that use the same styles, you can extend these matching styles from a class. Here is an example: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Sass"
        },
        "value":"// Define a general style to be extended for <span> elements\n.span-style {\n    @include sylexiad;\n    color: #990000;\n}\n\nspan {\n    @extend .span-style;\n}\n",
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
                "value":" Now all the styles in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":".span-style",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class will be used in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"span",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" element.  The best part is that this class can be extended into other CSS selectors as well!  It is yet another great way to reuse components in CSS. ",
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
                "value":" I am having a ton of fun exploring Sass and all the exciting possibilities it offers.  While I hope these features eventually make it into the native CSS spec, for now Sass is a nice alternative.  I am sure this won't be my last discovery on the matter! ",
                "children":null
            }
        ]
    }
];

postViews = db.posts.findOne({name: "mar-10-2018-sass"}).views;

db.posts.remove({name: "mar-10-2018-sass"});

db.posts.insertOne({
    name: "mar-10-2018-sass",
    title: "What I have Learned About Sass",
    date: new Date('2018-03-10T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Sass",
            picture: "./assets/sass.png",
            color: "sass"
        },
        {
            name: "CSS",
            picture: "./assets/css.png",
            color: "css"
        }
    ],
    content,
    sources: [
        {
            startName: "Dan Cederholm, ",
            endName: " (New York: A Book Apart, 2013), 14",
            linkName: "Sass for Web Designers",
            link: "https://abookapart.com/products/sass-for-web-designers"
        },
        {
            startName: "",
            endName: ", 44",
            linkName: "Ibid.",
            link: "https://abookapart.com/products/sass-for-web-designers"
        },
        {
            startName: "",
            endName: ", 36",
            linkName: "Ibid.",
            link: "https://abookapart.com/products/sass-for-web-designers"
        }
    ]
});