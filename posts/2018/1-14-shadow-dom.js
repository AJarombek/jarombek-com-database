/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/28/2018
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
                "value":" I was recently reading a book on Angular 2 and came across an HTML concept I had never heard of before - the shadow DOM.  Angular utilizes the shadow DOM to modularize HTML and CSS code, although you can also use the shadow DOM API in native JavaScript",
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
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" A shadow DOM results in modularization by removing its contents from the main document object model. You have to attach the shadow DOM onto an existing HTML element using the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Element.attachShadow()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function",
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
                "value":".  You can then add new HTML code or ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<style>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" elements containing CSS to the shadow DOM.  All of the styles and HTML specified in the shadow DOM will be hidden from the outer HTML implementation.  You can utilize shadow DOM to avoid CSS style conflicts, HTML ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"id",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" overlap, and more! ",
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
                "value":" Here is an example of some HTML code that creates a shadow DOM (notice that the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"shadow",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable is a new shadow DOM element and I add to the shadow using ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"innerHTML",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"). ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"HTML"
        },
        "value":"<!DOCTYPE html>\n<html>\n    <head>\n        <meta chartset=\"utf-8\">\n        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n    </head>\n    <body>\n        <!-- This div will be the holder for the shadow dom -->\n        <div id=\"host\"></div>\n        <div>\n            <style>\n                p {color: red}\n            </style>\n        </div>\n        <p>I'm outside the shadow.</p>\n        <script>\n            // Attach the shadow dom\n            const shadow = document.querySelector(\"#host\").attachShadow({mode: 'open'});\n\n            // Create the shadow dom's contents\n            shadow.innerHTML = \"<p>I'm in a Shadow!</p>\";\n            shadow.innerHTML += `<style>\n                                    p {\n                                        font-style: italic;\n                                        color: #999;\n                                        font-weight: bold;\n                                    }\n                                </style>`\n        </script>\n    </body>\n</html>\n",
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
                "value":" The result: ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":{
            "id":"webresult-image"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"     ",
                "children":null
            },
            {
                "el":"img",
                "attributes":{
                    "class": "jarombek-blog-image",
                    "src":"https://asset.jarombek.com/posts/1-14-18-webresult.png"
                },
                "value":null,
                "children":[

                ]
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
                "value":" When we look at our browsers developer tools and inspect the HTML, you can see that our shadow DOM is wrapped in a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"#shadow-root",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" item, which simply specifies that a shadow DOM exists",
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
                "value":". ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":{
            "id":"html-image"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"     ",
                "children":null
            },
            {
                "el":"img",
                "attributes":{
                    "src":"https://asset.jarombek.com/posts/1-14-18-html.png",
                    "align":"middle"
                },
                "value":null,
                "children":[

                ]
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
                "value":" The ability to modularize an HTML document is awesome!  I will utilize the shadow DOM in my Angular code and look into more native HTML modularization techniques like reusable custom elements in the future! ",
                "children":null
            }
        ]
    }
];

postViews = db.posts.findOne({name: "jan-14-2018-shadow-dom"}).views;

db.posts.remove({name: "jan-14-2018-shadow-dom"});

db.posts.insertOne({
    name: "jan-14-2018-shadow-dom",
    title: "The Shadow Dom",
    date: new Date('2018-01-14T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "DOM"
        },
        {
            name: "HTML",
            picture: "https://asset.jarombek.com/logos/html.png",
            color: "html"
        },
        {
            name: "CSS",
            picture: "https://asset.jarombek.com/logos/css.png",
            color: "css"
        },
        {
            name: "JavaScript",
            picture: "https://asset.jarombek.com/logos/js.png",
            color: "javascript"
        }
    ],
    content,
    sources: [
        {
            startName: "Yakov Fain &amp; Anton Moiseev, ",
            endName: " (Shelter Island, NY: Manning, 2017), 84",
            linkName: "Angular 2 Development with TypeScript",
            link: "https://www.manning.com/books/angular-2-development-with-typescript"
        },
        {
            startName: "\"Shadow DOM\", ",
            endName: "",
            linkName: "https://developer.mozilla.org/en-US/docs/Web/Web_Components/Shadow_DOM",
            link: "https://developer.mozilla.org/en-US/docs/Web/Web_Components/Shadow_DOM"
        },
        {
            startName: "\"What is shadow root\", ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/34119639/what-is-shadow-root",
            link: "https://stackoverflow.com/questions/34119639/what-is-shadow-root"
        }
    ]
});