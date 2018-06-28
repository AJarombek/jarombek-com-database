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
                "value":" Recently I've spent a lot of time working with React.js for my upcoming website (which by the time you are reading this is completed!).  One of the challenges I faced was dynamically deciding at runtime which JSX element to render.  It turns out there is a pretty nice solution to this problem. ",
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
                "value":" First off, if you have been following by posts you will know that this is the first time I have posted about React.js.  Therefore lets quickly review/initiate ourselves with the React library. ",
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
                "value":" React.js is a frontend library written in JavaScript for creating and maintaining views.  Unlike the Angular framework which I was using before, React does not control any aspect of your project besides for the views.  This makes it quite lightweight and gives the developer a lot of flexibility and freedom. ",
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
                "value":" Let’s quickly run through some of the major selling points of React.  React creates a virtual layer on top of the DOM called the Virtual DOM.  React uses the virtual DOM to interact with the real DOM API, allowing the developer to avoid interacting with the DOM itself",
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
                "value":".  React also has an extremely effective diffing algorithm which makes updates to the DOM quick by only changing necessary pieces of the DOM on state changes",
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
                "value":" React is also isomorphic (can be run on multiple platforms), meaning that it does not necessarily have to render its views on the DOM",
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
                "value":".  It can also render HTML on the server (server-side rendering) or on mobile apps!  I haven’t explored React Native for mobile apps yet but it sounds extremely cool!  My website takes advantage of server-side rendering, which I will cover in detail in a future discovery. ",
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
                "value":" Also importantly React tries to take advantage of JavaScripts functional aspects.  Much of my React code is functional, and working with React helps me to think functionally - making my code more testable and maintainable.  I went into detail on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/feb-8-2018-java8-functional"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" functional programming in Java",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in a previous discovery and all the benefits that come with it.  These benefits also carry over to JavaScript. ",
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
                "value":" So, how to we work with the virtual DOM that React offers us?  We do so with React elements, which correspond to HTML elements.  These React elements are JavaScript objects that we can manipulate.  We can even group together React elements into a modularized component.  These components can be passed to the virtual DOM as well.  When a component is passed to the virtual DOM, the elements inside it will be rendered.  This practice allows us to make easily reusable HTML elements. ",
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
                "value":" Here is an example of a React component that I will use later on: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"import React from \"react\";\n\nconst Link = ({ name, link }) =>\n    React.createElement(\"a\", {href: link}, name);\n\nexport default Link;\n",
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
                "value":" This React component is very simple, it creates a new React element which corresponds to the HTML ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<a>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" element.  This element has an ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"href",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" attribute which links it to a url passed in to the component via ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"link",
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
                "value":"<a>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" element also displays the text passed to the component by the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"name",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property. ",
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
                "value":" The function ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"React.createElement()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is what creates the React element corresponding to an HTML element.  You can image how tiresome and verbose it would be to type out this function call for every single HTML element that you wanted a webpage to display.  Luckily there is an alternative, JSX.  JSX allows us to create React elements in a syntax that looks like HTML.  Here is an example of the previous component except with JSX: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"import React from \"react\";\n\nconst Link = ({ name, link }) =>\n<a href={link}>\n    {name}\n</a>;\n\nexport default Link;\n",
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
                "value":" As you can see, it is much more readable and will be easier to write (although yes, it is a bit of an eyesore at first having to mix HTML syntax with JavaScript code). ",
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
                "value":" Now lets get to the issue I faced when building my website.  I wanted to dynamically choose the JSX element type at runtime based on some JSON that I received from my server.  For example, if the JSON had a property ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"tag",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" with the value ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"div",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", I would want the JSX element to represent an HTML ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"div",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  With this implemented, I could store a JSON representation of some HTML and have my React code create and render it on the fly. ",
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
                "value":" The code used by my website is a bit more involved than what I will show you but I’m going to cover the basic process.  First I import react and the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Link",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" component I created. I also get a JavaScript object representation of some HTML I want to display: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"import React from \"react\";\nimport { render } from \"react-dom\";\nimport Link from \"./Link\";\n\nconst elements = [\n    {\n        tag: \"h1\",\n        attributes: null,\n        value: \"Webpage Title\"\n    },\n    {\n        tag: \"p\",\n        attributes: null,\n        value: \"Hey\"\n    },\n    {\n        tag: \"Link\",\n        attributes: {\n            name: \"Check out my GitHub!\",\n            link: \"https://github.com/AJarombek\"\n        },\n        value: \"Ok\"\n    }\n];\n",
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
                "value":" The object representation of HTML has three properties.  ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"tag",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" represents the HTML element type, ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"attributes",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" lists the HTML attributes, and ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"value",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is the content shown inside the HTML tag. ",
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
                "value":" Next I need to loop through this ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"elements",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" array and generate some JSX for each object.  This JSX will then be rendered in the websites HTML. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"const App = () =>\n    elements.map(e => {\n        let Tag = e.tag;\n\n        // If we are using a custom react component, we must\n        // assign it directly.\n        if (Tag === \"Link\") {\n            Tag = Link;\n        }\n\n        return (\n        <Tag key={e.toString()} {...e.attributes}>\n            {e.value}\n        </Tag>\n        );\n    });\n\nrender(<App />, document.getElementById(\"root\"));\n",
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
                "value":" The important piece here is that we create a variable ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Tag",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" on the third line which represents the JSX element.  Remember that a JSX element is simply syntactic sugar for a React element (which itself is a JavaScript object) that will be sent to the virtual DOM. ",
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
                "value":" Native HTML elements are passed to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"React.createElement()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" as a string.  This is why the code above works for ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"h1",
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
                "value":"p",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" element names.  If you tried assigning the string “Link” to the variable ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Tag",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" it would not render.  The reason is because ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<Link>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is not a valid HTML element!  It is a React component that I created!  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"React.createElement()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" will take in React components as well, but you have to pass it the actual component module.  This is the reason for the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"if",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" clause in the code above. ",
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
                "value":" Here is the end result: ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":null,
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
                    "class":"jarombek-blog-image",
                    "src":"https://asset.jarombek.com/dynamic-jsx.png"
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
                "value":" I still have a lot of learning and work to do with React.  So far I have really enjoyed it despite a few shortcomings that I miss from frameworks such as Angular.  But that is for another post! ",
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
                "value":" The full code for this discovery is up on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/master/2018/04-Apr/4-30-React-Dynamic-JSX-Tag"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"  Github",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" (as it is for all my posts!). ",
                "children":null
            }
        ]
    }
];

postViews = db.posts.findOne({name: "apr-30-2018-react-dynamic-jsx-elements"}).views;

db.posts.remove({name: "apr-30-2018-react-dynamic-jsx-elements"});

db.posts.insertOne({
    name: "apr-30-2018-react-dynamic-jsx-elements",
    title: "React Dynamic JSX Elements",
    description: `Recently I've spent a lot of time working with React.js for my upcoming website.  
        One of the challenges I faced was dynamically deciding at runtime which JSX element to render.`,
    date: new Date('2018-04-30T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "React",
            picture: "https://asset.jarombek.com/logos/react.png",
            color: "react"
        },
        {
            name: "JSX"
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
            startName: "Alex Banks & Eve Porcello, ",
            endName: " (Beijing: O'Reilly, 2017), 62",
            linkName: "Learning React",
            link: "http://shop.oreilly.com/product/0636920049579.do"
        },
        {
            startName: "\"Choosing the Type at Runtime\"",
            endName: "",
            linkName: "https://reactjs.org/docs/jsx-in-depth.html#choosing-the-type-at-runtime",
            link: "https://reactjs.org/docs/jsx-in-depth.html#choosing-the-type-at-runtime"
        },
        {
            startName: "",
            endName: ", 297",
            linkName: "Ibid.",
            link: "http://shop.oreilly.com/product/0636920049579.do"
        }
    ]
});