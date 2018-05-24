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
                "value":" In my ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/jan-6-2018-angular-5-first-impressions"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"First Reactions to Angular",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" discovery post, I was quite critical of TypeScript.  I questioned whether it truly was a pure superset of JavaScript and didn’t like how it made the MEAN stack no longer just JavaScript.  With TypeScript I had to change languages again when moving between layers! ",
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
                "value":" I did finish my discussion about TypeScript in that post saying how I would continue to work with it and see if I warmed up to the language.  So is TypeScript going to replace my JavaScript now that I have built a full Angular 5 app with it?  Not quite, but it is a really cool language that brings a lot to the JavaScript ecosystem! ",
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
                "value":"Static Typing",
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
                "value":" The biggest argument for TypeScript over JavaScript is that it is statically typed.  This can allow for quicker development and for many bugs that stem from type incompatibilities to never appear in the first place!  TypeScript does a great job of this!  It is also really nice how TypeScript does not force static typing on you, meaning you don't have to give each variable a type. If you don't give a variable a type it uses the default ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"any",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" type. All types in TypeScript are subtypes of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"any",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", so these variables can be assigned any value just like JavaScript",
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
                "value":"!  This allows you to slowly incorporate TypeScript specific features into your code. ",
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
                "value":"Constructor Shortcuts",
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
                "value":" There were some other cool features of TypeScript that assisted my Angular development.  One of these features was access modifiers and constructors.  TypeScript allows you to use access modifiers on your constructor arguments and then automatically adds these arguments as properties of the object. This reduces code and the use of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"this",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in the constructor body. In fact almost none of my constructors even needed bodies! ",
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
                "value":" Here is an example of a TypeScript class with one of these constructor shortcuts: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"TypeScript"
        },
        "value":"class User {\n\n    // Access modifiers (public, private, protected in TS) on constructor variables\n    // automatically make them instance variables\n    constructor(private username: string, private first: string, private last: string) { }\n\n    public getUsername() {\n\n        // Access the automatically created instance variable\n        return this.username;\n    }\n}\n\nconst newUser: User = new User(\"andy\", \"Andrew\", \"Jarombek\");\nconsole.info(newUser.getUsername()); // andy\n",
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
                "value":" In JavaScript we don’t have this shortcut and have to explicitly assign properties on the object. Now here is the equivalent in JavaScript**: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"JavaScript"
        },
        "value":"class User {\n\n    constructor(username, first, last) {\n        this.username = username;\n        this.first = first;\n        this.last = last;\n    }\n\n    getUsername() {\n        return this.username;\n    }\n}\n\nconst user = new User('andy', 'Andrew', 'Jarombek');\nconsole.info(user.getUsername()); // andy\n",
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
                "value":" **Well, kinda.  The object properties in this class are accessible to the outside code.  To make them private requires additional complexity.  If you want to see exactly what TypeScript converts to, you can paste this code into the online ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://www.typescriptlang.org/play/"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":" TypeScript playground",
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
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" It is also important to remember that with both TypeScript and JavaScript classes are simply syntactic sugar on top of Prototype chains. ",
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
                "value":"Interfaces",
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
                "value":" While JavaScript still has no native interfaces, TypeScript gives developers an ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"interface",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keyword to work with. Interestingly TypeScript interfaces don't transpile into JavaScript code.  Instead they are used to enforce types among classes",
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
                "value":" In my Angular project I had classes that interacted with my backend APIs.  Since they all had similar structures, I had them all implement an interface to help enforce types and class structure: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"TypeScript"
        },
        "value":"interface HttpService {\n\n    getAll(): Observable<[any]>;\n    get(name: any): Observable<any>;\n    post(item: any): Observable<any>;\n    put(item: any): Observable<any>;\n    patch?(name: string, data: {[key: string]: any}): Observable<any>;\n    delete(name: any): Observable<any>;\n}\n",
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
                "value":" The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"patch()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function defined in the interface is optional for classes to implement (as defined with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"?",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" token).  This allows my classes to still be flexible even with an interface! ",
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
                "value":"Conclusions",
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
                "value":" These were a few of the features of TypeScript that really stood out to me.  I really missed optional static typing amongst other features when switching back to JavaScript.  While I don’t think TypeScript will replace my use of JavaScript, I do hope JavaScript implements some of these features in the future (in fact, there is a ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/michaelficarra/proposal-first-class-protocols"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"proposal",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" to add interfaces to JavaScript in the wild, so there is hope!). ",
                "children":null
            }
        ]
    }
];

postViews = db.posts.findOne({name: "mar-8-2018-typescript"}).views;

db.posts.remove({name: "mar-8-2018-typescript"});

db.posts.insertOne({
    name: "mar-8-2018-typescript",
    title: "What I Have Learned About TypeScript",
    date: new Date('2018-03-08T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "TypeScript",
            picture: "./assets/ts.png",
            color: "typescript"
        },
        {
            name: "JavaScript",
            picture: "./assets/js.png",
            color: "javascript"
        }
    ],
    content,
    sources: [
        {
            startName: "Yakov Fain &amp; Anton Moiseev, ",
            endName: " (Shelter Island, NY: Manning, 2017), 395",
            linkName: "Angular 2 Development with TypeScript",
            link: "https://www.manning.com/books/angular-2-development-with-typescript"
        },
        {
            startName: "",
            endName: ", 410",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/angular-2-development-with-typescript"
        }
    ]
});