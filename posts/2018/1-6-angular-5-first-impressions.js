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
                "value":" Recently I have been learning different JavaScript technologies to build a website.  I have looked into Node.js and MongoDB for my back-end and database.  Now it is time to switch to the front-end JavaScript technologies.  The two top picks for front-end frameworks is Angular and React.js.  Today I will begin to look at Angular and if it will be a suitable option for my website.  This post won't look at too much code but instead cover my initial reactions to the framework. ",
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
                "value":"Angular CLI",
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
                "value":" I struggled for a very long time getting all my Angular dependencies set up.  I wanted to do it all manually to start just so I could get to know the framework better.  I messed around with this for a few hours and eventually gave up.  The problem was my Angular \"hello world\" code returned a blank screen with no error messages (which is quite hard to debug).  I decided that I was in over my head with this framework and had to choose an easier entry approach. ",
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
                "value":" Luckily there is an easier option - the Angular CLI.  Angular CLI will build a new Angular project for you, create new components, run the server and more",
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
                "value":".  Angular CLI is also a wrapper around Webpack so it does all the module building for you",
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
                "value":".  I don't yet know the details of Webpack so for now it is nice that the process is done for me.  Later on however I'm sure I would like to have more control of the building of my project. ",
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
                "value":" The nice thing about Angular CLI is how easy it is to setup a new project.  After installing it from npm, creating and running a new Angular project is as easy as the following two Bash commands. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"# Create a new Angular project with name 'developer'\nng new developer\n\n# Start the Angular project on a server\nng serve\n",
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
                "value":" The one thing that annoyed me about Angular CLI is how much content they put into the base Angular project. The bloat included fully set up testing suites (multiple), fully populated gitignore file, and more.  For someone who likes to customize my own project, this was overkill.  I ended up deleting most of these files to get only the necessary components. ",
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
                "value":"TypeScript Language",
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
                "value":" The creators of Angular suggest that you use TypeScript, a superset of JavaScript that is statically typed. You can also use JavaScript, but I've decided to follow the advice and try TypeScript out.  So far I have found it to be more detrimental than beneficial. ",
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
                "value":" It is said that TypeScript is a true superset of JavaScript such that all JavaScript code is valid TypeScript code.  However in my first attempts at the language I had trouble using JavaScript features in TypeScript. For example when using a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"RegExp",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in TypeScript I got errors saying that its JavaScript functions did not exist.  This may be simply user error, but so far TypeScript has not been the smooth transition it is advertised to be. ",
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
                "value":" Also, using TypeScript in my front-end would end my truly full JavaScript web stack.  Although TypeScript is very similar to JavaScript, I would still have to adjust to using two different languages when developing. As my TypeScript experience grows I will see if I start seeing some benefits of the language. ",
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
                "value":"Modules & Components",
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
                "value":" This post has been fairly negative, so I will take a break from that and look at some exciting things about Angular that I can't wait to get my hands on.  Angular at its core is made up of modules that contain components, directives, services, etc",
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
                "value":".  When working with AngularJS a bit in the past I always liked the modularized structure of my JavaScript compared to the spaghetti code style of old. Angular 5 seems to be even more modularized then that, which at first glance looks nice. ",
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
                "value":" My hello world type Angular project contains one module.  It is exported for use in other classes. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"TypeScript"
        },
        "value":"import { BrowserModule } from '@angular/platform-browser';\nimport { NgModule } from '@angular/core';\n\nimport { DeveloperComponent } from './app.component';\nimport { ValidDevDirective } from './valid-dev.directive';\n\n// Wrap our component and directive into a module.\n// Each file can be one module which can be exported.\n// You must import BrowserModule in the root module\n@NgModule({\n    imports: [BrowserModule],\n    declarations: [DeveloperComponent, ValidDevDirective],\n    bootstrap: [DeveloperComponent]\n})\nexport class AppModule { }\n",
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
                "value":"@NgModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation specifies that this class is an Angular module.  Note that just like ES6 classes, TypeScript classes are not true classes.  They are transpiled into JavaScript prototypes and are simply syntactic sugar. ",
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
                "value":" The annotations object has a declarations property that specifies all the members of the module",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"4",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". These two declarations are what we will look at next. ",
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
                "value":" The first module member is a Component named ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"DeveloperComponent",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". A component has a TypeScript class defining model data and a view (which in my case is HTML). ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"TypeScript"
        },
        "value":"import { Component } from '@angular/core';\n\n// Create a new Angular component which can be used in HTML with the 'developer' tag\n// Angular uses annotations as metadata\n@Component({\n    selector: 'developer',\n    templateUrl: 'app.component.html'\n})\nexport class DeveloperComponent {\n    dev: string;\n\n    constructor() {\n        this.dev = 'Andy';\n    }\n}\n",
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
                "value":" The awesome thing about components is that you can place them in your HTML as a tag using the name defined in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@Component",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotations ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"selector",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property.  The component's HTML is defined in the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"template",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" or ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"templateUrl",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property and will render on your webpage at runtime. ",
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
                "value":" For example my code has an index.html file like so: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"HTML"
        },
        "value":"<body>\n    <developer></developer>\n</body>\n",
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
                "value":" When rendered in the browser, the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<developer>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" tag will be replaced with the markup defined in my component: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"HTML"
        },
        "value":"<div>\n    <h1>\n        Hello {{ dev }}!\n    </h1>\n    <p>Enter a developer name:</p>\n    <div>\n        <input type=\"text\" name=\"devInput\" valid-dev>\n    </div>\n</div>\n",
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
                "value":" Another cool thing to notice is that the line ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"{{dev}}",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in the HTML will be replaced with the variables value defined in the TypeScript component class.  This binding between the HTML and TypeScript model creates many possibilities for dynamic webpages that I can't wait to fully explore! ",
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
                "value":"Directives",
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
                "value":" The second module member is a directive called ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ValidDevDirective",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". A directive is like a component except that it has no view and is placed on an existing HTML element ",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"5",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  If you look back to the HTML input tag of my component you will see an attribute called ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"valid-dev",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  This attribute is defined in my directive, which right now changes the border color of the input field when a value is entered. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"TypeScript"
        },
        "value":"import { Directive, ElementRef, Renderer2 } from '@angular/core';\n\n@Directive({\n    selector: 'input[valid-dev]',\n    host: {\n        '(input)': 'onInput($event)'\n    }\n})\nexport class ValidDevDirective {\n    renderer: any;\n    element: any;\n\n    constructor(renderer: Renderer2, element: ElementRef) {\n        this.renderer = renderer;\n        this.element = element;\n\n        this.style('#ccc')\n    }\n\n    // When the value in the input field changes, check its contents\n    onInput(event) {\n        let value: string = event.target.value;\n\n        if (value === '') {\n            this.style('#ccc')\n        } else {\n            this.style('black')\n        }\n    }\n\n    style(color) {\n        this.renderer.setStyle(this.element.nativeElement, 'border-color', color)\n    }\n}\n",
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
                "value":" Obviously this is a very trivial implementation, but it is cool how you can extend the implementation of any HTML element.  Also the fact that directives an components are reusable makes them easy to use throughout a project.  Just like components I am excited to try out more complex implementations. ",
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
                "value":"Wrapping Up",
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
                "value":" So this was a quick glance at how my first attempts at Angular 5 went.  I am not convinced it is the solution for me yet, but there is still a lot of work and exploring to do.  Up next I will start building a full Angular project! ",
                "children":null
            }
        ]
    }
];

db.posts.remove({name: "jan-6-2018-angular-5-first-impressions"});

db.posts.insertOne({
    name: "jan-6-2018-angular-5-first-impressions",
    title: "Angular 5 First Impressions",
    date: new Date('2018-01-06T12:00:00'),
    type: "Discovery",
    tags: [
        {
            name: "Angular",
            picture: "./assets/angular.png",
            color: "angular"
        },
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
            startName: "\"The Ultimate Angular CLI Reference Guide\", ",
            endName: "",
            linkName: "https://www.sitepoint.com/ultimate-angular-cli-reference/",
            link: "https://www.sitepoint.com/ultimate-angular-cli-reference/"
        },
        {
            startName: "\"To use Angular CLI or not?\", ",
            endName: "",
            linkName: "https://medium.jonasbandi.net/to-use-angular-cli-or-not-187f87d0b550",
            link: "https://medium.jonasbandi.net/to-use-angular-cli-or-not-187f87d0b550"
        },
        {
            startName: "Yakov Fain &amp; Anton Moiseev, ",
            endName: " (Shelter Island, NY: Manning, 2017), 32",
            linkName: "Angular 2 Development with TypeScript",
            link: "https://www.manning.com/books/angular-2-development-with-typescript"
        },
        {
            startName: "",
            endName: ", 33",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/angular-2-development-with-typescript"
        },
        {
            startName: "",
            endName: ", 35",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/angular-2-development-with-typescript"
        }
    ]
});