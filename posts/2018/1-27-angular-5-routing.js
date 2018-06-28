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
                "value":" I have recently started work on my Angular 5 framework prototype.  The project will be a single page web application that displays cat pictures!  The page will have a number of tabs you can click on that will change the state of the application.  To start setting up the application, I first created all the routes which will be selected when users click on tabs. ",
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
                "value":" First I made an Angular component that has an HTML template for all the tabs. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"HTML"
        },
        "value":"<div>\n<header>\n    <div>\n        <a [routerLink]=\"['/']\">MeowPics</a>\n    </div>\n    <nav>\n        <ul>\n            <li>\n                <a [routerLink]=\"['/profile']\">Profile</a>\n            </li>\n            <li>\n                <a [routerLink]=\"['/profile/post']\">Post</a>\n            </li>\n            <li>\n                <a [routerLink]=\"['/about']\">About</a>\n            </li>\n        </ul>\n    </nav>\n    <div>\n        <div>\n            <a [routerLink]=\"['/login']\">Login</a>\n        </div>\n        <span>  </span>\n        <div>\n            <a [routerLink]=\"['/signup']\">Sign Up</a>\n        </div>\n    </div>\n</header>\n<!-- Router Outlet is where the router will render the component -->\n<router-outlet></router-outlet>\n</div>\n",
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
                "value":" The code sets up ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"[routerLink]",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" properties that specify which path to route to.  Each of these routes is assigned an Angular component as we will soon see.  When you click a route, its components template will be placed inside the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"<router-outlet>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" element. ",
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
                "value":" The Angular component for this template is called ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"AppComponent",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and is wrapped along with the router's components in a module called ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"AppModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  The most important aspect of this module is that it maps each route to the correct component: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"TypeScript"
        },
        "value":"export const routes: Routes = [\n    {path: '', component: HomeComponent},\n    {path: 'profile', loadChildren: './profile/profile.module#ProfileModule'},\n    {path: 'about', component: AboutComponent},\n    {path: 'profile/post', loadChildren: './profile/profile.module#ProfileModule'},\n    {path: 'login', component: LoginComponent},\n    {path: 'signup', component: SignupComponent}\n];\n\n/**\n* @NgModule configures an Angular module, helping to organize the application into components\n* In Angular 5 NgModules allow for Ahead-Of-Time Compilation, where the JavaScript and HTML\n* is compiled before the code is sent to the client\n* declarations - items used in templates (ex. components, directives)\n* imports - import other modules\n* providers - services used in the module\n* bootstrap - specifies the root component of the application as a bootstrap entry point\n*/\n@NgModule({\n    declarations: [\n        AppComponent,\n        HomeComponent,\n        AboutComponent,\n        LoginComponent,\n        SignupComponent\n    ],\n    imports: [\n        BrowserModule,\n        RouterModule.forRoot(routes)\n    ],\n    providers: [],\n    bootstrap: [AppComponent]\n})\nexport class AppModule { }\n",
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
                "value":" You can see that each route is assigned a HTTP path and an Angular component.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"routes",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable is then used in the ",
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
                "value":" annotation with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"RouterModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  We use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"RouterModule.forRoot()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method because ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"AppModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is the root module. ",
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
                "value":" Most of the components assigned to routes are very simple.  However, you will notice that some have a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"loadChildren",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" property instead of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"component",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  This is because these components are lazily loaded",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"1,2",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  They are not shipped with the main application and are only loaded from the server when they need to be accessed. ",
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
                "value":" The prototype project has two modules: the root module that we have viewed already and a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ProfileModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" that contains components that are only accessible to signed in users.  If you look closely at the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"loadChildren",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" properties you can see that they specify the path to the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ProfileModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in the project.  This module will be lazily loaded when you follow either of these paths.  Here you can see the code for the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ProfileModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" (and that it specifies its own sub-paths): ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"TypeScript"
        },
        "value":"import { NgModule } from '@angular/core';\nimport { CommonModule } from '@angular/common';\nimport { ProfileComponent } from './profile.component';\nimport { PostComponent } from './post/post.component';\nimport { RouterModule } from \"@angular/router\";\n\n// Define routing within the feature module\nexport const routes = [\n    {path: '', component: ProfileComponent},\n    {path: 'post', component: PostComponent}\n];\n\n// Export both the ProfileComponent and PostComponent to outside the module\n@NgModule({\n    imports: [\n        CommonModule,\n        RouterModule.forChild(routes)\n    ],\n    declarations: [ProfileComponent, PostComponent],\n    exports: [ProfileComponent, PostComponent]\n})\nexport class ProfileModule { }\n",
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
                "value":" You may be wondering what these components consist of.  Right now they are just placeholders that specify a simple template that displays some text.  For example, here is the text for the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ProfileComponent",
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
            "language":"HTML"
        },
        "value":"<p>\n    Profile Component!\n</p>\n",
        "children":null
    },
    {
        "el":"h5",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"Lazy Loading In Action",
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
                "value":" Let's make sure that lazy loading is actually working.  When we first load the page, the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"HomeComponent",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is displayed as expected and we can see in the developer tools all the files that were loaded with our application. ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":{
            "id":"prelazy-image"
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
                    "class":"jarombek-blog-image",
                    "src":"https://asset.jarombek.com/posts/1-27-17-prelazy.png"
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
                "value":" Now if I click on the Profile tab, we will load the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ProfileComponent",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":". This component is lazily loaded.  As expected the webpage displays ‘Profile Component!' and if we look at the developer tools, another file has been loaded!  This contains all the code for the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ProfileModule",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" including the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ProfileComponent",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Lazy loading confirmed! ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":{
            "id":"postlazy-image"
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
                    "class":"jarombek-blog-image",
                    "src":"https://asset.jarombek.com/posts/1-27-17-postlazy.png"
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
                "value":" I have just begun my journey on this Angular 5 prototype but am excited to continue to learn!  It is all a bit overwhelming at the moment, but I know it will get easier from here.  You can check out the code from this discovery on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/mean-client-prototype/tree/v0.1"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"GitHub",
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

postViews = db.posts.findOne({name: "jan-27-2018-angular-5-routing"}).views;

db.posts.remove({name: "jan-27-2018-angular-5-routing"});

db.posts.insertOne({
    name: "jan-27-2018-angular-5-routing",
    title: "Angular 5 Routing & Lazy Loading Modules",
    date: new Date('2018-01-27T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Angular",
            picture: "https://asset.jarombek.com/logos/angular.png",
            color: "angular"
        },
        {
            name: "TypeScript",
            picture: "https://asset.jarombek.com/logos/ts.png",
            color: "typescript"
        },
        {
            name: "HTML",
            picture: "https://asset.jarombek.com/logos/html.png",
            color: "html"
        }
    ],
    content,
    sources: [
        {
            startName: "\"Angular Module Intro\", ",
            endName: "",
            linkName: "https://blog.angular-university.io/angular2-ngmodule/",
            link: "https://blog.angular-university.io/angular2-ngmodule/"
        },
        {
            startName: "\"Angular 2 lazy loading compiling error\", ",
            endName: "",
            linkName: "https://github.com/angular/angular-cli/issues/6246",
            link: "https://github.com/angular/angular-cli/issues/6246"
        }
    ]
});