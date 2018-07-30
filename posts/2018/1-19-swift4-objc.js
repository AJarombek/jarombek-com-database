/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/28/2018
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
                "value":" Today I updated my iOS app ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/saints-xctf-ios"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"SaintsXCTF",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" from Swift 3 to Swift 4.  The process could not have been easier, with many of the automated conversions consisting of API changes and String struct upgrades.  There was however one change that had me confused - many of my functions were given a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@objc",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation.  So what is this mysterious annotation and why was it added to so many of my methods? ",
                "children":null
            }
        ]
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
                "value":" Today I updated my iOS app ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/saints-xctf-ios"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"SaintsXCTF",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" from Swift 3 to Swift 4.  The process could not have been easier, with many of the automated conversions consisting of API changes and String struct upgrades.  There was however one change that had me confused - many of my functions were given a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@objc",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation.  So what is this mysterious annotation and why was it added to so many of my methods? ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"@objc func womensXC(_ sender: UIView) {\n    os_log(\"Go to Women's Cross Country Page\", log: logTag, type: .debug)\n    loadGroup(withGroupname: \"wmensxc\")\n}\n",
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
                "value":"@objc",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation allows for functions to interact with Objective-C code",
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
                "value":".  Since many of Apple's APIs are built in Objective-C, a lot of my functions were actually interacting with non-Swift code without me even knowing!  This communication between Swift and Objective-C is called Interoperability and it lets you use Objective-C code in Swift and vice versa",
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
                "value":" Why is Swift 4 enforcing the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@objc",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation when Swift 3 didn't?  It looks like the Swift compiler used to infer the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@objc",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation for you, so you did not have to manually include it in your code",
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
                "value":". That has now changed in Swift 4. ",
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
                "value":" One of the reasons listed for enforcing the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@objc",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation is that in Swift 3 it was never obvious if the annotation would be inferred by the compiler.  I would argue however that having to use it in my code is more of a burden than a feature.  Now I am even more dependent on an IDE than before, since I often do not know if I am interacting with an Apple API written in Objective-C.  Some of my favorite languages are the ones that you can just write in a text editor (one of Java's major weak points in my opinion is that you are tied to an IDE), and Swift has now taken another step further away from that simplistic nature. ",
                "children":null
            }
        ]
    }
];

postName = "jan-19-2018-swift4-objc";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Swift 4 @objc Annotation",
    date: new Date('2018-01-19T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Swift",
            picture: "https://asset.jarombek.com/logos/swift.png",
            color: "swift"
        },
        {
            name: "Swift 3",
            picture: "https://asset.jarombek.com/logos/swift.png",
            color: "swift"
        },
        {
            name: "Swift 4",
            picture: "https://asset.jarombek.com/logos/swift.png",
            color: "swift"
        }
    ],
    preview,
    sources: [
        {
            startName: "\"when to use @objc in swift code?\", ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/30795117/when-to-use-objc-in-swift-code",
            link: "https://stackoverflow.com/questions/30795117/when-to-use-objc-in-swift-code"
        },
        {
            startName: "\"Interacting with Objective-C APIs\", ",
            endName: "",
            linkName: "https://developer.apple.com/library/content/documentation/Swift/Conceptual/BuildingCocoaApps/InteractingWithObjective-CAPIs.html",
            link: "https://developer.apple.com/library/content/documentation/Swift/Conceptual/BuildingCocoaApps/InteractingWithObjective-CAPIs.html"
        },
        {
            startName: "\"What's New in Swift 4?\", ",
            endName: "",
            linkName: "https://www.raywenderlich.com/163857/whats-new-swift-4#objc",
            link: "https://www.raywenderlich.com/163857/whats-new-swift-4#objc"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});