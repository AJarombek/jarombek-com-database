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
                "value":" Yesterday I began to fix some bugs in my website ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/saints-xctf"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"saintsxctf.com",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and its corresponding native iOS and Android apps (because as we know a codebase is never complete).  I was looking at the Swift code for my iOS app and found a bug where I incorrectly selected a substring from a base64 encoded image.  Luckily the fix was easy to implement, but viewing the code made me remember just how confusing the String structure was in Swift. ",
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
                "value":" Yesterday I began to fix some bugs in my website ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/saints-xctf"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"saintsxctf.com",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" and its corresponding native iOS and Android apps (because as we know a codebase is never complete).  I was looking at the Swift code for my iOS app and found a bug where I incorrectly selected a substring from a base64 encoded image.  Luckily the fix was easy to implement, but viewing the code made me remember just how confusing the String structure was in Swift. ",
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
                "value":" You would think something as simple as a string would be straightforward, but in Swift there were a few quirks.  I wrote the code in Xcode 8 and Swift 3 this fall.  Since then Xcode 9 and Swift 4 have released, making ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" a bit less confusing.  I will dive into strings in Swift 3 and then look at how the code has changed with Swift 4. ",
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
                "value":"Swift 3",
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
                "value":" The first quirk of strings in Swift 3 is obvious when you try to get the length of a string.  There is no length or count variable on the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structure.  Instead each ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" has a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"characters",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable of type ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CharacterView",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" which is a collection of all the characters in the string",
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
                "value":".  You can use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"characters",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object to get the strings size or iterate over all the characters. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"var hello = \"hello my name is andy 😺\"\n\nhello.characters.count // 23\n\n// The characters struct is a collection of Character structs\nlet chars: String.CharacterView = hello.characters\n\n// You can peform operations on the characters collection just like any other collection\nchars.forEach {\n    char -> Void in\n\n    print(char)\n}\n",
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
                "value":"     Like any other collection, you can also perform functions such as filter, map, or sort on     ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"characters",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Of course array operators are also possible. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"// Filter out all 'a' characters\nvar filtered = chars.filter {$0 != \"a\"}\n\n// Remove and return the last character in collection\nlet catEmoji = filtered.popLast() // '😺'\n\n// Convert back to a string\nString(filtered)\n\nString(filtered[6...7]) // returns 'my'\n",
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
                "value":" One of the difficult operations to perform on strings in Swift is getting a substring.  In other languages its as simple as a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":null,
                "value":"substring()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function with two integer arguments specifying the start and end indexes.  Swift also has a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":null,
                "value":"substring()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" function except it accepts a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Range<String.Index>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structure.  This struct is constructed with two ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structures used together with a half-open range operator ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"..<",
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
                "value":" Okay, that already sounds a bit complicated.  The question you might be asking now is how to we get these ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structures?  We can get them by calling ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"index(String.Index, offsetBy: Int)",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" on our string",
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
                "value":". The following code sample gets two ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structs and then use them to get a substring.  You will notice that since the first parameter of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"index()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structure we can use the strings built in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"startIndex",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" variable, which is zero. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"var runDescription = \"I ran 2.12 miles at 6:47 pace on January 3rd, 2018\"\n\nlet start: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 6)\nlet end: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 10)\n\nlet range: Range<String.Index> = start..<end\n\nrunDescription.substring(with: range) // 2.12\n",
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
                "value":" So why is this ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" madness even necessary?  The answer has to do with the way Swift characters are stored, and in particular the possible complexities of using unicode scalars.  Characters in Swift are unicode scalars which are characters defined in Unicode",
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
                "value":".  The problem is that some of these scalars are complex and can be combined into one visible character.  The following code shows two equivalent characters, except one uses two unicode scalars and the other uses one",
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
                "value":". ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"let e1 = \"\\u{00EB}\" // \"ë\"\nlet e2 = \"\\u{0065}\\u{0308}\" // \"ë\"\n\nif e1 == e2 {\n    print(\"They are equal!\")\n}\n\nif e1.characters.count == e2.characters.count {\n    print(\"They both have a length of 1!\")\n}\n",
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
                "value":" This means that an index in the internal character structure may actually hold only a piece of a visible character",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"6",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Because of this, Swift makes this complexity obvious by implementing the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" struct.  Annoying sure, but it makes sense so that no newcomers to Swift will be tricked by the character implementation. ",
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
                "value":"Swift 4",
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
                "value":" In Swift 4 the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"characters",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" collection is deprecated and the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" struct has become a collection.  This makes strings easier since you can perform all collection operations directly on the string object. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"var hello = \"hello my name is andy 😺\"\n\nprint(hello.count) // 23\n\nhello.forEach {\n    char -> Void in\n\n    print(char)\n}\n\n// In Swift 4 since Strings are collections, we can filter directly on the String\nvar filtered = String(hello.filter { $0 != \"a\" })\n\nfiltered.removeLast() // '😺'\n",
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
                "value":" We still have to use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structure in Swift 4 (unfortunately for those who know the internal structure). However, we can avoid the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Range",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" structure since our string is a collection! ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"var runDescription = \"I ran 2.12 miles at 6:47 pace on January 3rd, 2018\"\n\nlet start: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 6)\nlet end: String.Index = runDescription.index(runDescription.startIndex, offsetBy: 9)\n\nlet miles = runDescription[start...end] // 2.12\n",
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
                "value":" Swift 4 also joins other modern languages (including JavaScripts ES6 standard) by introducing multi-line strings in code",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"7",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"! ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Swift"
        },
        "value":"let stats = \"\"\"\n    name: andy\n    date: 2018-01-03\n    miles: \\(miles)\n\"\"\"\n",
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
                "value":" Although the verbose ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String.Index",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" struct is still necessary in Swift 4, changing ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"String",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" into a collection and supporting multi-line strings are major upgrades that make switching to Swift 4 worth it!  The code in this discovery is up on my ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/master/2018/01-Jan/\n1-3-Swift-String"
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

postViews = db.posts.findOne({name: "jan-3-2018-string-swift"}).views;

db.posts.remove({name: "jan-3-2018-string-swift"});

db.posts.insertOne({
    name: "jan-3-2018-string-swift",
    title: "Strings in Swift 3 & 4",
    date: new Date('2018-01-03T12:00:00'),
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
    content,
    preview,
    sources: [
        {
            startName: "\"String.CharacterView\", ",
            endName: "",
            linkName: "https://developer.apple.com/documentation/swift/string.characterview",
            link: "https://developer.apple.com/documentation/swift/string.characterview"
        },
        {
            startName: "\"Range\", ",
            endName: "",
            linkName: "https://developer.apple.com/documentation/swift/range",
            link: "https://developer.apple.com/documentation/swift/range"
        },
        {
            startName: "\"index(_:offsetBy:)\", ",
            endName: "",
            linkName: "https://developer.apple.com/documentation/swift/string/1786175-index",
            link: "https://developer.apple.com/documentation/swift/string/1786175-index"
        },
        {
            startName: "Matthew Mathias & John Gallagher, ",
            endName: " (Atlanta, GA: Big Nerd Ranch, 2016), 63",
            linkName: "Swift Programming: The Big Nerd Ranch Guide",
            link: "https://www.bignerdranch.com/books/swift-programming/"
        },
        {
            startName: "\"Unicode Character Table\", ",
            endName: "",
            linkName: "https://unicode-table.com/en/#control-character",
            link: "https://unicode-table.com/en/#control-character"
        },
        {
            startName: "\"How does String.Index work in Swift\", ",
            endName: "",
            linkName: "https://stackoverflow.com/questions/39676939/how-does-string-index-work-in-swift",
            link: "https://stackoverflow.com/questions/39676939/how-does-string-index-work-in-swift"
        },
        {
            startName: "\"What’s New in Swift 4?\", ",
            endName: "",
            linkName: "https://www.raywenderlich.com/163857/whats-new-swift-4",
            link: "https://www.raywenderlich.com/163857/whats-new-swift-4"
        }
    ]
});