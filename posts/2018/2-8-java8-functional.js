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
                "value":" With all the functional additions to Java 8, I do think it is important to try to define what it means for a program to be 'functional.'  The end of the book I was reading, ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://www.manning.com/books/java-8-in-action"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"Java 8 In Action",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", tried to explain exactly that. ",
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
                "value":" It is hard however to fully explain functional programming with Java since it doesn't follow a pure functional style.  Instead Java 8 tries to take pieces of what makes functional programming great and incorporate it into Java's existing imperative and object oriented paradigm. ",
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
                "value":" With all the functional additions to Java 8, I do think it is important to try to define what it means for a program to be 'functional.'  The end of the book I was reading, ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://www.manning.com/books/java-8-in-action"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"Java 8 In Action",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", tried to explain exactly that. ",
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
                "value":" It is hard however to fully explain functional programming with Java since it doesn't follow a pure functional style.  Instead Java 8 tries to take pieces of what makes functional programming great and incorporate it into Java's existing imperative and object oriented paradigm. ",
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
                "value":" My experience with functional programming is extremely limited.  The only true functional programming that I wrote was with ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://en.wikipedia.org/wiki/Racket_(programming_language)"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"Racket",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" back during the winter of Junior year in college (a few months before I really got invested in programming and software development).  So I don't remember much! ",
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
                "value":" Functional programming, like mathematics, is focused around the function.  This function takes a variable number of arguments and returns a variable number of results.  Most importantly however is that the function has zero side effects",
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
                "value":".  This means that each time the function is called with the same arguments, the same result will occur.  Also no variables of the program can be mutated. They all must be effectively final.  You also can't output to a log file or throw exceptions",
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
                "value":". These are a lot of things you can't do, but the result is a function that will not cause unexpected errors in the rest of your code.  It is very predictable. ",
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
                "value":" Functional style programming in Java is not quite this strict.  I don't think anyone will complain if your program outputs some information to a log file for debugging.  However, reducing the number of variables your function mutates and removing exceptions can be seen as beneficial.  Predictable functions are nice to work with and will help future developers who work on your code! ",
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
                "value":" Elimination null errors in your code is very simple now with Java 8's ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Optional",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class!  I looked into optionals in detail in a prior ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/jan-30-2018-java8-optionals"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"discovery post",
                        "children":null
                    }
                ]
            },
            {
                "el":"#text",
                "attributes":null,
                "value":", but you could easily express an invalid condition in functions with optionals. For example, the book I read discusses a divide by zero scenario: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public class Functional {\n\n    public static Optional<Double> divide(Double numerator, Double denominator) {\n\n        // Wrap arguments into optionals\n        Optional<Double> num = Optional.ofNullable(numerator);\n        Optional<Double> den = Optional.ofNullable(denominator);\n\n        // Return division result if arguments exist and the denominator isn't zero,\n        // otherwise return empty optional\n        return (!num.isPresent() || !den.isPresent()) ?\n                Optional.empty() : (den.get() == 0) ?\n                Optional.empty() : Optional.of(num.get() / den.get());\n    }\n\n    public static void main(String... args) {\n        Optional<Double> success = divide(5.0, 2.0);\n        Optional<Double> fail = divide(5.0, 0.0);\n        Optional<Double> failBecauseNull = divide(null, 6.5);\n\n        success.ifPresent(System.out::println);\n\n        // Both of these Optionals will be empty\n        if (!fail.isPresent()) {\n            System.out.println(\"Can't Divide By Zero\");\n        }\n\n        if (!failBecauseNull.isPresent()) {\n            System.out.println(\"Can't Pass an Empty Value\");\n        }\n    }\n}\n",
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
                "value":" Here we first try to divide 5 by 2, then 5 by 0, and finally the dreaded ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"null",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" by 6.5.  With optionals, we do not have to throw any errors here.  Instead the client of our APIs function simply checks the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Optional",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" for a value.  Our first valid division will contain a result while the other two will return ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Optional.empty",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  The output of this program is as follows: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"# 2.5\n# Can't Divide By Zero\n# Can't Pass an Empty Value\n",
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
                "value":" Functional programming also often utilizes recursion to avoid mutating variables.  The only issue in Java with recursion is that each time a recursive function is called, a new stack frame is created ",
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
                "value":".  This can cause overflows for large operations.  Java also does not have tail recursion optimizations in place, so you can't be smart and avoid stack frame creation",
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
                "value":" There are other functional practices that you can use in Java that I won't go into here, but they will be exciting for me to try out in the future.  While Java 8 doesn't introduce pure functional programming, it does give you more tools to create code without unwanted side effects! ",
                "children":null
            }
        ]
    }
];

postName = "feb-8-2018-java8-functional";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "What Is Functional Programming in Java 8?",
    date: new Date('2018-02-08T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Java",
            picture: "https://asset.jarombek.com/logos/java.png",
            color: "java"
        },
        {
            name: "Java 8",
            picture: "https://asset.jarombek.com/logos/java8.png",
            color: "java"
        },
        {
            name: "Functional Programming"
        }
    ],
    preview,
    sources: [
        {
            startName: "Raoul-Gabriel Urma, Mario Fusco &amp; Alan Mycroft, ",
            endName: " (Shelter Island, NY: Manning, 2015), 295",
            linkName: "Java 8 In Action",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 296",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 302",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 303",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});