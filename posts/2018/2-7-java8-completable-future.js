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
                "value":" This post is going to consist of first impressions of Java 8's new ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class, which builds on top of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Future",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class introduced in Java 5",
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
                "value":".  A ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" represents an operation in progress that when completed will contain a value",
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
                "value":".  We can use this class to create asynchronous code that won't block during long running tasks.  My initial impression is that a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is Java's version of a JavaScript ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Promise",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object.  You define an operation to perform once the value is resolved asynchronously, and if needed can chain these operations. ",
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
                "value":" I created a simple example API to test it out.  The API contains a list of programming languages that you can ask for information about. The operation of getting a language takes a long time to complete (to simulate a possible long running network call) so we will use a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" to deal with its value once it arrives.  In the meantime, we will be able to perform other operations. ",
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
                "value":" Here is the APIs class: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public class LanguageAPI {\n\n    private static Random random =  new Random();\n    private Map<String, Language> languages;\n\n    public LanguageAPI() {\n\n        // Populate the languages map\n        languages = Stream.of(\n            new Language(\"Java\", LocalDate.of(1995, Month.MAY, 23), \"9.0\", \"Object Oriented\"),\n            new Language(\"JavaScript\", LocalDate.of(1995, Month.DECEMBER, 4),\n                            \"9.0\", \"Prototype Based\"))\n            .collect(toMap(Language::getName, e -> e));\n    }\n\n    /**\n    * Get one of the languages stored in the API with a delay\n    * @param name - the name of the language\n    * @return - the language object\n    */\n    public Language getLanguage(String name) {\n        randomDelay();\n        return languages.get(name);\n    }\n\n    /**\n    * Create a random delay between 0-3 seconds\n    */\n    private void randomDelay() {\n        double delay = random.nextDouble() * 3_000;\n        System.out.println(\"The delay will be \" + delay + \" ms\");\n        try {\n            Thread.sleep((long) delay);\n        } catch (InterruptedException e) {\n            e.printStackTrace();\n        }\n    }\n}\n",
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
                "value":" You can see that we initialize the API's language values in the constructor.  I also used Java 8's nice new Date and Time API to specifies the languages creation date. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public class Language {\n\n    private String name;\n    private LocalDate created;\n    private String version;\n    private String paradigm;\n}\n",
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
                "value":"     Now to use this API on the client side: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public class APIClient {\n\n    private static LanguageAPI languageAPI = new LanguageAPI();\n\n    /**\n    * Get a language asynchronously.\n    * When the language object is finally received, print it\n    * @param name - the language name\n    * @return the CompletableFuture\n    */\n    public static CompletableFuture<Void> getLanguageAsync(String name) {\n\n        // Start up a new thread and perform the language APIs getLanguage()\n        // method asynchronously\n        CompletableFuture<Language> futureLanguage =\n            CompletableFuture.supplyAsync(() -> languageAPI.getLanguage(name));\n\n        // Print out the Language once the futures value is supplied\n        return futureLanguage.thenAccept(System.out::println);\n    }\n\n    public static void main(String... args) {\n\n        // Get the Java language asynchronously\n        CompletableFuture<Void> future = getLanguageAsync(\"Java\");\n        System.out.println(\"Finding Java...\");\n\n        // Wait for the future to complete before ending the Java process\n        future.join();\n    }\n}\n",
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
                "value":" Since the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"getLanguage()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method takes so long to complete, I wrapped it in a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  To do this I use ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"'s ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"supplyAsync()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method.  This method accepts a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Supplier",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" that will be asynchronously executed.  I will put the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"getLanguage()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method in this ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Supplier",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Once the value is resolved I will print out its value using the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"thenAccept()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method. The contents of this method are executed once the value exists, so I pass it the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"System.out::println",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method reference. ",
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
                "value":" If you run this code, you will see that 'Finding Java...' will be printed out before the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Langauge",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object is resolved and printed.  Asynchronous code at work! ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"# Finding Java...\n# The delay will be 2083.7637884812107 ms\n# Language(name=Java, created=1995-05-23, version=9.0, paradigm=Object Oriented)\n",
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
                "value":" I haven't tried more complex examples, but ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" does seem like a nice new addition to Java.  In the past when I had to perform asynchronous code in Java I used Android libraries or the Spring framework, but I can see myself using ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"CompletableFuture",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":"s in my code in the future!  The code for this discovery is on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-submittions/blob/master/\nDiscoveries/2018/02-Feb/2-7-Java-Completable-Future/Source"
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

db.posts.remove({name: "feb-7-2018-java8-completable-future"});

db.posts.insertOne({
    name: "feb-7-2018-java8-completable-future",
    title: "Java 8 Completable Future",
    date: new Date('2018-02-07T12:00:00'),
    type: "Discovery",
    tags: [
        {
            name: "Java",
            picture: "./assets/java.png",
            color: "java"
        },
        {
            name: "Java 8",
            picture: "./assets/java8.png",
            color: "java"
        }
    ],
    content,
    sources: [
        {
            startName: "Raoul-Gabriel Urma, Mario Fusco &amp; Alan Mycroft, ",
            endName: " (Shelter Island, NY: Manning, 2015), 251",
            linkName: "Java 8 In Action",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 252",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        }
    ]
});