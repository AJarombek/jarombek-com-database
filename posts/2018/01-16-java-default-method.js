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
                "value":" Java was originally designed to not support multiple inheritance of implemented methods.  Because of this, you could only extend one class.  However, you could simulate multiple inheritance by implementing multiple interfaces.  The only catch here was that the methods defined in the interfaces had no body and had to be created in the implementing class. ",
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
                "value":" This design avoided many issues that come with multiple inheritance such as the diamond problem.  However, there were some issues that came with using interfaces for an API as well. ",
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
                "value":" Java was originally designed to not support multiple inheritance of implemented methods.  Because of this, you could only extend one class.  However, you could simulate multiple inheritance by implementing multiple interfaces.  The only catch here was that the methods defined in the interfaces had no body and had to be created in the implementing class. ",
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
                "value":" This design avoided many issues that come with multiple inheritance such as the diamond problem.  However, there were some issues that came with using interfaces for an API as well. ",
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
                "value":" Let's say you needed to change an API that uses an interface.  If you add a new method definition to an interface, all classes that implement this interface also have to implement the new method.  If you have control over the implementing class this is an easy fix, but it is a very real possibility that someone else has a class that implements your interface.  This will break existing code! ",
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
                "value":" Java 8 has a fix for this called default methods.  Now interfaces can add bodies to methods specified with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"default",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keyword.  If this method does not exist in the implementing class, the default method in the interface is called instead!  Now existing code will not break on interface changes! ",
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
                "value":" But wait!  What is the difference between an abstract class and an interface with default methods?  Although both can now have method bodies, there are still other differences between the two.  First off a class can still only extend one abstract class but can implement multiple interfaces with default methods.  Second abstract classes can have instance variables while an interface still does not have this ability",
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
                "value":" Okay so now that one concern is answered, what about multiple inheritance?  Now you can inherit methods with multiple implementations right?  This is true, but now Java has added some rules to deal with situations where a class has multiple method bodies to choose from. ",
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
                "value":" First off, if one of the method bodies comes from a class and the rest come from default methods, the classes implementation is picked.  Second, multiple default methods exist on different levels of the inheritance chain, the implementation in the interface closest to the class along the chain is picked.  If neither of these conditions are met, the class has to explicitly select which method body to use",
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
                "value":" Now lets look at an example that shows the new default methods and what occurs in a diamond problem scenario. The interface hierarchy below represents animals: ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":{
            "id":"diamond-uml-image"
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
                    "src":"https://asset.jarombek.com/diamond-uml.png"
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
                "value":" Here is the code for each entity in the diagram (side note: you can also specify ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"static",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method implementations in interfaces with Java 8): ",
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
                "value":"     Animal.java ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public interface Animal {\n\n    String aboutMe();\n\n    /**\n    * Default method will be called if this method doesn't exist in the implemented class\n    * @return the age of the animal\n    */\n    default int age() {\n        return 0;\n    }\n\n    /**\n    * In Java 8 interfaces can have static methods\n    * @return a description of this animal\n    */\n    static String info() {\n        return \"I am an animal\";\n    }\n}\n",
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
                "value":"     LivingAnimal.java ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public interface LivingAnimal extends Animal {\n    void run();\n    void walk();\n    void sleep();\n    void eat();\n\n    default String status() {\n        return \"I am a living animal!\";\n    }\n}\n",
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
                "value":"     Pet.java ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public interface Pet extends Animal {\n    String owner();\n\n    default String status() {\n        return \"I am a pet!\";\n    }\n}\n",
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
                "value":"     Cat.java ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public class Cat implements LivingAnimal, Pet {\n\n    private String name;\n    private String owner;\n\n    public Cat(String name) {\n        this.name = name;\n    }\n\n    public Cat(String name, String owner) {\n        this.name = name;\n        this.owner = owner;\n    }\n}\n",
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
                "value":"     Main.java ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public class Main {\n\n    public static void main(String... args) {\n        Cat snickers = new Cat(\"Snickers\", \"Caroline D\");\n\n        int age = snickers.age();\n        System.out.println(age);\n\n        String status = snickers.status();\n        System.out.println(status);\n    }\n}\n",
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
                "value":" When we try to run the code right now, we get the following error: ",
                "children":null
            }
        ]
    },
    {
        "el":"figure",
        "attributes":{
            "id":"error-message-image"
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
                    "src":"https://asset.jarombek.com/error-message.png"
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
                "value":" The reason for the error is that we forgot to explicitly select which method body to implement in Cat.java for ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"status()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Let's fix this issue and explicitly call the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Pet",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" interface implementation",
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
                "value":": ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"...\n\n@Override\npublic String status() {\n    return Pet.super.status();\n}\n",
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
                "value":" Now when we run the code we get the result of age = 0 and status = 'I am a pet!'. ",
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
                "value":" For some of us this may be the expected result.  For others with diamond problem experience an error may be expected.  Although I don't really know C++ (yet...) I know that having diamond inheritance like this example will cause an error.  The reason why Java doesn't complain here has to do with the way Java is implemented.  Instead of both ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"LivingAnimal",
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
                "value":"Pet",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" having a copy of the default method specified in ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Animal",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" as they would in C++, Java knows that there is actually only one existence of the default method ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"age()",
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
                "value":". Therefore no conflict exists! ",
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
                "value":" Working with API's got a lot easier to deal with in Java 8.  You can check out all the code for this discovery ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/master/2018/01-Jan/\n1-16-Java-Default-Method/defaultmethods"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"here",
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

postName = "jan-16-2018-java-default-method";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Java 8 Default Method",
    date: new Date('2018-01-16T12:00:00'),
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
            name: "Inheritance"
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "Raoul-Gabriel Urma, Mario Fusco &amp; Alan Mycroft, ",
            endName: " (Shelter Island, NY: Manning, 2015), 214",
            linkName: "Java 8 In Action",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 219",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 222",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 223",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content, 
    contentString: JSON.stringify(content) 
});