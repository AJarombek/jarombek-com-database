/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 4/29/2018
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
                "value":" For anyone who has ever written code in Java (or most imperative and object oriented languages) you know the pain of dealing with ",
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
                "value":" values. These ",
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
                "value":" values are especially painful in Java since they throw a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"NullPointerException",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" whenever we want to perform operations on them.  When we see a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"NullPointerException",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in our code, the usual fix is to perform a null check similar to the following code snippet: ",
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
                "value":" has some other issues besides uglifying our code by littering it with null checks.  As explained in a book I was reading recently on Java 8, ",
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
                "value":" actually doesn't follow the statically typed system that Java uses",
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
                "value":".  It is the wrong way of representing the absence of a value. ",
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
                "value":" For anyone who has ever written code in Java (or most imperative and object oriented languages) you know the pain of dealing with ",
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
                "value":" values. These ",
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
                "value":" values are especially painful in Java since they throw a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"NullPointerException",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" whenever we want to perform operations on them.  When we see a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"NullPointerException",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" in our code, the usual fix is to perform a null check similar to the following code snippet: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"String str = \"Hello\";\n\nif (str != null) {\n    hello.toUpperCase();\n}\n",
        "children":null
    },
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
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
                "value":" has some other issues besides uglifying our code by littering it with null checks.  As explained in a book I was reading recently on Java 8, ",
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
                "value":" actually doesn't follow the statically typed system that Java uses",
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
                "value":".  It is the wrong way of representing the absence of a value. ",
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
                "value":" Upgrades in Java 8 allow you to implement missing values in a statically typed manner with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Optional<T>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class. The ",
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
                "value":" object wraps a value, which can be either present or absent (an absent value is simply ",
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
                "value":").  Now instead of performing null checks, you can simply interact with the ",
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
                "value":" class. ",
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
                "value":" In this discovery I am going to build an API for athletes in Java using the ",
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
                "value":" class for null values.  There are a couple of methods in the ",
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
                "value":" class that we need to be familiar with before getting started: ",
                "children":null
            }
        ]
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"empty",
            "iscode":"true"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"static Optional<T> empty()",
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
                "value":" The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"empty()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" static method returns an empty ",
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
                "value":" instance.  This is the equivalent of a ",
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
                "value":" value in a strictly typed setting. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"// Represent an Optional with an absent value\nOptional<String> empty = Optional.empty();\nSystem.out.println(empty); // Optional.empty\n",
        "children":null
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"of",
            "iscode":"true"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"static Optional<T> of(T value)",
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
                "value":" Wraps the argument value in the ",
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
                "value":" object.  However, if you pass ",
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
                "value":" to the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"of()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" static method it will throw a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"NullPointerException",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Only use ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"of()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" if you know that the value being passed is not ",
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
                "value":". ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"// If the Optional value is present, perform the Consumer method reference\nOptional<String> present = Optional.of(\"I am not null!\");\npresent.ifPresent(System.out::println); // I am not null!\n",
        "children":null
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"ofNullable",
            "iscode":"true"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"static Optional<T> ofNullable(T value)",
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
                "value":" Performs the same operation as ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"of()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" except if the value is absent no error will be thrown and an empty ",
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
                "value":" object will be returned. ",
                "children":null
            }
        ]
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"ifPresent",
            "iscode":"true"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"void ifPresent(Consumer<T> consumer)",
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
                "value":" If an optional's value is present, this method will perform the actions specified by the consumer argument. This can be a lambda function or method reference. ",
                "children":null
            }
        ]
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"isPresent",
            "iscode":"true"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"boolean isPresent()",
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
                "value":" Returns whether the optional's value is present. ",
                "children":null
            }
        ]
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"orElse",
            "iscode":"true"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"T orElse(T other)",
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
                "value":" If the optional's value is present, return its value.  Otherwise, return the value specified as an argument. ",
                "children":null
            }
        ]
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"athlete api"
        },
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"Athlete API",
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
                "value":" Now let's take a quick look at the API.  If you want the full code it is available on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/tree/master/2018/01-Jan/1-30-Java8-Optionals/\noptionals"
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
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"Required class"
        },
        "value":null,
        "children":[
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-header-code"
                },
                "value":"Required",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class",
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
                "value":" The first thing I created for this API was a helper class for dealing with values that should not be ",
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
                "value":".  When calling a getter or setter on a POJO, there is nothing really stopping users from entering an empty value by default.  You can however add some validation logic to make sure that the value passed is not empty.  If it is equal to ",
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
                "value":" you can either throw an error or assign that variable some default value.  This code would probably contain some null checks like discussed earlier.  We don't want our API littered with these checks! ",
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
                "value":" Instead we will use optionals to check for value existence.  The following class is used in my getters and setters to check for value existence.  You can see that if the value entered is ",
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
                "value":" we throw an error using a variation of the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"orElse()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method called ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"orElseThrow()",
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
        "value":"public class Required<T> {\n\n    /**\n    * Validate that a value exists otherwise throw an error\n    * @param input - the value to check for existence\n    * @return The input value if it exists\n    */\n    public T error(T input) {\n        return Optional.ofNullable(input)\n            .orElseThrow(() -> new Error(\"Unable to Set Required Field to null\"));\n    }\n\n    /**\n    * Validate that a value exists otherwise throw an error\n    * @param input - the value to check for existence\n    * @param name - the name of the value for error messaging purposes\n    * @return The input value if it exists\n    */\n    public T error(T input, String name) {\n        return Optional.ofNullable(input)\n            .orElseThrow(() -> new Error(\"Unable to Set Required Field '\" + name + \"' to null\"));\n    }\n}\n",
        "children":null
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"Athlete class"
        },
        "value":null,
        "children":[
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-header-code"
                },
                "value":"Athlete",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class",
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
                "value":" The athlete class is the heart of the API.  It holds information such as the athletes name, age, team, personal records, and exercise history.  All of this information is contained in instance variables, some of which are optional. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public class Athlete {\n\n    // Instance variables exposed via getters and setters for the API\n    private String firstName;\n    private String lastName;\n    private Optional<Integer> age;\n    private Optional<String> team;\n\n    private ArrayList<Log> logs = new ArrayList<>();\n    private HashMap<String, String> prs = new HashMap<>();\n\n    ...\n}\n",
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
                "value":" If you had this code in an IDE you would probably get a complaint on the ",
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
                "value":" instance variables.  This is because the new ",
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
                "value":" class was not meant to be used as a field type. It also does not implement the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Serializable",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" interface, which could cause issues in some applications",
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
                "value":".  In most cases however I fall among the group of developers that think using the ",
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
                "value":" class with instance variables can greatly increase the readability of an API.  Just be aware of its limitations! ",
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
                "value":" Next I have two athlete constructors: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public Athlete(String firstName, String lastName) {\n    this.firstName = Optional.ofNullable(firstName).orElse(\"Foo\");\n    this.lastName = Optional.ofNullable(lastName).orElse(\"Bar\");\n}\n\npublic Athlete(String firstName, String lastName, Integer age) {\n    this.firstName = Optional.ofNullable(firstName).orElse(\"Foo\");\n    this.lastName = Optional.ofNullable(lastName).orElse(\"Bar\");\n    this.age = Optional.ofNullable(age);\n}\n",
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
                "value":" For the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"age",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" parameter I simply wrap it in an ",
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
                "value":" because it's instance variable is of type ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Optional<Integer>",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  For the other two parameters I am assigning them to concrete types.  Therefore I want to check if their value is empty, and if it is give the instance variable a default value.  This is where the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"orElse()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" methods come in. ",
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
                "value":" After the constructors I have getters and setters for the instance variables.  Most of them are not interesting, but here are a few that utilize the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Required",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class that I showed off earlier.  These setters check for the existance of a variable, and if it is empty throw an error. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"private Required<String> requiredString = new Required<>();\nprivate Required<ArrayList<Log>> requiredArrayList = new Required<>();\nprivate Required<HashMap<String, String>> requiredHashMap = new Required<>();\n\npublic void setFirstName(String firstName) {\n    this.firstName = requiredString.error(firstName, \"firstName\");\n}\n\npublic void setLastName(String lastName) {\n    this.firstName = requiredString.error(lastName, \"lastName\");\n}\n\npublic void setLogs(ArrayList<Log> logs) {\n    this.logs = requiredArrayList.error(logs, \"logs\");\n}\n\npublic void setPrs(HashMap<String, String> prs) {\n    this.prs = requiredHashMap.error(prs, \"prs\");\n}\n",
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
                "value":" I also have a method that allows you to add a personal record to the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"prs",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" hash map.  It utilizes short circuiting and the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"isPresent()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method to add the new PR if both of the functions arguments exist and returns whether or not the value was added successfully. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"/**\n* Add a new PR to the hash map for this athlete\n* @param event - the event name\n* @param time - the time of the PR\n* @return whether or not the PR was added\n*/\npublic boolean addPr(String event, String time) {\n\n    // Last get() is necessary because put() returns null if the keys previous value was null\n    return (Optional.ofNullable(event).isPresent() && Optional.ofNullable(time).isPresent() &&\n            (Optional.ofNullable(this.prs.put(event, time)).isPresent() ||\n             Optional.ofNullable(this.prs.get(event)).isPresent()));\n}\n",
        "children":null
    },
    {
        "el":"sectiontitle",
        "attributes":{
            "title":"Main Class"
        },
        "value":null,
        "children":[
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-header-code"
                },
                "value":"Main",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class",
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
                "value":" The main class executes our API.  The only new piece here is the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"ifPresent()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method which I use to print out the value if it exists (and avoid a null check!). ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Java"
        },
        "value":"public static void main(String... args) {\n    Athlete athlete = new Athlete(\"andy\", \"jarombek\");\n\n    athlete.setAge(Optional.of(22));\n\n    // athlete.setFirstName(null); - Unable to Set Required Field 'firstName' to null\n    athlete.setFirstName(\"andrew\");\n\n    // Check to see if adding PR's are successful\n    boolean validPR = athlete.addPr(\"5000m\", \"15:27\");\n    boolean invalidPR = athlete.addPr(null, \"15:27\");\n\n    System.out.println(\"Valid PR Succeeded: \" + validPR);\n    System.out.println(\"Invalid PR Succeeded: \" + invalidPR);\n\n    // Safely try and get a value from a map with Optionals\n    Optional<String> value = Optional.ofNullable(athlete.getPrs().get(\"5000m\"));\n    value.ifPresent(System.out::println);\n\n    System.out.println(athlete);\n\n    Athlete noNameAthlete = new Athlete(null, null);\n    System.out.println(noNameAthlete);\n}\n",
        "children":null
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Bash"
        },
        "value":"Valid PR Succeeded: true\nInvalid PR Succeeded: false\n15:27\nAthlete(firstName=andrew, lastName=jarombek, age=Optional[22], prs={5000m=15:27} ...)\nAthlete(firstName=Foo, lastName=Bar, age=null, prs={}, ...)\n",
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
                "value":" I really am liking the new ",
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
                "value":" class in Java.  I have already started using it in some of my Java code! ",
                "children":null
            }
        ]
    }
];

postName = "jan-30-2018-java8-optionals";
postViews = db.posts.findOne({name: postName}).views;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Java 8 Optionals",
    date: new Date('2018-01-30T12:00:00'),
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
        }
    ],
    preview,
    previewString: JSON.stringify(preview),
    sources: [
        {
            startName: "Raoul-Gabriel Urma, Mario Fusco &amp; Alan Mycroft, ",
            endName: " (Shelter Island, NY: Manning, 2015), 228",
            linkName: "Java 8 In Action",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 240",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/java-8-in-action"
        },
        {
            startName: "",
            endName: ", 236",
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