/**
 * Script for the MongoDB Shell.
 * @author Andrew Jarombek
 * @since 7/4/2018
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
                "value":" This is the second post of my introduction to the Groovy programming language.  I'm looking at  the basic syntax I find interesting before diving into more complex topics.   To learn about some of the basic operators Groovy offers check out ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/jul-2-2018-groovy-basics-pt1"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"part I",
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

content = [
    {
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" This is the second post of my introduction to the Groovy programming language.  I'm looking at  the basic syntax I find interesting before diving into more complex topics.   To learn about some of the basic operators Groovy offers check out ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://jarombek.com/blog/jul-2-2018-groovy-basics-pt1"
                },
                "value":null,
                "children":[
                    {
                        "el":"#text",
                        "attributes":null,
                        "value":"part I",
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
                "value":" This discovery post will explore Groovy's object oriented features.  As far as existing object oriented libraries are concerned, Groovy extends the Java object libraries in what is commonly referred to as the GDK (Groovy Development Kit)",
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
                "value":".  For example, the GDK gives more methods to the collections framework and specifically the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Collection",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" interface.  Now all collections can use the GDK's ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"each()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method to iterate over its contents with a Groovy closure.  I utilize some of the GDK enhancements in this post. ",
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
                "value":" Groovy also enhances the programmers ability to create classes, instantiate objects, and set object properties.  The trend of shortening Java's verbose syntax continues.  Now it's time to start building objects with Groovy! ",
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
                "value":"Groovy Classes",
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
                "value":" The following is a basic class that represents a person. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"import groovy.transform.ToString\n\n@ToString\nclass Person {\n  String first\n  String last\n\n  Person(String first, String last) {\n    this.first = first\n    this.last = last\n  }\n}\n",
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
                "value":" Groovy shortens the syntax that is necessary to create a simple POJO with getters and setters. All non-private fields on an object are automatically given getter and setter functions in Groovy. One of the most verbose Java constructs is the POJO, since a getter and setter function is needed for each field.  This issue gave rise to IDE support to automatically generate getters and setters along with libraries such as Lombok which favor annotations over getter/setter methods.  In Groovy all the work of getters and setters is done for you by default. ",
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
                "value":" Another thing to note about this class is the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@ToString",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation.  ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@ToString",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" allows for quick generation of a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"toString()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method for the class.  If you wanted any sophisticated string representation for object instances, you would have to implement the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"toString()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method yourself.  Groovy exposes many different annotations to attach on classes and methods which either enforce rules or simplify the object/method creation process.  You will see more annotations throughout this post. ",
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
                "value":" The following lines show some of the ways to instantiate objects in Groovy: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"def andy = new Person('Andrew', 'Jarombek')\nassert andy.toString() == 'Person(Andrew, Jarombek)'\n\nPerson tom = ['Thomas', 'Cauliflower']\nassert tom.toString() == 'Person(Thomas, Cauliflower)'\n",
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
                "value":" The first object constructor call with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"new",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" keyword will look familiar if you have used Java before.  For explicitly declared constructors in Groovy  the notation to instantiate an object can be the same as Java.  I will show you an example of a class with an implicit constructor soon. ",
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
                "value":" You can also instantiate an object in Groovy by assigning a list to a variable of the object type. The second variable definition above assigns a list to a variable statically typed to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Person",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  Groovy is smart enough to instantiate a new ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Person",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" object, passing the list elements to the constructor as arguments (in order).  At first glance I still think the explicit constructor call is more readable, but it's cool that Groovy gives multiple options for object construction. There is one final way to construct an object with a list - resulting in a dynamically typed variable instead of the statically typed one shown above",
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
                "value":" If you have read any of my other posts you will notice something a bit different about these examples.  In the past I always used print statements to display the results of my code samples. For this post I am using ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"assert",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" statements.  This was an idea I got from the book I'm reading on Groovy which recommended the approach ",
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
                "value":".  So far I do really like using them, and hopefully they are easy to follow. Basically the assumption is that the condition in every ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"assert",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" statement returns ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"true",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  You can think of assertions as inline test code. ",
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
                "value":" One final thing about this snippet is the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"==",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operator in Groovy.  In Java the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"==",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operator tests for identity. For objects this meant that both the objects tested for equality were pointing to the same location in memory.  If you simply wanted to test for equality of the objects structure and state, the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"equals()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method inherited from the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Object",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" class was used. ",
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
                "value":" Groovy does away with this form of equality check.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"==",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" operator is simply used to test that the two objects are of equal value, not that they are necessarily pointing to the same memory location",
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
                "value":".  While I understand that equality in Java can be confusing for newcomers, my first thought is that this approach will be confusing for codebases that mix Java and Groovy.  This is one of the big selling points of Groovy - you can call Groovy code from Java code and vice versa.  You can even have inheritance hierarchies that switch between Java and Groovy",
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
                "value":".  The fact that such a fundamental part of the languages take two entirely different approaches is concerning.  Time will tell in my use of Groovy if this problem is as big as I fear. ",
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
                "value":" My tangent on equality aside, let's get back to Groovy classes.  The following is another basic class that represents cats: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"import groovy.transform.ToString\n\n@ToString(includeNames = true)\nclass Cat {\n  String name\n  String breed\n\n  def sayHello() {\n    return \"$name says Meow!\"\n  }\n}\n",
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
                "value":" A few quick details about this class.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@ToString",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation is passed an argument which alters object instances string representations.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"sayHello()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method is given no access modifier because by default methods are declared as public.  Also there is no explicitly declared constructor. To construct an object of this class you have to pass named arguments to the default constructor. Let's construct this object and modify some of its properties. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"def lily = new Cat(name: 'Lily', breed: 'Russian Blue')\n\nassert lily.toString() == 'Cat(name:Lily, breed:Russian Blue)'\nassert lily.sayHello() == 'Lily says Meow!'\n\ndef joe = new Cat(name: 'Joe')\n\nassert joe.toString() == 'Cat(name:Joe, breed:null)'\n\nassert joe.getName() == 'Joe'\nassert joe.name == 'Joe'\n\njoe.setBreed('Maine Coon')\nassert joe.breed == 'Maine Coon'\n",
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
                "value":" Instances of ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"Cat",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" are constructed above with named arguments.  Also the getters/setters that were implicitly created by Groovy are called. Interestingly the calls to ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"joe.getName()",
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
                "value":"joe.name",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" perform the same exact operations behind the scenes.  ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"joe.name",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" actually calls the getter method and does not access the instance variable directly",
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
                "value":".  It is simply more concise syntax. ",
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
                "value":" The previous code samples show off Groovy's shortened syntax for objects and their blueprints. Annotations also play a major role in helping build classes.  I showed you the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@ToString",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation and how it automatically generates a ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"toString()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" method for you.  Annotations can also be used to completely change the behavior of classes and their instances. ",
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
                "value":" If you want to alter a class so all their instances are immutable, all you have to do in Groovy is declare the class with the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@Immutable",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation. Note: for this to work, all the fields in the class must be immutable types and methods must not mutate any variables after construction",
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
                "value":". ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"import groovy.transform.Immutable\n\n@Immutable\nclass ImmutablePerson {\n  String first\n  String last\n}\n",
        "children":null
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"import groovy.transform.Immutable\n\n@Immutable\nclass ImmutableCat {\n  String name\n  String breed\n  ImmutablePerson owner\n}\n",
        "children":null
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"def immutableEvan = new ImmutablePerson(first: 'Evan', last: 'Gravey')\ndef dotty = new ImmutableCat(name: 'dotty', breed: 'Bengal', owner: immutableEvan)\nassert dotty == new ImmutableCat(name: 'dotty', breed: 'Bengal', owner: immutableEvan)\n\ntry {\n  dotty.breed = 'Siamese'\n  assert false\n} catch (ReadOnlyPropertyException ex) {\n  assert ex.message == 'Cannot set readonly property: breed for class: ImmutableCat'\n}\n",
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
                "value":"assert false",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" will never get called in the code above since I am trying to change a field value on an immutable class.  The annotation has successfully made the object instance immutable! ",
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
                "value":"Compile Time Type Checking in Groovy",
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
                "value":" By default Groovy is a dynamic language and because of that it performs type checking at runtime. While this gives you a lot of power and expressiveness, it can also lead to problems.  Let's say you pass a variable of the wrong type as a return value from a method.  In a dynamically typed language, the type of the return value is only known at runtime.  This means that while the program executes, an error will occur.  This can be catastrophic and bring an entire program down. ",
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
                "value":" This is where strict languages really shine.  Instead of only knowing the types passed from methods at runtime, they know them even sooner - at compile time.  Now if a variable of the wrong type is passed as a response from a method in your code you will know before the program even builds.  In fact the program won't build - you will get a compile time error.  Identifying errors as soon as possible (at compile time instead of runtime) ranges from a nice feature to a mandatory requirement for a code base. ",
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
                "value":" Groovy sits in an interesting position because it supports both strict type checking and dynamic type checking!  You can \"activate\" compile time (strict) type checking on a method by method basis using the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@TypeChecked",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation",
                "children":null
            },
            {
                "el":"sup",
                "attributes":null,
                "value":"8",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  To demonstrate this annotation, I created a method to compute factorial numbers. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"assert factorial(-1) == -1\nassert factorial(0) == 1\nassert factorial(1) == 1\nassert factorial(2) == 2\nassert factorial(3) == 6\nassert factorial(4) == 24\n\n@TypeChecked\nstatic int factorial(int number) {\n  switch (number) {\n    case { int n -> (n < 0) }:\n      return -1\n      break\n    case 0..1:\n      return 1\n      break\n    default:\n      return computeFactorial(number)\n  }\n}\n\nstatic int computeFactorial(int number) {\n  int answer = 1\n  2.upto(number) { n ->\n    answer *= n\n  }\n\n  return answer\n}\n",
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
                "value":" One quick thing to note about these methods.  You probably noticed I called the method ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"upto()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" directly on an integer literal.  Unlike Java which has primitive values, all values are objects in Groovy - including literals.  That change in behavior is what makes it completely valid to call ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"upto()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" on the literal ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"2",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" ( ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"upto()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" is also a method defined in the GDK). ",
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
                "value":" With that quick explanation out of the way, what would happen if ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"factorial()",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" was written to return a ",
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
                "value":" by accident if the argument is less than 0? ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Groovy"
        },
        "value":"static int factorial(int number) {\n  switch (number) {\n    case { int n -> (n < 0) }:\n      return 'Not Found'\n      break\n    case 0..1:\n      return 1\n      break\n    default:\n      return computeFactorial(number)\n  }\n}\n",
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
                "value":" The following code compiles, but once it is run the following error occurs: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":null,
        "value":"org.codehaus.groovy.runtime.typehandling.GroovyCastException:\n  Cannot cast object 'Not Found' with class 'java.lang.String' to class 'int'\n",
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
                "value":" Ideally this mistake would be caught in the compile phase and the compilation would fail.  This behavior is possible if the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"@TypeChecked",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" annotation is added to the method ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"factorial()",
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
            "language":"Groovy"
        },
        "value":"@TypeChecked\nstatic int factorial(int number) {\n  switch (number) {\n    case { int n -> (n < 0) }:\n      return 'Not Found'\n      break\n    case 0..1:\n      return 1\n      break\n    default:\n      return computeFactorial(number)\n  }\n}\n",
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
                "value":" Now the error is at compile time, and the code is never run: ",
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
                "el":"img",
                "attributes":{
                    "class":"jarombek-blog-image",
                    "src":"https://asset.jarombek.com/posts/7-4-18-groovy-strict-type-check.png"
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
                "value":" I have never come across a language that supports both dynamic typing and strict typing like Groovy.  I am really interested to see what benefits this can have in production level code.  I am sure there are some use cases where it could be very helpful! ",
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
                "value":" That concludes my introduction to Groovy.  I do have a few concerns with the language - such as equality operations not matching those in Java, a lack of primitives, and potential over-reliance of annotations.  However, some of the benefits look fantastic as well.  Benefits include strict and dynamic type checking, shortened syntax, closures, and list and map literals. If you look at the sources for this post you will notice that I started reading a book on Groovy in depth.  I am really excited to learn more and will be sharing my discoveries on this website.  As always, all the source code is up on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/\njarombek-com-sources/blob/master/2018/07-jul/7-4-groovy-basics-pt2"
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

postName = "jul-4-2018-groovy-basics-pt2";
existingPost = db.posts.findOne({name: postName});

postViews = (existingPost) ? existingPost.views : 0;

db.posts.remove({name: postName});
db.posts_content.remove({name: postName});

db.posts.insertOne({
    name: postName,
    title: "Groovy Basics Part II: Object Oriented Features",
    description: `This discovery post will explore more of the object oriented features 
        Groovy has to offer`,
    date: new Date('2018-07-04T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "Groovy",
            picture: "https://asset.jarombek.com/logos/groovy.png",
            color: "groovy"
        },
        {
            name: "Java",
            picture: "https://asset.jarombek.com/logos/java.png",
            color: "java"
        }
    ], 
    preview,
    sources: [
        {
            startName: "Dierk König & Paul King, ",
            endName: ", 2nd ed (Shelter Island, NY: Manning, 2015), 48",
            linkName: "Groovy In Action",
            link: "https://www.manning.com/books/groovy-in-action-second-edition?"
        },
        {
            startName: "\"Constructors\", ",
            endName: "",
            linkName: "http://groovy-lang.org/objectorientation.html#_constructors",
            link: "http://groovy-lang.org/objectorientation.html#_constructors"
        },
        {
            startName: "",
            endName: ", 33",
            linkName: "König.",
            link: "https://www.manning.com/books/groovy-in-action-second-edition?"
        },
        {
            startName: "",
            endName: ", 32",
            linkName: "König.",
            link: "https://www.manning.com/books/groovy-in-action-second-edition?"
        },
        {
            startName: "",
            endName: ", 8",
            linkName: "König.",
            link: "https://www.manning.com/books/groovy-in-action-second-edition?"
        },
        {
            startName: "",
            endName: ", 37",
            linkName: "König.",
            link: "https://www.manning.com/books/groovy-in-action-second-edition?"
        },
        {
            startName: "\"[Groovy] Annotation Type Immutable\", ",
            endName: "",
            linkName: "http://docs.groovy-lang.org/latest/html/gapi/groovy/transform/Immutable.html",
            link: "http://docs.groovy-lang.org/latest/html/gapi/groovy/transform/Immutable.html"
        },
        {
            startName: "",
            endName: ", 52",
            linkName: "König.",
            link: "https://www.manning.com/books/groovy-in-action-second-edition?"
        }
    ]
});

db.posts_content.insertOne({
    name: postName,
    content
});