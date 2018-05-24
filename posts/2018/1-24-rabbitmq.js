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
                "value":" Recently for work I've looked into RabbitMQ, a message broker that you can use to communicate between different parts of your application.  An analogy I really liked is that RabbitMQ is like putting a post office in your application - you can have producers put messages in the post offices queue and have these messages routed to consumers",
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
                "value":" The configuration for RabbitMQ can be written in any language with a RabbitMQ library (which consists of most languages you know).  This is extremely powerful since you could have different pieces of the RabbitMQ channel in different languages.  For example, let's say your RabbitMQ server has one producer and three consumers.  Your message producer might be written in Java, while your consumers are written in JavaScript, Python, and PHP.  Imagine all the different possibilities of sending RabbitMQ messages across applications! ",
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
                "value":" In order to get to know RabbitMQ better, I made an enhanced hello world example where producers can send JSON representing running logs to different consumers.  All the code is in Python, since the RabbitMQ Python API is very short and sweet!  Let's take a look! ",
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
                "value":"Producer",
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
                "value":" My producer starts by taking command line arguments and converting them to JSON.  This will be the body of the message sent across the RabbitMQ channel.  It will also take arguments to form the routing key, which in context of the post office analogy is like the mailing address. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Python"
        },
        "value":"# The ArgumentParser API is used for specifying command line arguments\n# description - a text description shown when you use the --help argument\nparser = ArgumentParser(description='Upload a New Log')\n\n# Arguments that specify the routing keys\n# dest - the destination variable to hold an array of routing keys\n# const - the string that will be placed in the dest array if the argument is used\nparser.add_argument(\"-WXC\", action=\"append_const\", dest=\"routing_keys\", const=\"womensxc\",\n                    default=[], help=\"use the woman's cross country routing key\")\nparser.add_argument(\"-WTF\", action=\"append_const\", dest=\"routing_keys\", const=\"womenstf\",\n                    default=[], help=\"use the woman's track & field routing key\")\nparser.add_argument(\"-MXC\", action=\"append_const\", dest=\"routing_keys\", const=\"mensxc\",\n                    help=\"use the men's cross country routing key\")\nparser.add_argument(\"-MTF\", action=\"append_const\", dest=\"routing_keys\", const=\"menstf\",\n                    help=\"use the men's track & field routing key\")\nparser.add_argument(\"-ALUM\", action=\"append_const\", dest=\"routing_keys\", const=\"alumni\",\n                    help=\"use the alumni routing key\")\n\n# Specify all the command line arguments that specify running log JSON properties\n# The first argument is the command line short form, and the second argument is the long form\n# help - description displayed when you use the --help argument\nparser.add_argument(\"-n\", \"--name\", help=\"the name of the runner\")\nparser.add_argument(\"-d\", \"--distance\", type=float, help=\"the distance run\")\nparser.add_argument(\"-m\", \"--metric\", choices=[\"miles\", \"kilometers\", \"meters\"],\n                    help=\"the distance run metric\")\nparser.add_argument(\"-t\", \"--time\", help=\"the time taken on the run\")\nparser.add_argument(\"-dt\", \"--date\", help=\"the date of the run 'yyyy-mm-dd'\")\nparser.add_argument(\"-l\", \"--location\", help=\"the location of the run\")\nparser.add_argument(\"-des\", \"--description\", help=\"a description of the run\")\n\n# Parse the command line arguments into the args object with the arguments as variables\nargs = parser.parse_args()\n\n# Build the JSON object to send across Rabbit\njson_object = dict()\njson_object['name'] = args.name\njson_object['distance'] = args.distance\njson_object['metric'] = args.metric\njson_object['time'] = args.time\njson_object['date'] = args.date\njson_object['location'] = args.location\njson_object['description'] = args.description\n\njson_data = json.dumps(json_object)\n",
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
                "value":" Now we need to generate a routing key to use for this message.  Routing keys in RabbitMQ are strings separated by ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":".",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" symbols.  For example, a key could be ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"foo.bar",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":".  These keys are used in the topic exchange type, where each consumer specifies a routing key pattern to subscribe to",
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
        "el":"codesnippet",
        "attributes":{
            "language":"Python"
        },
        "value":"def build_routing_key(key_list):\n    \"\"\" Build up the routing key from a list of routes \"\"\"\n\n    str = \"\"\n\n    for i in range(len(key_list)):\n        if i != 0:\n            str += \".\"\n\n        str += key_list[i]\n\n    return str\n\nrouting_key = build_routing_key(args.routing_keys)\n",
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
                "value":" Now we need to create a connection to RabbitMQ.  First lets use the command line to create two new RabbitMQ users, one for consuming and one for producing.  The following example gives both users full permissions to configure, read, and write to RabbitMQ  across all queues and exchanges.  The ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"\".*\"",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" pattern is what matches to any queue or exchange, with the first for configuring, the second for reading, and the last for writing",
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
                "value":". ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Python"
        },
        "value":"# Add a new RabbitMQ user to consume messages\nrabbitmqctl add_user running-log-consumer iconsume\n\n# Add a new RabbitMQ user to produce messages\nrabbitmqctl add_user running-log-producer iproduce\n\nrabbitmqctl set_permissions running-log-consumer \".*\" \".*\" \".*\"\nrabbitmqctl set_permissions running-log-producer \".*\" \".*\" \".*\"\n",
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
                "value":" Since we are building the producer now, I will use the ",
                "children":null
            },
            {
                "el":"code",
                "attributes":{
                    "class":"jarombek-inline-code"
                },
                "value":"running-log-producer",
                "children":null
            },
            {
                "el":"#text",
                "attributes":null,
                "value":" user.  A connection to RabbitMQ is done over TCP (a reliable data stream protocol between applications).  Inside each RabbitMQ TCP connection is a channel, which is a virtual connection to perform messaging commands",
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
                "value":".  The following code sets up this channel: ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Python"
        },
        "value":"credentials = pika.PlainCredentials(cred.USERNAME, cred.PASSWORD)\nconnection_params = pika.ConnectionParameters(cred.SERVER, virtual_host=cred.VHOST,\n                                                credentials=credentials)\n\nconnection = pika.BlockingConnection(connection_params)\nchannel = connection.channel()\n",
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
                "value":" Now comes the biggest piece of the producer: the exchange.  In fact, all that the producer really does is publish a message on the exchange and RabbitMQ handles the rest.  Eventually, the exchanges will hand off the messages to queues based on the routing keys we specified",
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
        "el":"p",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":" The following code declares the exchange and publishes a message to it with the JSON we created earlier. The full code for the producer is up on ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/\n2018/01-Jan/1-24-RabbitMQ/producer.py"
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
        "el":"codesnippet",
        "attributes":{
            "language":"Python"
        },
        "value":"# Declare the running-log exchange\n# exchange -- the name of the exchange\n# exchange_type -- the type of exchange, in this case a topic exchange\n# passive -- perform an exchange declare (False) or just check if it exists (True)\n# durable -- If the exchange will survive a RabbitMQ reboot\n# auto_delete -- If the exchange will be removed when no queues are bound to it\nchannel.exchange_declare(exchange=cred.EXCHANGE, exchange_type='topic', passive=False,\n                            durable=True, auto_delete=False)\n\n# Publish to the channel and check for publisher confirms\n# exchange -- the exchange to publish to\n# routing_key -- the routing key to bind to\n# properties -- basic properties of the message\n# body -- the message contents\nif channel.basic_publish(exchange=cred.EXCHANGE, routing_key=routing_key, properties=msg_props,\n                            body=json_data):\n    print(\"Confirm Received!\")\nelse:\n    print(\"Message Lost!\")\n",
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
                "value":"Consumer",
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
                "value":" Now it is time to build the RabbitMQ consumer.  Similarly to the producer, we start by creating a connection and defining an exchange.  We then create a queue on the exchange and determine all the routing keys that the queue accepts.  The python code will take routing keys as command line arguments.  If there is no command line argument, the queue will match all routing keys. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Python"
        },
        "value":"# Declare a queue and create it if needed\n# queue --  the name of the queue - if you dont specify a name RabbitMQ will auto create one\n# exclusive -- only allow access by the current connection\n# durable -- Survives a reboot\nresult = channel.queue_declare(queue='log', exclusive=True, durable=True)\nqueue_name = result.method.queue\n\n# Use command line arguments to get the binding keys\nbinding_keys = sys.argv[1:]\nif not binding_keys:\n    # Bind a queue to an exchange and match to all routing keys ('#' operator)\n    channel.queue_bind(exchange=cred.EXCHANGE, queue=queue_name, routing_key='#')\n",
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
                "value":" Now that the queues are set up, we can create a consumer to read the RabbitMQ messages.  The following Python code sets up a consumer and tells it to listen to the queue we created.  It also defines a callback function which will be called when messages are received. ",
                "children":null
            }
        ]
    },
    {
        "el":"codesnippet",
        "attributes":{
            "language":"Python"
        },
        "value":"def consumer(ch, method, properties, body):\n    \"\"\" Consumer callback is called when a message is consumed \"\"\"\n    if body == 'quit':\n\n        # Cancel a consumer\n        # consumer_tag -- id for the consumer\n        ch.basic_cancel(consumer_tag='consumer')\n        ch.stop_consuming()\n    else:\n        print(\" [x] %r:%r\" % (method.routing_key, body))\n    return\n\n# Start a queue consumer and bind messages to the callback function\n# queue -- the name of the queue to consume from\n# consumer_tag -- the id of the consumer\n# no_ack -- tell the broker not to expect a response back\nchannel.basic_consume(consumer, queue=queue_name, consumer_tag='consumer', no_ack=True)\nchannel.start_consuming()\n",
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
                "value":" If you would like to run both the producers and consumers to see messages in action, you can find the commands in ",
                "children":null
            },
            {
                "el":"a",
                "attributes":{
                    "href":"https://github.com/AJarombek/jarombek-com-sources/blob/master/2018/01-Jan/\n1-24-RabbitMQ/setup.sh"
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
        "el":"h5",
        "attributes":null,
        "value":null,
        "children":[
            {
                "el":"#text",
                "attributes":null,
                "value":"Moving Forward",
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
                "value":" The code to set up RabbitMQ here is all in Python, but as previously mentioned this can be done in many different languages! I hope this code shows how little configuration you can have to set up messaging. RabbitMQ is simply a data transfer mechanism, the real power of your applications will be the code that works around it. ",
                "children":null
            }
        ]
    }
];

postViews = db.posts.findOne({name: "jan-24-2018-rabbitmq"}).views;

db.posts.remove({name: "jan-24-2018-rabbitmq"});

db.posts.insertOne({
    name: "jan-24-2018-rabbitmq",
    title: "First Look at RabbitMQ",
    date: new Date('2018-01-24T12:00:00'),
    type: "Discovery",
    views: postViews,
    tags: [
        {
            name: "RabbitMQ",
            picture: "./assets/rabbitmq.png",
            color: "rabbitmq"
        },
        {
            name: "Message Broker"
        },
        {
            name: "Python",
            picture: "./assets/python.png",
            color: "python"
        }
    ],
    content,
    sources: [
        {
            startName: "Alvaro Videla &amp; Jason J.W. Williams, ",
            endName: " (Shelter Island, NY: Manning, 2012), 2",
            linkName: "RabbitMQ In Action",
            link: "https://www.manning.com/books/rabbitmq-in-action"
        },
        {
            startName: "",
            endName: ", 22",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/rabbitmq-in-action"
        },
        {
            startName: "",
            endName: ", 45",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/rabbitmq-in-action"
        },
        {
            startName: "",
            endName: ", 14",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/rabbitmq-in-action"
        },
        {
            startName: "",
            endName: ", 20",
            linkName: "Ibid.",
            link: "https://www.manning.com/books/rabbitmq-in-action"
        },
    ]
});