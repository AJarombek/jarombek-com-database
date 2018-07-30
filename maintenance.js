/**
 * Commonly used queries in MongoDB to perform maintenance on the database
 * @author Andrew Jarombek
 * @since 5/23/2018
 */

// Find all the posts views
db.viewed.find();

// Remove a certain viewed document
db.viewed.remove({"_id": ObjectId("5b01c0d469d1d157f1c2c276")});

// Remove all viewed documents
db.viewed.remove({});

// Sort all viewed documents by the post name
db.viewed.find().sort({"name": -1});

// Remove a property from all the documents in the viewed collection
db.viewed.updateMany(
    {},
    { $unset: {"item_id": ""}}
);

// Observe a single post
db.posts.findOne();