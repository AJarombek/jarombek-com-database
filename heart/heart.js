connection = new Mongo();
db = connection.getDB("andy");

db.posts.insertOne({
    date: new Date('2019-05-21T12:00:00'),
    content: [
        ["I don't know why but I felt like I had to write today.  I'm writing because I care.  And " +
        "because I truly love.  I keep doing it in case my love is hurting or struggling.  Or if she " +
        "needs a reminder that I'm here."],
        ["Everything will be okay.  Hold on to your little stuffed friend like I do to Dotty.  Love " +
        "is all around you if you look.  I love you."],
        ["-Andy"]
    ]
});

db.posts.insertOne({
    date: new Date('2019-05-05T12:00:00'),
    content: [
        ["I spent the last hour writing a letter about why it would help me to hear from the woman " +
        "I love.  However I quickly realized I'd written the same things so many times and for the " +
        "past 29 months it never did any good.  No matter how much love and kindness I " +
        "put in, the situation never changed.  The silence remained."],
        ["I know its selfish of me to pray and ask to hear her voice again.  If I look at it " +
        "positively its because I care and love.  Maybe she doesn't look at it that way."],
        ["I spoke with Garvey about things for only the second time since graduating.  I realize I " +
        "deserve someone who speaks to me just because they want to.  No love letters required."],
        ["I never wanted her to feel like she missed out on someone in life because of the " +
        "difficult situation she was placed in.  I still don't.  Nor do I want her to lose someone " +
        "who cares for her because of her struggles."],
        ["I'm just tired.  My heart deserves a vacation.  I pray someday soon it will get it."],
        ["Love, Andy"]
    ]
});

db.posts.insertOne({
    date: new Date('2019-04-25T12:00:00'),
    content: [
        ["The other day I tried calling and leaving a message, but got no response.  I'm not sure if " +
        "my number is blocked or if she just doesn't want to speak to me.  I know my love is good " +
        "and makes most people happy and smile.  I'm so sorry if it isn't good for her."],
        ["I heard a rumor that she is free.  If true that is an incredible thing that she " +
        "should be so proud of.  I can't even imagine the amount of strength she must have to " +
        "achieve that."],
        ["Besides for my love, I know another strength of mine is my work.  My code creates my " +
        "own little world where I can create anything I wish.  I can build websites as a gift to " +
        "our college team or as a way to help people learn programming.  I can also write messages " +
        "from my heart for someone who is no longer in my life."],
        ["I know she will likely never see these hidden letters.  This file will contain feelings " +
        "close to my heart.  I'll write them when I can find the right words."],
        ["I included a note below that I wrote for her special day this past winter.  I also created " +
        "a database for her 23rd which she probably never saw because I gave it to Garv.  I'll add " +
        "it to the nodejs-mongodb-api-prototype repository soon."],
        ["In my profile is a link to 'Stay Beautiful' by Tay.  It really is a reflection to how " +
        "I've felt for a long time.  While Taylor wrote it for a young crush, for me its for someone " +
        "who will always have a piece of my heart all to themselves.  As the song goes, that piece " +
        "of my heart will always be there for her at my front door.  I think I will leave the link " +
        "there as a reminder for her, if she ever decides to look at it."],
        ["Im proud of you, happy for your growth as a person, and wish you all the best."],
        ["With love, Andy"]
    ]
});

db.posts.insertOne({
    date: new Date('2018-12-19T12:00:00'),
    content: [
        ["This time of year has me thinking of someone I deeply care for and miss.  She is a " +
        "wonderful person who brightens everyone's day around her."],
        ["I haven’t spoken to her in a very long time.  I’m really scared writing this because I " +
        "know she may never want to hear from me again.  I guess she won’t see this unless she still " +
        "cares for me and misses me too."],
        ["So to her, a woman I love:  I hope life is treating you well.  You have overcome so much " +
        "and will overcome so much more.  I don’t know what your life is like anymore but I hope it’s " +
        "going well.  You deserve nothing but the best because you always have been and are an " +
        "amazing person.  I hope you’ve reached a point in life where you can make choices based on " +
        "how you feel and not based on what someone else wants.  If not I know you will get there soon."],
        ["I’m sorry for all the mistakes I’ve made and every time I hurt you.  You are always " +
        "welcome back into my life if that is where your emotions align.  I know there was a time " +
        "we both wanted a fair chance together, but I understand that time has likely passed."],
        ["I figured I’d write this message now before the holidays and your special day.  I really " +
        "don’t want to intrude on your life."],
        ["I hope you have a wonderful time on your special day surrounded by loved ones and family.  " +
        "Thank you for making me a better person."],
        ["With love, Andy"],
        ["https://www.youtube.com/watch?v=BBAtAM7vtgc"]
    ]
});