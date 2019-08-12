connection = new Mongo();
db = connection.getDB("andy");

const sorry = `I'm sorry, I hope I didn't ruin your night.  I was really scared and didn't know what 
                to do.  I didn't want to risk talking to you in case you didn't want me to.  I also 
                didn't want you to think I don't care for you.  So I ended up just kinda freezing.  
                If your hello was in response to my last message that is very sweet of you, 
                thank you.  If not that's okay too.  I hope you know that I didn't leave because I 
                didn't want to see you.  I did and do.  I was terrified that I was hurting you.  You 
                deserve all the happiness in the world.  I'm sorry again.  Love, Andy`;

const prayer = `My love seems to be moving away from me today.  I just want to pray that it's beause she 
                doesn't care for me and doesn't love me and not because of something else.  I don't want her to 
                leave because she feels like a burden.  I don't want her to leave because she thinks she 
                may never be ready for a relationship or commitment.  Please give her the strength to 
                reach out if she cares for me but is afraid.  I've screwed up so many times in the past.  
                Telling her I didn't want to hear from her unless she was ready for me.  I wish those words 
                never came out of my mouth.  Being emotional when I should have been supportive.  It breaks 
                my heart that my emotions scared her.  If I could pray for just one thing it would be that 
                she is moving away from me because she wants to.  Nothing else.  Otherwise I would love to 
                hear from her, even if its just a simple hello.`;

db.heart.insertOne({
    date: new Date('2019-07-09T12:00:00'),
    content: [
        ["I wish I could give her a big hug so that she can feel how deep my love is.  " +
        "Maybe I'm wrong, but I think we would both love that.  Whatever the appropriate means, " +
        "I wish we could both know the depth of our love for each other.  To be free of fearing " +
        "our loves end.  Again maybe I'm wrong, but that sounds beautiful to me."],
        ["If you just want to talk or reach out for help, I will love you.  If you decide you " +
        "never want to speak to me again, I will love you.  If you are too afraid of causing me " +
        "pain, I will love you.  If you changed your mind about your feelings for me, I will love " +
        "you.  If you reach out to give me a hug, I will love you.  If you decide its time for you " +
        "to move on and see someone else, I will love you.  I will love you."]
        ["Love Always, Andy"],
        ["... and it's okay that I'm not the one you address your love to.  One day you will find " +
        "the loving man that you seek.  I love you so much and dream of spending time with you, " +
        "but that doesn't make much difference if you don't love me too.  It's okay, I won't " +
        "bother your life.  I'll keep my feelings of love within me.  It will be hard, but I'll try " +
        "to stop writing these secret letters.  Maybe you never saw them anyway.  Maybe I need to " +
        "hide them away."],
        ["I told you that no matter what I will love you.  And I will love you always."]
    ]
});

db.heart.insertOne({
    date: new Date('2019-07-01T12:00:00'),
    content: [
        ["Today was one of those days controlled by my fears.  I'm afraid because I don't know " +
        "where I need to go from here.  I don't really know where my home is.  I know my present " +
        "one won't last.  The place where I'm with my love only exists inside my head."],
        ["The difficulty of today will pass and I'll be able to go back to being productive soon.  " +
        "However even on good days these questions will linger.  And I don't have the answers."],
        ["Hopefully she is doing better and has a clearer picture of things.  If she reads this, I " +
        "hope she knows that I love her so much.  That her most beautiful qualities and greatest " +
        "weaknesses all mold together into someone so deserving of love and respect.  Into the " +
        "person I love and hold close to my heart, always."],
        ["Towards the beginning of last month I questioned whether writing these are the good " +
        "thing I wish them to be.  I still don't know.  Sometimes she feels close to me, sending " +
        "her love my direction.  Other times she feels 2 1/2 years away.  There has been no " +
        "way for me to know.  I hope she knows I'm just trying my best.  Maybe that isn't any good."],
        ["Andy"]
    ]
});

db.heart.insertOne({
    date: new Date('2019-06-27T12:00:00'),
    content: [
        ["I wish you the best in your continued quest to open your heart to yourself and to someone " +
        "else.  You are a good person.  You might not know it yet, but your ability to love " +
        "is amazing and beautiful.  I've seen it myself."],
        ["Love, Andy"]
    ]
});

db.heart.insertOne({
    date: new Date('2019-06-16T12:00:00'),
    content: [
        ["I understand that freedom now doesn't mean the impact of everything that happened goes " +
        "away.  Just because I heard she's free doesn't mean she's without internal battles.  I'm " +
        "always here for her and praying for her."],
        ["I know that I failed her the last time she looked up to me for support.  Even though I " +
        "wanted to help, I ended up just breaking down and being extremely emotional.  So I completely " +
        "understand why she hasn't reached out.  Maybe speaking to me would interrupt her quest for " +
        "inner stability as well.  I don't want to hold her back."],
        ["All I can say is I'll always love you.  You make me smile like no other.  I know you " +
        "do the same for so many others, it really is such a beautiful gift you have."],
        ["Hopefully I haven't assumed too much in this letter.  I just want to help my love."],
        ["Love Always, Andy"],
        ["P.S. I keep seeing deer when I'm really in need of emotional support.  Like last weekend " +
        "when I saw a doe and she followed me through the woods.  Or Tuesday morning when I was sad " +
        "and two bucks crossed the parking lot and came up to my window at work.  Not sure if there " +
        "is a reason for that or not, she would know better than me.  Either way they always make " +
        "me happy."]
    ]
});

db.heart.insertOne({
    date: new Date('2019-06-10T12:00:00'),
    content: [
        ["Today was a challenging day.  I have them quite often, but I learn a lot each time which " +
        "is good.  I had time to reflect upon my love.  On my good days my love is very selfless.  I " +
        "love without asking for anything.  It's unconditional.  On my bad days the selfish thoughts " +
        "enter into my mind and I break down a bit from the struggles of missing her.  Like today " +
        "when I curled up with Dotty and listened to sad Taylor Swift songs."],
        ["I know its natural to miss someone you love.  And to be selfish by missing the way they " +
        "made you feel.  I try not to be too hard on myself about the selfish piece, instead " +
        "striving to be less selfish as time goes on."],
        ["My love will be with me for all my life.  Above all I want my love to be happy.  I try not " +
        "to assume what she is thinking or going through, but I know she would be here now if she " +
        "wanted to be.  She would be here if her selfish thoughts that pop up now and again matched " +
        "my own."],
        ["I have a lot still to learn.  I know that I want the energy of my love spent more on her " +
        "happiness than my selfish yearning.  Because of that I'm removing Still Beautiful from my " +
        "page, since a big piece of the song is hoping for the return of love.  As I said, if she " +
        "wanted to be here she would."],
        ["I'm not sure if I should keep writing these and leaving them open to read.  I have no idea " +
        "if she wants me writing.  I know my intentions are to help her towards happiness, but maybe my good " +
        "intentions aren't enough.  Or maybe my real intentions are to assist her so that she returns " +
        "to me.  Probably both.  I pray that I can get some guidance on that."],
        ["I don't expect to stop missing my love.  I just want my feelings of love to have as positive an impact " +
        "as possible.  So with that in mind, I hope she is having a wonderful day.  I know she will " +
        "bring sunshine to so many peoples lives.  I will always love her and always be on her side."],
        ["Love, Andy"],
        ["https://bit.ly/2I5MFU2"]
    ]
});

db.heart.insertOne({
    date: new Date('2019-06-01T12:00:00'),
    content: [
        ["To my love -"],
        ["Just want to let you know that you are a very special person and that I love you.  I hope " +
        "you are doing well. I hope you know that there are so many people rooting for you."],
        ["Dotty also wants to say hello and that she cares for you very much.  Don't worry, she is " +
        "all tucked in and snug for the day."]
        ["-Andy"]
    ]
});

db.heart.insertOne({
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

db.heart.insertOne({
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

db.heart.insertOne({
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

db.heart.insertOne({
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
