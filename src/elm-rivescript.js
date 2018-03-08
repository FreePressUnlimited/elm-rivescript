import RiveScript from 'rivescript';

export default function (mnt, brains) {

  // Instantiate RiveScript interpreter
  var bot = new RiveScript();

  // Load the bot brain
  bot.loadFile(brains, function (batch_num) {

    console.log("Bot brain #" + batch_num + " has finished loading");

    // Now replies must be sorted; this is a rivescript-js requirement
    bot.sortReplies();

    // Wire up the Elm app
    var app = Elm.Example.embed(mnt);
    app.ports.to.subscribe(function(data) {
      console.log("We're asking the bot brain: (" + data[0] + ") " + data[1]);
      var reply = bot.reply(data[0], data[1]);
      console.log("  and the bot brain replied (" + data[0] + ") " + reply);
      app.ports.with.send([data[0], reply]);
    });

  }, function (error) {
    console.log("Error when loading bot brain: " + error);
  });

}
