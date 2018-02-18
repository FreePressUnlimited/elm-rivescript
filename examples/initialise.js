var bot = new RiveScript()
  , mnt = document.getElementById('my-app');

var app;

// Load the bot brain
bot.loadFile([
  // Insert relative .rive file paths here
  "example.rive"

], function (batch_num) {

  console.log("Bot brain #" + batch_num + " has finished loading");
  // Now replies must be sorted; this is a rivescript-js requirement
  bot.sortReplies();

  // Wire up the Elm app
  app = Elm.Example.embed(mnt)
  app.ports.request.subscribe(function(data) {
    console.log("We're asking the bot brain: (" + data[0] + ") " + data[1]);
    var reply = bot.reply(data[0], data[1]);
    console.log("  and the bot brain replied (" + data[0] + ") " + reply);
    app.ports.respond.send([data[0], reply]);
  });

}, function (error) {

  console.log("Error when loading bot brain: " + error);
});
