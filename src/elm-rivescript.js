import RiveScript from 'rivescript';

var bot = new RiveScript()
  , node = document.getElementById('my-app')
  , app = Elm.MyApp.embed(node);

// All file loading operations are asynchronous, so you need handlers
// to catch when they've finished. If you use loadDirectory (or loadFile
// with multiple file names), the success function is called only when ALL
// the files have finished loading.
function loading_done (batch_num) {
  console.log("Batch #" + batch_num + " has finished loading!");

  // Now the replies must be sorted!
  bot.sortReplies();
}

// It's good to catch errors too!
function loading_error (error) {
  console.log("Error when loading files: " + error);
}

// Load a list of files all at once (the best alternative to loadDirectory
// for the web!)
bot.loadFile([
  // Insert relative .rive file paths here
], loading_done, loading_error);


app.ports.reply.subscribe(function(data) {
  var reply = bot.request("local-user", data);
  app.ports.respond.send(reply);
});
