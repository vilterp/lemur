// import needed libraries (installed with npm)
var express = require('express');
var bodyParser = require('body-parser');
var events = require('events');
var child_process = require('child_process');
var tmp = require('tmp');
var fs = require('fs');
var fs = require('fs-extra');
// var multer = require('multer');

var app = express();

// really annoyed that express doesn't do this by default
app.use(bodyParser.json()); // to support JSON-encoded bodies
app.use(bodyParser.urlencoded({ // to support URL-encoded bodies
  extended: true
})); 
// tell express to print requests in the console as they come in
app.use(require('morgan')('tiny'));
// for uploading files
// app.use(multer({ dest: './saved_data/uploads/'}));

// serve static files
app.use(express.static(__dirname + '/public'));

// tell express how to handle requests
app.get('/', function(req, res) {
	res.sendFile('public/index.html');
});

app.post('/run_python', function(req, res) {
  // TODO: get code field
  console.log(req.body);

  tmp.dir(function(err, path, cleanupCallback) {
    if (err) throw err;

    console.log("Dir: ", path);
    // ugh, that callback christmas tree... should have written in Go
    var code_path = path + '/code.py'
    var log_call_path = path + '/log_call.py'
    fs.copy('src/Runtime/log_call.py', log_call_path, function (err) {
      if (err) return console.error(err)
      
      fs.writeFile(code_path, req.body.code, function(err) {
        if(err) throw err;
        
        var stdout_messages = [];
        var stderr_messages = []

        var python = child_process.spawn('python', [code_path]);
        python.stdout.setEncoding('utf8');
        python.stdout.on('data', function(data) {
          var line = data.slice(0, -1);
          console.log('python out:', line);
          stdout_messages.push(line);
        });
        python.stderr.setEncoding('utf8');
        python.stderr.on('data', function(data) {
          var line = data.slice(0, -1);
          console.log('python err:', line);
          stderr_messages.push(line);
        });

        python.on('close', function(code, signal) {
          console.log('exit code: ', code);

          res.send({
            stdout_messages: stdout_messages,
            stderr_messages: stderr_messages
          });

          fs.unlinkSync(log_call_path);
          fs.unlinkSync(code_path);
          fs.unlinkSync(path + '/log_call.pyc'); // TODO: can I just glob somehow? or rm -rf?

          console.log('contents:', fs.readdirSync(path));

          cleanupCallback();

        });

      });

    });

  });

});

// start it up
app.listen(3000, function() {
	console.log('listening on http://localhost:3000/');
});
