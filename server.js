// import needed libraries (installed with npm)
var express = require('express');
var bodyParser = require('body-parser');
var events = require('events');
var child_process = require('child_process');
var tmp = require('tmp');
var fs = require('fs');
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
  tmp.file(function(err, path, fd, cleanupCallback) {
    if(err) throw err;

    console.log("File: ", path);
    console.log("Filedescriptor: ", fd);

    fs.writeFile(path, req.body.code, function(err) {
      if(err) throw err;
      
      var stdout_messages = [];
      var stderr_messages = []

      var python = child_process.spawn('python', [path]);
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

        cleanupCallback();

      });

    });
  });
});

// start it up
app.listen(3000, function() {
	console.log('listening on http://localhost:3000/');
});
