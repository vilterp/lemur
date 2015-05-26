// import needed libraries (installed with npm)
var express = require('express');
var bodyParser = require('body-parser');
var events = require('events');
var child_process = require('child_process');
var tmp = require('tmp');
var fs = require('fs-extra');
var EventEmitter = require('events').EventEmitter;
var util = require('util');
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

app.get('/run_python', function(req, res) {

  tmp.dir(function(err, path, cleanupCallback) {
    if (err) throw err;

    console.log("Dir: ", path);
    // ugh, that callback christmas tree... should have written in Go
    var code_path = path + '/code.py'
    var log_call_path = path + '/log_call.py'
    fs.copy('src/Runtime/log_call.py', log_call_path, function (err) {
      if (err) return console.error(err)
      
      var code = req.param('code');

      console.log('CODE:', code);

      fs.writeFile(code_path, code, function(err) {
        if(err) throw err;
        
        var stdout_messages = [];
        var stderr_messages = []

        var python = child_process.spawn('python', [code_path]);
        // TODO: implement f*#@$ing framed protocol
        var f_buf = new FramedBuffer();
        python.stdout.on('data', function(line) {
          console.log('python out:', line);
          f_buf.push(line);
        });
        python.stderr.setEncoding('utf8');
        python.stderr.on('data', function(data) {
          var line = data.trim();
          console.log('python err:', line);
          stderr_messages.push(line);
        });
        f_buf.on('message', function(msg) {
          stdout_messages.push(JSON.parse(msg));
        });

        python.on('close', function(code, signal) {
          console.log('exit code: ', code);

          // res.end();

          res.send(stdout_messages);

          console.log('contents:', fs.readdirSync(path));

          fs.removeSync(path)

        });

      });

    });

  });

});

// TODO: this must be in a package somewhere...
function FramedBuffer() {
  this.buffer = '';
  this.state = 'beginning';
  this.length_left = 0;
  return this;
}
util.inherits(FramedBuffer, EventEmitter);

FramedBuffer.prototype.push = function(buf) {
  if(buf.length == 0) {
    return;
  }
  if(this.state === 'beginning') {
    var length = buf.readInt32LE();
    this.length_left = length;
    buf = buf.slice(4);
    this.state = 'reading';
  }
  var chunk_length = Math.min(this.length_left, buf.length);
  var this_chunk = buf.toString('utf8', 0, chunk_length);
  this.buffer += this_chunk;
  this.length_left -= chunk_length;
  if(this.length_left === 0) {
    this.state = 'beginning';
    this.emit('message', this.buffer);
    this.buffer = '';
    this.push(buf.slice(chunk_length));
  }
}

// start it up
app.listen(3000, function() {
	console.log('listening on http://localhost:3000/');
});
