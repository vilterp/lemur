import types
import json
import sys

class EventLogger:
  def __init__(self):
    self.handlers = []
    self.log = []
    self.stack = []
  
  def clear(self):
    self.stack = []
    self.log = []

  def add_event(self, evt):
    self.log.append(evt)
    sys.stdout.write(json.dumps(evt) + ';\n')
    sys.stdout.flush()

  def start_call(self, ap_id, args):
    event = {
      'msg': 'start_call',
      'ap_id': ap_id,
      'path': [s for s in self.stack],
      'args': tag_record(args)
    }
    self.stack.append(ap_id)
    self.add_event(event)

  def end_call(self, result):
    event = {
      'msg': 'end_call',
      'results': tag_record(result),
      'path': [s for s in self.stack]
    }
    self.stack.pop()
    self.add_event(event)

# the dreaded global variable
events = EventLogger()

def tag_record(val):
  return {k: tag_value(v) for k, v in val.iteritems()}

def tag_value(val):
  if type(val) == int:
    return {'tag': 'int', 'value': val}
  elif type(val) == str or type(val) == unicode:
    return {'tag': 'string', 'value': val }
  elif type(val) == list:
    return {'tag': 'list', 'value': map(tag_value, val)}
  elif type(val) == dict:
    return {'tag': 'record', 'value': tag_record(val)}
  elif type(val) == file:
    return {'tag': 'file', 'value': val.name} # absolute? relative to what?
  elif type(val) == types.FunctionType:
    return {'tag': 'function'}

def log_call(function, ap_id, args):
  global events
  events.start_call(ap_id, args)
  result = function(**args)
  events.end_call(result)
  return result

def run_main(fun, args):
  global events
  events.clear()
  result = fun(**args)

# for testing

def log_fib(n):
  if n == 0 or n == 1:
    return {'result': 1}
  else:
    return {'result': log_call(log_fib, 'ap1', {'n': n-1})['result'] +\
                        log_call(log_fib, 'ap2', {'n': n-2})['result']}
    
# log_call(log_fib, 0, {'n': 2})
