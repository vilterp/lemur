import types

class EventEmitter:
  def __init__(self):
    self.handlers = []
  
  def add_handler(self, handler):
    self.handlers.append(handler)
  
  def fire(self, evt):
    for handler in self.handlers:
      handler(evt)

# the dreaded global variable
events = EventEmitter()
evt_list = []
def add_to_list(evt):
  evt_list.append(evt)
events.add_handler(add_to_list)

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
  events.fire({
    'msg': 'start_call',
    'ap_id': ap_id,
    'args': tag_record(args)
  })
  result = function(**args)
  events.fire({
    'msg': 'end_call',
    'results': tag_record(result)
  })
  return result

# for testing

def log_fib(n):
  if n == 0 or n == 1:
    return {'result': 1}
  else:
    return {'result': log_call(log_fib, 1, {'n': n-1})['result'] +\
                        log_call(log_fib, 2, {'n': n-2})['result']}
    
# log_call(log_fib, 0, {'n': 2})
