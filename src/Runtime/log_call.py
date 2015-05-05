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

def log_call(function, ap_id, args):
  events.fire({
    'msg': 'start_call',
    'ap_id': ap_id,
    'args': args
  })
  result = function(**args)
  events.fire({
    'msg': 'end_call',
    'results': result
  })
  return result

def log_fib(n):
  if n == 0 or n == 1:
    return {'result': 1}
  else:
    return {'result': log_call(log_fib, 1, {'n': n-1})['result'] +\
                        log_call(log_fib, 2, {'n': n-2})['result']}
    
log_call(log_fib, 0, {'n': 2})
