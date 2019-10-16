import socket

try:
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.connect(('127.0.0.1', 4938))
  def report():
    alive = True
    try:
      if RPR_GetPlayState() & 1:
        s.send('%f|' % RPR_GetPlayPosition())
      else:
        s.send('%f|' % RPR_GetCursorPosition())
    except Exception as e:
      alive = False
    if alive:
      RPR_defer("report()")
  RPR_defer("report()")
except Exception as e:
  pass

