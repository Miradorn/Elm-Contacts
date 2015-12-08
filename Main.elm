import ContactBook exposing (update, view, init)
import Effects exposing (Never)
import StartApp
import Task

app = StartApp.start
  { init = init
  , update = update
  , view = view
  , inputs = []
  }


main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
