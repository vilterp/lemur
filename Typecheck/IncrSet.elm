module TypeCheck.IncrSet where

import Dict as D

type alias IncrSet comparable =
  { nextId : Int
  , ids : D.Dict comparable Int
  }

empty = { nextId = 0, ids = D.empty }

getId : comparable -> IncrSet comparable -> (Int, IncrSet comparable) -- state monad...
getId x is =
    case D.get x is.ids of
      Just id -> (id, is)
      Nothing -> (is.nextId, { is | nextId <- is.nextId + 1
                                  , ids <- D.update x (always <| Just is.nextId) is.ids })
