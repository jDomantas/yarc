module ListHelp exposing (..)


removeAt : Int -> List a -> List a
removeAt index list =
  case list of
    [] ->
      []

    x :: xs ->
      if index == 0 then
        xs
      else
        x :: (removeAt (index - 1) xs)


mapAt : Int -> (a -> a) -> List a -> List a
mapAt index f = List.indexedMap <| \i x ->
  if i == index then
    f x
  else
    x


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
  case list of
    [] ->
      Nothing

    x :: xs ->
      if predicate x then
        Just x
      else
        find predicate xs


last : List a -> Maybe a
last list =
  case list of
    [] ->
      Nothing

    x :: [] ->
      Just x

    x :: xs ->
      last xs


setLength : Int -> List (Maybe a) -> List (Maybe a)
setLength length list =
  case list of
    [] ->
      List.repeat length Nothing

    x :: xs ->
      if length == 0 then
        []
      else
        x :: setLength (length - 1) xs


at : Int -> List a -> Maybe a
at index list =
  case list of
    [] ->
      Nothing

    x :: xs ->
      if index < 0 then
        Nothing
      else if index == 0 then
        Just x
      else
        at (index - 1) xs


flatten : List (Maybe a) -> List a
flatten = List.filterMap identity


collectResults : List (Result x a) -> Result (List x) (List a)
collectResults list =
  case list of
    [] ->
      Ok []

    Err e :: xs ->
      case collectResults xs of
        Ok _ ->
          Err [ e ]

        Err errors ->
          Err (e :: errors)

    Ok x :: xs ->
      case collectResults xs of
        Ok xs ->
          Ok (x :: xs)
        
        Err errors ->
          Err errors


join : appendable -> List appendable -> Maybe appendable
join sepparator list =
  case list of
    [] ->
      Nothing

    x :: xs ->
      case join sepparator xs of
        Just value ->
          Just (x ++ sepparator ++ value)
        
        Nothing ->
          Just x


extend : Int -> a -> List a -> List a
extend length value list =
  case list of
    [] ->
      if length <= 0 then
        []
      else
        value :: extend (length - 1) value []
  
    x :: xs ->
      x :: extend (length - 1) value xs
