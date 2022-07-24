module Data.Name exposing (Name, fromString, toString)


type Name
    = Name String


toString : Name -> String
toString (Name name) =
    name


fromString : String -> Name
fromString name =
    Name name
