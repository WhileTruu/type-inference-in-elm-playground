module Examples exposing (..)


type alias Example =
    { name : String
    , code : String
    , annotation : String
    }


examples : List Example
examples =
    [ { name = "deeply nested lambda"
      , code =
            String.join "\n"
                [ "\\a1 ->"
                , "  \\a2 ->"
                , "    \\a3 ->"
                , "      \\a3 ->"
                , "        \\a4 ->"
                , "          \\a5 ->"
                , "            \\a6 ->"
                , "              \\a7 ->"
                , "                \\a8 ->"
                , "                  \\a9 ->"
                , "                    \\a10 ->"
                , "                      \\a11 ->"
                , "                        \\a12 ->"
                , "                          \\a13 ->"
                , "                            \\a14 ->"
                , "                              \\a15 ->"
                , "                                \\a16 ->"
                , "                                  \\a17 ->"
                , "                                    \\a18 ->"
                , "                                      \\a19 ->"
                , "                                        \\a20 ->"
                , "                                          \\a21 ->"
                , "                                            \\a22 ->"
                , "                                              \\a23 ->"
                , "                                                \\a24 ->"
                , "                                                  \\a25 ->"
                , "                                                    \\a26 ->"
                , "                                                      1"
                ]
      , annotation =
            "∀ a b c d e f g h i j k l m n o p q r s t u v w x y z a1."
                ++ " a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o"
                ++ " -> p -> q -> r -> s -> t -> u -> v -> w -> x -> y -> z -> a1 -> Int"
      }
    , { name = "lambda argument is inferred"
      , code =
            String.join "\n"
                [ "\\a ->"
                , "  (add a) 1"
                ]
      , annotation = "Int -> Int"
      }
    , { name = "nested lambda arguments are inferred"
      , code =
            String.join "\n"
                [ "\\a ->"
                , "  \\b ->"
                , "    (add a) b"
                ]
      , annotation = "Int -> Int -> Int"
      }
    , { name = "variable shadowing"
      , code =
            String.join "\n"
                [ "\\a ->"
                , "  \\b ->"
                , "    (add ((\\a -> a) 10)) b"
                ]
      , annotation = "∀ a. a -> Int -> Int"
      }
    , { name = "context"
      , code =
            String.join "\n"
                [ "\\a ->"
                , "  ((\\potato ->"
                , "     \\b ->"
                , "         potato"
                , "  ) 10)"
                ]
      , annotation = "∀ a b. a -> b -> Int"
      }
    , { name = "complex weird stuff"
      , code =
            String.join "\n"
                [ "\\f1 ->"
                , "  (\\f2 ->"
                , "    \\n ->"
                , "      \\a ->"
                , "        \\b ->"
                , "           (((if ((gte 0) a)) (a)) ((((f2 ((add n) 1)) b) ((add a) b))))"
                , "  )"
                , "    (\\a -> f1 a)"
                ]
      , annotation = "(Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int"
      }
    , { name = "function with `gte` and `if`"
      , code = "\\a -> \\b -> ((if ((gte a) b)) ((add a) b)) b"
      , annotation = "Int -> Int -> Int"
      }
    ]
