module Formatting exposing (..)

import Array
import Board exposing (..)
import Dict
import E18 exposing (..)
import Set


tt =
    [ ( 'R'
      , Set.fromList
            []
      )
    , ( 'l'
      , Set.fromList
            []
      )
    , ( 'r'
      , Set.fromList
            []
      )
    , ( 't'
      , Set.fromList
            []
      )
    , ( 'B'
      , Set.fromList
            [ 'l'
            ]
      )
    , ( 'H'
      , Set.fromList
            [ 't'
            ]
      )
    , ( 'L'
      , Set.fromList
            [ 'R'
            ]
      )
    , ( 'b'
      , Set.fromList
            [ 'l'
            ]
      )
    , ( 'h'
      , Set.fromList
            [ 't'
            ]
      )
    , ( 'Q'
      , Set.fromList
            [ 'B'
            , 'l'
            ]
      )
    , ( 'g'
      , Set.fromList
            [ 'H'
            , 't'
            ]
      )
    , ( 'p'
      , Set.fromList
            [ 'B'
            , 'l'
            ]
      )
    , ( 'q'
      , Set.fromList
            [ 'B'
            , 'l'
            ]
      )
    , ( 'P'
      , Set.fromList
            [ 'B'
            , 'b'
            , 'l'
            ]
      )
    , ( 'f'
      , Set.fromList
            [ 'B'
            , 'l'
            , 'q'
            ]
      )
    , ( 'x'
      , Set.fromList
            [ 'B'
            , 'l'
            , 'q'
            ]
      )
    , ( 'F'
      , Set.fromList
            [ 'B'
            , 'b'
            , 'l'
            , 'q'
            ]
      )
    , ( 'T'
      , Set.fromList
            [ 'B'
            , 'P'
            , 'b'
            , 'l'
            ]
      )
    , ( 'X'
      , Set.fromList
            [ 'B'
            , 'P'
            , 'b'
            , 'l'
            , 'q'
            ]
      )
    , ( 'e'
      , Set.fromList
            [ 'B'
            , 'F'
            , 'b'
            , 'l'
            , 'q'
            ]
      )
    , ( 'm'
      , Set.fromList
            [ 'B'
            , 'F'
            , 'b'
            , 'l'
            , 'q'
            ]
      )
    , ( 'G'
      , Set.fromList
            [ 'B'
            , 'H'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'k'
      , Set.fromList
            [ 'B'
            , 'G'
            , 'H'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'K'
      , Set.fromList
            [ 'B'
            , 'G'
            , 'H'
            , 'P'
            , 'b'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'M'
      , Set.fromList
            [ 'B'
            , 'F'
            , 'G'
            , 'H'
            , 'b'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'j'
      , Set.fromList
            [ 'B'
            , 'G'
            , 'H'
            , 'K'
            , 'P'
            , 'b'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'J'
      , Set.fromList
            [ 'B'
            , 'G'
            , 'H'
            , 'K'
            , 'P'
            , 'b'
            , 'g'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'u'
      , Set.fromList
            [ 'B'
            , 'G'
            , 'H'
            , 'K'
            , 'P'
            , 'b'
            , 'j'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'd'
      , Set.fromList
            [ 'B'
            , 'G'
            , 'H'
            , 'J'
            , 'K'
            , 'P'
            , 'b'
            , 'g'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'D'
      , Set.fromList
            [ 'B'
            , 'F'
            , 'G'
            , 'H'
            , 'J'
            , 'K'
            , 'P'
            , 'b'
            , 'e'
            , 'g'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'U'
      , Set.fromList
            [ 'B'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'b'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'E'
      , Set.fromList
            [ 'B'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'U'
            , 'b'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 's'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'U'
            , 'b'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'S'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'U'
            , 'b'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'C'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'c'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'n'
      , Set.fromList
            [ 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'o'
      , Set.fromList
            [ 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'y'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'a'
      , Set.fromList
            [ 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'n'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'i'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'z'
      , Set.fromList
            [ 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'o'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'Y'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'J'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'g'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            ]
      )
    , ( 'v'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'h'
            , 'i'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'N'
      , Set.fromList
            [ 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'h'
            , 'i'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'I'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'J'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'Y'
            , 'b'
            , 'c'
            , 'g'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'O'
      , Set.fromList
            [ 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'N'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'h'
            , 'i'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'W'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'I'
            , 'J'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'Y'
            , 'b'
            , 'c'
            , 'g'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'w'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'I'
            , 'J'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'Y'
            , 'b'
            , 'c'
            , 'g'
            , 'h'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'A'
      , Set.fromList
            [ 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'N'
            , 'O'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'h'
            , 'i'
            , 'j'
            , 'k'
            , 'l'
            , 'n'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    , ( 'V'
      , Set.fromList
            [ 'B'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'I'
            , 'J'
            , 'K'
            , 'M'
            , 'P'
            , 'S'
            , 'U'
            , 'Y'
            , 'b'
            , 'c'
            , 'g'
            , 'h'
            , 'i'
            , 'j'
            , 'k'
            , 'l'
            , 'q'
            , 't'
            , 'w'
            , 'x'
            , 'y'
            ]
      )
    , ( 'Z'
      , Set.fromList
            [ 'A'
            , 'B'
            , 'C'
            , 'E'
            , 'F'
            , 'G'
            , 'H'
            , 'K'
            , 'M'
            , 'N'
            , 'O'
            , 'P'
            , 'S'
            , 'U'
            , 'b'
            , 'c'
            , 'h'
            , 'i'
            , 'j'
            , 'k'
            , 'l'
            , 'n'
            , 'o'
            , 'q'
            , 't'
            , 'x'
            , 'y'
            ]
      )
    ]
