-- #############################################################################
-- ###### GRADING TESTS                                               ##########
-- #############################################################################

import Test.Hspec

import Board
     (validateFEN,
      buildBoard,
      Player(White, Black),
      Cell(Empty, Flag, Soldier, General),
      Pos(Pos, col, row))

import Catapult (Move(Move, start, target), playerWon, flagMoves, generalMoves, soldierMoves, catapultMoves, listMoves, findCatapults, obereSpitzefinden, untereSpitzefinden)

main :: IO ()
main = hspec $ do
  ---Funktion aus ChatGPT-o1 auf PDF Seite 90-92
  describe "Pos col and row accessors" $ do
    it "returns the correct column and row for a position" $ do
      let position = Pos 'd' 5
      col position `shouldBe` 'd'
      row position `shouldBe` 5

    it "works for another position" $ do
      let position = Pos 'a' 1
      col position `shouldBe` 'a'
      row position `shouldBe` 1
  
  ---Funktion aus ChatGPT-o1 auf PDF Seite 93-94
  describe "Show instance for Move" $ do
    it "formats a move correctly from 'a1' to 'b2'" $ do
      let move = Move (Pos 'a' 1) (Pos 'b' 2)
      show move `shouldBe` "a1-b2"

    it "formats a move correctly from 'd5' to 'e6'" $ do
      let move = Move (Pos 'd' 5) (Pos 'e' 6)
      show move `shouldBe` "d5-e6"

    it "formats a move correctly from 'h9' to 'j7'" $ do
      let move = Move (Pos 'h' 9) (Pos 'j' 7)
      show move `shouldBe` "h9-j7"

    it "formats a move correctly from 'a0' to 'j9'" $ do
      let move = Move (Pos 'a' 0) (Pos 'j' 9)
      show move `shouldBe` "a0-j9"

  ---Funktion aus ChatGPT-o1 auf PDF Seite 94-96
  describe "Move data structure" $ do
    it "returns the correct start and target positions for a move" $ do
      let move = Move (Pos 'a' 1) (Pos 'b' 2)
      start move `shouldBe` Pos 'a' 1
      target move `shouldBe` Pos 'b' 2

    it "handles a move from 'd5' to 'e6'" $ do
      let move = Move (Pos 'd' 5) (Pos 'e' 6)
      start move `shouldBe` Pos 'd' 5
      target move `shouldBe` Pos 'e' 6

    it "handles a move from 'h9' to 'j7'" $ do
      let move = Move (Pos 'h' 9) (Pos 'j' 7)
      start move `shouldBe` Pos 'h' 9
      target move `shouldBe` Pos 'j' 7

    it "handles a move from 'a0' to 'j9'" $ do
      let move = Move (Pos 'a' 0) (Pos 'j' 9)
      start move `shouldBe` Pos 'a' 0
      target move `shouldBe` Pos 'j' 9

  ---Funktion aus ChatGPT-o1 auf PDF Seite 11-13
  -- Tests für validateFEN selber geschrieben
  describe "validateFEN" $ do
    it "gibt True für einen gültigen FEN-String mit gemischten Figuren zurück" $ do
      validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` True

    it "gibt False für einen FEN-String mit einer 0 zurück" $ do
      validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w0w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` False

    it "gibt True für einen gültigen FEN-String mit leeren Reihen zurück" $ do
      validateFEN "/////////" `shouldBe` True

    it "gibt False für einen FEN-String mit mehr als 10 Reihen zurück" $ do
      validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1b/7B2" `shouldBe` False

    it "gibt False für einen FEN-String mit ungültigen Zeichen zurück" $ do
      validateFEN "4W5/1w1w1w1x1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4X5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` False

    it "gibt False für einen FEN-String mit mehr als 10 Spalten in einer Reihe zurück" $ do
      validateFEN "4W6/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2/7b2" `shouldBe` False

    it "gibt False für einen FEN-String mit weniger als 10 Reihen zurück" $ do
      validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1" `shouldBe` False

    


  ---Funktion aus ChatGPT-o1 auf PDF Seite 18-20
  -- Tests für buildBoard selber geschrieben
  describe "buildBoard" $ do
      
    it "erstellt ein leeres Brett" $ do
      buildBoard "//////////" `shouldBe` []

    it "invalider Fen-String" $ do
      buildBoard "/x9/////////" `shouldBe` []

    it "Brett mit gemischten Figuren 1" $ do
      let fen = "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      buildBoard fen `shouldBe`
        [
          [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Empty, Empty, Empty, Empty, General White, Empty, Empty, Empty, Empty],
          [Empty, Empty, Empty, Empty, General Black, Empty, Empty, Empty, Empty, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
        ]    

    it "Brett mit gemischten Figuren 2" $ do
      let fen = "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/23g4/31G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      buildBoard fen `shouldBe`
        [
          [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Empty, Empty, Empty, Empty, General White, Empty, Empty, Empty, Empty],
          [Empty, Empty, Empty, Empty, General Black, Empty, Empty, Empty, Empty, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
        ]

    it "Brett mit gemischten Figuren 3" $ do
      let fen = "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      buildBoard fen `shouldBe`
        [
          [Empty, Empty, Empty, Empty, Flag White, Empty, Empty, Empty, Empty, Empty],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
          [Empty, Empty, Empty, Empty, Empty, General White, Empty, Empty, Empty, Empty],
          [Empty, Empty, Empty, Empty, General Black, Empty, Empty, Empty, Empty, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
          [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Flag Black, Empty, Empty]
        ]



  ---Funktion aus ChatGPT-o1 auf PDF Seite 28-29
  -- Tests für flagMoves selber geschrieben
  describe "flagMoves" $ do

    it "gibt eine leere Liste zurück, wenn das Brett keine gültige Startkonfiguration hat" $ do
      let invalidBoard = buildBoard "//////////"
      flagMoves invalidBoard White `shouldBe` []

    it "gibt alle gültigen Flaggenplatzierungszüge für Weiß zurück, wenn keine Flagge platziert ist und das Brett gültig ist" $ do
      let validBoard = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      flagMoves validBoard White `shouldBe` 
        [ Move (Pos 'b' 9) (Pos 'b' 9)
        , Move (Pos 'c' 9) (Pos 'c' 9)
        , Move (Pos 'd' 9) (Pos 'd' 9)
        , Move (Pos 'e' 9) (Pos 'e' 9)
        , Move (Pos 'f' 9) (Pos 'f' 9)
        , Move (Pos 'g' 9) (Pos 'g' 9)
        , Move (Pos 'h' 9) (Pos 'h' 9)
        , Move (Pos 'i' 9) (Pos 'i' 9)
        ]
      flagMoves validBoard Black `shouldBe` []

    it "wenn weiße Flagge in der Ecke steht ist es ungültig" $ do
      let validBoard = buildBoard "W9/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      flagMoves validBoard Black `shouldBe` []


    it "gibt alle gültigen Flaggenplatzierungszüge für Schwarz zurück, wenn keine Flagge platziert ist und das Brett gültig ist" $ do
      let validBoard = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      flagMoves validBoard Black `shouldBe` 
        [ Move (Pos 'b' 0) (Pos 'b' 0)
        , Move (Pos 'c' 0) (Pos 'c' 0)
        , Move (Pos 'd' 0) (Pos 'd' 0)
        , Move (Pos 'e' 0) (Pos 'e' 0)
        , Move (Pos 'f' 0) (Pos 'f' 0)
        , Move (Pos 'g' 0) (Pos 'g' 0)
        , Move (Pos 'h' 0) (Pos 'h' 0)
        , Move (Pos 'i' 0) (Pos 'i' 0)
        ]
      flagMoves validBoard White `shouldBe` []

    it "wenn keine Flagge steht, darf erst Weiß setzen und nicht Schwarz" $ do
      let validBoard = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      flagMoves validBoard Black `shouldBe` []

    it "wenn die schwarze Flagge steht, dann ist es ungültig, weil die weiße Flagge zuerst gesetzt werden soll" $ do
      let validBoard = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      flagMoves validBoard White `shouldBe` []
      flagMoves validBoard Black `shouldBe` []

    it "behandelt den Randfall, bei dem beide Flaggen platziert sind und gibt eine leere Liste zurück" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      flagMoves board White `shouldBe` []
      flagMoves board Black `shouldBe` []

    it "gibt eine leere Liste zurück für ein Brett mit ungültigen Zeichen im FEN-String" $ do
      let invalidFEN = buildBoard "4X5/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      flagMoves invalidFEN White `shouldBe` []
      flagMoves invalidFEN Black `shouldBe` []

    it "gibt eine leere Liste zurück, wenn der FEN-String unvollständig ist" $ do
      let validBoard = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b"
      flagMoves validBoard White `shouldBe` []
      flagMoves validBoard Black `shouldBe` []

    it "gibt eine leere Liste zurück, wenn der FEN-String ungültige Spalten enthält" $ do
      let validBoard = buildBoard "ww8/1w1w1w1w1w/1w1w1w1w1w/wwwwwwwwww/5g4/4G5/bbbbbbbbbb/b1b1b1b1b1/b1b1b1b1b1/"
      flagMoves validBoard White `shouldBe` []
      flagMoves validBoard Black `shouldBe` []



  ---Funktion aus ChatGPT-o1 auf PDF Seite 32-33
  -- Tests for generalMoves selber geschrieben
  describe "generalMoves" $ do
  
    it "gibt alle gültigen Züge für einen General in der Mitte des Brettes zurück" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      generalMoves board White (Pos 'f' 5) `shouldMatchList` 
        [ Move (Pos 'f' 5) (Pos 'e' 5)
        , Move (Pos 'f' 5) (Pos 'e' 6)
        , Move (Pos 'f' 5) (Pos 'f' 4)
        , Move (Pos 'f' 5) (Pos 'g' 4)
        , Move (Pos 'f' 5) (Pos 'g' 5)
        , Move (Pos 'f' 5) (Pos 'g' 6)
        ]

    it "gibt alle gültigen Züge für einen General in der Mitte des Brettes zurück" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      generalMoves board Black (Pos 'e' 4) `shouldMatchList` 
        [ Move (Pos 'e' 4) (Pos 'd' 3)
        , Move (Pos 'e' 4) (Pos 'd' 4)
        , Move (Pos 'e' 4) (Pos 'd' 5)
        , Move (Pos 'e' 4) (Pos 'e' 5)
        , Move (Pos 'e' 4) (Pos 'f' 4)
        , Move (Pos 'e' 4) (Pos 'f' 3)
        ]

    it "gibt eine leere Liste zurück, wenn das Feld leer ist" $ do
      let board = buildBoard "//////////"
      generalMoves board White (Pos 'e' 4) `shouldBe` []

    it "gibt eine leere Liste zurück, wenn der General nicht die Farbe des angegebenen Spielers hat" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      generalMoves board White (Pos 'e' 4) `shouldBe` []
      generalMoves board Black (Pos 'f' 5) `shouldBe` []


    it "gibt True zurück, wenn der Gegner keine gültigen Züge mehr hat" $ do
      let board = buildBoard "4W3wG/1w1w1w1www/1w1w1w1w1w/1w1w1w1w1w/1w1w1wWw1w/1w1w1w1w1w/1w1w1w1w1w/7B2"
      generalMoves board Black (Pos 'j' 9) `shouldBe` []






-- Tests for soliderMoves selber geschrieben
  describe "soliderMoves" $ do
  
    it "gibt alle normalen Züge für einen Soldaten zurück" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      soldierMoves board White (Pos 'b' 6) `shouldMatchList`
        [ Move (Pos 'b' 6) (Pos 'a' 6)
        , Move (Pos 'b' 6) (Pos 'b' 5)
        , Move (Pos 'b' 6) (Pos 'c' 6)
        ]

    it "gibt Angriffszüge für einen Soldaten zurück" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/1w1w1g4/4G5/bwb1b1b1b1/bbb1b1b1b1/b1b1b1b1b1/7B2"
      soldierMoves board White (Pos 'b' 3) `shouldMatchList`
        [ Move (Pos 'b' 3) (Pos 'a' 2)
        , Move (Pos 'b' 3) (Pos 'b' 2)
        , Move (Pos 'b' 3) (Pos 'c' 2)
        ]

    it "gibt Rückzugszüge für einen Soldaten zurück, wenn er bedroht ist" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/bwb1b1b1b1/bbb1b1b1b1/b1b1b1b1b1/7B2"
      soldierMoves board White (Pos 'b' 3) `shouldMatchList`
        [ Move (Pos 'b' 3) (Pos 'b' 5)
        , Move (Pos 'b' 3) (Pos 'd' 5)
        , Move (Pos 'b' 3) (Pos 'a' 2)
        , Move (Pos 'b' 3) (Pos 'b' 2)
        , Move (Pos 'b' 3) (Pos 'c' 2)
        ]

    it "normaler Move mit Bedrohung" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/2b1G5/bwb1b1b1b1/w1b1b1b1b1/b1b1b1b1b1/7B2"
      soldierMoves board White (Pos 'b' 3) `shouldMatchList`
        [ Move (Pos 'b' 3) (Pos 'b' 2)
        , Move (Pos 'b' 3) (Pos 'b' 5)
        , Move (Pos 'b' 3) (Pos 'c' 2)
        ]
    
    it "angriffe auf General und Flagge" $ do
      let board = buildBoard "4w5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/4g5/2gWG5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      soldierMoves board Black (Pos 'c' 3) `shouldMatchList`
        [ Move (Pos 'c' 3) (Pos 'b' 3)
        , Move (Pos 'c' 3) (Pos 'd' 4)
        , Move (Pos 'c' 3) (Pos 'c' 4)
        ]

    it "gibt Rückzugszüge für einen Soldaten zurück, wenn er bedroht ist" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1www1w1w1w/1wbw1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      soldierMoves board Black (Pos 'c' 6) `shouldMatchList`
        [ Move (Pos 'c' 6) (Pos 'b' 7)
        , Move (Pos 'c' 6) (Pos 'c' 7)
        , Move (Pos 'c' 6) (Pos 'd' 7)
        , Move (Pos 'c' 6) (Pos 'c' 4)
        , Move (Pos 'c' 6) (Pos 'a' 4)
        ]

    it "kein Ritter" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1www1w1w1w/1wbw1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      soldierMoves board Black (Pos 'c' 5) `shouldMatchList` []



  ---Funktion aus ChatGPT-o1 auf PDF Seite 71-72
  -- Test Cases for catapultMoves selber geschrieben
  describe "catapultMoves" $ do
  
    it "gibt eine leere Liste zurück, wenn die Position nicht Teil eines Katapults ist" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'd' 6) `shouldBe` []

    it "gibt eine leere Liste zurück, wenn die Position nicht Teil eines Katapults ist" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'd' 4) `shouldBe` []

    it "gibt eine leere Liste zurück, wenn das Stück kein Soldat des angegebenen Spielers ist" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'd' 5) `shouldBe` []

    it "gibt Bewegungen für ein gültiges Katapult zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board White (Pos 'd' 6) `shouldMatchList`
        [ Move (Pos 'd' 6) (Pos 'a' 6)
        , Move (Pos 'd' 6) (Pos 'a' 9)
        , Move (Pos 'd' 6) (Pos 'd' 9)
        ]
      
    it "gibt Schüsse für ein Katapult mit einem General in Reichweite (Schwarz) zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3w1g4/4bG4/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'g' 2) `shouldMatchList`
        [ Move (Pos 'g' 2) (Pos 'b' 7)
        , Move (Pos 'g' 2) (Pos 'c' 6)
        , Move (Pos 'g' 2) (Pos 'd' 2)
        , Move (Pos 'g' 2) (Pos 'j' 2)
        ]
      
    it "gibt Schüsse für ein Katapult mit einem General in Reichweite (Schwarz) zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3w1g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'g' 3) `shouldMatchList`
        [ Move (Pos 'g' 3) (Pos 'd' 0)
        , Move (Pos 'g' 3) (Pos 'j' 0)
        , Move (Pos 'g' 3) (Pos 'j' 3)
        ]

    it "gibt Move für ein Katapult (Schwarz) zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'e' 3) `shouldMatchList`
        [ Move (Pos 'e' 3) (Pos 'b' 3)
        , Move (Pos 'e' 3) (Pos 'e' 0)
        ]
    
    it "gibt Schüsse für ein Katapult mit einem General in Reichweite (Weiß) zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board White (Pos 'f' 8) `shouldMatchList`
        [ Move (Pos 'f' 8) (Pos 'f' 3)
        ]

    it "gibt Move für ein Katapult (Weiß) zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board White (Pos 'b' 6) `shouldMatchList`
        [ Move (Pos 'b' 6) (Pos 'e' 6)
        , Move (Pos 'b' 6) (Pos 'b' 9)
        ]

    it "gibt Schüsse für ein Katapult mit einem General in Reichweite horizontal (Weiß) zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3wwg4/4bG4/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board White (Pos 'c' 7) `shouldMatchList`
        [ Move (Pos 'c' 7) (Pos 'g' 3)
        , Move (Pos 'c' 7) (Pos 'h' 2)
        ]

    it "gibt Schüsse für ein Katapult ohne einen General in Reichweite horizontal (Weiß) zurück" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1bbw1w/1g8/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board White (Pos 'b' 6) `shouldMatchList`
        [ Move (Pos 'b' 6) (Pos 'e' 6)
        , Move (Pos 'b' 6) (Pos 'f' 6)
        , Move (Pos 'b' 6) (Pos 'b' 9)
        , Move (Pos 'b' 6) (Pos 'g' 6)
        ]    

    it "gibt Schüsse für ein Katapult mit einem General in Reichweite auf eine Flagge zurück(nicht)" $ do
      let board = buildBoard "4w5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3Wg5/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'f' 3) `shouldMatchList`
        [ Move (Pos 'f' 3) (Pos 'i' 0)
        ]
    
    it "gibt Schüsse für ein Katapult mit einem General in Reichweite auf einen General zurück(nicht)" $ do
      let board = buildBoard "4w5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3g6/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'f' 3) `shouldMatchList`
        [ Move (Pos 'f' 3) (Pos 'i' 0)
        ]
      
    it "gibt Schüsse für ein Katapult mit einem General in Reichweite auf eine Flagge zurück" $ do
      let board = buildBoard "4w5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3Wg5/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'h' 1) `shouldMatchList`
        [ Move (Pos 'h' 1) (Pos 'd' 5)
        , Move (Pos 'h' 1) (Pos 'c' 6)
        , Move (Pos 'h' 1) (Pos 'h' 4)
        ]
    
    it "gibt Schüsse für ein Katapult mit einem General in Reichweite auf einen General zurück" $ do
      let board = buildBoard "4w5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3g6/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'h' 1) `shouldMatchList`
        [ Move (Pos 'h' 1) (Pos 'd' 5)
        , Move (Pos 'h' 1) (Pos 'c' 6)
        , Move (Pos 'h' 1) (Pos 'h' 4)
        ]

    it "gibt keine Schüsse zurück, wenn kein General in Reichweit, als Spitze" $ do
      let board = buildBoard "4w5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/3Wg5//b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'f' 3) `shouldMatchList`
        [ Move (Pos 'f' 3) (Pos 'i' 0)
        ]
    
    it "gibt keine Schüsse zurück, wenn kein General in Reichweite, als rechts diago" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'f' 2) `shouldMatchList` []
  
    it "gibt keine Schüsse zurück, wenn kein General in Reichweite, als links diago" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'h' 2) `shouldMatchList` []
    
    ---Funktion aus ChatGPT-o1 auf PDF Seite 59-62
    it "Catapult Erkennen als rechts diago" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      findCatapults board Black (Pos 'e' 1) `shouldMatchList` [[Pos 'e' 1, Pos 'e' 2, Pos 'e' 3], [Pos 'e' 1, Pos 'f' 2, Pos 'g' 3]]

    it "Catapult Erkennen als links diago" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      findCatapults board Black (Pos 'i' 1) `shouldMatchList` [[Pos 'i' 1, Pos 'i' 2, Pos 'i' 3], [Pos 'i' 1, Pos 'h' 2, Pos 'g' 3]]
        
    it "Catapult Erkennen 1" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      findCatapults board White (Pos 'f' 7) `shouldMatchList` [[Pos 'f' 6, Pos 'f' 7, Pos 'f' 8]]

    it "Catapult Erkennen 2" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      findCatapults board Black (Pos 'c' 3) `shouldMatchList` [[Pos 'c' 3, Pos 'd' 3,  Pos 'e' 3], [Pos 'c' 1, Pos 'c' 2,  Pos 'c' 3]]

    it "Schwarz Katapult Rückzug Waage Senkr oben" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'a' 3) `shouldMatchList`
        [ Move (Pos 'a' 3) (Pos 'a' 0)
        ]
    
    it "Schwarz Katapult Rückzug Senkr unten" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'a' 1) `shouldMatchList`
        [ Move (Pos 'a' 1) (Pos 'a' 4)
        ]
    
    it "Schwarz Katapult Rückzug Senkr mitte" $ do
      let board = buildBoard "4W5/wwww1w1w1w/1www1w1w1w/1www1w1w1w/5g4/4G5/b1bbbbbbb1/b1b1bbbbb1/b1b1b2bb1/7B2"
      catapultMoves board Black (Pos 'a' 2) `shouldMatchList` []


    --Spitzen test
    it "finds the upper tip for a white player's catapult" $ do
      let catapult = [Pos 'd' 5, Pos 'f' 5, Pos 'e' 5]
      obereSpitzefinden White catapult `shouldBe` Pos 'd' 5 -- Lexikografisch minimal

    it "finds the upper tip for a black player's catapult" $ do
      let catapult = [Pos 'd' 5, Pos 'f' 5, Pos 'e' 5]
      obereSpitzefinden Black catapult `shouldBe` Pos 'f' 5 -- Lexikografisch maximal

    it "finds the lower tip for a white player's catapult" $ do
      let catapult = [Pos 'd' 5, Pos 'f' 5, Pos 'e' 5]
      untereSpitzefinden White catapult `shouldBe` Pos 'f' 5 -- Lexikografisch maximal

    it "finds the lower tip for a black player's catapult" $ do
      let catapult = [Pos 'd' 5, Pos 'f' 5, Pos 'e' 5]
      untereSpitzefinden Black catapult `shouldBe` Pos 'd' 5 -- Lexikografisch minimal





    it "Schwarz in einer reihe D3" $ do
      let board = buildBoard "/////4G5/wwwbbbwwww///"
      catapultMoves board Black (Pos 'd' 3) `shouldMatchList` 
        [ Move (Pos 'd' 3) (Pos 'h' 3)
        , Move (Pos 'd' 3) (Pos 'i' 3)
        ]

    it "Schwarz in einer reihe F3" $ do
      let board = buildBoard "/////4G5/wwwbbbwwww///"
      catapultMoves board Black (Pos 'f' 3) `shouldMatchList`
        [ Move (Pos 'f' 3) (Pos 'b' 3)
        , Move (Pos 'f' 3) (Pos 'a' 3)
        ]

    it "Weiß in einer reihe D3" $ do
      let board = buildBoard "/////4g5/bbbwwwbbbb///"
      catapultMoves board White (Pos 'd' 3) `shouldMatchList`
        [ Move (Pos 'd' 3) (Pos 'h' 3)
        , Move (Pos 'd' 3) (Pos 'i' 3)
        ]

    it "Weiß in einer reihe F3" $ do
      let board = buildBoard "/////4g5/bbbwwwbbbb///"
      catapultMoves board White (Pos 'f' 3) `shouldMatchList`
        [ Move (Pos 'f' 3) (Pos 'b' 3)
        , Move (Pos 'f' 3) (Pos 'a' 3)
        ]


---Tests aus ChatGPT-o1 auf PDF Seite 87-90
  -- Test Cases for playerWon selber geschrieben
  describe "playerWon" $ do

    it "gibt True zurück, wenn keine schwarze Flagge da ist" $ do
      let board = buildBoard "4W5/1www1w1w1w/1w1w1w1w1w/5g4/4Gb4/b1b1b1b1b1/1bbb1b1b1b/1b1b1b1b1b//82"
      playerWon board White `shouldBe` True
      playerWon board Black `shouldBe` False

    it "gibt True zurück, wenn keine weiße Flagge da ist" $ do
      let board = buildBoard "/1www1w1w1w/1w1w1w1w1w/5g4/4Gb4/b1b1b1b1b1/1bbb1b1b1b/1b1b1b1b1b//B9"
      playerWon board Black `shouldBe` True
      playerWon board White `shouldBe` False

    it "gibt True zurück, wenn der Gegner keine gültigen Züge mehr hat" $ do
      let board = buildBoard "4W3wG/1w1w1w1www/1w1w1w1w1w/1w1w1w1w1w/1w1w1wgw1w/1w1w1w1w1w/1w1w1w1w1w///7B2"
      playerWon board White `shouldBe` True
      playerWon board Black `shouldBe` False
    
    it "keine gültigen Züge mehr hat" $ do
      let board = buildBoard "4W3wG/1w1w1w1www/1w1w1w1w1w/1w1w1w1w1w/1w1w1wgw1w/1w1w1w1w1w/1w1w1w1w1w///7B2"
      listMoves board Black `shouldMatchList`  []

    it "gibt True zurück, wenn der kein weißer General" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/64/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      playerWon board Black `shouldBe` True
      playerWon board White `shouldBe` False

    it "gibt True zurück, wenn der kein schwarzer General" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/55/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      playerWon board White `shouldBe` True
      playerWon board Black `shouldBe` False

      


  -- Test Cases for listMoves selber geschrieben
  describe "listMoves" $ do
    
    it "gibt nur flagMoves zurück" $ do
      let board = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      listMoves board White `shouldMatchList`  
        [ Move (Pos 'b' 9) (Pos 'b' 9)
        , Move (Pos 'c' 9) (Pos 'c' 9)
        , Move (Pos 'd' 9) (Pos 'd' 9)
        , Move (Pos 'e' 9) (Pos 'e' 9)
        , Move (Pos 'f' 9) (Pos 'f' 9)
        , Move (Pos 'g' 9) (Pos 'g' 9)
        , Move (Pos 'h' 9) (Pos 'h' 9)
        , Move (Pos 'i' 9) (Pos 'i' 9)
        ]
    
    it "gibt nur normale Züge zurück" $ do
      let board = buildBoard "4W5///1w8/5g4/4G5/8b1///7B2"
      listMoves board White `shouldMatchList`  
        [ Move (Pos 'b' 6) (Pos 'b' 5)
        , Move (Pos 'b' 6) (Pos 'a' 6)
        , Move (Pos 'b' 6) (Pos 'c' 6)
        , Move (Pos 'f' 5) (Pos 'e' 5)
        , Move (Pos 'f' 5) (Pos 'e' 6)
        , Move (Pos 'f' 5) (Pos 'f' 6)
        , Move (Pos 'f' 5) (Pos 'g' 6)
        , Move (Pos 'f' 5) (Pos 'g' 5)
        , Move (Pos 'f' 5) (Pos 'g' 4)
        , Move (Pos 'f' 5) (Pos 'f' 4)
        ]

    it "gibt nur gültige Flaggenzüge für Schwarz zurück, wenn die weiße Flagge platziert ist 1" $ do
      let board = buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      listMoves board Black `shouldMatchList` 
        [ Move (Pos 'b' 0) (Pos 'b' 0)
        , Move (Pos 'c' 0) (Pos 'c' 0)
        , Move (Pos 'd' 0) (Pos 'd' 0)
        , Move (Pos 'e' 0) (Pos 'e' 0)
        , Move (Pos 'f' 0) (Pos 'f' 0)
        , Move (Pos 'g' 0) (Pos 'g' 0)
        , Move (Pos 'h' 0) (Pos 'h' 0)
        , Move (Pos 'i' 0) (Pos 'i' 0)
        ]
      
    it "gibt nur gültige Flaggenzüge für Schwarz zurück, wenn die weiße Flagge platziert ist 2" $ do
      let board = buildBoard "4W5////////8b1/"
      listMoves board Black `shouldMatchList`
          [ Move (Pos 'i' 1) (Pos 'j' 1)
          , Move (Pos 'i' 1) (Pos 'i' 2)
          , Move (Pos 'i' 1) (Pos 'h' 1)
          ]

    it "gibt nur gültige Flaggenzüge für Schwarz zurück, wenn die weiße Flagge platziert ist 4" $ do
      let board = buildBoard "w3W5/////////"
      listMoves board White `shouldMatchList` [
                Move (Pos 'a' 9) (Pos 'b' 9),
                Move (Pos 'a' 9) (Pos 'a' 8)
                ]