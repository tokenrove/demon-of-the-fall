;; -*- Lisp -*-
;; This is a data file.  See room.lisp for details.
((:test-room-a
     (:name . "Test Room A")
     (:floor . #2A((0 0 0 1 0 0 0)
		   (0 0 1 1 1 0 0)
		   (0 1 1 1 1 1 0)
		   (0 0 1 1 1 0 0)
		   (0 0 1 1 1 0 0)
		   (1 1 1 1 1 1 1)
		   (1 1 1 1 1 1 1)
		   (0 0 1 1 1 0 0)
		   (0 0 0 0 0 0 0)))
     (:blocks (9 4 0 4) (9 4 0 5) (7 4 2 4))
     (:actors (:float-block (256 64 384) 42))
     (:exits (:floor :test-room-a :ceiling)
      ((3 . 0) :test-room-b (3 . 8))
      ((0 . 5) :maze-room (0 . 0)))
     (:player-spawn (256 0 384)))
    (:test-room-b
     (:name . "Test Room B")
     (:floor . #2A((0 0 0 0 0 0 0)
		   (0 1 1 1 1 1 0)
		   (0 1 1 1 1 1 0)
		   (0 1 1 1 1 1 0)
		   (0 1 1 1 1 1 0)
		   (0 1 1 1 1 1 0)
		   (0 1 1 1 1 1 0)
		   (0 1 1 1 1 1 0)
		   (0 0 0 1 0 0 0)))
     (:exits ((3 . 8) :test-room-a (3 . 0))
      ((6 . 5) :test-room-a (3 . 5)))
     (:player-spawn (192 0 512))))