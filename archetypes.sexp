;; -*- Lisp -*-
;; This is a data file, not a program file.  Don't put routines here.
((:peter
  (:sprite
   (:image "ret-data/petsheet.pcx")
   (:blit-offset (12 . 0))
   (:frames ((0 0 24 48) (24 0 24 48) (48 0 24 48) (72 0 24 48)
	     (96 0 24 48) (120 0 24 48) (144 0 24 48) (168 0 24 48)
	     (192 0 24 48) (216 0 24 48) (240 0 24 48) (264 0 24 48)
	     ;; mirrored frames
	     (0 48 24 48) (24 48 24 48) (48 48 24 48) (72 48 24 48)
	     (96 48 24 48) (120 48 24 48) (144 48 24 48) (168 48 24 48)
	     (192 48 24 48) (216 48 24 48) (240 48 24 48) (264 48 24 48)))
   (:animations ((:default (0 . 60))
		 (:walk-east (1 . 5)
			     (2 . 5)
			     (3 . 5)
			     (2 . 5))
		 (:walk-west (18 . 5)
			     (17 . 5)
			     (16 . 5)
			     (17 . 5))
		 (:walk-north (5 . 5)
			      (6 . 5)
			      (7 . 5)
			      (6 . 5))
		 (:walk-south (22 . 5)
			      (21 . 5)
			      (20 . 5)
			      (21 . 5))
		 (:stand-east (0 . 60))
		 (:stand-west (19 . 60))
		 (:stand-north (4 . 60))
		 (:stand-south (23 . 60))
		 (:crouch-east (8 . 60))
		 (:jump-east	(9 . 60))
		 (:crouch-west (13 . 60))
		 (:jump-west	(12 . 60))
		 (:crouch-north (10 . 60))
		 (:jump-north (11 . 60))
		 (:crouch-south (15 . 60))
		 (:jump-south (14 . 60)))))
  (:box
   (0 0 0)
   (24 36 24))
  (:handler
   equinox:create-human-input-handler)
  (:contact
   equinox:player-contact-handler))
 (:evil-peter
  (:sprite
   (:image "ret-data/evilpete.pcx")
   (:blit-offset (12 . 0))
   (:frames ((0 0 24 48)
	     (24 0 24 48)
	     (48 0 24 48)
	     (72 0 24 48)
	     (96 0 24 48)
	     (120 0 24 48)
	     (144 0 24 48)
	     (168 0 24 48)
	     (192 0 24 48)
	     (216 0 24 48)
	     (240 0 24 48)
	     (264 0 24 48)
	     (288 0 24 48)
	     ;; mirrored frames
	     (0 48 24 48)
	     (24 48 24 48)
	     (48 48 24 48)
	     (72 48 24 48)
	     (96 48 24 48)
	     (120 48 24 48)
	     (144 48 24 48)
	     (168 48 24 48)
	     (192 48 24 48)
	     (216 48 24 48)
	     (240 48 24 48)
	     (264 48 24 48)
	     (288 48 24 48)))
   (:animations ((:default (0 . 60))
		 (:walk-east (1 . 5)
			     (2 . 5)
			     (3 . 5)
			     (2 . 5))
		 (:walk-west (18 . 5)
			     (17 . 5)
			     (16 . 5)
			     (17 . 5))
		 (:walk-north (5 . 5)
			      (6 . 5)
			      (7 . 5)
			      (6 . 5))
		 (:walk-south (22 . 5)
			      (21 . 5)
			      (20 . 5)
			      (21 . 5))
		 (:stand-east (0 . 60))
		 (:stand-west (19 . 60))
		 (:stand-north (4 . 60))
		 (:stand-south (23 . 60))
		 (:crouch-east (8 . 60))
		 (:jump-east	(9 . 60))
		 (:crouch-west (13 . 60))
		 (:jump-west	(12 . 60))
		 (:crouch-north (10 . 60))
		 (:jump-north (11 . 60))
		 (:crouch-south (15 . 60))
		 (:jump-south (14 . 60)))))
  (:box
   (0 0 0)
   (24 36 24))
  (:handler
   equinox:create-monster-handler)
  (:contact
   equinox:monster-contact-handler))
 (:apple
  (:sprite
   (:image "other-data/apple.pcx")
   (:blit-offset (0 . 0))
   (:frames ((0 0 16 16)))
   (:animations ((:default (0 . 60)))))
  (:handler
   equinox:create-do-nothing-handler)
  (:contact
   equinox:loot-contact-handler)
  (:box
   (0 0 0)
   (8 8 8)))
 (:dagger
  (:sprite
   (:image "other-data/dagger.pcx")
   (:blit-offset (0 . 0))
   (:frames ((0 0 16 16)))
   (:animations ((:default (0 . 60)))))
  (:handler
   equinox:create-do-nothing-handler)
  (:contact
   equinox:loot-contact-handler)
  (:box
   (0 0 0)
   (8 8 8)))
 (:push-block
  (:sprite
   (:image "ret-data/block.pcx")
   (:blit-offset (32 . 0))
   (:frames ((0 0 64 64)))
   (:animations ((:default (0 . 60)))))
  (:handler
   equinox:create-do-nothing-handler)
  (:contact
   equinox:pushable-block-handler)
  (:box
   (0 0 0)
   (64 32 64)))
 (:float-block
  (:sprite
   (:image "ret-data/bl-cushi.pcx")
   (:blit-offset (32 . 0))
   (:frames ((0 0 64 64)))
   (:animations ((:default (0 . 60)))))
  (:handler
   equinox:create-floating-block-handler)
  (:contact
   equinox:pushable-block-handler)
  (:box
   (0 0 0)
   (64 32 64)))
 (:floor-block
  (:sprite
   (:image "ret-data/fl-check.pcx")
   (:blit-offset (32 . 0))
   (:frames ((0 0 64 40)))
   (:animations ((:default (0 . 60)))))
  (:handler
   equinox:create-floating-block-handler)
  (:contact
   equinox:pushable-block-handler)
  (:box
   (0 0 0)
   (64 32 40)))
 #+nil(:iron-key
  (:sprite
   (:image "ret-data/iron-key.pcx")
   (:blit-offset (16 . 0))
   (:frames ((0 0 32 32)
	     (32 0 32 32)
	     (64 0 32 32)
	     (96 0 32 32)
	     (128 0 32 32)
	     (160 0 32 32)
	     (192 0 32 32)
	     (224 0 32 32)
	     (256 0 32 32)
	     (288 0 32 32)
	     (320 0 32 32)))
   (:animations ((:default (0 . 2)
		     (1 . 2)
		   (2 . 2)
		   (3 . 2)
		   (4 . 2)
		   (5 . 2)
		   (6 . 2)
		   (7 . 2)
		   (8 . 2)
		   (9 . 2)
		   (10 . 2)
		   (11 . 2))))
   (:handler
    create-key-handler)
   (:contact
    loot-contact-handler)
   (:box
    (0 0 0)
    (8 8 8)))))
