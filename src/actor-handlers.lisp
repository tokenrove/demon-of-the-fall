(in-package :demon-of-the-fall)

(defclass generic-block (actor)
  ()
  (:documentation "The base class for all blocks and furniture."))

(defclass pushable-block (generic-block)
  ())

(defclass floating-block (generic-block)
  ((direction :initarg :direction)
   (top :initarg :top)
   (bottom :initarg :bottom))
  (:default-initargs :direction :up :top 42 :bottom 0))

(defmethod update ((actor floating-block) room elapsed-time)
  "Create an actor handler which floats up and down."
  (declare (ignore room elapsed-time))  ; XXX should use room's gravity etc
  (with-slots (direction top bottom) actor
    (when (contact-surface-of actor)
      (setf direction :up))
    (if (eql direction :up)
        ;; XXX if there's something standing on me, push up to compensate.
        (if (< (iso-point-y (position-of actor)) top)
            (setf (iso-point-y (velocity-of actor)) 0.7)
            (setf direction :down))
        (if (> (iso-point-y (position-of actor)) bottom)
            (setf (iso-point-y (velocity-of actor)) -0.2)
            (setf direction :up)))))

(defclass human (actor)
  ()
  (:documentation "A human-controlled actor.  See also PETER."))

(defmethod update ((player human) room elapsed-time)
  "Updates ME based on current input events."
  (declare (ignore room elapsed-time))
  (let ((pressed-p nil)
        (speed 0.5))
    (when (fetus:event-pressedp :up)
      (setf (facing-of player) :north)
      (apply-impulse player :x speed)
      (setf pressed-p t)
      (fetus:set-sprite-animation (sprite-of player) :walk-north))
    (when (fetus:event-pressedp :down)
      (setf (facing-of player) :south)
      (apply-impulse player :x (- speed))
      (setf pressed-p t)
      (fetus:set-sprite-animation (sprite-of player) :walk-south))
    (when (fetus:event-pressedp :left)
      (setf (facing-of player) :west)
      (apply-impulse player :z speed)
      (setf pressed-p t)
      (fetus:set-sprite-animation (sprite-of player) :walk-west))
    (when (fetus:event-pressedp :right)
      (setf (facing-of player) :east)
      (apply-impulse player :z (- speed))
      (setf pressed-p t)
      (fetus:set-sprite-animation (sprite-of player) :walk-east))
    (unless pressed-p
      (fetus:set-sprite-animation (sprite-of player)
                                  (case (facing-of player)
                                    (:east :stand-east)
                                    (:west :stand-west)
                                    (:north :stand-north)
                                    (:south :stand-south))))))

(defclass peter (human) ()
  (:documentation "Peter:

First accessible
Well-rounded
Can use sword/wand
Can push light-block
average jumping skills
Weapon: SWORD"))

(defmethod update :after ((me peter) room elapsed-time)
  ;; when the action button is triggered,
  ;;   if we're holding something that can be dropped, drop it
  ;;   if we're standing on something that can be grabbed, grab it
  ;;   if we're wielding a weapon, use it.
  (declare (ignore room elapsed-time))
  (when (and (fetus:event-pressedp :button-a)
             (contact-surface-of me))
    (apply-impulse me :y 6)))

(defclass mendez (human) ()
  (:documentation "Mendez:
Second accessible
Can barely jump -only barely make onto waist-height block
Heavy blocks can land on his head with no damage (crushes others)
Can push heavy-blocks
Can take/do more damage
Weapon: AXE
"))

(defclass lopez (human) ()
  (:documentation "Lopez:
Third accessable
Can jump higher/longer, and change direction during jump
No combat abilities
Weapon: None?
"))

(defclass mikael (human) ()
  (:documentation "Mikael:
Last accessible
Cannot push any kind of block
When jump button is held at the height of jump, he begins to float
Magickal range attack (can be used while floating)
Weapon: PSYCHIC POWERS
"))


(defmethod notify ((us pushable-block) where (what (eql :contact)) &key with axis impulse)
  (declare (ignore with))
  (decf (iso-point-component axis (velocity-of us))
        (iso-point-component axis impulse)))

(defmethod notify ((us human) where (what (eql :contact)) &key with axis impulse)
  (declare (ignore with))
  (decf (iso-point-component axis (velocity-of us))
        (iso-point-component axis impulse)))

(defclass loot (actor)
  ()
  (:documentation "Base class for actors that are consumed on contact
  with a player."))

(defclass enemy (actor)
  ()
  (:documentation "Base class for actors that kill the player on
  contact."))

;; (defun monster-contact-handler (us them face impulse)
;;   (declare (ignore us them face impulse))
;; ;;    if something is on top of us,
;; ;;        sink its horizontal velocities by our friction,
;; ;;        add our velocity to its velocity.
;; ;;    if we're touching the player at all,
;; ;;        kill them.
;;   )

;; (defun loot-contact-handler (us them face impulse)
;;   (declare (ignore us them face impulse))
;;   ;; if we're touching the player, they can have us.
;;   )
