(in-package :demon-of-the-fall)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; XXX eventually needs to figure out how to find the data.
  (assert
   (load-foreign-library "./low-level.so"
			 :module "low-level"
			 :supporting-libraries '("c")
			 :force-load t)))

(defun maybe-null->nil (ptr)
  "Convenience function -- if the supplied pointer is NULL, returns
NIL.  Returns the pointer itself otherwise."
  (unless (null-pointer-p ptr) ptr))

(defun bool->int (pred) (if pred 1 0))


;;;; GRAPHICS

;; Careful!  This should correspond with the C SDL_Rect type.
#-openmcl(def-struct gfx-rect (x :short) (y :short)
	    (w :unsigned-short) (h :unsigned-short))

(declaim (inline ll-gfx-init))
(def-function "ll_gfx_init" ((fullscreen-p :int)
			     (width :int)
			     (height :int)
			     (bpp :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-gfx-shutdown))
(def-function "ll_gfx_shutdown" ((vbuffer :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-new-image-buffer))
(def-function "ll_gfx_new_image_buffer" ((width :int)
					 (height :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-gfx-load-image))
(def-function "ll_gfx_load_image" ((filename :cstring)
				   (colorkey :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-gfx-free-surface))
(def-function "ll_gfx_free_surface" ((sface :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-use-image-palette))
(def-function "ll_gfx_use_image_palette" ((image :pointer-void)
					  (vbuffer :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-refresh-display))
(def-function "ll_gfx_refresh_display" ((vbuffer :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-surface-w))
(def-function "ll_gfx_surface_w" ((sface :pointer-void))
  :returning :int :module "low-level")
(declaim (inline ll-gfx-surface-w))
(def-function "ll_gfx_surface_h" ((sface :pointer-void))
  :returning :int :module "low-level")

(declaim (inline ll-gfx-lock-surface))
(def-function "ll_gfx_lock_surface" ((sface :pointer-void))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-unlock-surface))
(def-function "ll_gfx_unlock_surface" ((sface :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-gfx-blit-surface))
#-openmcl(def-function "ll_gfx_blit_surface" ((src :pointer-void)
				     (srect (* gfx-rect))
				     (dst :pointer-void)
				     (drect (* gfx-rect)))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-blit-surface-stub))
(def-function "ll_gfx_blit_surface_stub" ((src :pointer-void)
                                          (x :int)
                                          (y :int)
                                          (w :int)
                                          (h :int)
                                          (dst :pointer-void)
                                          (x2 :int)
                                          (y2 :int))
  :returning :void :module "low-level")


(declaim (inline ll-gfx-draw-pixel))
(def-function "ll_gfx_draw_pixel" ((sface :pointer-void)
				   (x :int)
				   (y :int)
				   (color :int))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-fill-rect))
#-openmcl(def-function "ll_gfx_fill_rect" ((sface :pointer-void)
				  (rect (* gfx-rect))
				  (color :int))
  :returning :void :module "low-level")
(declaim (inline ll-gfx-fill-rect-stub))
(def-function "ll_gfx_fill_rect_stub" ((sface :pointer-void)
                                       (x :int)
                                       (y :int)
                                       (w :int)
                                       (h :int)
                                       (color :int))
  :returning :void :module "low-level")

;;;; EVENT

(def-struct ll-event
    (type :int)
  (value :int)
  (axis :int))

;; XXX should grovel or something.  This will do for now.
(defconstant +ll-event-key-down+ 2)
(defconstant +ll-event-key-up+ 3)
(defconstant +ll-event-joy-move+ 4)
(defconstant +ll-event-joy-button-down+ 5)
(defconstant +ll-event-joy-button-up+ 6)

(declaim (inline ll-event-init))
(def-function "ll_event_init" () :returning :void :module "low-level")
(declaim (inline ll-event-shutdown))
(def-function "ll_event_shutdown" () :returning :void :module "low-level")


(declaim (inline ll-poll-event-stub ll-event-type ll-event-axis
		 ll-event-value))
(def-function "ll_poll_event_stub" ()
  :returning :int :module "low-level")
(def-function "ll_event_type" ()
  :returning :int :module "low-level")
(def-function "ll_event_axis" ()
  :returning :int :module "low-level")
(def-function "ll_event_value" ()
  :returning :int :module "low-level")

(declaim (inline ll-poll-event))
(def-function "ll_poll_event" ((event (* ll-event)))
  :returning :int :module "low-level")
(declaim (inline ll-wait-event))
(def-function "ll_wait_event" ((event (* ll-event)))
  :returning :int :module "low-level")

;;;; TIMER

(declaim (inline timer-get-ticks))
(def-function "timer_get_ticks" () :module "low-level"
	      :returning :unsigned-long)
(declaim (inline timer-start-frame))
(def-function "timer_start_frame" ((frame-length :int))
  :module "low-level" :returning :void)
(declaim (inline timer-end-frame))
(def-function "timer_end_frame" () :module "low-level" :returning :int)

;;;; FONT

(declaim (inline ll-font-init))
(def-function "ll_font_init" ()
  :returning :void :module "low-level")

(declaim (inline ll-font-open))
(def-function "ll_font_open" ((filename :cstring)
			      (ptsize :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-font-close))
(def-function "ll_font_close" ((font :pointer-void))
  :returning :void :module "low-level")

(declaim (inline ll-font-render-solid))
(def-function "ll_font_render_solid" ((font :pointer-void)
				       (string :cstring)
				       (r :int)
				       (g :int)
				       (b :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-font-render-blended))
(def-function "ll_font_render_blended" ((font :pointer-void)
					(string :cstring)
					(r :int)
					(g :int)
					(b :int))
  :returning :pointer-void :module "low-level")
(declaim (inline ll-font-render-shaded))
(def-function "ll_font_render_shaded" ((font :pointer-void)
				       (string :cstring)
				       (r1 :int)
				       (g1 :int)
				       (b1 :int)
				       (r2 :int)
				       (g2 :int)
				       (b2 :int))
  :returning :pointer-void :module "low-level")
