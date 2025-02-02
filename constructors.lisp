;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

(See defpackage.lisp for license and copyright notigification)

|#

(in-package :cells)

(eval-now!
  (export '(.cache-bound-p

            ;; Cells Constructors
            c?n
            c?once
            c?n-until
            c?1
            c_1
            c?+n

            ;; Debug Macros and Functions
            c?dbg
            c_?dbg
            c-input-dbg

            )))

;___________________ constructors _______________________________

(defmacro c-lambda (&body body)
  `(c-lambda-var (slot-c) ,@body))

(defmacro c-lambda-var ((c) &body body)
  `(lambda (,c &aux (self (c-model ,c))
             (.cache (c-value ,c))
             (.cache-bound-p (cache-bound-p ,c)))
     (declare (ignorable .cache .cache-bound-p self))
     ,@body))

(defmacro with-c-cache ((fn) &body body)
  (let ((new (gensym)))
    `(or (bwhen (,new (progn ,@body))
           (funcall ,fn ,new .cache))
       .cache)))

;-----------------------------------------

(defmacro make-input-cell (&key (initial-value nil initial-value-p) debug)
  "Makes a settable, non-recomputed cell, optionally initialized to
`initial-value'.  (If not initialized, the cell is unbound, and an attempt
to read it before setting it will fail.)  If `debug' is true, debug tracing
of the cell will be possible."
  `(make-cell :inputp t
	      ,@(if initial-value-p `(:value ,initial-value :value-state ':valid)
		  '(:value-state ':unbound))
	      :debug ,debug))

(defmacro make-computed-cell ((&key settable (auto-update t) lazy debug)
			      &body body)
  "Makes a computed cell.  If `settable' is true, the value can be set
explicitly.  If `auto-update' is true (the default), the cell's value will be
recomputed automatically when the values of referenced cells change.  `lazy'
controls when the updates occur; it can be one of `nil', `t' or `:always',
`:once-asked', or `:until-asked':

   `nil': the cell is updated eagerly, as soon as referenced cells change
   `t' / `:always': the cell is updated only when read and referenced
          values have changed
   `:once-asked': like `:always' except that the cell's initial value is
          computed eagerly
   `:until-asked': the cell is not updated until it is first read, at
          which point it becomes eager

If `debug' is true, debug tracing of the cell will be possible.
`body' is an implicit `progn'; `self' is bound, around it, to the object
whose slot contains the cell."
  (assert (member lazy '(nil t :once-asked :until-asked :always)))
  (let ((args `(:inputp ,settable :lazy ,lazy :debug ,debug :value-state ':unevaluated)))
    (cond ((eq auto-update t)
	   `(make-c-dependent ,@args
			      :code ',body
			      :rule (c-lambda . ,body)))
	  ((eq auto-update nil)
	   `(make-c-dependent ,@args
			      :code '((without-c-dependency . ,body))
			      :rule (c-lambda (without-c-dependency . ,body))))
	  (t
	   ;; Have to defer the choice to runtime.
	   (let ((au-var (gensym "AUTO-UPDATE-")))
	     `(let ((,au-var ,auto-update))
		(make-c-dependent ,@args
				  :code (if ,au-var ',body
					  '((without-c-dependency . ,body)))
				  :rule (if ,au-var (c-lambda . ,body)
					  (c-lambda (without-c-dependency . ,body))))))))))

(defmacro c? (&body body)
  "Makes a non-settable cell initialized to the value of `body'.  If `body'
references other cells, it will be recomputed whenever the values of those cells
change.  Binds `self', around `body', to the object whose slot contains the cell."
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :rule (c-lambda ,@body)))

(defmacro c?+n (&body body)
  "Makes a settable cell initialized to the value of `body'.  If `body' references
other cells, it will be recomputed whenever the values of those cells change.
Binds `self', around `body', to the object whose slot contains the cell."
  `(make-c-dependent
    :inputp t
    :code ',body
    :value-state :unevaluated
    :rule (c-lambda ,@body)))

(defmacro c?n (&body body)
  "Makes a settable cell initialized to the value of `body'.  Does NOT recompute
`body' when its referenced cells change.  Binds `self', around `body', to the
object whose slot contains the cell."
  `(make-c-dependent
    :code '(without-c-dependency ,@body)
    :inputp t
    :value-state :unevaluated
    :rule (c-lambda (without-c-dependency ,@body))))

(defmacro c?n-dbg (&body body)
  "Makes a settable cell initialized to the value of `body', with debug tracing
enabled.  Does NOT recompute `body' when its referenced cells change.  Binds
`self', around `body', to the object whose slot contains the cell."
  `(make-c-dependent
    :code '(without-c-dependency ,@body)
    :inputp t
    :debug t
    :value-state :unevaluated
    :rule (c-lambda (without-c-dependency ,@body))))

(defmacro c?n-until (args &body body)
  `(make-c-dependent
    :optimize :when-value-t
    :code ',body
    :inputp t
    :value-state :unevaluated
    :rule (c-lambda ,@body)
    ,@args))

(defmacro c?once (&body body)
  "Makes a non-settable cell initialized to the value of `body'.  Does NOT
recompute `body' when its referenced cells change.  Binds `self', around `body',
to the object whose slot contains the cell."
  `(make-c-dependent
    :code '(without-c-dependency ,@body)
    :inputp nil
    :value-state :unevaluated
    :rule (c-lambda (without-c-dependency ,@body))))

(defmacro c_1 (&body body)
  "Makes a non-settable, fully lazy cell that will be initialized to the value
of `body' when it is first read.  Does NOT recompute `body' when its referenced
cells change.  Binds `self', around `body', to the object whose slot contains
the cell."
  `(make-c-dependent
    :code '(without-c-dependency ,@body)
    :inputp nil
    :lazy t
    :value-state :unevaluated
    :rule (c-lambda (without-c-dependency ,@body))))

(defmacro c?1 (&body body)
  "Makes a non-settable cell initialized to the value of `body'.  Does NOT
recompute `body' when its referenced cells change.  Binds `self', around `body',
to the object whose slot contains the cell."
  `(c?once ,@body))

(defmacro c?dbg (&body body)
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :debug t
    :rule (c-lambda ,@body)))

(defmacro c?_ (&body body)
  "Makes a non-settable, fully lazy cell that will be initialized to the value
of `body' when it is first read.  If `body' references other cells, it will be
recomputed whenever it is referenced and the values of those cells have changed.
Binds `self', around `body', to the object whose slot contains the cell."
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :lazy t
    :rule (c-lambda ,@body)))

(defmacro c_? (&body body)
  "Makes a non-settable, lazy-until-asked cell initialized to the value of `body'.  If
`body' references other cells, it will be recomputed after it has first been
read, whenever the values of those cells change.  Binds `self', around `body',
to the object whose slot contains the cell."
  "Lazy until asked, then eagerly propagating"
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :lazy :until-asked
    :rule (c-lambda ,@body)))

(defmacro c_?dbg (&body body)
  "Lazy until asked, then eagerly propagating"
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :lazy :until-asked
    :rule (c-lambda ,@body)
    :debug t))

(defmacro c?? ((&key (tagp nil) (in nil) (out t))&body body)
  (let ((result (copy-symbol 'result))
        (thetag (gensym)))
     `(make-c-dependent
       :code ',body
       :value-state :unevaluated
       :rule (c-lambda
              (let ((,thetag (gensym "tag"))
                    (*trcdepth* (1+ *trcdepth*))
                    )
                (declare (ignorable self ,thetag))
                ,(when in
                   `(trc "c??> entry" (c-slot-name c) (c-model c) (when ,tagp ,thetag)))
                (count-it :c?? (c-slot-name c) (md-name (c-model c)))
                (let ((,result (progn ,@body)))
                  ,(when out `(trc "c?? result:" ,result (c-slot-name c) (when ,tagp ,thetag)))
                  ,result))))))

(defmacro c-formula ((&rest keys &key lazy &allow-other-keys) &body forms)
  "Makes a cell initialized to the value of `forms'.  If `forms' references
other cells, it will be recomputed whenever the values of those cells change.
Binds `self', around `forms', to the object whose slot contains the cell.
`lazy' controls when updates occur; it can be one of `nil', `t' or `:always',
`:once-asked', or `:until-asked':

   `nil': the cell is updated eagerly, as soon as referenced cells change
   `t' / `:always': the cell is updated only when read and referenced
          values have changed
   `:once-asked': like `:always' except that the cell's initial value is
          computed eagerly
   `:until-asked': the cell is not updated until it is first read, at
          which point it becomes eager

`keys' may contain other keyword arguments accepted by `make-c-dependent',
such as `debug'."
  (assert (member lazy '(nil t :once-asked :until-asked :always)))
  `(make-c-dependent
    :code ',forms
    :value-state :unevaluated
    :rule (c-lambda ,@forms)
    ,@keys))

(defmacro c-input ((&rest keys) &optional (value nil valued-p))
  "Makes a settable cell, initialized to `value' if supplied.  (If it's not
supplied, the cell is unbound until set.)  `keys' can contain additional options
to pass to `make-cell', such as `:lazy'."
  `(make-cell
    :inputp t
    :value-state ,(if valued-p :valid :unbound)
    :value ,value
    ,@keys))

(defmacro c-in (value)
  "Makes a settable cell, initialized to `value'."
  `(make-cell
    :inputp t
    :value-state :valid
    :value ,value))

(export! c-in-lazy c_in)

(defmacro c-in-lazy (&body body)
  `(c-input (:lazy :once-asked) (progn ,@body)))

(defmacro c_in (&body body)
  `(c-input (:lazy :once-asked) (progn ,@body)))

(defmacro c-input-dbg (&optional (value nil valued-p))
  `(make-cell
    :inputp t
    :debug t
    :value-state ,(if valued-p :valid :unbound)
    :value ,value))

(defmacro c... ((value) &body body)
  "Makes a drifter cell initialized to `value'.  `body'"
  `(make-c-drifter
    :code ',body
    :value-state :valid
    :value ,value
    :rule (c-lambda ,@body)))

(defmacro c-abs (value &body body)
  `(make-c-drifter-absolute
    :code ',body
    :value-state :valid
    :value ,value
    :rule (c-lambda ,@body)))


(defmacro c-envalue (&body body)
  `(make-c-envaluer
    :envalue-rule (c-lambda ,@body)))

