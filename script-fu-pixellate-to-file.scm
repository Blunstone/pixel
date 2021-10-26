  (script-fu-register
            "script-fu-pixellate-to-file"                ;func name
            "Pixellate to file"                       ;menu label
            "Converts an image to cmyk pixels.
	    \Saves the result as a .raw file"         ;description
            "Neil McAree"                             ;author
            "copyright 2019, Neil McAree"        ;copyright notice
            "14th Januaary 2020"                          ;date created
            "RGB*"                     ;image type that the script works on
	SF-IMAGE	"image"		0	;currently open image
	SF-DRAWABLE	"drawable"	0	;currently open drawable
        SF-ADJUSTMENT  "resolution"     '(0.350 0.200 0.450 0.001 0.05 3 0)
  )
  (script-fu-menu-register "script-fu-pixellate-to-file" "<Image>/Image/Pixellate to file")
(define (script-fu-pixellate-to-file image drawable resolution)
	   (let* ( (err 0) (rblock 0) (width 0) (raw-file "") )

(define (my-remainder m d) (if (< m d)  m (my-remainder (- m d) d)))
(define (my-quotient i k) (if (= k 0) ("error") (/ (- i (my-remainder i k)) k)))
		
(define (placing 9-cols rb cb)
	(let* ( (place-data '()) (row-co-ord 0) (col-co-ord 0))

	(define (each-pixel x col-list)
		
		(define (body)
		(set! col-list (cdr col-list))
		(each-pixel (+ x 1) col-list)

		  (set! row-co-ord (my-quotient x 3))
		  (set! col-co-ord (my-remainder x 3))
		  (gimp-image-select-rectangle image CHANNEL-OP-REPLACE (+ rb row-co-ord) (+ cb col-co-ord) 1 1)
		  (gimp-context-set-foreground (car col-list))
		  (gimp-edit-fill drawable FILL-FOREGROUND)
		)
		(if (< (my-quotient x 3) 3) (body))
	)
(set! place-data (cons "dummy" 9-cols))

(each-pixel 0 place-data)

	)
)

(define (rgb-cmyk pixel)

(define (my-round n)
	(if (and (integer? (- n 0.5)) (not (= (round n) (round (+ n 1))))) (round (+ n 0.1)) (round n)))

(define (round-up p) (if (= p (truncate p)) p (+ (truncate p) 1)))


(define rgb-k
	(let* ((cyan 0) (magenta 0) (yellow 0))

		(set! cyan (my-round (* (/ (car pixel) 255) 9)))

		(set! magenta (my-round (* (/ (cadr pixel) 255) 9)))

		(set! yellow (my-round (* (/ (caddr pixel) 255) 9)))
			(if (and (= cyan magenta) (= cyan yellow))
			
			(my-round (+ 9 (* (- 0 1) cyan)))
 			(my-round (* 9 (- 1 (/ (max (car pixel) (cadr pixel) (caddr pixel)) 255))))
			)
	)
)
(define (rgb-c pixel)
	(let* ((cyan 0) (magenta 0) (yellow 0) (colsum 0) (black 0))
		
		(set! black (- 1 (/ (max (car pixel) (cadr pixel) (caddr pixel)) 255)))
		(set! cyan (/ (- (- 1 black) (/ (car pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! magenta (/ (- (- 1 black) (/ (cadr pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! yellow (/ (- (- 1 black) (/ (caddr pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! colsum (+ cyan magenta yellow))

			(if (and (= cyan magenta) (= cyan yellow)) 
			(my-round (round-up (/ (* (/ (car pixel) 255) 9) 3))) ;Slightly overcomplicated 
;to allow ease of adaption to pixillation for 4x4 etc
			(my-round (/ (* cyan (- 9 (round (* 9 black)))) colsum))
			)
	)
)
(define (rgb-m pixel)
	(let* ((cyan 0) (magenta 0) (yellow 0) (colsum 0) (black 0))
	
		(set! black (- 1 (/ (max (car pixel) (cadr pixel) (caddr pixel)) 255)))
		(set! cyan (/ (- (- 1 black) (/ (car pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! magenta (/ (- (- 1 black) (/ (cadr pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! yellow (/ (- (- 1 black) (/ (caddr pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! colsum (+ cyan magenta yellow))

			(if (and (= cyan magenta) (= cyan yellow))  
			(my-round (round-up (/ (* (/ (cadr pixel) 255) 9) 3))) ;Slightly overcomplicated 
;to allow ease of adaption to pixillation for 4x4 etc
			(my-round (/ (* magenta (- 9 (round (* 9 black)))) colsum))
			)
	
	)
)
(define (rgb-y pixel)
	(let* ((cyan 0) (magenta 0) (yellow 0) (colsum 0) (black 0))
	
		(set! black (- 1 (/ (max (car pixel) (cadr pixel) (caddr pixel)) 255)))
		(set! cyan (/ (- (- 1 black) (/ (car pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! magenta (/ (- (- 1 black) (/ (cadr pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! yellow (/ (- (- 1 black) (/ (caddr pixel) 255)) (- 1 (if (= black 1) 0 black))))
		(set! colsum (+ cyan magenta yellow))

			(if (and (= cyan magenta) (= cyan yellow))  
			(my-round (round-up (/ (* (/ (caddr pixel) 255) 9) 3))) ;Slightly overcomplicated 
;to allow ease of adaption to pixillation for 4x4 etc
			(my-round (/ (* yellow (- 9 (round (* 9 black)))) colsum))
			)	
	)
)

(list (rgb-c pixel) (rgb-m pixel) (rgb-y pixel) rgb-k)
)

(define (arrange-pixels cols)
	(let* ( (9-colours '() ) (jumbled '() ) 
		(c (car cols)) (m (cadr cols)) (y (caddr cols)) (k (cadddr cols)) (not-9 0))

	(define (add-col n color)
		(if (> n 1) (add-col (- n 1) color) )
	(set! 9-colours (cons color 9-colours))
	)

	(define (reduce-to-9)
	(set! not-9 (random (+ c m y k)) )
		(cond ((< not-9 c) (set! c (- c 1)) )
			((< not-9 (+ c y)) (set! y (- y 1)) )
			((< not-9 (+ c y m)) (set! m (- m 1)) )
			(else (set! k (- k 1)) )
		)
	(if (< 9 (+ c m y k))  (reduce-to-9) )
	)

	(define (raise-to-9)
	(set! not-9 (random 4))
		(cond ((= not-9 1) (set! c (+ c 1)) )
			((= not-9 2) (set! y (+ y 1)) )
			((= not-9 3) (set! m (+ m 1)) )
			(else (set! k (+ k 1)) )
		)
	(if (> 9 (+ c m y k))  (raise-to-9) )
	)

(define (jumble 9-list)
(let* ((chosen 0) (jumb '() ))

	(set! chosen (- (random (length 9-list)) 1)) 
	(set! jumb (cons (list-ref 9-list chosen) jumb))
	(set! jumbled (cons (car jumb) jumbled))
	
	(set! 9-list (append 
		(if (list? (list-tail 9-list (+ chosen 1)) ) (list-tail 9-list (+ chosen 1)) )
		(if (list? (list-tail (reverse 9-list) (- (length 9-list) chosen)) ) 
		(list-tail (reverse 9-list) (- (length 9-list) chosen)) )
		     );need to simplify
	) 
	(if (< 0 (length 9-list)) (jumble 9-list) )

)
)

(if (< 9 (+ c m y k))  (reduce-to-9) ) ;(if (= 0 0) (after))
(if (> 9 (+ c m y k))  (raise-to-9) )
(if (> k 0) (add-col k '(0 0 0)))
(if (> y 0) (add-col y '(255 255 0)))
(if (> m 0) (add-col m '(255 0 255)))
(if (> c 0) (add-col c '(0 255 255)))

(jumble 9-colours)
jumbled
	)
)
	
		(define (do-loop cblock)	
		
			(define (body-for-each-3x3)
				(gimp-context-set-foreground (car (gimp-image-pick-color image drawable rblock cblock TRUE FALSE 0)))
				(placing (arrange-pixels (rgb-cmyk (car (gimp-context-get-foreground)))) rblock cblock)
				(do-loop cblock)
			)
;need to add error check for image dimensions not divisible by 3
						
			(set! err (+ err 1)) (if (= err 1000000000) ((error)))
			(set! cblock (+ cblock 3))			
			(set! rblock (+ rblock (* 3 (quotient cblock (car (gimp-drawable-height drawable))))))
			(set! cblock (remainder cblock (car (gimp-drawable-height drawable))))
			(if (< rblock width) (body-for-each-3x3))
		)

     ;(gimp-image-flip image ORIENTATION-HORIZONTAL)
     (if (< (car (gimp-image-width image)) (car (gimp-image-height image))) (gimp-image-rotate image ROTATE-90))
     (gimp-context-set-interpolation INTERPOLATION-CUBIC)
     (if (> (/ (car (gimp-image-height image)) (car (gimp-image-width image))) 0.84)
         (gimp-image-scale image (truncate (* (car (gimp-image-width image)) (/ (/ 70 resolution) (car (gimp-image-height image)))))
                                 (truncate (/ 70 resolution)))
         (gimp-image-scale image (truncate (/ (/ 250 3) resolution))
                                 (truncate (* (car (gimp-image-height image)) (/ (/ (/ 250 3) resolution) (car (gimp-image-width image))))))
     )
     (gimp-context-set-interpolation INTERPOLATION-NONE)
     (gimp-image-scale image (* 3 (car (gimp-image-width image))) (* 3 (car (gimp-image-height image))))
     (set! width (car (gimp-drawable-width drawable)))
	
	(do-loop -3)

     (set! raw-file (string-append (substring (car (gimp-image-get-filename image)) 0 (- (string-length (car (gimp-image-get-filename image))) 3)) "raw"))     
     (file-raw-save2 RUN-NONINTERACTIVE image drawable  raw-file "" 0 0)

	)	
	(gimp-displays-flush)

)
