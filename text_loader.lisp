
; +------------------------------------------------+
; | Title: Common Lisp Utilities                   |
; | Author: la-ci (laci.kosco@gmail.com            |
; | (C) 2016                                       |
; +------------------------------------------------+

; Text file encoding detection
;
; FE FF 		is UTF-16 Big-Endian 
; FF FE 		is UTF-16 Little-Endian 
; 00 00 FE FF 	is UTF-32 Big-Endian 
; FF FE 00 00 	is UTF-32 Little-Endian 
; EF BB BF 		is UTF-8

(defun detect-encoding(path)
"tries to detect UTF or acii encoding of text file to read"
  (if (not (probe-file path)) NIL
    (with-open-file (stream path :element-type '(unsigned-byte 8))
     (let* (
  		   (bom1 (read-byte stream nil 'EOF))
  		   (bom2 (read-byte stream nil 'EOF))
  		   (bom3 (read-byte stream nil 'EOF))
  		   (bom4 (read-byte stream nil 'EOF)))
  	  (cond ((and (= bom1 #xFE) (= bom2 #xFF)) :UTF-16BE)
  	  	    ((and (= bom1 #xFF) (= bom2 #xFE) (= bom3 #x00) (= bom4 #x00)) :UTF-32BE)
  	  	    ((and (= bom1 #x00) (= bom2 #x00) (= bom3 #xFE) (= bom4 #xFF)) :UTF-32LE)
  	  	    ((and (= bom1 #xFF) (= bom2 #xFE)) :UTF-16LE)
  	  	    ((and (= bom1 #xEF) (= bom2 #xBB) (= bom3 #xBF)) :UTF-8)
  	  	    (T NIL))))))

; load text file detecting correct encoding
; tested in CCL
(defun file-string (path)
  "Loads given filename and returns it as string - does silently input encoding detection"
  (with-open-file (stream path :external-format (detect-encoding path))
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
