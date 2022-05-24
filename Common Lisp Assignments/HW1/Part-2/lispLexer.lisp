(defun run_Program()
		
		(if (equal (car *args*) nil) 
			(interpretation)
			(interpretation (car *args*))
		)
	
)

(defun lexer(word_Lex output)
	(setf word_Lex (string-downcase word_Lex))
	(let ((x 0) (arr (make-array 2 :element-type 'character :adjustable t :fill-pointer 0)))
		(loop for c across word_Lex
			do 
			(if (equal 1 (list_Check c op)) 
				(progn
					(if (> (length arr) 0) (kw_ID arr output))
					(setf (fill-pointer arr) 0)
					(if (equal c #\*) 
						(if (and (< x (- (length word_Lex) 1)) (equal (char word_Lex (+ x 1)) #\*)) (op_Check (string c) #\* output) (op_Check (string c) nil output)) 	
						(op_Check (string c) nil output))
				)
				(if (or (and (< (char-code c) 58) (> (char-code c) 47)) (and (< (char-code c) 123) (> (char-code c) 96)) (equal (char-code c) 95))
					(vector-push-extend c arr)
					(progn
						(if (> (length arr) 0) 
							(progn (kw_ID arr output) (setf (fill-pointer arr) 0))
						)
						(error_Check c output)
					)
				)
			)
			(incf x)
		)
		(if (> (length arr) 0) (kw_ID arr output))
	)
)

(defun interpretation(&rest name_Of_File)
	(with-open-file (output "lisp_Parser.txt" :direction :output :if-exists :supersede)
		(if (equal name_Of_File nil) (translate output) (my_File name_Of_File output))
	)
)

(defun translate(output)
		(setq str_Read nil)
		(loop while(not (equal str_Read ""))
			do  (format t "Enter an Input : ")
				(setq str_Read (read-line))
				(setq check_Comm (search ";;" str_Read))
				(if (not (equal check_Comm nil)) (setf str_Read (subseq str_Read 0 (+ check_Comm 2))))
	      		(setf last_Token nil)
				(seperate_Word str_Read output)
		)
)

(defun kw_Check (elm)
	(dolist (l kw)
		(if (equal elm l) (return-from kw_Check t))
	)
	(not t)
)

(defun kw_ID(str output)
	(if (kw_Check str) 
		(format output "KW_~A~%" (string-upcase str))
		(progn 
			(let ((val 0))
				(loop for c across str
					do
					(if (not (digit-char-p c)) (setf val 1))	
				)
				(if (equal val 1) (progn (if (digit-char-p (char str 0)) (format output "ERROR ~A cannot be tokenized~%" str) (format output "IDENTIFIER~%")) ) (progn (if (and (> (length str) 1) (equal (char str 0) #\0))(format output "ERROR ~A cannot be tokenized~%" str) (format output "VALUE~%")) ) )	
			)
		)
	)
)

(defun seperate_Word(words output)
	(let ((string-array (make-array 2 :element-type 'character :adjustable t :fill-pointer 0)) (comm 0))
		(loop for c across words
			do
			(if (and (equal c #\Newline) (equal comm 1)) (setf comm 0))
			(if (and (not (equal c #\Space)) (not (equal c #\Newline)) (not (equal c #\tab)) (not (equal (comment_Check string-array) 1)))  
				(if (equal comm 0) (vector-push-extend c string-array)) 
				(if (equal comm 0)
					(progn 
					(if (equal (comment_Check string-array) 1) (progn (if (> (length string-array) 2) (lexer (subseq string-array 0 (- (length string-array) 2)) output)) (format output "COMMENT~%") (progn (setf comm 1) (setf (fill-pointer string-array) 0))))
					(progn (if (> (length string-array) 0) (progn (lexer string-array output) (setf (fill-pointer string-array) 0)))) 
				))
			)
		)
		
		(if (equal comm 0) 
			(progn 
			(if (equal (comment_Check string-array) 1) (progn (if (> (length string-array) 2) (lexer (subseq string-array 0 (- (length string-array) 2)) output)) (format output "COMMENT~%") (return-from seperate_Word 1)))
			(if (> (length string-array) 0) (progn (lexer string-array output) (setf (fill-pointer string-array) 0)))
		))
	)
)

(defun my_File(name_Of_File output)
	
	(with-open-file (stream (car name_Of_File))
	    (let ((contents (make-string (file-length stream))))
	      (read-sequence contents stream)
	      contents
	      	(setf last_Token nil)
	    	(seperate_Word contents output)
	    )
    )
)

(defun error_Check(c output)
	(format output "ERROR ~A cannot be tokenized~%" c)
)

(defun list_Check(elm my-list)
	(dolist (l my-list)
		(if (equal (string elm) l) (return-from list_Check 1))
	)
	0
)

(defun op_Check(c next output)
	(if (equal next #\*) (progn (format output "~A~%" "OP_DBLMULT") (incf check_Star) (return-from op_Check 1)))

	(cond
		((equal c "(") (format output "~A~%" "OP_OP"))
        ((equal c ")") (format output "~A~%" "OP_CP"))
        ((equal c "/") (format output "~A~%" "OP_DIV"))
        ((equal c "*") (if (equal check_Star 1) (decf check_Star) (format output "~A~%" "OP_MULT")))		
		((equal c "+") (format output "~A~%" "OP_PLUS"))
        ((equal c "-") (format output "~A~%" "OP_MINUS"))


        ((equal c "\"") (if (equal (mod quote_Mark 2) 0) (progn (format output "~A~%" "OP_OC") (incf quote_Mark)) (progn (format output "~A~%" "OP_CC") (incf quote_Mark)))) 
        ((equal c ",") (format output "~A~%" "OP_COMMA"))
        (t (format output "~A~%" "ERROR")))
)

(defun comment_Check (arr_Comm)
	(if (and (> (length arr_Comm) 1) (equal (char arr_Comm (- (length arr_Comm) 1)) #\;) (equal (char arr_Comm (- (length arr_Comm) 2)) #\;)) (return-from comment_Check 1) (return-from comment_Check 0))
)

(setq last_Token nil)
(setq check_Star 0)
(setq quote_Mark 0)
(setq op (list "+" "-" "/" "*" "(" ")" "**" "\"" ","))
(setq kw (list "and" "or" "not" "equal" "less" "nil" "list"
	"append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))


(run_Program)