(defun cria_classificador (nome path)
	(let ((line nil) (cont 0) (parametros) (regras) (funcao_retorno) (expressao))
	(with-open-file (stream path)
		
			(loop 
				(setq line (read-line stream nil 'eof))
				(when (eq line 'eof) (return))
				(when (eq cont 0)
					(setq parametros (read-from-string (format nil "(~A)" line) nil 'eol))
				)
				(when (> cont 0)
					(setq regras (cons (read-from-string (format nil "(~A)" line) nil 'eol) regras))
					
				)
				(incf cont)
			)
			(setq regras (reverse regras))
			(dolist (regra regras)
				(setq expressao  (fomatar_classificador regra))
				; Cada regra sera transformada numa expressao lisp
				(if (eq (cdr expressao) nil)
					(setq funcao_retorno (cons (list 't (car expressao)) funcao_retorno))
					(setq funcao_retorno (cons (list (formatar_expressao (cdr expressao)) (car expressao)) funcao_retorno))
				)
			)
			(setq funcao_retorno (cons 'cond (reverse funcao_retorno)))
			
		)

		(setq parametros (separar_args parametros))

		 (eval (list 'defun nome (car parametros) funcao_retorno (cadr parametros)))
		 
	)
)

(defun separar_args (argumentos)
	(let ((params) (retorno))
		(loop
			(unless argumentos (return t))
			(if (eq (cdr argumentos) nil)
				(setq retorno (car argumentos))
				(setq params (cons (car argumentos) params))
			)
			(setq argumentos (cdr argumentos))
		)
		;(format t "~a ~%" params)
		(list (reverse params) retorno) 
	)
)

; Define condicoes e conclusoes
(defun fomatar_classificador (regra)
	(let ((condicoes) (conclusao) (regra_aux regra) (expressao))
		(loop
			(unless regra_aux (return t))
			; converte o elemento pra string
			; e testa elementos a esquerda da seta
			(if (not (equal (write-to-string (car regra_aux)) "=>"))
				(setq condicoes (cons (car regra_aux) condicoes))
				(progn 
					(setq conclusao (list 'setq (cadr regra_aux) (format nil "~a" (car (cdddr regra_aux)))))
					(setq regra_aux nil)
				)
			)
			(setq regra_aux (cdr regra_aux))
		)

		(setq expressao (cons conclusao (reverse condicoes)))

	)
)

(defun formatar_condicao (regra)
	(let ((expressao_lisp))
		(setq expressao_lisp (list (cadr regra) (car regra) (caddr regra) ))
		expressao_lisp
	)
)

(defun formatar_expressao (regras)
	(let ((expressao) (condicoes) (simbolos) (cont 0))
		(loop 
			(unless regras (return t))
				(if (listp (car regras))
					(setq condicoes (cons (formatar_condicao (car regras)) condicoes))
					(setq simbolos (cons (car regras) simbolos))
				)
				(setq regras (cdr regras))
		)
		(setq condicoes (reverse condicoes))
		(setq simbolos (reverse simbolos))
		(loop
			(unless simbolos (return t))
			(if (eq cont 0)

				(setq expressao (list (car simbolos) (car condicoes) (cadr condicoes)))
				(progn (setq expressao (list (car simbolos) expressao (car condicoes))) )  
			)
			(incf cont)
			(if (eq (length (cdr condicoes)) 1)
				(setq condicoes (cdr condicoes))
				(setq condicoes (cddr condicoes))
			)
			(setq simbolos (cdr simbolos))
		)
		expressao
	)
)

(defun avaliar_classificador_csv (path funcao)
	(let ((line nil) (l1))
		(with-open-file (stream path)
			(with-open-file (str "saida.csv" :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
				(loop 
					(setq line (read-line stream nil 'eof))
					(when (eq line 'eof) (return))
					(setq l1 (read-from-string (format nil "(~A)" (substitute #\SPACE #\, line)) nil 'eol))
					(format str "~a~a~%" (format nil "~{~A, ~}" l1) (apply funcao l1))
				)
			)
		)
	)
	t
)
