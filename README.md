# running-krep1-in-browser-using-JSCL

Common Lisp のサブセット実装であるJSCL を使ってブラウザー上で「実用Common Lisp by Peter Norvig」のP.446 14.8節 のDTREE を動かす試み。

# 動作確認済み

- Windows10 Microsoft Edge
- Windows10 Firefox

# DTREE を動かすまで

1. JSCL のWeb サイトにアクセスし、JCSL REPL を起動する

https://jscl-project.github.io/

2. krep1.lisp の全フォームをPROGN にまとめた以下の２つのチャンクを順にREPL にペーストする

- MEMO 本来はLOAD したいが、それがない.
- MEMO 全体を１つのPROGN にまとめたいが "ERROR: Unknown generalized reference." というエラーが出てしまうため、試行錯誤でPROGN を分割した.

```
(progn
  (defconstant fail nil)
  (defconstant no-bindings '((t . t)))
  (defun match-variable (var input bindings) "Does VAR match input?  Uses (or updates) and returns bindings." (let ((binding (get-binding var bindings))) (cond ((not binding) (extend-bindings var input bindings)) ((equal input (binding-val binding)) bindings) (t fail))))
  (defun binding-val (binding) "Get the value part of a single binding." (cdr binding))
  (defun get-binding (var bindings) "Find a (variable . value) pair in a binding list." (assoc var bindings))
  (defun lookup (var bindings) "Get the value part (for var) from a binding list." (binding-val (get-binding var bindings)))
  (defun extend-bindings (var val bindings) "Add a (var . value) pair to a binding list." (cons (cons var val) (if (eq bindings no-bindings) nil bindings)))
  (defun reuse-cons (x y x-y) "Return (cons x y), or reuse x-y if it is equal to (cons x y)" (if (and (eql x (car x-y)) (eql y (cdr x-y))) x-y (cons x y)))
  (defun variable-p (x) "Is x a variable (a symbol beginning with `?')?" (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))
  (defun pat-match (pattern input &optional (bindings no-bindings)) "Match pattern against input in the context of the bindings" (cond ((eq bindings fail) fail) ((variable-p pattern) (match-variable pattern input bindings)) ((eql pattern input) bindings) ((segment-pattern-p pattern) (segment-matcher pattern input bindings)) ((single-pattern-p pattern) (single-matcher pattern input bindings)) ((and (consp pattern) (consp input)) (pat-match (rest pattern) (rest input) (pat-match (first pattern) (first input) bindings))) (t fail)))
  (setf (get '?is  'single-match) 'match-is)
  (setf (get '?or  'single-match) 'match-or)
  (setf (get '?and 'single-match) 'match-and)
  (setf (get '?not 'single-match) 'match-not)
  (setf (get '?*  'segment-match) 'segment-match)
  (setf (get '?+  'segment-match) 'segment-match+)
  (setf (get '??  'segment-match) 'segment-match?)
  (setf (get '?if 'segment-match) 'match-if)
  (defun segment-pattern-p (pattern) "Is this a segment-matching pattern like ((?* var) . pat)?" (and (consp pattern) (consp (first pattern)) (symbolp (first (first pattern))) (segment-match-fn (first (first pattern)))))
  (defun single-pattern-p (pattern) "Is this a single-matching pattern?  E.g. (?is x predicate) (?and . patterns) (?or . patterns)." (and (consp pattern) (single-match-fn (first pattern))))
  (defun segment-matcher (pattern input bindings) "Call the right function for this kind of segment pattern." (funcall (segment-match-fn (first (first pattern))) pattern input bindings))
  (defun single-matcher (pattern input bindings) "Call the right function for this kind of single pattern." (funcall (single-match-fn (first pattern)) (rest pattern) input bindings))
  (defun segment-match-fn (x) "Get the segment-match function for x, if it is a symbol that has one." (when (symbolp x) (get x 'segment-match)))
  (defun single-match-fn (x) "Get the single-match function for x, if it is a symbol that has one." (when (symbolp x) (get x 'single-match)))
  (defun match-is (var-and-pred input bindings) "Succeed and bind var if the input satisfies pred, where var-and-pred is the list (var pred)." (let* ((var (first var-and-pred)) (pred (second var-and-pred)) (new-bindings (pat-match var input bindings))) (if (or (eq new-bindings fail) (not (funcall pred input))) fail new-bindings)))
  (defun match-and (patterns input bindings) "Succeed if all the patterns match the input." (cond ((eq bindings fail) fail) ((null patterns) bindings) (t (match-and (rest patterns) input (pat-match (first patterns) input bindings)))))
  (defun match-or (patterns input bindings) "Succeed if any one of the patterns match the input." (if (null patterns) fail (let ((new-bindings (pat-match (first patterns) input bindings))) (if (eq new-bindings fail) (match-or (rest patterns) input bindings) new-bindings))))
  (defun match-not (patterns input bindings) "Succeed if none of the patterns match the input.  This will never bind any variables." (if (match-or patterns input bindings) fail bindings))
  (defun segment-match (pattern input bindings &optional (start 0)) "Match the segment pattern ((?* var) . pat) against input." (let ((var (second (first pattern))) (pat (rest pattern))) (if (null pat) (match-variable var input bindings) (let ((pos (first-match-pos (first pat) input start))) (if (null pos) fail (let ((b2 (pat-match pat (subseq input pos) (match-variable var (subseq input 0 pos) bindings)))) (if (eq b2 fail) (segment-match pattern input bindings (+ pos 1)) b2)))))))
  (defun first-match-pos (pat1 input start) "Find the first position that pat1 could possibly match input, starting at position start.  If pat1 is non-constant, then just return start." (cond ((and (atom pat1) (not (variable-p pat1))) (position pat1 input :start start :test #'equal)) ((<= start (length input)) start) (t nil)))
  (defun segment-match+ (pattern input bindings) "Match one or more elements of input." (segment-match pattern input bindings 1))
  (defun segment-match? (pattern input bindings) "Match zero or one element of input." (let ((var (second (first pattern))) (pat (rest pattern))) (or (pat-match (cons var pat) input bindings) (pat-match pat input bindings))))
  (defun match-if (pattern input bindings) "Test an arbitrary expression involving variables.  The pattern looks like ((?if code) . rest)." (and (progv (mapcar #'car bindings) (mapcar #'cdr bindings) (eval (second (first pattern)))) (pat-match (rest pattern) input bindings)))
  (defun pat-match-abbrev (symbol expansion) "Define symbol as a macro standing for a pat-match pattern." (setf (get symbol 'expand-pat-match-abbrev) (expand-pat-match-abbrev expansion)))
  (defun expand-pat-match-abbrev (pat) "Expand out all pattern matching abbreviations in pat." (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev))) ((atom pat) pat) (t (cons (expand-pat-match-abbrev (first pat)) (expand-pat-match-abbrev (rest pat))))))
  (defun rule-based-translator (input rules &key (matcher 'pat-match) (rule-if #'first) (rule-then #'rest) (action #'sublis)) "Find the first rule in rules that matches input, and apply the action to that rule." (some #'(lambda (rule) (let ((result (funcall matcher (funcall rule-if rule) input))) (if (not (eq result fail)) (funcall action result (funcall rule-then rule))))) rules))
  (defparameter *occurs-check* t "Should we do the occurs check?")
  (defun unify (x y &optional (bindings no-bindings)) "See if x and y match with given bindings." (cond ((eq bindings fail) fail) ((eql x y) bindings) ((variable-p x) (unify-variable x y bindings)) ((variable-p y) (unify-variable y x bindings)) ((and (consp x) (consp y)) (unify (rest x) (rest y) (unify (first x) (first y) bindings))) (t fail))) 
  (defun unify-variable (var x bindings) "Unify var with x, using (and maybe extending) bindings." (cond ((get-binding var bindings) (unify (lookup var bindings) x bindings)) ((and (variable-p x) (get-binding x bindings)) (unify var (lookup x bindings) bindings)) ((and *occurs-check* (occurs-check var x bindings)) fail) (t (extend-bindings var x bindings))))
  (defun occurs-check (var x bindings) "Does var occur anywhere inside x?" (cond ((eq var x) t) ((and (variable-p x) (get-binding x bindings)) (occurs-check var (lookup x bindings) bindings)) ((consp x) (or (occurs-check var (first x) bindings) (occurs-check var (rest x) bindings))) (t nil)))
  (defun subst-bindings (bindings x) "Substitute the value of variables in bindings into x, taking recursively bound variables into account." (cond ((eq bindings fail) fail) ((eq bindings no-bindings) x) ((and (variable-p x) (get-binding x bindings)) (subst-bindings bindings (lookup x bindings))) ((atom x) x) (t (reuse-cons (subst-bindings bindings (car x)) (subst-bindings bindings (cdr x)) x))))
  (defun unifier (x y) "Return something that unifies with both x and y (or fail)." (subst-bindings (unify x y) x))
  (defun clause-head (clause) (first clause))
  (defun clause-body (clause) (rest clause))
  (defun get-clauses (pred) (get pred 'clauses))
  (defun predicate (relation) (first relation))
  (defun args (x) "The arguments of a relation" (rest x))
  (defvar *db-predicates* nil "a list of all predicates stored in the database.")
  (defmacro <- (&rest clause) "add a clause to the data base." `(add-clause ',(replace-?-vars clause)))
  (defun add-clause (clause) "add a clause to the data base, indexed by head's predicate." (let ((pred (predicate (clause-head clause)))) (pushnew pred *db-predicates*) (setf (get pred 'clauses) (nconc (get-clauses pred) (list clause))) pred))
  (defun clear-db () "remove all clauses (for all predicates) from the data base." (mapc #'clear-predicate *db-predicates*))
  (defun clear-predicate (predicate) "remove the clauses for a single predicate." (setf (get predicate 'clauses) nil))
  (defun rename-variables (x) "replace all variables in x with new ones." (sublis (mapcar #'(lambda (var) (cons var (gensym (string var)))) (variables-in x)) x))
  (defun unique-find-anywhere-if (predicate tree &optional found-so-far) "return a list of leaves of tree satisfying predicate, with duplicates removed." (if (atom tree) (if (funcall predicate tree) (adjoin tree found-so-far) found-so-far) (unique-find-anywhere-if predicate (first tree) (unique-find-anywhere-if predicate (rest tree) found-so-far))))
  (defun find-anywhere-if (predicate tree) "does predicate apply to any atom in the tree?" (if (atom tree) (funcall predicate tree) (or (find-anywhere-if predicate (first tree)) (find-anywhere-if predicate (rest tree)))))
  (defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))
  (defun prove-all (goals bindings) "Find a solution to the conjunction of goals." (cond ((eq bindings fail) fail) ((null goals) bindings) (t (prove (first goals) bindings (rest goals)))))
  (defun prove (goal bindings other-goals) "Return a list of possible solutions to goal." (let ((clauses (get-clauses (predicate goal)))) (if (listp clauses) (some #'(lambda (clause) (let ((new-clause (rename-variables clause))) (prove-all (append (clause-body new-clause) other-goals) (unify goal (clause-head new-clause) bindings)))) clauses) (funcall clauses (rest goal) bindings other-goals))))
  (defun top-level-prove (goals) (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals))) no-bindings) (format t "~&No.") (values))
  (defun show-prolog-vars (vars bindings other-goals) "Print each variable with its binding.  Then ask the user if more solutions are desired." (if (null vars) (format t "~&Yes") (dolist (var vars) (format t "~&~a = ~a" var (subst-bindings bindings var)))) (if (continue-p) fail (prove-all other-goals bindings)))
  (setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)
  (defparameter *continue-p* #\; "#\; を指定すればT, #\. を指定すればNIL")
  (defun continue-p () "Ask user if we should continue looking for solutions." (case *continue-p* (#\; t) (#\. nil) (otherwise nil))) 
  (defun variables-in (exp) "Return a list of all the variables in EXP." (unique-find-anywhere-if #'non-anon-variable-p exp))
  (defun non-anon-variable-p (x) (and (variable-p x) (not (eq x '?))))
  (defun replace-?-vars (exp) "Replace any ? within exp with a var of the form ?123." (cond ((eq exp '?) (gensym "?")) ((atom exp) exp) (t (reuse-cons (replace-?-vars (first exp)) (replace-?-vars (rest exp)) exp))))
  (defun make-empty-nlist () "Create a new, empty nlist." (cons 0 nil))
  (defun nlist-n (x) "The number of elements in an nlist." (car x))
  (defun nlist-list (x) "The elements in an nlist." (cdr x))
  (defun nlist-push (item nlist) "Add a new element to an nlist." (incf (car nlist)) (push item (cdr nlist)) nlist)
  (defstruct (dtree (:type vector)) (first nil) (rest nil) (atoms nil) (var (make-empty-nlist)))
  (defvar *predicates* nil)
  (defun get-dtree (predicate) "Fetch (or make) the dtree for this predicate." (cond ((get predicate 'dtree)) (t (push predicate *predicates*) (setf (get predicate 'dtree) (make-dtree)))))
  (defun clear-dtrees () "Remove all the dtrees for all the predicates." (dolist (predicate *predicates*) (setf (get predicate 'dtree) nil)) (setf *predicates* nil))
  (defun index (key) "Store key in a dtree node.  Key must be (predicate . args); it is stored in the predicate's dtree." (dtree-index key key (get-dtree (predicate key))))
)
```

ペーストしたあと、追加のエンターを押すのを忘れないこと. それで初めて評価される.

```
(progn
  (defun dtree-index (key value dtree) "Index value under all atoms of key in dtree." (cond ((consp key) (dtree-index (first key) value (or (dtree-first dtree) (setf (dtree-first dtree) (make-dtree)))) (dtree-index (rest key) value (or (dtree-rest dtree) (setf (dtree-rest dtree) (make-dtree))))) ((null key)) ((variable-p key) (nlist-push value (dtree-var dtree))) (t (nlist-push value (lookup-atom key dtree)))))
  (defun lookup-atom (atom dtree) "Return (or create) the nlist for this atom in dtree." (or (lookup atom (dtree-atoms dtree)) (let ((new (make-empty-nlist))) (push (cons atom new) (dtree-atoms dtree)) new)))
  (defun test-index () (let ((props '((p a b) (p a c) (p a ?x) (p b c) (p b (f c)) (p a (f . ?x))))) (clear-dtrees) (mapc #'index props) (write (list props (get-dtree 'p))) (values)))
  (defun fetch (query) "Return a list of buckets potentially matching the query, which must be a relation of form (predicate . args)." (dtree-fetch query (get-dtree (predicate query)) nil 0 nil most-positive-fixnum))
  (defun dtree-fetch (pat dtree var-list-in var-n-in best-list best-n) "Return two values: a list-of-lists of possible matches to pat, and the number of elements in the list-of-lists." (if (or (null dtree) (null pat) (variable-p pat)) (values best-list best-n) (let* ((var-nlist (dtree-var dtree)) (var-n (+ var-n-in (nlist-n var-nlist))) (var-list (if (null (nlist-list var-nlist)) var-list-in (cons (nlist-list var-nlist) var-list-in)))) (cond ((>= var-n best-n) (values best-list best-n)) ((atom pat) (dtree-atom-fetch pat dtree var-list var-n best-list best-n)) (t (multiple-value-bind (list1 n1) (dtree-fetch (first pat) (dtree-first dtree) var-list var-n best-list best-n) (dtree-fetch (rest pat) (dtree-rest dtree) var-list var-n list1 n1)))))))
  (defun dtree-atom-fetch (atom dtree var-list var-n best-list best-n) "Return the answers indexed at this atom (along with the vars), or return the previous best answer, if it is better." (let ((atom-nlist (lookup atom (dtree-atoms dtree)))) (cond ((or (null atom-nlist) (null (nlist-list atom-nlist))) (values var-list var-n)) ((and atom-nlist (< (incf var-n (nlist-n atom-nlist)) best-n)) (values (cons (nlist-list atom-nlist) var-list) var-n)) (t (values best-list best-n)))))
  (proclaim '(inline mapc-retrieve))
  (defun mapc-retrieve (fn query) "For every fact that matches the query, apply the function to the binding list." (dolist (bucket (fetch query)) (dolist (answer bucket) (let ((bindings (unify query answer))) (unless (eq bindings fail) (funcall fn bindings))))))
  (defun retrieve (query) "Find all facts that match query.  Return a list of bindings." (let ((answers nil)) (mapc-retrieve #'(lambda (bindings) (push bindings answers)) query) answers)) 
  (defun retrieve-matches (query) "Find all facts that match query.  Return a list of expressions that match the query." (mapcar #'(lambda (bindings) (subst-bindings bindings query)) (retrieve query)))
  (defmacro query-bind (variables query &body body) "Execute the body for each match to the query.  Within the body, bind each variable." (let* ((bindings (gensym "BINDINGS")) (vars-and-vals (mapcar #'(lambda (var) (list var `(subst-bindings ,bindings ',var))) variables))) `(mapc-retrieve #'(lambda (,bindings) (let ,vars-and-vals ,@body)) ,query)))
)
```

3. REPL に以下のフォームを貼り付けて動くことを確認

PAIP P.454 より
```
(test-index) ; 結果はPPRINT されず、横一列に表示されて横スクロールを余儀なくされる。その最後に CL-USER> のプロンプトが表示されるので、処理が終了したことに気づかないことがあるので注意
(fetch '(p ?x c))
(retrieve '(p ?x c))
(retrieve-matches '(p ?x c))
(retrieve-matches '(p ?x (?fn c)))
(query-bind (?x ?fn) '(p ?x (?fn c)) (format t "~&P holds between ~a and ~a of c." ?x ?fn))
```

(PAIP P.458 からはkrep2.lisp の話となり、動作確認対象外とした。)

# 背景説明

以下、JSCL の環境を使ってDTREE を動作させるまでに分かったことをまとめる。

## DTREE のソース構成

DTREE はkrep1.lisp に実装されているが、以下の関連ファイルを順に読み込んでいる.

1. auxfns.lisp ... https://github.com/norvig/paip-lisp/blob/main/lisp/auxfns.lisp
	のうち、patmatch に関わる部分のみ
	```
	(defconstant fail nil)
	(defconstant no-bindings '((t . t)))
	(defun match-variable (var input bindings) "Does VAR match input?  Uses (or updates) and returns bindings." (let ((binding (get-binding var bindings))) (cond ((not binding) (extend-bindings var input bindings)) ((equal input (binding-val binding)) bindings) (t fail))))
	(defun binding-val (binding) "Get the value part of a single binding." (cdr binding))
	(defun get-binding (var bindings) "Find a (variable . value) pair in a binding list." (assoc var bindings))
	(defun lookup (var bindings) "Get the value part (for var) from a binding list." (binding-val (get-binding var bindings)))
	(defun extend-bindings (var val bindings) "Add a (var . value) pair to a binding list." (cons (cons var val) (if (eq bindings no-bindings) nil bindings)))
	(defun reuse-cons (x y x-y) "Return (cons x y), or reuse x-y if it is equal to (cons x y)" (if (and (eql x (car x-y)) (eql y (cdr x-y))) x-y (cons x y)))
	```
2. patmatch.lisp ... https://github.com/norvig/paip-lisp/blob/main/lisp/patmatch.lisp
3. unify.lisp ... https://github.com/norvig/paip-lisp/blob/main/lisp/unify.lisp
4. prolog.lisp ... https://github.com/norvig/paip-lisp/blob/main/lisp/prolog.lisp
5. krep1.lisp ... https://github.com/norvig/paip-lisp/blob/main/lisp/krep1.lisp

これらのファイルを１つにまとめたのが src/krep1.lisp である.
更にREPL にペーストしやすいように各フォームをコメントを削って１行にまとめたのが src/krep1-shorten.lisp である.

## JSCL のREPL の特徴

- load が無いようである. (load "krep1.lisp") のようにはできない.
- 改行を扱うことができる. 次のフォームは認識できる.
	```
	CL-USER> (defun fact (n) 
	...        ;; コメントも書ける 
	...        "Document も書ける" 
	... 	   (if (zerop n) 1 
	...            (* n (fact (1- n))))) 
	... 
	FACT

	CL-USER> (fact 100) ; ついでにBIGINT の確認
	93326215443944080860486828628220646840004426888400626848604808684682244840464022862608208886284486804200400484620848004862686866402802860622640446000204666408

	CL-USER> (fact 1000)
	Infinity

	CL-USER> (fact 10000)
	ERROR: Maximum call stack size exceeded

	CL-USER> (describe 'fact) ; ついでにDESCRIBE の確認
	FACT
	Class: #<builtin-in-class SYMBOL>
	:INTERNAL in package CL-USER
	Print name: "FACT"
	FACT names a function
	#<FUNCTION FACT>
	Class: #<builtin-in-class FUNCTION>
	Name:FACT
	Documentation: Document も書ける
	```
- エラーメッセージが出る
	```
	CL-USER> hello
	ERROR: Variable HELLO is unbound. ; Deno 用のREPL では "ERROR: ERROR[!]: Function '!CONDITION-ARGS' undefined" となり、まともなエラーメッセージを見ることができなかったのが改善されている.
	```
- １つ前は、デバッガが無いことも表している
- PPRINT がない
- 複数のフォームを入力した時、最初のフォームしか評価されない。PROGN を使う必要がある。
	```
	CL-USER> 1 2
	1
	```
	```
	CL-USER> (progn 1 2)
	2
	```

## JSCL の制限

DTREE を作動させるために、以下の部分を変更しているため、要注意.

- *standard-input* がない. あるのは*standard-output* のみ.
	```
	CL-USER> *standard-input*
	ERROR: Variable *STANDARD-INPUT* is unbound.
	CL-USER> *standard-output*
	#<structure stream>				<---- これはある
	```
	そのためProlog の一部であるCONTINUE-P の中の READ-CHAR が使えず、書き換えている.
	```
	;;; original
	(defun continue-p ()
	  "Ask user if we should continue looking for solutions."
	  (case (read-char)
		(#\; t)
		(#\. nil)
		(#\newline (continue-p))
		(otherwise
		  (format t " Type ; to see more or . to stop")
		  (continue-p))))

		↓

	;;; CONTINUE-P はこの大域変数の値で振る舞いを決める
	;;;     #\; を指定すればT, #\. を指定すればNIL
	(defparameter *continue-p* #\;)

	(defun continue-p ()
	  "Ask user if we should continue looking for solutions."
	  (case *continue-p*
		(#\; t)
		(#\. nil)
		(otherwise nil)))
	```
	従って質問にはインタラクティブな振る舞いはできず、 ; ですぐに止めるか、. で止まるまで続けるかのいずれかしか選べない. 
	```
	CL-USER> (?- (loves Marie_Curie Pierre_Curie))

	Yes
	No.
	```
- write がキーワード引数をサポートしていない
	```
    (write '(1)) => (1) (1) ; 多値
    (write '(1) :circle t) => ERROR
    (write '(1) :array t) => ERROR
    (write '(1) :pretty t) => ERROR
    -> write のキーワード引数がだめだ
	```
	```
	(defun test-index ()
	  (let ((props '((p a b) (p a c) (p a ?x) (p b c)
					 (p b (f c)) (p a (f . ?x)))))
		(clear-dtrees)
		(mapc #'index props)
		(write (list props (get-dtree 'p))
			   :circle t :array t :pretty t)
		(values)))

	↓ write の部分を変更

	(defun test-index ()
	  (let ((props '((p a b) (p a c) (p a ?x) (p b c)
					 (p b (f c)) (p a (f . ?x)))))
		(clear-dtrees)
		(mapc #'index props)
		(write (list props (get-dtree 'p)))
		(values)))
	```
- 以下のコード(PAIP P.457-458) は動作するがゆえに無限ループに入り、止める手段がなくブラウザが固まってしまう
	```
	(<- (natural 0))
	(<- (natural (1+ ?n)) (natural ?n))
	(?- (natural ?n))
	?N = (1+ (1+ (1+ ))) ... これはWeb ブラウザ版のREPL では実際には表示されることはない.
							Deno/Node.js 版のREPL では出力を見ることができ、Ctrl-c で強制終了することも可能.
	```

以下はDTREE には関係がないが、気づいたこと.

- 分数がない 
	```
	CL-USER> (/ 10 3)
	3.3333333333333335
	```
- パッケージ構成
	```
	CL-USER> (list-all-packages)
	(#<PACKAGE JSCL/LOOP> #<PACKAGE CL-USER> #<PACKAGE KEYWORD> #<PACKAGE CL> #<PACKAGE JSCL>)
	```
- clos はclosette 実装. ただ結構制約もある.
	https://github.com/jscl-project/jscl/blob/master/src/clos/README.md

## その他、メモ

- JSCL のコンパイラはホスト言語がなんで有ろうと(SBCL だろうがJavaScript だろうが) "JSCL 言語" だけ.
	- SBCLで書いた関数を JSCL でコンパイルできるわけではない
- JSCL 言語でできることはsrc フォルダやtest フォルダが参考になる
	- https://github.com/jscl-project/jscl/tree/master/src
	- https://github.com/jscl-project/jscl/tree/master/tests

