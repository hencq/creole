(ns creole.core
  (:require [clojure.set]
            [clojure.string :as string]))

;; Some helper functions to profile. By sprinkling functions with
;; inc-counter it's easy to see where a parser spends most of its time
(def counter (atom {}))

(defn inc-counter [src]
  (swap! counter
         (fn [c]
           (assoc c src (inc (c src 0))))))

(defn reset-counter []
  (swap! counter (fn [_] {})))

(defmacro instrument [form]
  `(do
     (reset-counter)
      ~form))
(defprotocol Parser
  (parse [p input] "Parse an input"))

;; Clojure functions can be parsers. Parse just calls the function.
(extend-type clojure.lang.AFn
  Parser
  (parse [p input]
    (p input)))

(defn- split-lines [txt]
  (mapv first
        (drop-last 
         (re-seq #"[^\r\n]*(\z|\r?\n)" txt))))

(defn init-state
  "Takes a string and produces an initial parser state"
  [txt]
  {:lines (map first (drop-last (re-seq #"[^\r\n]*(\z|\r?\n)" txt)))
   :pos [0 0]})

(defn advance-state
  "Moves the cursor in the state forward by n characters"
  [state n]
  (loop [[lnum col :as pos] (:pos state)
         n n]
    (if (> n 0)
      (if (>= lnum (count (:lines state)))
        (assoc state :pos ::eof)
        (let [len (count (nth (:lines state) lnum))
              rpad (- len col)]
          (if (>= n rpad)
            (recur [(inc lnum) 0] (- n rpad))
            (recur [lnum (+ col n)] 0))))
      (assoc state :pos pos))))


(defn- update-state
  "Take a state and a result and advance the cursor on success"
  [state result]
  (if (= ::success (:tag result))
    (advance-state state (:len result))
    result))

(defn substring
  "Return the string at a given position in a multiline input"
  ([state len]
   (let [[startline col] (:pos state)
         endpos (:pos (advance-state state len))]
     (if (= endpos ::eof)
       ::eof
       (subs 
        (string/join "" (take (inc (first endpos)) (drop startline (:lines state))))
        col
        (+ col len))))))

(defn success [pos len raw result]
  {:tag ::success
   :pos pos
   :len len
   :raw raw
   :val result})

(defn error [pos expected actual]
  {:tag ::fail
   :pos pos
   :expected expected
   :actual actual})

(defn success? [res]
  (= ::success (:tag res)))

(defn str-matcher
  "Returns a matcher that tries to match a string"
  [cand]
  (fn [input]
    (inc-counter cand)
    (let [len (count cand)
          s (substring input len)]
      (if (= s cand)
        (success (:pos input) len s s)
        (error (:pos input) cand s)))))

;; Strings participate in the Parser protocol
(extend-type java.lang.String
  Parser
  (parse [p input]
    ((str-matcher p) input)))

(defn choice
  "Takes a seq of matchers and returns a matcher that tries all matchers in turn"
  [& matchers]
  (fn [input]
    (inc-counter :choice)
    (let [tried (map #(parse % input) matchers)
          matched (seq (drop-while #(= ::fail (:tag %)) tried))]
      (if matched
        (first matched)
        (let [error (first tried)
              expected (reduce
                        (fn [s e]
                          (if (set? e)
                            (clojure.set/union s e)
                            (conj s e)))
                        #{}
                        (map #(:expected %) tried))]
          (assoc error :expected expected))))))


(defn chain
  "Takes a seq of parsers and returns a parser that tries to match all of them in order"
  [& matchers]
  (fn [input]
    (inc-counter :chain)
    (let [result (reduce
                  (fn [matched parser]
                    (let [result (parse parser
                                        (update-state input matched))]
                      (if (= ::success (:tag result))
                        (let [matched (assoc matched
                                             :len (+ (:len matched) (:len result)))]
                          ;; Ignore nil results
                          (if (nil? (:val result))
                            matched
                            (assoc matched :val (conj (:val matched) (:val result)))))
                        (reduced result))))
                  {:tag ::success :len 0 :val []}
                  matchers)]
      (if (= ::success (:tag result))
        ;; get the raw match
        (assoc result :raw (substring input (:len result)))
        result))))

(defn option
  "Makes a parser optional by always returning success"
  [p]
  (fn [input]
    (inc-counter :option)
    (let [result (parse p input)]
      (if (= ::success (:tag result))
        result
        (success (:pos input) 0 "" nil)))))

(defn many
  "Tries to match a parser zero or more times"
  [p]
  (fn [input]
    (inc-counter :many)
    (loop [i input
           res []
           raw ""]
      (let [result (parse p i)]
        (if (= ::success (:tag result))
          (recur (update-state i result)
                 (conj res (:val result))
                 (str raw (:raw result)))
          (success (:pos input) (count raw) raw res))))))

(defn many1
  "Match a parser 1 or more times"
  [p]
  (let [many-p (many p)]
    (fn [input]
      (inc-counter :many1)
      (let [res (parse p input)]
        (if (success? res)
          (many-p input)
          res)))))

                                    
(defn char-range
  "Matcher that checks whether a character is in a certain range"
  [c1 c2]
  (fn [input]
    (let [char (substring input 1)]
      (if (and
           (not= char ::eof)
           (>= (int (first char)) (int c1))
           (>= (int c2) (int (first char))))
        (success (:pos input) 1 char char)
        (error (:pos input) (str c1 "-" c2) char)))))

(defn char-choice
  "Match any of the characters in a string"
  [string]
  (apply choice (map str string)))

(defn skip
  "Skip the result in case of a succesful parse"
  [p]
  (fn [input]
    (let [res (parse p input)]
      (if (success? res)
        (assoc res :val nil)
        res))))

(defn look-ahead
  "Does not consume anything if the parse is successful"
  [p]
  (fn [input]
    (let [res (parse p input)]
      (if (success? res)
        (assoc res :len 0)
        res))))

(defn parse-all
  "Tries to parse the whole input and fails if it can't. Contrast with parse which will
  parse as much as it can"
  [p input]
  (let [res (parse p input)]
    (if (success? res)
      (if (= ::eof (advance-state input (inc (:len res))))
        (error (:pos input) ::eof (substring input 1))
        res)
      res)))

(defn with-value
  "Transform the result of a parser"
  [p f]
  (fn [input]
    (let [res (parse p input)]
      (if (success? res)
        (assoc res :val (f (:val res)))
        res))))

(defn with-raw-value
  "Use the raw result of a parser to get a value"
  ([p f]
   (fn [input]
     (let [res (parse p input)]
       (if (success? res)
         (assoc res :val (f (:raw res)))
         res))))
  ([p]
   (with-raw-value p identity)))

(defn expect
  "Set an expected value in case the parser fails"
  [p exp]
  (fn [input]
    (let [res (parse p input)]
      (if (success? res)
        res
        (assoc res :expected exp)))))

(def lowercase (char-range \a \z))
(def uppercase (char-range \A \Z))
(def letters (choice lowercase uppercase))
(def digits (char-range \0 \9))
(def spaces (char-choice " \t"))
(def whitespace (char-choice " \t\r\n"))
(def new-line (chain (option "\r") "\n"))
(defn eof [input]
  (if (= ::eof (:pos (advance-state input 1)))
    (success (:pos input) 0 "" ::eof)
    (error (:pos input) ::eof (substring input 1))))


(def integer (with-raw-value
               (expect (many1 digits) "integer")
              #(java.lang.Integer/parseInt %)))

(def number (with-raw-value
             (expect 
              (chain integer (option (chain "." integer)))
              "number")
             #(java.lang.Float/parseFloat %)))

(def identifier (with-raw-value
                  (expect
                   (chain (choice "_" letters)
                          (many (choice letters digits "_")))
                   "identifier")))

(defn token
  "Tokenizes by skipping any whitespace after the parser"
  [p]
  (with-value
   (chain
    p
    (skip (many spaces)))
   first))

(defn parens
  "Parses a parser between parens"
  [p]
  (with-value
   (chain
    (skip (token "("))
    (token p)
    (skip (token ")")))
   first))


(defn- prefix-val
  "Applies result-fns for prefix operators from right to left"
  [val prefixes]
  (reduce
   (fn [val [_ _ rfn]]
     (rfn val))
   val
   (reverse prefixes)))

(defn- precedence
  "Creates a parser for a precedence level. Precedence levels are a vector of rules.
  Each rule is formatted as [type op result-fn <direction>]
  Type is one of :infix or :prefix
  Op is a parser
  result-fn gets applied to the values to either side of the op
  direction is one of :left or :right
  "
  [prev rules]
  (let [[prefix infix] (reduce ;; Create 2 vectors of prefix and infix operators
                        (fn [[prefix infix] [type op rfn order :as rule]]
                          (cond
                            ;; have the operator parsers return their rule as a value
                            ;; we'll use these later on to distinguish between
                            ;; left/right associativity and to apply the result-fns
                            (= type :prefix) [(conj prefix (with-value op
                                                             (fn [_] rule)))
                                              infix]
                            (= type :infix) [prefix
                                             (conj infix (with-value op
                                                           (fn [_] rule)))]))
                        [[] []]
                        rules)
        ;; build the actual parser
        prefix-parser (apply choice prefix)
        infix-parser (apply choice infix)
        parser (chain (many prefix-parser)
                      prev
                      (many (chain
                             infix-parser
                             (many prefix-parser)
                             prev)))]
    ;; The result is unwieldy. We'll transform it by applying all the result-fns in
    ;; the right order depending on associativity of the operators
    (with-value parser
      (fn [[pre prev inf :as result]]
        (loop [res [(prefix-val prev pre)] ;;first apply any prefix operators
               ops []
               rem inf]
          (if (seq rem) ;;loop while there are remaining infix terms
            (let [[[_ _ rfn dir :as op] pres prev] (first rem)
                  val (prefix-val prev pres)]
              (cond
                ;; in case of left associative, pop 1 value and apply rfn to it and
                ;; the matched prev value after applying prefix matches
                (= dir :left) (recur (conj (pop res) (rfn (peek res) val))
                                     ops
                                     (rest rem))
                ;; in case of right associative, add value and op to the stack
                (= dir :right) (recur (conj res val)
                                      (conj ops op)
                                      (rest rem))))
            ;;when no remaining terms, we empty the ops stack
            (if (seq ops)
              ;; while there are ops, pop top 2 values and 1 op and apply
              (let [[_ _ rfn dir] (peek ops)
                    rhs (peek res)
                    lhs (peek (pop res))]
                (recur (conj (pop (pop res)) (rfn lhs rhs))
                       (pop ops)
                       rem))
              ;; we should have 1 result left after applying all ops
              (first res))))))))

(defn expr-parser
  "Build an expression parser out of terminals and a table of precedence rules"
  [terms & table]
  (reduce
   precedence
   terms
   table))

(defn unary [op]
  (fn [val]
    [op val]))

(defn binary [op]
  (fn [lhs rhs]
    [op lhs rhs]))

(defmacro wrap [name]
  `(fn [& args#]
      (apply ~name args#)))

(declare expr)

;; Parse simple arithmetic expressions
(def math-expr (expr-parser (choice (token number)
                                    (token identifier)
                                    (parens (wrap expr)))
                       [[:prefix (token "-") (unary :-)]
                        [:prefix (token "+") (unary :+)]]
                       [[:infix (token "^") (binary :exp) :right]]
                       [[:infix (token "*") (binary :*) :left]
                        [:infix (token "/") (binary :/) :left]
                        [:infix (token "%") (binary :%) :left]]
                       [[:infix (token "-") (binary :-) :left]
                        [:infix (token "+") (binary :+) :left]]))

(def let-expr (with-value (chain 
                           (skip (many whitespace))
                           (skip (token "let"))
                           (token identifier)
                           (skip (token "="))
                           (wrap expr)
                           (with-value
                             (chain 
                              (skip new-line)
                              (many (chain (wrap expr) (skip new-line)))
                              (option (wrap expr))
                              (skip (look-ahead (choice "}" eof))))
                             #(apply conj %)))
                (fn [[var val exprs]]
                  [:let var val exprs])))

(def block (with-value  (chain
                         (skip (token "{"))
                         (skip (many whitespace))
                         (many (with-value (chain (wrap expr) (skip new-line)) first))
                         (option (wrap expr))
                         (skip (token "}")))
             (fn [[exprs expr]]
               [:block (conj exprs expr)])))

(def expr (choice let-expr math-expr block))

(def program (many (with-value (chain expr (skip (choice eof new-line))) first)))

(defn eval-tree [ast env]
  (cond
    (number? ast) ast
    (string? ast) (if-let [val (env ast)]
                    val
                    (throw (Error. (str ast " is not defined."))))
    (= (count ast) 2) (let [[op term] ast]
                        (cond
                          (= op :-) (- (eval-tree term env))
                          (= op :+) (eval-tree term env)))
    (= (count ast) 3) (let [[op left right] ast]
                        (cond
                          (= op :+) (+ (eval-tree left env) (eval-tree right env))
                          (= op :-) (- (eval-tree left env) (eval-tree right env))
                          (= op :/) (/ (eval-tree left env) (eval-tree right env))
                          (= op :*) (* (eval-tree left env) (eval-tree right env))
                          (= op :%) (mod (eval-tree left env) (eval-tree right env))
                          (= op :exp) (Math/pow (eval-tree left env)
                                                (eval-tree right env))))))

(defn calc [str env]
  (let [res (parse-all expr (init-state str))]
    (if (success? res)
      (eval-tree (:val res) env)
      res)))
