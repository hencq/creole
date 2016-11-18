(ns creole.core
  (:require [clojure.set]
            [clojure.string :as string]))

(defprotocol Parser
  (parse [p input] "Parse an input"))

;; Clojure functions can be parsers. Parse just calls the function.
(extend-type clojure.lang.AFn
  Parser
  (parse [p input]
    (p input)))


(defn init-state
  "Takes a string and produces an initial parser state"
  [str]
  {:txt str
   :pos 0})


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
  (let [len (count cand)]
    (fn [{:keys [txt pos] :as input}]
      (inc-counter cand)
      (if (<= (+ pos len) (count txt))
        (let [head (subs txt pos (+ pos len))]
          (if (= head cand)
            (success pos len head head)
            (error pos cand head)))
        (error pos cand (subs txt pos))))))

;; Strings participate in the Parser protocol
(extend-type java.lang.String
  Parser
  (parse [p input]
    ((str-matcher p) input)))

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


(defn- update-state [state result]
  (if (= ::success (:tag result))
    (assoc state :pos (+ (:pos state) (:len result)))
    result))

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
        (let [startpos (:pos input)
              endpos (+ startpos (:len result))
              raw (subs (:txt input) startpos endpos)]
          (assoc result :raw raw))
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
           res []]
      (let [result (parse p i)]
        (if (= ::success (:tag result))
          (recur (update-state i result)
                 (conj res (:val result)))
          (let [startpos (:pos input)
                endpos (:pos i)
                len (- endpos startpos)
                raw (subs (:txt input) startpos endpos)
                ;; ignore empty matches
                res (if (empty? res) nil res)] 
            (success startpos len raw res)))))))

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

                                    
(defn char-range [c1 c2]
  (fn [input]
    (if (>= (:pos input) (count (:txt input)))
      (error (:pos input) (str c1 "-" c2) ::eof)
      (let [char (.charAt (:txt input) (:pos input))]
        (if (and (>= (int char) (int c1))
                 (>= (int c2) (int char)))
          (success (:pos input) 1 (str char) (str char))
          (error (:pos input) (str c1 "-" c2) (subs (:txt input) (:pos input)
                                                    (inc (:pos input)))))))))

(defn char-choice [string]
  (apply choice (map str string)))

(defn skip
  "Skip the result in case of a succesful parse"
  [p]
  (fn [input]
    (let [res (parse p input)]
      (if (success? res)
        (assoc res :val nil)
        res))))


(defn parse-all
  "Tries to parse the whole input and fails if it can't. Contrast with parse which will
  parse as much as it can"
  [p input]
  (let [res (parse p input)]
    (if (success? res)
      (let [endpos (+ (:pos input) (:len res))]
        (if (< endpos (count (:txt input)))
          (error endpos ::eof (subs (:txt input) endpos (inc endpos)))
          res))
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
(def whitespace (char-choice " \t\r\n"))

(def integer (with-raw-value
              (expect (many1 digits) "integer")
              #(java.lang.Integer/parseInt %)))

(def number (with-raw-value
             (expect 
              (chain integer (option (chain "." integer)))
              "number")
             #(java.lang.Float/parseFloat %)))

(defn token
  "Tokenizes by skipping any whitespace after the parser"
  [p]
  (with-value
   (chain
    p
    (skip (many whitespace)))
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

(defn- ast [v]
  (if (= (count v) 1)
    (first v)
    (reduce
     (fn [expr [op right]]
       [(keyword op) expr right])
     (first v)
     (second v))))

(defn- infix-val
  "Applies result-fns for infix operators from right to left"
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
        parser (chain (with-value (many prefix-parser) #(or % []))
                      prev
                      (with-value
                       (many (chain
                              infix-parser
                              (with-value (many prefix-parser) #(or % []))
                              prev))
                       #(or % [])))]
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

;; Parse simple arithmetic expressions
(def expr (expr-parser (choice (token number)
                               (parens expr))
                       [[:prefix (token "-") #(vector :- %)]
                        [:prefix (token "+") #(vector :+ %)]]
                       [[:infix (token "^") #(vector :exp %1 %2) :right]]
                       [[:infix (token "*") #(vector :* %1 %2) :left]
                        [:infix (token "/") #(vector :/ %1 %2) :left]
                        [:infix (token "%") #(vector :% %1 %2) :left]]
                       [[:infix (token "-") #(vector :- %1 %2) :left]
                        [:infix (token "+") #(vector :+ %1 %2) :left]]))


(defn eval-tree [ast]
  (cond
    (number? ast) ast
    (= (count ast) 2) (let [[op term] ast]
                        (cond
                          (= op :-) (- (eval-tree term))
                          (= op :+) (eval-tree term)))
    (= (count ast) 3) (let [[op left right] ast]
                        (cond
                          (= op :+) (+ (eval-tree left) (eval-tree right))
                          (= op :-) (- (eval-tree left) (eval-tree right))
                          (= op :/) (/ (eval-tree left) (eval-tree right))
                          (= op :*) (* (eval-tree left) (eval-tree right))
                          (= op :%) (mod (eval-tree left) (eval-tree right))
                          (= op :exp) (Math/pow (eval-tree left) (eval-tree right))))))

(defn calc [str]
  (let [res (parse-all expr (init-state str))]
    (if (success? res)
      (eval-tree (:val res))
      res)))







