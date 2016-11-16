(ns creole.core)

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

(def test-input (to-input "2 + 3 * 4"))

(defn- update [m k f]
  (assoc m k (f (m k))))

(defn str-matcher
  "Returns a matcher that tries to match a string"
  [cand]
  (let [len (count cand)]
    (fn [{:keys [txt pos] :as input}]
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

(defn choice
  "Takes a seq of matchers and returns a matcher that tries all matchers in turn"
  [& matchers]
  (fn [input]
    (let [tried (map #(parse % input) matchers)
          matched (seq (drop-while #(= ::fail (:tag %)) tried))]
      (if matched
        (first matched)
        (let [error (first tried)
              expected (set (map #(:expected %) tried))]
          (assoc error :expected expected))))))


(defn- update-state [state result]
  (if (= ::success (:tag result))
    (assoc state :pos (+ (:pos state) (:len result)))
    result))

(defn chain
  "Takes a seq of parsers and returns a parser that tries to match all of them in order"
  [& matchers]
  (fn [input]
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
    (let [result (parse p input)]
      (if (= ::success (:tag result))
        result
        (success (:pos input) 0 "" nil)))))

(defn many
  "Tries to match a parser zero or more times"
  [p]
  (fn [input]
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
  (fn [input]
    (let [res (parse p input)]
      (if (success? res)
        ((many p) input)
        res))))

                                    
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

(defn val
  "Transform the result of a parser"
  [p f]
  (fn [input]
    (let [res (parse p input)]
      (if (success? res)
        (assoc res :val (f (:val res)))
        res))))

(defn raw
  "Use the raw result of a parser to get a value"
  ([p f]
   (fn [input]
     (let [res (parse p input)]
       (if (success? res)
         (assoc res :val (f (:raw res)))
         res))))
  ([p]
   (raw p identity)))

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

(def integer (raw
              (expect (many1 digits) "integer")
              #(java.lang.Integer/parseInt %)))

(def number (raw
             (expect 
              (chain integer (option (chain "." integer)))
              "number")
             #(java.lang.Float/parseFloat %)))

(defn token
  "Tokenizes by skipping any whitespace after the parser"
  [p]
  (val 
   (chain
    p
    (skip (many whitespace)))
   first))

(defn parens
  "Parses a parser between parens"
  [p]
  (val 
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

(declare expr)
(def term (val 
           (chain
            (choice
             (chain
              (val (token (char-choice "+-")) keyword)
              #(term %))
             (token number)
             (parens #(expr %))))
           first))

(def product (val
              (chain
               #(term %)
               (many (chain
                      (token (char-choice "/*%"))
                      #(term %))))
              ast))

(def expr (val 
           (chain
            (skip (many whitespace))
            #(product %)
            (many (chain
                   (token (char-choice "+-"))
                   #(product %))))
           ast))

(defn- prefix-parser
  "Return a parser that parses prefix operators"
  [op goal resultfn]
  (val 
   (chain op goal)
   (fn [[_ v]] (resultfn v))))

(defn- infix-left-parser
  "Return a parser that parses binary left associative operators"
  ([op rule resultfn]
   (val 
    (chain
     rule
     (many (chain op rule)))
    (fn [[left right]]
      (reduce
       (fn [l [_ r]]
         (resultfn l r))
       left
       right)))))

(defn- infix-right-parser
  "Return a parser that parses binary right associative operators"
  [op goal rule resultfn]
  (choice
   (val
    (chain rule op goal)
    (fn [[l _ r]] (resultfn l r)))
   rule))

(defn- expr-rule-parser
  "Create a rule for an expression parser"
  [rule goal [notation op resultfn assc]]
  (cond
    (= notation :prefix) (prefix-parser op goal resultfn)
    (= notation :infix) (cond
                          (= assc :left) (infix-left-parser op rule resultfn)
                          (= assc :right) (infix-right-parser op goal rule resultfn))))

(defn- precedence-level
  [prev rules]
  (let [goal (fn goal [i]
               ((apply choice
                       (conj
                        (vec (map (fn [rule] (expr-rule-parser prev goal rule))
                                  rules))
                        prev))
                i))]
    goal))


(defn expr-parser
  "Build an expression parser out of terminals and a table of rules"
  [terms & table]
  (reduce
   precedence-level
   terms
   table))

(def expr (expr-parser (choice (token number)
                               (parens #(expr %)))
                       [[:prefix (token "-") #(vector :- %)]
                        [:prefix (token "+") #(vector :+ %)]]
                       [[:infix (token "^") #(vector :exp %1 %2) :right]]
                       [[:infix (token "*") #(vector :* %1 %2) :left]
                        [:infix (token "/") #(vector :/ %1 %2) :left]
                        [:infix (token "%") #(vector :% %1 %2) :left]]
                       [[:infix (token "+") #(vector :+ %1 %2) :left]
                        [:infix (token "-") #(vector :- %1 %2) :left]]))

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







