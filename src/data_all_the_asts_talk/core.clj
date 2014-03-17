(ns data-all-the-asts-talk.core)


;; When compilers often consist of at least three phases:
;; 1. Lexer/Parser
;; 2. Analyzer
;; 3. Emitter


;; Clojure Lexer/Parser (they are often combined in lisps)
(read-string "(+ 1 2)")

;; Currently the analyzer and emitter are combined in Clojure:

(eval '(+ 1 2))


;; The data exchange between the analyzer and the emitter is known as
;; the Abstract Syntax Tree or "AST". Clojure is no exception. The
;; Lexer/Parser is the reader, and we can invoke the analyzer via Java
;; interop:


(clojure.lang.Compiler/analyze clojure.lang.Compiler$C/EXPRESSION '(+ 1 2))

;; This returns a instance of StaticMethodExpr. Okay, what on earth is
;; that?


;; Most ASTs (and the JAVA AST is one) are built on closed types. To
;; really dig into them you would need to read the sourcecode and
;; dispatch on the node type. You can dig into Compiler.java if you
;; want and figure out what all these types do and how to access them.
;; Warning, some are closed and aren't really possible to read from
;; the outside.

;; A few more examples

(let [exprs '[1
             (+ 1 2)
             (do 44 43)]]
  (doall (map #(clojure.lang.Compiler/analyze clojure.lang.Compiler$C/EXPRESSION %) exprs)))



;; This idea of using types to represent ASTs is not new. Example from
;; "Kaleidoscope in Haskell"
;; https://github.com/sdiehl/kaleidoscope/blob/master/src/chapter7/Syntax.hs

;; --------------------------------------------------------------------
;; -- |
;; -- Module    :  Syntax
;; -- Copyright :  (c) Stephen Diehl 2013
;; -- License   :  MIT
;; -- Maintainer:  stephen.m.diehl@gmail.com
;; -- Stability :  experimental
;; -- Portability: non-portable
;; --
;; --------------------------------------------------------------------

;; module Syntax where

;; type Name = String

;; data Expr
;;   = Int Integer
;;   | Float Double
;;   | Var String
;;   | Call Name [Expr]
;;   | Function Name [Name] Expr
;;   | Extern Name [Name]
;;   | BinaryOp Name Expr Expr
;;   | UnaryOp Name Expr
;;   | If Expr Expr Expr
;;   | For Name Expr Expr Expr Expr
;;   | BinaryDef Name [Name] Expr
;;   | UnaryDef Name [Name] Expr
;;   | Let Name Expr Expr
;;   deriving (Eq, Ord, Show)




;; LINQ Expression trees on .NET are the same sort of thing. Here we
;; can use ASTs to construct code on the fly

;; This code creates (fn [num] (< num 5))

;; http://msdn.microsoft.com/en-us/library/bb397951.aspx

;; ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
;; ConstantExpression five = Expression.Constant(5, typeof(int));
;; BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
;; Expression<Func<int, bool>> lambda1 =
;;    Expression.Lambda<Func<int, bool>>
;;                     ( numLessThanFive,
;;                       new ParameterExpression[] { numParam } ) ;



;; And in  OCAML:

;; (*===----------------------------------------------------------------------===
;;  * Abstract Syntax Tree (aka Parse Tree)
;;  *===----------------------------------------------------------------------===*)

;; (* expr - Base type for all expression nodes. *)
;; type expr =
;;   (* variant for numeric literals like "1.0". *)
;;   | Number of float

;;   (* variant for referencing a variable, like "a". *)
;;   | Variable of string

;;   (* variant for a binary operator. *)
;;   | Binary of char * expr * expr

;;   (* variant for function calls. *)
;;   | Call of string * expr array

;; (* proto - This type represents the "prototype" for a function, which captures
;;  * its name, and its argument names (thus implicitly the number of arguments the
;;  * function takes). *)
;; type proto = Prototype of string * string array

;; (* func - This type represents a function definition itself. *)
;; type func = Function of proto * expr



;; So all these ASTs have something in common, they all use types not
;; data. We would shun this approach when writing application code, so
;; why do we suddenly feel the need for types when writing compilers?
;; What would happen if we switched to representing our data as pure
;; data, hashmaps, vectors, etc.

;; What would be the simplest way to write a constant AST node with
;; only data? How about this?

{:op :const
 :value 42
 :type :integer}

;; That's simple, what about a if?

{:op :if
 :test {:op :const
        :value true
        :type :boolean}
 :then {:op :const
        :value 42
        :type :integer}
 :else {:op :const
        :value 0
        :type :integer}}


;; One of the key parts to working with ASTs is the ability to analyze
;; them. Sadly we haven't gained much here. We still need the ability
;; to traverse the AST in a generic way. To help with this, we'll add
;; one more memeber to each map, :children will list the children in
;; the order they would be executed when compiled:

{:op :if
 :children [:test :then :else]
 :test {:op :const
        :children []
        :value true
        :type :boolean}
 :then {:op :const
        :children []
        :value 42
        :type :integer}
 :else {:op :const
        :children []
        :value 0
        :type :integer}}


;; Let's also assume we can have nodes like the following:

(def test-ast {:op :do
               :children [:body]
               :body [{:op :const
                       :children []
                       :value true
                       :type :boolean}
                      {:op :const
                       :children []
                       :value 42
                       :type :integer}]})

;; Now we can write a simple pre-walk function:

(defn prewalk [ast f]
  (f (reduce
      (fn [acc key]
        (let [value (get ast key)]
          (if (vector? value)
            (assoc acc key (doall (map (fn [node]
                                         (prewalk node f))
                                       value)))
            (assoc acc key (prewalk value f)))))
      ast
      (:children ast))))


(prewalk test-ast (fn [ast]
                    (println (:op ast))
                    ast))

(prewalk test-ast (fn [ast]
                    (println (:op ast))
                    [:replaced]))




(require '[clojure.tools.analyzer.jvm :as jvm-an])


(clojure.pprint/pprint (jvm-an/analyze '(if true 42 0) (jvm-an/empty-env)))


(defmulti to-clj :op)



(to-clj (jvm-an/analyze '((fn [x & r] (let [v x] (if v r 0)
                                          ))) (jvm-an/empty-env)))

(to-clj (jvm-an/analyze '(let [x 42] x) (jvm-an/empty-env)))

;; A AST->CLJ emitter...because why the heck not?

(defmethod to-clj :if
  [{:keys [test then else]}]
  `(if ~(to-clj test)
     ~(to-clj then)
     ~(to-clj else)))

(defmethod to-clj :const
  [{:keys [form]}]
  form)

(defmethod to-clj :do
  [{:keys [statements ret]}]
  `(do ~@(map to-clj statements)
       ~(to-clj ret)))

(defmethod to-clj :fn
  [{:keys [methods internal-name] :as ast}]
  (println (keys ast))
  `(fn ~(symbol internal-name) ~@(map to-clj methods)))

(defmethod to-clj :fn-method
  [{:keys [params body variadic?] :as ast}]
  (let [param-names (map :name params)]
    (if variadic?
      `([~@(butlast param-names) ~'& ~(last param-names)] ~(to-clj body))
      `([~@param-names] ~(to-clj body)))))

(defmethod to-clj :local
  [{:keys [name]}]
  name)

(defmethod to-clj :let
  [{:keys [bindings body]}]
  `(let [~@(mapcat to-clj bindings)]
     ~(to-clj body)))

(defmethod to-clj :binding
  [{:keys [name init]}]
  [name (to-clj init)])

(defmethod to-clj :invoke
  [{:keys [fn args]}]
  `(~(to-clj fn) ~@(map to-clj args)))

;; Cool now on to the fun stuff.


(require '[clojure.tools.analyzer.ast :as an-ast])


(defn simple-const-fold
  ([state {:keys [op] :as ast}]
     (cond (and (= op :binding)
              (= (-> ast :init :op) :const))
       (do (swap! state assoc (:name ast) (:init ast))
           (assoc ast :init {:op :const :form nil}))

       (and (= op :local)
            (contains? @state (:name ast)))

       (get @state (:name ast))

       (and (= op :if)
            (= (-> ast :test :op) :const))

       (if (-> ast :test :form)
         (:then ast)
         (:else ast))
       

       :else ast)))

(let [passes (partial simple-const-fold (atom {}))]
  (-> (jvm-an/analyze '(let [x 42]
                         (if x
                           true
                           false))
                      (jvm-an/empty-env))

      (an-ast/postwalk passes)

      (to-clj)))

(defn constant-folding-optimizer [form]
  (let [passes (partial simple-const-fold (atom {}))]
    (-> (jvm-an/analyze form
                        (jvm-an/empty-env))

        (an-ast/postwalk passes)

        (to-clj))))

(constant-folding-optimizer '(let [x 42
                                   y x]
                               (let [z y]
                                 (if z
                                   true
                                   false))))


(defn const? [ast]
  (= (:op ast) :const))

(defn find-non-const-idxs [keys values]
  (reduce
   (fn [acc idx]
     (let [key (nth keys idx)
           value (nth values idx)
           is-const? (if (and (const? key)
                              (const? value))
                       :const
                       :non-const)]
       (update-in acc [is-const?] (fnil conj []) {:key key
                                                  :value value})))
   {}
   (range (count keys))))


(defn debug [x]
  (clojure.pprint/pprint x)
  x)

(defn make-assoc-call [const non-const env]
  {:op :invoke
   :env env
   :children [:fn :args]
   :fn {:op :var
        :var #'assoc
        :env env}
   :args (concat [{:op :map
                   :keys (map :key const)
                   :vals (map :value const)
                   :env env}]
                 (mapcat (juxt :key :value) non-const))})

(defn map-optimizer [{:keys [op keys vals env] :as ast}]
  (if (= op :map)
    (let [{:keys [const non-const]} (find-non-const-idxs keys vals)]
      (if (or (= (count const) 0)
              (= (count non-const) 0))
        ast
        (make-assoc-call const non-const env)))
    ast))


(defmethod to-clj :map
  [{:keys [keys vals] :as ast}]
  (zipmap (map to-clj keys)
          (map to-clj vals)))

(defmethod to-clj :static-call
  [{:keys [class method args] :as ast}]
  `(. ~class ~method ~@(map to-clj args)))

(defmethod to-clj :var
  [{:keys [var]}]
  var)


(defn run-map-optimizer [form]
  (-> (jvm-an/analyze form
                      (jvm-an/empty-env))
      (an-ast/postwalk map-optimizer)
      (to-clj)))

(run-map-optimizer '{:a 1 :b (+ 4 3)})



;; Passes can be combined by using comp:

(defn run-passes [form]
  (let [passes (comp (partial simple-const-fold (atom {}))
                     map-optimizer)]
    (-> (jvm-an/analyze form
                        (jvm-an/empty-env))
        (an-ast/postwalk passes)
        to-clj)))


;; Notice how x is removed, but y since its value is an expression.
;; Since :a and :b are now constant the 2nd pass converts that part of
;; the map to a const and then calls assoc to pull in :c

(run-passes '(let [x 42
                   y (+ 1 2)]
               
               {:a 1 :b x :c y}))


;; Okay, now let's create a real constant folder

;; Helper function

(def const-fold-node nil)
(defmulti const-fold-node (fn [ast consts]
                            (:op ast)))

(defmethod const-fold-node :const
  [{:keys [name form] :as ast} consts]
  ast)

(defmethod const-fold-node :binding
  [{:keys [name init] :as ast} consts]
  (if (const? init)
    (do (swap! consts assoc name init)
        nil)
    ast))

(defmethod const-fold-node :do
  [{:keys [statements ret] :as ast} consts]
  (if-let [statements (not-empty (filter const? statements))]
    (assoc ast :statements statements)
    ret))

(defmethod const-fold-node :local
  [{:keys [name init] :as ast} consts]
  (if-let [val (get @consts name)]
    val
    ast))

(defmethod const-fold-node :let
  [{:keys [bindings body] :as ast} consts]
  (let [bindings (remove nil? bindings)]
    (if (empty? bindings)
      body
      (assoc ast :bindings bindings))))

(defmethod const-fold-node :var
  [ast consts]
  ast)

(defmethod const-fold-node :map
  [{:keys [keys vals env]} consts]
  (if (and (every? const? keys)
           (every? const? vals))
    {:op :const
     :form (zipmap (map :form keys)
                   (map :form vals))
     :env env}))


(def pure-vars #{#'assoc
                 #'dissoc
                 #'into})

(defn var-node? [ast]
  (= (:op ast) :var))

(defmethod const-fold-node :invoke
  [{:keys [fn args env] :as ast} consts]
  (if (and (var-node? fn)
           (contains? pure-vars (:var fn))
           (every? const? args))
    {:op :const
     :form (apply (:var fn) (map :form args))
     :env env}
    ast))

(def approved-static-calls
  #{[clojure.lang.Numbers "add"]})

(defmethod const-fold-node :static-call
  [{:keys [class method args env] :as ast} consts]
  (if (and (contains? approved-static-calls [class (name method)])
           (every? const? args))
    {:op :const
     :form (eval `(. ~class ~(symbol method) ~@(map :form args)))
     :env env}
    ast))


(defn run-constant-folding [form]
  (-> (jvm-an/analyze form
                      (jvm-an/empty-env))
      (an-ast/postwalk #(const-fold-node % (atom {})))
      to-clj))

(run-constant-folding '(let [x 42]
                         (dissoc {:a (+ 3 1)
                                  :c (into [4 5] [1 2 3])
                                  :b (assoc {} :c 42)}
                                 :b)))



;; Room for improvements
;; 1) replace CLJS analyzer (GSOC project)
;; 2) replace core.async / core.typed analyzer
;; 3) integration with lein/clojure compiler?
;; 4) SSA layer?
