(def say1 ([name age] (str "Hello, " name ". I am " age " years old.")))
(def say ([name age] (str "Hello, " name ". I am " age " years old.")))
(def add ([a b] (+ a b)))
(def fib ([n] (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))