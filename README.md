# A Lambda-Mu Interpreter in Clojure

This is an interpreter for [Lambda-Mu calculus][1] implemented in Clojure. The code I have is based on [the Scala version I wrote before][3] and [a similar interpreter implemented in Haskell][2]. 

Type "lein run" to start the interpreter. If you prefer a standalone uberjar, type `lein uberjar` and then
```
java -jar target/lambda-mu-0.1.0-SNAPSHOT-standalone.jar
```
to get the ball running.

You may define short-cuts via `let <name> = <lambda-mu expression>` and then use `name` for the corresponding expression.  You may save the context (the let bindings you defined) via `:save "<filename>"` and then load it back using `:load "<filename>"` inside the interpreter. 

[1]: https://en.wikipedia.org/wiki/Lambda-mu_calculus
[2]: https://stackoverflow.com/questions/28752112/interpret-parigots-lambda-mu-calculus-in-haskell
[3]: https://github.com/kaygun/LambdaMu
