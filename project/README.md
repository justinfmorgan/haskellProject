# Project

Follow the [instructions](INSTRUCTIONS.md), use this space to document your project for yourself and the graders.

## Names
Alex Fatyga, Noah Malhi, Justin Morgan
## Summary
* We will work on our project to add functionality to our last homework assignment.
* Our add-ons (for now) will be:
*	5pt Add an infix function composition operator (.). So you may write f . g instead of \x -> f (g x)
*	5pt Make lambdas support multiple arguments. So you may write \x y z -> x instead of \x -> \ y -> \ z -> x
*	5pt Add multiple sequential definitions to let. So you may write let x = 4, y = x + 5, z = y in z * 2 instead of let x = 4 in (let y = x + 5 in (let z = y in z * 2))
*	5pt Warn when a variable is introduced but never used
*	10pt Add runtime warnings to the monad, and flag appropriate conditions which are not errors, but cause concern (e.g., you defined a variable or function but then didn't use it, as in the Ok monad presented in lecture).
## Plan
* Files to Complete:
* Ast.hs ->
*	Data Ast => Alex 
*	instance show => Noah 
*	showfullyParen => Noah 
*	showPretty => Noah 
* Check.hs ->
*	Data WarningMsg => Justin 
*	Check => Justin 
* Eval.hs -> 
*	Data Val => Alex  
*	Instance show Val = > Noah 
*	Run => Justin 
* Exec.hs ->
*	Exec => Alex  
*	Warn => Alex 
* Parser 
*	Parser! => Mix of everyone 
* LangMonad => Will be done as someone needs it for their part
*	Data LangMonad 
*	runLangMonad 
*	Functor 
*	Monad

* We will meet up to work on the project and discuss as we each work on our separate parts. 
* We will begin with the vanilla features and then work on our add-ons. 
* Justin will work on testing primarily while Alex and Noah will work on the vanilla project unless issues pop up to switch things around. 
