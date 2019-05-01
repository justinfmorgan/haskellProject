# Project

Follow the [instructions](INSTRUCTIONS.md), use this space to document your project for yourself and the graders.

## Names
Alex Fatyga, Noah Malhi, Justin Morgan
## Summary
* We will work on our project to add functionality to our last homework assignment.
* Our add-ons (for now) will be:
*	5pt Add an infix function composition operator (.). So you may write f . g instead of \x -> f (g x)
*	5pt Add multiple sequential definitions to let. So you may write let x = 4, y = x + 5, z = y in z * 2 instead of let x = 4 in (let y = x + 5 in (let z = y in z * 2))
*	5pt Warn when a variable is introduced but never used
*	15pt Checking simple types, where every variable has a type annotation (lecture will be presented on this
## Plan
*
* To Do and who it's assigned to: 
* modify the EnvUnsafe monad code to include logging (the Writer monad) using a print expression, as we did in a previous homework (because we will be adding the print and separator from lang2) => Justin			Done?
* Tests
* EvalTest.hs => all Done
* ParserTest.hs => Justin Done 	
* CheckTest.hs => Justin Done	
* Mix-Ins
* 5pt Warn when a variable is introduced but never used => Alex
* 5pt Add an infix function composition operator (.). So you may write f . g instead of \x -> f (g x) => Alex
* 5pt Add letrec to make recursion more convenient. So you can write letrec f = \ x -> if x == 0 then 1 else x * (f (x-1)) in f 5. => Alex
* 15pt Checking simple types, where every variable has a type annotation (lecture will be presented on this => Noah
* Ast.hs
* Data Ast => Alex				Done
* showfullyParen => Alex			Done
* showPretty => Noah				Done!!
* Check.hs
* Data WarningMsg => Justin			Done
* Check => Noah, Alex
* Eval.hs
* Stdlib => Noah
* Data Val => Alex				Done			
* Instance show Val => Alex			Done
* Run => Noah					Done?
* Exec.hs
* Exec => Alex					Done
* Instance Eq LangOut => Justin
* Warn => Noah (wait till after check is done)
* Parser.hs => Alex
* LangMonad => Will be done as someone needs it for their part


* We will meet up to work on the project and discuss as we each work on our separate parts. 
* We will begin with the vanilla features and then work on our add-ons. 
* Justin will work on testing primarily while Alex and Noah will work on the vanilla project unless issues pop up to switch things around. 
