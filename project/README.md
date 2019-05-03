# Project

Follow the [instructions](INSTRUCTIONS.md), use this space to document your project for yourself and the graders.

## Names
Alex Fatyga, Noah Malhi, Justin Morgan
## Summary
* We will work on our project to add functionality to our last homework assignment.
* Our add-ons are:
*	5pt Add an infix function composition operator (.). So you may write f . g instead of \x -> f (g x)
* 5pt Writing a quickcheck generator and shrinker for your Ast and using it to test your parser
*	5pt Warn when a variable is introduced but never used
*	15pt Checking simple types, where every variable has a type annotation (lecture will be presented on this
## Plan
*
* Who did what: 
* modify the EnvUnsafe monad code to include logging (the Writer monad) using a print expression, as we did in a previous homework (because we will be adding the print and separator from lang2) => Justin	
* Tests
* EvalTest.hs => all
* ParserTest.hs => Justin (w/ the mixin of shrinkers + quickcheck generator) 	
* CheckTest.hs => all
* Mix-Ins
* 5pt Warn when a variable is introduced but never used => Alex
* 5pt Add an infix function composition operator (.). So you may write f . g instead of \x -> f (g x) => Alex
* 5pt Writing a quickcheck generator and shrinker for your Ast and using it to test your parser => Alex + Justin
* 15pt Checking simple types, where every variable has a type annotation (lecture will be presented on this => Noah + Justin
* Ast.hs => Alex + Noah
* Check.hs => all
* Eval.hs
* Stdlib => Noah
* Eval => all
* Run => Justin
* Exec.hs
* Exec => Alex
* Warn => Noah
* Parser.hs => Alex
* We will meet up to work on the project and discuss as we each work on our separate parts. 
* We will begin with the vanilla features and then work on our add-ons. 
* Justin will work on testing primarily while Alex and Noah will work on the vanilla project unless issues pop up to switch things around. 
