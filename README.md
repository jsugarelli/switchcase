The switchcase package
================
Joachim Zuckarelli

**New: switchcase tutorial on YouTube**

To learn more about the `switchcase` package and how it relates to R’s
built-in primitive function `switch()` please watch the new `switchcase`
tutorial on [YouTube](https://youtu.be/3ybF8u_PE7w).

# Introduction

In contrast to many other programming languages, R has no real native
switch-case statement. It does provide the primitive `switch()`
function; but `switch()` is actually made to select elements from a list
based on their name or index position. It cannot (at least not without
awkward workarounds) be used to build conditions with more complex
comparison values. Also, when fed with a numeric expression, it
interprets the expression as an index and selects the element from the
list at this index position. This is not useful when you want to test an
expression against another numeric expression/value.

It would be good to have an efficient way of testing multiple, similar
conditions, including conditions that are more complex in their
comparison values or result in numeric values. At the same time nested
if-else constructs should be avoided because they make the code less
clear and readable.

The `switchcase` package provides a solution by offering a true
switch-case construct for R. It is encapsulated in the package’s main
function, `switchCase()`.

`switchCase()` allows to define multiple ‘case’ branches (alternatives)
that consist of a condition and a code block that is executed if the
condition is fulfilled. Also, it can be specified if the switch-case
construct shall be left after one alternative code block has been
executed, or if the other (following) conditions shall be tested, as
well. This ‘break’ behavior can be defined on the level of the whole
switch-case construct or on the level of each individual alternative,
with the alternative’s ‘break’ behavior setting overruling the
construct-level option. Also, an alternative branch of a switch-case
construct can return a value (which is then the return value of the
`switchCase()` function).

# Installing `switchcase`

Before using `switchCase()` we need to install and load the package:

    install.packages("switchcase", dependencies = TRUE)

``` r
library(switchcase)
```

# Working with `switchcase`

To understand how the `switchCase()` function works, it makes sense to
have a look at an example. In this simple example we are calculating the
body-mass index of a person and then interpreting its value:

``` r
bmi <- function(mass, size) {
ind <- mass / size^2

switchCase(
  ind,
  alt(
    ..expr <= 15,
    { cat("Your body mass index is ", ind, " which is very severely underweight.\n") },
    "very severely underweight"
  ),
  alt(
    ..expr > 15 & ..expr <= 16,
    { cat("Your body mass index is ", ind, " which is severely underweight.\n") },
  "severely underweight"
  ),
  alt(
    ..expr > 16 & ..expr <= 18.5,
    { cat("Your body mass index is ", ind, " which is underweight.\n") },
    "underweight"
  ),
  alt(
    ..expr > 18.5 & ..expr <= 25,
    { cat("Your body mass index is ", ind, " which is normal.\n") },
    "normal"
  ),
  alt(
    ..expr > 25 & ..expr <= 30,
    { cat("Your body mass index is ", ind, " which is overweight.\n") },
    "overweight"
  ),
  alt(
    ..expr > 30 & ..expr <= 35,
    { cat("Your body mass index is ", ind, " which is moderately obese.\n") },
    "moderately obese"
  ),
  alt(
    ..expr > 35 & ..expr <= 40,
    { cat("Your body mass index is ", ind, " which is severely obese.\n") },
    "severely obese"
  ),
  alt(
    ..expr > 40,
    { cat("Your body mass index is ", ind, " which is Very severely obese.\n") },
    "very severely obese"
  )
 )
}

bmi.person1 <- bmi(82.5, 1.79)
```

    ## Your body mass index is  25.74826  which is overweight.

``` r
cat("Person1 turned out to be ", bmi.person1)
```

    ## Person1 turned out to be  overweight

Let us go through the call of `switchCase()` step by step:

The first argument is the value to be tested, in our case here the
calculated body-mass index (variable `ind`). This non-optional argument
is followed by a set of alternative branches. The alternatives are built
using the `alt()` function. The `alt()` function takes a condition as
its first argument. If you want to refer to the value that is tested in
this switch-case construct then use the construct’s standard variable
`..expr` (yes, two dots\!). In our example, `..expr` would take the
value of ind. Working with `..expr` allwows you can build complex
conditions for your alternative ‘case’ branches.

The second argument of `alt()` is the code that will be executed if the
condition evaluates to `TRUE`. If your code has multiple R statements
then put the statements in curly brackets. It is recommended to do that
in any case. If an alternative has `NULL` as its code (i.e. the second
argument of the `alt()` function is `NULL`) then this alternative is the
default alternative that will be exceuted if no other condition
evaluates to `TRUE`. If you define multiple defaults (which should be
avoided, of course) then only the first one would be executed.

The optional third argument is the return value that the `switchCase()`
function will provide if this particular ‘case’ alternative is executed.
If multiple alternatives are executed (recall, that this is possible
because the testing of alternative conditions does not necessarily need
to stop after one condition has been found to be `TRUE`) then only the
return value of the last alternative branch is returned.

The optional fourth argument of the `alt()` function (not used in this
example) is a logical value indicating the ‘break’ behavior of this
alternative branch. If set to `TRUE` then the execution of the
switch-case construct will stop, regardless of the break option that has
been defined on the construct level. The alternative’s definition of the
‘break’ behavior always overrules the more general definition of ‘break’
behavior on the `switchCase()` function level.

The last and optional argument (break) of the `switchCase()` function
(not used in this example) is a logical value indicating if the
switch-case construct will be left after a condition was fulfilled and
the R code associated with that ‘case’ branch has been executed. The
default value for break is `TRUE`. Note, that this behavior
specification can be overruled by the ‘break’ behavior specified in
individual ‘case’ alternatives.

All code in the alternative ‘case’ branches is evaluated in the
environment from which `switchCase()` was called so it is easy to access
variables of your script or function in which `switchCase()` is used.

## Citing `switchcase`

To cite this package in publications, run `citation("switchcase")` in R. This
prints the reference information stored in the package's `inst/CITATION` file.

# Contact

Follow me on Twitter (<https://twitter.com/jsugarelli>) to stay
up-to-date on new development around the `switchcase` package.
