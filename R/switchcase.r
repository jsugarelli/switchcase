#' @title Package 'switchcase'
#'
#' @description A simple yet flexible switch-case construct for R. Function
#'   \code{\link{switchCase}()} provides the actual switch-case functionality.
#'   The \code{\link{alt}()} function is used to formulate alternative 'case'
#'   branches of the switch-case construct.
#'
#' @name switchcase
#'
NULL



# define variable to avoid error messages
..expr <- 0


# helper function
ifnull <- function(x) {
  if(is.null(x)) {
    return(FALSE)
  }
  else {
    return(x == TRUE)
  }
}


#' @title The switch-case construct
#'
#' @description \code{switchCase()} provides the functionality of a typical
#'   switch-case statement as it is available in other programming languages.
#'
#' @param expr The expression which is evaluated and the value of which is
#'   tested in the alternative 'case' code branches.
#' @param ... The alternative code branches ('case' blocks). Each alternative is
#'   produced with the \code{\link{alt}()} function and is basically a
#'   \code{list} with up to four elements: (1) The expression against which the
#'   \code{expr} argument is tested (if the resulting condition is \code{TRUE}
#'   then the code of this alternative is executed) - if this first element of
#'   the alternative is \code{NULL} then this alternative is the \code{default}
#'   of the switch-case construct and will be executed if no condition of any
#'   other alternative evaluate to \code{TRUE}; (2) the code of the alternative
#'   that is executed if the condition of this alternative is fulfilled; (3) an
#'   optional return value that will be returned by \code{switchCase()} if the
#'   condition of this alternative evaluates to \code{TRUE} and after the
#'   alternative's code has been executed (default is \code{NULL} which means
#'   nothing is returned); (4) an optional logical value (with default
#'   \code{TRUE}) that indicates if the switch-case construct will be left after
#'   the code of this alternative has been executed, or if the subsequent
#'   alternatives are tested as well. This value overrides any option provided
#'   via the \code{break.case} argument.
#' @param break.case An optional logical value with default \code{TRUE}
#'   indicating if the switch-case construct will be left after the condition of
#'   one of the alternative is fulfilled and this alternative's code has been
#'   executed. This can be overwritten with the fourth element of an alternative
#'   'case' branch produced with the \code{\link{alt}()} function and provided
#'   via the \code{...} argument.
#'
#' @details The \code{expr} argument is the expression that is evaluated and
#'   tested. Using the \code{..expr} placeholder, this expression can be used in
#'   the first element (the test condition) of an alternative branch provided
#'   via the \code{...} argument. If, for example, the first element of an
#'   alternative branch is \code{..expr > 10} then the condition of this
#'   alternative 'case' will be met if the value of \code{swichCase()}'s
#'   \code{expr} argument is larger than \code{10}. Each alternative 'case'
#'   branch can return a value if its condition is fulfilled. This value needs
#'   to be provided as the third element of the alternative 'case' branch (the
#'   third argument of the \code{\link{alt}()} function that is used to create
#'   alternative branches) and is \code{NULL} by default resulting in no return
#'   whatsoever. If multiple branches are executed (e.g. because
#'   \code{break.case = FALSE}) and want to return a value then only the 'case'
#'   branch that is execxuted \emph{last} will return its value. A default
#'   'case' which is executed if no other alternative branch of the switch-case
#'   statement is applicable can be modeled by setting the first element of an
#'   alternative (which would normally contain the test condition) to
#'   \code{NULL}. If multiple defaults are found, only the first one is
#'   executed. Technical note: All code is executed in the enviroment from which
#'   \code{switchCase()} is called so it is easy to access variables and
#'   functions from there.
#'
#' @return The return value of the 'case' alternative that is executed last. If
#'   there is no applicable alternative or no applicable alternative returns a
#'   value then nothing is retuned.
#'
#' @family switchcase
#' @export
#'
#' @examples
#'
#' # Calculation and interpretation of a person's body-mass index (mass in kilograms)
#' bmi <- function(mass, size) {
#' ind <- mass / size^2
#' switchCase(
#'   ind,
#'   alt(
#'     ..expr <= 15,
#'     { cat("Your body mass index is ", ind, " which is very severely underweight.\n") },
#'     "very severely underweight"
#'   ),
#'   alt(
#'     ..expr > 15 & ..expr <= 16,
#'     { cat("Your body mass index is ", ind, " which is severely underweight.\n") },
#'   "severely underweight"
#'   ),
#'   alt(
#'     ..expr > 16 & ..expr <= 18.5,
#'     { cat("Your body mass index is ", ind, " which is underweight.\n") },
#'     "underweight"
#'   ),
#'   alt(
#'     ..expr > 18.5 & ..expr <= 25,
#'     { cat("Your body mass index is ", ind, " which is normal.\n") },
#'     "normal"
#'   ),
#'   alt(
#'     ..expr > 25 & ..expr <= 30,
#'     { cat("Your body mass index is ", ind, " which is overweight.\n") },
#'     "overweight"
#'   ),
#'   alt(
#'     ..expr > 30 & ..expr <= 35,
#'     { cat("Your body mass index is ", ind, " which is moderately obese.\n") },
#'     "moderately obese"
#'   ),
#'   alt(
#'     ..expr > 35 & ..expr <= 40,
#'     { cat("Your body mass index is ", ind, " which is severely obese.\n") },
#'     "severly obese"
#'   ),
#'   alt(
#'     ..expr > 40,
#'     { cat("Your body mass index is ", ind, " which is Very severely obese.\n") },
#'     "very severly obese"
#'   )
#'  )
#' }
#' bmi.person1 <- bmi(82.5, 1.79)
#' cat("Person1 turned out to be ", bmi.person1)
switchCase <- function(expr, ..., break.case = TRUE) {
  args <- list(...)

  ret.value = NULL
  cond.true <- FALSE
  f <- function(lst) { return(is.null(lst[[1]])) }

  expr <- eval(expr, envir = parent.frame())
  i <- 1
  exit <- FALSE
  while(i <= length(args) & !exit) {
    if(!is.null(args[[i]][[1]])) {
      # cond <- stringr::str_replace_all(deparse(args[[i]][[1]]), "\\.\\.expr", as.character(expr))
      cond <- gsub("\\.\\.expr", as.character(expr), deparse(args[[i]][[1]]))[1]
      if(eval(parse(text = cond))) {
        if(!is.null(args[[i]][[2]])) {
          if(!is.null(args[[i]][[2]])) eval(args[[i]][[2]], envir = parent.frame())
          # return value if required
          if(!is.null(args[[i]][[3]])) ret.value <- args[[i]][[3]]
          # note that at least one condition has been met, so no default situation
          cond.true <- TRUE
          # break if required
          if(ifnull(args[[i]][[4]]) | (break.case == TRUE & is.null(args[[i]][[4]]))) {
            invisible(ret.value)
            exit = TRUE
          }
        }
      }
    }

    i <- i + 1
  }

  if(!cond.true) {
    defs <- unlist(lapply(args, f))
    if(sum(defs) > 0) {
      ind <- which(defs)[1]
      eval(args[[ind]][[2]])
    }
  }
  invisible(ret.value)
}



#' @title Building alterantive 'case' branches for the switch-case construct
#'
#' @description The \code{\link{alt}()} function is used to build alternative 'case'
#'   branches for the switch-case construct. Each alternative branch consist of
#'   four elements: The test condition, the code that is executed if the
#'   condition evaluates to \code{TRUE}, an optional return value which
#'   \code{\link{switchCase}()} will return if the condition is met and after
#'   the branch code has ben executed, and an optional logical value indicating
#'   if the switch-case construct will be left after this branch has been
#'   executed.
#'
#' @param condition The condition that the test expression (\code{expr} argument
#'   in the call of the \code{\link{switchCase}()} function) needs to fulfill.
#' @param code.if The code that is executed if the condition is met. If the code stretches
#'   over multiple R statements then use curly brackets to
#'   enclose the whole code segment.
#' @param return.val A value that is returned if the condition is met and after
#'   the code in \code{code.if} has been executed. See
#'   \code{\link{switchCase}()} for more details on return values.
#' @param break.case A logical value indicating if the switch-case construct
#'   will be left after the condition of this alternative 'case' branch of the
#'   switch-case construct has been met and the code in \code{code.if} has been
#'   executed. Default is \code{TRUE}, i.e. exiting the switch-case construct
#'   after this branch. See \code{\link{switchCase}()} for more details on
#'   breaking out of a switch-case construct.
#'
#' @return An alternative branch that can be used in a switch-case construct
#'   (technically, a \code{list} object).
#'
#' @family switchcase
#' @export
#'
#' @examples
#' alt(.(..expr > 10), .(cat("The result is larger than 10.")), "10plus")
alt <- function(condition, code.if, return.val = NULL, break.case = TRUE) {
  return(list(substitute(condition), substitute(code.if), return.val, break.case))
}
