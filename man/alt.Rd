% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/switchcase.r
\name{alt}
\alias{alt}
\title{Building alternative 'case' branches for the switch-case construct}
\usage{
alt(condition, code.if, return.val = NULL, break.case = TRUE)
}
\arguments{
\item{condition}{The condition that the test expression (\code{expr} argument
in the call of the \code{\link{switchCase}()} function) needs to fulfill.}

\item{code.if}{The code that is executed if the condition is met. If the code stretches
over multiple R statements then use curly brackets to
enclose the whole code segment.}

\item{return.val}{A value that is returned if the condition is met and after
the code in \code{code.if} has been executed. See
\code{\link{switchCase}()} for more details on return values.}

\item{break.case}{A logical value indicating if the switch-case construct
will be left after the condition of this alternative 'case' branch of the
switch-case construct has been met and the code in \code{code.if} has been
executed. Default is \code{TRUE}, i.e. exiting the switch-case construct
after this branch. See \code{\link{switchCase}()} for more details on
breaking out of a switch-case construct.}
}
\value{
An alternative branch that can be used in a switch-case construct
  (technically, a \code{list} object).
}
\description{
The \code{\link{alt}()} function is used to build alternative 'case'
  branches for the switch-case construct. Each alternative branch consist of
  four elements: The test condition, the code that is executed if the
  condition evaluates to \code{TRUE}, an optional return value which
  \code{\link{switchCase}()} will return if the condition is met and after
  the branch code has been executed, and an optional logical value indicating
  if the switch-case construct will be left after this branch has been
  executed.
}
\examples{
alt(.(..expr > 10), .(cat("The result is larger than 10.")), "10plus")
}
\seealso{
Other switchcase: 
\code{\link{switchCase}()}
}
\concept{switchcase}
