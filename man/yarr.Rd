\name{yarr}
\alias{yarr}
\alias{default_delim}
\alias{default_handlers}
\alias{capture_handler}
\alias{capture_handler_evaluate}
\alias{silent_handler}
\alias{result_handler}
\alias{html_capture_handler}
\alias{html_result_handler}
\title{Mixing R Output with Text}
\description{
    The \code{yarr} function utilizes an extensible framework for
    mixing R code with text, including text containing markup 
    (e.g. LaTeX, HTML). 
}
\usage{
yarr(file=stdin(), envir=parent.frame(), output=stdout(), text=NULL,
     delim=default_delim(), handlers=default_handlers())
}
\arguments{
  \item{file}{A connection, or a character string naming the file
    to read. stdin() is the default.}
  \item{envir}{The \code{\link{environment}} where input should 
    be evaluated. Default is the caller's environment}
  \item{output}{A connection, or a character string naming the file
    to write. stdout() is the default.}
  \item{text}{A character string treated as if it contained lines of a file
    to read. Only one of \code{file} or \code{text} is used as input. 
    Default is \code{NULL}.}
  \item{delim}{A character vector with two elements, indcating the opening
    and closing delimiters (regular expressions) for code}
  \item{handlers}{A list of code handlers. Each elements is itself a list,
    consisting of the named elements \samp{regex} and \samp{handler}. The
    former should be a character string containing a regular expression to
    be matched against text delimited by \samp{delim}, but not including the
    delimiters, which are stripped before matching. If matching, the 
    delimited text is passed to the corresponding \samp{handler}, which should
    be a function with the prototype \samp{function(code, envir)}, where 
    \samp{code} is the delimited text and \samp{envir} is an environment where
    \R code evaluation should occur. Each \samp{handler} should return a character
    vector containing text to replace the markup text. If \samp{handlers} contains
    more than one element, delimited text is passed to the \samp{handler} 
    corresponding to the last matching \samp{regex} in sequence. If no
    matching handler is found, no output is generated.}
}
\details{
    
    The \code{yarr} function provides a mechanism to parse, evaluate, and
    substitute delimited code in plain text or documents with markup. The 
    delimited code is usually a complete \R expression, and the code handler
    typically outputs text generated in the process of evaluating the \R expression.

    The \code{yarr} package utilizes a default pair of delimiters and code
    handlers, inspired by the syntax used by \code{\link{Sweave}} and the 
    \code{brew} package. However, these are highly configurable.

    The default \code{yarr} delimiters and code handlers have the following
    effects on \R code delimited by the following delimiters

    \enumerate{
    \item{\code{'<<', '>>'}}{ - evaluate silently}
    \item{\code{'<<=', '>>'}}{ - evaluate and print output}
    \item{\code{'<</=', '>>'}}{ - additionally escape reserved HTML characters}
    \item{\code{'<<&', '>>'}}{ - evaluate and print code and output}
    \item{\code{'<</&', '>>'}}{ - additionally escape reserved HTML characters}
    \item{\code{'<<@', '>>'}}{ - evaluate and print prompted code and output}
    \item{\code{'<</@', '>>'}}{ - additionally escape reserved HTML characters}
    }

    Each of the code handlers corresponding to the delimiters above evaluate
    code in the environment specified by \samp{envir}. This evaluation 
    environment is propagated from one evaluation to the next. Hence, variables
    and functions declared by earlier expressions are available to subsequent
    code. 

    The \samp{handler} variables are stored in the \samp{envir} environment
    under the name \samp{.handler}. Modifications to this variable affects
    subsequent handler dispatch. Hence, the \code{yarr} syntax may be modified on
    the fly. The \samp{citation.yarr} file included with this package illustrates
    some advanced usage of this variable.

    If the \code{evaluate} package is installed and delimited code generates 
    graphical output, the last graphics state is stored in \samp{envir} with the
    symbol \samp{.recordedplot}. This object may also be accessed by subsequent code.
    The \samp{.recordedplot} variable is overwritten if subsequent code generates
    graphical output. If \code{evaluate} is not installed, graphics devices must
    be managed manually.

    The following document illustrates the default yarr syntax.

\preformatted{
---------------
Dear R-Help,
<<
# Function to format the date
d <- function() format(Sys.time(), "\%m-\%d-\%Y")
>>
I want write an email that contains R code and output, as if I 
had entered the code at the R prompt. For example:
<<@
f <- function(x) {
    x + 1
}
f(1)
stem(rnorm(50))
>>
Is there a way to do that without copy and pasting from R?
Also, is there a way to include the date (<<= d() >>) in the
text of my email?

Regards,
useR

<<@ sessionInfo() >>
---------------
}

    The default output of \code{yarr} is:

\preformatted{
--------------
Dear R-Help,

I want write an email that contains R code and output, as if I 
had entered the code at the R prompt. For example:
> f <- function(x) {
+     x + 1
+ }
> f(1)
[1] 2
> stem(rnorm(50))

  The decimal point is at the |

  -2 | 7
  -2 | 
  -1 | 8
  -1 | 44221000
  -0 | 998877555
  -0 | 422111000
   0 | 0122344
   0 | 6778899
   1 | 1112233
   1 | 6

Is there a way to do that without copy and pasting from R?
Also, is there a way to include the date (08-07-2010) in the
text of my email?

Regards,
useR

> sessionInfo() 
R version 2.11.1 (2010-05-31) 
i686-pc-linux-gnu 

locale:
 [1] LC_CTYPE=en_US.utf8       LC_NUMERIC=C             
 [3] LC_TIME=en_US.utf8        LC_COLLATE=en_US.utf8    
 [5] LC_MONETARY=C             LC_MESSAGES=en_US.utf8   
 [7] LC_PAPER=en_US.utf8       LC_NAME=C                
 [9] LC_ADDRESS=C              LC_TELEPHONE=C           
[11] LC_MEASUREMENT=en_US.utf8 LC_IDENTIFICATION=C      

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] evaluate_0.3 yarr_0.0    

loaded via a namespace (and not attached):
[1] plyr_1.1     stringr_0.4  tools_2.11.1
--------------
}
}
\value{
    None
}
\author{ Matt Shotwell <Matt.Shotwell@Vanderbilt.edu> }
\examples{
    yarr(system.file('email.yarr', package='yarr'))
    yarr(system.file('blog.yarr', package='yarr'))
    yarr(system.file('citation.yarr', package='yarr'))
}
\keyword{utilities}
