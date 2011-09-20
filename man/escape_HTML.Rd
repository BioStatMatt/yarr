\name{escape_HTML}
\alias{escape_HTML}
\title{Escape Reserved HTML Characters}
\description{
    The \code{escape_HTML} function escapes reserved HTML characters from
    a character vector.
}
\usage{
escape_HTML(text)
}
\arguments{
  \item{text}{A character vector.}
}
\details{
    
    The \code{escape_HTML} function escapes the reserved HTML characters
    from a character vector. The HTML reserved characters "&", "\"",
    "'", ">", and "<" are replaced by "&amp;", "&quot;", "&apos;", "&gt;",
    and "&lt;" respectively.  
}
\value{
    A character vector.
}
\author{ Matt Shotwell <Matt.Shotwell@Vanderbilt.edu> }
\examples{
    escape_HTML("x <- pi")
    escape_HTML("famous \"quotes\"")
}
\keyword{utilities}
