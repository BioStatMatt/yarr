\name{mkyarr}
\alias{mkyarr}
\title{Create a ReDoc Style \code{yarr} Compendium}
\description{
    The \code{mkyarr} function creates a directory structure for ReDoc
    style compendia that use \code{yarr} for mixing R output with text, 
    or markup (i.e., HTML).
}
\usage{
mkyarr(project, remote_host="", remote_user="",
       remote_path="", remote_url ="", verbose=FALSE)
}
\arguments{
  \item{project}{A character string, the project (and main directory) name}
  \item{remote_host}{A character string, address of remote server where HTML files will be hosted}
  \item{remote_user}{A character string, remote server user name}
  \item{remote_path}{A character string, remote server path for hosted HTML files}
  \item{remote_url}{A character string, remote server URL for hosted HTML files}
  \item{verbose}{A logical, whether \code{mkyarr} should be verbose about the files created}
}
\details{
    
    The \code{mkyarr} function creates a directory structure and template files
    for projects that utilize a ReDoc (reproducible document) framework. The
    ReDoc framework uses the Linux utility \samp{make} to implement 
    \samp{build}, \samp{clean}, \samp{view}, and \samp{burn} procedures
    for a reproducible document. The \samp{build} procedure inputs source
    documents and renders an output document (an HTML file in this case). The
    \samp{clean} procedure removes ancillary files that may have been created
    during the build process. The \samp{view} procedure opens the output
    documents for viewing using the appropriate software (i.e., a web browser).
    And, the \samp{burn} procedure removes (deletes) output documents. These
    four procedures implement the classical ReDoc framework. An additional 
    procedure \samp{publish} is implemented here in order to publish HTML
    files to a remote web server. The \code{mkyarr} function creates a
    \samp{Makefile} with each of these five targets. The \samp{remote_*}
    arguments to \code{mkyarr} supply details related to the \samp{publish}
    target. These details and others (e.g., the external programs used to 
    synchronize files with the remote server, and to view web pages) may be
    modified by editing the resulting \samp{Makefile}.

    The \code{mkyarr} function creates the following directories and files:
    \enumerate{
    \item{\samp{<project>/}}{main project directory}
    \item{\samp{<project>/Makefile}}{ReDoc \samp{Makefile}}
    \item{\samp{<project>/yarr/}}{\samp{yarr} subdirectory}
    \item{\samp{<project>/yarr/index.html.R}}{template HTML \code{yarr} file}
    \item{\samp{<project>/html/}}{\samp{html} subdirectory}
    \item{\samp{<project>/html/cache/}}{\samp{html/cache} subdirectory}
    \item{\samp{<project>/html/style.css}}{HTML CSS style file}
    \item{\samp{<project>/data/}}{\samp{data} subdirectory}
    \item{\samp{<project>/data/cache/}}{\samp{data/cache} subdirectory}
    \item{\samp{<project>/docs/}}{\samp{docs} subdirectory}
    \item{\samp{<project>/R/}}{\R subdirectory}
    \item{\samp{<project>/R/common.R}}{common \R functions}
    }
}
\value{
    None
}
\author{ Matt Shotwell <Matt.Shotwell@Vanderbilt.edu> }
\examples{
    \dontrun{
        mkyarr("example")
        # from terminal
        # $ cd example
        # $ make build
        # $ make view
    }
}
\keyword{utilities}
