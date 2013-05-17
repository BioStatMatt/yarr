#  yarr - Mixing R Output with Text
#
#  Copyright (C) 2012 Matthew S. Shotwell
#
#  yarr is free software; licensed under the terms of the GNU General Public
#  License; either version 2 of the License, or any later version.
#  http://www.gnu.org
#
#  This program is distributed WITHOUT ANY WARRANTY nor the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

# Escape reserved HTML characters
escape_HTML <- function(text) {
    text <- gsub('&', '&amp;', text)
    text <- gsub('"', '&quot;', text)
    text <- gsub("'", '&apos;', text)
    text <- gsub('>', '&gt;', text)
            gsub('<', '&lt;', text)
}

# Code handlers with prototype: function(code, envir, ...), where code is a
# character vector of length one, and envir is the environment where evaluation
# should occur. By convention, handlers strip trailing newlines.

# The capture_handler_evaluate and capture_handler_classic functions
# are Sweave-like functions that return a string that appears as if the code
# were evaluated at the R prompt. The _evaluate version uses the 'evaluate'
# package, whereas the _classic version does not. These functions are 
# configurable. The 'output' parameter indicates whether the returned string
# should contain anything other than errors, warnings, or messages. The
# 'source' parameter indicates whether the returned string should contain the
# code that generated output. The 'prompt' argument indicates whether the R
# prompt should be printed (i.e., '>') before code. Clearly, 'prompt' has no
# effect when 'source' is FALSE, and neither 'source' nor 'prompt' have an
# effect when 'output' is FALSE.
capture_handler_evaluate <-
function(code, envir, output=TRUE, source=TRUE, prompt=TRUE) {
    require("evaluate", quietly=TRUE)
    code <- sub('^@','',code)
    code <- sub('^\\n','',code)
    res <- evaluate(code, envir)
    out <- ''
    for(val in res) {
        if(is.character(val) && output)
            out <- paste(out,val,sep='',collapse='')
        if(is.source(val) && output && source)
            out <- paste(out,line_fmt(val$src, prompt),sep='',collapse='')
        if(is.warning(val))
            out <- paste(out,'Warning: ',val$message,sep='')
        if(is.message(val))
            out <- paste(out,'Message: ',val$message,sep='')
        if(is.error(val))
            out <- paste(out,'Error: ',val$message,sep='')
        if(is.recordedplot(val))
            assign('.recordedplot',val,envir=envir)
    }
    out <- sub('\\n$','',out)
    return(out)
}

capture_handler_classic <-
function(code, envir, output=TRUE, source=TRUE, prompt=TRUE) {
    code <- sub('^@','',code)
    exp <- try(parse(text=code),TRUE)
    if(inherits(exp, 'try-error'))
        return(exp) 
    out <- ''
    if(length(exp) == 0) return(out)
    for(i in 1:length(exp)) {
        dep <- deparse(exp[[i]])
        res <- try(capture.output(eval(exp[i],envir)),TRUE)
        if(output) {
            if(source)
                out <- paste(out, line_fmt(dep, prompt), sep='')
            if(length(res)>0) {
                res <- paste(res, collapse='\n', sep='')
                #if(!grepl('\\n$', res))
                    res <- paste(res, '\n', sep='')
                out <- paste(out, res, sep='')
            }
        }
    }
    out <- sub('\\n$', '', out)
    return(out)
}

# The capture_handler_evaluate function is used when the 'evaluate' package is
# installed
capture_handler <-
function(code, envir, output=TRUE, source=TRUE, prompt=TRUE) {
    if(suppressWarnings(require("evaluate", quietly=TRUE))) {
        capture_handler_evaluate(code, envir, output, source, prompt)
    } else {
        capture_handler_classic(code, envir, output, source, prompt)
    }
}

# Return errors, warnings, and messages
silent_handler <- function(code, envir) {
    capture_handler(code, envir, output=FALSE)
}

# Return R output, including errors, warnings, and messages, but not code
result_handler <- function(code, envir) {
    code <- sub('^=','',code)
    capture_handler(code, envir, source=FALSE)
}

# Return R output, including errors, warnings, messages, and code, but don't
# print the R prompt. This is useful, for example, when the code will be copied
# and pasted from an email or webpage into the R interpreter.
source_handler <- function(code, envir) {
    code <- sub('^&','',code)
    capture_handler(code, envir, prompt=FALSE)
}

# Collapse lines into a single string, inserting linebreaks and, if 'prompt' is
# TRUE, the R prompt. FIXME: should the linebreak characters be extracted from
# options()?
line_fmt <- function(x, prompt=TRUE) {
    lines <- unlist(strsplit(x, '\n'))
    if(prompt) {
        lines[1] <- paste(options('prompt'), lines[1], sep='')
        if(length(lines) > 1)
            lines[-1] <- paste(options('continue'), lines[-1], sep='')
    }
    paste(lines, '\n', collapse='', sep='')
}

# Same as result_handler, but escape special HTML characters
html_result_handler <- function(code, envir) {
    code <- sub('^/','',code)
    escape_HTML(result_handler(code,envir))
}

# Same as source_handler, but escape special HTML characters
html_source_handler <- function(code, envir) {
    code <- sub('^/','',code)
    escape_HTML(source_handler(code,envir))
}

# Same as capture_handler, but escape special HTML characters
html_capture_handler <- function(code, envir) {
    code <- sub('^/','',code)
    escape_HTML(capture_handler(code,envir))
}

# This structure is an example of how the 'handlers' argument to yarr should
# be constructed. This is the default collection of code handlers.
default_handlers <- function() {
    handlers <- list()
    handlers[[1]] <- list(regex='',handler=silent_handler)
    handlers[[2]] <- list(regex='^=',handler=result_handler)
    handlers[[3]] <- list(regex='^&',handler=source_handler)
    handlers[[4]] <- list(regex='^@',handler=capture_handler)
    handlers[[5]] <- list(regex='^/=',handler=html_result_handler)
    handlers[[6]] <- list(regex='^/&',handler=html_source_handler)
    handlers[[7]] <- list(regex='^/@',handler=html_capture_handler)
    return(handlers)
}

# Other symbols that are not syntactically valid at the 
# beginning of an expression: '&', '*', '%', '<' '>'

# This closing delimiter regex permits removal of trailing
# newlines in the style of the brew package.
default_delim <- function() c('<<', '>>|->>|->>\n')

# The dispatch function matches delimited code, *excluding the delimiter*,
# against the collection of code handlers.
dispatch <- function(code, envir) {
    handlers <- list()
    if(exists(".handlers", envir))
        handlers <- get(".handlers", envir)
    hdl <- function(code, envir) 
        return("Error: no handler found")
    # Search for handler
    for(type in handlers) 
        if(grepl(type$regex, code))
            hdl <- type$handler
    # Call handler
    try(as.character(hdl(code, envir)),TRUE)
}

yarr <- function(file=stdin(),envir=parent.frame(),output=stdout(),text=NULL,
         delim=default_delim(),handlers=default_handlers()) {

    closeIcon <- closeOcon <- FALSE

    # Check file, text
    if (is.character(text) && length(text) > 0) {
        closeIcon <- TRUE
        icon <- textConnection(text)
    } else if (inherits(file,'connection') && isOpen(file,"read")) {
        icon <- file
    } else if (is.character(file)) {
        closeIcon <- TRUE
        icon <- file(file,open="rt")
    } else {
        if(!is.null(text))
            stop("\'text\' must be NULL or non-empty character vector")
        stop("\'file\' is not valid")
    }

    # Check output
    if (inherits(output,'connection') && isOpen(output,"write")) {
        ocon <- output
    } else if ( is.character(output) ) {
        closeOcon <- TRUE
        ocon <- file(output,open="wt")
    } else {
        stop("\'output\' is not valid")
    }

    # Check envir
    if (!is.environment(envir))
        stop("\'envir\' is not an environment")

    # Check delim
    # FIXME need to check for identical opening and closing delimiters?
    if(!is.character(delim) || length(delim) != 2)
        stop("\'delim\' must be a character vector of length two")
    if(nchar(delim[1]) < 1 || nchar(delim[2]) < 1)
        stop("\'delim\' items must not be empty")

    # Check handlers
    if(!is.list(handlers))
        stop("\'handlers\' must be a list")
    for(hdl in handlers) {
        if(!is.list(hdl))
            stop("each handler must be a list")
        if(!all(c("regex", "handler") %in% names(hdl)))
            stop("each handler must have elements \'regex\' and \'handler\'")
        if(!is.character(hdl$regex) || length(hdl$regex) != 1)
            stop("\'regex\' must be a character vector of length 1")
        if(!is.function(hdl$handler) || length(formals(hdl$handler)) < 2)
            stop("\'handler\' must be a function of two or more arguments")
    }

    # Make the handlers list available to code within the yarr document
    assign(".handlers", handlers, envir)

    # The 'status' variable indicates whether the parser is within a delimited
    # code block (status == YCODE), or a text block (status == YTEXT).
    YCODE  <- 0   
    YTEXT  <- status <- 1
    
    # The 'code' variable accumulates delimited code strings. The
    # value of 'code' is passed to the 'dispatch' function when the entire
    # delimited code block is parsed. The 'line' serves as a text buffer.
    code   <- line  <- '' 

    # The 'input' variable is a list of all text and code blocks within the 
    # yarr file. This variable is returned by the 'yarr' function invisibly.
    input  <- list()
 
    # Convenience function to extract "match.length"
    mlen   <- function(regex) attr(regex, "match.length")

    while(TRUE) {

        # Each iteration corresponds to a single item of either text
        # or code. Text items may be no longer than one line (i.e. up
        # to and including '\n'). However, code items may span multiple
        # lines. The 'indx' variable identifies the current 'input' index.
        indx <- length(input) + 1

        # Read line, add back '\n', break on EOF
        if(line == '') {
            line <- readLines(icon, 1)
            if(length(line) < 1) break
            line <- paste(line,'\n',sep='')
        }

        if(status == YTEXT) {
            # Look for opening delimiter
            odel <- regexpr(delim[1], line)
            input[[indx]] <- list(type='text')
            if(odel < 0) {
                input[[indx]]$data <- line
                line <- ''
            } else {
                input[[indx]]$data = substr(line, 1, odel-1)
                line <- substr(line, odel + mlen(odel), nchar(line))
                status <- YCODE
            } 
            writeLines(input[[indx]]$data, ocon, sep='')
            next
        }
        
        if(status == YCODE) {
            # Look for closing delimiter
            cdel <- regexpr(delim[2], line)
            if(cdel < 0) {
                code <- paste(code, line, sep='')
                line <- ''
            } else {
                code <- paste(code, substr(line, 1, cdel-1), sep='')
                input[[indx]] <- list(type='code',data=code)
                writeLines(dispatch(code, envir), ocon, sep='')
                code <- ''
                line <- substr(line, cdel + mlen(cdel), nchar(line))
                status <- YTEXT
            }
        }
    }
    
    if(closeIcon) close(icon)
    if(closeOcon) close(ocon)
    if(status == YCODE)
        warning(paste("file ended while scanning for closing delimiter '",
             delim[2], "'", sep=""))
    
    invisible(input)
}
