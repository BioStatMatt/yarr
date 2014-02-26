#  yarr - Mixing R Output with Text
#
#  Copyright (C) 2013 Matthew S. Shotwell
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

# The and capture_handler_classic function is an Sweave-like function that
# returns a string that appears as if the code were evaluated at the R prompt.
# These function is configurable. The 'output' parameter indicates whether the
# returned string should contain anything other than errors, warnings, or
# messages. The 'source' parameter indicates whether the returned string should
# contain the code that generated output. The 'prompt' argument indicates
# whether the R prompt should be printed (i.e., '>') before code. Clearly,
# 'prompt' has no effect when 'source' is FALSE, and neither 'source' nor
# 'prompt' have an effect when 'output' is FALSE.

capture_handler <-
function(code, envir, output=TRUE, source=TRUE, prompt=TRUE, indent='') {
    code <- sub('^@','',code)
    exp <- parse(text=code, keep.source=TRUE)
    #use this to get source text: getSrcLines(attr(exp, 'srcfile'), a, b)
    if(length(exp) == 0) return(out)
    out <- ''
    #scl <- 1 # current source line
    for(i in 1:length(exp)) {
        #stl <- attr(exp[i], 'srcref')[[1]][3]
        #dep <- getSrcLines(attr(exp, 'srcfile'), scl, stl)
        #scl <- stl + 1
        dep <- deparse(exp[[i]])
        res <- capture.output(eval(exp[i],envir))
        if(output) {
            if(source)
                out <- paste0(out, line_fmt(dep, prompt, indent))
            if(length(res)>0) {
                res <- paste0(indent, res, collapse=paste0('\n', indent))
                #if(!grepl('\\n$', res))
                    res <- paste0(res, '\n')
                out <- paste(out, res, sep='')
            }
        }
    }
    out <- sub('\\n$', '', out)
    return(out)
}

# Return errors, warnings, and messages
silent_handler <- function(code, envir) {
    capture_handler(code, envir, output=FALSE)
}

# Return R output, including errors, warnings, and messages, but not code
result_handler <- function(code, envir, indent='') {
    code <- sub('^=','',code)
    capture_handler(code, envir, source=FALSE, indent=indent)
}

# Return R output, including errors, warnings, messages, and code, but don't
# print the R prompt. This is useful, for example, when the code will be copied
# and pasted from an email or webpage into the R interpreter.
source_handler <- function(code, envir, indent='') {
    code <- sub('^&','',code)
    capture_handler(code, envir, prompt=FALSE, indent=indent)
}

# Collapse lines into a single string, inserting linebreaks and, if 'prompt' is
# TRUE, the R prompt. FIXME: should the linebreak characters be extracted from
# options()?
line_fmt <- function(x, prompt=TRUE, indent='') {
    lines <- unlist(strsplit(x, '\n'))
    if(prompt) {
        lines[1] <- paste(indent, options('prompt'), lines[1], sep='')
        if(length(lines) > 1)
            lines[-1] <- paste(indent, options('continue'), lines[-1], sep='')
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

# Same as result_handler, but indent by two spaces
idnt_result_handler <- function(code, envir) {
  code <- sub('^>','',code)
  result_handler(code,envir,indent='  ')
}

# Same as source_handler, but indent by two spaces
idnt_source_handler <- function(code, envir) {
  code <- sub('^>','',code)
  source_handler(code,envir,indent='  ')
}

# Same as capture_handler, but indent by two spaces
idnt_capture_handler <- function(code, envir) {
  code <- sub('^>','',code)
  capture_handler(code,envir,indent='  ')
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
    handlers[[8]] <- list(regex='^>=',handler=idnt_result_handler)
    handlers[[9]] <- list(regex='^>&',handler=idnt_source_handler)
    handlers[[10]] <- list(regex='^>@',handler=idnt_capture_handler)
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
        stop("no handler found")
    # Search for handler
    for(type in handlers) 
        if(grepl(type$regex, code))
            hdl <- type$handler
    # Call handler
    try(as.character(hdl(code, envir)), silent=TRUE)
}

yarr <- function(file=stdin(),envir=parent.frame(),output=stdout(),
         delim=default_delim(),handlers=default_handlers()) {

    closeIcon <- closeOcon <- FALSE

    # Check file
    if (inherits(file,'connection') && isOpen(file,"read")) {
        icon <- file
    } else if (is.character(file)) {
        closeIcon <- TRUE
        icon <- file(file,open="rt")
    } else {
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
        # FIXME there is no easy way to read a single line without 
        # removing the EOL character(s); one possibility is to read
        # one character at a time with readChar()
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
