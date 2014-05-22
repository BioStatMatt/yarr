rst <- function(object, ...)
  UseMethod("rst")

rst.data.frame <- 
function(object, rownames=TRUE, ...) {
  #esc <- function (text) {
  #  text <- gsub("&", "&amp;", text)
  #  text <- gsub("\"", "&quot;", text)
  #  text <- gsub("'", "&apos;", text)
  #  text <- gsub(">", "&gt;", text)
  #  gsub("<", "&lt;", text)
  #}
  row_count <- nrow(object)
  col_count <- ncol(object)
  row_names <- row.names(object)
  col_names <- names(object)
  for(i in 1:col_count)
    object[[i]] <- format(object[[i]], ...)
  rnm_width <- max(c(3,nchar(row_names)))
  col_width <- sapply(1:col_count, function(i)
    max(nchar(c(col_names[i], object[,i]))))
  row_names <- format(row_names, width=rnm_width)
  for(i in 1:col_count) {
    col_names[i] <- format(col_names[i], width=col_width[i])
    object[,i] <- format(object[,i], width=col_width[i])
  }
  if(rownames) {
    sep_string <-
      paste(c(paste(rep("=", rnm_width), collapse=""),            
        sapply(col_width, function(w)
          paste(rep("=", w), collapse=""))), collapse=" ")
  } else {
    sep_string <-
      paste(sapply(col_width, function(w)
        paste(rep("=", w), collapse="")), collapse=" ")
  }
  cat(sep_string, "\n", sep="")
  cat(ifelse(rownames, paste0(format("Row", width=rnm_width), " "), ""),
      paste(col_names, collapse=" "), "\n", sep="")
  cat(sep_string, "\n", sep="")
  for(i in 1:row_count)
    cat(ifelse(rownames, paste(row_names[i], " ", sep="", collapse=""), ""),
        paste(object[i,], collapse=" "), "\n", sep="")
  cat(sep_string, "\n", sep="")
}

rst.basegr <- function(expr,
                       basename=deparse(substitute(expr)),
                       basepath="html/",
                       baseurl="",
                       pngd=c(500,500),
                       pdfd=c(7,7)) {
  pngfile <- paste(basepath, basename, '.png', sep='')
  pngurl  <- paste(baseurl, basename, '.png', sep='')
  png(file=pngfile, height=pngd[1], width=pngd[2],
      type="cairo")
  eval(expr)
  dev.off()
  pdffile <- paste(basepath, basename, '.pdf', sep='')
  pdfurl  <- paste(baseurl, basename, '.pdf', sep='')
  pdf(file=pdffile, height=pdfd[1], width=pdfd[2])
  eval(expr)
  dev.off()
  cat('\nClick the image below for a high resolution (PDF) version.\n\n')
  cat('.. image:: ', pngurl, '\n')
  cat('   :target: ', pdfurl, '\n')
  if(pngd[2] > 800)
    cat('   :width: 800px')
  cat("\n\n")
}

mtexti <- function(text, side, off = 0.25,
                   srt = if(side == 2) 90  else
                     if(side == 4) 270 else 0, ...) {
  # dimensions of plotting region in user units
  usr <- par('usr')
  # dimensions of plotting region in inches
  pin <- par('pin')
  # user units per inch
  upi <- c(usr[2]-usr[1],
           usr[4]-usr[3]) / pin
  # default x and y positions
  xpos <- (usr[1] + usr[2])/2
  ypos <- (usr[3] + usr[4])/2
  if(1 == side)
    ypos <- usr[3] - upi[2] * off
  if(2 == side)
    xpos <- usr[1] - upi[1] * off
  if(3 == side)
    ypos <- usr[4] + upi[2] * off
  if(4 == side)
    xpos <- usr[2] + upi[1] * off
  text(x=xpos, y=ypos, text, xpd=NA, srt=srt, ...)
}html.data.frame <- function(object, header='', footer='',
    rownames=TRUE, fontsize=10, ...) {
    esc <- function (text) {
        text <- gsub("&", "&amp;", text)
        text <- gsub("\"", "&quot;", text)
        text <- gsub("'", "&apos;", text)
        text <- gsub(">", "&gt;", text)
        gsub("<", "&lt;", text)
    }
    row_count <- nrow(object)
    col_count <- ncol(object)
    row_names <- gsub(" ", "&nbsp;", esc(row.names(object)))
    col_names <- gsub(" ", "&nbsp;", esc(names(object)))
    cat(header, "<table style=\"font-size:",fontsize,"pt;\"><tr><th></th><th>",
         paste(col_names, collapse="</th><th>"), "</th></tr>", sep="")
    evenodd <- "e"
    for(i in 1:row_count) {
        cat("<tr class=\"",evenodd,"\"><td>", 
            ifelse(rownames, row_names[i], ""),
            "</td><td>", paste(format(object[i,], ...),
            collapse = "</td><td>"), "</td></tr>", sep="")
        evenodd <- ifelse(evenodd=="e", "o", "e")
    }
    if(!is.null(attr(object, 'caption')))
        cat("<tr class=\"c\"><td colspan=\"",col_count+1,"\">",
            attr(object, 'caption'),"</td></tr>", sep="")
    cat("</table>",footer)
}

html.trellis <- function(object, basename=deparse(substitute(object)),
    basepath="html/cache/", baseurl="cache/", pngd=c(500,500), pdfd=c(7,7)) {
    basename <- gsub('[[:punct:]][[:space:]]', '', basename)
    pngfile <- paste(basepath, basename, '.png', sep='')
    pngurl  <- paste(baseurl, basename, '.png', sep='')
    trellis.device(png, file=pngfile, height=pngd[1], width=pngd[2],
        type="cairo")
    print(object)
    dev.off()
    pdffile <- paste(basepath, basename, '.pdf', sep='')
    pdfurl  <- paste(baseurl, basename, '.pdf', sep='')
    trellis.device(pdf, file=pdffile, height=pdfd[1], width=pdfd[2])
    print(object)
    dev.off()
    cat('<a href="', pdfurl, '"><img class="r" src="', pngurl,
        if (pngd[2] > 800) 'width="800"></a>' else '"></a>', sep='')
}

html.basegr <- function(expr, basename=deparse(substitute(object)),
    basepath="html/cache/", baseurl="cache/", pngd=c(500,500), pdfd=c(7,7)) {
    pngfile <- paste(basepath, basename, '.png', sep='')
    pngurl  <- paste(baseurl, basename, '.png', sep='')
    png(file=pngfile, height=pngd[1], width=pngd[2],
        type="cairo")
    eval(expr)
    dev.off()
    pdffile <- paste(basepath, basename, '.pdf', sep='')
    pdfurl  <- paste(baseurl, basename, '.pdf', sep='')
    pdf(file=pdffile, height=pdfd[1], width=pdfd[2])
    eval(expr)
    dev.off()
    cat('<a href="', pdfurl, '"><img class="r" src="', pngurl,
       if (pngd[2] > 800) 'width="800"></a>' else '"></a>', sep='')
}
    
