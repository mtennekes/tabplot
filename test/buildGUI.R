  require(canvas, quietly=TRUE, warn=FALSE) ## require quietly


  ## Make a simple graph
  f <- tempfile() ## A file to store the javascript code
  canvas(file=f, width=1024, height=768)
require(grid)
vp <- viewport(width=unit(4, "inches"), height=unit(2, "inches"))
pushViewport(vp)
tableplot(iris)
  dev.off()  ## write javascript code to f

  w <- gwindow("Canvas test")
  g <- ggroup(cont = w, horiz=F)
  cv <- gcanvas(f,cont = g)  ## when f is given, graph an initial graph
                             ## is drawn
  ## How to update the graphic using the svalue method
  b <- gbutton("click me", cont = g, handler = function(h,...) {
    canvas(file=f, width=1024, height=1024, bg="#ffffff") ## specify a
                                                        ## background color
    tableplot(iris)
    dev.off()
    svalue(cv) <- f
  })
  
  visible(w) <- TRUE