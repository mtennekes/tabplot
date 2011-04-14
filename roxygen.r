library(roxygen)

unlink('tabplot/man', TRUE)

roxygenize( 'tabplot'
          , roxygen.dir='tabplot'
          , copy.package=FALSE
          , unlink.target=TRUE
          , use.Rd2 = TRUE
		    )

