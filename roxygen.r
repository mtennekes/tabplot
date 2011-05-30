library(roxygen)

unlink('pkg/man', TRUE)

roxygenize( 'pkg'
          , roxygen.dir='pkg'
          , copy.package=FALSE
          , unlink.target=TRUE
          , use.Rd2 = TRUE
		    )

if (length(list.files('pkg/inst/doc')) == 0){
   unlink( 'pkg/inst/doc', TRUE)   
}
