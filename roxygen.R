## create documentation
require(roxygen2)
options(error=traceback)
unlink( 'man', TRUE)

roxygenize( '.'
			, roxygen.dir='.'
			, copy.package=FALSE
			, unlink.target=TRUE
)