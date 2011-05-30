 # assign iris tableplot as object
 tab <- tableplot(iris, plot=FALSE)
 
 # modify the tabplot object: reverse order of columns and customize palette
 tab <- changeTabplot(tab, colNames=rev(names(iris)), pals=list(c("purple", "yellow", "blue")))
 
 # plot modified tabplot object
 plot(tab)