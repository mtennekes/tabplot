markLabels <- function(marks, brokenX) {
	nonzero <- marks!=0
	
	diff <- min(marks[nonzero][-1] - marks[nonzero][-sum(nonzero)])
	(minID <- floor(round(log10(diff), digits=5)))
	(maxID <- floor(log10(max(abs(marks)))))
	
	
	if ((minID==0 && maxID<=3) || (minID>=-2 && maxID<=2 && maxID-minID <=2)) {
		scientific <- FALSE
		add_intercept <- FALSE
	} else {
		scientific <- TRUE
		add_intercept <- (maxID - minID > 2) 
	}
	
	if (add_intercept && brokenX!=0) {
		smallest <- min(abs(marks[nonzero]))
		intercept <- sign(min(marks[nonzero])) * floor(smallest / 10^(minID+1)) * 10^(minID+1)
		marks[nonzero] <- marks[nonzero] - intercept
	} else intercept <- 0
	
	if (scientific) {
		(maxID2 <- floor(log10(max(abs(marks)))))
		step <- 10^maxID2
		marks <- marks / step
	} else step <- 1
	
	stepLabel <- formatC(step, digits=0,format="e")
	stepLabel <- paste("x", stepLabel)
	
	interceptLabel <- format(intercept)
	
	interceptLabel <- ifelse(substr(interceptLabel, 1, 1)=="-", 
							paste("-", substr(interceptLabel, 2, nchar(interceptLabel))),
							paste("+", interceptLabel))
	
	list(markLabels = format(marks, trim=TRUE),
		 stepLabel = ifelse(step==1, "", stepLabel),
		 interceptLabel = ifelse(intercept==0, "", interceptLabel))
}
# markLabels(marks=c(0, 5000, 10000, 15000), brokenX=0)
# markLabels(marks=c(0, 0.00995, 0.01000, 0.01005), brokenX=1)
# markLabels(marks=c(0, 0.005, 0.010, 0.015), brokenX=0)
# markLabels(marks=c(-0.015, -0.010, -0.005, 0), brokenX=0)
# markLabels(marks=c(0, 65.034, 65.035, 65.036), brokenX=1)
# markLabels(marks=c(0, 0.034, 0.035, 0.036), brokenX=1)
# markLabels(marks=c(0, 1500, 3000), brokenX=0)
# markLabels(marks=c(0, 4000000, 4300000, 4600000), brokenX=1)
# markLabels(marks=c(0, 4000010, 4000020, 4000030), brokenX=1)
# markLabels(marks=c(-100040, -100020, -100000, 0), brokenX=-1)
# markLabels(marks=c(0, 61.5, 62), brokenX=1)
# markLabels(marks=c(0, 1, 2), brokenX=1)
# markLabels(marks=c(0, 651.5, 652), brokenX=1)
