enrichGenes <- function(genes, annotationFile) {
	library(WGCNA)
	geneFile <- tempfile("geneList", fileext = ".txt")
	on.exit(unlink(geneFile))
	inputGenes <- c("TESTLIST1")
	genes <- unlist(genes)
	inputGenes <- append(inputGenes, genes)
	writeLines(inputGenes, geneFile)
	BrainLists=read.table(file=annotationFile,header=TRUE,sep="\t")
	geneR=BrainLists[,2]
	genes = intersect(genes, geneR)
	if(length(genes) < 2) {
		return("ERROR: Gene list has less than two pathways.")
	}
	labelR=BrainLists[,1]
	TestResults = userListEnrichment(geneR, labelR=labelR, fnIn=c(geneFile), catNmIn=c("TEST1"), nameOut = "testEnrichment.csv",outputCorrectedPvalues=TRUE)
	pathwayIndex <- TestResults$sigOverlaps[,1]
	pathways = c()
	overlaps = c()
	for(i in pathwayIndex) {
		pathways = append(pathways, TestResults$ovGenes[as.numeric(i)])
		overlaps = append(overlaps, paste(unlist(lapply(TestResults$ovGenes[as.numeric(i)][1], as.character)), collapse=","))
	}
	pathwayNames = names(pathways)
	pvalues <- TestResults$sigOverlaps[,4]
	results <- data.frame(pathwayNames, pvalues, overlaps)
	return(results)
}