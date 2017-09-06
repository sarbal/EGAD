context("Multifunctinality assessments")

test_that("", {
        genelist <- c("gene1","gene2","gene3", "gene4","gene5","gene6")
	ontology <- cbind(genes=c("gene1","gene1","gene2","gene3","gene3","gene3", "gene4", "gene5", "gene6"),
            GO=c("GO:1","GO:3","GO:2","GO:1","GO:2","GO:3", "GO:3", "GO:3", "GO:3"))
	goterms <- c("GO:1","GO:2","GO:3")
	labels <- make_annotations(ontology,genelist,goterms)
	mf = calculate_multifunc(labels)
        optimallist_genes = as.numeric(mf[,4])

	AUC <- apply(labels,2,auroc_analytic,optimallist_genes)
	AUC2 <- sapply(1:dim(labels)[2], function(i) auroc_analytic( optimallist_genes,labels[,i]) )
  })

