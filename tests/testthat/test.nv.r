context("Run GBA")

test_that("GBA runs as expected", {

	data <- cbind(geneA=c("gene1","gene2","gene4", "gene5"),geneB=c("gene3","gene3","gene6", "gene6"))
	genelist <- c("gene1","gene2","gene3", "gene4","gene5","gene6")
	network <- make_gene_network(data,genelist)
	ontology <- cbind(genes=c("gene1","gene1","gene2","gene3","gene3","gene3", "gene4", "gene5", "gene6"),
            GO=c("GO:1","GO:3","GO:2","GO:1","GO:2","GO:3", "GO:3", "GO:3", "GO:3"))
	goterms <- c("GO:1","GO:2","GO:3")

	# create interaction and annotation matrix
	labels <- make_annotations(ontology,genelist,goterms)

	GBA_analysis <- run_GBA(network,labels, min=0, max=1000)
        roc.scores <- neighbor_voting(labels,network,3)

  })

