context("Network checks")

test_that("Make networks acts as expected ", {

	data <- cbind(geneA=c("gene1","gene2","gene4", "gene5"),geneB=c("gene3","gene3","gene6", "gene6"))
	genelist <- c("gene1","gene2","gene3", "gene4","gene5","gene6")
	network <- make_gene_network(data,genelist)

        sym = isSymmetric(network)
	expect_equal( sym , TRUE )

	nd = colSums(network, na.rm=T)
	expect_equal( nd , node_degree(network) )
  })

test_that("Annotation functions acts as expected", {

	genelist <- c("gene1","gene2","gene3", "gene4","gene5","gene6")
	ontology <- cbind(genes=c("gene1","gene1","gene2","gene3","gene3","gene3", "gene4", "gene5", "gene6"),
            GO=c("GO:1","GO:3","GO:2","GO:1","GO:2","GO:3", "GO:3", "GO:3", "GO:3"))
	goterms <- c("GO:1","GO:2","GO:3")
	sizes = c(2,2,5)
	names(sizes) = goterms

	labels <- make_annotations(ontology,genelist,goterms)

	expect_equal( sizes , colSums(labels) )


  })

