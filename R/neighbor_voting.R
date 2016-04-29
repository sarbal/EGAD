#' Evaluating Gene Function Prediction
#'
#' The function performs gene function prediction based on 'guilt by association' 
#' using cross validation ([1]). Performance and significance are evaluated by 
#' calculating the AUROC or AUPRC of each functional group.
#' 
#' @param genes.labels numeric array
#' @param network numeric array symmetric, gene-by-gene matrix 
#' @param nFold numeric value, default is 3
#' @param output string, default is AUROC  
#' @param FLAG_DRAW binary flag to draw roc plot
#'
#' @return scores numeric matrix  
#' 
#' @keywords neighbor voting 
#' guilt by association 
#' gene function prediction evaluation 
#' cross validation
#'
#' @examples 
#' genes.labels <- matrix( sample( c(0,1), 1000, replace=TRUE), nrow=100)
#' rownames(genes.labels) = paste('gene', 1:100, sep='')
#' colnames(genes.labels) = paste('function', 1:10, sep='')
#' net <- cor( matrix( rnorm(10000), ncol=100), method='spearman')
#' rownames(net) <- paste('gene', 1:100, sep='')
#' colnames(net) <- paste('gene', 1:100, sep='')
#' 
#' aurocs <- neighbor_voting(genes.labels, net, output = 'AUROC') 
#' 
#' auprcs <- neighbor_voting(genes.labels, net, output = 'PR') 
#' 
#' @export
#'
#'  
neighbor_voting <- function(genes.labels, network, nFold = 3, output = "AUROC", FLAG_DRAW = FALSE) {
    
    genes.labels <- as.matrix(genes.labels)
    
    # Filter for common genes between network and labels
    ord <- order(rownames(network))
    network <- network[ord, ord]
    
    ord <- order(rownames(genes.labels))
    genes.labels <- as.matrix(genes.labels[ord, ])
    
    match.lab <- match(rownames(genes.labels), rownames(network))
    filt.lab <- !is.na(match.lab)
    filt.net <- match.lab[filt.lab]
    network <- network[filt.net, filt.net]
    genes.labels <- as.matrix(genes.labels[filt.lab, ])
    
    # genes.label : needs to be in 1s and 0s
    l <- dim(genes.labels)[2]
    g <- dim(genes.labels)[1]
    ab <- which(genes.labels != 0, arr.ind = TRUE)
    n <- length(ab[, 1])
    
    
    # print('Make genes label CV matrix')
    test.genes.labels <- matrix(genes.labels, nrow = g, ncol = nFold * l)
    
    # For each fold in each GO group, remove 1/nth of the values of the genes.label
    for (j in 1:l) {
        d <- which(ab[, 2] == j)  # Which indices the genes are in this particular GO group
        t <- length(d)  # Total number of genes in the GO group
        r <- sample(1:t, replace = FALSE)
        f <- t/nFold
        for (i in 1:nFold) {
            e <- c((1:f) + f * (i - 1))
            e <- sort(r[e])
            c <- j + l * (i - 1)  # GO group to look at (ie column)
            test.genes.labels[ab[d], c][e] <- 0
        }
    }
    
    # print('Get sums - mat. mul.') sumin = ( t(network) %*% test.genes.labels) sumin <- ((network)
    # %*% test.genes.labels)
    sumin <- crossprod(network, test.genes.labels)
    
    # print('Get sums - calc sumall')
    sumall <- matrix(apply(network, 2, sum), ncol = dim(sumin)[2], nrow = dim(sumin)[1])
    
    # print('Get sums - calc predicts')
    predicts <- sumin/sumall
    
    
    if (output == "AUROC") {
        # print('Hide training data')
        nans <- which(test.genes.labels == 1, arr.ind = TRUE)
        
        predicts[nans] <- NA
        
        # print('Rank test data')
        predicts <- apply(abs(predicts), 2, rank, na.last = "keep", ties.method = "average")
        
        filter <- matrix(genes.labels, nrow = g, ncol = nFold * l)
        negatives <- which(filter == 0, arr.ind = TRUE)
        positives <- which(filter == 1, arr.ind = TRUE)
        
        predicts[negatives] <- 0
        
        # print('Calculate ROC - np')
        np <- colSums(filter) - colSums(test.genes.labels)  # Postives
        
        # print('Calculate ROC - nn')
        nn <- dim(test.genes.labels)[1] - colSums(filter)  # Negatives
        
        # print('Calculate ROC - p')
        p <- apply(predicts, 2, sum, na.rm = TRUE)
        
        # print('Calculate ROC - rocN')
        rocN <- (p/np - (np + 1)/2)/nn
        rocN <- matrix(rocN, ncol = nFold, nrow = l)
        rocN <- rowMeans(rocN)
        
        # print('Calculate node degree')
        node_degree <- rowSums(network)
        colsums <- colSums(genes.labels)
        
        # print('Calculate node degree - sum across gene labels')
        node_degree <- matrix(node_degree)
        temp <- t(node_degree) %*% genes.labels
        
        
        # print('Calculate node degree - average')
        average_node_degree <- t(temp)/colsums
        
        # print('Calculate node degree roc - rank node degree')
        ranks <- apply(abs(node_degree), 2, rank, na.last = "keep", ties.method = "average")
        ranks <- matrix(ranks, nrow = length(ranks), ncol = dim(genes.labels)[2])
        
        # print('Calculate node degree roc - remove negatives')
        negatives <- which(genes.labels == 0, arr.ind = TRUE)
        ranks[negatives] <- 0
        
        # print('Calculate node degree roc - np')
        np <- colSums(genes.labels)
        
        # print('Calculate node degree roc - nn')
        nn <- dim(genes.labels)[1] - np
        
        # print('Calculate node degree roc - p')
        p <- apply(ranks, 2, sum, na.rm = TRUE)
        
        # print('Calculate node degree roc - roc')
        roc <- (p/np - (np + 1)/2)/nn
        
        if (FLAG_DRAW == TRUE) {
            plot_roc_overlay(predicts, test.genes.labels)
        }
        
        scores <- cbind(rocN, matrix(average_node_degree)[, 1], roc)
    } else if (output == "PR") {
        
        
        # print('Rank test data')
        predicts <- apply(abs(predicts), 2, rank, na.last = "keep", ties.method = "average")
        
        filter <- matrix(genes.labels, nrow = g, ncol = nFold * l)
        negatives <- which(filter == 0, arr.ind = TRUE)
        positives <- which(filter == 1, arr.ind = TRUE)
        
        n.s <- colSums(test.genes.labels)
        o.s <- sapply(1:(nFold * l), function(i) order(predicts[, i], decreasing = TRUE))
        
        fp.s <- lapply(1:(nFold * l), function(i) cumsum(!test.genes.labels[o.s[, i], i]))
        tp.s <- lapply(1:(nFold * l), function(i) cumsum(test.genes.labels[o.s[, i], i]))
        fn.s <- lapply(1:(nFold * l), function(i) n.s[i] - tp.s[[i]])
        
        precis.s <- lapply(1:(nFold * l), function(i) tp.s[[i]]/(tp.s[[i]] + fp.s[[i]]))
        recall.s <- lapply(1:(nFold * l), function(i) tp.s[[i]]/n.s[i])
        auprc.s <- lapply(1:(nFold * l), function(i) get_auc(recall.s[[i]], precis.s[[i]]))
        auprc.null <- lapply(1:(nFold * l), function(i) n.s[i]/g)
        auprc.null <- rowMeans(matrix(unlist(auprc.null), ncol = nFold, nrow = l, byrow = FALSE), 
            na.rm = TRUE)
        auprc <- rowMeans(matrix(unlist(auprc.s), ncol = nFold, nrow = l, byrow = FALSE), na.rm = TRUE)
        names(auprc) <- colnames(genes.labels)
        
        # print('Calculate node degree')
        node_degree <- rowSums(network)
        colsums <- colSums(genes.labels)
        
        # print('Calculate node degree - sum across gene labels')
        node_degree <- matrix(node_degree)
        temp <- t(node_degree) %*% genes.labels
        
        
        # print('Calculate node degree - average')
        average_node_degree <- t(temp)/colsums
        
        scores <- cbind(auprc, matrix(average_node_degree)[, 1], auprc.null)
    }
    
    return(scores)
} 
