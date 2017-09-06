#' @name attr.human
#' @title Human GENCODE annotations (v22)
#' 
#' @description
#' A dataset containing identifiers for gene transcripts
#'
#'
#' @docType data
#' 
#' @format A data frame with 60483 rows and 10 variables:
#' \describe{
#'   \item{chr}{chromosome}
#'   \item{start}{chromosomal start position, in base pairs}
#'   \item{end}{chromosomal end position, in base pairs}
#'   \item{strand}{chromosomal strand, + or - }
#'   \item{un}{unknown}
#'   \item{ensemblID}{ENSEMBL identifier}
#'   \item{type}{type of transcript}
#'   \item{stat}{status of transcript}
#'   \item{name}{HUGO identifier}
#'   \item{entrezID}{Entrez identifier}
#'  
#' }
#'  @source \url{ftp://ftp.sanger.ac.uk/pub/gencode/Gencode_human/release_22/}
#'  
#' @rdname atrr.human
#'
NULL

