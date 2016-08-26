#' readNMRMLProcessed
#'
#' Extract binary processed data from nmrML
#'
#' This is the Details section
#'
#' @param filename character Filename of the nmrML to check
#' @return A vector with the numeric values of the processed data
#' @author Steffen Neumann
#' @examples
#' # length(readNMRMLProcessed(system.file("examples/HMDB00005.nmrML", package = "nmRIO")))
#' @export

readNMRMLProcessed <- function (filename) {
    tree <- xmlTreeParse(filename)
    root <- xmlRoot(tree)

    ## Currently reads only the FIRST spectrumDataArray from file:
    spectrumData <- xmlElementsByTagName(root, "spectrumDataArray",
                                    recursive = TRUE)[["spectrumList.spectrum1D.spectrumDataArray"]]
    
    ## Extract base64encoded data 
    b64s <- gsub("\n", "", xmlValue(spectrumData))

    byteFormat <- xmlAttrs(spectrumData)["byteFormat"]
    what <- switch(byteFormat,
                   float64 = "double", ## This is what I found in Sample_21.nmrML / spectrumDataArray
                   Complex128 = "double", # that's because complex128 is misleading
                   Complex64 = "double",
                   Integer32 = "integer",
                   Complex32int = "integer",
                   "class java.lang.Integer" = "integer",
                   Complex64int = "currentlynotsupported")
    
    compression <- ifelse(xmlAttrs(spectrumData)["compressed"]=="true", "gzip", "none")
    
    dproc <- nmRIO:::binaryArrayDecode(b64s,
                              what=what, compression=compression)
    intensities <- dproc[c(FALSE, TRUE)]
    
    ## Get required parameters from nmrML
    irradiationFrequency <- as.double(xmlAttrs(xmlElementsByTagName(root, "irradiationFrequency", recursive = TRUE)[[1]])["value"])

    sweepWidth <- as.double(xmlAttrs(xmlElementsByTagName(root, "sweepWidth", recursive = TRUE)[[1]])["value"])

    numberOfDataPoints <- as.integer(xmlAttrs(xmlElementsByTagName(root, "DirectDimensionParameterSet", recursive = TRUE)[[1]])["numberOfDataPoints"])    
    
    ## delayTime <- 1.0
    ## ppmOffset <- delayTime / 599.4094446,
    
    ppm <- seq(from=14.77180,
               to= -5.239921,
               length=numberOfDataPoints)
        
    datamatrix <- cbind(ppm, intensities)
    names(datamatrix) <- c("ppm", basename(filename))
    datamatrix
}



if (FALSE) {
    ## This section contains test snippets during development
    library(nmRIO)
    
    filename <- "inst/examples/Sample_21.nmrML"
    d <- readNMRMLProcessed(filename)

    read
    
}
