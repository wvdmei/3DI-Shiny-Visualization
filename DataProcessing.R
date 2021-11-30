files <- list.files("/Users/wfvdmei/Documents/Rotation Fall 2021/Shiny App/3DI Visualization/AdjustedEstimates/", full.names = TRUE)

op25Files <- files[grepl(pattern = "0p25", x = files, ignore.case = TRUE)]

op67Files <- files[grepl(pattern = "0p67", x = files, ignore.case = TRUE)]

estimatesListOp25 <- lapply(op25Files, read.csv, stringsAsFactors = FALSE)

estimatesOp25 <- do.call(rbind, estimatesListOp25)

estimatesOp25$range = "fold7"

estimatesListOp67 <- lapply(op67Files, read.csv, stringsAsFactors = FALSE)

estimatesOp67 <- do.call(rbind, estimatesListOp67)

estimatesOp67$range = "fold25"

estimatesProcesssed <- rbind(estimatesOp25, estimatesOp67)

estimatesProcesssed$pSB <- ifelse(estimatesProcesssed$SB_beta <= 0, 2*pnorm(estimatesProcesssed$SB_beta/sqrt(estimates$SB_var), lower.tail = TRUE), 2*pnorm(estimatesProcesssed$SB_beta/sqrt(estimates$SB_var), lower.tail = FALSE))

estimatesProcesssed$log10P <- log10(1/estimatesProcesssed$pSB)

estimatesProcesssed$log2RR <- log2(estimatesProcesssed$SB_RR)

estimatesProcesssed$log2RRLower <- log2(estimatesProcesssed$RR_lower)

estimatesProcesssed$log2RRUpper <- log2(estimatesProcesssed$RR_upper)

readr::write_csv(estimatesProcesssed, "/Users/wfvdmei/Documents/Rotation Fall 2021/Shiny App/3DI Visualization/AdjustedEstimates/adjustedEstimatesProcessed.csv")

