files <- list.files("/Users/wfvdmei/Documents/Rotation Fall 2021/Shiny App/3DI Visualization/AdjustedEstimates/", full.names = TRUE)

op25Files <- files[grepl(pattern = "0p25", x = files, ignore.case = TRUE)]

op67Files <- files[grepl(pattern = "0p67", x = files, ignore.case = TRUE)]

estimatesListOp25 <- lapply(op25Files, read.csv, stringsAsFactors = FALSE)

estimatesOp25 <- do.call(rbind, estimatesListOp25)

estimatesOp25$range = "fold7"

estimatesListOp67 <- lapply(op67Files, read.csv, stringsAsFactors = FALSE)

estimatesOp67 <- do.call(rbind, estimatesListOp67)

estimatesOp67$range = "fold25"

estimatesProcessed <- rbind(estimatesOp25, estimatesOp67)

estimatesProcessed$pSB <- ifelse(estimatesProcessed$SB_beta <= 0, 2*pnorm(estimatesProcessed$SB_beta/sqrt(estimatesProcessed$SB_var), lower.tail = TRUE), 2*pnorm(estimatesProcessed$SB_beta/sqrt(estimatesProcessed$SB_var), lower.tail = FALSE))

estimatesProcessed$log10P <- log10(1/estimatesProcessed$pSB)

estimatesProcessed$log2RR <- log2(estimatesProcessed$SB_RR)

estimatesProcessed$log2RRLower <- log2(estimatesProcessed$RR_lower)

estimatesProcessed$log2RRUpper <- log2(estimatesProcessed$RR_upper)

estimatesProcessed$Object <- gsub(x = estimatesProcessed$Object, pattern = "_", replacement = " ", perl = FALSE)

estimatesProcessed$Base_Precipitant <- gsub(x = estimatesProcessed$Base_Precipitant, pattern = "_", replacement = " ", perl = FALSE)

estimatesProcessed$Precipitant <- gsub(x = estimatesProcessed$Precipitant, pattern = "_", replacement = " ", perl = FALSE)

estimatesProcessed$Object <- str_to_title(estimatesProcessed$Object)

estimatesProcessed$Base_Precipitant <- str_to_title(estimatesProcessed$Base_Precipitant)

estimatesProcessed$Precipitant <- str_to_title(estimatesProcessed$Precipitant)

readr::write_csv(estimatesProcessed, "/Users/wfvdmei/Documents/Rotation Fall 2021/Shiny App/3DI Visualization/AdjustedEstimates/adjustedEstimatesProcessed.csv")

