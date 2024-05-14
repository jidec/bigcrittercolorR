
#bcc_records <- data
getColorClusterHexCodes <- function(bcc_records){
    df <- bcc_records
    pattern <- "^(?!.*mean).*(_r|_g|_b).*"
    columns_to_keep <- grep(pattern, names(df), value = TRUE,perl=TRUE)
    df <- df[, columns_to_keep]
    row <- df[1, ]
    colors_df <- data.frame(matrix(ncol = 3, nrow = ncol(df) / 3))
    names(colors_df) <- c("R", "G", "B")
    colors_df <- colors_df[!is.na(colors_df[,1]),]
    for (i in 1:(ncol(df) / 3)) {
        colors_df[i, ] <- c(row[[paste0("col_", i, "_r")]], row[[paste0("col_", i, "_g")]], row[[paste0("col_", i, "_b")]])
    }
    rgbToHex <- function(r, g, b) {
        sprintf("#%02X%02X%02X", r, g, b)
    }
    color_hex <- apply(colors_df, 1, function(color) rgbToHex(color['R'], color['G'], color['B']))
    return(color_hex)
}
