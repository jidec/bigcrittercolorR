#Mode <- function(x) {
#    ux <- unique(x)
#    ux[which.max(tabulate(match(x, ux)))]
#}

#bcc_records = read.csv("D:/GitProjects/bigcrittercolorR/5_13_gomphids_records.csv")
#taxon = "genus"
#subtaxon = "species"
#sample_n_taxa = 5
#sample_n_per_taxon = NA
#point_image_folder = NA

plotTaxonDiffsPCA <- function(bcc_records,taxon="species",sample_n_taxa=5,sample_n_per_taxon=NA,subtaxon=NA,point_image_folder=NA){
    library(stringr)
    library(dplyr)
    library(ggplot2)
    data <- bcc_records

    # create sex column
    data$sex <- rep("unknown",nrow(data))
    data$sex[str_detect(data$annotations,"Sex:Male")] <- "male"
    data$sex[str_detect(data$annotations,"Sex:Female")] <- "female"

    # add path column
    data$image_path <- paste0(point_image_folder, "/", data$img_id, "_pattern.png")

    # select columns for PCA
    pca_data <- data %>% select(contains("prop"))
    pca <- prcomp(pca_data, scale. = TRUE)

    # extract PCA scores
    scores <- as.data.frame(pca$x)
    scores$species <- data$species
    scores$genus <- as.factor(data$genus)
    scores$sex <- as.factor(data$sex)
    scores$img_id <- data$img_id
    # todo prevent the final records_with_metrics from getting img_id_x
    scores$img_id <- as.factor(data$img_id_x)

    if(!is.na(sample_n)){
        scores <- scores %>%
            filter({
                col_data <- .[[taxon]] # Dynamically select column
                col_data %in% sample(unique(col_data), sample_n_taxa)
            })
    }

    if(!is.na(subtaxon)){
        scores <- scores %>% group_by(.data[[subtaxon]]) %>%
            summarize(
                across(where(is.numeric), mean, na.rm = TRUE),  # Apply mean to all numeric columns
                across(where(is.factor), Mode)                 # Apply mode to all factor columns
            )
    }

    if(!is.na(sample_n_per_taxon)){
        scores <- scores %>%
            group_by(taxon) %>%
            slice_sample(n = sample_n_per_taxon, replace = TRUE) %>%
            ungroup()  #
    }

    # get loadings and scale
    loadings <- as.data.frame(pca$rotation)
    scaling_factor <- max(abs(scores$PC1), abs(scores$PC2)) / max(abs(loadings$PC1), abs(loadings$PC2)) * 0.7
    loadings_scaled <- loadings * scaling_factor

    # start with the scores plot
    p <- ggplot() +
        geom_point(data = scores, aes(x = PC1, y = PC2, color = get(taxon)), size = 2) +
        xlab("Principal Component 1") +
        ylab("Principal Component 2") +
        ggtitle("PCA Plot with Scores and Loadings")

    if(!is.na(point_image_folder)){
        p <- addPointImages(p=p,data=scores,image_folder=point_image_folder,img_extension="_pattern.png",x_col = "PC1", y_col="PC2",size_x = 0.4,size_y = 0.4)

        # Add images as custom annotations
        #for (i in 1:nrow(scores)) {
        #    image_path <- data$image_path[data$img_id == scores$img_id[i]]
        #    if (file.exists(image_path)) {
        #        img <- png::readPNG(image_path)
        #        raster_grob <- grid::rasterGrob(img, interpolate=TRUE)
        #        # Increase the size of the images
        #        size_x <- 0.4 # Increase for larger width
        #        size_y <- 0.4 # Increase for larger height
        #        p <- p + annotation_custom(grob = raster_grob,
        #                                   xmin = scores$PC1[i] - size_x, xmax = scores$PC1[i] + size_x,
        #                                   ymin = scores$PC2[i] - size_y, ymax = scores$PC2[i] + size_y)
        #    }
        #}
    }

    # overlay the loadings as arrows
    p <- p + geom_segment(data = loadings_scaled, aes(x = 0, y = 0, xend = PC1, yend = PC2),
                          arrow = arrow(type = "closed", length = unit(0.2, "inches"))) +
        geom_text(data = loadings_scaled, aes(x = PC1, y = PC2, label = row.names(loadings)),
                  hjust = 1.2, vjust = 1.2)
    p
}

#visualizeTaxonDiffsPCA(anacs, taxon="species", sample_n=3, point_image_folder="D:/anac_tests/patterns/phylo_preclustered")
#visualizeTaxonDiffsPCA(gomphids, taxon="genus", sample_n=5, point_image_folder=NA)

