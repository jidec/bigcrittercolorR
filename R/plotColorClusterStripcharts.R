
#bcc_records <- anacs
plotColorClusterStripcharts1 <- function(bcc_records,point_image_folder="D:/anac_tests/patterns/phylo_preclustered",point_img_ext="_pattern.png"){
    library(stringr)
    data <- bcc_records
    col_prop_colnames <- colnames(data)[str_detect(colnames(data),"_prop")]

    for(name in col_prop_colnames){
        # Create a ggplot
        p <- ggplot(data, aes_string(x = name)) +
            geom_point(aes(y = 0), size = 2) +  # Set y to a constant value, like 0
            theme_minimal() +
            labs(title = "Points on One Axis with ggplot2") +
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        p <- addPointImages(p=p, data=data, image_folder=point_image_folder, img_extension=point_img_ext,x_col = name)#,size_x=5,size_y=5)
        plot(p)
    }
}

plotColorClusterStripcharts2 <- function(bcc_records, point_image_folder=NA, point_img_ext="_pattern.png") {
    library(ggplot2)
    library(stringr)

    data <- bcc_records
    col_prop_colnames <- colnames(data)[str_detect(colnames(data), "_prop")]

    # Initialize the ggplot object outside the loop
    p <- ggplot() + theme_minimal()

    adjust = 0
    for (name in col_prop_colnames) {
        # Add layers to the existing ggplot object
        p <- p + geom_point(data = data, aes_string(x = name, y = adjust), size = 2) +
            labs(title = paste("Points on One Axis with ggplot2 for", name)) +
            #ylim(c(0,50)) +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())

        if(!is.na(point_image_folder)){
            p <- addPointImages(p = p, data = data, image_folder = point_image_folder, img_extension = point_img_ext, x_col = name,y_adj=adjust)
        }

        adjust <- adjust + 0.02
    }

    # Finally, plot the result outside the loop
    print(p)
}
