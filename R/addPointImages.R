# data must have "image_path" column
addPointImages <- function(p, data, image_folder, img_extension = "_pattern.png", x_col="col_1_prop", y_col=NA, y_adj=0, size_x=0.01, size_y=0.01){
    library(dplyr)
    data$image_path <- paste0(image_folder, "/", data$img_id, img_extension)
    print(data$image_path)
    #data <- sample_n(data,10)
    # Add images as custom annotations
    for (i in 1:nrow(data)) {
        image_path <- data$image_path[i]
        if (file.exists(image_path)) {
            img <- png::readPNG(image_path)
            raster_grob <- grid::rasterGrob(img, interpolate=TRUE)

            # add
            if(is.na(y_col)){
                p <- p + annotation_custom(grob = raster_grob,
                                           xmin = data[i,x_col] - size_x, xmax = data[i,x_col] + size_x,
                                           ymin = y_adj - size_y, ymax = y_adj + size_y)
            }
            else{
                p <- p + annotation_custom(grob = raster_grob,
                                           xmin = data[i,x_col] - size_x, xmax = data[i,x_col] + size_x,
                                           ymin = data[i,y_col] - size_y, ymax = data[i,y_col] + size_y)
            }
        }
    }
    return(p)
}
