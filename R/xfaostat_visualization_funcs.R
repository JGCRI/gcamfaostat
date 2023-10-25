

#' treemap_wrapper: a warpper function of the treemap function
#'
#' @param .DF  Input data frame. The data frame needs to include index columns (need to be first columns) and a value column.
#' @param .Depth Grouping depth of the tree; the columns will be used. The first 2 columns in .DF will be index in treemap
#' @param .Palette Color palette (RColorBrewer is used)
#' @param .FigTitle Figure title
#' @param .SaveDir The directory for saving the plot; the default is man/figures.
#' @param .SaveName Name of the saved png file in .SaveDir
#' @param .FigTitleSize Size of title font
#' @param .LastLabelCol Color of the last label
#' @param .SaveScaler A scaler controling the size (dpi = 300)
#' @importFrom grDevices dev.off png
#' @description
#' See https://cran.r-project.org/web/packages/treemap/index.html for the details of the treemap function
#'
#' @return A saved plot in .SaveDir
#' @export

treemap_wrapper <- function(.DF,
                            .Depth = 2,
                            .Palette = "Set2",
                            .FigTitle,
                            .FigTitleSize = 12,
                            .LastLabelCol = "orange",
                            .SaveDir = "man/figures",
                            .SaveName,
                            .SaveScaler = 1){

  assertthat::assert_that(is.data.frame(.DF))
  assertthat::assert_that(is.character(.SaveName))
  assertthat::assert_that(is.character(.FigTitle))
  assertthat::assert_that(.Depth%%1==0)
  assertthat::assert_that(is.numeric(.SaveScaler))


  # treemap save to a path
  png(filename= file.path(.SaveDir, paste0(.SaveName,".png")), res = 300,
      width= 2500 * .SaveScaler, height= 1800 * .SaveScaler )
  # treemap
  treemap(.DF,
          index= names(.DF)[1:.Depth],
          vSize="value",
          type="index",
          fontsize.title = .FigTitleSize,
          fontsize.labels=c(15,12, 9)[1:.Depth],                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
          fontcolor.labels=c(.LastLabelCol, "black", "grey")[1:.Depth],    # Color of labels
          fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
          bg.labels=c("transparent"),              # Background color of labels
          align.labels=list(
            c("right", "bottom"),
            c("center", "center"),
            c("right", "bottom")
          )[1:.Depth],                                   # Where to place labels in the rectangle?
          overlap.labels=0.1,                      # number between 0 and 1 that determines the tolerance of the overlap between labels.
          inflate.labels= F,                        # If true, labels are bigger when rectangle is bigger.
          palette = .Palette,                        # Select your color palette from the RColorBrewer presets or make your own.
          title = .FigTitle
  )

  dev.off()
}
