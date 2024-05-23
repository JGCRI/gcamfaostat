


#' chord_wrapper: a wrapper function of the chordDiagram function
#' Title
#'
#' @param .DF Input data frame.
#' @param .FigTitle Figure title
#' @param .SaveDir The directory for saving the plot; the default is man/figures.
#' @param .GRIDCOLOR Colors of grid; consider using circlize::rand_color function
#' @param .ORDER Order of the gird
#' @param .SaveName Name of the saved file in .SaveDir
#' @param .SaveScaler A scaler controling the size (dpi = 600)
#' @importFrom grDevices dev.off png
#'
#' @return A saved plot in .SaveDir
#' @export


chord_wrapper <- function(.DF,
                          .FigTitle = NULL,
                          .SaveDir = "../man/figures",
                          .GRIDCOLOR = NULL,
                          .ORDER = NULL,
                          .SaveName,
                          .SaveScaler = 1){

  # treemap save to a path
  png(filename= file.path(.SaveDir, paste0(.SaveName,".png")), res = 600,
      width= 7000 * .SaveScaler, height= 7000 * .SaveScaler )


  chordDiagram(as.data.frame(.DF),
               transparency = 0.5,
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               diffHeight = -uh(2, "mm")
               ,link.arr.type = "big.arrow"
               ,annotationTrack = c("grid")
               ,grid.col = .GRIDCOLOR
               ,order = .ORDER
               ,preAllocateTracks = list(list(track.height = c(0.3))
                                         ,list(track.height = c(0.035))
               ))

  title(main = .FigTitle)

  circos.track(track.index = 3, panel.fun = function(x, y) {
    circos.axis(h = 1, labels.cex = 0.8)
  }, bg.border = NA)

  circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")

    #make text label vertical when space is too small; cex to adjust font size

    if(abs(xplot[2] - xplot[1]) < 20 | abs(xplot[2] - xplot[1]) > 340) {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                  niceFacing = TRUE, adj = c(0, 0.5), col = "black",
                  cex = 1)
    } else {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                  niceFacing = TRUE, adj = c(0.5, 0), col= "black",
                  cex = 1)
    }  }, bg.border = NA)

  dev.off() #dump

  circos.clear()
}


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
                            .SaveDir = "../man/figures",
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
