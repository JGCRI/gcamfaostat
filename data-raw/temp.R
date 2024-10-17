

GCAM_APE_Beef <- function(TREE_DATA = T, TREE_PLOT = F){

  # For livestock sectors, use meat (carcass) as primary products
  # Live animals are included as meat EQ but trade is not included.
  # https://extension.tennessee.edu/publications/documents/pb1822.pdf

  # Note that hide offal primary fat won't be included for now
  # Because they are not in carcass weight
  # "Tallow", "Fat nes, prepared" are use to explain processed "Fat, cattle butcher"

  assertthat::assert_that(is.logical(TREE_PLOT))
  assertthat::assert_that(is.logical(TREE_DATA))


  data.frame(
    item = c("Beef",
             "Primary, Beef",
             "Meat, cattle",  "Cattle Stock", "Meat, buffalo", "Buffalo Stock",
             "Processed, Beef",
             "Meat, cattle, boneless (beef & veal)",
             "Meat, beef, dried, salted, smoked",
             "Meat, extracts",
             "Meat, beef and veal sausages",
             "Meat, homogenized preparations",
             "Meat, beef, preparations",
             "Fat, cattle butcher",
             "Tallow", "Fat nes, prepared"),
    dist = c(800,
             400, 400, 400, 400, 400,
             rep(200, 10)),
    dist = c(800,
             400, 350, 350, 350, 350,
             200, rep(150, 9)),
    dist = c(800,
             400, 340, 340, 330, 330,
             200, rep(150, 6), 120, 119, 119),
    row.names = T) -> den

  if (TREE_DATA == T) {
    return(den)}

  if (TREE_PLOT == T) {

    dd <- dist(den, method = "euclidean")
    hc <- hclust(dd, method = "ward.D2")
    #ggdendrogram(hc) + theme(axis.text.y = element_blank())
    hc$height = hc$height^.5 + 5
    hcdata <- dendro_data_k(hc, 5)
    plot_ggdendro(hcdata,
                  direction   = "tb",
                  expand.y    = 1) + theme_void() -> p

    return(p)
  }



}



GCAM_APE_Corn <- function(TREE_DATA = T, TREE_PLOT = F){

  # There are 8 SUA items for maize and the processed food.
  # Note that maize germ cake data are not available
  # Maize germ cake is added to maize oil for primarizing
  # That is, APE includes both maize oil and cake
  # Need to remove Maize germ oil in FAO_ag_items_cal_SUA.csv


  data.frame(
    item = c("Maize",
             "Flour, maize",  "Starch, maize", "Gluten, maize", "Feed and meal, gluten1",
             "Germ, maize", "Oil, maize", "Cake, maize",
             "Bran, maize", "Feed and meal, gluten2"),
    dist = c(800,
             400, 400, 400, 400,
             200, 200, 200,
             100, 100),
    dist = c(800,
             400, 350, 350, 350,
             200, 150, 150,
             100, 50),
    dist = c(800,
             400, 350, 300, 300,
             200, 150, 150,
             100, 50),
    row.names = T) -> den

  if (TREE_DATA == T) {
    return(den)}

  if (TREE_PLOT == T) {

    dd <- dist(den, method = "euclidean")
    hc <- hclust(dd, method = "ward.D2")
    #ggdendrogram(hc) + theme(axis.text.y = element_blank())
    hc$height = hc$height^.5 + 5
    hcdata <- dendro_data_k(hc, 5)
    plot_ggdendro(hcdata,
                  direction   = "tb",
                  expand.y    = 1) + theme_void() -> p

    return(p)
  }

}





GCAM_APE_Dairy <- function(TREE_DATA = T, TREE_PLOT = F){

  # "Milk, reconstituted", "Milk, products of natural constituents nes")); all zeros or not needed

  # "Buttermilk, curdled, acidified milk" remove this due to strong data concerns
  # "Buttermilk, curdled, acidified milk" has issues with huge jump in prod and feed/processed in 2013-2017
  # "Yoghurt, concentrated or not" is not really needed
  # "Whey, cheese"	"Whey, condensed"	"Whey, dry" won't be included
  #  so there will be processed use of "Whey, fresh"


  # The APE approach works okay for non-cow milk products given the small trade/storage; (mainly sharing processed and data aligned well across the chain)
  # But this was not the case for cow milk; so manually adjust extraction rates here
  GCAM_COMM <- "Dairy"


  data.frame(
    item = c(GCAM_COMM,
             "Milk, whole fresh buffalo",
             "Ghee, buffalo milk",	"Butter, buffalo milk",	"Milk, skimmed buffalo",	"Cheese, buffalo milk",
             "Milk, whole fresh goat",
             "Milk, skimmed goat",	"Cheese, goat milk",	"Butter, goat milk",
             "Milk, whole fresh sheep",
             "Milk, skimmed sheep",	"Butter and ghee, sheep milk", "Cheese, sheep milk",
             "Milk, whole fresh cow",

             "Milk, whole condensed",	"Milk, whole dried",	"Milk, whole evaporated",
             "Ghee, butteroil of cow milk",	"Butter, cow milk",
             "Yoghurt", "Cheese, whole cow milk", "Skim Buttermilk", "Whey, fresh",

             "Milk, skimmed cow",
             "Cheese, skimmed cow milk",	"Milk, skimmed condensed",
             "Milk, skimmed dried",	"Milk, skimmed evaporated",	"Milk, dry buttermilk",
             "Casein",
             "Cream fresh",
             "Ice cream and edible ice","Cheese, processed" ),
    dist = c(800, rep(400, 33)),
    dist = c(800, rep(100, 33)),
    dist = c(800,
             rep(400, 5),
             rep(400, 4),
             rep(400, 4),
             rep(300, 20)
    ),
    dist = c(800,
             rep(400, 5),
             rep(350, 4),
             rep(300, 4),
             rep(200, 20)
    ),
    dist = c(800,
             400, rep(380, 4),
             350, rep(330, 3),
             300, rep(280, 3),
             200, rep(180, 19)
    ),
    dist = c(800,
             400, rep(380, 4),
             350, rep(330, 3),
             300, rep(280, 3),
             200, rep(100, 3), rep(90, 6), rep(70, 7), rep(60, 3)
    ),
    dist = c(800,
             400, rep(380, 4),
             350, rep(330, 3),
             300, rep(280, 3),
             200, rep(100, 3), rep(90, 6), 70, rep(65, 6), 60, rep(55, 2)
    ),
    row.names = T) -> den


  if (TREE_DATA == T) {
    return(den)}

  if (TREE_PLOT == T) {
    dd <- dist(den, method = "euclidean")
    hc <- hclust(dd, method = "ward.D2")
    #ggdendrogram(hc) + theme(axis.text.y = element_blank())
    hc$height = hc$height^.5 + 5
    hcdata <- dendro_data_k(hc, 7)
    plot_ggdendro(hcdata,
                  direction   = "tb",
                  expand.y    = 1) + theme_void() -> p

    return(p)
  }
}


GCAM_APE_FiberCrop <- function( TREE_DATA = T, TREE_PLOT = F){


  # Kapok fruit should be moved back from oilcrops?
  #  813, 778, "Coir", "Kapok fibre" are under OilCrop as coproduct

    # Large variation in linter rate relative to cake
    # China 2/55 India 2/50 USA 6.8/44 Pakistan 3/50
    # othershare in 3.5 - 11% and using 5% here

  data.frame(
    item = c("FiberCrop",
             "Seed cotton",  "Cotton lint", "Cottonseed", "Oil, cottonseed", "Cake, cottonseed", "Fiber, cottonseed",
             "Agave fibres nes", "Bastfibres, other", "Fibre crops nes",
             "Flax fibre and tow", "Hemp tow waste", "Jute",
             "Manila fibre (abaca)", "Ramie", "Sisal"
    ),
    dist = c(800,
             rep(400, 15)),
    dist = c(800,
             rep(300, 6),
             rep(400, 9)),
    dist = c(800,
             300, rep(350, 5),
             rep(400, 9)),
    dist = c(800,
             300, rep(250, 1), rep(220, 4),
             rep(400, 9)),
    dist = c(800,
             300, rep(250, 1), 220, rep(200, 3),
             rep(400, 9)),
    row.names = T) -> den

  if (TREE_DATA == T) {
    return(den)}

  if (TREE_PLOT == T) {
    dd <- dist(den, method = "euclidean")
    hc <- hclust(dd, method = "ward.D2")
    #ggdendrogram(hc) + theme(axis.text.y = element_blank())
    hc$height = hc$height^.5 + 5
    hcdata <- dendro_data_k(hc, 3)
    plot_ggdendro(hcdata,
                  direction   = "tb",
                  expand.y    = 1) + theme_void() -> p

    return(p)
  }

}



GCAM_APE_Fruits <- function(TREE_DATA = T, TREE_PLOT = F){

  data.frame(
    item = c("Fruits",
             "Primary, Fruits", COMM_primary %>% pull(SCL_item),
             "Processed, Fruits", COMM_processed %>% pull(SCL_item)
    ),
    dist = c(800,
             rep(400, length(c(COMM_primary %>% pull(SCL_item), COMM_processed %>% pull(SCL_item))) + 2)),
    dist = c(800,
             300, rep(300, length(c(COMM_primary %>% pull(SCL_item))) ),
             250, rep(250, length(c(COMM_processed %>% pull(SCL_item))) )),
    dist = c(800,
             300, rep(260, length(c(COMM_primary %>% pull(SCL_item))) ),
             200, rep(150, length(c(COMM_processed %>% pull(SCL_item))) )),
    row.names = T) -> den


  if (TREE_DATA == T) {
    return(den)}

  if (TREE_PLOT == T) {
    dd <- dist(den, method = "euclidean")
    hc <- hclust(dd, method = "ward.D2")
    #ggdendrogram(hc) + theme(axis.text.y = element_blank())
    hc$height = hc$height^.5 + 5
    hcdata <- dendro_data_k(hc, 5)
    plot_ggdendro(hcdata,
                  direction   = "tb",
                  expand.y    = 1) + theme_void() -> p

    return(p)
  }

}


GCAM_APE_Legumes <- function(TREE_DATA = T, TREE_PLOT = F){


  #!!!note "Broad beans, horse beans, dry" in vegetable
  #!!!note "Vetches" was in FodderHerb in Prod items; but moved to Legumes


  data.frame(
    item = c("Legumes",
             "Primary, Legumes", c(COMM_primary %>% pull(SCL_item)),
             "Processed, Legumes", c(COMM_processed %>% pull(SCL_item))
    ),
    dist = c(800,
             rep(400, length(c(COMM_primary %>% pull(SCL_item), COMM_processed %>% pull(SCL_item))) + 2)),
    dist = c(800,
             300, rep(300, length(c(COMM_primary %>% pull(SCL_item))) ),
             250, rep(250, length(c(COMM_processed %>% pull(SCL_item))) )),
    dist = c(800,
             300, rep(260, length(c(COMM_primary %>% pull(SCL_item))) ),
             200, rep(150, length(c(COMM_processed %>% pull(SCL_item))) )),
    row.names = T) -> den

  if (TREE_DATA == T) {
    return(den)}

  if (TREE_PLOT == T) {
    dd <- dist(den, method = "euclidean")
    hc <- hclust(dd, method = "ward.D2")
    #ggdendrogram(hc) + theme(axis.text.y = element_blank())
    hc$height = hc$height^.5 + 5
    hcdata <- dendro_data_k(hc, 3)
    plot_ggdendro(hcdata,
                  direction   = "tb",
                  expand.y    = 1) + theme_void() -> p

    return(p)
  }

}


#Neet to test r package data.tree

GCAM_APE_MiscCrop <- function(TREE_DATA = T, TREE_PLOT = F){


  TREEDATA_APE %>% filter(GCAM_commodity == GCAM_crop)
  GCAM_crop <- "MiscCrop"
 # # remove "Tea, mate extracts" no production small impacts
 #  SCL_item = c("Cocoa, butter", "Peppermint", "Hops",
 #               "Pyrethrum, dried", "Tobacco, unmanufactured", #four not included in FBS & SCL but in QCL
 #                "Rubber, natural")  # Added rubber given the land implications no "Gums, natural"
 #
 #
  data.frame(
    item = c(GCAM_crop,
             "Coffee, green", "Coffee, roasted", "Coffee, extracts",
             "Cocoa, beans", "Cocoa, paste",
             "Cocoa, butter", "Chocolate products nes1",
             "Cocoa, powder & cake", "Chocolate products nes2",
             setdiff(c(COMM_primary %>% pull(SCL_item)),c("Coffee, green", "Cocoa, beans")) ),
    dist = c(800,
             rep(400, 24)),
    dist = c(800,
             rep(300, 3),
             rep(200, 6),
             rep(400, 15)),
    dist = c(800,
             300, rep(280, 2),
             200, rep(180, 5),
             rep(400, 15)),
    dist = c(800,
             300, rep(280, 2),
             200, 180, rep(170, 2), rep(168, 2),
             rep(400, 15)),
    row.names = T) -> den

  if (TREE_DATA == T) {
    return(den)}

  if (TREE_PLOT == T) {
    dd <- dist(den, method = "euclidean")
    hc <- hclust(dd, method = "ward.D2")
    #ggdendrogram(hc) + theme(axis.text.y = element_blank())
    hc$height = hc$height^.5 + 5
    hcdata <- dendro_data_k(hc, 4)
    plot_ggdendro(hcdata,
                  direction   = "tb",
                  expand.y    = 1) + theme_void() -> p

    return(p)
  }

}

library(ggdendro)

data.frame(
  item = c("Maize",
           "Flour, maize",  "Starch, maize", "Gluten, maize", "Feed and meal, gluten1",
           "Germ, maize", "Oil, maize", "Cake, maize",
           "Bran, maize", "Feed and meal, gluten2"),
  dist = c(800,
           400, 400, 400, 400,
           200, 200, 200,
           100, 100),
  dist = c(800,
           400, 350, 350, 350,
           200, 150, 150,
           100, 50),
  dist = c(800,
           400, 350, 300, 300,
           200, 150, 150,
           100, 50),
  row.names = T) -> den

# soybeans
data.frame(
  item = c("Soybean",
           "Oil, soybean", "Cake, soybeans", "Soya sauce", "Soya paste",  "Soya curd"
  ),
  dist = c(800,
           rep(400, 5)),
  dist = c(800,
           350,350, rep(400, 3)),
  row.names = T) -> den

dd <- dist(den, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
#ggdendrogram(hc) + theme(axis.text.y = element_blank())
hc$height = hc$height^.5 + 5
hcdata <- dendro_data_k(hc, 3)
plot_ggdendro(hcdata,
              branch.size = 1,
              label.size  = 10,
              nudge.label = 0.02,
              direction   =  c("lr"),
              expand.y    = 1.4) + theme_void()  -> p;p

p + scale_x_reverse()  -> p1;p1

ggsave(filename = file.path(paste0("Soy1",".png")),
       plot = p1,
       dpi = 300, width = 6 , height = 5.1 )

ggsave(filename = file.path(paste0("Maize",".png")),
       plot = p1,
       dpi = 300, width = 6 , height = 5.1 )

hc$height = hc$height^.5 + 5
hcdata <- dendro_data_k(hc, 6)
plot_ggdendro(hcdata,
              branch.size = 1,
              label.size  = 10,
              nudge.label = 0.02,
              direction   =  c("tb"),
              expand.y    = 0.8) + theme_void()  -> p;p

ggsave(filename = file.path(paste0("Maize",".png")),
       plot = p,
       dpi = 300, width = 18 , height = 10 )


