library(rJava)
library(grid)
library(dplyr)
library(tibble)
library(data.tree)
library(stringr)

jar_path <- "Voronoi-Treemap-Library/build/libs/JVoroTreemap.jar"

if (!file.exists(jar_path)) {
  stop("Before using this script you need to run `gradle build` in `Voronoi-Treemap-Library`")
}

.jinit()
.jaddClassPath(jar_path)

.tree_to_TreeData <- function(tree) {
  treedata <- .jnew("kn/uni/voronoitreemap/interfaces/data/TreeData")
  root <- tree$pathString
  tree$Do(function (node) {
    if (is.null(node$parent))
      return()
    # this is the root node
    .jcall(treedata,
           "V",
           "addLink",
           node$pathString,
           node$parent$pathString)
    weight <- if (is.null(node$weight))
      1.0
    else
      node$weight
    .jcall(treedata,
           "V",
           "setWeight",
           node$pathString,
           as.double(weight))
    if (!is.null(node$relative_x) && !is.null(node$relative_y)) {
      .jcall(treedata,
             "V",
             "setRelativeVector",
             node$pathString,
             node$relative_x,
             node$relative_y)
    }
  })
  .jcall(treedata, "V", "setRoot", root)
  treedata
}

voronoi_treemap <- function(...) {
  voronoi_treemap_begin(...) %>% voronoi_treemap_finish()
}

voronoi_treemap_begin <- function(tree,
                            poly_x = c(0, 1, 1, 0),
                            poly_y = c(0, 0,
                                       1, 1),
                            max_error = 0.05,
                            max_iterations = 100000,
                            n_threads = 1)
{
  treedata <- .tree_to_TreeData(tree)
  treemap <- .jnew("kn/uni/voronoitreemap/treemap/VoronoiTreemap")
  root_polygon <- .jnew("kn/uni/voronoitreemap/j2d/PolygonSimple")
  scale_factor <-
    20000 # It seems to need to work with whole numbers
  for (i in 1:length(poly_x)) {
    .jcall(root_polygon,
           "V",
           "add",
           poly_x[i] * scale_factor,
           poly_y[i] * scale_factor)
  }
  .jcall(treemap, "V", "setRootPolygon", root_polygon)
  .jcall(treemap, "V", "setTreeData", treedata)
  .jcall(treemap, "V", "setCancelOnMaxIteration", TRUE)
  .jcall(treemap,
         "V",
         "setNumberMaxIterations",
         as.integer(max_iterations))
  .jcall(treemap, "V", "setErrorAreaThreshold", as.double(max_error))
  .jcall(treemap, "V", "setUniformWeights", FALSE)
  .jcall(treemap, "V", "setNumberThreads", as.integer(n_threads))
  thread <- .jcall(treemap, "Ljava/lang/Thread;", "computeLockedNewThread")
  list(root_polygon = root_polygon,
       treemap = treemap,
       tree = tree,
       thread = thread,
       scale_factor = scale_factor)
}

voronoi_treemap_finish <- function (handle, timeout = NA) { 
  terminated <- .jfield("java/lang/Thread$State", NULL, "TERMINATED")
  start_time <- as.numeric(Sys.time())
  repeat {
    if (.jcall(handle$thread, "Ljava/lang/Thread$State;", "getState") == terminated) break
    Sys.sleep(1)
    if (!is.na(timeout) && as.numeric(Sys.time()) - start_time > timeout) return(tibble())
  }
  root_polygon_area <- .jcall(handle$root_polygon, "D", "getArea")
  out <- handle$treemap %>%
    as.list() %>%
    tibble(n = .) %>%
    rowwise() %>%
    do({
      pathString <- .jfield(.$n, "S", "name")
      # weight <- .jcall(.$n, "D", "getWeight")
      level <- .jcall(.$n, "I", "getHeight")
      cur_node <- .$n
      repeat {
        polygon <-
          .jcall(cur_node,
                 "Lkn/uni/voronoitreemap/j2d/PolygonSimple;",
                 "getPolygon")
        if (!is.null(polygon)) {
          break
        }
        # If a parent only has one child, no polygon is created,
        # so inherit the parent's polygon for consistency.
        cur_node <-
          .jcall(cur_node,
                 "Lkn/uni/voronoitreemap/treemap/VoroNode;",
                 "getParent")
      }
      polygon_area <-
        .jcall(polygon, "D", "getArea") / root_polygon_area
      xs <- .jcall(polygon, "[I", "getXpointsClosed") / handle$scale_factor
      ys <- .jcall(polygon, "[I", "getYpointsClosed") / handle$scale_factor
      centroid <-
        .jcall(polygon,
               "Lkn/uni/voronoitreemap/j2d/Point2D;",
               "getCentroid")
      centroid_x <- .jfield(centroid, "D", "x") / handle$scale_factor
      centroid_y <- .jfield(centroid, "D", "y") / handle$scale_factor
      data.frame(
        pathString,
        polygon_area,
        level,
        xs,
        ys,
        centroid_x,
        centroid_y,
        stringsAsFactors = FALSE
      )
    }) %>%
    ungroup() %>%
    as_tibble()
  tree_table <-
    do.call(ToDataFrameTree, c(handle$tree, "pathString", "name", handle$tree$fieldsAll)) %>%
    filter(pathString != handle$tree$pathString) # remove parent
  tree_table <- left_join(tree_table, out, by = "pathString")
  
  # calculate area errors for each leaf
  tree_leaves <- ToDataFrameTable(handle$tree, "pathString", "weight") %>%
    mutate(weight = replace(weight, is.na(weight), 1.0),
           weight = weight / sum(weight))
  area_errors <- left_join(tree_leaves, out, by = "pathString") %>%
    group_by(pathString) %>%
    summarise(polygon_area = first(polygon_area),
              weight = first(weight)) %>%
    mutate(area_error = (weight - polygon_area) / weight) %>%
    select(pathString, area_error)
  
  left_join(tree_table, area_errors, by = "pathString")
}


