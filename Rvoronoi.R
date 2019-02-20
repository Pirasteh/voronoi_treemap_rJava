jar_path <- "Voronoi-Treemap-Library/build/libs/JVoroTreemap.jar"

if (!file.exists(jar_path)) {
  stop("Before using this script you need to run `gradle build` in `Voronoi-Treemap-Library`")
}

rJava::.jinit()
rJava::.jaddClassPath(jar_path)

.tree_to_TreeData <- function(tree) {
  treedata <- rJava::.jnew("kn/uni/voronoitreemap/interfaces/data/TreeData")
  root <- tree$pathString
  tree$Do(function (node) {
    if (is.null(node$parent))
      return()
    # this is the root node
    rJava::.jcall(treedata,
                  "V",
                  "addLink",
                  node$pathString,
                  node$parent$pathString)
    weight <- if (is.null(node$weight))
      1.0
    else
      node$weight
    rJava::.jcall(treedata,
                  "V",
                  "setWeight",
                  node$pathString,
                  as.double(weight))
    if (!is.null(node$relative_x) && !is.null(node$relative_y)) {
      rJava::.jcall(treedata,
                    "V",
                    "setRelativeVector",
                    node$pathString,
                    node$relative_x,
                    node$relative_y)
    }
  })
  rJava::.jcall(treedata, "V", "setRoot", root)
  treedata
}

voronoi_treemap <- function(...) {
  voronoi_treemap_finish(voronoi_treemap_begin(...))
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
  treemap <- rJava::.jnew("kn/uni/voronoitreemap/treemap/VoronoiTreemap")
  root_polygon <- rJava::.jnew("kn/uni/voronoitreemap/j2d/PolygonSimple")
  scale_factor <-
    20000 # It seems to need to work with whole numbers
  for (i in 1:length(poly_x)) {
    rJava::.jcall(root_polygon,
                  "V",
                  "add",
                  poly_x[i] * scale_factor,
                  poly_y[i] * scale_factor)
  }
  rJava::.jcall(treemap, "V", "setRootPolygon", root_polygon)
  rJava::.jcall(treemap, "V", "setTreeData", treedata)
  rJava::.jcall(treemap, "V", "setCancelOnMaxIteration", TRUE)
  rJava::.jcall(treemap,
                "V",
                "setNumberMaxIterations",
                as.integer(max_iterations))
  rJava::.jcall(treemap, "V", "setErrorAreaThreshold", as.double(max_error))
  rJava::.jcall(treemap, "V", "setUniformWeights", FALSE)
  rJava::.jcall(treemap, "V", "setNumberThreads", as.integer(n_threads))
  thread <- rJava::.jcall(treemap, "Ljava/lang/Thread;", "computeLockedNewThread")
  list(root_polygon = root_polygon,
       treemap = treemap,
       tree = tree,
       thread = thread,
       scale_factor = scale_factor)
}

voronoi_treemap_finish <- function (handle, timeout = NA) { 
  terminated <- rJava::.jfield("java/lang/Thread$State", NULL, "TERMINATED")
  start_time <- as.numeric(Sys.time())
  repeat {
    if (rJava::.jcall(handle$thread, "Ljava/lang/Thread$State;", "getState") == terminated) break
    Sys.sleep(1)
    if (!is.na(timeout) && as.numeric(Sys.time()) - start_time > timeout) return(tibble::tibble())
  }
  root_polygon_area <- rJava::.jcall(handle$root_polygon, "D", "getArea")
  out <- handle$treemap
  out <- as.list(out)
  out <- lapply(out, function(n) {
    pathString <- rJava::.jfield(n, "S", "name")
    # weight <- rJava::.jcall(.$n, "D", "getWeight")
    level <- rJava::.jcall(n, "I", "getHeight")
    cur_node <- n
    repeat {
      polygon <-
        rJava::.jcall(cur_node,
                      "Lkn/uni/voronoitreemap/j2d/PolygonSimple;",
                      "getPolygon")
      if (!is.null(polygon)) {
        break
      }
      # If a parent only has one child, no polygon is created,
      # so inherit the parent's polygon for consistency.
      cur_node <-
        rJava::.jcall(cur_node,
                      "Lkn/uni/voronoitreemap/treemap/VoroNode;",
                      "getParent")
    }
    polygon_area <-
      rJava::.jcall(polygon, "D", "getArea") / root_polygon_area
    xs <- rJava::.jcall(polygon, "[I", "getXpointsClosed") / handle$scale_factor
    ys <- rJava::.jcall(polygon, "[I", "getYpointsClosed") / handle$scale_factor
    centroid <-
      rJava::.jcall(polygon,
                    "Lkn/uni/voronoitreemap/j2d/Point2D;",
                    "getCentroid")
    centroid_x <- rJava::.jfield(centroid, "D", "x") / handle$scale_factor
    centroid_y <- rJava::.jfield(centroid, "D", "y") / handle$scale_factor
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
  })
  out <- tibble::as_tibble(dplyr::bind_rows(out))
  tree_table <-
    do.call(data.tree::ToDataFrameTree, c(handle$tree, "pathString", "name", handle$tree$fieldsAll))
  tree_table <- dplyr::filter(tree_table, pathString != handle$tree$pathString) # remove parent
  tree_table <- dplyr::left_join(tree_table, out, by = "pathString")
  
  # calculate area errors for each leaf
  tree_leaves <- data.tree::ToDataFrameTable(handle$tree, "pathString", "weight")
  tree_leaves <- dplyr::mutate(tree_leaves, weight = replace(weight, is.na(weight), 1.0),
                               weight = weight / sum(weight))
  area_errors <- dplyr::left_join(tree_leaves, out, by = "pathString")
  area_errors <- dplyr::group_by(area_errors, pathString)
  area_errors <- dplyr::summarise(area_errors, polygon_area = dplyr::first(polygon_area),
                                  weight = dplyr::first(weight))
  area_errors <- dplyr::mutate(area_errors, area_error = (weight - polygon_area) / weight)
  area_errors <- dplyr::select(area_errors, pathString, area_error)
  
  dplyr::left_join(tree_table, area_errors, by = "pathString")
}
