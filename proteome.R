source("Rvoronoi.R", chdir = TRUE)
source("geom_text_in_polygon.R")
library(tidyverse)

proteomics <- read_csv("schramm_dnaKJ.csv") %>%
  mutate_at(vars(`hierarchy level 1`:`hierarchy level 4`), funs(factor(.))) %>%
  filter(!(is.na(area_xyl) | is.na(area_glu)))

h <- proteomics %>%
  mutate_at(vars(`hierarchy level 1`:gene), funs(str_replace_all(., "/", "|"))) %>%
  mutate(
    pathString = paste(
      `hierarchy level 1`,
      `hierarchy level 2`,
      `hierarchy level 3`,
      `hierarchy level 4`,
      gene,
      sep = "/"
    )
  ) %>%
  mutate(weight = area_xyl) %>%
  
  filter(weight > 0.0001) %>%
  as.Node()

# random_positions_x <- runif(h$totalCount)
# random_positions_y <- runif(h$totalCount)
#h$Set(relative_x = random_positions_x, relative_y = random_positions_y)

x <- voronoi_treemap(h, max_error = 0.01, max_iterations = 10000)


lattice <- lapply(1:max(x$level), function (l) {
  x %>%
    filter(level <= l) %>%
    mutate(size = level) %>%
    mutate(level = l)
}) %>% bind_rows()
g <-
  ggplot(x %>% filter(level == 4)) + geom_polygon(aes(xs, ys, group = pathString, fill = area_error), colour = NA) +
  geom_polygon(
    data = lattice,
    aes(xs, ys, group = pathString, size = size),
    colour = "black",
    fill = NA,
    show.legend = FALSE
  ) +
  scale_size_continuous(range = c(1, 0.1)) +
  scale_fill_gradient2()

labelled_levels <- c(1,2,3,4)

for (i in 1:length(labelled_levels)) {
  g <- g + geom_text_in_poly(
    data = x %>%
      filter(level == labelled_levels[[i]]),
    aes(
      centroid_x,
      centroid_y,
      xs = xs,
      ys = ys,
      group = pathString,
      label = str_replace(gene, "CCNA_", "")
    ),
    min.size = 1
  )
}

print(g)
