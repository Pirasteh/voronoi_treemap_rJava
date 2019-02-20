source("../Rvoronoi.R", chdir = TRUE)
source("../geom_text_in_polygon.R", chdir = TRUE)
library(tidyverse)
library(RColorBrewer)
library(colorspace)

theme_nothing = theme(
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_blank()
)

proteomics <- read_csv("Jahn_et_al_CellReports_2018.csv") %>%
  filter(!is.na(mean_massfraction)) %>%
  mutate(weight = mean_massfraction) %>%
  mutate_at(vars(Process:Protein), funs(str_replace_all(., "/", "|"))) %>%
  mutate(pathString = paste(Process,
                            Pathway,
                            Protein,
                            sep = "/")) %>%
  arrange(pathString) %>%
  split(.$condition) %>% # create a separate diagram for each condition
  purrr::map(function(group) {
    tree <- group %>%
      select(-condition) %>%
      as.Node()
    voronoi_treemap_begin(tree) # begins calculating the diagram in a new thread and returns a handle
                                # we can use later to retrieve the result
  }) %>%
  purrr::map2(., names(.), function(handle, n) {
    voronoi_treemap_finish(handle) %>% mutate(condition = n) # blocks until the calculation is complete
  }) %>% bind_rows()

n_colours <- 1000
pal <- rev(colorRampPalette(brewer.pal(4, "RdBu"))(n_colours))

make_gg <- function(h_fc, coloured_level, labelled_levels) {
  lattice <- h_fc %>%
    group_by(condition) %>%
    do({
      lapply(1:max(.$level), function (l, data) {
        data %>%
          filter(level <= l) %>%
          mutate(size = level) %>%
          mutate(level = l)
      },
      .) %>% bind_rows()
    } %>% mutate(condition = first(.$condition)))
  g <-
    ggplot(h_fc %>% filter(level == coloured_level)) + geom_polygon(aes(xs, ys, group = pathString, fill = area_error)) +
    geom_polygon(
      data = lattice,
      aes(xs, ys, group = pathString, size = size),
      colour = "black",
      fill = NA,
      show.legend = FALSE
    ) +
    scale_size_continuous(range = c(1, 0.1)) +
    scale_fill_gradientn(colours = pal) +
    facet_wrap(~ condition) +
    theme_nothing
  for (i in 1:length(labelled_levels)) {
    g <- g + geom_text_in_poly(
      data = h_fc %>%
        filter(level == labelled_levels[[i]]),
      aes(
        centroid_x,
        centroid_y,
        xs = xs,
        ys = ys,
        group = pathString,
        label = protein
      ),
      min.size = 0
    )
  }
  g
}

g <- make_gg(proteomics, 2, c(2))
print(g)
