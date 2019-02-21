# Grid representing possible values
probtree <- data.frame(
  handedness = c(0, 0, 1, 1),
  dominance = c(0, 1, 0, 1),
  index = 1:4
)

usethis::use_data(probtree, internal = TRUE)
