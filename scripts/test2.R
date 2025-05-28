library(ape)
library(ggplot2)

# number of species
nspp <- 20

# make up some random traits
trt <- rnorm(nspp)
s <- sd(trt) # SD of those triats

# make up a random phylogeny
tre <- rphylo(nspp, 1, 0.8)

# calculate phylogenetic distances
tre_dist <- cophenetic(tre)

# simulate traits under BM with sigma coming from SD of "actual" traits
sim_trt <- rTraitCont(tre, sigma = s)

# calculate trait distance and concert to matrix
trt_dist <- dist(sim_trt) |> as.matrix()

# combine phylo dist and trait dist in data.frame
df <- data.frame(phylo_dist = tre_dist[lower.tri(tre_dist)], 
                 trait_dist = trt_dist[lower.tri(trt_dist)])

# plot it
ggplot(df, aes(x = phylo_dist, y = trait_dist)) +
  geom_point()



# Sample data frame
df <- data.frame(
  A = c(1, 2, 2, 3, "Z", 3),
  B = c("X", "Y", "Y", "Z", "3", "Z"),
  C = c(10, 20, 30, 40, 50, 60)
)

# Keep only one row for each unique combination of A and B
df_filtered <- df %>% distinct(A, B, .keep_all = TRUE)

# Print result
print(df_filtered)
