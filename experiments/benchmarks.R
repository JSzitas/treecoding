remove(list=ls())
pkgload::load_all()

n = 100000
p = 6

df <- data.frame( fact = sample(letters[1:6], n, replace = TRUE),
                  matrix(rnorm(n*(p-1)), ncol = p-1))

tree <- random_tree(df)
encoded_tree <- encode( tree, df[1:100,] )

decoded_tree <- decode( tree, encoded_tree )
#
#
#
# # future::plan("multisession")
# # future::plan("sequential")

forest <- encoder_forest(df, n_tree = 10, max_depth = 4, subsample_size = 256)

# benchmark encoding speeds:
bmark <- bench::press(
  size = c( 100,500,1000,2000 ),
  {
    encoded_forest <- encode( forest, df[1:size,] )
    bench::mark(
      decoded_forest <- decode( forest, encoded_forest ),
      iterations = 50
    )
  }
)
