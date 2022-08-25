
remove(list=ls())

pkgload::load_all(compile = FALSE)

n = 100000
p = 10
k = 8
l = 3

df <- data.frame( matrix( sample( letters[1:k],
                                  n*l,
                                  replace = TRUE
                                  ),
                          ncol = l),
                  matrix( rnorm( n*(p) ),
                          ncol = p )
                  )

tree <- random_tree(df, max_depth = 12)
encoded_tree <- encode( tree, df )
decoded_tree <- decode( tree, encoded_tree )

# # future::plan("multisession")
# # future::plan("sequential")

tictoc::tic("Forest fit:")
forest <- encoder_forest(df, n_tree = 30, max_depth = 8, subsample_size = 1000 )
tictoc::toc()
tictoc::tic("Encoding:")
encoded_forest <- encode( forest, df )
tictoc::toc()
tictoc::tic("Decoding time: ")
decoded_forest <- decode( forest, encoded_forest )
tictoc::toc()

accuracy <- function( tbl ) { sum(diag(tbl))/sum(tbl) }
r2 <- function(x, y) {
  1- ( sum((x-y)^2, na.rm = TRUE)/sum(x^2))
}


sprintf( "Average Categorical reconstruction accuracy: %f",
         mean(sapply( colnames(df)[1:l],function(column){ accuracy( table(df[[column]], decoded_forest[[column]]) )  }))
       )
sprintf( "Average Numeric reconstruction R2: %f",
         mean(sapply( colnames(df)[((l+1):(p+l))],function(column){ r2( df[[column]], decoded_forest[[column]] )  }))
)



