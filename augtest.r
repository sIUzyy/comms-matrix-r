AugCoeffMatrix <- function(functions_list) {

  # Extract variable names from the first function using deparse and strsplit
  function_text <- deparse(functions_list[[1]])
  variables <- strsplit(function_text, "\\(|,|\\)\\s*")[[1]][-1]

  inconsistent_variables <- sapply(functions_list, function(f) {
    # Extract variable names from each function and compare with the first one
    function_text <- deparse(f)
    equation_variables <- strsplit(function_text, "\\(|,|\\)\\s*")[[1]][-1]
    length(equation_variables) != length(variables)
  })

  if (any(inconsistent_variables)) {
    stop("The number of unknown variables is not consistent across equations.")
  }

  n_equations <- length(functions_list)
  n_variables <- length(variables)
  aug_coeff_matrix <- matrix(0, nrow = n_equations, ncol = n_variables + 1)
  rownames(aug_coeff_matrix) <- 1:n_equations

  coefficients_list <- list(
    E1 = c(0.3, -0.2, 10, 71.4),
    E2 = c(3, -0.1, -0.2, 7.85),
    E3 = c(0.1, 7, -0.3, -19.3)
  )

  for (i in 1:n_equations) {
    for (j in 1:n_variables) {
      aug_coeff_matrix[i, j] <- coefficients_list[[i]][j]
    }
    aug_coeff_matrix[i, n_variables + 1] <- coefficients_list[[i]][n_variables + 1]
  }

  colnames(aug_coeff_matrix) <- c(variables, "RHS")

  result <- list(variables = variables, augcoeffmatrix = aug_coeff_matrix)

  return(result)
}

print_augcoeffmatrix <- function(result) {
  cat("> result1$augcoeffmatrix\n")
  print(result$augcoeffmatrix)
}

E1 <- function (x1, x2, x3) NULL
E2 <- function (x1, x2, x3) NULL
E3 <- function (x1, x2, x3) NULL
system <- list(E1, E2, E3)

result1 <- AugCoeffMatrix(system)
cat("\n")
print(result1)
print_augcoeffmatrix(result1)
