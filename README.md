# Classification in Unbalanced Data Problems

## Importance of Investigating Unbalanced Data

Investigating unbalanced data is crucial because it can lead to several issues in classification models:

### Poor Optimization

When data is unbalanced, the decision boundary can be rotated without significantly affecting the objective function. This can result in a flat objective function around the optimal solution, making it difficult to find the true optimal parameters.

### Hessian Matrix Issues

The Hessian matrix, which indicates the curvature of the objective function, can have low values (indicating a flat surface). This means that the model might not be sensitive to changes in parameter values, leading to poor convergence during optimization.

### Model Performance

Unbalanced data can cause the model to be biased towards the majority class, reducing its ability to accurately predict the minority class. This affects the overall performance and reliability of the model.

By generating and analyzing both balanced and unbalanced datasets, this script helps illustrate these issues, providing a visual and mathematical understanding of how unbalanced data affects logistic regression models.

## Installation

To run the main R script, you will need some standard R packages which you can install with the following command:

``` r
install.packages(c("MASS", "ggplot2", "plotly"))
```

## Key Steps of the R Script

1.  **Load Required Packages:**
    -   Load necessary libraries for data generation, plotting, and interactive visualization.
2.  **Set Seed for Reproducibility:**
    -   Set the seed to ensure that the random data generation is consistent across runs.
3.  **Define Number of Points in Each Class:**
    -   Specify the number of data points for the majority and minority classes, allowing for both balanced and unbalanced datasets.
4.  **Define True Logistic Regression Parameters:**
    -   Set the true coefficients for the logistic regression model to create a known decision boundary.
5.  **Generate Data:**
    -   Generate multivariate normal data for both classes, including both feature values and class labels.
6.  **Fit Logistic Regression Model:**
    -   Train a logistic regression model using the combined dataset to estimate the decision boundary.
7.  **Create a Grid for Plotting the Decision Boundary:**
    -   Generate a grid of values to visualize the estimated decision boundary of the logistic regression model.
8.  **Plot the Data and Decision Boundary:**
    -   Use `ggplot2` to plot the data points along with the true and estimated decision boundaries, providing a visual comparison.
9.  **Plot Log-Likelihood Surface:**
    -   Plot the log-likelihood surface using `plotly` to visualize how the model's objective function behaves with different parameter values.
