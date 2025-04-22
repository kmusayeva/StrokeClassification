### Description
This project is a  detailed statistical analysis and predictive modeling of stroke dataset. 
There are more than 5000 instances in this dataset, but only 5% of them are stroke cases: this is a highly imbalanced binary-classification problem.
This problem is usually handled by
  - generating synthetic examples of rare class based on the k-nearest neighbour approach (although from precision medicine point of view it might raise questions)
  - choosing a cutoff threshold based on the ROC curve analysis using validation data set

which increases sensitivity of the learning algorithm or diagnostic tool, at the cost of generating false positives.
Then, it reduces to finding some trade-off between false negatives and false-positives (i.e., incorrectly predicting a non-stroke individual as a stroke case, or vice versa?)

### Predictive Modeling
Logistic regression, random forest and extreme gradient boosting are used for predictive modeling. All perform bad accroding to Matthiew's correlation coefficient (MCC).
But logistic regression is superior according to AUC, F1, MCC measures, and specifity.

Considering logistic regression model as a diagnostic tool, the probability that an individual predicted to have stroke actually has stroke is 17% and the probability that an individual predicted not to have stroke does not have stroke is 98%.


### View the project:

* Rmarkdown report of detailed analysis can be viewed [here](https://kmusayeva.github.io/StrokeClassification/).

* Stroke risk predictor can be tried [here](https://kmusayeva.shinyapps.io/strokeclassification/) via an interactive shiny app.




