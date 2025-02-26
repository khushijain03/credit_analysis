## Credit_analysis

### Overview
This project analyzes loan application data to predict potential loan defaulters. By leveraging machine learning models and statistical analysis, we provide insights to help banks and financial institutions make informed loan decisions.

### Problem Definition
Loan defaulter prediction is a binary classification problem. Borrowers are classified as defaulters or non-defaulters based on their financial history, loan details, and personal information.

### The goal is to:<br/>
✅ Identify probable defaulters.<br/>
✅ Provide suggestions for informed decision-making.<br/>
✅ Compare features to identify loan applicants most in need.

### Target Audience
- Banks & Financial Institutions – To improve loan approval strategies.
- Students – Seeking education loans.
- Home & Vehicle Loan Borrowers – To understand loan approval probabilities.
- Non-Profit Organizations – To target financial aid effectively.

### Dataset
Used two datasets for this analysis:
- current_app.csv – Contains details of current loan applications, including repayment status.
- previous_app.csv – Holds information on past loan applications (approved, rejected, canceled).
Since these files exceeded GitHub's upload limits, they have been compressed.

### Exploratory Data Analysis (EDA)<br/>
EDA was performed to:<br/>
✅ Discover patterns.<br/>
✅ Spot anomalies.<br/>
✅ Test hypotheses.<br/>
✅ Visualize key features.

### Key Insights:<br/>
📌 Loan Type: More people opt for cash loans than revolving loans.<br/>
📌 Marital Status: Married individuals are the most frequent loan applicants.<br/>
📌 Property Ownership: Homeowners are more likely to take loans (collateral availability).<br/>
📌 Gender-Based Defaulting: Single men default more often than single women.<br/>
📌 Education Impact: People with academic degrees take higher loan amounts.

### Machine Learning Models Used
Implemented and compared multiple models for predicting loan defaults:<br/>

- Decision Tree:  Models decision paths to classify defaulters.<br/>
- Naive Bayes: Uses probability-based classification.<br/>
- K-Nearest Neighbors (KNN): Classifies based on nearest data points.<br/>
- Feature transformations were applied before model fitting to enhance accuracy.<br/>

## How to Run the Project

### Extract the Data
Before using the data, extract the files inside the data folder:

```
cd data
unzip current_app.csv.zip
unzip previous_app.csv.zip
```

1. Clone the Repository
```
git clone https://github.com/khushijain03/credit_analysis.git
cd credit_analysis
```
2. Install Dependencies (R Libraries)
```
install.packages(c("dplyr", "ggplot2", "tidyverse", "data.table"))
```
3. Run the Analysis
```
source("src/analysis_script.R")
```

## Contributing
I welcome contributions! If you find any improvements, feel free to open an issue or a pull request.
