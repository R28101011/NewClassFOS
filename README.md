# Install the packages

```
if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools")
library(devtools)
install_github("R28101011/NewClassFOS")
library(FOS)
```

# Read Dataset

```
data(FOSdt)
```

# Data Preprocessing

```
x1 = prepro(dt_secom$V2)[1:61]
x2 = prepro(dt_secom$V25)[1:379] 
x3 = prepro(dt_secom$V158)[1:751]
x4 = prepro(dt_secom$V190)[1:1536]
x6 = prepro(dt_sonar$V6)[1:200]
```

# Calculate EPC Control Limits
In this package, we provide the implements of the three methods, Class FOS, adaptive FOS, and 3-term FOS, for calculating EPC control limits.
The correspoding R functions are `FOS()`, `FOS_ad()` and `FOS_3terms()`. For example, we want to calculate the EPC control limits for variable `x1`.

```{r}
FOS(x1, alpha = 0.0027)
FOS_ad(x1, alpha = 0.0027, pn = 0.3)
FOS_3terms(x1, alpha = 0.0027, pn = 0.3)
```

Moreover, we want to calculate the Sonar dataset's EPC control limits for variable `x6`.

```{r}
FOS(x6, alpha = 0.0027)
FOS_ad(x6, alpha = 0.0027, pn = 0.3)
FOS_3terms(x6, alpha = 0.0027, pn = 0.3)
```

# Fine-tuning the hyperparameters

We provide the `FineTune()` and `FineTuneWithData()` functions for implementing the fine-tune producer. For example, we would like to find the optimal hyperparameters for the normal distribution with a sample size of 370 and the desired nominal coverage of 0.6.

```{r}
FineTune(n = 370, pn = 0.6, dist="normal")
```
Moreover, we would like to find the optimal hyperparameters for the t distribution with a sample size of 370 and the desired nominal coverage of 0.6.
```{r}
FineTune(n = 370, pn = 0.6, dist="t")
```
Suppose that we have a training and testing dataset with a sample size of 370.
We would like to find the optimal hyperparameters for the desired nominal coverage of 0.7.
```{r}
Train = matrix(rnorm(10000*370), nrow = 10000 )
Test = matrix(rnorm(10000*370), nrow = 10000 )
FineTuneWithData(Train = Train, Test=Test, pn = 0.7)
```
