---
layout: page
title: Frequently Asked Questions
---

This page will be updated as the class progresses with common questions I receive from students. I recommend that you check this page when you have a question to see if it has already been answered. If not, send me an email, and if the answer to your question might benefit the whole class I'll post it here as well. Note that references are to the [OpenIntro Statistics, 2nd Edition](http://www.openintro.org/stat/textbook.php?stat_book=os).

If you have a question about something in the textbook that looks like it might be a typo or doesn't make sense, first check to see if it's been corrected already on the [textbook errata](https://docs.google.com/spreadsheet/ccc?key=0AuDHaDEbiOGkdDhLbXVsZHR4U0RDaUsxR2dQMEJpOFE#gid=0). If not, send me an email about it so that I can confirm whether it's a typo or not, and if it is, it'll be added to the list.


### Conceptual:

1. [What is the difference between an explanatory and response variable?](#c1)
2. [What exactly is a confounding variable?](#c2)
3. [What exactly is the difference between stratified sampling and cluster sampling?](#c3)
4. [What is meant by a ``typical" observation?](#c4)
5. [If a distribution is extremely skewed or has extreme outliers should we use the standard deviation to describe the variability since it will take into account these extreme observations as well?](#c5)

### R related:

1. [My lab report gives an error when I click on KnitPDF and it doesn't compile OR it does, however my code, plots, output do not show up in the compiled document. What is going on?](#r1)
2. [If I have counts for categorical data in a table form how do I recreate the data in R?](#r2)
3. [My dataset is in a CSV or RData file on my computer. How can I get it into RStudio?](#r3)
4. [Where can I find a list of R commands we've learned so far?](#r4)
5. [How can I export my .Rmd file so that I can submit it on Sakai?](#r5)
6. [How can I take a random sample of cases from my dataset?](#r6)
7. [How can I make a plot visualizing the relationships between all of the variables in my dataset?](#r7)
8. [How can I calculate confidence intervals for the slopes in linear regression using R?](#r8)
9. [How can I create a new variable based on values in an existing variable in my dataset?](#r9)
10. [How can I resize my plots in RMarkdown?](#r10)

### General:

1. [Will tables be provided with the exams, or should we incorporate them into our cheat sheets?](#g1)
2. [Can the cheat sheets be typed?](#g2)
3. [I forgot my clicker today, can I write my responses on a piece of paper and get credit?](#g3)
4. [I have a class on East Campus before this class and I might be late to class. Will this affect my grade?](#g4)
5. [Is the final cumulative?](#g5)


* * *
* * *

<a name="c1">1. What is the difference between an explanatory and response variable?</a>

To identify the explanatory variable in a pair of variables, identify which of the two is suspected of affecting the other. We're saying ``suspected" here because in order to make a causal statement we need to have a randomized experiment. But these terms are defined for all types of studies. For example, let's say we're interested in finding out if studying for an exam with the TV on affects exam performance. Below are two studies investigating this issue:

* At the beginning of an exam ask students if they have studied for the exam while also watching TV. Once the exams are graded, compare the average exam scores of those who studied with the TV on and those who studied without.
* Randomly assign a group of students to study for an upcoming exam while watching TV and others to study without the TV on. Once the exams are graded, compare the average exam scores of those who studied with the TV on and those who studied without.

In both scenarios the explanatory variable is studying with the TV on or off, and the response variable is exam score. However in the first scenario we have an observational study. Therefore even if we find a significant difference between the average scores of the two groups, we can't make a causal connection between watching TV and exam performance. The second study is a randomized experiment, therefore a causal connection can be made between the explanatory and response variables.

*Review section 1.3.4 for more information.*

* * *

<a name="c2">2. What exactly is a confounding variable?</a>

A confounding variable (also called a lurking variable) is a variable that is correlated with both the explanatory and response variables. Confounding variables are basically the reason why we cannot make causal connections between the explanatory and response variables in observational studies. For example, a study has found that the 20 year survival rate (how likely is a subject to be alive in 20 years) for smokers was higher than that of non-smokers, which seems to indicate that smokers are less likely to die. This seems to contradict what we know about the health effects of smoking. A closer look at the data shows that majority of the smokers were young people, and majority of the non-smokers were old people. In this case age seems to be a confounding variable. What is causing the survival rate to appear lower for the smokers is that they're younger (hence more likely to live longer), not that smoking is good for you. Another example on sunscreen use and skin cancer is provided in the textbook.

*Review section 1.4.1 for more information.*

* * *

<a name="c3">3. What exactly is the difference between stratified sampling and cluster sampling?</a>

In both cases we first divide our target population into smaller groups. The main difference between stratified and cluster sampling is that in stratified sampling the groups (strata) are homogeneous with respect to a variable we think might have an effect on the response variable. For example, say we're studying the effect of of watching TV on academic performance at a particular college. We might want to make sure that we include equal numbers of first-years, sophomores, juniors, and seniors in our study. So we divide up our target population into four strata based on year, and randomly sample students from within each sample. This will probably require getting a list of all students from the registrar, randomly picking a given number of students from each year, and finding these students and collecting information on how much they watch TV and their academic performance, like GPA. Sounds like a tedious process... Alternatively we might randomly pick a few courses from the list of all courses offered at this college and go to the classrooms for these students and sample everyone in each of the classes we picked. In this case the classes are the groups (clusters), and they're probably not homogeneous groups. We simply picked them to make data collection a little more manageable.

*Review section 1.4.2 for more information.*

* * *

<a name="c4">4. What is meant by a ``typical" observation?</a>

Depending on the shape of the distribution the typical observation can be the mean (symmetric) or the median (skewed).

* * *

<a name="c5">5. If a distribution is extremely skewed or has extreme outliers should we use the standard deviation to describe the variability since it will take into account these extreme observations as well?</a>

Au contraire, you want to use the IQR in this case to describe variability in distributions with extreme observations. The IQR is robust to these observations, and this is preferred because we want statistics that describe the bulk of the data. However, as usual when describing distributions, you should always mention shape, center, spread, and unusual observations. So these extreme cases, which may be the most interesting ones, still get their place in the spotlight, we just don't want to factor them into the calculation of statistics that describe the distribution as a whole.

*Review section 1.6.6 for more information.*

* * *
* * *

<a name="r1">1. My lab report gives an error when I click on KnitHTML and it doesn't compile OR it does, however my code, plots, output do not show up in the compiled document. What is going on?</a>

There are two common errors, first check if your issue is related to one of these:

1. 
    * Error: > sign in code chunks in lab report - This sign in R means it's done processing the previous command and is ready to accept another. When you copy and paste your code from the console to your lab document you should not include that sign.
    * Solution:</font> Get rid of > signs at the beginning of your commands in your lab report and reprocess. If this was the only issue, it should now be resolved, and your document should now look how you expect it to.
2. 
    * Error: code outside of chunk - In your markdown document all R code should be in code chunks indicated by the three ticks and the `{r}` (it is highlighted in a light gray band). If you insert your code outside of these allotted code chunk areas, the code won't process and the document might give you an error or might just not look how you expect it to.
    * Solution: Place all code in the allotted code chunks. If this was the only issue, it should now be resolved, and your document should now look how you expect it to.

If your problem isn't related to one of these, send me an email with your lab report document attached, or copy and paste your entire lab report at the bottom of your email and I'll help you figure out the issue.

* * *

<a name="r2">If I have counts for categorical data in a table form how do I recreate the data in R?</a>

You can make use of the `rep()` (repeat) function in R to do this. 

Here is a small example. Let's say I have the following contingency table: 

|---------------+---------+-------------+-------|
|               | married | not married | total |
|:-------------||:-------:|:-----------:|:-----:|
| **mature mom**| 25      | 107         | 132   |
| **younger mom**| 361     | 506         | 867   |
| **total**     | 386     | 613         | 999   |
|---------------+---------+-------------+-------|  

In order to recreate a data set where each row represents one respondent, use the following code: 

~~~~~~~~~~
mature = c(rep("mature mom", 132), rep("younger mom", 867))
married = c(rep("married", 25), rep("not married", 107), rep("married", 361), rep("not married", 506))
~~~~~~~~~~

You can make a table using the following code to double check that the data looks like the original contingency table. 

~~~~~~~~~~
table(mature, married)
~~~~~~~~~~

You can also create a new data set where these data are variables make up the columns of the data frame.

~~~~~~~~~~
momdata = as.data.frame(cbind(mature,married))
~~~~~~~~~~

Once you have your data you can write it out to a .csv file using the following. 

~~~~~~~~~~
write.csv(momdata, file = "moms.csv", quote = FALSE, row.names = FALSE)
~~~~~~~~~~

You should now see a file called *mom.csv* in the Files window. You can select that file and export it (check the box next to the file, click on More, and then Export...) so that you can have a copy of this file on your computer, and can submit it with your project. For more information on the `write.csv()` function, use the following. 

~~~~~~~~~~
?write.csv
~~~~~~~~~~

* * *

<a name="r3">3. My dataset is in a CSV or RData file on my computer. How can I get it into RStudio?</a>

Under the Files tab in the bottom right corner of RStudio you should see a button called Upload (with a yellow up arrow). Click on that, and then click on Choose File and find your data file and hit OK. You should then see this file listed in the Files window. 

This means that you have successfully uploaded your file to RStudio, but it's not yet in your Workspace. Loading the dataset to your website requires slightly different methods depending on if it's a CSV or RData file.

**CSV file:** Click on Import Dataset (under the Workspace tab on the top right corner of RStudio), then click on From Text File... 

![from_text_file](from_text_file.png)

and choose your data file from the list. Make sure the radio button for Heading is selected for Yes (assuming that the first row of your dataset is the header row).

![header_yes](header_yes.png)

In order to use this dataset as a part of your write up, you need to include a piece of code in your .Rmd file to read the data in. Suppose your data file's name is "d_prj1.csv", and you want to call your dataset "d" then use the following: 

~~~~~~~~~~
```{r}
d = read.csv("d_prj1.csv")
```
~~~~~~~~~~

You can also simply copy and paste the code R automatically generates when you import the data file into an R chunk in your markdown document.

**RData file:** Since an RData file is already formatted for R, it takes fewer steps to load this type of data. In fact, it's only two steps. Just click on the file in your Files pane. This will prompt a pop-up window asking whether you want to load the R data file into the global environment. Click Yes, and your data should be ready for use.

In order to use this dataset as a part of your write up, you need to include a piece of code in your .Rmd file to read the data in. Suppose your data file's name is "d_prj1.Rdata", use the following: 

~~~~~~~~~~
```{r}
load("d_prj1.Rdata")
```
~~~~~~~~~~

You can also simply copy and paste the code R automatically generates when you import the data file into an R chunk in your markdown document.

* * *

<a name="r4">4. Where can I find a list of R commands we've learned so far?</a>

Click [here](https://stat.duke.edu/~mc301/R/Rcommands.pdf) for a PDF with a list of useful R commands. The list will be updated as we progress through the semester.

* * *

<a name="r5">5. How can I export my files (like HTML or Rmd files) from RStudio so that I can submit it on Sakai?</a>

Locate the file you want to export in the Files pane (lower right corner), check the box next it, then click on More -> Export, and then click on Download in the pop-up window. 

![export](export.png)

* * *

<a name="r6">6. How can I take a random sample of cases from my dataset?</a>

Let's assume you want a random sample of 1000 observations, and you want to sample without replacement. 

This is a two step process:

* First, generate random numbers between 1 and the number of rows in your original dataset, and store these.

~~~~~~~~~~
rows_to_sample = sample(1:nrow(original_data), 1000, replace = FALSE)
~~~~~~~~~~

* Then, grab the rows corresponding to the random numbers from the previous step, and store them in a new data set.

~~~~~~~~~~
samp_data = original_data[rows_to_sample, ]
~~~~~~~~~~

* * *

<a name="r7">7. How can I make a plot visualizing the relationships between all of the variables in my dataset?</a>

The simplest approach is to use the plot function on the entire dataset. The second approach is to use a new function from a contributed R package to get a much fancier plot. The two downsides with the second option are (1) It doesn't handle NAs automatically (this may not be an issue with your second project since there aren't many NAs), and (2) it takes a while to generate the plot so you'll need to be patient. The examples below use the ACS data from the multiple regression lab.

* `plot` function:

~~~~~~~~~~
plot(acs)
~~~~~~~~~~

In this output you'll see that the lower diagonal of the plot matrix has repetitive information from the upper diagonal (same plots, with axes reversed). Also, depending on the number of variables you have, the plots may be small. If R complains about the plotting window being too small, just increase the size of your plotting window by dragging the margins in RStudio. You can use this to quickly determine which variables are related, then make single plots for those relationships that you'd like to view more closely. If you want to plot only certain variables, you can first make a subset, and then use the plot function.

*Subsetting based on column number:* Only plot relationships between variables in columns 1 through 5.

~~~~~~~~~~
plot(acs[,1:5])
~~~~~~~~~~

*Subsetting based on variable names:* First subset the data selecting variables that are numerical, and them plot the relationships between them.

~~~~~~~~~~
acs_num = subset(acs,select = c("income","hrs_work","age","time_to_work"))
plot(acs_num)
~~~~~~~~~~

\\
* `ggpairs` function from the GGally package:

~~~~~~~~~~
install.packages("GGally")  # install package
library(GGally)             # load package
acs_noNA=na.omit(acs)       # omit rows with NAs
ggpairs(acs_noNA)
~~~~~~~~~~

If you only want to plot certain columns of the dataset (say, 1 through 5), use

~~~~~~~~~~
ggpairs(acs_noNA, columns = c(1:5))
~~~~~~~~~~

This might be very useful since otherwise the plot gets very busy. Another parameter you might want to change in the `ggpairs` function is the font size of the correlation coefficients (they're pretty small by default).

~~~~~~~~~~
ggpairs(acs_noNA, columns = c(1:5), params=list(corSize=10))
~~~~~~~~~~

* * *

<a name="r8">8. How can I calculate confidence intervals for the slopes in linear regression using R?</a>

You can calculate confidence intervals for slopes manually (finding the appropriate t* for the degrees of freedom and confidence level you need), or you can use the `confint` function in R. The example below uses the ACS dataset from the multiple regression lab. You can either get confidence intervals for all slopes using:

~~~~~~~~~~
m = lm(income ~ gender + hrs_work, data = acs)
confint(m)
~~~~~~~~~~

or for one parameter at a time using:

~~~~~~~~~~
confint(m, parm = "hrs_work")
~~~~~~~~~~

Use the help file for the function to figure out how to change the confidence level.

~~~~~~~~~~
?confint
~~~~~~~~~~

* * *

<a name="r9">9. How can I create a new variable based on values in an existing variable in my dataset?</a>

Note that methods discussed in this answer can be used for combining levels of a categorical variable, or creating a categorical variable based on a numerical variable, etc.

Let's illustrate this using the ACS dataset:

~~~~~~~~~~
download("http://stat.duke.edu/~mc301/data/acs.RData", destfile = "acs.RData")
load("acs.RData")
~~~~~~~~~~
<br>
**categorical -> categorical:** One of the variables in this dataset is `race`, with levels `white`, `black`, `asian`, and `other` Suppose we don't want this fine grained information and we're interested in whether the respondent is `white/black` or `asian/other`. In order not to overwrite this variable we'll create a new variable in our dataset, called `race_new`. Then, we'll fill in the information for this variable:

~~~~~~~~~~
acs$race_new[acs$race == "white" | acs$race == "black"] = "white/black"
acs$race_new[acs$race == "asian" | acs$race == "other"] = "asian/other"
~~~~~~~~~~
<br>
Remember that `|` means "or".

Lastly, we want to make sure R recognizes this variable as a categorical variable, which R calls factor.

~~~~~~~~~~
acs$race_new = as.factor(acs$race_new)
~~~~~~~~~~
<br>
Now if we peek at the summary for this variable we will see only the two levels we were interested in:

~~~~~~~~~~
summary(acs$race_new)
   asian/other white/black 
           239        1761
~~~~~~~~~~
<br>
**numerical -> categorical:** Let's give one more example, this time creating a new variable based on an existing numerical variable. A quick peek at the summary statistics for the `time_to_work` variable in this dataset tells us that the median time to work is 20 minutes. Suppose we're only interested in whether the time to work is above or below the median. We'll store this information in a new variable we call `time_to_work_cat`.

~~~~~~~~~~
acs$time_to_work_cat[acs$time_to_work > 20] = "above median"
acs$time_to_work_cat[acs$time_to_work <= 20] = "at/below median"
~~~~~~~~~~
<br>
Once again, we need to ensure that the new variable is treated as a categorical variable (rather than just a character string).

~~~~~~~~~~
acs$time_to_work_cat = as.factor(acs$time_to_work_cat)
~~~~~~~~~~
<br>
We can see in the summary output for this variable the number of people whose time work are above median or at or below median as well as that there were some NAs in the dataset that were left as is.

~~~~~~~~~~
summary(acs$time_to_work_cat)
   above median at/below median            NA's 
            334             449            1217 
~~~~~~~~~~

* * *

<a name="r10">10. How can I resize my plots in RMarkdown?</a>

In order to change the size of the plots in your RMarkdown document you need to add some arguments to the piece of code that sets your chunk. These arguments are `fig.width` for width of your figure, and `fig.height` for height of your plot. For example

~~~~~~~~~~
```
{r fig.width = 5, fig.height = 3}
plot(d$var)
```
~~~~~~~~~~

will produce a plot that is 5 x 3 (width x height). Play around with the size until you're happy with it. But don't make your plots too small, they should still be readable.

The <a href="http://www.rstudio.com/resources/cheatsheets/">RStudio cheatsheet</a> (scroll down to R Markdown Cheat Sheet) has more information on how to customize your RMarkdown file. Some of it may be too advanced for what you're trying to do, but some of it might be useful. Feel free to ask on Piazza if you want to do something mentioned there but you're not sure how.

* * *
* * *

<a name="g1">1. Will tables be provided with the exams, or should we incorporate them into our cheat sheets?</a>

Tables will be provided with the exam, you don't need to worry about bringing one to the exam with you.

* * *

<a name="g2">2. Can the cheat sheets be typed?</a>

Sure, but they need to be prepared by you.

* * *

<a name="g3">3. I forgot my clicker today, can I write my responses on a piece of paper and get credit? 

No, you need to have your clicker to be able to get credit for the day. Note that up to two unexcused late arrivals or absences will not affect your clicker grade.

* * *

<a name="g4">4. I have a class on East Campus before this class and I might be late to class. Will this affect my grade?

Yes and no. If there is a readiness assessment that day and you walk in late, you won't be given additional time and you may not be able to perform as well as you would have had you had more time. If there is no readiness assessment and you walk in just a few minutes late, you'll at most miss one or two clicker questions for the day. This shouldn't affect your score since answering at least 75% of the questions gets you a full score for the day.

* * *

<a name="g5">5. Is the final cumulative?

Yes. But it will be weighed more heavily towards material you haven't yet been tested on.
