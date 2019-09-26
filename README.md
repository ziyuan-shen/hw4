# STA 523 :: Homework 4

wercker-badge-here

## Introduction

You will work with semi-synthetic data on meteorite landings comprised by
The Meteoritical Society. The data was not tidy from the start and was made
less tidy by me.

The main purpose of this assignment is to get practice tidying data via
`tidyr`, `purrr`, and regexs. The details
below will give you some hints on how best to achieve tidy data and what it
means in this context.

## Tasks

Convert `nasa` into a tidy data frame. The following variables should be included
in your final tidy data frame: `name`, `id`, `features`, `nametype`, `recclass`, 
`mass`, `fall`, `year`, `month`, `day`, `hours`, `minutes`, `seconds`, 
`reclat`, `reclong`, `geo_coord`.

#### Task 1

Transform list `nasa` into a data frame called `nasa_df`. Try as much as 
possible to avoid referencing variables by position or name.
Unimportant variables may be removed in this process; however, parsing 
individual data values, correcting errors, converting variable types, and so on,
should be left for task 2.

<i>
Your score will depend on your code's efficiency, quality, and correctness.
In this setting, `map()` and `apply()` variants are much better than loops.
</i>

#### Task 2

Tidy `nasa_df` so it only contains the relevant variables mentioned above.

Below are some hints to help you get `nasa_df` tidy.

1. Each row should be a unique meteorite landing.

2. Your variables should be of a workable and reasonable type. For example,
	numeric-style variables should not be of type raw.

3. At no point in your code should you output the entire list/data frame.

4. Values may need to be parsed and cleaned; obvious mistakes should be 
	corrected or handled appropriately.

5. Create helper functions.

<i>
Your score will depend on your code's efficiency, quality, and correctness.
</i>

#### Task 3

Document your tidying process. Non-obvious choices should be justified. Your
write-up should clearly and concisely reflect your code. This documentation
should supplement your code comments.

## Essential details

#### Deadline and submission

**The deadline to submit Homework 4 is 11:59pm on Thursday, October 3.** 
Only the code in the master branch will be graded.

#### git / GitHub

Each group will have a master branch and others with a GitHub name as a prefix.
One will be your GitHub name. You will only be able to push to your branch as 
the master branch is protected. To get your work into branch master 
(the only branch that will be graded), initiate a pull request on GitHub. 
This will then merge your work into the master branch upon approval by one of 
your teammates.

#### Help

- Post your questions in the #hw4 channel on Slack. Explain your error / problem
  in as much detail as possible or give a reproducible example that generates 
  the same error. Make use of the code snippet option available in Slack.

- Visit the instructor or TAs in office hours.

- The instructor and TAs will not answer any questions within the first 24
  hours of this homework being assigned, and they will not answer questions
  within 6 hours of the deadline.

#### Academic integrity

This is a group assignment. You should not communicate specifics about this
assignment with other groups. As a reminder, any code you use or find as 
inspiration must be cited. Include a references section in your Rmd file if
needed.

>Duke University is a community dedicated to scholarship, leadership, and 
service and to the principles of honesty, fairness, respect, and accountability.
Citizens of this community commit to reflect upon and uphold these principles 
in all academic and non-academic endeavors, and to protect and promote a culture
of integrity. Cheating on exams and quizzes, plagiarism on homework assignments 
and projects, lying about an illness or absence and other forms of academic 
dishonesty are a breach of trust with classmates and faculty, violate the [Duke 
Community Standard](https://gradschool.duke.edu/academics/academic-policies-and-forms/standards-conduct/duke-community-standard),
and will not be tolerated. Such incidences will result in a 
0 grade for all parties involved as well as being reported to the [University 
Judicial Board](https://gradschool.duke.edu/academics/academic-policies-and-forms/standards-conduct/judicial-code-and-procedures). 
Additionally, there may be penalties to your final class grade. 
Please review [Duke's Standards of Conduct](https://gradschool.duke.edu/academics/academic-policies-and-forms/standards-conduct).

#### Grading

**Topic**|**Points**
---------|----------:|
Task 1 |  11
Task 2 |  11
Task 3 |   6
2 commits minimum / member | 2
**Total**|**30**

- *Documents that fail to knit will receive a 0*.
