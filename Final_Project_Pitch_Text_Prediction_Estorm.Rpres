Text Prediction App for SwiftKey
========================================================
author: Elizabeth Storm
date: March 31, 2019
autosize: true

Purpose of the App
========================================================

Have you ever wished your phone could help you text faster?
Well this is for you! SwiftKey is a company dedicated to improving the keyboard experience on phones. You can learn more about SwiftKey at the link below:
<https://www.microsoft.com/en-us/swiftkey/about-us>.

For my capstone in the Data Science Specialization, I have built a similar app as SwiftKeys to help with the following:
- Predict the most likely next work in your sentence
- Recommend other words that you might also want to pick from

How did I do this?

Algorithm Behind the App
========================================================

Since this was my first time working with a text dataset, I am using a simple Backoff Model. The algorithm takes the last n-1 words of the sentence and finds it in the appropriate n-gram frequency table. I enhanced it to also search all variations of n-grams, such as looking for the last two words in the 4-gram table, taking the 1st & 2nd, 2nd & 3rd, and 1st and 3rd word cominations. 

For example: The 4-gram "President Barack Obama bypassed" will return "bypassed as the next word and is available for matching as: 
- "President Barack Obama" 
- "President Barack 
- "President Obama"
- "Barack Obama"

If the last three words of a sentence are "then President Obama" then the algorithm will return "bypassed" based on matching the 1st and 3rd word from the 4-gram above.

For each n-gram found in the dataset, the frequency is scored, such that 4-grams get the most weight and 1-grams get the least. 
        Score = Frequency of n-gram / sum(all frequencies) * weight
The predicted word with the highest score is chosen.


How to Use the App
========================================================

Over all the app is really simple to use:

- On the left side of the App, there is space to enter text. 
- In the main panel, the input text is repeated.
- Below that is the Predicted Word.
- Additionally, a list of all top 5 predicted words is provided. 

Note: when you first launch the app, the default shows you what is predicted if nothing is input. These words are also what the app will default to if your words are not found in the dataset. 



Documentation & Notes on Improvement
========================================================
1. GitHub directory:

<https://github.com/edemmon/Data-Science-Capstone/tree/master/text_prediction>

2. GitHub directory for data creation:

<https://github.com/edemmon/Data-Science-Capstone/blob/master/data_creation.R>

3. Notes on Improvement:

- I generated the dataset of the most frequency n-grams (n=1 to 5) using a small random sample of the full data available. My computing power was limited so my sample data only have ~60K lines. I used stop words in everything but my 1-grams, but removed sparse records with stop words. Adding more records would improve the prediction accuracy.
- Adding parts of speach and getting into more complicated Natural Language Processing would also make a better model. I am just glad this one runs.
