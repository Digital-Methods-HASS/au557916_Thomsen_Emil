#!/usr/bin/env python
# coding: utf-8

# # Assignment 1: Word Count
# __Basic scripting with Python__
# 
# Using the corpus called 100-english-novels found on the cds-language GitHub repo, write a Python programme which does the following:
# 
# - Calculate the total word count for each novel
# - Calculate the total number of unique words for each novel
# - Save result as a single file consisting of three columns: filename, total_words, unique_words

# In[38]:


#First i have to acces all the files, by loading them in to the script.

import os #Importing operating system


# In[39]:


from pathlib import Path #Importing Path that we are going to acces our files with


# In[40]:


data_path = os.path.join("..", "data", "100_english_novels", "corpus") # I save the path to where the novels are saved.


# In[41]:


#creating a container for the total word count
wordcount = ["  Filename                                                TotalWC  UniqueWC"]


# In[42]:


for filename in Path(data_path).glob("*.txt"): #Then I create a for loop that can run through the novels in the folder corpus, that we just createt a path to.
    with open(filename, "r", encoding="utf-8") as file: #Inside the for loop, I open each file as a file.
        loaded_text = file.read() #Then I read the file.
        split_text = loaded_text.split() #I split the loaded text on whitespace. Each word is now a an item in a list in each text.
                                         #This enable me to count the number of words in the text.
        unique_set = set(split_text) #Using the set function to get the unique wordcount for each text. Set only counts words once.
        
        wordcount.append(f"{filename}, {len(split_text)}, {len(unique_set)}." + "\n")


# In[43]:


print(len(wordcount[1]))
wordcount[1]


# In[44]:


wordcount


# In[45]:


#making list into a string so it can be written to a file

string_wordcount = '\n'.join(wordcount)


# In[47]:


#Writing the file to a document.
wordcount_file = os.path.join("..", "data", "test_files", "wordcount.txt")
#We are writing out the path to where the document is being saved and naming the file wordcount.txt


# In[48]:


with open(wordcount_file, "w", encoding="utf-8") as file:
    file.write(string_wordcount)
#Here we put the content of our string into the file.

