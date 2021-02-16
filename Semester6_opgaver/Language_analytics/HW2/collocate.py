#!/usr/bin/env python

#Importing libraries:

import re #regex
import string #string
import math
import os
from pathlib import Path
import csv 



#creating parameters:
keyword = "black"
window_size= 50 
data_path = os.path.join("..", "data", "100_english_novels", "corpus") # The path to the corpus
output = os.path.join("..", "collocate.csv") # Destination for the endproduct


#Defining functions that will be used in the main function

#Deffining the tokenize funktion
def tokenize(input_string):

    input_string = input_string.lower()
    token_list = re.findall(r'\b\w+\b', input_string) # The regex makes sure that we only gets normal words.
    # re.findall returns all non-overlapping matches of pattern in a string, as a list of strings.
    
    return token_list

#Concordance lines

#Deffining a function with the following parameters:
    # text = text file of the
    # keyword = The word we are centuring the collocation around.
    # Window_size = the number of characters that comes before the word. if nothing is put in it will be 50
def kwic(text, keyword, window_size=50):
    
    lines = [] #creating an empty list, to contain the lines gathered from the function, when they have been through the for-loop
    
    # re.finditer looks at the keyword, in the text.
    for match in re.finditer(keyword, text):
        
        # word_start matches all first character index of the pattern that is the keyword.
        word_start = match.start()
        
        # word_end matches all last character index of the pattern that is the keyword.
        word_end = match.end()
        
        # Left window. the number of characters that comes before the word.
        # if nothing is put in the parameter window size we get the 50 charactors before the keyword starts.
        left_window_start = max(0, word_start-window_size) 
        
        #square brackets to indicate we are indexing and slicing.
        left_window = text[left_window_start:word_start] 
        
        # Right window
        right_window_end = word_end + window_size
        right_window = text[word_end : right_window_end]
        
        # print line
        line = f"{left_window} __{keyword}__ {right_window}"
        line = tokenize(line) #tokenizing the line with the tokenize function deffined above.
        lines.append(line) #appending it to the list deffined before the forlook, so it is accesseble put of the loop.
    
    
    return lines # the list lines it what is returned from the function, when it is called.
    #print (lines)
    

    
    
#Deffining main function:    

def main():
    

    
#creating corpus:
    corpus = "" # Creating an empty container. this time as the "text" parameter is a string.

    for filename in Path(data_path).glob("*.txt"): #Creating a for loop that can run through the novels in the folder corpus.
        with open(filename, "r", encoding="utf-8") as file: #Inside the for loop, I open each file as a file.
        
            loaded_text = file.read() #reading the files.
        
            corpus = corpus + loaded_text # adding the files to "corpus", which is now a sting containing all the text in the corpus.
            

    

#Tokenizing corpus 

    #Using kwic with corpus as a parameter and saving the lines.
    tokenized_corpus = tokenize(corpus)
    
    #Using tokenized on the corpus and saving it.
    tokenized_lines = kwic(corpus, keyword, window_size)
    
    
    
# Counting collocates:

    #Create a list of collocates
    collocate_list = [] #now creating a list for collocates
    index_number = 0 # creating a variable called index_number, to use in the loops below

# The first for loop runs through a list of lists. That means each list has it's own index.
    for line in tokenized_lines:
        
# This for loop only runs through the line of the index it is currently on, and then moves on as the index_number changes
        for token in tokenized_lines[index_number]:
            if token not in collocate_list:
                collocate_list.append(token) #if the token is not already in the collocate_list it is added, to the list.
        index_number = index_number + 1 # Making sure the index number grows with 1 everytime the nested for loop is done.
        
        
        
    #Removing the incomplete words, the words that has been cut in half by the kwic function.
    for collocate in collocate_list: #run through the collocate_list
        if collocate not in tokenized_corpus: #if the incompleet word is not in tokenized_corpus (which contains all words in the corpus, in a list.):
            collocate_list.remove(collocate) #remove it from the collocate list.
            
# Creating for loops that can count the number of collocates in each line
    collocate_counts = []
    for collocate in collocate_list: #running through every collocate in the collocate_list
        count = 0 #creating a count variable
        for line in tokenized_lines:  # running throught the lines in lines_tokenized
            count = count + line.count(collocate)# for each line we update the count by adding adding to the count everytime a collocae is found.
        collocate_counts.append(count)#and appending it to the collocate_count

        


        
        
#Creating the csv, with the values:

    with open(output, mode = "w") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=["collocate", "raw_frequency", "MI"])
        writer.writeheader()
        
        N = len(tokenized_corpus) #The total number of words in our corpus
        u = tokenized_corpus.count(keyword) #The total number of keyword in our corpus.  
        
        
        index_number = 0 # resetting index_number to 0 as we need to use it agian.
        
        for collocate in collocate_list: 
            # Inside the loop we count how often the keyword appears with the collocate.
            line_keywords = 0
            for line in tokenized_lines:
                if collocate in line:
                    line_keywords = line.count(keyword) + line_keywords
              
            #The rest of the values are calculated:
            v = tokenized_corpus.count(collocate) # v = the collocate
            O11 = collocate_counts[index_number] #O11= collocate and keyword together
            O12 = u - line_keywords #O12 = keyword without collocate?
            O21 = v - O11 #O21 collocate without keyword
            R1 = O11 + O12
            C1 = O11 + O21
            E11 = (R1*C1)/N 

            #If E11 has to be positive
            if E11 <= 0:
                continue
            
            else: # calculating the MI
                MI = math.log(O11/E11)
                
                # using writer.writerow to write rows into csv files.
                writer.writerow({"collocate": collocate, "raw_frequency": O11, "MI": MI})
                print(f"Word: {collocate}, frequency: {O11}, MI: {MI}")
            
            index_number = index_number + 1    

            # making sure it can be called from the command line.
if __name__=="__main__":
    main()