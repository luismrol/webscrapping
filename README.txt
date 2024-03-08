README WEBSCRAPPING

This file (in spanish) implements a protocol to extract instagram accounts that are related to an account that have been previously defined. 
The input is a .txt called "base_aliados" that includes the names of the main accounts whose alikes you are going to look for. 
1. The algorithm browses the official account in google.
2. The algorithm reads the html from instagram. 
3. The algorithm withdraws the metadata of the alike accounts. 
4. The algorithm gets in to the account of the related user. 
5. The algorithm fills a dataframe and exports it. 