###import the necessary packages####

import requests
from bs4 import BeautifulSoup
import json
#import this package if you have encoding errors
# import sys
# sys.setdefaultencoding('utf8')


### extract the HTML into text 

r = requests.get('https://www.imdb.com/title/tt0071853/')

#print r 
r_unparsed = r.text


# ### now make some Soup using Beautiful Soup and XML parser


#print b

# ### extract the title and save it into a variable
##### you can use some specific methods of Beautiful Soup or follow the tree
#### print b.find('h1').text


print(title)

# ### if there was more than 1 title it would have to be


print(title)

# ### extract the description and save it into a variable



print(desc)

# ### extract the Rating eg: R and save into a variable




## extract the actors 
actors = json.loads(b.find('script', type='application/ld+json').text)['actor']




# ## create a function that extracts this information of any IMDB movie of your choosing
# ## ^ into the form of a dictionary 

def movie_info(id):
	### FILL IN YOUR FUNCTION with what you learned above
	return movie_dict

Adrift = movie_info('tt6306064')
print(Adrift)
