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
start = time.time()
b = BeautifulSoup(r_unparsed,'lxml')
end = time.time()
print end - start

#print b

# ### extract the title and save it into a variable
##### you can use some specific methods of Beautiful Soup or follow the tree
#### print b.find('h1').text
title = b.title.text
print(title)

# ### if there was more than 1 title it would have to be

title = b.find_all('title')
print(title)

# ### extract the description and save it into a variable


desc = b.find('div','summary_text').text.strip()
print(desc)

# ### extract the Rating eg: R and save into a variable

#print b.find('div','subtext').text.strip()[0:2]

#rating = b.find('script',type='application/ld+json').text.strip()
#print(rating)	

rating = json.loads(b.find('script', type='application/ld+json').text)['contentRating']


## extract the actors 
actors = json.loads(b.find('script', type='application/ld+json').text)['actor']

actors_list = []

for actor in actors:
	actors_list.append(actor[u'name'])

def actors(x):
	actors_list = []
	actors = json.loads(x.find('script', type='application/ld+json').text)['actor']
	for actor in actors:
		actors_list.append(str(actor['name']))
	return actors_list

def directors(x):
	directors = json.loads(x.find('script', type='application/ld+json').text)['director']
	directors_list = []
	for director in directors:
		directors_list.append(str(director['name']))
	return directors_list

print directors(b)


# ## create a function that extracts this information of any IMDB movie of your choosing
# ## ^ into the form of a dictionary 

def movie_info(id):
	r = requests.get('https://www.imdb.com/title/{0}/'.format(id))
	b = BeautifulSoup(r.text,'lxml')
	movie_dict = {}
	movie_dict[id] = {}
	movie_dict[id]['title'] = b.title.text
	movie_dict[id]['desc'] = b.find('div','summary_text').text.strip()
	movie_dict[id]['rating'] = json.loads(b.find('script', type='application/ld+json').text)['contentRating']
	movie_dict[id]['actors'] = actors(b)
	# movie_dict[id]['directors'] = directors(b)
	return movie_dict

Adrift = movie_info('tt6306064')
print(Adrift)
