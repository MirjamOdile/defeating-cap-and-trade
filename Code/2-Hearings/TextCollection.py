# Regular expressions
import re
# Operating system dependent functionality
import os
# Work with json files
import json
# # Parallel processing
# import multiprocessing as mp
# Timer
import time
# Pickle (save data)
import pickle
# Progress bar
from tqdm import tqdm
# Decoding
import unidecode
# Fuzzy matching 
from fuzzywuzzy import fuzz


import codecs
from bs4 import BeautifulSoup

class find_string:

    def first_match(list_of_strings, text):
        for i, string in enumerate(list_of_strings):
            result = re.search(string, text, re.DOTALL)
            if result is not None:
                try: 
                    return result[1]
                except:
                    return result[0]

        return ''

    def first_match_all(list_of_strings, text):
        for i, string in enumerate(list_of_strings):
            result = re.findall(string, text, re.DOTALL)
            if len(result) != 0:
                return result
        return []

    def all_matches_first(list_of_strings, text):
        results = []
        for i, string in enumerate(list_of_strings):
            result = re.findall(string, text, re.DOTALL)
            if len(result) != 0:
                results.append(result[0])
        return results

    def all_matches(list_of_strings, text):
        results = []
        for i, string in enumerate(list_of_strings):
            result = re.findall(string, text, re.DOTALL)
            if len(result) != 0:
                results.append(result)
        return [item for sublist in results for item in sublist]

    def all_matches_nested(list_of_strings, text):
        results = []
        for i, string in enumerate(list_of_strings):
            result = re.findall(string, text, re.DOTALL)
            if len(result) != 0:
                results.append(result)
        return results

# def parallel_processing(function_name, args_dictionary, texts, threads = 2):

#     cutoff = int(len(texts)/threads)
#     dictionaries = []
#     for i in range(threads):
#         args_dictionary['texts_subset'] = texts[i*cutoff : (i+1)*cutoff]
#         dictionaries.append(args_dictionary.copy())
#     pool = mp.Pool(processes = threads)
#     return pool.map(function_name, dictionaries)

def save_as(obj, filename):
    with open(filename, 'wb') as output:  # Overwrites any existing file.
        pickle.dump(obj, output, pickle.HIGHEST_PROTOCOL)

def load(filename):
    with open(filename, 'rb') as input:
        return  pickle.load(input)


from collections import Iterable
def flatten_return_object(lis):
     for item in lis:
         if isinstance(item, Iterable) and not isinstance(item, str):
             for x in flatten(item):
                 yield x
         else:        
             yield item
                
def flatten(lis):
    return list(flatten_return_object(lis))


# Sound alert
from IPython.display import Audio, display


def done():
  display(Audio(url='https://www.myinstants.com/media/sounds/kq-beep.mp3',
                autoplay=True))
# playsound('kq-beep.mp3')
def alert_interrupt():
  display(Audio(url='https://www.myinstants.com/media/sounds/untitled_gZHw8yL.mp3',
                autoplay=True))
def alert():
  display(Audio(url='https://www.myinstants.com/media/sounds/8d82b5_pacman_dies_sound_effect.mp3',
                autoplay=True))


    
def rexify_listofstrings(list_of_strings):
    regex_strings = []
    for string in list_of_strings:
       regex_strings.append(string) #.replace(' ', '\s+'))
    separator = '|'
    regex = separator.join(regex_strings)
    #regex = re.compile(regex)
    return regex 

    
        


class TextCollection:
    def __init__(self, directory, basename, filetype):

        self.directory = directory

        self.texts = []

        try:
            for file in os.listdir(self.directory):
                
                if file.endswith(filetype):
                    
                    f = codecs.open(self.directory + file, 'r', 'utf-8')
                    content_raw = BeautifulSoup(f.read(),"lxml").get_text()

                    file_dict = {'filename': file,
                                 'identifier': file.replace(basename,'').replace(filetype, ''),
                                 'content_raw': content_raw,
                                 'content': content_raw.lower(),
                                 'content_stripped': re.sub(r'[\n\t\s]+', ' ', content_raw.lower())
                                 }
                    
                    self.texts.append(file_dict)
        except KeyboardInterrupt:
            alert_interrupt()
        except Exception as e:
            print('Error! Code: {c}, Message, {m}'.format(c = type(e).__name__, m = str(e)))


    def __len__(self):
        return len(self.texts)

    def add_text(self, file, directory, basename, filetype):

        f = codecs.open(directory + file, 'r', 'utf-8')
        content_raw = BeautifulSoup(f.read(),"lxml").get_text()

        file_dict = {'filename': file,
                     'identifier': file.replace(basename,'').replace(filetype, ''),
                     'content_raw': content_raw,
                     'content': content_raw.lower(),
                     'content_stripped': re.sub(r'[\n\t\s]+', ' ', content_raw.lower())}
                
        self.texts.append(file_dict)
    
 
    def get_key(self, key, index = False, max_index = False):
        """
        Takes a key (and optionally an index and a max_index) and returns the 
        resulting content for each text (including empty lists and None).
        """
        if type(index) == int:
            if not max_index:
                return self.texts[index][key]
            else:
                return [text[key] for text in self.texts[index:max_index]]
        else:
            return [text[key] for text in self.texts]

    
    def get_key_i(self, key, index = False, max_index = False):
        """
        Takes a key (and optionally an index and a max_index) and returns the 
        index and the resulting content for each text (including empty lists and None).
        """
        if type(index) == int:
            if not max_index:
                return index, self.texts[index][key]
            else:
                return [(i, text[key]) for i, text in enumerate(self.texts[index:max_index])]
        else:
            return [(i, text[key]) for i, text in enumerate(self.texts)]


    def print_key(self, key, index = False, max_index = False):
        """
        Takes a key (and optionally an index and a max_index) and prints the 
        resulting content for each text (including empty lists and None).
        """
    
        if type(index) == int:
            if not max_index:    
                print(self.texts[index][key])
            else:
                for text in self.texts[index:max_index]:
                    print(text[key])
        else: 
            for text in self.texts:
                print(text[key])


    def print_key_i(self, key, index = False, max_index = False, all = False, missing = False, return_set = False):
        """
        Takes a key and prints the index and resulting content for each 
        non-empty instance of the key.

        Options: index: specify the text to access
                 index + max_index: specify a range of texts to access
                 all = True includes empty instances;
                 missing = True only shows empty instances;
                 return_set = True shows the set
        """
        if return_set == False:
            if type(index) == int:
                if not max_index:    
                    print(self.texts[index][key])
                else:
                    counter = 0
                    for i, text in enumerate(self.texts[index:max_index]):
                        counter += 1
                        if all:
                            print(index+counter-1, text[key])
                        else:
                            if len(text[key]) != 0:
                                print(index+counter-1, text[key])
            else: 
                for i, text in enumerate(self.texts):
                    if all:
                        print(i, text[key])
                    else:
                        if missing == True:
                            if len(text[key]) == 0:
                                print(i, text[key])
                        else:
                            if len(text[key]) != 0:
                                print(i, text[key])

        else:
            if type(index) == int:
                if not max_index:    
                    print(index, set(self.texts[index][key]) or '{}')
                else:
                    counter = 0
                    for i, text in enumerate(self.texts[index:max_index]):
                        counter += 1
                        if all:
                            print(index+counter-1, set(text[key]) or '{}')
                        else:
                            if len(text[key]) != 0:
                                print(index+counter-1, set(text[key]) or '{}')                   
            else: 
                for i, text in enumerate(self.texts):
                    if all:
                        print(i,set(text[key]) or '{}')
                    else:
                        if missing == True:
                            if len(text[key]) == 0:
                                print(i, set(text[key]) or '{}')
                        else:
                            if len(text[key]) != 0:
                                print(i, set(text[key]) or '{}')

    def print_keys_i(self, key, key1, index = False, max_index = False, all = False, missing = False, return_set = False): #**kwargs
        """
        Takes two keys and prints the index and resulting content for each text
        with non-empty instances of both keys.
        Options: index: specify the text to access
                 index + max_index: specify a range of texts to access
                 all = True includes empty instances; 
                 missing = 'either' shows texts with one empty instance;
                 missing = 'both' shows texts with two empty instances;
                 return_set = True shows the set
        """
        if return_set == False:
            if type(index) == int:
                if not max_index:    
                    print(index, self.texts[index][key], self.texts[index][key1])
                else:
                    counter = 0
                    for i, text in enumerate(self.texts[index:max_index]):
                        counter += 1
                        if all:
                            print(index+counter-1, text[key], text[key1])
                        else:
                            if missing == 'both':
                                if len(text[key]) + len(text[key1]) == 0:
                                    print(index+counter-1, text[key], text[key1])
                            elif missing == 'either':
                                if len(text[key]) + len(text[key1]) != 0 and (len(text[key]) == 0 or len(text[key1]) == 0):
                                    print(index+counter-1, text[key], text[key1])
                            else:
                                if len(text[key]) != 0 and len(text[key1]) != 0:
                                    print(index+counter-1, text[key], text[key1])
            else: 
                for i, text in enumerate(self.texts):
                    if all:
                        print(i, text[key], text[key1])
                    else:
                        if missing == 'both':
                            if len(text[key]) + len(text[key1]) == 0:
                                print(i, text[key], text[key1])
                        elif missing == 'either':
                            if len(text[key]) + len(text[key1]) != 0 and (len(text[key]) == 0 or len(text[key1]) == 0):
                                print(i, text[key], text[key1])
                        else:
                            if len(text[key]) != 0 and len(text[key1]) != 0:
                                print(i, text[key], text[key1])
        else:
            if type(index) == int:
                if not max_index:    
                    print(index, set(self.texts[index][key]) or '{}', set(self.texts[index][key1]) or '{}')
                else:
                    counter = 0
                    for i, text in enumerate(self.texts[index:max_index]):
                        counter += 1
                        if all:
                            print(index+counter-1, set(text[key]) or '{}', set(text[key1]) or '{}')
                        else:
                            if missing == 'both':
                                if len(text[key]) + len(text[key1]) == 0:
                                    print(index+counter-1, set(text[key]) or '{}', set(text[key1]) or '{}')
                            elif missing == 'either':
                                if len(text[key]) + len(text[key1]) != 0 and (len(text[key]) == 0 or len(text[key1]) == 0):
                                    print(index+counter-1, set(text[key]) or '{}', set(text[key1]) or '{}')
                            else:
                                if len(text[key]) != 0 and len(text[key1]) != 0:
                                    print(index+counter-1, set(text[key]) or '{}', set(text[key1]) or '{}')
            else: 
                for i, text in enumerate(self.texts):
                    if all:
                        print(i, set(text[key]) or '{}', set(text[key1]) or '{}')
                    else:
                        if missing == 'both':
                            if len(text[key]) + len(text[key1]) == 0:
                                print(i, set(text[key]) or '{}', set(text[key1]) or '{}')
                        elif missing == 'either':
                            if len(text[key]) + len(text[key1]) != 0 and (len(text[key]) == 0 or len(text[key1]) == 0):
                                print(i, set(text[key]) or '{}', set(text[key1]) or '{}')
                        else:
                            if len(text[key]) != 0 and len(text[key1]) != 0:
                                print(i, set(text[key]) or '{}', set(text[key1]) or '{}')

    def print_key_head(self, key, index = False, max_index = False, min = 0, max = 500):
        """
        Takes one key (and optionally an index and a max_index) and prints the index and the head of each instance (first 500 items/characters).
        Options: min/max changes the start and end point for display (default is set to  min = 0, max = 500).
        """
    
        if type(index) == int:
            if not max_index:    
                print(self.texts[index][key][min:max])
            else:
                for i, text in enumerate(self.texts[index:max_index]):
                    print(i, text[key][min:max],  '\n\n')
        else: 
            for i, text in enumerate(self.texts):
                print(i, text[key][min:max], '\n\n')
                
    def remove_key(self, key):
        for text in self.texts:
            text.pop(key, None)

class HearingsCollection(TextCollection):

    def __init__(self, *args, congress_list_of_strings = ['(?:senate|house|s\.|h\.){1}\s(?:hearing|hrg\.),*\s+(?:\d{3}){1}']): # '(?:\d{3}){1}(?:th)*\scongress']
        super().__init__(*args)
        try:
            for text in self.texts:
                congress = find_string.first_match(congress_list_of_strings, text['content'])
                if len(congress) > 0:
                    text['congress'] = re.findall('\d{3}', congress)[0]
                else: 
                    text['congress'] = ''
            print('{} hearings were loaded.'.format(len(self.texts)))
        except KeyboardInterrupt:
            alert_interrupt()
        except:
            alert()
            print('There has been a problem with adding the congress numbers.')
            
            
    def subset(self, keyword, list_of_values):

        try:
            self.texts = [text for text in self.texts if text[keyword] in list_of_values]
            print('The data now contains {} hearings.'.format(len(self.texts)))
        except KeyboardInterrupt:
            alert_interrupt()
        except:
            alert()

            
    def select_industry(self, sector_selection, industries_selection):
        self.sector = sector_selection
        self.industries = industries_selection
        self.industry = self.sectors[self.sector]['industries'][self.industries]['industry']
        print('{}.{} {} - {}:\n'.format(self.sector, self.industries, 
                                       self.sectors[self.sector]['name'].upper(), self.industry))

        
    def count_witnesses(self, variable_name, keyword, print_witnesses = False, silent = False):
        count_witnesses_counter = 0
        for i, text in enumerate(self.texts):
            for j, witness in enumerate(text['witnesses']):
                if text[variable_name][j] == keyword:
                    count_witnesses_counter += 1
        if silent == False:
            print('There are {} {} witnesses.\n'.format(count_witnesses_counter, keyword))
        else:
            return count_witnesses_counter
        if print_witnesses == True:
            for i, text in enumerate(self.texts):
                for j, witness in enumerate(text['witnesses']):
                    if text[variable_name][j] == keyword:
                        print(i, j, witness)
            print('\n')


    def match_witnesses(self, keywords, antikeywords = [], print_witnesses = False):
        try:
            industry = self.sectors[self.sector]['industries'][self.industries]['industry']
            match_witnesses_counter = 0
            for i, text in enumerate(self.texts):
                for j, witness in enumerate(text['witnesses']):
                    if text['witness_affiliation'][j] == None:
                        if (len(find_string.first_match(keywords, witness.lower())) > 0 and
                            len(find_string.first_match(antikeywords, witness.lower())) == 0):
                            match = find_string.first_match(keywords, witness.lower()).title()
                            match = match.replace('Of ', 'of ').replace('And ', 'and ')
                            match = match.replace('For', 'for').replace('On', 'on').replace(' The ', ' the ')
                            match = match.replace('Usda', 'USDA').replace('Iii', 'III').replace('Nasa', 'NASA')
                            match = match.replace('Mit', 'MIT').replace('Ipcc', 'IPCC')
                            match_witnesses_counter += 1
                            text['witness_affiliation'][j] = match
                            text['witness_sector'][j] = self.sectors[self.sector]['name']
                            text['witness_industry'][j] = industry
                            if print_witnesses == True:
                                print(i, j, witness, '\n', match, '\n\n')
            new_witnesses = self.count_witnesses('witness_industry', industry, silent = True)
            print('{} more {} witnesses were matched, resulting in a total of {} witnesses.'.format(match_witnesses_counter, industry, new_witnesses))
        except:
            print("Unexpected error:", sys.exc_info()[0])
            raise
            
    def search_witnesses(self, keywords, antikeywords = []):
        search_witnesses_counter = 0
        for i, text in enumerate(self.texts):
            for j, witness in enumerate(text['witnesses']):
                if text['witness_affiliation'][j] == None:
                    if (len(find_string.first_match(keywords, witness.lower())) > 0 and
                       len(find_string.first_match(antikeywords, witness.lower())) == 0):
                            match = find_string.first_match(keywords, witness.lower())
#                             match = match.replace('Of ', 'of ').replace('And ', 'and ')
#                             match = match.replace('For', 'for').replace('On', 'on').replace(' The ', ' the ')
#                             match = match.replace('Usda', 'USDA').replace('Iii', 'III').replace('Nasa', 'NASA')
                            print(i, j, witness.lower(), '\n', match, '\n\n') 
                            if text['desmog_witness'][j] != None:
                                print('>> Denialist:', i, j, text['desmog_witness'][j], '\n\n') 
                            search_witnesses_counter += 1
        print(f'{search_witnesses_counter} witnesses were found.')

    # def subset_condition(self, keyword, condition, value):

    #     if condition = <=        
    #     try:
    #         self.texts = [text for text in self.texts if text[keyword] <= value]
    #         print('The data now contains {} hearings.'.format(len(self.texts)))
    #     except KeyboardInterrupt:
    #         alert_interrupt()
    #     except:
    #         alert()

    #     if condition = >=        
    #     try:
    #         self.texts = [text for text in self.texts if text[keyword] >= value]
    #         print('The data now contains {} hearings.'.format(len(self.texts)))
    #     except KeyboardInterrupt:
    #         alert_interrupt()
    #     except:
    #         alert()

    #     if condition = ==        
    #     try:
    #         self.texts = [text for text in self.texts if text[keyword] == value]
    #         print('The data now contains {} hearings.'.format(len(self.texts)))
    #     except KeyboardInterrupt:
    #         alert_interrupt()
    #     except:
    #         alert()

    #     if condition = !=        
    #     try:
    #         self.texts = [text for text in self.texts if text[keyword] != value]
    #         print('The data now contains {} hearings.'.format(len(self.texts)))
    #     except KeyboardInterrupt:
    #         alert_interrupt()
    #     except:
    #         alert()


    def get_keywords_subset(self, dictionary):
        keys = set(dictionary.keys())
        excludes = set(['texts_subset'])
        for key in keys.difference(excludes):
            for text in dictionary['texts_subset']:
                text[key] = find_string.all_matches(dictionary[key], text['content'])
        return dictionary['texts_subset']


    def get_keywords_parallel(self, dictionary_of_lists_of_keywords, threads = 2):

        #try:
        processed_texts_split = parallel_processing(self.get_keywords_subset, dictionary_of_lists_of_keywords, self.texts, threads)
        self.texts = [text for texts_subset in processed_texts_split for text in texts_subset]
        # except KeyboardInterrupt:
        #     alert_interrupt()
        # except:
        #     alert()

### Extract all matching keywords for a dictionary of lists of keywords in the stripped lower case content
    def get_keywords(self, dictionary_of_lists_of_keywords):
        try:
            keys = set(dictionary_of_lists_of_keywords.keys())
            for key in keys:
                for text in self.texts:
                    text[key] = find_string.all_matches(dictionary_of_lists_of_keywords[key], text['content_stripped'])
        except KeyboardInterrupt:
            alert_interrupt()
        except:
            alert()



    def get_titles_subset(self, dictionary):
        for text in dictionary['texts_subset']:
            title = find_string.first_match(dictionary['title_list_of_strings'], text['content'][0:3000])
            if len(title) > 0:
                text['title'] = re.sub(r"[\s]+", ' ', title)
            else:
                text['title'] = temp
        return dictionary['texts_subset']


    def get_titles_parallel(self,  title_list_of_strings = {'title_list_of_strings': ['\n\n\s*(.+?)\s*\n*={10,}', 
                                                                            '\n\n\s*(.+?)\s*\n*_{10,}', 
                                                                            '\n\n\s*(.+?)\s*\n*-{8,}']}, 
                         threads = 2):
        try:
            processed_texts_split = parallel_processing(self.get_titles_subset, title_list_of_strings, self.texts, threads)
            self.texts = [text for texts_subset in processed_texts_split for text in texts_subset]
        except KeyboardInterrupt:
            alert_interrupt()
        except:
            alert()


    def get_titles(self, title_list_of_strings = ['\n\n\s*(.+?)\s*\n*={10,}', 
                                                  '\n\n\s*(.+?)\s*\n*_{10,}', 
                                                  '\n\n\s*(.+?)\s*\n*-{8,}']):
        try:
            for text in self.texts:
                title = find_string.first_match(title_list_of_strings, text['content'][0:5000])
                if len(title) > 0:
                    text['title'] = re.sub(r"[\s]+", ' ', title)
                else:
                    text['title'] = title
        except KeyboardInterrupt:
            alert_interrupt()
        except:
            alert()
            
    def get_dates(self, date_list_of_strings =  ['((january|february|march|april|may|june|july|august|september|october|november|december){1}\s?\d{1,2},?\s?\d{4})']):
        try:
            for text in self.texts:
                text['date'] = find_string.first_match(date_list_of_strings, text['content'].lower())
                text['year'] = re.findall('\d{4}', text['date'])[0]
        except KeyboardInterrupt:
            alert_interrupt()
        except:
            alert()

    def get_committees(self, committee_list_of_strings = ['joint economic committee',
                                                          '[sub]*committe[e]* on[\n]*(.+?)\n', 
                                                          '\n\n(.+?)committe[e]\n',
                                                          '\n(.+?)committe[e]\n']):
        try:
            for text in self.texts:
                committee = find_string.first_match_all(committee_list_of_strings, text['content'][0:2000])
                committee = [re.sub(r'senate|house of representatives', '', i) for i in committee]
                committee = [re.sub(r'[\n\t\s,]+', ' ', i) for i in committee]
                committee = list(map(str.strip, committee))
                committee = [i for i in committee if len(i) < 200]
                if len(committee) > 0:
                    text['committee'] = list(set(committee))
                else:
                    text['committee'] = committee

        except KeyboardInterrupt:
            alert_interrupt()
        except:
            alert()


    def get_mods(self, source):
    	
    	files = os.listdir(source)

    	try:
    		for text in self.texts:
    			for file in files:
    				if file.find(text['identifier']) != -1:
    					f = codecs.open(source + file, 'r', 'utf-8')
    					text['MODS'] = BeautifulSoup(f.read(), 'html.parser')
    	except KeyboardInterrupt:
    		alert_interrupt()
    	except:
    		alert()