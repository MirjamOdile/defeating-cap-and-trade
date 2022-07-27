# Packages

# Operating system dependent functionality
import os
# System-specific parameters and functions
import sys
# JSON encoder and decoder
import json
# Progress bars
from tqdm.auto import tqdm
# URL handling
from urllib.request import urlopen
# HyperText Markup Language support
import html
# Regular expression operations
import re
# Decoding
import unidecode
# Fuzzy string matching 
from thefuzz import fuzz
# Pandas dataframes
import pandas as pd
# Scientific computing 
import numpy as np
# Plotting
from plotly.offline import iplot
# %matplotlib inline

# Functions

def all_matches(list_of_strings, text):
        results = []
        for string in list_of_strings:
            result = re.findall(string, text, re.DOTALL)
            if len(result) != 0:
                results.append(result)
        return [item for sublist in results for item in sublist]
    
def first_match(list_of_strings, text):
        for i, string in enumerate(list_of_strings):
            result = re.search(string, text, re.DOTALL)
            if result is not None:
                try: 
                    return result[1]
                except:
                    return result[0]
        return ''
    
def get_keywords(list_of_dictionaries_of_hearings, dictionary_text_key,
                 dictionary_of_lists_of_keywords):
    keys = set(dictionary_of_lists_of_keywords.keys())
    for key in keys:
        for dictionary in tqdm(list_of_dictionaries_of_hearings):
            dictionary[key] = all_matches(dictionary_of_lists_of_keywords[key],
                                          dictionary[dictionary_text_key].lower())

def drop_variables(list_of_dictionaries, list_of_keys):
        return [{k: v for k, v in d.items() if not k in list_of_keys} for d in list_of_dictionaries]
    
def get_htm(ID):
    print("https://www.govinfo.gov/content/pkg/CHRG-" + ID + "/html/CHRG-" + ID + ".htm")
    
def get_mods(ID):
    print("https://www.govinfo.gov/metadata/pkg/CHRG-" + ID + "/mods.xml")
    
def select_industry(data, sector_id, industry_id):
    sector = data[sector_id]['name']
    industry = data[sector_id]['industries'][industry_id]['industry']
    print('{}.{} {} - {}:\n'.format(sector_id, industry_id, sector.upper(), industry))
    return(sector, industry)

def count_witnesses(data, variable_name, keyword, print_witnesses = False, silent = False):
    count_witnesses_counter = 0
    for i, text in enumerate(data):
        for j, witness in enumerate(text['witnesses']):
            if text[variable_name][j] == keyword:
                count_witnesses_counter += 1
                if print_witnesses == True:
                    print(i, j, witness)
    if silent == False:
        print(' ')
        print('>>> There are {} {} witnesses.\n'.format(count_witnesses_counter, keyword))
    else:
        return count_witnesses_counter
    
# def match_witnesses(data, keywords, antikeywords = [], search = False, print_witnesses = False, save = True):
#     if search == True:
#         print_witnesses = True
#         save = False
#     try:
#         match_witnesses_counter = 0
#         for i, text in enumerate(data):
#             for j, witness in enumerate(text['witnesses']):
#                 if text['witness_affiliation'][j] == None:
#                     if (len(first_match(keywords, witness.lower())) > 0 and
#                         len(first_match(antikeywords, witness.lower())) == 0):
#                         match = first_match(keywords, witness.lower()).title()
#                         match = ' '.join([word.lower() if word in ['And', 'For', 'Of', 'On', 'The'] and i > 0
#                                           else word for i, word in enumerate(match.split())])
#                         match = ' '.join([word.upper() if word in ['Iii', 'Ipcc', 'Nasa', 'Mit', 'Us', 'Usa', 'Usda', 'Ge']
#                                           else word for word in match.split()])
#                         match_witnesses_counter += 1
#                         if save == True:
#                             text['witness_affiliation'][j] = match
#                             text['witness_sector'][j] = sector
#                             text['witness_industry'][j] = industry
#                         if print_witnesses == True:
#                             print(i, j, witness, '\n', match, '\n')
#         new_witnesses = count_witnesses(data, 'witness_industry', industry, silent = True)
#         if search == False:
#             print('>>> {} more {} witnesses have been matched, resulting in a total of {} witnesses.'.format(match_witnesses_counter, 
#                                                                                                              industry, new_witnesses))            
#     except Exception as e: 
#         print(e)
#         raise

def match_witnesses(data, keywords, antikeywords = [], search = False, print_witnesses = False, save = True, sector = None, industry = None):
    if sector == None or industry == None:
        sys.exit('>>>Please define the sector and industry.<<<')
    if search == True:
        print_witnesses = True
        save = False
    try:
        match_witnesses_counter = 0
        for i, text in enumerate(data):
            for j, witness in enumerate(text['witnesses']):
                if text['witness_affiliation'][j] == None:
                    if (len(first_match(keywords, witness.lower())) > 0 and
                        len(first_match(antikeywords, witness.lower())) == 0):
                        match = first_match(keywords, witness.lower()).title()
                        match = ' '.join([word.lower() if word in ['And', 'For', 'Of', 'On', 'The'] and i > 0
                                          else word for i, word in enumerate(match.split())])
                        match = ' '.join([word.upper() if word in ['Iii', 'Ipcc', 'Nasa', 'Mit', 'Us', 'Usa', 'Usda', 'Ge']
                                          else word for word in match.split()])
                        match_witnesses_counter += 1
                        if save == True:
                            text['witness_affiliation'][j] = match
                            text['witness_sector'][j] = sector
                            text['witness_industry'][j] = industry
                        if print_witnesses == True:
                            print(i, j, witness, '\n', match, '\n')
        new_witnesses = count_witnesses(data, 'witness_industry', industry, silent = True)
        if search == False:
            print('>>> {} more {} witnesses have been matched, resulting in a total of {} witnesses.'.format(match_witnesses_counter, 
                                                                                                             industry, new_witnesses))            
    except Exception as e: 
        print(e)
        raise