# -*- coding: utf-8 -*-
"""
Created on Wed Jan 04 12:19:44 2017

@author: Bhupinder Singh
"""
#%%
import pandas as pd
from pattern.en import sentiment
import os,re,nltk
import numpy as np
import codecs
#affin, vader_sentiment

class SentimentUtils:
    
    def __init__(self):
        pass
    
    def load_corpus(self, corpus_folder, filter_by = '.*'):
        #glob.glob('./[0-9].*')
        for root, dirs, files in os.walk(corpus_folder):
            for file_name in files:
                if len(re.findall(filter_by, file_name))>0 or filter_by ==None:
                    file_abspath = os.path.join(root, file_name)
                    doc = self.read_doc(file_abspath)
                    #os.path.dirname(f)
                    #os.path.basename(f)
                    yield file_abspath, doc
                    
    def read(self, csvFileName = 'data.csv'):
        df = pd.read_csv(csvFileName)
        messages = []
        messages.append('NReading input file..%s' % csvFileName)
        messages.append('No. of rows: %s ' % df.shape[0])
        messages.append('No. of cols: %s ' % df.shape[1])
        messages.append('Columns...')
        messages.append(df.columns)
        #self._show_messages(read_messages)
        return df
    
    def read_doc(self, file_name):
        f_handle = open(file_name, 'rb')#.decode('utf8', 'ignore')
        doc = ' '.join(f_handle.readlines())
        doc = codecs.decode(doc, 'ascii', 'ignore')
        f_handle.close()
        return doc

    def tokenize_sentences(self, doc, filter_by = []):
        paras = doc.split('\n') #para_tokenizer(doc)
        sentences = []
        found = False
        for para in paras:
            if filter_by !=None and len(filter_by)>0:
                found  = any([ w in para for w in filter_by])
                if found:
                    sentences.extend(nltk.sent_tokenize(para))
                else:
                    pass
            else:
                sentences.extend(nltk.sent_tokenize(para))
            #print sentences
        return sentences
        
    def _sentence_sentiment(self, sentence, cutoffs = [-0.3, 0.3]):
        sentiment_classes = ['negative', 'neutral', 'positive']
        #sentiment_classes = [-1,0,1]
        polarity, subjectivity = sentiment(sentence)
        if polarity <= cutoffs[0]:
            sentiment_class = sentiment_classes[0]
        elif polarity <= cutoffs[1]:
            sentiment_class = sentiment_classes[1]
        else:
            sentiment_class = sentiment_classes[2]
        return polarity, subjectivity, sentiment_class
    
    def get_sentiment(self, doc, filter_by = [], doc_id = None, cutoffs = [-0.3, 0.3], return_type = 'score'):
        return_types = ['score', 'report']
        sentences = self.tokenize_sentences(doc, filter_by = filter_by)
        polarities = map(lambda x: self._sentence_sentiment(x, cutoffs=cutoffs)[0], sentences)
        sentiment_classes = map(lambda x: self._sentence_sentiment(x, cutoffs=cutoffs)[2], sentences)        
        pos_count = sentiment_classes.count('positive')
        neg_count = sentiment_classes.count('negative')
        #sentiment_score = np.mean(polarities)
        sentiment_score = float(pos_count+1)/(neg_count+1)
        if return_type == 'score':
            result = sentiment_score
        elif return_type == 'report':
            df_doc = pd.DataFrame({'sentence': sentences, 'sentiment_polarity': polarities})
            df_doc['doc_id'] = doc_id
            result = sentiment_score, df_doc
        return result

    def analyze(self, corpus_folder, filter_by = []):
        f_loader = su.load_corpus(corpus_folder)
        df = pd.DataFrame()
        for path, doc in f_loader:
            score = su.get_sentiment(doc, filter_by = filter_by, doc_id = None, cutoffs = [-0.3, 0.3], return_type = 'score')
            df = df.append(pd.Series({'doc_id': path, 'sentiment_score': score}), ignore_index=True)
            #yield path, score
        return df
    
    def analyze_doc(self, file_name, filter_by = [], return_type = 'report'):
        doc = self.read_doc(file_name)
        #print doc
        result = su.get_sentiment(doc, filter_by = filter_by, doc_id = None, cutoffs = [-0.3, 0.3], return_type = 'report')
        return result
#%%     
#------
#User Inputs
        
inFile = 'data.csv'
outFile = 'result.csv'
corpus_folder = 'D:/Bhupinder/Projects/2017/POCs/11_Hackathon/corpus'
attr_filters = ['growth', 'revenue', 'profit']
#attr_filters = []
file_name = 'D:/Bhupinder/Projects/2017/POCs/11_Hackathon/corpus/file5.txt'
#------

base_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(base_dir)
su = SentimentUtils()

#analyze corpus
df = su.analyze(corpus_folder, filter_by = attr_filters)

#analyze single doc
sentiment_score, df_doc = su.analyze_doc(file_name, filter_by = attr_filters, return_type = 'report')
df_doc
#---
#df = ie.read_csv(inFile)
#df = pd.DataFrame({'text': ['this is good', 'that was really bad', 'horrible!']})
#df['sentiment'] = df['text'].apply(lambda x: pd.Series(su.get_sentiment(x, cutoffs = [-0.3, 0.3]))
#df = df.apply(lambda row: row.append(pd.Series(su.get_sentiment(row['text'], return_type = 'score', cutoffs = [-0.3, 0.3]))), axis=1) #, 'index' = ['polarity', 'subjectivity', 'sentiment_class']

