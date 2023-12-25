# -*- coding: utf-8 -*-
import transformers
from transformers import AutoTokenizer, AutoModelForCausalLM
import torch
import torch.nn.functional as F
import numpy as np
import math
import re
import pandas as pd


# Creating model and tokenizer instances
model_id = "stefan-it/secret-gpt2"
tokenizer = AutoTokenizer.from_pretrained(model_id, add_prefix_space=True)
model = AutoModelForCausalLM.from_pretrained(model_id)
bos_id = model.config.bos_token_id
word_embeddings = model.transformer.wte.weight # torch.Size([50265, 765])
model.eval()

def chunkstring(string, length):
    return (list(string[0+i:length+i] for i in range(0, len(string), length)))

def get_surprisal(seq,pad='dot'):

    max_input_size = int(0.75*1024) # GPT-2 can handle input size up until 1024, need to consider BPE splits

    seq_chunks = chunkstring(seq.split(),max_input_size) # returns chunks with words as items

    words, word_surprisal = [] , []

    pad_id = 18 if pad=='dot' else bos_id # 18 is the id of '.'

    for seq in seq_chunks:

        story_tokens, story_token_surprisal = [] , []
        
        inputs = tokenizer(seq, is_split_into_words=True)

        model_inputs = transformers.BatchEncoding({"input_ids":torch.tensor(inputs.input_ids).unsqueeze(0),
            "attention_mask":torch.tensor(inputs.attention_mask).unsqueeze(0)})
        
        with torch.no_grad():
            outputs = model(**model_inputs)
        
        output_ids = model_inputs.input_ids.squeeze(0)[1:]
        tokens = tokenizer.convert_ids_to_tokens(model_inputs.input_ids.squeeze(0))[1:]
        index = torch.arange(0, output_ids.shape[0])
        surp = -1 * torch.log2(F.softmax(outputs.logits, dim = -1).squeeze(0)[index, output_ids])

        story_tokens.extend(tokens)
        story_token_surprisal.extend(np.array(surp))

        # Word surprisal
        i = 0
        temp_token = ""
        temp_surprisal = 0
        
        while i <= len(story_tokens)-1:

            temp_token += story_tokens[i]
            temp_surprisal += story_token_surprisal[i]

            if i == len(story_tokens)-1 or tokens[i+1].startswith("Ġ"):
                # remove start-of-token indicator
                words.append(temp_token[1:])
                word_surprisal.append(temp_surprisal)
                # reset temp token/surprisal
                temp_surprisal = 0
                temp_token = ""
            i += 1

    # convert back surprisals into probs for later use
    word_prob = [1/(2**s) for s in word_surprisal]

    # convert_ids_to_tokens can't handle unicode, therefore has problems with German Umlaute and ß
    # To deal with this:
    replace_dict = {'ÃĦ':'Ä','Ã¤':'ä','Ãĸ':'Ö','Ã¶':'ö','Ãľ':'Ü','Ã¼':'ü',
					'ÃŁ':'ß','âĢľ':'“','âĢŀ':'„','Ãł':'à','ÃĢ':'À','Ã¡':'á',
					'Ãģ':'Á','Ã¨':'è','ÃĪ':'È','Ã©':'é','Ãī':'É','Ã»':'û',
					'ÃĽ':'Û','ÃŃ':'í','âĢĵ':'–','âĢĻ':'’'}
    for k in replace_dict.keys():
        words = [w.replace(k,replace_dict[k]) for w in words]

    #return ({'ws_list':list(zip(words,word_surprisal)),
     #       'words':words,
      #      'surprisals':word_surprisal,
       #     'probs':word_prob,
        #    'input_ids':output_ids,
         #   'story_tokens':story_tokens,
          #  'story_token_surprisal':story_token_surprisal})

    return word_surprisal[-1]


def BPE_split(word):
    encoded_w = tokenizer.encode(word) # type: list
    return 1 if len(encoded_w)>2 else 0 # needs to be >2 because secret gpt2 will append </s> id to every encoding 


def get_surprisals_adbc23():
    df = pd.read_csv('./adbc23/adbc23_stimuli_preproc.csv',sep=';')
    df['GPT2_s'] = df['Stimulus_tf'].apply(get_surprisal) 
    df['BPE_split'] = df['Target'].apply(BPE_split)
    df.to_csv('./adbc23/adbc23_gpt2_surprisals.csv',sep=';',encoding='utf-8',index=False)

def get_surprisals_adsbc21():
    df = pd.read_csv('./adsbc21/adsbc21_stimuli_preproc.csv',sep=';')
    df['GPT2_s'] = df['Stimulus_tf'].apply(get_surprisal) 
    df['BPE_split'] = df['Target'].apply(BPE_split)
    df.to_csv('./adsbc21/adsbc21_gpt2_surprisals.csv',sep=';',encoding='utf-8',index=False)

def get_surprisals_dbc21():
    df = pd.read_csv('./dbc21/dbc21_stimuli_preproc.csv',sep=';')
    df['GPT2_s'] = df['Sentence'].apply(get_surprisal)
    df['BPE_split'] = df['Target'].apply(BPE_split)
    df.to_csv('./dbc21/dbc21_gpt2_surprisals.csv',sep=';',encoding='utf-8',index=False)

def get_surprisals_dbc19():
    df = pd.read_csv('./dbc19/dbc19_stimuli_preproc.csv',sep=';',encoding='utf-8')
    df['GPT2_s'] = df['Sentence_short'].apply(get_surprisal)
    df['BPE_split'] = df['Target'].apply(BPE_split)
    df.to_csv('./dbc19/dbc19_gpt2_surprisals.csv',sep=';',encoding='utf-8',index=False)
    return df

def get_surprisal_all():
    print('DBC19...')
    get_surprisals_dbc19()

    print('DBC21...')
    get_surprisals_dbc21()
    
    print('ADSBC21...')
    get_surprisals_adsbc21()

    print('ADBC23...')
    get_surprisals_adbc23()

    print('Done')


def get_sim(word_list):

    cos = torch.nn.CosineSimilarity(dim=0)
    
    out_dict = dict()
    word_embeds = []
    for w in word_list:

        # First convert input word to id list
        ids = tokenizer.encode(w)

        # Get tensor of shape [number_of_ids,765]
        ids_embed = word_embeddings[ids,:]

        # Sum up along dim 0 and average by number of ids
        word_embed = ids_embed.sum(dim=0)/ids_embed.shape[0]

        word_embeds.append(word_embed)

    out_dict['word_embeds'] = word_embeds

    sim_scores = []
    for e in word_embeds:
        pass


    return out_dict


if __name__=='__main__':
    get_surprisal_all()
