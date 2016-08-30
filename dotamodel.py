# -*- coding: utf-8 -*-
"""
Created on Tue Jun 28 21:12:06 2016

@author: Elvan
"""
import numpy as np
import pandas as pd

def readToDF(filename):
    
    with open(filename, 'r') as f:
        read_data = f.readlines()
    
    columns = ['Player_Logo', 'Player', 'Team', 'Wins', 'Losses', 'KDA', 
               'Kill', 'Death', 'Assists', 'LH', 'DN', 'GPM', 'XPM']
    temp = []
    for i, item in enumerate(read_data):
        
        col = i % 12
        
        if col <3:
            temp.append(item)
        elif col == 3:
            winloss = item.split("-")
            temp.append(float(winloss[0]))
            temp.append(float(winloss[1]))
        else:
            temp.append(float(item))
    
    index = np.arange(len(temp)/13)
    df = pd.DataFrame(index=index, columns=columns)
    df.fillna(0)
    
    for i, item in enumerate(temp):
        
        col = i%13
        row_id = i/13
        df.iloc[row_id, col] = temp[i]
    df.drop(df.columns[[0]], axis = 1, inplace = True)
    return(df)
    
workfile = 'C:/Users/Elvan/Desktop/PyData/frankfurt.txt'
frankfurt = readToDF(workfile)

workfile = 'C:/Users/Elvan/Desktop/PyData/shanghai.txt'
shanghai = readToDF(workfile)

workfile = 'C:/Users/Elvan/Desktop/PyData/manila.txt'
manila = readToDF(workfile)

frankfurt_team_results = [['OG Dota2\n', 1],
                          ['Team Secret\n',2],
                        ['Evil Geniuses\n',3],
                        ['EHOME\n', 4],
                        ['CDEC Gaming\n',5],
                        ['Vici Gaming\n',6],
                        ['Virtus.pro\n',7],
                        ['LGD-GAMING\n', 8],
                        ['Alliance\n', 9],
                        ['Mineski.Sports5\n', 10],
                        ['UNKNOWN #####\n',11],
                        ['Vega Squadron\n',12],
                        ['Cloud9 G2A\n',13],
                        ['Fnatic\n',14],
                        ['Newbee\n',15],
                        ['Newbee.Young\n',16]]
                        
shanghai_team_results = [['Team Secret\n', 1],
                          ['Team Liquid\n',2],
                        ['Evil Geniuses\n',3],
                        ['MVP Phoenix\n', 4],
                        ['compLexity Gaming\n',5],
                        ['Fnatic\n',6],
                        ['Alliance\n',7],
                        ['OG Dota2\n', 8],
                        ['EHOME\n',9],
                        ['LGD-GAMING\n', 10],
                        ['Virtus.pro\n', 11],
                        ['Newbee\n',12],
                        ['Team Archon\n',13],
                        ['Team. Spirit\n',14],
                        ['Vici Gaming\n',15],
                        ['CDEC Gaming\n',16]]

manila_team_results = [['OG Dota2\n', 1],
                          ['Team Liquid\n',2],
                        ['Newbee\n',3],
                        ['LGD-GAMING\n', 4],
                        ['Fnatic\n',5],
                        ['MVP Phoenix\n',6],
                        ['Natus Vincere\n',7],
                        ['Vici Gaming Reborn\n', 8],
                        ['compLexity Gaming\n', 9],
                        ['Alliance\n', 10],
                        ['Digital Chaos\n',11],
                        ['Team Empire\n',12],
                        ['the wings gaming\n',13],
                        ['Mineski\n',14],
                        ['Evil Geniuses\n',15],
                        ['Team Secret\n',16]]

frankfurt_team_results = pd.DataFrame(frankfurt_team_results, columns = ['Team', 'Frankfurt_Result'])
frankfurt = pd.merge(frankfurt, frankfurt_team_results, on = 'Team', how='outer', sort = False, left_index = False, right_index = False)
frankfurt.fillna(0, inplace = True)

shanghai_team_results = pd.DataFrame(shanghai_team_results, columns = ['Team', 'Shanghai_Result'])
shanghai = pd.merge(shanghai, shanghai_team_results, on = 'Team', how='outer', sort = False, left_index = False, right_index = False)
shanghai.fillna(0, inplace = True)

manila_team_results = pd.DataFrame(manila_team_results, columns = ['Team', 'Manila_Result'])
manila = pd.merge(manila, manila_team_results, on = 'Team', how='outer', sort = False, left_index = False, right_index = False)
manila.fillna(0, inplace = True)

frankfurt['Player'] = frankfurt['Player'].str.replace('\n', '')
frankfurt['Team'] = frankfurt['Team'].str.replace('\n', '')
frankfurt.to_csv('frankfurt_df.csv', index = False, na_rep = "", encoding = 'utf-8')

shanghai['Player'] = shanghai['Player'].str.replace('\n', '')
shanghai['Team'] = shanghai['Team'].str.replace('\n', '')
shanghai.to_csv('shanghai_df.csv', index = False, na_rep = "", encoding = 'utf-8')

manila['Player'] = manila['Player'].str.replace('\n', '')
manila['Team'] = manila['Team'].str.replace('\n', '')
manila.to_csv('manila_df.csv', index = False, na_rep = "", encoding = 'utf-8')