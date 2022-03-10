#Function to extract the names of each protein/gene presented in 
#the Diamond's blast tabular output that match the assembled contigs.

import sys
import re
import pandas as pd
import time

def progressbar(it, prefix="", size=60, file=sys.stdout):
    count = len(it)
    def show(j):
        x = int(size*j/count)
        file.write("%s[%s%s] %i/%i\r" % (prefix, "#"*x, "."*(size-x), j, count))
        file.flush()        
    show(0)
    for i, item in enumerate(it):
        yield item
        show(i+1)
    file.write("\n")
    file.flush()


if __name__ == "__main__":
    if len(sys.argv[1]) == 0:
        print("At least one argument must be supplied (input file).n")
    else:
        IN = open(sys.argv[1],"r")
        IN2 = IN.readlines()

        i = 0
        df = { "Contigs": [""],
                "Length_contig": [""],
                "Protein_code": [""],
                "Protein_ID": [""],
                "Length_protein": [""],
                "Expect": [""],
                "Identities": [""]
                }
        df = pd.DataFrame(df)
        for i in progressbar(range(len(IN2)), "Extracting data: ", 30):
            time.sleep(0.1)
            if re.match("(Query= \w+)", IN2[i]):
                if re.match("(>\w+)", IN2[i+4]):
                    length_contig = IN2[i+2]
                    res = IN2[i+7].split()
                    data_temp = { "Contigs": [IN2[i][7:]],
                    "Length_contig": [length_contig[7:]],
                    "Protein_code": [IN2[i+4]],
                    "Protein_ID": [IN2[i+4].split('>')[1].split('.1')[0]],
                    "Length_protein": [IN2[i+5][7:]],
                    "Expect": [res[7]],
                    "Identities": [IN2[i+8].split('Identities = ')[1].split(', Positives = ')[0]]
                }
                    
                    df_temp = pd.DataFrame(data_temp)
                    df = pd.concat([df, df_temp], axis=0)
                else:
                    length_contig = IN2[i+2]
                    data_temp = { "Contigs": [IN2[i][7:]],
                    "Length_contig": [length_contig[7:]],
                    "Protein_code": [IN2[i+5]],
                    "Protein_ID": ["NA"],
                    "Length_protein": ["NA"],
                    "Expect": ["NA"],
                    "Identities": ["NA"]
                }
                    df_temp = pd.DataFrame(data_temp)
                    df = pd.concat([df, df_temp], axis=0)
                    
        
        df.to_csv('matches_diamond.csv', index=False, sep='\t')
