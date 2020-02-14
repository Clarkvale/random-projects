import sqlite3
from Bio import SeqIO
import re

connection = sqlite3.connect("DATABASE_NAME")
cursor = connection.cursor()
cursor.execute("CREATE TABLE IF NOT EXISTS CDS(Gene_ID TEXT, Locus_ID TEXT, Chromosome TEXT, Start_pos REAL, End_pos REAL, Strand REAL, GO_Id TEXT)")

def enter_gene(input_dict):
    
    cursor.execute("INSERT INTO CDS VALUES(?,?,?,?,?,?,?)",
                   (input_dict["Gene_ID"], input_dict["Locus_ID"], input_dict["Chromosome"],
                   input_dict["Start_pos"],input_dict["End_pos"], input_dict["Strand"],
                   input_dict["GO_Id"]))
    connection.commit()

genbank_file = SeqIO.parse(open("PATH_TO_GB_FILE", "r"), "genbank")
input_dict = {}
chro = ""
go_query = re.compile("GO:\d*")
for record in genbank_file:
    for feature in record.features:
        if feature.type == "source":
            try:
                chro = feature.qualifiers["chromosome"][0]
                input_dict["Chromosome"] = chro
            except KeyError:
                pass
            
       
                
        elif feature.type == "CDS":
            try:
                input_dict["Gene_ID"] = feature.qualifiers["gene"][0]
            except KeyError:
                input_dict["Gene_ID"] = ""
            input_dict["Locus_ID"] = feature.qualifiers["locus_tag"][0]
            input_dict["Start_pos"] = int(feature.location.start)
            input_dict["End_pos"] = int(feature.location.end)
            input_dict["Strand"] = feature.location.strand
            go_string = ""
            input_dict["Chromosome"] = chro
            counter = 1
            try:
                go_anno = feature.qualifiers["experiment"]
                for i in go_anno:
                    identifier = go_query.findall(i)
                    if counter == 1:
                        go_string = identifier[0]
                    else:
                        go_string += ", " + identifier[0]
                    
                        
                    counter += 1
                input_dict["GO_Id"] = go_string 
                    
                    
            except KeyError:
                input_dict["GO_Id"] = ""
                
                
            enter_gene(input_dict)
            input_dict.clear()
cursor.close()
connection.close()


