from bs4 import BeautifulSoup
from bs4 import UnicodeDammit
import fnmatch
from os import listdir
import csv

source_folder = '../www.whitehouse.gov-2017-01-25/html/'
output_file = 'trump-20170125.csv'

corpus = []
for file in listdir(source_folder):
    if file not in ['.DS_Store', 'white-house']:
        corpus.append(file)

def writeToCSV(dataToWrite, outputFileName):
    with open(outputFileName, 'w') as csvfile:
        w = csv.writer(csvfile, delimiter=',')
        for row in dataToWrite:
            w.writerow(row)
    print(outputFileName, 'successfully created.')

def articleRow(filename):
    soup = BeautifulSoup(open(source_folder + filename), 'lxml')
    title = soup.title.string
    content = soup.find(id='page').get_text().replace('\r', ' '). replace('\n', ' ').replace('\t', ' ').replace('  ', ' ').replace('  ', ' ').replace('  ', ' ').replace('  ', ' ')
    row = []
    row.append(title)
    row.append(content)
    return(row)

data_to_write = []

for file in corpus:
    data_to_write.append(articleRow(file))
    print(file, 'successfully added to corpus output.')

writeToCSV(data_to_write, output_file)
