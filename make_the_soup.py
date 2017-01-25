
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
    content = soup.find(id='page').get_text().replace('\r', ' '). replace('\n', ' ').replace('\t', ' ').replace('  ', ' ').replace('  ', ' ').replace('  ', ' ').replace('  ', ' ').replace("the WHITE HOUSEPresident Donald J. Trump Get in Touch  Home Briefing RoomFrom the News RoomLatest NewsRead the latest news from the White House From the Press OfficeSpeeches & Remarks Press Briefings Statements & Releases Presidential Actions Legislation Nominations & Appointments Disclosures IssuesTop IssuesAmerica First Energy Plan America First Foreign Policy Bringing Back Jobs And Growth Making Our Military Strong Again Standing Up For Our Law Enforcement Community Trade Deals Working For All Americans The AdministrationPeoplePresident Donald J. Trump Vice President Mike Pence First Lady Melania Trump Mrs. Karen Pence The Cabinet Special EventsThe 58th Presidential Inauguration ParticipateJoin UsTours & Events Jobs with the Administration Internships White House Fellows Share Your ThoughtsWe the People Petitions Contact the White House 1600 PennHistory & GroundsPresidents First Ladies The Vice President's Residence & Office Eisenhower Executive Office Building Camp David Air Force One Our GovernmentThe Executive Branch The Legislative Branch The Constitution Federal Agencies & Commissions Elections & Voting State & Local Government Search form Search You are hereHome", '').replace("Follow Us: Twitter Facebook Instagram Youtube Email Twitter Instagram Facebook Contact Us Home Briefing RoomFrom the News RoomLatest News From the Press OfficeSpeeches & Remarks Press Briefings Statements & Releases Presidential Actions Legislation Nominations & Appointments Disclosures IssuesTop IssuesAmerica First Energy Plan America First Foreign Policy Bringing Back Jobs And Growth Making Our Military Strong Again Standing Up For Our Law Enforcement Community Trade Deals Working For All Americans The AdministrationPeoplePresident Donald J. Trump Vice President Mike Pence First Lady Melania Trump Mrs. Karen Pence The Cabinet Special EventsThe 58th Presidential Inauguration ParticipateJoin UsTours & Events Jobs with the Administration Internships White House Fellows Share Your ThoughtsWe the People Petitions Contact the White House 1600 PennHistory & GroundsPresidents First Ladies The Vice President's Residence & Office Eisenhower Executive Office Building Camp David Air Force One Our GovernmentThe Executive Branch The Legislative Branch The Constitution Federal Agencies & Commissions Elections & Voting State & Local Government USA.gov Privacy Policy Copyright Policy", '').replace('ParticipateWhite House Fellows Join Us Tours & Events Jobs with the Administration Internships White House FellowsAbout the Fellowship Current Class Selection Process & Calendar Frequently Asked Questions Contact Apply', '').replace("1600 PennFirst Ladies History & Grounds Presidents First Ladies The Vice President's Residence & Office Eisenhower Executive Office Building Camp David Air Force One", '').replace("1600 PennPresidents History & Grounds Presidents First Ladies The Vice President's Residence & Office Eisenhower Executive Office Building Camp David Air Force One", '').replace("Briefing RoomPresidential ActionsExecutive Orders Briefing Room Speeches & Remarks Press Briefings Statements & Releases Presidential ActionsExecutive Orders Presidential Memoranda Proclamations Legislation Nominations & Appointments Disclosure", '').replace('Briefing RoomPresidential Actions', '').replace("Presidential Memoranda Briefing Room Speeches & Remarks Press Briefings Statements & Releases Presidential ActionsExecutive Orders Presidential Memoranda Proclamations Legislation Nominations & Appointments Disclosures", '').replace("the WHITE HOUSEPresident Donald J. Trump Get in Touch  Home Briefing RoomFrom the News RoomLatest NewsRead the latest news from the White House From the Press OfficeSpeeches & Remarks Press Briefings Statements & Releases Presidential Actions Legislation Nominations & Appointments Disclosures IssuesTop Issues", '').replace("The White House Office of the Press Secretary For Immediate Release", '')
    row = []
    row.append(title)
    row.append(content)
    return(row)

data_to_write = []
data_to_write.append(['title', 'text'])

for file in corpus:
    data_to_write.append(articleRow(file))
    print(file, 'successfully added to corpus output.')

writeToCSV(data_to_write, output_file)
