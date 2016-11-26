import csv
import json

def init():
    import sys
    import os
    import os.path
    if sys.version_info.major != 2:
        sys.exit('Python 2 is required to run this program')

    with open('apikey.cfg') as f:
        exec(f.read())

    srv = locals().get('SERVER')
    from facepp import API
    return API(API_KEY, API_SECRET, srv = srv)

def print_result(result):
    from pprint import pformat
    def encode(obj):
        if type(obj) is unicode:
            return obj.encode('utf-8')
        if type(obj) is dict:
            return {encode(k): encode(v) for (k, v) in obj.iteritems()}
        if type(obj) is list:
            return [encode(i) for i in obj]
        return obj
    result = encode(result)
    #print '\n'.join(['  ' + i for i in pformat(result, width = 75).split('\n')])
    
api = init()
from facepp import API, File

del init

def _run():
    '''
        The input file, profile-images.csv, includes the URLs to Twitter users' profile images 
        extracted from the raw data.
    '''
    i = 0
    with open('profile-images.csv','r') as infile, open('gender-facepp.csv','w') as outfile, open('gender-face-2.json','w') as outfile2:
        r = csv.DictReader((line.replace('\0','') for line in infile), delimiter=",")
        fieldnames = r.fieldnames + ['gender'] + ['gender_confidence'] + ['age'] + ['age_range'] + ['smiling'] + ['race'] + ['race_confidence'] + ['number_people_in_image']
        w = csv.DictWriter(outfile, fieldnames, extrasaction="ignore")
        w.writeheader()
        
        for row in r:
            #if row['user_id'] == '10453122':
                url = row["user_profile_image_url"]
                url = url.replace("_normal", "")
                #print url

                row['gender'] = 'NA'
                row['gender_confidence'] = 'NA'
                row['age'] = 'NA'
                row['age_range'] = 'NA'
                row['smiling'] = 'NA'
                row['race'] = 'NA'
                row['race_confidence'] = 'NA'
                row['number_people_in_image'] = 'NA'

                try:
                    face = api.detection.detect(url = url)
                    #print_result(face)
                    json.dumps(face, outfile2)

                    if face["face"][0]["attribute"]["gender"] != None:
                        row['gender'] = face["face"][0]["attribute"]["gender"]["value"]
                        #print row['user_id'] + ": " + row['gender']
                        row['gender_confidence'] = face["face"][0]["attribute"]["gender"]["confidence"]
                        #print row['gender_confidence']

                        row['age'] = face["face"][0]["attribute"]["age"]["value"]
                        #print str(row['age'])
                        row['age_range'] = face["face"][0]["attribute"]["age"]["range"]
                        #print row['age_range']

                        row['smiling'] = face["face"][0]["attribute"]["smiling"]["value"]
                        #print row['smiling']

                        row['race'] = face["face"][0]["attribute"]["race"]["value"]
                        #print row['race']
                        row['race_confidence'] = face["face"][0]["attribute"]["race"]["confidence"]
                        #print row['race_confidence']
                        row['number_people_in_image'] = len(face["face"])

                        i+=1
                        print i
                except Exception as e:
                    print(e)
                    pass
                w.writerow(row)
                #i = i+ 1
                #if i >2:
                #break

if __name__ == '__main__':
    _run()


