import json
import sys

GIT_URL = 'https://github.com/elegaanz/rustre/blob/main/'

if __name__ == '__main__':
    if len(sys.argv) > 1:
        if sys.argv[1] == "supports": 
            sys.exit(0)

    context, book = json.load(sys.stdin)
    for i, s in enumerate(book['sections']):
        if 'Chapter' in s.keys():
            book['sections'][i]['Chapter']['content'] = book['sections'][i]['Chapter']['content'].replace('@@@', GIT_URL)
    print(json.dumps(book))