def makePairs(filename):
    cur_fname = None
    cur_tags = []
    work = None

    def nothing(l):
        then(parseurl)

    def parseurl(l):
        nonlocal cur_fname
        cur_fname = "/".join(l.split('/')[-2:]).strip()#.replace('.jpeg', '.jpg')
        then(addtag)

    def addtag(l):
        cur_tags.append(l.strip().replace(' ', '_'))
        then(addtag)

    def pop():
        nonlocal cur_fname, cur_tags
        yield (cur_fname, cur_tags)
        cur_fname = None
        cur_tags = []
        then(nothing)

    def then(f):
        nonlocal work
        work = f

    then(nothing)

    with open(filename, 'r') as f:
        for line in f:
            if len(line) > 1:
                work(line)
            else:
                yield from pop()

def printTags(pairs):
    tags = list(set((t for p in pairs for t in p[1])))
    tags.sort()

    for t in tags:
        print(t)


def writeCsv(pairs):
    with open('./tags.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile, quoting=csv.QUOTE_MINIMAL)

        for pair in pairs:
            print(pair)
            if len(pair[0]) < 10:
                raise Exception()
            writer.writerow([pair[0]] + pair[1])

if __name__ == "__main__":
    from pprint import pprint
    import csv

    #pairs = list(makePairs('./data/tag_data.txt'))
    pairs = list(makePairs('./data/new_booru_tag_data.txt'))
    lines = []

    for (filename, tags) in pairs:
        lines.append(filename)

        for t in tags:
            lines.append(t)

        lines.append("")

    for line in lines[:-1]:
        print(line)
