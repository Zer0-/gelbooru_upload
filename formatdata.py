def makePairs(filename):
    cur_fname = None
    cur_tags = []
    work = None

    def nothing(l):
        then(parseurl)

    def parseurl(l):
        nonlocal cur_fname
        cur_fname = l.split('/')[-1].strip()
        then(addtag)

    def addtag(l):
        cur_tags.append(l.strip())
        then(addtag)

    def pop():
        nonlocal cur_fname, cur_tags
        print(cur_fname, cur_tags)
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

if __name__ == "__main__":
    from pprint import pprint
    import csv

    with open('./tags.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile, quoting=csv.QUOTE_MINIMAL)

        pairs = list(makePairs('./data/tag_data.txt'))

        for pair in pairs:
            print(pair)
            if len(pair[0]) < 10:
                raise Exception()
            writer.writerow([pair[0]] + pair[1])

        tags = list(set((t for p in pairs for t in p[1])))
        tags.sort()

        for t in tags:
            print(t)
