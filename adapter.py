from clips import Environment, Symbol
import re

environment = Environment()
environment.load('minesweeper.clp')
environment.run()

def getCoor():
    coor = []
    for fact in environment.facts():
        fact = re.split("f-\d+\s+",str(fact))
        if len(fact) == 1:
            fact = fact[0]
        else:
            fact = fact[1]
        if (re.search('\(cell\s+', fact)):
            fact = re.sub(r'[\(|\)]',r'',fact)
            fact = fact.replace('cell','')
            fact = re.split("\s*id\s*",fact)[1]
            fact = re.split("\s*row\s*",fact)
            temp = fact[1]
            temp = re.split("\s*column\s*",temp)
            coor.append((int(fact[0]),int(temp[0]),int(temp[1])))
    return coor

def getMineCoor():
    minecoor = []
    for fact in environment.facts():
        fact = re.split("f-\d+\s+",str(fact))
        if len(fact) == 1:
            fact = fact[0]
        else:
            fact = fact[1]
        if (re.search('\(mine\s+', fact)):
            fact = re.sub(r'[\(|\)]',r'',fact)
            fact = fact.replace('mine','')
            fact = re.split("\s*row\s*",fact)[1]
            fact = re.split("\s*column\s*",fact)
            minecoor.append((int(fact[0]),int(fact[1])))
    return minecoor

def getMoves():
    move = []
    for fact in environment.facts():
        fact = re.split("f-\d+\s+",str(fact))
        if len(fact) == 1:
            fact = fact[0]
        else:
            fact = fact[1]
        if (re.search('\(move\s+', fact)):
            fact = re.sub(r'[\(|\)]',r'',fact)
            fact = fact.replace('move','')
            fact = re.split("\s*id\s*",fact)[1]
            move.append(('move',int(fact)))
        if (re.search('\(mark-mine\s+', fact)):
            fact = re.sub(r'[\(|\)]',r'',fact)
            fact = fact.replace('mark-mine','')
            fact = re.split("\s*id\s*",fact)[1]
            move.append(('flag',int(fact)))
    return move
