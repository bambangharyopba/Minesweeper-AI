class Sentence():
    def __init__(self, domain={}, mine_count=0):
        self.domain = domain
        self.mine_count = mine_count

    def __sub__(self, other):
        domain = self.domain - other.getDomain()
        mine_count = self.mine_count - other.getMineCount()
        return Sentence(domain, mine_count)

    def issubset(self, other):
        return self.domain.issubset(other.getDomain())

    def isequal(self, other):
        return self.domain == other.getDomain() and self.mine_count == other.getMineCount()

    def addDomain(self, domain):
        self.domain.append(domain)

    def setMineCount(self, mc):
        self.mine_count = mc
    
    def getDomain(self):
        return self.domain

    def getMineCount(self):
        return self.mine_count

    def getCopy(self):
        return Sentence(self.domain, self.mine_count)

class WorkingMemory():
    def __init__(self):
        self.sentences = []
        self.flag = []
        self.safe = []
        self.move = []

    def addSentence(self, sentence):
        add = True
        for s in self.sentences:
            if s.isequal(sentence):
                add = False
                break
        if add: 
            self.sentences.append(sentence)
            if len(sentence.getDomain()) == 1:
                if sentence.getMineCount() == 0:
                    e = list(sentence.getDomain())[0]
                    self.addSafe(e)
                else:
                    e = list(sentence.getDomain())[0]
                    self.addFlag(e)

    def setSentences(self, sentences):
        self.sentences = [sentence for sentence in sentences]
        for sentence in sentences:
            if len(sentence.getDomain()) == 1:
                if sentence.getMineCount() == 0:
                    e = list(sentence.getDomain())[0]
                    self.addSafe(e)
                else:
                    e = list(sentence.getDomain())[0]
                    self.addFlag(e)
    
    def getSentences(self):
        return self.sentences

    def addFlag(self, coordinate):
        add = True
        for c in self.flag:
            if c == coordinate:
                add = False
                break
        if add:
            self.flag.append(coordinate)
    
    def getFlag(self):
        return self.flag

    def addSafe(self, coordinate):
        add = True
        for c in self.safe:
            if c == coordinate:
                add = False
                break
        if add:
            self.safe.append(coordinate)
    
    def getSafe(self):
        return self.safe
    
    def addMove(self, coordinate):
        add = True
        for c in self.move:
            if c == coordinate:
                add = False
                break
        if add:
            self.move.append(coordinate)
    
    def getMove(self):
        return self.move

class PatternMatcher():
    def __init__(self, sentences=[]):
        self.sentences = [sentence for sentence in sentences]

    def setSentences(self, sentences):
        self.sentences = [sentence for sentence in sentences]

    def _atomizeSentence(self, sentence):
        if len(sentence.getDomain()) != 1:
            if sentence.getMineCount() != 0 and len(sentence.getDomain()) == sentence.getMineCount():
                atoms = []
                for domain in sentence.getDomain():
                    atoms.append(Sentence({domain}, 1))
                return atoms
            elif sentence.getMineCount() == 0:
                atoms = []
                for domain in sentence.getDomain():
                    atoms.append(Sentence({domain}, 0))
                return atoms
        return [sentence]

    def run(self):
        sentences1 = [sentence.getCopy() for sentence in self.sentences]
        sentences2 = [sentence.getCopy() for sentence in self.sentences]
        iterate = True
        while iterate:

            for i in range(len(sentences1)):
                for j in range(len(sentences2)):
                    if sentences2[j].issubset(sentences1[i]) and not sentences2[j].isequal(sentences1[i]):
                        sentences1[i] = sentences1[i] - sentences2[j]
            
            to_append = []
            to_delete = []
            for i in range(len(sentences1)):
                atoms = self._atomizeSentence(sentences1[i])
                if [sentences1[i]] != atoms:
                    to_append += atoms
                    to_delete.append(i)

            for i in to_delete:
                sentences1.pop(i)

            for s in to_append:
                add = True
                for sentence in sentences1:
                    if s.isequal(sentence):
                        add = False
                        break
                if add:
                    sentences1.append(s)

            iterate = False

            if len(sentences1) != len(sentences2):
                iterate = True
            else:
                for i in range(len(sentences1)):
                    if not sentences1[i].isequal(sentences2[i]):
                        iterate = True
                        break

            if iterate:
                sentences1 = [sentence.getCopy() for sentence in sentences1]
                sentences2 = [sentence.getCopy() for sentence in sentences1]

        return sentences1

class Agent():
    def __init__(self):
        self.WM = WorkingMemory()
        self.PM = PatternMatcher()
        self.last_move = ()

    def seeBoard(self, size, mine_count):
        self.board_size = size
        self.board_mine_count = mine_count
        self.matrix = [['.' for _ in range(self.board_size)] for __ in range(self.board_size)]

    def _compileKnowledge(self, cellvalue):
        selected = self.last_move
        domain = []
        for i in range(-1,2):
            for j in range(-1,2):
                if selected[0] + i >= 0 and selected[0] + i < self.board_size:
                    if selected[1] + j >= 0 and selected[1] + j < self.board_size:
                        domain.append((selected[0] + i, selected[1] + j))
        sentence = Sentence(set(domain), cellvalue)
        return sentence
    
    
    def addKnowledge(self, cellvalue):
        sentence = self._compileKnowledge(cellvalue)
        self.WM.addSentence(sentence)
        self.PM.setSentences(self.WM.getSentences())
        sentences = self.PM.run()
        self.WM.setSentences(sentences)
        self._updateMatrix()
        self.matrix[self.last_move[0]][self.last_move[1]] = cellvalue
        if cellvalue == "m":
            print("Agent Lose")
            return False
        return self.checkStatus()

    def _updateMatrix(self):
        flags = self.WM.getFlag()
        for flag in flags:
            self.matrix[flag[0]][flag[1]] = "f"
        
    def firstMove(self):
        self.last_move = (0,0)
        return (0,0)
    
    def move(self):
        coordinate = self._safe_move()
        if coordinate:
            self.WM.addMove(coordinate)
            self.last_move = coordinate
            return coordinate
        
        coordinate = _probability_move()
        if coordinate:
            self.WM.addMove(coordinate)
            self.last_move = coordinate
            return coordinate
        
        return "Agent Error"

    def _safe_move(self):
        safeCoor = self.WM.getSafe()
        moveCoor = self.WM.getMove()

        for coordinate in safeCoor:
            if coordinate not in moveCoor:
                return coordinate
        return None

    def _probability_move(self):
        pass

    def printBoard(self):
        for row in self.matrix:
            for cell in row:
                print(cell, end=" ")
            print()

    def checkStatus(self):
        stop = True
        for row in self.matrix:
            for cell in row:
                if cell == '.':
                    stop = False
                    break
            if not stop:
                break
        return not stop