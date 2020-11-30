import sys

class Board():
    def __init__(self, _file="", _size=0, blank=False):
        if _file != "":
            self.readProblem(_file) 
        else:
            self._matrix = []
            self._size = 0
            self._mine_count = 0
        

    def _setCellValue(self, cell):
        selected = self._matrix[cell[0]][cell[1]]
        for i in range(-1,2):
            for j in range(-1,2):
                if cell[0] + i >= 0 and cell[0] + i < self._size:
                    if cell[1] + j >= 0 and cell[1] + j < self._size:
                        if not(i == 0 and j == 0) and selected != "m":
                            if self._matrix[cell[0] + i][cell[1] + j] == "m":
                                selected += 1
        self._matrix[cell[0]][cell[1]] = selected

    def _initValue(self):
        for row in range(self._size):
            for col in range(self._size):
                self._setCellValue([row, col])

    def readProblem(self, _file):
        try:
            with open(_file) as f:
                line = f.readline()
                self._size = int(line)
                self._matrix = [[0 for i in range(self._size)] for j in range(self._size)]
                line = f.readline()
                self._mine_count = int(line)
                while True:
                    line = f.readline()
                    if not line:
                        break
                    line = [int(x.strip()) for x in line.strip().split(",")]
                    self._matrix[line[0]][line[1]] = 'm'
            
            self._initValue()
        except FileNotFoundError:
            print("File not found")
    
    def getCellValue(self, cell):
        return self._matrix[cell[0]][cell[1]]   
    
    def getMatrix(self):
        return self.matrix

    def getSize(self):
        return self._size
    
    def getMineCount(self):
        return self._mine_count

    def printBoard(self):
        for row in self._matrix:
            for cell in row:
                print(cell, end=" ")
            print()
