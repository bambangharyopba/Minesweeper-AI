import Board as board
import Agent as agent
import sys

if __name__ == "__main__":
    B = board.Board(sys.argv[1])
    B.printBoard()
    A = agent.Agent()

    A.seeBoard(B.getSize(), B.getMineCount())
    A.printBoard()
    print()
    
    move = A.firstMove()
    play = True
    while play:
        cellvalue = B.getCellValue(move)
        play = A.addKnowledge(cellvalue)
        if not play:
            break
        A.printBoard()
        move = A.move()
        print()

    print("Agent Board")
    A.printBoard()
    print()

    print("Problem Board")
    B.printBoard()




