#!/Users/psc/anaconda3/bin/python

import socket
from os import system
import numpy as np
from time import sleep
import argparse
parser = argparse.ArgumentParser(description='Server Configuration')

parser.add_argument('--arch', dest='arch', type=str, help='System architecture')
args = parser.parse_args()

solver_command = './sudoku_arm' if args.arch == 'arm' else './sudoku_x64'

def getAnswer():
    with open('answer.txt', 'r') as answerFile:
        answer = answerFile.read()
        return answer[:-1]

def convertStr(strInput):
    values = strInput.split(' ')
    print(values)
    board1D = np.array([list(map(int, value)) for value in values])
    board2D = np.reshape(board1D, (9, 9)).tolist()
    strBoard = "\n".join([" ".join(list(map(str, row))) for row in board2D])
    return strBoard

def generateQuestionFile(questionStr):
    with open('question.txt', 'w') as questionFile:
        questionFile.write(questionStr)
        
def callHaskellSolver():
    system(solver_command)
    

def boardToStr(boardInput):
    strBoard = "\n".join([" ".join(list(map(str, row))) for row in boardInput])
    return strBoard

serverPort = 7838
serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
serverSocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
serverSocket.bind(('', serverPort))
serverSocket.listen(1)
print('ready to receive')
while True:
    connectionSocket, addr = serverSocket.accept()
    tcpRequest = connectionSocket.recv(1024).decode()
    print(tcpRequest)
    
    generateQuestionFile(tcpRequest)
    callHaskellSolver()
    resultStr = convertStr(getAnswer())
    
    
    connectionSocket.send(resultStr.encode())
    connectionSocket.close()
    
