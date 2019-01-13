import numpy as np
import math
import time

def calc_heuristics_kvazi_manthattan(aPoint, bPoint, lab):
    dist = 0
    prevPoint = aPoint.copy()
    while(abs(aPoint[0] - bPoint[0]) != 0):
        if(aPoint[0] > bPoint[0]):
            aPoint[0] -= 1
        else:
            aPoint[0] += 1

        if (lab[tuple(aPoint)] == -1 and lab[tuple(prevPoint)] != -1):
            dist += 2
        else:
            dist += 1
        prevPoint = aPoint.copy()


    while(abs(aPoint[1] - bPoint[1] != 0)):
        if (aPoint[1] > bPoint[1]):
            aPoint[1] -= 1
        else:
            aPoint[1] += 1

        if (lab[tuple(aPoint)] == -1 and lab[tuple(prevPoint)] != -1):
            dist += 2
        else:
            dist += 1
        prevPoint = aPoint.copy()

    return dist

def get_neighbors(current):
    neighbors = list()
    current = list(current)
    original = current.copy()

    #levo
    current[1] -= 1
    if current[1] >= 0:
        if narray[tuple(current)] >= 0 or narray[tuple(current)] == -3:
            neighbors.append(tuple(current))
    current = original.copy()

    #desno
    current[1] += 1
    if current[1] < np.size(narray, 1):
        if narray[tuple(current)] >= 0 or narray[tuple(current)] == -3:
            neighbors.append(tuple(current))
    current = original.copy()

    #gor
    current[0] -= 1
    if current[0] >= 0:
        if narray[tuple(current)] >= 0 or narray[tuple(current)] == -3:
            neighbors.append(tuple(current))
    current = original.copy()

    #dol
    current[0] += 1
    if current[0] < np.size(narray, 0):
        if narray[tuple(current)] >= 0 or narray[tuple(current)] == -3:
            neighbors.append(tuple(current))
    current = original.copy()

    return neighbors

def search(path, g, bound, end ):
    node = path[-1]
    f = g + calc_heuristics_kvazi_manthattan(list(node), end, narray)
    if f > bound:
        return f
    if node == end:
        return "FOUND"
    mini = math.inf
    neighbors = get_neighbors(node)
    for neighbor in neighbors:
        if neighbor not in path:
            path.append(neighbor)
            t = search(path, g + narray[tuple(neighbor)], bound, end)
            if t == "FOUND":
                return "FOUND"
            if t < mini:
                mini = t
            path.pop()
    return mini



def ida_star(end):
    bound = calc_heuristics_kvazi_manthattan(start.copy(), end, narray)
    path = list()
    path.append(tuple(start))
    while True:
        t = search(path, 0, bound, end)
        if t == "FOUND":
            return (path, bound)
        if t == math.inf:
            return "NOT FOUND"
        bound = t


narray = np.loadtxt("labyrinth_12.txt", int, delimiter=",")
start = np.argwhere(narray == -2)
start = list(map(list, start))[0]
ends = np.argwhere(narray == -3)
ends = list(map(list,ends))


for end in ends:
    timeStart = time.clock()
    A = ida_star(tuple(end))
    timeStart = time.clock() - timeStart
    print("Time spent on finding path: {}s  Price of path: {}".format(timeStart, A[1]))

