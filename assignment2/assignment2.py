import copy
from Tkconstants import CURRENT

def expandElementsChildren(queueElement):
    '''
    Return the children of a state of the list string
    '''
    queueElement = list(queueElement)
    elementsChildren = []
    for index, character in enumerate(queueElement):
        # Create a copy of the queue element
        copyQueueElement = queueElement[:]
        if character is '-':
            # Don't do anything because this character means empty space
            pass
        else:
            # Check if we are at the end of the list
            if (index + 1) is len(queueElement):
                # Do nothing
                pass
            
            # if there is an empty adjacent space move there
            elif queueElement[index + 1] is '-':
                # Swap the letter with the adjacent space
                copyQueueElement[index + 1] = copyQueueElement[index]
                copyQueueElement[index] = '-'
                # Add the child to the list that will be returned
                elementsChildren.append("".join(copyQueueElement))
                
            # if we can jump a letter to the right like checkers
            elif (index + 2) is not len(queueElement) and queueElement[index + 2] is '-':
                # Jump the letter to the right of the current letter
                copyQueueElement[index + 2] = copyQueueElement[index]
                copyQueueElement[index] = '-'
                # Add the child to the list that will be returned
                elementsChildren.append("".join(copyQueueElement))
                     
    return elementsChildren

def removeDuplicates(queueOfPartialPaths):
    queueOfPartialPathsWithoutDuplicates = []
    for path in queueOfPartialPaths:
        if path not in queueOfPartialPathsWithoutDuplicates:
            queueOfPartialPathsWithoutDuplicates.append(path)
    return queueOfPartialPathsWithoutDuplicates

def heuristicEstimateofDistanceToGoal(currentState, finalState):
    totalDistanceToGoal = 0
    # Start from the end of the current state
    for i, currentStateCharacter in reversed (list(enumerate(currentState))):
        if currentStateCharacter == '-':
            # Do nothing since we won't move - characters
            pass
        else:   
            for j, finalStateCharacter in enumerate(finalState):
                # if the characters are the same and the current state characters 
                # is not passed where it should be compared to the final state
                if currentStateCharacter == finalStateCharacter and i <= j:
                    # If it is possible to move the character to a position in the current state
                    # where it should be in the final state
                    if currentState[j] == '-':
                        # Track the number of moves we have made
                        totalDistanceToGoal += (j - i)
                        # Remove the character from the final state so
                        # so we can keep track of duplicates
                        tempListFinalState = list(finalState)
                        tempListFinalState[j] = '-'
                        finalState = ''.join(tempListFinalState)
                        
                        # Remove the character from the current state
                        # so we keep track of duplicates
                        tempListCurrentState = list(currentState)
                        tempListCurrentState[i] = '-'
                        currentState = ''.join(tempListCurrentState)
                        break
                    # If the character in the currentState is already in the same
                    # position that it is in the final state don't add any
                    # distance to it
                    elif currentState[j] == finalState[j]:
                        break
                    else:
                        # We know the current state will never lead to the final state
                        # For example, '-r-t-a' and '---art'
                        # return a number that is higher than the # of possible moves
                        # which indicates this is not a good path to follow
                        return (len(currentState)^2)

    return totalDistanceToGoal

'''
A*

Add root to queue of partial paths
Until queue is empty or goal is attained
    If first queue element equals the goal then  do nothing
    Else 
        remove the first queue element
        add its children to the front of the queue of the partial paths
        sort the queue of partial paths by distance traveled plus the estimate of distance to goal
        remove redundant paths
If goal is attained then announce success
'''
def solveWithAStar(initialState, finalState):
    if len(initialState) < 3:
        #return None
        return False
    # Add root to queue of partial paths
    queueOfPartialPaths = [[initialState]]
    
    # Until the queue is empty or goal is attained
    while len(queueOfPartialPaths) > 0:
        # If first queue element equals the goal
        if queueOfPartialPaths[0][0] == finalState:
            # Return the path from the initial state to the final state
            result = queueOfPartialPaths[0]
            # Reverse the path before returning it so that it goes 
            # initial state -> final state
            # return result[::-1]
            print 'A* path solution', result
            return True
        else:
            # Remove the first queue element
            firstList = queueOfPartialPaths.pop(0)
            firstElement = firstList[0]
            
            # Adds its children to the front of the queue of the partial paths
            firstElementsChildren = expandElementsChildren(firstElement)
            for child in firstElementsChildren:
                copyOfFirstList = copy.deepcopy(firstList)
                copyOfFirstList.insert(0, child)
                queueOfPartialPaths.insert(0, copyOfFirstList)
            
            # sort the queue of partial paths by distance traveled plus the estimate of distance to goal
            # queueOfPartialPaths.sort(key=len)
            queueOfPartialPaths = sortWithDistanceTravelledAndHeuristic(queueOfPartialPaths, finalState)
            
            # remove redundant paths
            queueOfPartialPaths = removeDuplicates(queueOfPartialPaths)             
            
    # return None
    return False

def sortWithDistanceTravelledAndHeuristic(queueOfPartialPaths, finalState):
    queueOfPartialPaths.sort(key = lambda x: len(x) + heuristicEstimateofDistanceToGoal(x[0], finalState))
    return queueOfPartialPaths

'''
Branch and Bound

Add root to queue of partial paths
Until queue is empty or goal is attained
    If first queue element equals the goal then do nothing
    Else
        remove the first queue element
        add its children to the front of the queue of the partial paths
        sort the queue of partial paths by distance traveled
If goal is attained then announce success
'''
def solveWithBranchAndBound(initialState, finalState):
    if len(initialState) < 3:
        return None
    
    # Add root to queue of partial paths
    queueOfPartialPaths = [[initialState]]
    
    # Until the queue is empty or goal is attained
    while len(queueOfPartialPaths) > 0:
        # If first queue element equals the goal
        if queueOfPartialPaths[0][0] == finalState:
            # Return the path from the initial state to the final state
            result = queueOfPartialPaths[0]
            # Reverse the path before returning it so that it goes 
            # initial state -> final state
            return result[::-1]
        else:
            # Remove the first queue element
            firstList = queueOfPartialPaths.pop(0)
            firstElement = firstList[0]
            
            # Adds its children to the front of the queue of the partial paths
            firstElementsChildren = expandElementsChildren(firstElement)
            for child in firstElementsChildren:
                copyOfFirstList = copy.deepcopy(firstList)
                copyOfFirstList.insert(0, child)
                queueOfPartialPaths.insert(0, copyOfFirstList)
            
            #Sort the queue of partial paths by distance traveled
            queueOfPartialPaths.sort(key=len)
            
    return None

if __name__ == '__main__':
    pass
