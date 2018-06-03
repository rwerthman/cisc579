import copy

def expandElementsChildren(queueElement):
    '''
    Return the children of a state (queueElement)
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
            elif ((index + 2) is not len(queueElement) and 
                  queueElement[index + 2] is '-'):
                # Jump the letter to the right of the current letter
                copyQueueElement[index + 2] = copyQueueElement[index]
                copyQueueElement[index] = '-'
                # Add the child to the list that will be returned
                elementsChildren.append("".join(copyQueueElement))
                     
    return elementsChildren

def removeDuplicates(queueOfPartialPaths):
    '''
    Removes duplicate lists that have elements the same order
    '''
    queueOfPartialPathsWithoutDuplicates = []
    for path in queueOfPartialPaths:
        if path not in queueOfPartialPathsWithoutDuplicates:
            queueOfPartialPathsWithoutDuplicates.append(path)
    return queueOfPartialPathsWithoutDuplicates

def heuristicEstimateofDistanceToGoal(currentState, finalState):
    '''
    Estimates the distance from the current state to the final state
    '''
    totalDistanceToGoal = 0
    # Iterate backwards from the end of the current state
    for i, currentStateCharacter in reversed(
        list(enumerate(currentState))):
        if currentStateCharacter == '-':
            # Do nothing since we won't move - characters
            pass
        else:   
            for j, finalStateCharacter in reversed(
                list(enumerate(finalState))):
                # if the characters are the same and the current 
                # state characters are still in valid positions
                # according to the constraints
                if currentStateCharacter == finalStateCharacter:
                    if i <= j:
                        # If the character in the currentState is 
                        # already in the right position
                        if currentState[j] == finalState[j]:
                            # Remove the character from the final state 
                            # so we can keep track of duplicates
                            tempListFinalState = list(finalState)
                            tempListFinalState[j] = '-'
                            finalState = ''.join(tempListFinalState)
                            break
                        else:
                            # Track the number of moves we have to
                            # make to move the character to the right
                            # position
                            totalDistanceToGoal += (j - i)
                            # Remove the character from the final state 
                            # so we can keep track of duplicates
                            tempListFinalState = list(finalState)
                            tempListFinalState[j] = '-'
                            finalState = ''.join(tempListFinalState)
                            break
                    else:
                        # We know the current state will never lead to 
                        # the final state because the characters can't
                        # move to the left
                        # For example, '-r-t-a' and '---art'
                        # Return a large number indicating this is
                        # a bad state
                        return (len(finalState) * len(finalState))

    return totalDistanceToGoal

def solveWithAStar(initialState, finalState):
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
    
    Function returns true if there is a path to the final state
    from the initial state, false if there isn't a path
    '''
    if len(initialState) < 3:
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
            print 'A*: Path solution', result[::-1]
            print 'A*: Size of queue of partial paths', len(queueOfPartialPaths)
            return True
        else:
            # Remove the first queue element
            firstList = queueOfPartialPaths.pop(0)
            firstElement = firstList[0]
#             if debugAStar: print 'First element', firstElement
#             if debugAStar: print 'Heuristic cost of first element', heuristicEstimateofDistanceToGoal(firstElement, finalState)
#             if debugAStar: print 'Length of first path', len(firstList)
            
            # Adds its children to the front of the queue of the partial paths
            firstElementsChildren = expandElementsChildren(firstElement)
#             if debugAStar: print 'First elements children', firstElementsChildren
                
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
    '''
    Sort the queue of partial paths by the length of the paths
    and the hueristic of the most recent state
    '''
    queueOfPartialPaths.sort(key = lambda x: len(x) + heuristicEstimateofDistanceToGoal(x[0], finalState))
    return queueOfPartialPaths

def timingWrapper(func, initialState, finalState):
    '''
    Create a wrapper function that we can send to timeit
    because timeit only excepts functions
    with no arguments but our functions we want to time
    have arguments
    '''
    def timingWrapped():
        return func(initialState, finalState)
    return timingWrapped

def solveWithBranchAndBound(initialState, finalState):
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
    
    Function returns true if there is a path to the final state
    from the initial state, false if there isn't a path
    '''
    if len(initialState) < 3:
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
            print 'Branch and Bound: Path solution', result[::-1]
            print 'Branch and Bound: Size of queue of partial paths', len(queueOfPartialPaths)
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
            
            #Sort the queue of partial paths by distance traveled
            queueOfPartialPaths.sort(key=len)
            
    return False

if __name__ == '__main__':
    pass
