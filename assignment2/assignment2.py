
def expandElementsChildren(queueElement):
    '''
    Return the children of a state of the list string
    '''
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
                # Swap index + 1 with index
                copyQueueElement[index + 1] = copyQueueElement[index]
                copyQueueElement[index] = '-'
                # Add the child to the list that will be returned
                elementsChildren.append(copyQueueElement)
                
            # if we can jump a letter do that
            elif index + 2 is not len(queueElement) and queueElement[index + 2] is '-':
                # Swap index + 2 with index
                copyQueueElement[index + 2] = copyQueueElement[index]
                copyQueueElement[index] = '-'
                # Add the child to the list that will be returned
                elementsChildren.append(copyQueueElement)
                     
    return elementsChildren
 

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
        return None
    return []

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
    # Queue of partial paths is a list of lists
    queueOfPartialPaths = [[initialState]]
    
    # Until the queue is empty or goal is attained
    while len(queueOfPartialPaths) != 0:
        # If first queue element equals the goal
        if queueOfPartialPaths[0][0] == finalState:
            # Return the path to the from the initial state
            # to the final state
            result = queueOfPartialPaths[0]
            # Reverse it before returning it
            return result[::-1]
        else:
            # Remove the first queue element
            firstElement = queueOfPartialPaths[0][0]
            
            # Adds its children to the front of the queue of the partial paths
            firstElementsChildren = expandElementsChildren(firstElement)
            firstList = queueOfPartialPaths.pop(0)
            for child in firstElementsChildren:
                copyOfFirstList = firstList[:]
                copyOfFirstList.insert(0, child)
                queueOfPartialPaths.insert(0, copyOfFirstList)
            
            #Sort the queue of partial paths by distance traveled
            queueOfPartialPaths.sort(key=len)
            
    return None

if __name__ == '__main__':
    pass