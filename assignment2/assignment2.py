
def expandElementsChildren(queueElement):
    '''
    Return the children of a state of the list string
    '''
    elementPluschildren = []
    for index, character in enumerate(queueElement):
        if character is '-':
            # Don't do anything because this character means empty space
            pass
        else:
            # Check if we are at the end of the list
            if (index + 1) is len(queueElement):
                # Do nothing
                pass
            
            # if there is an empty adjacent space
            # move there
            elif queueElement[index + 1] is '-':
                pass
                
            # if we can jump a letter do that
            elif index + 2 is not len(queueElement) and queueElement[index + 1] is '-':
                pass
                     
    return elementPluschildren
 

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
    while not queueOfPartialPaths:
        # If last queue element equals the goal
        if queueOfPartialPaths[0][0] == finalState:
            return queueOfPartialPaths[0]
        else:
            # Remove the first queue element
            firstElement = queueOfPartialPaths[0]
            firstElementWithChildren = expandElementsChildren(firstElement)
            
            # Adds its children to the front of the queue of the partial paths
            firstElementWithChildren + queueOfPartialPaths
            
            #Sort the queue of partial paths by distance traveled
            
    return []

if __name__ == '__main__':
    pass