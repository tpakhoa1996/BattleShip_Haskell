# Battle Ship (Haskell implementation)

### Introduction
This project implement a strategy for playing battleships in haskell

### Structure
All the nescessary files for my algorithm is stored in folder src

App folder is for playing

Test folder is for testing

### Algorithm
My algorithm is divied into two cases:

- Case 1: There is no Hit shot on the board. The algorithm will choose to shoot the cell with the largest impact on the board

- Case 2: There is at least a Hit shot on the board. The algorithm will find a posibility of the boat on the board that contain the most Hit shot and then try to complete that boat.
