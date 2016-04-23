# Project Title: Sudoku Solver

### Statement
In our project, there will be a GUI display of a sudoku.  On the bottom of the display there will be a button that
says "solve".  When pressed, the brute force algorithm will begin stepping through the sudoku and attempt to solve
it.  There will also be an additional mode called "slow solve" that will pause for a short period of time in between
the solve.  This will allow the user to watch and understand the algorithm that solves the sudoku.

### Updates

As of Fri Apr 22nd, we have the Algorithm and GUI fully completed and functional. Now the only thing that we need to do is join both parts of our project and make sure that they work together.

### Analysis
Most of the functions in the solver use recursive aspects to extract data about the sudoku.
The main solver function will be written iteratively.  The sudoku will be represented as an
object with many getters and setters.

### Data set or other source materials
We will not be getting any external data, the sudoku solver will be using puzzles that we give it.

### Deliverable and Demonstration
Once our program is complete, we will have a fully working sudoku solver that will be able to take any given sudoku puzzle and solve it. During our presentation we will use the slow solving part of our application to explain the algorithm that we used and to demo the product as well as a quick solve that will show how quickly our solution can solve a puzzle

### Evaluation of Results
We will know if our program works successfully if it is able to solve multiple different puzzles and can also display our algorithm fully working. 

## Architecture Diagram

![arch_diagram](https://cloud.githubusercontent.com/assets/11009351/14321375/f494b164-fbe6-11e5-8e62-a30142c516d0.png)

There are two main parts to the program: the brute force algorithm which will be written by Connor, and the GUI which will be written by Yusuf.  The brute force algorithm will send information about the sudoku to the GUI as it is being solved.  The GUI then extracts the information from the sudoku it received to create something that the user can see.

If we have time at the end, me way be able to add some additional functionality to our program.  For example, the GUI may be able to reset the puzzle or take a puzzle inputted by the user.  The GUI will then send information to the algorithm so it can prepare another solve.

## Schedule

### First Milestone (Fri Apr 15)
A basic GUI display will be submitted along with a fully functional sudoku solver.  The two will not be linked just yet.

### Second Milestone (Fri Apr 22)
The GUI and the sudoku solver will be linked.  The user will be able to use the "solve" button on the GUI to start
the agorithm.

### Final Presentation (last week of semester)
During the last week we'll work on the most curcial part of our presentation, the slow-solving part of our program.
We'll use this to show the class how our algorithm is workng through the puzzle while we're presenting our project.

## Group Responsibilities
### Connor McGrory @conno1234
I will write the solver for the sudoku for the first milestone.  For the second milestone I will adjust the code
to work with Yusuf's GUI.

### Yusuf Yildiz @yyildiz
For the first milestone I will be working on the GUI and the interactivity of the program which will allow the users to step through Connor's algorithm. For the second milestone, I will make the GUI work with Connors underlying algorithm implementation.
