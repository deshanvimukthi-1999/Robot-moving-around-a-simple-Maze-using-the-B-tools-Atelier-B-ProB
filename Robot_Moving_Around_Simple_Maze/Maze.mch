/* Maze
 * Author: User
 * Creation date: 1/6/2023
 */
MACHINE Maze

CONSTANTS
    x_axis_range,
    y_axis_range,
    maze,
    internal_walls,
    entrance_square,
    exit_square
    
PROPERTIES
    // range of the x-axis is constrained to 1-7
    x_axis_range <: NATURAL1 & x_axis_range = 1..7 & 
    // range of the y-axis is constrained to 1-7
    y_axis_range <: NATURAL1 & y_axis_range = 1..5 &
    // element of relationship between x and y axis ranges(x |-> y) is the maze
    maze : x_axis_range <-> y_axis_range &
    // x and y axis ranges are products that are combined to create a maze.
    maze = x_axis_range * y_axis_range &
    // internal walls are the element of the relation between x and y axis ranges
    internal_walls : x_axis_range <-> y_axis_range &
    // Define the internal walls 
    internal_walls = {
        (2 |-> 1), (6 |-> 1), (4 |-> 2), (6 |-> 2), (1 |-> 3), (2 |-> 3), (3 |-> 3), (4 |-> 3), (4 |-> 4), (6 |-> 4), (7 |-> 4), (2 |-> 5)
    } &
    // Define the entrance & exit squares 
    entrance_square : x_axis_range <-> y_axis_range &
    entrance_square = {(1 |-> 1)} &
    exit_square : x_axis_range <-> y_axis_range &
    exit_square = {(1 |-> 5)}
    

END
