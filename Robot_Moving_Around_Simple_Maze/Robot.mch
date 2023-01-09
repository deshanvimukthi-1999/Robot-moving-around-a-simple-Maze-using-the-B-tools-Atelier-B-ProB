/* Robot
 * Author: User
 * Creation date: 1/6/2023
 */
MACHINE Robot
    
SEES Maze
    
SETS
    REPORT = {
        Overstepping_The_Maze_Boundary,
        Crashed_The_Maze_Wall,
        Moved_North, 
        Moved_East, 
        Moved_South, 
        Moved_West,
        No_Teleportation_To_Same_Square,
        Teleported,
        Yes,
        No,
        Completed
    }
       
CONSTANTS
    width, 
    height,
    maze_x_range, 
    maze_y_range,
    internal_wall
    
PROPERTIES
    width = 7 & 
    height = 5 &
    maze_x_range <: NATURAL1 & maze_x_range = 1..width & 
    maze_y_range <: NATURAL1 & maze_y_range = 1..height &
    internal_wall = {(1 |-> 3), (2 |-> 1), (2 |-> 3), (2 |-> 5), (3 |-> 3), (4 |-> 2), (4 |-> 3), (4 |-> 4), (6 |-> 1), (6 |-> 2), (6 |-> 4), (7 |-> 4)} 
    
VARIABLES
    robot_X_Position,
    robot_Y_Position,
    current_Position,
    visited_Squares
 
    
INVARIANT
    // x-axis position of robot is within the x-axis range of the maze (1-7)
    robot_X_Position : x_axis_range &
    // y-axis position of robot is within the y-axis range of the maze (1-5)
    robot_Y_Position : y_axis_range &
    // Robot's current position (maplet (x |-> y)) is a maze element.
    current_Position : maze &
    // The squares that have been visited by the robot is in a sequence of maze maplet
    visited_Squares : seq(maze)
    
INITIALISATION
     // initializes all position-related variables by using the initial square (1, 1).
    robot_X_Position := 1 ||
    robot_Y_Position := 1 ||
    current_Position := ( 1 |-> 1 ) ||
    visited_Squares := [( 1 |-> 1 )]
    
DEFINITIONS
    // This functions are used to create the graph
    ANIMATION_FUNCTION == ({ row, col, img | col:x_axis_range & 
    row: y_axis_range & img=0 & (col, 6-row) /: internal_wall });
    ANIMATION_FUNCTION1 == ({ row, col, img | col: x_axis_range & 
    row: y_axis_range  & img=1 & (col, 6-row) : internal_wall });
    ANIMATION_FUNCTION2 == ({ row, col, img | col: x_axis_range & 
    row: y_axis_range & img=2 & col= robot_X_Position & row=(6- robot_Y_Position) });
    // Images for the animations
    ANIMATION_IMG0 == "images/whiteBox.gif";
    ANIMATION_IMG1 == "images/blackBox.gif";
    ANIMATION_IMG2 == "images/blackDot.png";
    

OPERATIONS
    
 report <-- MoveNorth = 
        PRE report : REPORT
        THEN
            // checks if the square is above current position inside the maze. 
            IF (robot_Y_Position + 1 > max(y_axis_range))
            THEN 
                report := Overstepping_The_Maze_Boundary
            // checks if the square above current position isn't a wall 
            ELSIF ((robot_X_Position |-> robot_Y_Position + 1) : internal_walls) 
            THEN 
                report := Crashed_The_Maze_Wall
            ELSIF(current_Position : exit_square )
            THEN
                report := Completed

            ELSE 
                visited_Squares := (visited_Squares <- (robot_X_Position |-> robot_Y_Position + 1)) ||
                current_Position := (robot_X_Position |-> robot_Y_Position + 1) ||
                robot_Y_Position := robot_Y_Position + 1 ||
                report := Moved_North
            END
        END;


    report <-- MoveEast = 
        PRE report : REPORT
        THEN
            // checks if the square is on the right side of current position inside the maze.
            IF (robot_X_Position + 1 > max(x_axis_range))
            THEN 
                report := Overstepping_The_Maze_Boundary
            // checks if the square to right side of current position isn't a wall 
            ELSIF ((robot_X_Position + 1 |-> robot_Y_Position) : internal_walls) 
            THEN 
                report := Crashed_The_Maze_Wall
            ELSIF(current_Position : exit_square )
            THEN
                report := Completed
            ELSE 
                visited_Squares := (visited_Squares <- (robot_X_Position + 1 |-> robot_Y_Position)) ||
                current_Position := (robot_X_Position + 1 |-> robot_Y_Position) ||
                robot_X_Position := robot_X_Position + 1 ||
                report := Moved_East
            END
        END;


    report <-- MoveSouth = 
        PRE report : REPORT
        THEN
            // checks if the square is below of current position inside the maze.
            IF (robot_Y_Position - 1 < min(y_axis_range))
            THEN 
                report := Overstepping_The_Maze_Boundary
            // checks if the square below of current position isn't a wall 
            ELSIF ((robot_X_Position |-> robot_Y_Position - 1) : internal_walls) 
            THEN 
                report := Crashed_The_Maze_Wall
            ELSIF(current_Position : exit_square )
            THEN
                report := Completed
            ELSE 
                visited_Squares := (visited_Squares <- (robot_X_Position |-> robot_Y_Position - 1)) ||
                current_Position := (robot_X_Position |-> robot_Y_Position - 1) ||
                robot_Y_Position := robot_Y_Position - 1 ||
                report := Moved_South
            END        
        END;


        report <-- MoveWest = 
            PRE report : REPORT
            THEN
                // checks if the square is on left side of current position inside the maze.
                IF (robot_X_Position - 1 < min(x_axis_range))
                THEN 
                    report := Overstepping_The_Maze_Boundary
                // checks if the square to left side of current position isn't a wall 
                ELSIF ((robot_X_Position - 1 |-> robot_Y_Position) : internal_walls) 
                THEN 
                    report := Crashed_The_Maze_Wall
                ELSIF(current_Position : exit_square )
                THEN
                report := Completed
                ELSE 
                    visited_Squares := (visited_Squares <- (robot_X_Position - 1 |-> robot_Y_Position)) ||
                    current_Position := (robot_X_Position - 1 |-> robot_Y_Position) ||
                    robot_X_Position := robot_X_Position - 1 ||
                    report := Moved_West
                END
            END;


        report <-- Teleport(teleport_X_Position, teleport_Y_Position) =
            PRE report : REPORT & teleport_X_Position : NATURAL1 & teleport_Y_Position : NATURAL1 & teleport_X_Position : x_axis_range & teleport_Y_Position : y_axis_range
               
            THEN

                // Ensures that the robot won't teleport to an internal wall.
                IF ((teleport_X_Position |-> teleport_Y_Position) : internal_walls)
                THEN 
                    report := Crashed_The_Maze_Wall
                // Ensures that the robot won't teleport to same current square
                ELSIF (teleport_X_Position = robot_X_Position & teleport_Y_Position = robot_Y_Position)
                THEN
                    report := No_Teleportation_To_Same_Square
                // Ensures that the robot won't teleport itself out of the maze
                ELSIF ((teleport_X_Position |-> teleport_Y_Position) /: maze)
                THEN
                    report := Overstepping_The_Maze_Boundary
                ELSIF(current_Position : exit_square )
                THEN
                    report := Completed
                ELSE
                    visited_Squares := (visited_Squares <- (teleport_X_Position |-> teleport_Y_Position)) ||
                    current_Position := (teleport_X_Position |-> teleport_Y_Position) ||
                    robot_X_Position := teleport_X_Position ||
                    robot_Y_Position := teleport_Y_Position ||
                    report := Teleported
                END
            END;


        position <-- getPosition =
            BEGIN 
            position := current_Position
            END;


        enquiry <-- foundExit = 
            PRE enquiry : REPORT
            THEN
                // checks if the robot is already in the exit square.
                IF (current_Position : exit_square)
                THEN 
                    enquiry := Yes
                ELSE 
                    enquiry := No
                END
            END;


        visited <-- visitedSquare(visited_X_Position, visited_Y_Position) =
            PRE visited : REPORT & visited_X_Position : NATURAL1 & visited_Y_Position : NATURAL1
            THEN
                // confirms that the square's x, y coordinates are within the maze.
                IF ((visited_X_Position |-> visited_Y_Position) : maze)
                THEN 
                    // checks whether a specific square has already been visited by the robot. 
                    IF ((visited_X_Position |-> visited_Y_Position) : ran(front(visited_Squares)))
                    THEN 
                        visited := Yes
                    ELSE
                        visited := No
                    END
                ELSE
                    visited := Overstepping_The_Maze_Boundary
                END
            END;


        route <-- robotsRoute =
            BEGIN
                route := visited_Squares
            END;
            
        reset = 
        BEGIN
            robot_X_Position := 1 ||
            robot_Y_Position := 1 ||
            current_Position := ( 1 |-> 1 ) ||
            visited_Squares := [( 1 |-> 1 )]
        END
END
