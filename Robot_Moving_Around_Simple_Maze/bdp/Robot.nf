Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Robot))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Robot))==(Machine(Robot));
  Level(Machine(Robot))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Robot)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Robot))==(Maze)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Robot))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Robot))==(?);
  List_Includes(Machine(Robot))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Robot))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Robot))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Robot))==(?);
  Context_List_Variables(Machine(Robot))==(?);
  Abstract_List_Variables(Machine(Robot))==(?);
  Local_List_Variables(Machine(Robot))==(visited_Squares,current_Position,robot_Y_Position,robot_X_Position);
  List_Variables(Machine(Robot))==(visited_Squares,current_Position,robot_Y_Position,robot_X_Position);
  External_List_Variables(Machine(Robot))==(visited_Squares,current_Position,robot_Y_Position,robot_X_Position)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Robot))==(?);
  Abstract_List_VisibleVariables(Machine(Robot))==(?);
  External_List_VisibleVariables(Machine(Robot))==(?);
  Expanded_List_VisibleVariables(Machine(Robot))==(?);
  List_VisibleVariables(Machine(Robot))==(?);
  Internal_List_VisibleVariables(Machine(Robot))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Robot))==(btrue);
  Gluing_List_Invariant(Machine(Robot))==(btrue);
  Expanded_List_Invariant(Machine(Robot))==(btrue);
  Abstract_List_Invariant(Machine(Robot))==(btrue);
  Context_List_Invariant(Machine(Robot))==(btrue);
  List_Invariant(Machine(Robot))==(robot_X_Position: x_axis_range & robot_Y_Position: y_axis_range & current_Position: maze & visited_Squares: seq(maze))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Robot))==(btrue);
  Abstract_List_Assertions(Machine(Robot))==(btrue);
  Context_List_Assertions(Machine(Robot))==(btrue);
  List_Assertions(Machine(Robot))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Robot))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Robot))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Robot))==(robot_X_Position,robot_Y_Position,current_Position,visited_Squares:=1,1,1|->1,[1|->1]);
  Context_List_Initialisation(Machine(Robot))==(skip);
  List_Initialisation(Machine(Robot))==(robot_X_Position:=1 || robot_Y_Position:=1 || current_Position:=1|->1 || visited_Squares:=[1|->1])
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Robot))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Robot),Machine(Maze))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Robot))==(btrue);
  List_Constraints(Machine(Robot))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Robot))==(MoveNorth,MoveEast,MoveSouth,MoveWest,Teleport,getPosition,foundExit,visitedSquare,robotsRoute,reset);
  List_Operations(Machine(Robot))==(MoveNorth,MoveEast,MoveSouth,MoveWest,Teleport,getPosition,foundExit,visitedSquare,robotsRoute,reset)
END
&
THEORY ListInputX IS
  List_Input(Machine(Robot),MoveNorth)==(?);
  List_Input(Machine(Robot),MoveEast)==(?);
  List_Input(Machine(Robot),MoveSouth)==(?);
  List_Input(Machine(Robot),MoveWest)==(?);
  List_Input(Machine(Robot),Teleport)==(teleport_X_Position,teleport_Y_Position);
  List_Input(Machine(Robot),getPosition)==(?);
  List_Input(Machine(Robot),foundExit)==(?);
  List_Input(Machine(Robot),visitedSquare)==(visited_X_Position,visited_Y_Position);
  List_Input(Machine(Robot),robotsRoute)==(?);
  List_Input(Machine(Robot),reset)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Robot),MoveNorth)==(report);
  List_Output(Machine(Robot),MoveEast)==(report);
  List_Output(Machine(Robot),MoveSouth)==(report);
  List_Output(Machine(Robot),MoveWest)==(report);
  List_Output(Machine(Robot),Teleport)==(report);
  List_Output(Machine(Robot),getPosition)==(position);
  List_Output(Machine(Robot),foundExit)==(enquiry);
  List_Output(Machine(Robot),visitedSquare)==(visited);
  List_Output(Machine(Robot),robotsRoute)==(route);
  List_Output(Machine(Robot),reset)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Robot),MoveNorth)==(report <-- MoveNorth);
  List_Header(Machine(Robot),MoveEast)==(report <-- MoveEast);
  List_Header(Machine(Robot),MoveSouth)==(report <-- MoveSouth);
  List_Header(Machine(Robot),MoveWest)==(report <-- MoveWest);
  List_Header(Machine(Robot),Teleport)==(report <-- Teleport(teleport_X_Position,teleport_Y_Position));
  List_Header(Machine(Robot),getPosition)==(position <-- getPosition);
  List_Header(Machine(Robot),foundExit)==(enquiry <-- foundExit);
  List_Header(Machine(Robot),visitedSquare)==(visited <-- visitedSquare(visited_X_Position,visited_Y_Position));
  List_Header(Machine(Robot),robotsRoute)==(route <-- robotsRoute);
  List_Header(Machine(Robot),reset)==(reset)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Robot),MoveNorth)==(report: REPORT);
  List_Precondition(Machine(Robot),MoveEast)==(report: REPORT);
  List_Precondition(Machine(Robot),MoveSouth)==(report: REPORT);
  List_Precondition(Machine(Robot),MoveWest)==(report: REPORT);
  List_Precondition(Machine(Robot),Teleport)==(report: REPORT & teleport_X_Position: NATURAL1 & teleport_Y_Position: NATURAL1 & teleport_X_Position: x_axis_range & teleport_Y_Position: y_axis_range);
  List_Precondition(Machine(Robot),getPosition)==(btrue);
  List_Precondition(Machine(Robot),foundExit)==(enquiry: REPORT);
  List_Precondition(Machine(Robot),visitedSquare)==(visited: REPORT & visited_X_Position: NATURAL1 & visited_Y_Position: NATURAL1);
  List_Precondition(Machine(Robot),robotsRoute)==(btrue);
  List_Precondition(Machine(Robot),reset)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Robot),reset)==(btrue | robot_X_Position,robot_Y_Position,current_Position,visited_Squares:=1,1,1|->1,[1|->1]);
  Expanded_List_Substitution(Machine(Robot),robotsRoute)==(btrue | route:=visited_Squares);
  Expanded_List_Substitution(Machine(Robot),visitedSquare)==(visited: REPORT & visited_X_Position: NATURAL1 & visited_Y_Position: NATURAL1 | visited_X_Position|->visited_Y_Position: maze ==> (visited_X_Position|->visited_Y_Position: ran(front(visited_Squares)) ==> visited:=Yes [] not(visited_X_Position|->visited_Y_Position: ran(front(visited_Squares))) ==> visited:=No) [] not(visited_X_Position|->visited_Y_Position: maze) ==> visited:=Overstepping_The_Maze_Boundary);
  Expanded_List_Substitution(Machine(Robot),foundExit)==(enquiry: REPORT | current_Position: exit_square ==> enquiry:=Yes [] not(current_Position: exit_square) ==> enquiry:=No);
  Expanded_List_Substitution(Machine(Robot),getPosition)==(btrue | position:=current_Position);
  Expanded_List_Substitution(Machine(Robot),Teleport)==(report: REPORT & teleport_X_Position: NATURAL1 & teleport_Y_Position: NATURAL1 & teleport_X_Position: x_axis_range & teleport_Y_Position: y_axis_range | teleport_X_Position|->teleport_Y_Position: internal_walls ==> report:=Crashed_The_Maze_Wall [] not(teleport_X_Position|->teleport_Y_Position: internal_walls) ==> (teleport_X_Position = robot_X_Position & teleport_Y_Position = robot_Y_Position ==> report:=No_Teleportation_To_Same_Square [] not(teleport_X_Position = robot_X_Position & teleport_Y_Position = robot_Y_Position) ==> (teleport_X_Position|->teleport_Y_Position/:maze ==> report:=Overstepping_The_Maze_Boundary [] not(teleport_X_Position|->teleport_Y_Position/:maze) ==> (current_Position: exit_square ==> report:=Completed [] not(current_Position: exit_square) ==> visited_Squares,current_Position,robot_X_Position,robot_Y_Position,report:=visited_Squares<-(teleport_X_Position|->teleport_Y_Position),teleport_X_Position|->teleport_Y_Position,teleport_X_Position,teleport_Y_Position,Teleported))));
  Expanded_List_Substitution(Machine(Robot),MoveWest)==(report: REPORT | robot_X_Position-1<min(x_axis_range) ==> report:=Overstepping_The_Maze_Boundary [] not(robot_X_Position-1<min(x_axis_range)) ==> (robot_X_Position-1|->robot_Y_Position: internal_walls ==> report:=Crashed_The_Maze_Wall [] not(robot_X_Position-1|->robot_Y_Position: internal_walls) ==> (current_Position: exit_square ==> report:=Completed [] not(current_Position: exit_square) ==> visited_Squares,current_Position,robot_X_Position,report:=visited_Squares<-(robot_X_Position-1|->robot_Y_Position),robot_X_Position-1|->robot_Y_Position,robot_X_Position-1,Moved_West)));
  Expanded_List_Substitution(Machine(Robot),MoveSouth)==(report: REPORT | robot_Y_Position-1<min(y_axis_range) ==> report:=Overstepping_The_Maze_Boundary [] not(robot_Y_Position-1<min(y_axis_range)) ==> (robot_X_Position|->robot_Y_Position-1: internal_walls ==> report:=Crashed_The_Maze_Wall [] not(robot_X_Position|->robot_Y_Position-1: internal_walls) ==> (current_Position: exit_square ==> report:=Completed [] not(current_Position: exit_square) ==> visited_Squares,current_Position,robot_Y_Position,report:=visited_Squares<-(robot_X_Position|->robot_Y_Position-1),robot_X_Position|->robot_Y_Position-1,robot_Y_Position-1,Moved_South)));
  Expanded_List_Substitution(Machine(Robot),MoveEast)==(report: REPORT | robot_X_Position+1>max(x_axis_range) ==> report:=Overstepping_The_Maze_Boundary [] not(robot_X_Position+1>max(x_axis_range)) ==> (robot_X_Position+1|->robot_Y_Position: internal_walls ==> report:=Crashed_The_Maze_Wall [] not(robot_X_Position+1|->robot_Y_Position: internal_walls) ==> (current_Position: exit_square ==> report:=Completed [] not(current_Position: exit_square) ==> visited_Squares,current_Position,robot_X_Position,report:=visited_Squares<-(robot_X_Position+1|->robot_Y_Position),robot_X_Position+1|->robot_Y_Position,robot_X_Position+1,Moved_East)));
  Expanded_List_Substitution(Machine(Robot),MoveNorth)==(report: REPORT | robot_Y_Position+1>max(y_axis_range) ==> report:=Overstepping_The_Maze_Boundary [] not(robot_Y_Position+1>max(y_axis_range)) ==> (robot_X_Position|->robot_Y_Position+1: internal_walls ==> report:=Crashed_The_Maze_Wall [] not(robot_X_Position|->robot_Y_Position+1: internal_walls) ==> (current_Position: exit_square ==> report:=Completed [] not(current_Position: exit_square) ==> visited_Squares,current_Position,robot_Y_Position,report:=visited_Squares<-(robot_X_Position|->robot_Y_Position+1),robot_X_Position|->robot_Y_Position+1,robot_Y_Position+1,Moved_North)));
  List_Substitution(Machine(Robot),MoveNorth)==(IF robot_Y_Position+1>max(y_axis_range) THEN report:=Overstepping_The_Maze_Boundary ELSIF robot_X_Position|->robot_Y_Position+1: internal_walls THEN report:=Crashed_The_Maze_Wall ELSIF current_Position: exit_square THEN report:=Completed ELSE visited_Squares:=visited_Squares<-(robot_X_Position|->robot_Y_Position+1) || current_Position:=robot_X_Position|->robot_Y_Position+1 || robot_Y_Position:=robot_Y_Position+1 || report:=Moved_North END);
  List_Substitution(Machine(Robot),MoveEast)==(IF robot_X_Position+1>max(x_axis_range) THEN report:=Overstepping_The_Maze_Boundary ELSIF robot_X_Position+1|->robot_Y_Position: internal_walls THEN report:=Crashed_The_Maze_Wall ELSIF current_Position: exit_square THEN report:=Completed ELSE visited_Squares:=visited_Squares<-(robot_X_Position+1|->robot_Y_Position) || current_Position:=robot_X_Position+1|->robot_Y_Position || robot_X_Position:=robot_X_Position+1 || report:=Moved_East END);
  List_Substitution(Machine(Robot),MoveSouth)==(IF robot_Y_Position-1<min(y_axis_range) THEN report:=Overstepping_The_Maze_Boundary ELSIF robot_X_Position|->robot_Y_Position-1: internal_walls THEN report:=Crashed_The_Maze_Wall ELSIF current_Position: exit_square THEN report:=Completed ELSE visited_Squares:=visited_Squares<-(robot_X_Position|->robot_Y_Position-1) || current_Position:=robot_X_Position|->robot_Y_Position-1 || robot_Y_Position:=robot_Y_Position-1 || report:=Moved_South END);
  List_Substitution(Machine(Robot),MoveWest)==(IF robot_X_Position-1<min(x_axis_range) THEN report:=Overstepping_The_Maze_Boundary ELSIF robot_X_Position-1|->robot_Y_Position: internal_walls THEN report:=Crashed_The_Maze_Wall ELSIF current_Position: exit_square THEN report:=Completed ELSE visited_Squares:=visited_Squares<-(robot_X_Position-1|->robot_Y_Position) || current_Position:=robot_X_Position-1|->robot_Y_Position || robot_X_Position:=robot_X_Position-1 || report:=Moved_West END);
  List_Substitution(Machine(Robot),Teleport)==(IF teleport_X_Position|->teleport_Y_Position: internal_walls THEN report:=Crashed_The_Maze_Wall ELSIF teleport_X_Position = robot_X_Position & teleport_Y_Position = robot_Y_Position THEN report:=No_Teleportation_To_Same_Square ELSIF teleport_X_Position|->teleport_Y_Position/:maze THEN report:=Overstepping_The_Maze_Boundary ELSIF current_Position: exit_square THEN report:=Completed ELSE visited_Squares:=visited_Squares<-(teleport_X_Position|->teleport_Y_Position) || current_Position:=teleport_X_Position|->teleport_Y_Position || robot_X_Position:=teleport_X_Position || robot_Y_Position:=teleport_Y_Position || report:=Teleported END);
  List_Substitution(Machine(Robot),getPosition)==(position:=current_Position);
  List_Substitution(Machine(Robot),foundExit)==(IF current_Position: exit_square THEN enquiry:=Yes ELSE enquiry:=No END);
  List_Substitution(Machine(Robot),visitedSquare)==(IF visited_X_Position|->visited_Y_Position: maze THEN IF visited_X_Position|->visited_Y_Position: ran(front(visited_Squares)) THEN visited:=Yes ELSE visited:=No END ELSE visited:=Overstepping_The_Maze_Boundary END);
  List_Substitution(Machine(Robot),robotsRoute)==(route:=visited_Squares);
  List_Substitution(Machine(Robot),reset)==(robot_X_Position:=1 || robot_Y_Position:=1 || current_Position:=1|->1 || visited_Squares:=[1|->1])
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Robot))==(width,height,maze_x_range,maze_y_range,internal_wall);
  Inherited_List_Constants(Machine(Robot))==(?);
  List_Constants(Machine(Robot))==(width,height,maze_x_range,maze_y_range,internal_wall)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Robot),REPORT)==({Overstepping_The_Maze_Boundary,Crashed_The_Maze_Wall,Moved_North,Moved_East,Moved_South,Moved_West,No_Teleportation_To_Same_Square,Teleported,Yes,No,Completed});
  Context_List_Enumerated(Machine(Robot))==(?);
  Context_List_Defered(Machine(Robot))==(?);
  Context_List_Sets(Machine(Robot))==(?);
  List_Valuable_Sets(Machine(Robot))==(?);
  Inherited_List_Enumerated(Machine(Robot))==(?);
  Inherited_List_Defered(Machine(Robot))==(?);
  Inherited_List_Sets(Machine(Robot))==(?);
  List_Enumerated(Machine(Robot))==(REPORT);
  List_Defered(Machine(Robot))==(?);
  List_Sets(Machine(Robot))==(REPORT)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Robot))==(?);
  Expanded_List_HiddenConstants(Machine(Robot))==(?);
  List_HiddenConstants(Machine(Robot))==(?);
  External_List_HiddenConstants(Machine(Robot))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Robot))==(btrue);
  Context_List_Properties(Machine(Robot))==(x_axis_range <: NATURAL1 & x_axis_range = 1..7 & y_axis_range <: NATURAL1 & y_axis_range = 1..5 & maze: x_axis_range <-> y_axis_range & maze = x_axis_range*y_axis_range & internal_walls: x_axis_range <-> y_axis_range & internal_walls = {2|->1,6|->1,4|->2,6|->2,1|->3,2|->3,3|->3,4|->3,4|->4,6|->4,7|->4,2|->5} & entrance_square: x_axis_range <-> y_axis_range & entrance_square = {1|->1} & exit_square: x_axis_range <-> y_axis_range & exit_square = {1|->5});
  Inherited_List_Properties(Machine(Robot))==(btrue);
  List_Properties(Machine(Robot))==(width = 7 & height = 5 & maze_x_range <: NATURAL1 & maze_x_range = 1..width & maze_y_range <: NATURAL1 & maze_y_range = 1..height & internal_wall = {1|->3,2|->1,2|->3,2|->5,3|->3,4|->2,4|->3,4|->4,6|->1,6|->2,6|->4,7|->4} & REPORT: FIN(INTEGER) & not(REPORT = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Robot),Machine(Maze))==(?);
  Seen_Context_List_Enumerated(Machine(Robot))==(?);
  Seen_Context_List_Invariant(Machine(Robot))==(btrue);
  Seen_Context_List_Assertions(Machine(Robot))==(btrue);
  Seen_Context_List_Properties(Machine(Robot))==(btrue);
  Seen_List_Constraints(Machine(Robot))==(btrue);
  Seen_List_Operations(Machine(Robot),Machine(Maze))==(?);
  Seen_Expanded_List_Invariant(Machine(Robot),Machine(Maze))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Robot),MoveNorth)==(?);
  List_ANY_Var(Machine(Robot),MoveEast)==(?);
  List_ANY_Var(Machine(Robot),MoveSouth)==(?);
  List_ANY_Var(Machine(Robot),MoveWest)==(?);
  List_ANY_Var(Machine(Robot),Teleport)==(?);
  List_ANY_Var(Machine(Robot),getPosition)==(?);
  List_ANY_Var(Machine(Robot),foundExit)==(?);
  List_ANY_Var(Machine(Robot),visitedSquare)==(?);
  List_ANY_Var(Machine(Robot),robotsRoute)==(?);
  List_ANY_Var(Machine(Robot),reset)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Robot)) == (width,height,maze_x_range,maze_y_range,internal_wall,REPORT,Overstepping_The_Maze_Boundary,Crashed_The_Maze_Wall,Moved_North,Moved_East,Moved_South,Moved_West,No_Teleportation_To_Same_Square,Teleported,Yes,No,Completed | ? | visited_Squares,current_Position,robot_Y_Position,robot_X_Position | ? | MoveNorth,MoveEast,MoveSouth,MoveWest,Teleport,getPosition,foundExit,visitedSquare,robotsRoute,reset | ? | seen(Machine(Maze)) | ? | Robot);
  List_Of_HiddenCst_Ids(Machine(Robot)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Robot)) == (width,height,maze_x_range,maze_y_range,internal_wall);
  List_Of_VisibleVar_Ids(Machine(Robot)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Robot)) == (?: ?);
  List_Of_Ids(Machine(Maze)) == (x_axis_range,y_axis_range,maze,internal_walls,entrance_square,exit_square | ? | ? | ? | ? | ? | ? | ? | Maze);
  List_Of_HiddenCst_Ids(Machine(Maze)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Maze)) == (x_axis_range,y_axis_range,maze,internal_walls,entrance_square,exit_square);
  List_Of_VisibleVar_Ids(Machine(Maze)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Maze)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Robot)) == (Type(REPORT) == Cst(SetOf(etype(REPORT,0,10))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Robot)) == (Type(Overstepping_The_Maze_Boundary) == Cst(etype(REPORT,0,10));Type(Crashed_The_Maze_Wall) == Cst(etype(REPORT,0,10));Type(Moved_North) == Cst(etype(REPORT,0,10));Type(Moved_East) == Cst(etype(REPORT,0,10));Type(Moved_South) == Cst(etype(REPORT,0,10));Type(Moved_West) == Cst(etype(REPORT,0,10));Type(No_Teleportation_To_Same_Square) == Cst(etype(REPORT,0,10));Type(Teleported) == Cst(etype(REPORT,0,10));Type(Yes) == Cst(etype(REPORT,0,10));Type(No) == Cst(etype(REPORT,0,10));Type(Completed) == Cst(etype(REPORT,0,10));Type(width) == Cst(btype(INTEGER,?,?));Type(height) == Cst(btype(INTEGER,?,?));Type(maze_x_range) == Cst(SetOf(btype(INTEGER,"[maze_x_range","]maze_x_range")));Type(maze_y_range) == Cst(SetOf(btype(INTEGER,"[maze_y_range","]maze_y_range")));Type(internal_wall) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Robot)) == (Type(visited_Squares) == Mvl(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(current_Position) == Mvl(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(robot_Y_Position) == Mvl(btype(INTEGER,?,?));Type(robot_X_Position) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Robot)) == (Type(reset) == Cst(No_type,No_type);Type(robotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(visitedSquare) == Cst(etype(REPORT,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(foundExit) == Cst(etype(REPORT,?,?),No_type);Type(getPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(Teleport) == Cst(etype(REPORT,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MoveWest) == Cst(etype(REPORT,?,?),No_type);Type(MoveSouth) == Cst(etype(REPORT,?,?),No_type);Type(MoveEast) == Cst(etype(REPORT,?,?),No_type);Type(MoveNorth) == Cst(etype(REPORT,?,?),No_type));
  Observers(Machine(Robot)) == (Type(robotsRoute) == Cst(SetOf(btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?))),No_type);Type(visitedSquare) == Cst(etype(REPORT,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(foundExit) == Cst(etype(REPORT,?,?),No_type);Type(getPosition) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
