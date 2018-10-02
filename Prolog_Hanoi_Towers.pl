% Encapsulation of the solution method
solution(InitialStatus, ListOfGoals, Plan) :-
	resolve(InitialStatus, ListOfGoals, [], [], _, RevPlan),
	reverse(RevPlan, Plan).

% Base Case
resolve(Status, ListOfGoals, Plan, _, Status, Plan) :-
 subset(ListOfGoals, Status).

% Plan
resolve(Status, ListOfGoals, Plan, FailActions, NewStatus, NewPlan) :-
	member(Goal, ListOfGoals),

	% Find unsatisfactory goals
	not(member(Goal, Status)),

	% Find Actions and get them
	add(Actions, Goal),

        % No repite Actions
	not(member(Actions, FailActions)),

	% Find preconditions and achieve them
	ListOfPreconditions(Actions, ListOfPreconditions),
	resolve(Status, ListOfPreconditions, Plan, [Actions|FailActions], Temporary1, TmpPlan1),

	% Recursive with a new Status and goals
	ApplyRules(Actions, Temporary1, Temporary2),
	resolve(Temporary2,ListOfGoals,[Actions|TmpPlan1],FailActions,NewStatus,NewPlan).

% Apply rule
ApplyRules(Actions, Status, NewStatus) :-

	% Find deleted propositions
	DeleteFromList(Actions, DelList),

	% Remove deleted propositions
	subtract(Status, DelList, Statustemporal),

	% Find added propositions
	AddFromList(Actions, AddList),

	% Add propositions to the old Status
	union(AddList, Statustemporal, NewStatus).

% Function

% Conditions to move the blocks
ListOfPreconditions(Action, Plist) :- move(Action, Plist, _, _).
precondition(Action, Condition) :- ListOfPreconditions(Action, Plist), member(Condition, Plist).

% Add the move to the list
AddFromList(Action, Alist) :- move(Action, _, Alist, _).
add(Action, Condition) :- AddFromList(Action, Alist), member(Condition, Alist).

% Remove the move from the list
DeleteFromList(Action, Dlist) :- move(Action, _, _, Dlist).
delete(Action, Condition) :- DeleteFromList(Action, Dlist), member(Condition, Dlist).

% Print Initial status, goal and plan
plan(InitialStatus, Goal) :-
	solution(InitialStatus,Goal,Plan),
	write('\n\nStatus Inicial: '), write(InitialStatus),nl,
	write('Goal: '),write(Goal),nl,
	write('Plan:\n'),
        printPlan(Plan),nl,!.

% Print the plan
printPlan([]).
printPlan([A|B]):-
  write('       '),write(A),nl,
  printPlan(B).

move(stack(X,Y),
    [clear(Y), grab(X)],
    [top(X,Y), clear(X), emptyHand],
    [clear(Y), grab(X)]).

move(unStack(X,Y),
    [top(X,Y), clear(X), emptyHand],
    [clear(Y), grab(X)],
    [top(X,Y), clear(X), emptyHand]).

move(pick(X),
    [onTheTable(X), clear(X), emptyHand],
    [grab(X)],
    [onTheTable(X), clear(X), emptyHand]).

move(release(X),
    [grab(X)],
    [onTheTable(X), clear(X), emptyHand],
    [grab(X)]).

test1 :- plan([top(c,a), onTheTable(a), onTheTable(b), clear(b), clear(c), emptyHand],[top(b,c), top(a,b)]).
test2	:- plan([top(a,b), top(b,c), onTheTable(c), clear(a), emptyHand],[top(c,a), onTheTable(a), onTheTable(b)]).
