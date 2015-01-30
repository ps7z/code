% guess

{ edge(X,Y) } :- node(X), node(Y), X != Y.
{ conf(X,Y) } :- node(X), node(Y), X<Y.

%:- edge(X,Y), indep(X,Y,C,J), not ismember(C,X), not ismember(C,Y),  not ismember(J,Y), X<Y.
%:- edge(Y,X), indep(X,Y,C,J), not ismember(C,X), not ismember(C,Y),  not ismember(J,X), X<Y.
%:- conf(X,Y), indep(X,Y,C,J), not ismember(C,X), not ismember(C,Y), not ismember(J,X), not ismember(J,Y), X<Y.

%fail(X,Y,C,J) :- edge(X,Y), indep(X,Y,C,J), not ismember(C,X), not ismember(C,Y),  not ismember(J,Y),jset(J),cset(C), X<Y.
%fail(X,Y,C,J) :- edge(Y,X), indep(X,Y,C,J), not ismember(C,X), not ismember(C,Y),  not ismember(J,X),jset(J),cset(C), X<Y.
%fail(X,Y,C,J) :- conf(X,Y), indep(X,Y,C,J), not ismember(C,X), not ismember(C,Y), not ismember(J,X), not ismember(J,Y),jset(J),cset(C), X<Y.

% symmetries

% paths
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Base case

% X->Y => X-->Y ADDED THE FACT THAT X-- path is never d-con if X in C (this is commented as ADDED after)
pathth(X,Y,C,J) :- edge(X,Y),not ismember(J,Y),not ismember(C,X),jset(J),cset(C),X != Y.
%OK


% <-> => <-->
pathhh(X,Y,C,J) :- conf(X,Y),not ismember(J,X),not ismember(J,Y),jset(J),cset(C),X != Y. 
%OK

%no need for base case for --- it is never added
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -->

% X->Z + Z-->Y => --> ADDED
pathth(X,Y,C,J) :- edge(X,Z), pathth(Z,Y,C,J), not ismember(C,Z), not ismember(J,Z), not ismember(C,X),jset(J),cset(C), X!=Y, Z!=X, Z != Y.
%OK

% X->Z + Z<->Y => --> ADDED
pathth(X,Y,C,J) :- edge(X,Z), pathhh(Z,Y,C,J), ismember(C,Z), not ismember(J,Z), not ismember(C,X),jset(J),cset(C), X!= Y, X!= Z, Z < Y.
pathth(X,Y,C,J) :- edge(X,Z), pathhh(Y,Z,C,J), ismember(C,Z), not ismember(J,Z), not ismember(C,X),jset(J),cset(C), X!= Y, X!= Z, Z > Y.
% needs the other one fore symmetry
%OK

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% <->

% X<--Y + Y-->Z => X<->Y
pathhh(X,Z,C,J) :- edge(Y,X), pathth(Y,Z,C,J), not ismember(C,Y), not ismember(J,X), not ismember(J,Z),jset(J),cset(C), X < Z, X!=Y, Y != Z.
%OK

% <-> + --> => <->
pathhh(X,Z,C,J) :- conf(Y,X), pathth(Y,Z,C,J), not ismember(C,Y), not ismember(J,Y),not ismember(J,X),jset(J),cset(C), X < Z, Y<X, Y != Z.
pathhh(X,Z,C,J) :- conf(X,Y), pathth(Y,Z,C,J), not ismember(C,Y), not ismember(J,Y),not ismember(J,X),jset(J),cset(C), X < Z, X<Y, Y != Z.
%OK

% X->Y + Y<->Z => X<->Z
pathhh(X,Z,C,J) :- edge(Y,X), pathhh(Y,Z,C,J), not ismember(C,Y),not ismember(J,X), not ismember(J,Z),jset(J),cset(C), X < Z, Y < Z, Y !=X.
pathhh(X,Z,C,J) :- edge(Y,X), pathhh(Z,Y,C,J), not ismember(C,Y),not ismember(J,X), not ismember(J,Z),jset(J),cset(C), X < Z, Z < Y, Y !=X.
%KOK

% X<->Y + Y<->Z => X<->Z
pathhh(X,Z,C,J) :- conf(X,Y), pathhh(Y,Z,C,J), ismember(C,Y), not ismember(J,Y), not ismember(J,X), not ismember(J,Z),jset(J),cset(C), X < Z, X < Y, Y < Z.
pathhh(X,Z,C,J) :- conf(Y,X), pathhh(Y,Z,C,J), ismember(C,Y), not ismember(J,Y), not ismember(J,X), not ismember(J,Z),jset(J),cset(C), X < Z, Y < X, Y < Z.
pathhh(X,Z,C,J) :- conf(X,Y), pathhh(Z,Y,C,J), ismember(C,Y), not ismember(J,Y), not ismember(J,X), not ismember(J,Z),jset(J),cset(C), X < Z, X < Y, Z < Y.
pathhh(X,Z,C,J) :- conf(Y,X), pathhh(Z,Y,C,J), ismember(C,Y), not ismember(J,Y), not ismember(J,X), not ismember(J,Z),jset(J),cset(C), X < Z, Y < X, Z < Y.
%OK

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- only
% we do never need paths to itself because we are always adding to the beginning
% NO WE DO NEED THEM:2->4->5->3<-5<-4 path is formed only from pathtt(4,4)?

%X->Z + Z---Y  => X---Y
pathtt(X,Y,C,J) :- edge(X,Z), pathtt(Z,Y,C,J), not ismember(C,Z), not ismember(J,Z), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X <= Y, X!= Z, Z <= Y.
pathtt(X,Y,C,J) :- edge(X,Z), pathtt(Y,Z,C,J), not ismember(C,Z), not ismember(J,Z), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X <= Y, X!= Z, Y <= Z.
%OK

% X-->Z + Z<--Y  => X---Y
pathtt(X,Y,C,J) :- edge(X,Z), pathth(Y,Z,C,J), ismember(C,Z),not ismember(J,Z), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X<=Y,X!=Z,Y!=Z.
pathtt(X,Y,C,J) :- edge(Y,Z), pathth(X,Z,C,J), ismember(C,Z),not ismember(J,Z), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X<=Y,X!=Z,Y!=Z.
%OK


%inconsistent (in)dependencies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pathth(X,Y,C,J), indep(X,Y,C,J,W), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X<Y.
:- pathth(Y,X,C,J), indep(X,Y,C,J,W), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X<Y.
:- pathhh(X,Y,C,J), indep(X,Y,C,J,W), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X<Y.
:- pathtt(X,Y,C,J), indep(X,Y,C,J,W), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X<Y.

:- not pathth(X,Y,C,J), not pathth(Y,X,C,J), not pathhh(X,Y,C,J), not pathtt(X,Y,C,J), dep(X,Y,C,J,W), not ismember(C,X), not ismember(C,Y),jset(J),cset(C), X<Y.

