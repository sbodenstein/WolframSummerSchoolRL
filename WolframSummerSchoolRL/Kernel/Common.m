Package["WolframSummerSchoolRL`"]

PackageImport["GeneralUtilities`"]

(*----------------------------------------------------------------------------*)
PackageScope["$Environments"]
If[!AssociationQ[$Environments], $Environments = <||>];

PackageScope["$EnvironmentID"]
If[!IntegerQ[$EnvironmentID], $EnvironmentID = 1]

(*----------------------------------------------------------------------------*)
PackageExport["RLEnvironment"]

DefineCustomBoxes[RLEnvironment, 
	e:RLEnvironment[type_, id_, name_] :> Block[
	{},
	BoxForm`ArrangeSummaryBox[
		RLEnvironment, e, 
		None, 
		{BoxForm`SummaryItem[{"ID: ", id}],
		BoxForm`SummaryItem[{"Name: ", name}],
		BoxForm`SummaryItem[{"Type: ", type}]
		 },
		{},
		StandardForm
	]]
];

(*----------------------------------------------------------------------------*)
PackageScope["getState"]

General::wssrlenvdead = "The environment with ID `` no longer exists."
getState[id_] := 
	Lookup[$Environments, id, ThrowFailure[General::wssrlenvdead, id]];

(*----------------------------------------------------------------------------*)
PackageScope["safeEE"]

safeEE[session_, command_] := Scope[
	(* Block: from docs of ExternalEvaluate, 
		"Individual write operations to standard output are immediately printed 
		to the notebook or terminal." 
		Some operations keep printing warning messages
	*)
	out = Block[{Print}, ExternalEvaluate[session, command]];
	If[Failure @ out, ThrowFailure[]];
	out
]

(*----------------------------------------------------------------------------*)
(* These are generic to all environments *)

PackageExport["RLEnvironmentCreate"]
SetUsage["
RLEnvironmentCreate[name$] creates an instance of the environment with string name 
$name.
"]

PackageExport["RLEnvironmentClose"]
SetUsage["
RLEnvironmentClose[RLEnvironment[$$]] closes the environment.
"]

PackageExport["RLEnvironmentReset"]
SetUsage["
RLEnvironmentReset[RLEnvironment[$$]] resets the state of the environment and \
returns an initial observation.
"]

PackageExport["RLEnvironmentStep"]
SetUsage["
RLEnvironmentStep[RLEnvironment[$$], act$] steps through an environment using \
an action act$. RLEnvironmentReset[GymEnvironmentObject[id$]] must be called before the first \
call to RLEnvironmentStep. 
RLEnvironmentStep[GymEnvironmentObject[id$], act$, render$] displays the current state \
of the environment. 
"]

PackageExport["RLEnvironmentRandomAction"]
SetUsage["
RLEnvironmentRandomAction[RLEnvironment[$$]] returns a sampled action from the \
environments action space.
"]

