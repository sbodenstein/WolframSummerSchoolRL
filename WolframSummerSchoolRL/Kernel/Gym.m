Package["WolframSummerSchoolRL`"]

PackageImport["GeneralUtilities`"]

(*----------------------------------------------------------------------------*)
Options[RLEnvironmentCreate] = {
	"Executable" -> Automatic
}

RLEnvironmentCreate[name_, opts:OptionsPattern[]] := CatchFailure @ Scope[
	UnpackOptions[executable];
	sessInfo = <|"System" -> "Python"|>;
	If[executable =!= Automatic, sessInfo["Executable"] = executable];

	session = StartExternalSession[sessInfo];
	If[FailureQ[session], Return @ $Failed];

	safeEE[session, "import gym"];
	safeEE[session, "import gym.spaces"];

	comm = "env = gym.make(\"" <> name <> "\")";
	safeEE[session, comm];

	id = $EnvironmentID;
	$EnvironmentID += 1;

	AssociateTo[$Environments, id -> session];
	
	System`Private`SetNoEntry @ RLEnvironment["Gym", id, name]
]

(*----------------------------------------------------------------------------*)
RLEnvironmentClose[RLEnvironment["Gym", id_, _]] := CatchFailure @ Scope[
	state = getState[id];
	(* delete session *)
	safeEE[getState[id], "env.close()"];
	DeleteObject[state];
	(* remove key *)
	KeyDropFrom[$Environments, id];
]

(*----------------------------------------------------------------------------*)
RLEnvironmentReset[RLEnvironment["Gym", id_, _]] := CatchFailure @ Scope[
	state = getState[id];
	safeEE[getState[id], "env.reset()"]
]

(*----------------------------------------------------------------------------*)
RLEnvironmentStep[RLEnvironment["Gym", id_, _], action_, render_:False] := CatchFailure @ Scope[
	state = getState[id];
	If[render, safeEE[state, "env.render()"]];

	stepString = "env.step(" <> ToString[action] <> ")";
	safeEE[state, stepString]
]

(*----------------------------------------------------------------------------*)
RLEnvironmentStep[RLEnvironment["Gym", id_, _], action_, render_:False] := CatchFailure @ Scope[
	state = getState[id];
	If[render, safeEE[state, "env.render()"]];

	stepString = "env.step(" <> ToString[action] <> ")";
	out = safeEE[state, stepString];
	<|"Observation" -> First[out], "Reward" -> out[[2]], "Done" -> out[[3]]|>
]

(*----------------------------------------------------------------------------*)
RLEnvironmentSampleAction[RLEnvironment["Gym", id_, _]] :=  
	CatchFailure @ safeEE[getState[id], "env.action_space.sample()"]

(*----------------------------------------------------------------------------*)
RLEnvironmentRender[RLEnvironment["Gym", id_, _]] :=  
	CatchFailure @ safeEE[getState[id], "env.render()"]

