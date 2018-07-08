Package["WolframSummerSchoolRL`"]

PackageImport["GeneralUtilities`"]

(*----------------------------------------------------------------------------*)
PackageExport["PlayRandomAgent"]

PlayRandomAgent[env_, render_:False] := Scope[
	first = RLEnvironmentReset[env];
	done = False;
	observations = Internal`Bag[];
	rewards = Internal`Bag[];
	actions = Internal`Bag[];
	renderings = Internal`Bag[];
	While[!done,
		a = RLEnvironmentSampleAction[env];
		obs = RLEnvironmentStep[env, a, render];
		done = obs["Done"];
		Internal`StuffBag[rewards, obs["Reward"]];
		Internal`StuffBag[observations, obs["Observation"]];
		Internal`StuffBag[actions, a];
		If[KeyExistsQ[obs, "Rendering"],
			Internal`StuffBag[renderings, obs["Rendering"]]
		];
	];
	out = <|
		"Actions" -> Internal`BagPart[actions, All], 
		"Rewards" -> Internal`BagPart[rewards, All], 
		"Observations" -> Internal`BagPart[observations, All],
		"InitialObservation" -> first
	|>;
	If[render, out["Renderings"] = Internal`BagPart[renderings, All]];
	out
]

PackageExport["PlayListActionAgent"]
PlayListActionAgent[env_, actions_] := Scope[
	done = False;
	observations = Internal`Bag[];
	rewards = Internal`Bag[];
	Do[
		a = actions[[i]];
		obs = RLEnvironmentStep[env, a];
		done = obs["Done"];
		Internal`StuffBag[rewards, obs["Reward"]];
		Internal`StuffBag[observations, obs["Observation"]];
		,
		{i, Length@actions}
	];
	<|
		"Rewards" -> Internal`BagPart[rewards, All], 
		"Observations" -> Internal`BagPart[observations, All],
		"Done" -> done
	|>
]