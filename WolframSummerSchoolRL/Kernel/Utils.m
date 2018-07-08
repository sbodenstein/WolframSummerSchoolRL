Package["WolframSummerSchoolRL`"]

PackageImport["GeneralUtilities`"]

(*----------------------------------------------------------------------------*)
PackageExport["PlayRandomAgent"]

PlayRandomAgent[env_] := Scope[
	done = False;
	observations = Internal`Bag[];
	rewards = Internal`Bag[];
	actions = Internal`Bag[];
	While[!done,
		a = RLEnvironmentSampleAction[env];
		obs = RLEnvironmentStep[env, a];
		done = obs["Done"];
		Internal`StuffBag[rewards, obs["Reward"]];
		Internal`StuffBag[observations, obs["Observation"]];
		Internal`StuffBag[actions, a];
	];
	<|
		"Actions" -> Internal`BagPart[actions, All], 
		"Rewards" -> Internal`BagPart[rewards, All], 
		"Observations" -> Internal`BagPart[observations, All]
	|>
]