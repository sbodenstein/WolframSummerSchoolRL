Package["WolframSummerSchoolRL`"]

PackageImport["GeneralUtilities`"]

(* This implementation exactly reproduces the OpenAI Gym CartPole-v1 environment,
up to numerical error.
Note: the rendering is different
*)

(*----------------------------------------------------------------------------*)
RLEnvironmentCreate["WLCartPole"] := CatchFailure @ Scope[
	state = <|"State" -> RandomReal[{-0.05, 0.05}, 4], "Done" -> False|>;
	id = $EnvironmentID;
	$EnvironmentID += 1;
	AssociateTo[$Environments, id -> state];
	System`Private`SetNoEntry @ RLEnvironment["WL", id, "WLCartPole"]
]

(*----------------------------------------------------------------------------*)
RLEnvironmentClose[RLEnvironment[_, id_, "WLCartPole"]] := 
CatchFailure @ Scope[
	getState[id]; (* checks whether state is there *)
	KeyDropFrom[$Environments, id];
]

(*----------------------------------------------------------------------------*)
RLEnvironmentReset[RLEnvironment[_, id_, "WLCartPole"]] := 
CatchFailure @ Scope[
	getState[id]; (* checks whether state is there *)
	$Environments[id] = 
		<|"State" -> RandomReal[{-0.05, 0.05}, 4], "Done" -> False|>;
	$Environments[id]["State"]
]

(*----------------------------------------------------------------------------*)
boolLookup = <|1 -> True, 0 -> False|>

RLEnvironmentStep[RLEnvironment[_, id_, "WLCartPole"], action_, render_:False] := 
CatchFailureAsMessage @ Scope[
	state = getState[id];

	If[state["Done"], ThrowFailure[RLEnvironmentStep::done]];
	If[(action =!= 1) && (action =!= 0), ThrowFailure[RLEnvironmentStep::invact, action]];

	update = cartpoleUpdate[state["State"], action];
	newState = Most[update];
	done = boolLookup @ Round @ Last[update];
	(* update state *)
	$Environments[id] = <|"State" -> newState, "Done" -> done|>;
	out = <|"Observation" -> newState, "Done" -> done, "Reward" -> 1|>;
	If[render, out["Rendering"] = cartRender[newState]];
	out
]

(*----------------------------------------------------------------------------*)
RLEnvironmentSampleAction[RLEnvironment[_, id_, "WLCartPole"]] :=  
	First @ RandomSample[{0, 1}, 1]

(*----------------------------------------------------------------------------*)
RLEnvironmentRender[RLEnvironment[_, id_, "WLCartPole"]] :=  
	CatchFailure @ cartRender[getState[id]["State"]]

(*----------------------------------------------------------------------------*)
RLEnvironmentStateSet[RLEnvironment[_, id_, "WLCartPole"], state_] := Scope[
	getState[id];
	$Environments[id] = state;
]

(*----------------------------------------------------------------------------*)
(* max position of cart *)
$xThreshold = 2.4;
(* Angle limit set to 2 * theta_threshold_radians so failing 
         observation is still within bounds *)
$ThetaThresholdRadians = 12. * 2. * Pi / 360.;

(*----------------------------------------------------------------------------*)
PackageScope["cartpoleUpdate"]

cartpoleUpdate := cartpoleUpdate = 
Compile[{{state, _Real, 1}, {action, _Integer}}, Module[
	{
		xThreshold = 2.4,
		thetaThresholdRadians = 12. * 2. * Pi / 360.,
		gravity = 9.8,
		length = 0.5, (* actually half the pole's length *)
		masscart = 1.0,
		masspole = 0.1,
		forceMag = 10.0,
		tau = 0.02, (* seconds between state updates*)
		totalMass, 
		polemassLength, force,
		x, xDot, theta, thetaDot, costheta, sintheta, temp,
		thetaacc, xacc, newState, done
	},

	(* derived quantities *)
	totalMass = (masspole + masscart);
	polemassLength = (masspole * length);

	force = If[action == 1, forceMag, -forceMag];
	{x, xDot, theta, thetaDot} = state;
	costheta = Cos[theta];
	sintheta = Sin[theta];
	temp = (force + polemassLength * thetaDot * thetaDot * sintheta) / totalMass;
	thetaacc = (gravity * sintheta - costheta * temp) / 
		(length * (4.0/3.0 - masspole * costheta * costheta / totalMass));
	xacc  = temp - polemassLength * thetaacc * costheta / totalMass;
	
	x  = x + tau * xDot;
	xDot = xDot + tau * xacc;
	theta = theta + tau * thetaDot;
	thetaDot = thetaDot + tau * thetaacc;
	
	done = Boole[(x < -xThreshold) || (x > xThreshold) || 
		(theta < -thetaThresholdRadians) || (theta > thetaThresholdRadians)
	];
	{x, xDot, theta, thetaDot, done}
]]

cartRender[{x_, xdot_, t_, tdot_}] := Module[
	{
		maxX = 2.4,
		polewidth = 0.1,
		polelen = 1.5,
		cartwidth = 0.5,
		cartheight = 0.3,
		xmargin = 1.3,
		ymargin = 1.2,
		line, cart, pole, hinge,
		hingepos, maxY, minY, frame
	},
	line = Line[{{-(maxX + cartwidth/2), 0}, {maxX + cartwidth/2, 0}}];
	cart = Rectangle[{x - cartwidth/2, -cartheight/2},{x + cartwidth/2, cartheight/2}];
	(* hinge *)
	hingepos = {x, (cartheight/2) * 0.5};
	hinge = Disk[hingepos, polewidth/2];
	(* pole *)
	pole = Rectangle[
		{x - polewidth/2, hingepos[[2]] - polewidth/2}, 
		{x + polewidth/2, hingepos[[2]] - polewidth/2 + polelen}
	];
	pole = Rotate[pole, -t, hingepos];
	
	Graphics[{
		line, cart, Blue, pole, 
		Red, hinge
		}, 
		PlotRange -> {{-maxX, maxX}*1.3, {-0.3, polelen*1.2}}
	]
]

(* testing code to ensure this implementation matches openai gym *)
PackageScope["testCartpole"]
testCartpole[] := Scope[
	$wenv = RLEnvironmentCreate["WLCartPole"];
	$env = RLEnvironmentCreate["CartPole-v1"];

	a = PlayRandomAgent[$env];
	WolframSummerSchoolRL`PackageScope`RLEnvironmentStateSet[
		$wenv,<|"State" -> a["InitialObservation"],"Done"->False|>
	];
	wolf = PlayListActionAgent[$wenv, a["Actions"]];
	
	test1 = (Total@a["Rewards"] == Total@wolf["Rewards"]);
	test2 = (Max[Abs[a["Observations"] - wolf["Observations"]]] < 0.001);
	test3 = (wolf["Done"] == True);
	RLEnvironmentClose@$wenv;
	RLEnvironmentClose@$env;
	VectorQ[{test1, test2, test3}, TrueQ]
]