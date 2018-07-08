Package["WolframSummerSchoolRL`"]

PackageImport["GeneralUtilities`"]

(*----------------------------------------------------------------------------*)
(* copy of https://github.com/openai/baselines/blob/master/baselines/ppo2/policies.py *)
(* Note: ensure inputs are scaled to range (0, 1) *)

(* TODO: add recurrent options *)
ConvPolicy[] := Module[
	{net},
	net = NetChain[{
		ConvolutionLayer[32, 8, "Stride" -> 4], Ramp,
		ConvolutionLayer[64, 8, "Stride" -> 2], Ramp,
		ConvolutionLayer[32, 8, "Stride" -> 1], Ramp,
		LinearLayer[512], Ramp
	}];
	NetGraph[<|
		"policy" -> net, 
		"critic" -> LinearLayer[1], 
		"actor" -> LinearLayer[]
		|>, 
		{"policy" -> {"critic", "actor"}}
	]
]

(* TODO: add critic *)
MLPPolicy[] := Module[
	{actor, critic},
	actor = NetChain[{
		LinearLayer[64], Tanh,
		LinearLayer[64], Tanh,
		LinearLayer[]
	}]
]