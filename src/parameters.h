#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "json.hpp"

struct parameters
{
	int seed = 123456789;
	double N = 10000;	//There will be N receivers and N senders. Stored as double for calculations
	int G = 2000;
	double c1 = 0.95;	//Signal cost to T1
	double c2 = 1.05;	//Signal cost to T2
	double v1 = 1;		//Benefit to T1
	double v2 = 1;		//Benefit to T2
	double w1 = 1;		//Receiver Payoff: A1 to T1
	double w2 = 0;		//Receiver Payoff: A2 to T1
	double w3 = 0;		//Receiver Payoff: A1 to T2
	double w4 = 1;		//Receiver Payoff: A2 to T2
	double m = .25;	//Probability of being T1
	int interactionPartners = 1;
	double mutRateAlpha = 	0.01;	//Chance of a mutation occurring each generation
	double mutRateBeta = 	0.01;
	double mutRateStrategySender =   0.01;
	double mutRateStrategyReceiver = 0.01;
	double mutStepAlpha = 	0.4;	//SD of a normal distribution of mutation size with mean 0
	double mutStepBeta = 	0.4;
	std::string alphaBetaMutation = "always"; //"always" or "strict" or "random". Always = alpha and beta can mutate with any strategy. Strict = alpha and beta can only mutate with strategy 1. Random = a new alpha or beta are drawn when an individual mutates to strategy 1
	std::string initializationType = "random"; //"random" or "parameter". For latter option, see below
	bool cauchyDist = true; //If true, use caucy dist for mutations. If false, use normal dist
	int initStrategySender = 1;
	int initStrategyReceiver = 1;
	int initAlpha = 1;
	int initBeta = 0;
	int replicates = 1;
	int coutReport = 0;
	int reportFreq = 50; //Export data every this many generations
	std::string dataFileName = "same";
	std::string dataFileFolder = "C:/Users/owner/Documents/S4/Simulation";
};

void from_json(const nlohmann::json& j, parameters& p);


#endif // SIM_PARAMETERS_H
