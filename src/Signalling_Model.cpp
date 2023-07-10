//============================================================================
// Name        : Signalling.cpp
// Author      : Jacob Chisausky
// Version     :
// Copyright   :
// Description : Signalling Model
//============================================================================

#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <ctime>
#include <fstream>		// for writing output to files

#include "rndutils.hpp"
#include "parameters.h"
//#include "Signalling Model.h"

using namespace std;

class Sender {
public:
	double s01;
	double s11;
	int Type;	//T1 or T2
	double fitness = 0.0;

	Sender(double a, double b){
		s01 = a;
		s11 = b;
		//s01: q = 0, send 1
		//s11: q = 1, send 1

		Type = 2;	//Default
	}
};

class Receiver {
public:
	double r01;
	double r11;
	double fitness = 0.0;

	Receiver(double a, double b){
		r01 = a;
		r11 = b;
		//r01; no signal, respond 1
		//r11; signal received, respond 1
	}
};


int main(int argc, char* argv[]) {
//int main() {

	int selectionMethod = 2; //1 = normalized roulette. 2 = k-tournament
	//int k = 2;	//for k tournament selection - for now, code only written to support k=2
/*
	//Take parameters - old way
	int	seed	=	123456789;
	double	N	=	1000;
	int	G	=	1000;
	double	c1	=	0.1;
	double	c2	=	0.5;
	double	v1	=	1.0;
	double	v2	=	1.0;
	double	w1	=	1.0;
	double	w2	=	0.0;
	double	w3	=	0.0;
	double	w4	=	1.0;
	double	m	=	1/3;
	int	interactionPartners	=	10;
	double	mutRate	=	0.01;
	double	mutStep	=	0.01;
	bool	cauchyDist	=	false;
	double	initS01	=	0;
	double	initS11	=	1;
	double	initR01	=	0;
	double	initR11	=	1;
	int	replicates	=	1	;
	int	reportFreq	=	10;
	std::string	dataFileName	=	"dataFile";
	std::string	dataFileFolder	=	"D:/StAndrews/Discrete_local/";
	bool	computeMeansInCpp	=	true;
*/

	//Take parameters from parameter file using json - new way

	std::cout << argv[1] << std::endl;

	nlohmann::json json_in;
	std::ifstream is(argv[1]);   //assumes that the file name is given as a parameter in the command line
	is >> json_in;
	parameters sim_pars = json_in.get<parameters>();

	int seed = sim_pars.seed;
	double N = sim_pars.N;	//There will be N receivers and N senders. Stored as double for calculations
	int G = sim_pars.G;
	double c1 = sim_pars.c1;	//Signal cost to T1
	double c2 = sim_pars.c2;	//Signal cost to T2
	double v1 = sim_pars.v1;		//Benefit to T1
	double v2 = sim_pars.v2;		//Benefit to T2
	double w1 = sim_pars.w1;		//Receiver Payoff: A1 to T1
	double w2 = sim_pars.w2;		//Receiver Payoff: A2 to T1
	double w3 = sim_pars.w3;		//Receiver Payoff: A1 to T2
	double w4 = sim_pars.w4;		//Receiver Payoff: A2 to T2
	double m = sim_pars.m;	//Probability of being T1
	int interactionPartners = sim_pars.interactionPartners;
	double mutRate = sim_pars.mutRate;
	double mutStep = sim_pars.mutStep;
	bool cauchyDist = sim_pars.cauchyDist; //If true, use caucy dist for mutations. If false, use normal dist
	double initS01 = sim_pars.initS01;	//Chance of sending a signal if q = 0
	double initS11 = sim_pars.initS11;  //Chance of sending a signal if q = 1
	double initR01 = sim_pars.initR01;	//Chance of r=1 if receive s = 0
	double initR11 = sim_pars.initR11;  //Chance of r=1 if receive s = 1
	int replicates = sim_pars.replicates;
	int reportFreq = sim_pars.reportFreq; //Export data every this many generations
	std::string dataFileName = sim_pars.dataFileName;
	std::string dataFileFolder = sim_pars.dataFileFolder;
	bool computeMeansInCpp = sim_pars.computeMeansInCpp;  //Add option to compute generational means in CPP instead of exporting all data, becaues with large N the .csv files become difficult to work with





	//double mutRateAlpha = 	sim_pars.mutRateAlpha;	//Chance of a mutation occurring each generation
	//double mutRateBeta = 	sim_pars.mutRateBeta;
	//double mutRateStrategySender =   sim_pars.mutRateStrategySender;
	//double mutRateStrategyReceiver = sim_pars.mutRateStrategyReceiver;
	//double mutStepAlpha = 	sim_pars.mutStepAlpha;	//SD of a normal distribution of mutation size with mean 0
	//double mutStepBeta = 	sim_pars.mutStepBeta;

	//	std::string alphaBetaMutation = sim_pars.alphaBetaMutation; //"always" or "strict" or "random". Always = alpha and beta can mutate with any strategy. Strict = alpha and beta can only mutate with strategy 1. Random = a new alpha or beta are drawn when an individual mutates to strategy 1
	//	if (alphaBetaMutation != "always" && alphaBetaMutation != "strict" && alphaBetaMutation != "random") {
	//		std::cout << "Invalid entry for alphaBetaMutation";
	//		return 91;
	//	}


	//	std::string initializationType = sim_pars.initializationType; //"random" or "parameter". For latter option, see below
	//	if (initializationType != "random" && initializationType != "parameter"){
	//		std::cout << "Invalid entry for initializationType";
	//		return 92;
	//	}


	//int initStrategySender = sim_pars.initStrategySender;
	//int initStrategyReceiver = sim_pars.initStrategyReceiver;
	//int initAlpha = sim_pars.initAlpha;
	//int initBeta = sim_pars.initBeta;

	//_________End parameter input

	int numT1 = round(m*N);	//**This rounding could produce minor deviations from analytic results
	double mCorrected = double(numT1)/double(N);

	const std::time_t now = std::time(nullptr) ; // get the current time point
	const std::tm calendar_time = *std::localtime( std::addressof(now) ) ;

	std::ofstream dataLog;
	std::ofstream params;
	std::ofstream summaryStats;

	tm *ltm = localtime(&now);
	int yday = ltm->tm_yday;

	string hr = to_string(calendar_time.tm_hour);
	if (hr.length() == 1){
		hr = "0" + hr;
	}

	string min = to_string(calendar_time.tm_min);
	if (min.length() == 1){
		min = "0" + min;
	}

	string sec = to_string(calendar_time.tm_sec);
	if (sec.length() == 1){
		sec = "0" + sec;
	}

	string strTime = to_string(yday) + "_" +  hr + "_" + min + "_" + sec;
	string str1 = dataFileFolder + "/" + strTime + "_data_" +  dataFileName + ".csv";
	string str2 = dataFileFolder + "/" + strTime + "_params_" + dataFileName + ".csv";
	string str3 = dataFileFolder + "/" + strTime + "_summaryStats_" + dataFileName + ".csv";
	dataLog.open(str1);
	params.open(str2);

	if (computeMeansInCpp == true){
		summaryStats.open(str3);
	}
	//Prepare output files
	dataLog << "rep,gen,ind,indType,strat01,strat11,fitness"; //We only need to report 2 cells of each individual
	params << "seed,N,G,c1,c2,v1,v2,w1,w2,w3,w4,m,mCorrected,interactionPartners,mutRate,mutStep,initS01,initS11,initR01,initR11,replicates,cauchyDist";
	params << "\n" << std::to_string(seed) << "," << std::to_string(N) << "," << std::to_string(G) << "," << std::to_string(c1) << "," << std::to_string(c2) << "," << std::to_string(v1) << "," << std::to_string(v2) << "," << std::to_string(w1) << "," << std::to_string(w2) << "," << std::to_string(w3) << "," << std::to_string(w4) << "," << std::to_string(m) << "," << std::to_string(mCorrected) << "," << std::to_string(interactionPartners) << "," << std::to_string(mutRate) << "," << std::to_string(mutStep) << "," << std::to_string(initS01) << "," << std::to_string(initS11) << "," << std::to_string(initR01) << "," << std::to_string(initR11) << "," << std::to_string(replicates) << "," << std::to_string(cauchyDist),"\n";
	//params << "\n" << to_string(seed) << "," << to_string(N) <<","<< to_string(G) <<","<< to_string(c1) <<","<< to_string(c2) <<","<< to_string(v1)<<","<<to_string(v2)<<","<<to_string(w1)<<","<<to_string(w2)<<","<<to_string(w3)<<","<<to_string(w4)<<","<<to_string(m)<<","<<to_string(interactionPartners)<<","<<to_string(mutRateAlpha)<<","<<to_string(mutRateBeta)<<","<<to_string(mutRateStrategySender)<<","<<to_string(mutRateStrategyReceiver)<<","<<to_string(mutStepAlpha)<<","<<to_string(mutStepBeta)<<","<<initializationType<<","<<to_string(initStrategySender)<<","<<to_string(initStrategyReceiver)<<","<<to_string(initAlpha)<<","<<to_string(initBeta)<<","<<to_string(replicates)<<","<<alphaBetaMutation<<","<<to_string(cauchyDist),"\n";
	if (computeMeansInCpp == true){
		//summaryStats << "rep,gen,indType,stratNum,stratType,meanAlphaBeta,meanFit,expAlphaBeta,seed,N,G,c1,c2,v1,v2,w1,w2,w3,w4,m,interactionPartners,mutRateAlpha,mutRateBeta,mutRateStrategySender,mutRateStrategyReceiver,mutStepAlpha,mutStepBeta,initializationType,initStrategySender,initStrategyReceiver,initAlpha,initBeta,replicates,alphaBetaMutation,cauchyDist";
		summaryStats << "rep,gen,indType,strat01,strat11,meanFit,expAlphaBeta,seed,N,G,c1,c2,v1,v2,w1,w2,w3,w4,m,mCorrected,interactionPartners,mutRate,mutStep,init01,init11,replicates,cauchyDist";
	}

	auto mutDist = std::normal_distribution<double>(0.0, std::abs(mutStep));
	//auto MutationDistBeta = std::normal_distribution<double>(0.0, std::abs(mutStepBeta));

	if (cauchyDist == 1) {
		auto mutDist = std::cauchy_distribution<double>(0.0, std::abs(mutStep));
		//auto MutationDistBeta = std::cauchy_distribution<double>(0.0, std::abs(mutStepBeta));
	}

//Random number generators
	auto rng = std::default_random_engine {seed};
	std::uniform_real_distribution<double> prob(0,1);
	//std::uniform_int_distribution<int> randInt(1,3);
	std::uniform_int_distribution<int> randN(0,N-1);

	//Fitness distribution
	//auto SenderFitnessDist = rndutils::mutable_discrete_distribution<int, rndutils::all_zero_policy_uni>{};
	//auto ReceiverFitnessDist = rndutils::mutable_discrete_distribution<int, rndutils::all_zero_policy_uni>{};

	//Vector used for drawing random numbers without replacement
	std::vector<int> nullVec;
	for (int i = 0; i < N; i++){
		nullVec.push_back(i);
	}

	//Expected alpha and beta
	double expAlpha = std::max(std::min( (m/(1-m)), 1.0),0.0);
	double expBeta = std::max( std::min( c2 , 1.0), 0.0);

	//Determine max and min possible fitnesses for normalization
	//	double maxReceiverFit = double(interactionPartners)*double(std::max(w1,std::max(w2,std::max(w3,w4))));
	//	double minReceiverFit = double(interactionPartners)*double(std::min(w1,std::min(w2,std::min(w3,w4))));
	//	double maxSenderFit = double(interactionPartners)* (max(v1,v2) - std::min(0.0,std::min(c1,c2)));	//To account for negative costs which will increase max fitness
	//	double minSenderFit = double(interactionPartners)*(-1.0*std::max(c1,c2) + std::min(0.0,std::min(v1,v2)));	//To account for negative benefits

	double maxReceiverFit = 100.0;
	double minReceiverFit = -100.0;
	double maxSenderFit = 100.0;
	double minSenderFit = -100.0;

	//double maxReceiverFitOld = double(interactionPartners)*double(std::max(w1,std::max(w2,std::max(w3,w4))));
	//double minReceiverFitOld = double(interactionPartners)*double(std::min(w1,std::min(w2,std::min(w3,w4))));
	//double maxSenderFitOld = double(interactionPartners)* (max(v1,v2) - std::min(0.0,std::min(c1,c2)));	//To account for negative costs which will increase max fitness
	//double minSenderFitOld = double(interactionPartners)*(-1.0*std::max(c1,c2) + std::min(0.0,std::min(v1,v2)));	//To account for negative benefits

	/*
	if (selectionMethod == 1){
		if (maxReceiverFitOld > maxReceiverFit){
			std::cout << "Possible receiver fitness too high (>100)";
			return -1000;
		}
		if (minReceiverFitOld < minReceiverFit){
			std::cout << "Possible receiver fitness too low (<-100)";
			return -1000;
		}
		if (maxSenderFitOld > maxSenderFit){
			std::cout << "Possible sender fitness too high (>100)";
			return -1000;
		}
		if (minSenderFitOld < minSenderFit){
			std::cout << "Possible sender fitness too low (<-100)";
			return -1000;
		}
	}

	cout << maxReceiverFitOld << endl << minReceiverFitOld << endl << maxSenderFitOld << endl << minSenderFitOld << endl;
	 */

	/*if (minReceiverFit == maxReceiverFit){	//To avoid divide by 0
		maxReceiverFit += 0.00001;
	}
	if (minSenderFit == maxSenderFit){
		maxSenderFit += 0.00001;
	}*/

	//Start replicate loop
	for (int rep = 1; rep <= replicates; rep++){

		//Population vectors
		std::vector<Sender> SenderVector;
		std::vector<Receiver> ReceiverVector;

		//Offspring vectors which will store offspring generation before it replaces parental generation
		std::vector<Sender> OffspringSenderVector;
		std::vector<Receiver> OffspringReceiverVector;

		//Vectors to hold fitnesses to generate discrete distribution for reproduction
		std::vector<double> SenderFitnesses(N);
		std::vector<double> ReceiverFitnesses(N);


		//Initialize Population
		//	if (initializationType == "parameter"){
		for (int i = 0; i < N; i++){
			SenderVector.push_back(Sender(initS01,initS11));
			ReceiverVector.push_back(Receiver(initR01,initR11));

			OffspringSenderVector.push_back(Sender(1,0));
			OffspringReceiverVector.push_back(Receiver(1,1));
		}

		//	} else if (initializationType == "random"){
		//		for (int i = 0; i < N; i++){
		//			SenderVector.push_back(Sender(randInt(rng),prob(rng)));
		//			ReceiverVector.push_back(Receiver(randInt(rng),prob(rng)));

		//			OffspringSenderVector.push_back(Sender(1,0));
		//			OffspringReceiverVector.push_back(Receiver(1,1));
		//		}
		//	} else {
		//		return -1;
		//	}

		//Start Generation Loop
		for (int g = 0; g < G; g++){

			//Assign m/N senders to T1. All senders are initialized as T2 by default
			std::shuffle(std::begin(nullVec), std::end(nullVec), rng);
			for (int i = 0; i < numT1; i++){
				SenderVector[nullVec[i]].Type = 1;
			}


			//Determine Fitnesses
			//Randomize order of sender and receiver populations
			//Then pair sender 1 with receiver 1, 2 with 2, etc
			//Randomize order again and repeat interactionPartner # of times

			for (int i = 0; i < interactionPartners; i++){

				//Shuffle population vectors
				std::shuffle(std::begin(SenderVector), std::end(SenderVector), rng);
				std::shuffle(std::begin(ReceiverVector), std::end(ReceiverVector), rng);

				for (int j = 0; j < N; j++){
					//Determine payoffs for each individual in this interaction.
					//Sender = SenderVector[j]
					//Receiver = ReceiverVector[j]
					bool signal = 0;
					if (SenderVector[j].Type == 1){ //High quality
						if (prob(rng) < SenderVector[j].s11){
							signal = 1;
						}
					} else { // type = 2, low quality
						if (prob(rng) < SenderVector[j].s01){
							signal = 1;
						}
					}
					//Now whether or not a signal has been sent has been determined

					int receiverAction = 0; //A1 or A2. A2 by default. 0 = A2
					if (signal == 1){ // signal received
						if (prob(rng) < ReceiverVector[j].r11){
							receiverAction = 1;
						}
					} else { // signal not received
						if (prob(rng) < ReceiverVector[j].r01){
							receiverAction = 1;
						}
					}

					//The actions of sender and receiver are decided - now determine payoffs
					double payoffSender = 0;
					double payoffReceiver = 0;

					if (SenderVector[j].Type == 1){
						if (signal == 1){
							if (receiverAction == 1){
								//T1, Signal, A1
								payoffSender = v1-c1;
								payoffReceiver = w1;
							} else {
								//T1, Signal, A2
								payoffSender = -c1;
								payoffReceiver = w2;
							}
						} else {
							if (receiverAction == 1){
								//T1, no Signal, A1
								payoffSender = v1;
								payoffReceiver = w1;
							} else {
								//T1, no Signal, A2
								//payoffSender = 0;
								payoffReceiver = w2;
							}
						}
					} else { //Sender type T2
						if (signal == 1){
							if (receiverAction == 1){
								//T2, Signal, A1
								payoffSender =  v2-c2;
								payoffReceiver = w3;
							} else {
								//T2, Signal, A2
								payoffSender = -c2;
								payoffReceiver = w4;
							}
						} else {
							if (receiverAction == 1){
								//T2, no Signal, A1
								payoffSender = v2;
								payoffReceiver = w3;
							} else {
								//T2, no Signal, A2
								//payoffSender = 0;
								payoffReceiver = w4;
							}
						}
					}
					SenderVector[j].fitness += payoffSender;
					ReceiverVector[j].fitness += payoffReceiver;
					//std::cout << "T" << SenderVector[j].Type <<  " sig: " << signal << " A" << receiverAction << " sender: " << payoffSender << " receiver: " << payoffReceiver << "\n";
				}//End loop for this individual
				//std::cout << "\n-----------------------------------\n\n";
			}//End loop for interaction partners

			//k-tournament selection
			//First, pick k individuals (k=2)
			//Determine highest fitness one
			//If tie, pick random
			//Winner reproduces
			//Repeat
			//randN(rng) produces random int from 0 to N-1

			for (int n = 0; n < N; n++){
				int rand1 = randN(rng);
				int rand2 = randN(rng);
				while (rand1 == rand2){
					rand2 = randN(rng);
				}
				if (SenderVector[rand1].fitness > SenderVector[rand2].fitness){
					OffspringSenderVector[n] = SenderVector[rand1];
					OffspringSenderVector[n].fitness = 0.0;
					OffspringSenderVector[n].Type = 2;
				} else if (SenderVector[rand1].fitness < SenderVector[rand2].fitness){
					OffspringSenderVector[n] = SenderVector[rand2];
					OffspringSenderVector[n].fitness = 0.0;
					OffspringSenderVector[n].Type = 2;
				} else {
					OffspringSenderVector[n] = SenderVector[rand1];
					OffspringSenderVector[n].fitness = 0.0;
					OffspringSenderVector[n].Type = 2;
				}

				//Receivers
				rand1 = randN(rng);
				rand2 = randN(rng);
				while (rand1 == rand2){
					rand2 = randN(rng);
				}
				if (ReceiverVector[rand1].fitness > ReceiverVector[rand2].fitness){
					OffspringReceiverVector[n] = ReceiverVector[rand1];
					OffspringReceiverVector[n].fitness = 0.0;
				} else if (ReceiverVector[rand1].fitness < ReceiverVector[rand2].fitness){
					OffspringReceiverVector[n] = ReceiverVector[rand2];
					OffspringReceiverVector[n].fitness = 0.0;
				} else {
					OffspringReceiverVector[n] = ReceiverVector[rand1];
					OffspringReceiverVector[n].fitness = 0.0;
				}
			}

			//Mutation
			for (int i = 0; i < N; i++){
				if (prob(rng) < mutRate){ //Mutation occurs to s01
					double mut = mutDist(rng);
					if (OffspringSenderVector[i].s01 == 1.0 & mut > 0.0){ //This ensures that all mutations have an effect - if the value is 1.0 and the mutation is positive, make the mutation negative
						mut = -1.0*mut;
					}
					if (OffspringSenderVector[i].s01 == 0.0 & mut < 0.0){ //This ensures that all mutations have an effect - if the value is 0.0 and the mutation is negative, make the mutation positive
						mut = -1.0*mut;
					}
					OffspringSenderVector[i].s01 += mut;
					OffspringSenderVector[i].s01 = std::max(0.0,std::min(OffspringSenderVector[i].s01,1.0));	//Truncate from 0 to 1
				}

				if (prob(rng) < mutRate){ //Mutation occurs to s11
					double mut = mutDist(rng);
					if (OffspringSenderVector[i].s11 == 1.0 & mut > 0.0){ //This ensures that all mutations have an effect - if the value is 1.0 and the mutation is positive, make the mutation negative
						mut = -1.0*mut;
					}
					if (OffspringSenderVector[i].s11 == 0.0 & mut < 0.0){ //This ensures that all mutations have an effect - if the value is 0.0 and the mutation is negative, make the mutation positive
						mut = -1.0*mut;
					}
					OffspringSenderVector[i].s11 += mut;
					OffspringSenderVector[i].s11 = std::max(0.0,std::min(OffspringSenderVector[i].s11,1.0));	//Truncate from 0 to 1
				}

				if (prob(rng) < mutRate){ //Mutation occurs to s01
					double mut = mutDist(rng);
					if (OffspringReceiverVector[i].r01 == 1.0 & mut > 0.0){ //This ensures that all mutations have an effect - if the value is 1.0 and the mutation is positive, make the mutation negative
						mut = -1.0*mut;
					}
					if (OffspringReceiverVector[i].r01 == 0.0 & mut < 0.0){ //This ensures that all mutations have an effect - if the value is 0.0 and the mutation is negative, make the mutation positive
						mut = -1.0*mut;
					}
					OffspringReceiverVector[i].r01 += mut;
					OffspringReceiverVector[i].r01 = std::max(0.0,std::min(OffspringReceiverVector[i].r01,1.0));	//Truncate from 0 to 1
				}

				if (prob(rng) < mutRate){ //Mutation occurs to s01
					double mut = mutDist(rng);
					if (OffspringReceiverVector[i].r11 == 1.0 & mut > 0.0){ //This ensures that all mutations have an effect - if the value is 1.0 and the mutation is positive, make the mutation negative
						mut = -1.0*mut;
					}
					if (OffspringReceiverVector[i].r11 == 0.0 & mut < 0.0){ //This ensures that all mutations have an effect - if the value is 0.0 and the mutation is negative, make the mutation positive
						mut = -1.0*mut;
					}
					OffspringReceiverVector[i].r11 += mut;
					OffspringReceiverVector[i].r11 = std::max(0.0,std::min(OffspringReceiverVector[i].r11,1.0));	//Truncate from 0 to 1
				}
			}

			if (g%reportFreq == 0){
				if (computeMeansInCpp == false){

					for (int i = 0; i < N; i++){
						dataLog << "\n" << rep << "," << g << "," << i << ",Sender," << SenderVector[i].Type << "," << SenderVector[i].s01 << "," << SenderVector[i].s11 << "," << SenderVector[i].fitness;
					}

					for (int i = 0; i < N; i++){
						dataLog << "\n" << rep << "," << g << "," << i << ",Receiver,na," << ReceiverVector[i].r01 << "," << ReceiverVector[i].r11 << "," << ReceiverVector[i].fitness;
					}
				} else {
					//Compute means
					long double total_s01 = 0.0;
					long double total_s11 = 0.0;
					long double total_r01 = 0.0;
					long double total_r11 = 0.0;

					long double totalFit_s = 0.0;
					long double totalFit_r = 0.0;

					for (int i = 0; i < N; i++){

						total_s01 += SenderVector[i].s01;
						total_s11 += SenderVector[i].s11;
						total_r01 += ReceiverVector[i].r01;
						total_r11 += ReceiverVector[i].r11;

						totalFit_s += SenderVector[i].fitness;
						totalFit_r += ReceiverVector[i].fitness;
					}

					double mean_s01 = total_s01/N;
					double mean_s11 = total_s11/N;
					double mean_r01 = total_r01/N;
					double mean_r11 = total_r11/N;

					double meanFit_s = totalFit_s/N;
					double meanFit_r = totalFit_r/N;

					//					   "rep,      gen,      indType,   strat01,            strat11,           meanFit,             expAlphaBeta,    seed,       N,      G,      c1,      c2,      v1,      v2,      w1,     w2,       w3,      w4,     m,       mCorrected,      interactionPartners,     mutRate,       mutStep,     init01,        init11,replicates,cauchyDist";
					summaryStats << "\n" << rep << "," << g << ",Sender," << mean_s01 << "," << mean_s11 << "," << meanFit_s << "," << expAlpha  <<","<< seed<<","<<N<<","<<G<<","<<c1<<","<<c2<<","<<v1<<","<<v2<<","<<w1<<","<<w2<<","<<w3<<","<<w4<<","<<m<<","<<mCorrected<<","<<interactionPartners<<","<<mutRate<<","<<mutStep<<","<<initS01<<","<<initS11<<","<<replicates<<","<<cauchyDist;
					summaryStats << "\n" << rep << "," << g << ",Receiver," << mean_r01 << "," << mean_r11 << "," << meanFit_r << "," << expBeta  <<","<< seed<<","<<N<<","<<G<<","<<c1<<","<<c2<<","<<v1<<","<<v2<<","<<w1<<","<<w2<<","<<w3<<","<<w4<<","<<m<<","<<mCorrected<<","<<interactionPartners<<","<<mutRate<<","<<mutStep<<","<<initR01<<","<<initR11<<","<<replicates<<","<<cauchyDist;

					//Write means to summaryStats log.
				}
			}

			//Replace Parents with Offspring
			SenderVector.swap(OffspringSenderVector);
			ReceiverVector.swap(OffspringReceiverVector);

		}//End Generation Loop
	}//End replicate loop

	dataLog.close();
	params.close();
	summaryStats.close();

	std::cout << "\nDone";
	return 0;
}



