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
	int Strategy;
	double Alpha;
	double fitness;
	int Type;	//T1 or T2

	Sender(int Str, double ab){
		Strategy = Str;
		Alpha = ab;
		fitness = 0;
		Type = 2;	//Default
	}

	//	void setFitness(double val){
	//		fitness = val;
	//	}
};

class Receiver {
public:
	int Strategy;
	double Beta;
	double fitness;

	Receiver(int Str, double ab){
		Strategy = Str;
		Beta = ab;
		fitness = 0;
	}

	//	void setFitness(double val){
	//		fitness = val;
	//	}
};


int main(int argc, char* argv[]) {

	//Notes
	//When c1 and c2 are close, alpha is much lower than expectation (N = 1000)


	//Take parameters - old way
	/*
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

	string alphaBetaMutation = "always"; //"always" or "strict" or "random". Always = alpha and beta can mutate with any strategy. Strict = alpha and beta can only mutate with strategy 1. Random = a new alpha or beta are drawn when an individual mutates to strategy 1

	string initializationType = "random"; //"random" or "parameter". For latter option, see below

	bool cauchyDist = true; //If true, use caucy dist for mutations. If false, use normal dist

	int initStrategySender = 1;
	int initStrategyReceiver = 1;
	int initAlpha = 1;
	int initBeta = 0;

	int replicates = 1;

	int coutReport = 0;
	int reportFreq = 50; //Export data every this many generations

	string dataFileName = "same";
	string dataFileFolder = "C:/Users/owner/Documents/S4/Simulation";
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

	double mutRateAlpha = 	sim_pars.mutRateAlpha;	//Chance of a mutation occurring each generation
	double mutRateBeta = 	sim_pars.mutRateBeta;
	double mutRateStrategySender =   sim_pars.mutRateStrategySender;
	double mutRateStrategyReceiver = sim_pars.mutRateStrategyReceiver;
	double mutStepAlpha = 	sim_pars.mutStepAlpha;	//SD of a normal distribution of mutation size with mean 0
	double mutStepBeta = 	sim_pars.mutStepBeta;

	std::string alphaBetaMutation = sim_pars.alphaBetaMutation; //"always" or "strict" or "random". Always = alpha and beta can mutate with any strategy. Strict = alpha and beta can only mutate with strategy 1. Random = a new alpha or beta are drawn when an individual mutates to strategy 1
	if (alphaBetaMutation != "always" && alphaBetaMutation != "strict" && alphaBetaMutation != "random") {
		std::cout << "Invalid entry for alphaBetaMutation";
		return 91;
	}

	std::string initializationType = sim_pars.initializationType; //"random" or "parameter". For latter option, see below
	if (initializationType != "random" && initializationType != "parameter"){
		std::cout << "Invalid entry for initializationType";
		return 92;
	}

	bool cauchyDist = sim_pars.cauchyDist; //If true, use caucy dist for mutations. If false, use normal dist

	int initStrategySender = sim_pars.initStrategySender;
	int initStrategyReceiver = sim_pars.initStrategyReceiver;
	int initAlpha = sim_pars.initAlpha;
	int initBeta = sim_pars.initBeta;

	int replicates = sim_pars.replicates;

	int coutReport = sim_pars.coutReport;	//0 = don't report
	int reportFreq = sim_pars.reportFreq; //Export data every this many generations

	std::string dataFileName = sim_pars.dataFileName;
	std::string dataFileFolder = sim_pars.dataFileFolder;

	//_________End parameter input


	const std::time_t now = std::time(nullptr) ; // get the current time point
	const std::tm calendar_time = *std::localtime( std::addressof(now) ) ;

	std::ofstream dataLog;
	std::ofstream params;

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
	dataLog.open(str1);
	params.open(str2);


	dataLog << "rep,gen,ind,indType,sendType,strategy,alphaBeta,fitness";
	params << "seed,N,G,c1,c2,v1,v2,w1,w2,w3,w4,m,interactionPartners,mutRateAlpha,mutRateBeta,mutRateStrategySender,mutRateStrategyReceiver,mutStepAlpha,mutStepBeta,initializationType,initStrategySender,initStrategyReceiver,initAlpha,initBeta,replicates,alphaBetaMutation,cauchyDist";
	params << "\n" << to_string(seed) << "," << to_string(N) <<","<< to_string(G) <<","<< to_string(c1) <<","<< to_string(c2) <<","<< to_string(v1)<<","<<to_string(v2)<<","<<to_string(w1)<<","<<to_string(w2)<<","<<to_string(w3)<<","<<to_string(w4)<<","<<to_string(m)<<","<<to_string(interactionPartners)<<","<<to_string(mutRateAlpha)<<","<<to_string(mutRateBeta)<<","<<to_string(mutRateStrategySender)<<","<<to_string(mutRateStrategyReceiver)<<","<<to_string(mutStepAlpha)<<","<<to_string(mutStepBeta)<<","<<initializationType<<","<<to_string(initStrategySender)<<","<<to_string(initStrategyReceiver)<<","<<to_string(initAlpha)<<","<<to_string(initBeta)<<","<<to_string(replicates)<<","<<alphaBetaMutation<<","<<to_string(cauchyDist),"\n";

	auto MutationDistAlpha = std::normal_distribution<double>(0.0, std::abs(mutStepAlpha));
	auto MutationDistBeta = std::normal_distribution<double>(0.0, std::abs(mutStepBeta));

	if (cauchyDist == 1) {
		auto MutationDistAlpha = std::cauchy_distribution<double>(0.0, std::abs(mutStepAlpha));
		auto MutationDistBeta = std::cauchy_distribution<double>(0.0, std::abs(mutStepBeta));
	}

	//Random number generators
	//auto rd = std::random_device {}; 	//This should be changed to a parameterized seed variable
	//auto rng = std::default_random_engine { rd() };
	auto rng = std::default_random_engine {seed};
	std::uniform_real_distribution<double> prob(0,1);
	std::uniform_int_distribution bol(1,3);

	//Fitness distribution
	auto SenderFitnessDist = rndutils::mutable_discrete_distribution<int, rndutils::all_zero_policy_uni>{};
	auto ReceiverFitnessDist = rndutils::mutable_discrete_distribution<int, rndutils::all_zero_policy_uni>{};

	//Vector used for drawing random numbers
	std::vector<int> nullVec;
	for (int i = 0; i < N; i++){
		nullVec.push_back(i);
	}

	//Determine max and min possible fitnesses for normalization
	double maxReceiverFit = double(interactionPartners)*double(std::max(w1,std::max(w2,std::max(w3,w4))));
	double minReceiverFit = double(interactionPartners)*double(std::min(w1,std::min(w2,std::min(w3,w4))));
	double maxSenderFit = double(interactionPartners)* (max(v1,v2) - std::min(0.0,std::min(c1,c2)));	//To account for negative costs which will increase max fitness
	double minSenderFit = double(interactionPartners)*(-1.0*std::max(c1,c2) + std::min(0.0,std::min(v1,v2)));	//To account for negative benefits

	cout << maxReceiverFit << endl << minReceiverFit << endl << maxSenderFit << endl << minSenderFit << endl;

	if (minReceiverFit == maxReceiverFit){	//To avoid divide by 0
		maxReceiverFit += 0.00001;
	}
	if (minSenderFit == maxSenderFit){
		maxSenderFit += 0.00001;
	}

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
		if (initializationType == "parameter"){
			for (int i = 0; i < N; i++){
				SenderVector.push_back(Sender(initStrategySender,initAlpha));
				ReceiverVector.push_back(Receiver(initStrategyReceiver,initBeta));

				OffspringSenderVector.push_back(Sender(1,0));
				OffspringReceiverVector.push_back(Receiver(1,1));
			}
		} else if (initializationType == "random"){
			for (int i = 0; i < N; i++){
				SenderVector.push_back(Sender(bol(rng),prob(rng)));
				ReceiverVector.push_back(Receiver(bol(rng),prob(rng)));

				OffspringSenderVector.push_back(Sender(1,0));
				OffspringReceiverVector.push_back(Receiver(1,1));
			}
		} else {
			return -1;
		}

		int numT1 = round(m*N);	//**This rounding could produce minor deviations from analytic results
		//Start Generation Loop
		for (int g = 0; g < G; g++){

			//Assign m/N senders to T1. All senders are initialized as T2 by default
			std::shuffle(std::begin(nullVec), std::end(nullVec), rng);
			for (int i = 0; i < numT1; i++){
				SenderVector[nullVec[i]].Type = 1;
			}



			//Determine Fitnesses

			//Method 1 - Whole Population
			/*
		//Determine fitness for strategy 2 and 3 for receiver and senders (these will be the same for all individuals because A and B don't matter

		//Determine Composition of Sender and Receiver Populations

		double numSS1 = 0; //Number of individuals using sender strategy 1
		double numSS2 = 0; //Stored as doubles so that nothing gets cast as int in calculations
		double numSS3 = 0;
		double numRS1 = 0;
		double numRS2 = 0;
		double numRS3 = 0;
		long double totalAlpha = 0;
		long double totalBeta = 0;
		double meanAlpha = 0;
		double meanBeta = 0;

		for (int i = 0; i < SenderVector.size(); i++){
			if (SenderVector[i].Strategy == 1){
				numSS1++;
				totalAlpha += SenderVector[i].AlphaBeta;
			} else if (SenderVector[i].Strategy == 2){
				numSS2++;
			} else {
				numSS3++;
			}
		}
		meanAlpha = totalAlpha/numRS1;

		for (int i = 0; i < ReceiverVector.size(); i++){
			if (ReceiverVector[i].Strategy == 1){
				numRS1++;
				totalBeta += ReceiverVector[i].AlphaBeta;
			} else if (ReceiverVector[i].Strategy == 2){
				numRS2++;
			} else {
				numRS3++;
			}
		}
		meanBeta = totalBeta/numRS1;


		//Senders
		//Strategy SS2: Never Signal
		//Fitness based only on receiver action (receivers playing A1 to no signal), which occurs in RS2 and RS3.
		double fitnessSS2 = 0;
		if (numSS2 > 0){
			fitnessSS2 = m*v1*((numRS2+numRS3)/N) + (1-m)*v2*((numRS2+numRS3)/N);
		}

		//Strategy SS3: Signal if T2. Not if T1
		double fitnessSS3 = 0;
		if (numSS3 > 0){
			//Fitness = signal,A1 + signal,A2, + noSignal,A1 + noSignal,A2
			fitnessSS3 = ((1-m)*(v1-c2)*(meanBeta*numRS1+numRS2)/N) +
					((1-m)*(0-c2)*(1-meanBeta)*(numRS1+numRS3)/N) +
					 m*v1*((numRS2+numRS3)/N) +
					 0;
		}

		//Strategy SS1:
		//We can determine a constant value which needs to be multiplied by alpha to give to each sender agent

		//Fitness = T1,Sig,A1 + T1,Sig,A2 + T2,Sig,A1 + T2,Sig,A2 + T1,noSig,A1 + T1,noSig,A2 + T2,noSig,A1 + T2,noSig,A2
		(m*(v1-c1)*(numRS1*meanBeta+numRS2)/N) +
				(m*(0-c1)*(numRS1*(1-meanBeta)+numRS3)/N) +
	wip
			 */

			//Method 2 - Pairwise Comparisons

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
					if (SenderVector[j].Type == 1 && SenderVector[j].Strategy == 1){
						signal = 1;
					} else if (SenderVector[j].Type == 2 && SenderVector[j].Strategy == 1){
						//Determine using alpha...
						if (prob(rng) < SenderVector[j].Alpha){
							signal = 1;
						}
					} else if (SenderVector[j].Type == 2 && SenderVector[j].Strategy == 3){
						signal = 1;
					}

					int receiverAction = 2; //A1 or A2. A2 by default
					if (signal == 1){
						if (ReceiverVector[j].Strategy == 1){
							if (prob(rng) < ReceiverVector[j].Beta){
								receiverAction = 1;
							}
						} else if (ReceiverVector[j].Strategy == 2){
							receiverAction = 1;
						}
					} else { //no signal
						if (ReceiverVector[j].Strategy == 2 || ReceiverVector[j].Strategy == 3){
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

			//SenderVector[10].fitness = 10;

			//Reproduction
			//Using a discrete distribution of fitnesses

			//Determine max and min fitnesses for normalization - now this is done earlier.
			/*
			double minSenderFit = SenderVector[1].fitness;
			double maxSenderFit = SenderVector[1].fitness;
			double minReceiverFit = ReceiverVector[1].fitness;
			double maxReceiverFit = ReceiverVector[1].fitness;

			for (int i = 0; i < N; i++){
				if (SenderVector[i].fitness > maxSenderFit){
					maxSenderFit = SenderVector[i].fitness;
				} else if (SenderVector[i].fitness < minSenderFit){
					minSenderFit = SenderVector[i].fitness;
				}
				if (ReceiverVector[i].fitness > maxReceiverFit){
					maxReceiverFit = ReceiverVector[i].fitness;
				} else if (ReceiverVector[i].fitness < minReceiverFit){
					minReceiverFit = ReceiverVector[i].fitness;
				}
			}
			 */

			for (int i = 0; i < N; i++){
				if (SenderVector[i].fitness < minSenderFit){
					//	cout << SenderVector[i].fitness << " " << minSenderFit <<" minYYY";
					//return 2;
				}
			}
			//cout << endl;

			for (int i = 0; i < N; i++){
				//With normalization
				//Round to nearest 4 decimal places - because double numbers are imprecise, there are errors when comparing using <. E.g., 0.7 < 0.7 will evaluate as true sometimes for doubles.
				//This causes errors because sender fitnesses after normalization can be slightly negative. Rounding fixes the issue.
				//Though there is likely a faster way to resolve this.
				if (SenderVector[i].fitness < minSenderFit){
					SenderFitnesses[i] = round(  (SenderVector[i].fitness - minSenderFit)/(maxSenderFit-minSenderFit) * 10000 ) / 10000;
				} else {
					SenderFitnesses[i] = (SenderVector[i].fitness - minSenderFit)/(maxSenderFit-minSenderFit);
				}
				ReceiverFitnesses[i] = (ReceiverVector[i].fitness - minReceiverFit)/(maxReceiverFit-minReceiverFit);

			}

			SenderFitnessDist.mutate(SenderFitnesses.cbegin(), SenderFitnesses.cend());
			ReceiverFitnessDist.mutate(ReceiverFitnesses.cbegin(), ReceiverFitnesses.cend());

			//Determining parents of offspring. Store offspring in Offspring vector
			for (int i = 0; i < N; i++){
				//cout << SenderFitnessDist(rng) << " ";
				OffspringSenderVector[i] = SenderVector[SenderFitnessDist(rng)];
				OffspringReceiverVector[i] = ReceiverVector[ReceiverFitnessDist(rng)];

				OffspringSenderVector[i].fitness = 0;
				OffspringReceiverVector[i].fitness = 0;
				OffspringSenderVector[i].Type = 2;
			}

			//Mutation
			for (int i = 0; i < N; i++){
				if (prob(rng) < mutRateStrategySender){
					int mut = bol(rng);
					while (OffspringSenderVector[i].Strategy == mut){
						mut = bol(rng);
					}
					OffspringSenderVector[i].Strategy = mut;
					if (mut == 1 && alphaBetaMutation == "random"){
						OffspringSenderVector[i].Alpha = prob(rng);
					}
				}

				if (prob(rng) < mutRateStrategyReceiver){
					int mut = bol(rng);
					while (OffspringReceiverVector[i].Strategy == mut){
						mut = bol(rng);
					}
					OffspringReceiverVector[i].Strategy = mut;
					if (mut == 1 && alphaBetaMutation == "random"){
						OffspringReceiverVector[i].Beta = prob(rng);
					}
				}

				if (OffspringSenderVector[i].Strategy == 1 || alphaBetaMutation == "always"){
					if (prob(rng) < mutRateAlpha){
						OffspringSenderVector[i].Alpha += MutationDistAlpha(rng);
						if (OffspringSenderVector[i].Alpha < 0){	//These corrections will make 0 and 1 'sticky', because half of the mutations
							OffspringSenderVector[i].Alpha = 0;		//when alpha or beta = 0 or 1 will be thrown out. Is this bad?
						}
						if (OffspringSenderVector[i].Alpha > 1){
							OffspringSenderVector[i].Alpha = 1;
						}
					}
				}

				if (OffspringReceiverVector[i].Strategy == 1 || alphaBetaMutation == "always"){
					if (prob(rng) < mutRateBeta){
						OffspringReceiverVector[i].Beta += MutationDistBeta(rng);
						if (OffspringReceiverVector[i].Beta < 0){
							OffspringReceiverVector[i].Beta = 0;
						}
						if (OffspringReceiverVector[i].Beta > 1){
							OffspringReceiverVector[i].Beta = 1;
						}
					}
				}
			}

		//	if (g%500 == 0){
		//		cout <<" " << g;
		//	}

			if (g%reportFreq == 0){
				if (coutReport == 1){
					for (int i = 0; i < 20; i++){
						cout << SenderVector[i].Strategy << " " << SenderVector[i].Alpha << " " << SenderVector[i].fitness << " | ";
					}
					cout << endl;
					for (int i = 0; i < 20; i++){
						cout << ReceiverVector[i].Strategy << " " << ReceiverVector[i].Beta << " " << ReceiverVector[i].fitness << " | ";
					}
					cout << "\n--------------------------\n";
				}

				for (int i = 0; i < N; i++){
					dataLog << "\n" << rep << "," << g << "," << i << ",Sender," << SenderVector[i].Type << "," << SenderVector[i].Strategy << "," << SenderVector[i].Alpha << "," << SenderVector[i].fitness;
				}

				for (int i = 0; i < N; i++){
					dataLog << "\n" << rep << "," << g << "," << i << ",Receiver,na," << ReceiverVector[i].Strategy << "," << ReceiverVector[i].Beta << "," << ReceiverVector[i].fitness;
				}
			}

			//Replace Parents with Offspring
			SenderVector.swap(OffspringSenderVector);
			ReceiverVector.swap(OffspringReceiverVector);

		}//End Generation Loop

	}//End replicate loop

	dataLog.close();
	params.close();

	std::cout << "\nDone";
	return 0;
}



