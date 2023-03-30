#include "parameters.h"

void from_json(const nlohmann::json& j, parameters& t)
{
	NLOHMANN_JSON_FROM(seed);
	NLOHMANN_JSON_FROM(N);
	NLOHMANN_JSON_FROM(G);
	NLOHMANN_JSON_FROM(c1);
	NLOHMANN_JSON_FROM(c2);
	NLOHMANN_JSON_FROM(v1);
	NLOHMANN_JSON_FROM(v2);
	NLOHMANN_JSON_FROM(w1);
	NLOHMANN_JSON_FROM(w2);
	NLOHMANN_JSON_FROM(w3);
	NLOHMANN_JSON_FROM(w4);
	NLOHMANN_JSON_FROM(m);
	NLOHMANN_JSON_FROM(interactionPartners);
	NLOHMANN_JSON_FROM(mutRateAlpha);
	NLOHMANN_JSON_FROM(mutRateBeta);
	NLOHMANN_JSON_FROM(mutRateStrategySender);
	NLOHMANN_JSON_FROM(mutRateStrategyReceiver);
	NLOHMANN_JSON_FROM(mutStepAlpha);
	NLOHMANN_JSON_FROM(mutStepBeta);
	NLOHMANN_JSON_FROM(alphaBetaMutation);
	NLOHMANN_JSON_FROM(initializationType);
	NLOHMANN_JSON_FROM(cauchyDist);
	NLOHMANN_JSON_FROM(initStrategySender);
	NLOHMANN_JSON_FROM(initStrategyReceiver);
	NLOHMANN_JSON_FROM(initAlpha);
	NLOHMANN_JSON_FROM(initBeta);
	NLOHMANN_JSON_FROM(replicates);
	NLOHMANN_JSON_FROM(coutReport);
	NLOHMANN_JSON_FROM(reportFreq);
	NLOHMANN_JSON_FROM(dataFileName);
	NLOHMANN_JSON_FROM(dataFileFolder);

}
