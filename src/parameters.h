#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "json.hpp"

struct parameters
{
	int	seed	=	123456789;
	double	N	=	100000;
	int	G	=	100000;
	double	c1	=	0.1;
	double	c2	=	0.5;
	double	v1	=	1;
	double	v2	=	1;
	double	w1	=	1;
	double	w2	=	0;
	double	w3	=	0;
	double	w4	=	2;
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
	std::string	dataFileFolder	=	"./";
	bool	computeMeansInCpp	=	true;
};

void from_json(const nlohmann::json& j, parameters& p);


#endif // SIM_PARAMETERS_H
