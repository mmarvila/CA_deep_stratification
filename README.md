# Survival model of cardiovascular risk prediction using deep features from cartoid artery

This repository contains the code to reproduce the results in "[Deep-stratification of the cardiovascular risk by ultrasound carotid artery images](https://www.sciencedirect.com/science/article/pii/S1746809424000934)", Maria del Mar Vila, by Lucas Gago, Pablo Pérez-Sánchez, Maria Grau, Beatriz Remeseiro and Laura Igual, published in Computer Methods and Programs in Biomedicine.


The code creates the survival model using the deep features obtained with https://github.com/gagolucasm/DL_CIMT_and_plaque_estimation/tree/master

It also includes the code to perform the copmarison between the Model 0 (REGICOR risk function) and the Model 1 (the deep stratification model presented in the mentioned paper). The comaprison is performed using the measurement of AUC and Net Reclassification Improvement (NRI).


This code was trained on the [REGICOR](https://regicor.cat/en/introduction/) database. It can be requested for academic purposes.

