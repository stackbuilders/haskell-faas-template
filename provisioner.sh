#!/bin/bash

# Step 1. Install openfaas-cli
curl -sSL https://cli.openfaas.com | sudo -E sh

# Step 2. Install microk8s and configure it
# This step will add all the necessary packages
# And permissions to use microk8s as needed
sudo snap install microk8s --classic
sudo usermod -a -G microk8s vagrant
sudo chown -f -R vagrant ~/.kube
newgrp microk8s
microk8s status --wait-ready
microk8s enable dns dashboard storage registry
microk8s start

# Step 3. Clone OpenFaaS repo in order to use it
git clone https://github.com/openfaas/faas-netes
cd faas-netes

# Step 4. Create the OpenFaaS namespaces for the functions
microk8s kubectl apply -f ./namespaces.yml
microk8s kubectl apply -f ./yaml

# Step 5. Create a local Docker container registry 
docker run -d -p 5000:5000  --restart=always --name registry registry:2