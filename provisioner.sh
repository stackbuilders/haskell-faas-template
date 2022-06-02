#!/bin/bash

set -e

function install_microk8s () {
  # Install microk8s
  sudo snap install microk8s --classic

  # Start the service with all modules enabled
  microk8s status --wait-ready
  microk8s enable dns dashboard storage registry
  microk8s start
}

function microk8s_permissions () {
  # Add all the necessary packages and permissions to use microk8s
  sudo usermod -a -G microk8s vagrant
  sudo chown -f -R vagrant ~/.kube
  newgrp microk8s
}

function install_openfaas () {
  # Install openfaas-cli
  curl -sSL https://cli.openfaas.com | sudo -E sh
}

function clone_faasnetes () {
  # Clone the faasnetes git repo in order to create the functions
  git clone https://github.com/openfaas/faas-netes
  cd faas-netes
}

function openfaas_namespaces () {
  # Create the OpenFaaS namespaces for the functions
  microk8s kubectl apply -f ./namespaces.yml
  microk8s kubectl apply -f ./yaml
}

function create_registry () {
  # Create local docker registry
  docker run -d -p 5000:5000  --restart=always --name registry registry:2
}

install_microk8s
microk8s_permissions
install_openfaas
clone_faasnetes
openfaas_namespaces
create_registry