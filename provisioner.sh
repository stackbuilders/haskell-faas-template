#!/bin/bash

set -e

function install_microk8s () {
  sudo snap install microk8s --classic

  # After installing, it is needed to enable 
  # the needed modules and start the service
  microk8s status --wait-ready
  microk8s enable dns dashboard storage registry
  microk8s start
}

function microk8s_permissions () {
  # To avoid the permission insuficience, 
  # is recommended to run the following commands
  sudo usermod -a -G microk8s vagrant
  sudo chown -f -R vagrant ~/.kube
  newgrp microk8s
}

function install_openfaas_cli () {
  curl -sSL https://cli.openfaas.com | sudo -E sh
}

function openfaas_namespaces () {
  git clone https://github.com/openfaas/faas-netes

  # Create the OpenFaaS namespaces in order to build the functions
  cd faas-netes
  microk8s kubectl apply -f ./namespaces.yml
  microk8s kubectl apply -f ./yaml
}

function create_registry () {
  # Create local docker registry
  docker run -d -p 5000:5000  --restart=always --name registry registry:2
}

install_microk8s
microk8s_permissions
install_openfaas_cli
openfaas_namespaces
create_registry