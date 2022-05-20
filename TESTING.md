# Testing the template

## Tools to install

To test the FaaS template you may need to install the following tools:

- [MicroK8s](https://ubuntu.com/tutorials/install-a-local-kubernetes-with-microk8s#1-overview)
- [Docker](https://docs.docker.com/get-docker/)
- [OpenFaaS CLI](https://docs.openfaas.com/cli/install/)

## 1. Step: Setting up Microk8s

Make sure Microk8s has the right privileges by using the following commands:

```
$ sudo usermod -a -G microk8s <user>
$ sudo chown -f -R <user> ~/.kube
$ newgrp microk8s
```

Check if the Kubernetes status to check everything works correctly, and enable all the needed tools:

```
$ microk8s status --wait-ready
$ microk8s enable dns dashboard storage
```

Use `microk8s start` to run the Kubernetes and `microk8s stop` to stop it (stop the microk8s after completing the test).

## 2. Step: Cloning the OpenFaaS repo

Start by cloning the Git Repo of openfaas/faas-netes:

`git clone https://github.com/openfaas/faas-netes`

Access to the repo and create the namespaces and services:

```
$ cd faas-netes
$ microk8s kubectl apply -f ./namespaces.yml
$ microk8s kubectl apply -f ./yaml
```

Make sure the OpenFaaS is deployed with the command `microk8s kubectl get deployments --namespace openfaas` and the ouput should be similar to:

NOT WORKING YET
[Imgur](https://imgur.com/72BfGWY)

## 3. Step: Setup password

Also, it is necessary to create a password to access the OpenFaaS dashboard and visualize the deployed functions. This process is described below:

`PASSWORD=$(head -c 12 /dev/urandom | shasum| cut -d' ' -f1)`

If needed, you can `echo $PASSWORD` to make sure the password is created.

Now, you need to set the password for the OpenFaaS, and then it is necessary to stop the gateway and start it again:

```
$ microk8s kubectl -n openfaas create secret generic basic-auth --from-literal=basic-auth-user=admin --from-literal=basic-auth-password="$PASSWORD"
$ microk8s kubectl --namespace openfaas scale --replicas=0 deploy/gateway
$ microk8s kubectl --namespace openfaas scale --replicas=1 deploy/gateway
```

Make sure the gateway is running by using `microk8s kubectl get deployments --namespace openfaas` once again. Everything should be ready. It is possible that a restart may be required.

## 4. Create the function

Before creating a function, you may configure a SSH key for GitHub if using ssh. Or url.

Now, you need to pull the template in order to create the function:

`faas-cli template pull https://github.com/stackbuilders/haskell-faas-template.git`

Create your sample function using:

`faas-cli new --lang haskell-http --prefix ghcr.io/<user> <my-function>`

Build the docker image with:

`faas-cli build -f ./<my-function>.yml`

