# Testing the template

You can test the function using the Vagrant VM provided by the Vagrantfile. This VM has been configured in order to directly start in step 3. You may need to install [Vagrant](https://www.vagrantup.com/docs/installation) and use the following commands:

```
#To start the VM
$ vagrant up

#To access to SSH
$ vagrant ssh

#To stop the VM
$ vagrant halt

#To destroy VM instance
$ vagrant destroy
```

NOTE: You will need to access to the microk8s dashboard through a tunnel configured in the Vagrantfile. By default it is located at http://localhost:31112 in your host machine but you can change it as needed.

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

Make sure the OpenFaaS is deployed with the command `microk8s kubectl get deployments --namespace openfaas` and every state should be 1/1.

## 3. Step: Setup password

Also, it is necessary to create a password to access the OpenFaaS dashboard and visualize the deployed functions. This process is described below:

`PASSWORD=$(head -c 12 /dev/urandom | shasum| cut -d' ' -f1)`

If needed, you can `echo $PASSWORD` to make sure the password is created.

Now, you need to set the password for the OpenFaaS, and then it is necessary to stop the gateway and start it again:

```
$ microk8s kubectl -n openfaas create secret generic basic-auth --from-literal=basic-auth-user=admin --from-literal=basic-auth-password=$PASSWORD
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

Once the function is created, change the gateway port to 31112 using:

`nano <my-function>.yml`

Before building the image, make sure you are logged in using:

`faas-cli login -p <password> --gateway 127.0.0.1:31112`

Build the docker image with:

`faas-cli build -f ./<my-function>.yml`

And before pushing the image package, you will need to login to docker with:

`docker login ghcr.io -u <user> -p <PAT>`

NOTE: This login method will use [GHCR](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry) as registry to upload containers. Check out about creating a PAT ([Personal Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token)).

Now you can push your function package to GHCR:

`faas-cli push -f ./<my-function>.yml`

And deploy it to OpenFaaS:

`faas-cli deploy -f ./<my-function>.yml`

## 5. Test the function

Finally, you can test the function accessing to http://localhost:31112 or using `microk8s dashboard`. Use the user and password created at step 3 (by default admin is the user). In this dashboard you can visualize all the created and deployed functions and test them.
