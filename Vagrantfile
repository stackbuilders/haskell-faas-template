$script = <<SCRIPT
sudo usermod -a -G microk8s vagrant
sudo chown -f -R vagrant ~/.kube
newgrp microk8s
microk8s status --wait-ready
microk8s enable dns dashboard storage registry
microk8s start
git clone https://github.com/openfaas/faas-netes
cd faas-netes
microk8s kubectl apply -f ./namespaces.yml
microk8s kubectl apply -f ./yaml
docker run -d -p 5000:5000  --restart=always --name registry registry:2
microk8s kubectl -n openfaas create secret generic basic-auth --from-literal=basic-auth-user=admin --from-literal=basic-auth-password=123
microk8s kubectl --namespace openfaas scale --replicas=0 deploy/gateway
microk8s kubectl --namespace openfaas scale --replicas=1 deploy/gateway
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "hashicorp/bionic64"
  config.vm.provider "virtualbox" do |v|
    v.memory = 4096
    v.cpus = 2
  end

  config.vm.network :forwarded_port, guest: 31112, host: 31112

  # Installs
  config.vm.provision "docker"
  config.vm.provision "shell",
    inline: "curl -sSL https://cli.openfaas.com | sudo -E sh"
  config.vm.provision "shell",
    inline: "sudo snap install microk8s --classic"

  config.vm.provision "shell",
    inline: $script
end
