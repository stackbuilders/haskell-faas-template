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
    path: "provisioner.sh"
end
