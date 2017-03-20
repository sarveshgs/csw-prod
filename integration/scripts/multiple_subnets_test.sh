#!/usr/bin/env bash

RED='\033[0;31m'
YELLOW='\033[1;33m'
ORANGE='\033[0;33m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

HOST_DIR_MAPPING=$1
printf "${YELLOW} Executing multiple subnet test... ${NC}\n"
printf "${PURPLE} Creating docker subnet : tmt_net_1 ${NC}\n"

docker network create -d macvlan --subnet=192.168.210.0/24 --subnet=192.168.220.0/24 --gateway=192.168.210.254 --gateway=192.168.220.254 -o parent=eth0.10 -o macvlan_mode=bridge macvlan-10

docker run --name=lan10.1 --net=macvlan-10 --ip=192.168.210.10 -d -v /vagrant/:/source/ -v /home/vagrant/.ivy2/:/root/.ivy2 tmt/local-csw-centos tail -f /dev/null
docker run --name=lan10.2 --net=macvlan-10 --ip=192.168.220.10 -d -v /vagrant/:/source/ -v /home/vagrant/.ivy2/:/root/.ivy2 tmt/local-csw-centos tail -f /dev/null
docker run --name=lan10.3 --net=macvlan-10 --ip=192.168.210.5 -d -v /vagrant/:/source/ -v /home/vagrant/.ivy2/:/root/.ivy2 tmt/local-csw-centos tail -f /dev/null

printf "${YELLOW}----------- Starting HCD App -----------${NC}\n"
docker exec -d lan10.1 bash -c 'cd source && export PORT=2555;./integration/target/universal/integration-10000/bin/trombone-h-c-d'

printf "${PURPLE}------ Waiting for 10 seconds to boot up HCD ------${NC}\n"
sleep 10

printf "${YELLOW}----------- Starting Redis App -----------${NC}\n"
docker exec -d lan10.2 bash -c 'cd source && export PORT=2556;./integration/target/universal/integration-10000/bin/test-service'

printf "${PURPLE}------ Waiting for 10 seconds to boot up Reddis ------${NC}\n"
sleep 10

printf "${YELLOW}------ Starting Test App ------${NC}\n"
docker exec -it lan10.3 bash -c 'cd source && export PORT=2557;./integration/target/universal/integration-10000/bin/test-app'
test_exit_code=$?

printf "${ORANGE}------ [Debug] Inspecting network information ------${NC}"

docker network inspect macvlan-10

printf "${PURPLE}---------- Stopping and Removing all docker containers ---------- ${NC}"

docker stop lan10.1 lan10.2 lan10.3
docker rm lan10.1 lan10.2 lan10.3

docker network rm macvlan-10

exit ${test_exit_code}