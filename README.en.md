
# Startalk EJABBERD

Startalk takes ejabberd as basis, but revised it based on the need of business. Startalk team revised and extended some functions that are not available in ejabberd.

## Key functions

- Distributed: removed the codes that depend on mnesia cluster for supporting bigger cluster, which avoid the inconsistent of condition of cluster due to partition network
- Message handling: through connecting ejabberd and kafka, it achieves message routing and subscription publishing, enabling us to add more ways to handle the messages.

## Modules of Startalk

### Startalk includes:

+ [ejabberd](https://github.com/startalkIM/ejabberd)

the core component of IM. It maintains the connection with client app and message routing

+ [or](https://github.com/startalkIM/openresty_ng)

the load balancing component of IM. It verifies the identity of client app and forwards the request from http to the corresponding backstage services

+ [im_http_service](https://github.com/startalkIM/im_http_service)

Port service of IM HTTP. It takes charge of searching the data and settings, as well as synchronizing the chat history (a java service based on tomcat).

+ [qfproxy](https://github.com/startalkIM/qfproxy)

IM file service. It takes charge of uploading and downloading the file (a java service based on tomcat).

+ [push_service](https://github.com/startalkIM/push_service)

Push service in IM. It pushes off-line messages (a java service based on tomcat).

+ [serach](https://github.com/startalkIM/search)

It provides the service to search people and groups remotely

+ redis

Caching service in IM

+ postgresql

database service in IM

### Startalk 各个模块之间的关系

![architecture](image/arch.png)

## Announcements

Please read and follow the guide carefully:

* DO NOT use root account for following operations. Many software check the current username, so please create a new user;
* You need to load the configuration before starting redis;
* For startalk, the domain in configuration is important, so please check the configuration carefully;
* Please make sure that the following ports are NOT occupied before start:
```
openresty：8080
im_http_service：8005 8009 8081
qfproxy：8006 8010 8082
push_service：8007 8011 8083
search：8888

im： 5202 10050 5280

db: 5432 

redis: 6379
```

## Installation

Prerequisite (If the hostname and username are not same as the name below, you need to change them to your name)

+ Server: centos7.x
+ Hostname: startalk.com
+ Add hosts: 127.0.0.1 startalk.com(sudo vim /etc/hosts)
+ All of the items need to be install in “/startalk”
+ Installation user and user group: startalk:startalk  make sure that startalk users have the permission of sudo
+ Under home directory, there is a folder called “download”, all of the files will be downloaded to this folder
+ The username and password: ejabberd:123456, IP address of the server: 127.0.0.1
+ The password of redis: 123456, IP address of the server: 127.0.0.1
+ SQL of the data base initialization is under the doc directory
+ Make sure that you are able to access the port 5202 and 8080 (turn off the firewall: sudo systemctl stop firewalld.service)
+ The domain name of IM service: qtalk (before installing, please determine this name carefully; after determining the domain name, it will cost a lot of time and money to revise it)
+ TLS certification: The defaulted one is a test certification, which can be use online. Please change the file: /startalk/ejabberd/etc/ejabberd/server.pem
+ If there is a warning of overlapping files, please enter “yes” and press the “enter”
+ In the installation files, the commands start with “#” are ran by root; those start with “$” means ordinary users 

### Dependencies

```
# yum -y install epel-release
# yum -y update
# yum -y groupinstall Base "Development Tools" "Perl Support"
# yum -y install openssl openssl-devel unixODBC unixODBC-devel pkgconfig libSM libSM-devel libxslt ncurses-devel libyaml libyaml-devel expat expat-devel libxml2-devel libxml2 java-1.8.0-openjdk  java-1.8.0-openjdk-devel  pam-devel pcre-devel gd-devel bzip2-devel zlib-devel libicu-devel libwebp-devel gmp-devel curl-devel postgresql-devel libtidy libtidy-devel recode aspell libmcrypt  libmemcached gd readline-devel libxslt-devel vim docbook-dtds docbook-style-xslt fop 
```

### Add host

```
# vim /etc/hosts

# Add the line below:
127.0.0.1 startalk.com
```

### Create Installation Users

```
# groupadd startalk
# useradd -g startalk startalk
# passwd startalk

# groupadd postgres
# useradd -g postgres postgres
# passwd postgres  
```

### Create Installation Directory

```
# mkdir /startalk
# chown startalk:startalk /startalk
```

### Add sudo permission for Startalk users

```
# visudo 

# add
startalk     ALL= (ALL)    ALL
postgres     ALL= (ALL)    ALL
# after
root    ALL= (ALL)    ALL

# Exit after saved
```

###  Download source code

```
# su - startalk
$ mkdir /startalk/download
$ cd /startalk/download
$ git clone https://github.com/startalkIM/ejabberd.git
$ git clone https://github.com/startalkIM/openresty_ng.git
$ git clone https://github.com/startalkIM/search.git

$ cp ejabberd/doc/qtalk.sql /startalk/
$ cp ejabberd/doc/init.sql /startalk/
$ chmod 777 /startalk/qtalk.sql
```

### Test ports

```
# sudo netstat -antlp | egrep "8080|8005|8009|8081|8006|8010|8082|8007|8011|8083|8888|5202|10050|5280|6379"
# If there is no output, it shows that the ports that Startalk is using are not occupied by other programs, or you need to close the programs that are occupying the ports.
```

### Install Redis

```
$ sudo yum install -y redis
$ sudo vim /etc/redis.conf
# Revise the corresponding configuration to the content below: 
daemonize yes
requirepass 123456
maxmemory 134217728
 
# Start redis:
$ sudo redis-server /etc/redis.conf
 
# Make sure that redis started successfully:
$ sudo netstat -antlp | grep 6379
tcp        0      0 127.0.0.1:6379          0.0.0.0:*               LISTEN      8813/redis-server 1
```
 
### Install database

```
１ Download source code:
$ wget https://ftp.postgresql.org/pub/source/v11.1/postgresql-11.1.tar.gz
 
2 Compiled installation:
#decompress
$ tar -zxvf postgresql-11.1.tar.gz
$ cd postgresql-11.1/
$ sudo ./configure --prefix=/opt/pg11 --with-perl --with-libxml --with-libxslt
 
$ sudo make world
#The complied result must be the same as the sentence below, or you need to detect and correct errors
#PostgreSQL, contrib, and documentation successfully made. Ready to install.
 
$ sudo make install-world
#The result of installation must be the same as the sentence below, or the installation failed
#PostgreSQL installation complete.
 
3. Add the user of postgres OS
$ sudo mkdir -p /export/pg110_data
  
$ sudo chown postgres:postgres /export/pg110_data
 
4. Create database instance
$ su - postgres
 
$ /opt/pg11/bin/initdb -D /export/pg110_data
 
5. Change the database configuration file


Change the value of “logging_collector” in “/export/pg110_data/postgresql.conf” to “on”)

6. Start DB instance
 
$ /opt/pg11/bin/pg_ctl -D /export/pg110_data start
Make sure it started successfully
$ sudo netstat -antlp | grep 5432
tcp        0      0 127.0.0.1:5432          0.0.0.0:*               LISTEN      4751/postmaster     
 
7. Initialize the structure of DB
 
$ /opt/pg11/bin/psql -U postgres -d postgres -f /startalk/qtalk.sql
$ /opt/pg11/bin/psql -U postgres -d ejabberd -f /startalk/init.sql
 
8. Initialize the password of DB user: ejabberd
 
$ /opt/pg11/bin/psql -U postgres -d postgres -c "ALTER USER ejabberd WITH PASSWORD '123456';"
 
9. Connect psql to database

$ psql -U postgres -d ejabberd -h 127.0.0.1
psql (9.2.24, server 11.1)
WARNING: psql version 9.2, server version 11.0.
         Some psql features might not work.
Type "help" for help.

ejabberd=# select * from host_users;
```

###  Install openresty

```
$ su - startalk
$ cd /startalk/download
$ wget https://openresty.org/download/openresty-1.13.6.2.tar.gz
$ tar -zxvf openresty-1.13.6.2.tar.gz
$ cd openresty-1.13.6.2
$ ./configure --prefix=/startalk/openresty --with-http_auth_request_module
$ make
$ make install

# install or
$ cd /startalk/download/openresty_ng
$ cp -rf conf /startalk/openresty/nginx
$ cp -rf lua_app /startalk/openresty/nginx

# operate or
start：/startalk/openresty/nginx/sbin/nginx

# Make sure it has started successfully
$ sudo netstat -antlp | grep 8080
tcp        0      0 0.0.0.0:8080            0.0.0.0:*               LISTEN      23438/nginx: master
```

### Install erlang

```
$ cd /startalk/download
$ wget http://erlang.org/download/otp_src_19.3.tar.gz
$ tar -zxvf otp_src_19.3.tar.gz
$ cd otp_src_19.3
$ ./configure --prefix=/startalk/erlang1903
$ make
$ make install

# add PATH
$ vim ~/.bash_profile
 
----------------------------------
# User specific environment and startup programs
ERLANGPATH=/startalk/erlang1903
PATH=$PATH:$HOME/bin:$ERLANGPATH/bin
----------------------------------
 
$ . ~/.bash_profile

# Make sure erlang has installed successfully
$ erl
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> 
```

### Install ejabberd

```
$ cd /startalk/download
$ cd ejabberd/
$ ./configure --prefix=/startalk/ejabberd --with-erlang=/startalk/erlang1903 --enable-pgsql --enable-full-xml
$ make
$ make install
$ cp ejabberd.yml.qunar /startalk/ejabberd/etc/ejabberd/ejabberd.yml
$ cp ejabberdctl.cfg.qunar /startalk/ejabberd/etc/ejabberd/ejabberdctl.cfg

# start ejabberd

$ cd /startalk/ejabberd
# start
$ ./sbin/ejabberdctl start

# Make sure ejabberd has installed successfully
$ ps -ef | grep 's ejabberd'
startalk 23515     1  4 09:58 ?        00:00:03 /startalk/erlang1903/lib/erlang/erts-8.3/bin/beam.smp -K true -P 250000 -- -root /startalk/erlang1903/lib/erlang -progname erl -- -home /home/startalk -- -name ejabberd@startalk.com -noshell -noinput -noshell -noinput -mnesia dir "/startalk/ejabberd/var/lib/ejabberd" -ejabberd log_rate_limit 20000 log_rotate_size 504857600 log_rotate_count 41 log_rotate_date "$D0" -s ejabberd -smp auto start
```

### Install java service: Under “/startalk/download/openresty_ng/deps/tomcat/”, there are 3 java services zips; alternatively, you can decompress them by source code and deploy them

```
$ cd /startalk/download/
$ cp -rf openresty_ng/deps/tomcat /startalk/
$ cd /startalk/tomcat

# Change the navigation address and extended keyboard
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/nav.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/androidqtalk.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/androidstartalk.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/iosqtalk.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/iosstartalk.json

# Change the IP to the IP address of the corresponding computer(sed -i "s/ip/xxx.xxx.xxx.xxx/g")

# Change the address of push service

$ vim /startalk/tomcat/push_service/webapps/push_service/WEB-INF/classes/app.properties
# Use Startalk push url
qtalk_push_url=http://ip:8091/qtapi/token/sendPush.qunar
# Use Startalk push key
qtalk_push_key=12342a14-e6c0-463f-90a0-92b8faec4063

# Start java service
$ cd /startalk/tomcat/im_http_service
$ ./bin/startup.sh


$ cd /startalk/tomcat/qfproxy
$ ./bin/startup.sh

$ cd /startalk/tomcat/push_service
$ ./bin/startup.sh

# Make sure that the service has started successfully
$ sudo netstat -antlp | egrep '8081|8082|8083|8009|8010|8011|8005|8006|8007'
tcp6       0      0 127.0.0.1:8007          :::*                    LISTEN      23853/java          
tcp6       0      0 :::8009                 :::*                    LISTEN      23748/java          
tcp6       0      0 :::8010                 :::*                    LISTEN      23785/java          
tcp6       0      0 :::8011                 :::*                    LISTEN      23853/java          
tcp6       0      0 :::8081                 :::*                    LISTEN      23748/java          
tcp6       0      0 :::8082                 :::*                    LISTEN      23785/java          
tcp6       0      0 :::8083                 :::*                    LISTEN      23853/java          
tcp6       0      0 127.0.0.1:8005          :::*                    LISTEN      23748/java          
tcp6       0      0 127.0.0.1:8006          :::*                    LISTEN      23785/java 
```

### Installing back-end searching service

```
Install python 3 or above, the take 3.6 as a standard
$ cd /startalk/download/search
$ sudo yum install https://centos7.iuscommunity.org/ius-release.rpm
$ sudo yum install python36u
Install pip3
$ sudo yum -y install python-pip
Please find the modules in“/startalk/download/search/requirements.txt”. You’d better use virtualenv to deploy the environment for modules, or python3.6 will be installed under system python environment, which will lead to conflict with python2.7 in centos; meanwhile, you need to install the pip of python3, and appoint the modules needed by the installation of pip3
$ sudo pip install -U virtualenv （install virtualenv）
$ sudo pip install --upgrade pip
$ virtualenv --system-site-packages -p python3.6 ./venv （create venv environment under current directory）
Operating environment
$ source venv/bin/activate
Configure “conf/configure.ini”, please see the file for exact parameters. You don’t need to change them if you don’t have a special need
$ sudo vim ./conf/configure.ini
Install the module that needed by this program (if you didn’t install virtualenv, you need sudo yum install python36u-pip, replacing the defaulted pip by sudo pip3.6)
$ pip install -r requirements.txt
Set “PYTHONPATH”
$ export PYTHONPATH=path/to/project/search:$PYTHONPATH
Start at backstage
$ supervisord -c conf/supervisor.conf
$ supervisorctl -c conf/supervisor.conf reload
Make sure that the service has started
$ tail -100f log/access.log 
```

Please run the code below to check common errors: [check.sh](https://github.com/startalkIM/openresty_ng/blob/master/tools/check.sh)

```
# sed -i 's/ip/your ip/g' ./check.sh
# chmod +x check.sh
# ./check.sh
```
If the system reminds you that the 5202 port cannot be visited from the outside network and you need to open the port or turn off the firewall (which is usually caused by the restriction of firewall or the port is not monitored), please use telnet IP 5202 to check.   

Now, the server-side has been installed. Please download the client-side of Startalk

The configured navigation address of client-side:http://ip:8080/newapi/nck/qtalk_nav.qunar 
Please use the username and password “admin & testpassword” to login (replace IP with the IP of your own server)

Information about how to configure navigation: how to configure navigation on client-side 

Please generate a QR Code at [http://www.liantu.com/](http://www.liantu.com) for the navigation address at (or you can generate QR Code at other websites), then add the navigation on mobile app by scanning the QR Code.

## Branches and PR
Now, the branches of our projects are 
* Master (the main branch)
* Release (the release branch that used for combining to the master)
* Develop (the current develop branch
* V1.0 (the branch of version 1.0)
* V1.1 (the branch of version 1.1, the most stable branch)
* V2.0 (the branch of version 2.0, allows Erlang/OTP 21.2)

When submitting the pull request, you can combine it to different branches based on their functions.

## Address of wiki

[wiki](https://github.com/startalkIM/ejabberd/wiki)

##  Change the deployment file 

Please see [setting.md](doc/setting.md)

## Interface file

Please see [interface.md](doc/interface.md)

## Guide for developer

- [developer guide](https://docs.ejabberd.im/developer/guide/)

##  Feedback

- qchat@qunar.com（email）
- 852987381（QQ group）
- QQ qrcode ![qq](image/qq.png)
