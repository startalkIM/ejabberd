[返回首页](https://github.com/startalkIM/Startalk)

## 安装
前提条件(如果主机名，用户名和这里的不一致，则需要将安装步骤中的换成自己的名字)：

+ 服务器要求：ubuntu 1904
+ 主机名是：startalk.com
+ hosts添加： 127.0.0.1 startalk.com(sudo vim /etc/hosts)
+ 所有项目都安装到/startalk下面
+ 安装用户和用户组是：startalk:startalk，要保证startalk用户有sudo权限
+ 家目录下有download文件夹，所有文件会下载到该文件夹下
+ 数据库用户名密码是ejabberd:123456，服务地址是：127.0.0.1
+ redis密码是：123456，服务地址是：127.0.0.1
+ 数据库初始化sql在doc目录下
+ 保证可访问主机的：5202、8080端口（关掉防火墙：sudo ufw disable）
+ IM服务的域名是:qtalk(大家安装线上之前，最好确定好这个值，一旦定了，之后修改的成本就很高，可以参考[domain修改](https://github.com/qunarcorp/ejabberd-open/wiki/host%E4%BF%AE%E6%94%B9)来修改)
+ tls证书：默认安装用的是一个测试证书，线上使用，请更换/startalk/ejabberd/etc/ejabberd/server.pem文件，生成方法见[securing-ejabberd-with-tls-encryption](https://blog.process-one.net/securing-ejabberd-with-tls-encryption/)
+ 出现文件覆盖提示时，输入yes敲回车即可
+ 安装文档中#开头输入的命令表示root执行的，$开头的命令表示普通用户

### 依赖包
```
$ sudo apt-get install autoconf libncurses-dev build-essential m4 unixodbc-dev libssl-dev  libwxgtk3.0-dev libglu-dev  fop xsltproc g++  default-jdk  libxml2-utils  libyaml-dev libexpat1-dev
```

### 添加host

```
$ sudo vim /etc/hosts
添加下面一行
127.0.0.1 startalk.com
```

### 新建安装目录

```
$ sudo mkdir /startalk
$ chown startalk:startalk /startalk
```


### 下载源码

```
$ mkdir /startalk/download
$ cd /startalk/download
$ git clone https://github.com/qunarcorp/ejabberd-open.git
$ git clone https://github.com/qunarcorp/or_open.git
$ git clone https://github.com/qunarcorp/qtalk_search.git

$ cp ejabberd-open/doc/qtalk.sql /startalk/
$ cp ejabberd-open/doc/init.sql /startalk/
$ chmod 777 /startalk/qtalk.sql
```

### 检测端口使用：

```
# sudo netstat -antlp | egrep "8080|8005|8009|8081|8006|8010|8082|8007|8011|8083|8888|10056|5202|10050|5280|6379"
若没有任何输出，怎表明没有程序占用startalk使用的端口，否则需要关闭已经在使用端口的程序
```

### redis安装

```
$ sudo apt-get install redis-server
$ sudo vim /etc/redis/redis.conf
将对应的配置修改为下面内容 
daemonize yes
requirepass 123456
maxmemory 134217728
 
启动redis
$ sudo systemctl enable redis-server.service
$ sudo systemctl start redis-server.service
 
确认启动成功：
$ sudo netstat -antlp | grep 6379
tcp        0      0 127.0.0.1:6379          0.0.0.0:*               LISTEN      8813/redis-server 1
```

### 数据库安装

```
1、安装pg
$ sudo apt install postgresql-11
    如果提示：Unable to locate package postgresql-11，请执行下面语句，然后再安装:sudo apt install postgresql-11
    $ sudo apt-get install wget ca-certificates
    $ wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
    $ sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list'
$ sudo systemctl status postgresql
确认启动成功
● postgresql.service - PostgreSQL RDBMS
   Loaded: loaded (/lib/systemd/system/postgresql.service; enabled; vendor preset: enabled)
   Active: active (exited) since Tue 2019-09-24 15:57:59 CST; 13s ago
 Main PID: 28991 (code=exited, status=0/SUCCESS)
    Tasks: 0 (limit: 4915)
   Memory: 0B
   CGroup: /system.slice/postgresql.service

9月 24 15:57:59 0x00 systemd[1]: Starting PostgreSQL RDBMS...
9月 24 15:57:59 0x00 systemd[1]: Started PostgreSQL RDBMS.
 
2. 初始化DB结构
 
$ sudo -i -u postgres
$ psql
$ psql -d postgres -f /startalk/qtalk.sql
$ psql -d ejabberd -f /startalk/init.sql
 
3. 初始化DB user: ejabberd的密码
 
$ psql -d postgres -c "ALTER USER ejabberd WITH PASSWORD '123456';"
 
4 psql连接数据库

$ psql
psql (9.2.24, server 11.1)
WARNING: psql version 9.2, server version 11.0.
         Some psql features might not work.
Type "help" for help.

ejabberd=# select * from host_users;
```
5、退出postgres用户

$ exit

### openresty安装

```
$ su - startalk
$ cd /startalk/download
$ wget https://openresty.org/download/openresty-1.13.6.2.tar.gz
$ tar -zxvf openresty-1.13.6.2.tar.gz
$ cd openresty-1.13.6.2
$ ./configure --prefix=/startalk/openresty --with-http_auth_request_module
$ make
$ make install

or安装
$ cd /startalk/download/or_open
$ cp -rf conf /startalk/openresty/nginx
$ cp -rf lua_app /startalk/openresty/nginx

or操作
启动：/startalk/openresty/nginx/sbin/nginx

确认启动成功
$ sudo netstat -antlp | grep 8080
tcp        0      0 0.0.0.0:8080            0.0.0.0:*               LISTEN      23438/nginx: master
```

### 安装openssl

```
由于erlang19.3不支持openssl1.1*，所以如果本地openssl大于1.1，则需要单独安装一个低版本openssl

检测openssl版本：openssl version

$ cd /startalk/download/
$ wget https://www.openssl.org/source/openssl-1.0.2l.tar.gz
$ tar -zxvf openssl-1.0.2l.tar.gz
$ cd openssl-1.0.2l/
$ ./config --prefix=/startalk/openssl1.0.2l
config之后，会生成Makefile，打开Makefile找到gcc，在CFLAG参数列表里加上-fPIC
$ vim Makefile

CC= gcc
CFLAG= -fPIC -DOPENSSL_THREADS -D_REENTRANT -DDSO_DLFCN -DHAVE_DLFCN_H -Wa,--noexecstack -m64 -DL_ENDIAN -DTERMIO -O3 -Wall -DOPENSSL_IA32_SSE2 -DOPENSSL_BN_ASM_MONT -DOPENSSL_BN_ASM_MONT5 -DOPENSSL_BN_ASM_GF2m -DSHA1_ASM -DSHA256_ASM -DSHA512_ASM -DMD5_ASM -DAES_ASM -DVPAES_ASM -DBSAES_ASM -DWHIRLPOOL_ASM -DGHASH_ASM

$ make install
```

### 安装erlang

```
$ cd /startalk/download
$ wget http://erlang.org/download/otp_src_19.3.tar.gz
$ tar -zxvf otp_src_19.3.tar.gz
$ cd otp_src_19.3
$ ./configure --prefix=/startalk/erlang1903 --with-ssl=/startalk/openssl1.0.2l/
$ make
$ make install

添加PATH
$ vim ~/.bashrc
 
----------------------------------
# User specific environment and startup programs
ERLANGPATH=/startalk/erlang1903
PATH=$PATH:$HOME/bin:$ERLANGPATH/bin
----------------------------------
 
$ . ~/.bashrc

确认erlang安装成功
$ erl
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> 
```

### 安装ejabberd

```
$ cd /startalk/download
$ cd ejabberd-open/
$ ./configure --prefix=/startalk/ejabberd --with-erlang=/startalk/erlang1903 --enable-pgsql --enable-full-xml
$ make
$ make install
$ cp ejabberd.yml.qunar /startalk/ejabberd/etc/ejabberd/ejabberd.yml
$ cp ejabberdctl.cfg.qunar /startalk/ejabberd/etc/ejabberd/ejabberdctl.cfg

启动ejabberd

$ cd /startalk/ejabberd
启动
$ ./sbin/ejabberdctl start

确认ejabberd安装成功
$ ps -ef | grep 's ejabberd'
startalk 23515     1  4 09:58 ?        00:00:03 /startalk/erlang1903/lib/erlang/erts-8.3/bin/beam.smp -K true -P 250000 -- -root /startalk/erlang1903/lib/erlang -progname erl -- -home /home/startalk -- -name ejabberd@startalk.com -noshell -noinput -noshell -noinput -mnesia dir "/startalk/ejabberd/var/lib/ejabberd" -ejabberd log_rate_limit 20000 log_rotate_size 504857600 log_rotate_count 41 log_rotate_date "$D0" -s ejabberd -smp auto start
```

### 安装java服务(/startalk/download/or_open/deps/tomcat/下的是打好包的三个java服务，自己也可以使用源码打包，然后自己部署)

```
$ cd /startalk/download/
$ cp -rf or_open/deps/tomcat /startalk/
$ cd /startalk/tomcat

修改导航地址和扩展键盘：
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/nav.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/androidqtalk.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/androidstartalk.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/iosqtalk.json
$  vim /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/iosstartalk.json

将ip替换成对应机器的ip地址(sed -i "s/ip/xxx.xxx.xxx.xxx/g")

修改推送服务的地址

$ vim /startalk/tomcat/push_service/webapps/push_service/WEB-INF/classes/app.properties
#使用星语push url
qtalk_push_url=http://ip:8091/qtapi/token/sendPush.qunar
#使用星语push key
qtalk_push_key=12342a14-e6c0-463f-90a0-92b8faec4063

启动java服务
$ cd /startalk/tomcat/im_http_service
$ ./bin/startup.sh


$ cd /startalk/tomcat/qfproxy
$ ./bin/startup.sh

$ cd /startalk/tomcat/push_service
$ ./bin/startup.sh

确认服务启动成功
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

### 安装后端搜索服务
```
待完成
```

可以执行以下脚本来检查一些常见的错误: 下载该文件[check.sh](https://github.com/qunarcorp/or_open/blob/master/tools/check.sh)

```
$ sed -i 's/ip/自己的ip/g' ./check.sh
$ chmod +x check.sh
$ ./check.sh
```

如果发现有提示："ip的5202端口未开启外网访问，请开启该端口访问或者关掉防火墙"，请在服务器上使用telnet ip 5202检查是否可以连上，一般是因为防火墙限制了或者端口就没监听。

到此，服务端已经安装完成。
请下载[startalk客户端](https://im.qunar.com/new/#/download)

客户端配置导航地址：[http://ip:8080/newapi/nck/qtalk_nav.qunar](http://ip:8080/newapi/nck/qtalk_nav.qunar)，使用账号：admin，密码：testpassword登陆(将ip替换成自己服务器的ip)

客户端配置导航的说明[配置导航](https://im.qunar.com/#/platform/access_guide/config_navs?id=config_navs)

可以在二维码生成网站[http://www.liantu.com/](http://www.liantu.com/)生成导航地址的二维码，然后通过扫码在手机客户端添加导航

[返回首页](https://github.com/startalkIM/Startalk)
