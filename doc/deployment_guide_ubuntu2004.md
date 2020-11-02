## ubuntu 20.04
## 所需占用端口:
## openresty服务：8080
## im_http_service服务：8005 8009 8081
## qfproxy服务：8006 8010 8082
## push_service服务：8007 8011 8083
## search服务：8884
## im服务： 5202 10050 5280
## db: 5432 
## redis: 6379
## 所需开放对外的端口: 5202(ejabberd)、 8080(nginx), 可使用systemctl disable firewalld关闭防火墙放开端口
## 所有的自动操作都将原文件进行了备份.bak, 在测试情况下可以将所有命令使用&&并联实现一键部署(中途有三处需要人工)


## 安装openresty以及postgresql的apt源
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
wget -qO - https://openresty.org/package/pubkey.gpg | sudo apt-key add -
sudo add-apt-repository -y "deb http://openresty.org/package/ubuntu $(lsb_release -sc) main"
RELEASE=$(lsb_release -cs)
echo "deb http://apt.postgresql.org/pub/repos/apt/ ${RELEASE}"-pgdg main | sudo tee  /etc/apt/sources.list.d/pgdg.list
sudo apt update
sudo sed -i -e '$a127.0.0.1 startalk.com' /etc/hosts

## 创建startalk用户, 家目录为 /startalk, 默认shell为/bin/bash, 并且添加sudo权限
sudo mkdir /startalk 
sudo groupadd startalk
sudo useradd -d /startalk -g startalk -s /bin/bash startalk
sudo passwd startalk
sudo chown -R startalk:startalk /startalk
# 将startalk与postgres加入sudo用户
sudo vim /etc/sudoers
		startalk     ALL= (ALL)    ALL
		postgres     ALL= (ALL)    ALL
		

sudo su - startalk
mkdir /startalk/download
cd /startalk/download

## 进行所有需要下载的工作, 这部将占据较长时间, 这部如果没有完成或报错请不要继续, 先debug. 命令请复制整个章节
git clone https://github.com/startalkIM/ejabberd.git &&
git clone https://github.com/startalkIM/openresty_ng.git &&
git clone https://github.com/startalkIM/search.git &&
wget https://www.openssl.org/source/openssl-1.0.2l.tar.gz &&
wget http://erlang.org/download/otp_src_19.3.tar.gz &&
wget https://www.python.org/ftp/python/3.7.9/Python-3.7.9.tgz &&
&& sudo apt install -y autoconf libncurses-dev build-essential m4 unixodbc-dev libssl-dev libglu-dev  fop xsltproc g++  default-jdk  libxml2-utils  libyaml-dev libexpat1-dev zlib1g zlib1g-dev vim  postgresql-11 git redis-server libpcre3-dev libssl-dev perl make build-essential curl openresty mlocate libffi-dev


## 为erlang19安装低版本openssl1.0.2, 安装于/startalk/openssl1.0.2l
cd /startalk/download
tar -zxvf openssl-1.0.2l.tar.gz
cd openssl-1.0.2l/
./config --prefix=/startalk/openssl1.0.2l
cp Makefile Makefile.bak
sed -i '0,/CFLAG=/s//CFLAG= -fPIC /' /startalk/download/openssl-1.0.2l/Makefile
make install

## redis安装, 密码为123456, 未开通外部连接仅限本机
sudo cp /etc/redis/redis.conf /etc/redis/redis.conf.bak
sudo sed -i 's/#\s*daemonize\s*off/daemonize yes/g' /etc/redis/redis.conf
sudo sed -i 's/#\s*requirepass\s*foobared/requirepass 123456/g' /etc/redis/redis.conf
sudo sed -i 's/#\s*maxmemory\s*<bytes>/maxmemory 134217728/g' /etc/redis/redis.conf
sudo systemctl enable redis-server.service
sudo systemctl restart redis-server.service

## openresty, 这样处理可以让更新openresty变得更方便
sudo cp -r /usr/local/openresty/nginx/conf /usr/local/openresty/nginx/conf.bak
sudo cp -rn /usr/local/openresty/nginx/conf /startalk/download/openresty_ng/
sudo cp -rf /startalk/download/openresty_ng/conf /usr/local/openresty/nginx/
sudo cp -rf /startalk/download/openresty_ng/lua_app /usr/local/openresty/nginx/

sudo systemctl enable openresty
sudo systemctl restart openresty

## 数据库采用PostgreSQL-11, 使用包管理工具进行安装, dbinit.sh来简化初始化, 由于将超级用户与普通用户进行了分别, 需要管理员是postgres以及它的账号密码
#TODO: !! 修改pg_hba.conf 将local的peer改为md5, 使得可以密码登陆
sudo su - postgres
psql -c "alter user postgres password '123456'";
cd /startalk/download/ejabberd/doc/
# 使用脚本进行创建, 可能会遇到因pg版本不同导致的命令错误, 需要进行人工交互
cp  /etc/postgresql/11/main/postgresql.conf  /etc/postgresql/11/main/postgresql.conf.bak
sed -i 's/#\s*logging_collector\s*=\s*off/logging_collector = on/g'  /etc/postgresql/11/main/postgresql.conf
# 修改pg_hba.conf使账户可以通过密码登陆, 文件通常位于/etc/postgresql/11/main/pg_hba.conf
vim /etc/postgresql/11/main/pg_hba.conf
	# "local" is for Unix domain socket connections only
	local   all             all                                     md5
./dbinit.sh
exit
sudo systemctl restart postgresql



## ejabberd, IM的核心, 高并发XMPP服务器, 安装erlang19.3以及高版本ubuntu所需的低版本openssl1.0.2
cd /startalk/download
tar zxvf otp_src_19.3.tar.gz
cd /startalk/download/otp_src_19.3
./configure --prefix=/startalk/erlang1903 --with-ssl=/startalk/openssl1.0.2l/
make -j2
make install 
echo 'ERLANGPATH=/startalk/erlang1903' >> ~/.bash_profile
echo 'PATH=$PATH:$HOME/bin:$ERLANGPATH/bin' >> ~/.bash_profile
source ~/.bash_profile
cd /startalk/download/ejabberd
./configure --prefix=/startalk/ejabberd --with-erlang=/startalk/erlang1903 --enable-pgsql --enable-full-xml
make -j2
make install
cd /startalk/ejabberd
./sbin/ejabberdctl start


## java服务们, 需要三个tomcat
## 其中qfproxy为文件服务, push_service为推送服务, im_http_service为im相关几乎所有http接口的java平台
cp -r /startalk/download/openresty_ng/deps/tomcat /startalk
cp -r /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes.bak
# !!!!! 需要互动, hostname -I不确定是否正确
ip a
echo -n "监测到ip `hostname -I`, 确认请回车, 错误请手动输入: " && read MY_IP && MY_IP=${MY_IP:-`hostname -I|sed 's/ //g'`}
sed -i "s/ip/$MY_IP/g" /startalk/tomcat/im_http_service/webapps/im_http_service/WEB-INF/classes/*.json
sed -i "s/qtalk_push_url=.*/qtalk_push_url=http:\/\/$MY_IP:8091\/qtapi\/token\/sendPush.qunar/g" /startalk/tomcat/push_service/webapps/push_service/WEB-INF/classes/app.properties
sed -i "s/qtalk_push_key=.*/qtalk_push_key=12342a14-e6c0-463f-90a0-92b8faec4063/g" /startalk/tomcat/push_service/webapps/push_service/WEB-INF/classes/app.properties
/startalk/tomcat/im_http_service/bin/startup.sh && sleep 1
/startalk/tomcat/qf_proxy/bin/startup.sh && sleep 1
/startalk/tomcat/push_service/bin/startup.sh && sleep 1


## 搜索及其他python后台服务, 将在/startalk/search/venv下创建虚拟环境, 虚拟环境只对python相关影响, 键入deactivate退出
## 项目通过supervisord托管, 配置文件位于 /startalk/search/conf/supervisor.conf, 可使用supervisorctl进行重启, 停止等操作
cd /startalk/download
tar zxvf /startalk/download/Python-3.7.9.tgz 
cd /startalk/download/Python-3.7.9
./configure
make -j2
sudo make install
pip3 install --upgrade pip
sudo pip3 install -U virtualenv
virtualenv --system-site-packages -p python3.7 /startalk/search/venv
source /startalk/search/venv/bin/activate
cp -rn /startalk/download/search/ /startalk/
pip3 install -r /startalk/search/requirements.txt
supervisord -c /startalk/search/conf/supervisor.conf
deactivate



## 至此结束, 请查看http://ip:8080/newapi/nck/qtalk_nav.qunar导航是否正确显示, 并下载客户端进行登录

