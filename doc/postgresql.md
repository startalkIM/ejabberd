备用
```
数据库安装
sudo yum -y install https://download.postgresql.org/pub/repos/yum/11/redhat/rhel-7-x86_64/pgdg-centos11-11-2.noarch.rpm
sudo yum -y install postgresql11 postgresql11-server postgresql11-libs
 
sudo /usr/pgsql-11/bin/postgresql-11-setup initdb
sudo systemctl enable postgresql-11
sudo systemctl start postgresql-11
 
修改posgres的密码
sudo passwd postgres
su - postgres
psql -f /home/monkboy/download/qtalk.sql
psql
# ALTER USER ejabberd WITH PASSWORD '123456';

插入测试账号：
# insert into host_info (host, description, host_admin) values ('qtalk.test.org', 'qtalk.test.org', 'test');
# insert into host_users (host_id, user_id, user_name, department, dep1, pinyin, frozen_flag, version, user_type, hire_flag, gender, password, initialpwd, ps_deptid) values ('1', 'test', '测试账号', '/机器人', '机器人', 'test', '0', '1', 'U', '1', '1', '1234567890', '1', 'qtalk');
# insert into vcard_version (username, version, profile_version, gender, host, url) values ('test', '1', '1', '1', 'qtalk.test.org', 'https://qt.qunar.com/file/v2/download/avatar/1af5bc967f8535a4af19eca10dc95cf1.png');

修改配置文件
[monkboy@monk download]$ sudo vim /var/lib/pgsql/11/data/pg_hba.conf
 
# "local" is for Unix domain socket connections only
local   all             all                                     peer
# IPv4 local connections:
host    all             all             127.0.0.1/32            md5
# IPv6 local connections:
host    all             all             ::1/128                 md5
```
