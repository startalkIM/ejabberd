#!/bin/bash
#v1.00
# set -e 
# 检查db实例是否启动
#-------------------
pg_isready
dbstance_result=$?
if [ "${dbstance_result}" ]; then
   echo "监测到存在的数据库实例..."
   dbstance_result=true
else
   sudo systemctl restart postgresql.service
   sudo systemctl status postgresql.service
   dbstance_status=$?
   if [ "${dbstance_status}" = 0 ]; then
       echo "数据库启动成功"
       dbstance_result=true
    elif [ "${dbstance_status}" = 3 ]; then
       echo "数据库启动失败, 请检查报错"
       dbstance_result=false
   fi
fi 
# ------------------
if ! $dbstance_result; then
  echo "数据库实例创建失败, 请手动启动"
  exit 0 
fi
echo -n "请输入数据库ip地址 (缺省为127.0.0.1):"
read default_host
default_host=${default_host:-localhost}

echo -n "请输入数据库端口 (缺省为5432):"
read default_port
default_port=${default_port:-5432}

echo -n "请输入管理员账户名称 (缺省为postgres):"
read admin_account
admin_account=${admin_account:-postgres}

echo -n "请输入管理员密码:"
read admin_password

db_status=0
# 检测是否已经有ejabberd数据库
startalk_db_result=`psql postgres://${admin_account}:${admin_password}@${default_host}:${default_port} -lqt | cut -d \| -f 1`
if fgrep startalk <<< $startalk_db_result ; then
  if egrep ejabberd <<< $startalk_db_result ; then
    echo "数据库已存在, 将进行更新检查"
    db_status=2
  else
    echo "数据库不完整, 将进行更新"
    db_status=1
  fi
else
  if egrep ejabberd <<< $startalk_db_result ; then
      echo "数据库不完整, 将进行更新"
      db_status=1
  else
       echo "数据库未建立, 将进行创建" 
      db_status=0
  fi
fi
# ------------------

if [ "${db_status}" = "0" ]; then
  echo "即将安装以下扩展:
  plpgsql
  adminpack
  btree_gist
  pg_buffercache
  pg_trgm
  pgstattuple"
    psql postgres://${admin_account}:${admin_password}@${default_host}:${default_port} -f superUser.sql 
    psql postgres://ejabberd:123456@${default_host}:${default_port}/ejabberd -f ejabberd.sql
elif [ "${db_status}" = "1" ]; then
    psql postgres://${admin_account}:${admin_password}@${default_host}:${default_port} -f superUser.sql 
    psql postgres://ejabberd:123456@${default_host}:${default_port}/ejabberd -f ejabberd.sql
else 
    # TODO: 对比新旧结构并且进行更新
    printf "请从https://github.com/startalkIM/ejabberd/tree/master/doc获取最新sqls"
    exit 0
fi
exit 0
