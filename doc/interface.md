# 接口说明

- [0.概述](#0.概述)
- [1.发送消息](#1.发送消息)
- [2.发送事件通知](#2.发送事件通知)
- [3.建群接口](#3.建群接口)
- [4.添加群用户](#4.添加群用户)
- [5.删除群成员](#5.删除群成员)
- [6.销毁群](#6.销毁群)
- [7.获取个人信息](#7.获取个人信息)
- [8.发送消息](#8.发送消息)
- [9.获取群成员](#9.获取群成员)
- [10.验证uk](#10.验证uk)

## 0.概述

HTTP服务是通过cowboy组件来实现的，启动模块是[http_service.erl](../src/http_service.erl)，服务端口是10050。

## 1.发送消息

```
接口：/qtalk/send_thirdmessage
请求方式：POST
参数：
{
   "from":"test1@qtalk.test.org",
   "to":"test2@qtalk.test.org",
   "message":"<message xml:lang='en' from='test1@qtalk.test.org' to='test2@qtalk.test.org' type='chat'><body msgType='1' maType='3' id='7aea7e1c7b544992ace3bdb4b3ab233271sd3ffsadfsad22'>dddddddddddddddddddddddddddddddddddddddd</body></message>"
}

返回值：
{
    "ret": true,
    "errcode": 0,
    "errmsg": "success",
    "data":""
}
```

单人消息格式：

```
<message from='test1@qtalk.test.org'        //发消息的人@xxxx
          to='test2@qtalk.test.org'               //收消息的人@xxxx
          type='chat'>                        //固定写死
              <body id='watcher-18DE3DBE1AF941B3B786C31AA894617F'      //消息id，生成方式是uuid，加上自己系统的唯一标示前缀
                    maType='20'                                //消息来源
                    msgType='1'>                               //消息类型
                  恩                                           //发送内容
              </body>
</message>
```

群消息格式

```
<message type="groupchat"                          // 固定写死
         to="f273a142f23047369c19aab73bc80b6c@conference.qtalk.test.org"    //群@xxxx
         from="test2@qtalk.test.org"                                    //发消息的人@xxxx
         client_ver='0'>                        //固定写死
             <body id="watcher-939f7a51be664bb78dfff32457cd2171"    //消息id，生成方式是uuid，加上自己系统的唯一标示前缀
                    maType='20'                                //消息来源
                    msgType="1" >     //消息类型
                       11111111111
             </body>
</message>
```

msgType说明请参考[msgType说明](msgtype.md)
maType说明参考[matype说明](matype.md)

## 2.发送事件通知

```
接口：/qtalk/send_notify
请求方式：POST
参数：
{
   "from":"test1@qtalk.test.org",
   "to":"test2@qtalk.test.org",
   "category":"参见事件说明",
   "data":"参见事件说明"
}

返回值：
{
    "ret": true,
    "errcode": 0,
    "errmsg": "success",
    "data":""
}
```

时间说明请参考[事件说明](eventtype.md)

## 3.建群接口

```
接口：/qtalk/create_muc
请求方式：POST
参数：
{
    "muc_id": "test-8470e92d33304a27a847ddcbc3b9ae2b",  # 聊天室id(使用system+UUID，保证唯一) （必传）
    "muc_owner": "admin", # 群管理员
    "muc_domain":"conference.qtalk.test.org",   # 群domain
    "system":"test"    # 调用方标示
}        

返回值：
{
    "ret": true,
    "errcode": 0,
    "errmsg": "success",
    "data":""
}
```

## 4.添加群用户

```
接口：/qtalk/add_muc_user
请求方式：POST
参数：
{
    "muc_id": "test-8470e92d33304a27a847ddcbc3b9ae2b",
    "muc_owner": "admin",
    "muc_domain":"conference.qtalk.test.org",
    "muc_member": ["test1","test2"],
    "system":"test"
}

返回值：
{
    "ret": true,
    "errcode": 0,
    "errmsg": "success",
    "data":""
}
```

## 5.删除群成员

```
接口：/qtalk/del_muc_user
请求方式：POST
参数：
{
    "muc_id": "test-8470e92d33304a27a847ddcbc3b9ae2b",
    "muc_owner": "admin",
    "muc_domain":"conference.qtalk.test.org",
    "muc_member": ["test1"],
    "system":"test"
}

返回值：
{
    "ret": true,
    "errcode": 0,
    "errmsg": "success",
    "data":""
}
```

## 6.销毁群

```
接口：/qtalk/destroy_muc
请求方式：POST
参数：
{
    "muc_id": "test-8470e92d33304a27a847ddcbc3b9ae2b",
    "muc_owner": "admin",
    "muc_domain":"conference.qtalk.test.org",
    "system":"test"}

返回值：
{
    "ret": true,
    "errcode": 0,
    "errmsg": "success",
    "data":""
}
```

## 7.获取个人信息

```
接口：/qtalk/get_user_nick
请求方式：POST
参数：
{
    "ID":"test",
    "Domain":"qtalk.test.org"
}

返回值：
{
    "ret":true,
    "errcode":0,
    "errmsg":"",
    "data":{
        "Nick":"测试",
        "Vcard":"file/v2/download/2048b3321d1798e94d6329d464b22baf.jpg?name=2048b3321d1798e94d6329d464b22baf.jpg&file=2048b3321d1798e94d6329d464b22baf.jpg&fileName=2048b3321d1798e94d6329d464b22baf.jpg"
    }
}
```

## 8.发送消息

```
接口：/qtalk/send_message
请求方式：POST
参数：
{
    "From":"test1", 
    "To":[{"User":"test"}],
    "Body":"xxxxxxx",
    "Type":"chat",
    "Msg_Type":"1",
    "Host":"qtalk.test.org",
    "Domain":"qtalk.test.org",
    "Extend_Info":""
}

返回值：
{
    "ret":true,
    "errcode":0,
    "errmsg":"success"
}
```

## 9.获取群成员

```
接口：/qtalk/muc_users
请求方式：POST
参数：
{
    "muc_id":"5f0b6bfa2e3a4d78aefe047ab06dc589", 
    "muc_domain":"conference.qtalk.test.org"
}

返回值：
{
    "ret":true,
    "errcode":0,
    "errmsg":"",
    "data":[
        {"U":"test","H":"qtalk.test.org","N":"测试1"},
        {"U":"test1","H":"qtalk.test.org","N":"测试2"}
    ]
}
```

## 10.验证uk

```
接口：/qtalk/auth_uk
请求方式：GET
参数：
?u=test&k=290711548828672407278

返回值：
{
    "ret":true,
    "errcode":0,
    "errmsg":"success"
}
```

## 11.清理离职人员所在的群

```
接口：qtalk/clear_staff
请求方式：POST
参数：
{
    "users":["test", "test1"],
    "host":"qtalk.test.org"
}

返回值：
{
    "ret":true,
    "errcode":0,
    "errmsg":"",
    "data":""
}
```
