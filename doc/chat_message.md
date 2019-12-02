# 单人消息

单人消息是指：两个人之间聊天的消息（除了单人消息之外，还有群消息）

## 消息格式

```
<message from='test1@qtalk'        //发消息的人@xxxx
          to='test2@qtalk'               //收消息的人@xxxx
          realfrom='test1@qtalk'        // 真正的from，type=consult才会有
          realto='test2@qtalk'         // 真正的to,type=consult才会有
          msec_times='1530785714622'    // 消息的时间
          type='chat'>                            // chat或者consult
              <body id='watcher-18DE3DBE1AF941B3B786C31AA894617F'     //消息id，生成方式是uuid，加上自己系统的唯一标示前缀
                    extendInfo=''                              //扩展信息
                    backupinfo=''                             //扩展信息
                    msgType='1'>                               //消息类型
                  恩                                           //发送内容
              </body>
</message>
```

| message属性  | 含义  | 备注  |
|:--|:--|:--|
| from  | 消息的发送者  |   |
| to  | 消息接受者  |   |
| type    | chat或者consult       |   |
| msec_times        | 毫秒时间戳           |   |

| body属性  | 含义  | 备注  |
|:--|:--|:--|
| id  | 消息id  |   |
| msgType  | 消息子类型  |   |
| extendInfo  | 扩展信息  |   |
| backupinfo  | 扩展信息  |   |
| cdata  | 消息内容  |   |

## 消息存储

消息存储逻辑代码在：ejabberd_sm.erl中的insert_chat_msg函数

## 数据库格式

数据库表名：msg_history

| 字段名  | 含义  | 备注  |
|:--|:--|:--|
| m_from  | 消息发送者的用户id  |   |
| m_to  | 消息接受者的用户id  |   |
| m_body  | xml消息体  |   |
| create_time  | 消息的时间  |   |
| read_flag  | 已读标志（0：未发送给接收者，1已发送给接收者，3接收者已经阅读了消息）  |   |
| msg_id  | 消息id  |   |
| chat_id  | 已废弃  |   |
| from_host  | 发送者的域  |   |
| to_host  | 接受者的域  |   |
| realfrom  | 消息的真正发送者，consult消息才会有  |   |
| realto  | 消息的真正接收者，consult消息才会有  |   |
| msg_type  | 消息类型（chat：单聊消息,consult：咨询消息）  |   |
| update_time  | 已读标志的更新时间  |   |
