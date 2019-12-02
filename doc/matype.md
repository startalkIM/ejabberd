# matype说明


* mac客户端(1)
* ios客户端(2、8、9、10)
* PC客户端(3)
* 安卓客户端(4、11)
* Linux客户端(5)
* web客户端(6)
* 来自接口发的消息(20)

## 备注：

消息样例
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