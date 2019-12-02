# msgtype说明

## 图文混排消息

* msgType:1
* 普通文本：xxxxxxxxxxxx
* 链接：[obj type="url" value="http://www.qunar.com"]
* 图片：[obj type="image" value="http://www.qunar.com/image.jpg"]
* 电话：[obj type="tel" value="13900001111"]
* 样例：

```
<message from='test1@qtalk.test.org'        //发消息的人@xxxx
          to='test2@qtalk.test.org'               //收消息的人@xxxx
          type='chat'>                        //固定写死
              <body id='watcher-18DE3DBE1AF941B3B786C31AA894617F'      //消息id，生成方式是uuid，加上自己系统的唯一标示前缀
                    maType='20'                                //消息来源
                    msgType='1'>                               //消息类型
                  xxxxxxx[obj type=&quot;url&quot; value=&quot;http://www.qunar.com&quot;][obj type=&quot;image&quot; value=&quot;http://www.qunar.com/image.jpg&quot;][obj type=&quot;tel&quot; value=&quot;13900001111&quot;]                         //发送内容
              </body>
</message>
```



## 备注
消息样例
```
<message from='test1@qtalk.test.org'        //发消息的人@xxxx
          to='test2@qtalk.test.org'               //收消息的人@xxxx
          type='chat'>                        //固定写死
              <body id='watcher-18DE3DBE1AF941B3B786C31AA894617F'      //消息id，生成方式是uuid，加上自己系统的唯一标示前缀
                    maType='20'                                //消息来源
                    msgType='1'>                               //消息类型
                  xxxxxxx[obj type=&quot;url&quot; value=&quot;http://www.qunar.com&quot;][obj type=&quot;image&quot; value=&quot;http://www.qunar.com/image.jpg&quot;][obj type=&quot;tel&quot; value=&quot;13900001111&quot;]                         //发送内容
              </body>
</message>
```

