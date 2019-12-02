# 人员管理

## 插入组织架构里的人员

```
插入用户相关信息
insert into host_users (host_id, user_id, user_name, department, dep1, pinyin, frozen_flag, version, user_type, hire_flag, gender, password, initialpwd, ps_deptid) values ('1', 'test', '测试账号', '/机器人', '机器人', 'test', '0', '1', 'U', '1', '1', '1234567890', '1', 'qtalk');

host_id: host_info表中id
user_id: 用户id
user_name: 用户昵称
department: 完整的组织架构(/部门1/部门2/部门3)
dep1,dep2,dep3,dep4,dep5: 各个子部门(部门1,部门2,部门3)，没有的不插入
pinyin: 用户搜索用的拼音
frozen_flag: 禁止登陆的标志，目前没用到
version: 版本号，修改某个人的信息后，要把version置成当前所有version最大值加1
user_type: U|R(U是普通人员，R是虚拟人员)
hire_flag: 在职标志(1是在职，0是不在职)
gender: 性别(0是女，1是男)
password: 密码
initialpwd：初始密码，暂时没用到
ps_depid: 公司名


插入用户名片
insert into vcard_version (username, version, profile_version, gender, host, url) values ('test', '1', '1', '1', 'qtalk.test.org', 'https://qt.qunar.com/file/v2/download/avatar/1af5bc967f8535a4af19eca10dc95cf1.png');

username: 对应的host_users的user_id
version: 名片的版本号，更新名片的时候，要把版本号加1
profile_version: 名片版本号，兼容问题，更新名片的时候，要把版本号加1
gender: 性别(0是女，1是男)
host: 对应的域名
url: 头像图片的地址
```
