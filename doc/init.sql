CREATE TABLE sys_role
(
  id          SERIAL                                                NOT NULL
    CONSTRAINT sys_role_pkey
    PRIMARY KEY,
  describe    VARCHAR(100)                                          NOT NULL,
  create_time TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  update_time TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  role_name   VARCHAR(100)
);

COMMENT ON COLUMN sys_role.id IS '自增id';

COMMENT ON COLUMN sys_role.describe IS '角色描述';

COMMENT ON COLUMN sys_role.create_time IS '创建时间';

COMMENT ON COLUMN sys_role.update_time IS '更新时间';

COMMENT ON COLUMN sys_role.role_name IS '角色名称';


INSERT INTO sys_role ( describe, create_time, update_time, role_name) VALUES ('所有权限', '2019-05-15 03:25:07.078138', '2019-05-15 03:25:07.078138', '超级管理员');



CREATE TABLE sys_role_permission
(
  id            SERIAL NOT NULL
    CONSTRAINT sys_role_permission_pkey
    PRIMARY KEY,
  role_id       INTEGER,
  permission_id INTEGER
);

COMMENT ON COLUMN sys_role_permission.id IS '自增id';

COMMENT ON COLUMN sys_role_permission.role_id IS '角色id';

COMMENT ON COLUMN sys_role_permission.permission_id IS '权限id';

INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 1);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 2);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 3);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 4);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 5);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 6);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 7);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 8);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 9);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 10);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 11);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 12);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 13);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 14);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 15);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 16);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 17);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 18);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 19);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 20);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 21);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 22);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 23);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 24);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 25);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 26);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 27);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 28);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 29);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 30);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 31);
INSERT INTO sys_role_permission (role_id, permission_id) VALUES (1, 32);


CREATE TABLE sys_permission
(
  id                 SERIAL                                                NOT NULL
    CONSTRAINT sys_permission_pkey
    PRIMARY KEY,
  url                VARCHAR(200)                                           NOT NULL,
  describe           VARCHAR(100)                                          NOT NULL,
  create_time        TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  update_time        TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  status             INTEGER DEFAULT 0,
  sub_permission_ids INTEGER [],
  navigation_flag    INTEGER DEFAULT 0
);

COMMENT ON COLUMN sys_permission.id IS '自增id';

COMMENT ON COLUMN sys_permission.url IS '权限地址';

COMMENT ON COLUMN sys_permission.describe IS '权限描述';

COMMENT ON COLUMN sys_permission.create_time IS '创建时间';

COMMENT ON COLUMN sys_permission.update_time IS '更新时间';

COMMENT ON COLUMN sys_permission.status IS '0:导航栏不显示,1:导航栏显示';

COMMENT ON COLUMN sys_permission.sub_permission_ids IS '子权限id列表';

COMMENT ON COLUMN sys_permission.navigation_flag IS '是否映射导航栏';

INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (1, '/qtalk_background_management/startalk/management/addUser/user', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (2, '/qtalk_background_management/startalk/management/download/template', '', '2019-05-15 09:56:18.308918', '2019-05-15 09:56:18.308918', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (3, '/qtalk_background_management/startalk/management/update/user', '', '2019-05-15 09:57:07.807083', '2019-05-15 09:57:07.807083', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (4, '/qtalk_background_management/startalk/management/delete/user', '', '2019-05-15 09:57:07.835557', '2019-05-15 09:57:07.835557', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (5, '/qtalk_background_management/startalk/management/getuserDetail', '', '2019-05-15 09:57:08.712239', '2019-05-15 09:57:08.712239', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (6, '/qtalk_background_management/startalk/management/organ/addDep', '', '2019-05-15 09:57:08.140355', '2019-05-15 09:57:08.140355', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (7, '/qtalk_background_management/startalk/management/organ/deleteDep', '', '2019-05-15 09:57:08.169407', '2019-05-15 09:57:08.169407', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (8, '/qtalk_background_management/startalk/management/organ/addRoleClass', '', '2019-05-15 09:57:08.226841', '2019-05-15 09:57:08.226841', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (9, '/qtalk_background_management/startalk/management/organ/addRole', '', '2019-05-15 09:57:08.252260', '2019-05-15 09:57:08.252260', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (10, '/qtalk_background_management/startalk/management/organ/updateRole', '', '2019-05-15 09:57:08.284820', '2019-05-15 09:57:08.284820', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (11, '/qtalk_background_management/startalk/management/organ/deleteRole', '', '2019-05-15 09:57:08.316238', '2019-05-15 09:57:08.316238', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (12, '/qtalk_background_management/startalk/management/organ/getRoleClass', '', '2019-05-15 09:57:08.359823', '2019-05-15 09:57:08.359823', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (13, '/qtalk_background_management/startalk/management/searchUser', '用户搜索', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (14, '/qtalk_background_management/startalk/management/unbindRole', '解绑用户', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (15, '/qtalk_background_management/startalk/management/bindRole', '绑定用户', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (16, '/qtalk_background_management/startalk/management/findAllRoles', '所有角色', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, '{18,19,20,21,22}', 1);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (17, '/qtalk_background_management/startalk/management/queryUserList', '用户列表', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, '{13,14,15,16}', 1);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (18, '/qtalk_background_management/startalk/management/findAllPermissions', '所有权限', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (19, '/qtalk_background_management/startalk/management/findPermissionByRoleId', '角色查找权限', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (20, '/qtalk_background_management/startalk/management/updateRolePermissions', '更新权限', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (21, '/qtalk_background_management/startalk/management/deleteRole', '删除角色', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (22, '/qtalk_background_management/startalk/management/addNewRole', '添加角色', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (23, '/qtalk_background_management/startalk/management/organ/getStructure', '', '2019-05-15 09:57:08.078643', '2019-05-15 09:57:08.078643', 1, '{6,7}', 1);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (24, '/qtalk_background_management/startalk/management/organ/getAllRole', '', '2019-05-15 09:57:08.501870', '2019-05-15 09:57:08.501870', 1, '{8,9,10,11,12}', 1);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (25, '/qtalk_background_management/startalk/management/search', '', '2019-05-15 09:57:08.662159', '2019-05-15 09:57:08.662159', 1, '{1,2,3,4,5}', 1);

INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (26, '/qtalk_background_management/startalk/management/find/add/application', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (27, '/qtalk_background_management/startalk/management/find/get/group', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (28, '/qtalk_background_management/startalk/management/find/update/app', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (29, '/qtalk_background_management/startalk/management/find/action/app', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (30, '/qtalk_background_management/startalk/management/find/add/group', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (31, '/qtalk_background_management/startalk/management/file/upload', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (32, '/qtalk_background_management/startalk/management/find/management', '', '2019-05-15 09:57:08.662159', '2019-05-15 09:57:08.662159', 1, '{26,27,28,29,30,31}', 1);

alter sequence sys_permission_id_seq restart with 33;


CREATE TABLE sys_user_role
(
  id      SERIAL      NOT NULL
    CONSTRAINT sys_user_role_pkey
    PRIMARY KEY,
  role_id INTEGER,
  user_id VARCHAR(60) NOT NULL
);

COMMENT ON COLUMN sys_user_role.id IS '自增id';

COMMENT ON COLUMN sys_user_role.role_id IS '角色id';

COMMENT ON COLUMN sys_user_role.user_id IS '用户id';

CREATE UNIQUE INDEX sys_user_role_user_id_uindex ON sys_user_role (user_id);
INSERT INTO sys_user_role (role_id, user_id) VALUES ( 1, 'admin');



CREATE TABLE persistent_logins
(
  username  VARCHAR(64) NOT NULL,
  series    VARCHAR(64) NOT NULL
    CONSTRAINT persistent_logins_pkey
    PRIMARY KEY,
  token     VARCHAR(64) NOT NULL,
  last_used TIMESTAMP   NOT NULL
);

create table find_application_table
(
    id                  serial              not null
        constraint find_application_table_pk
            primary key,
    application_type    integer             not null,
    visible_range       text,
    application_name    text                not null,
    application_class   text                not null,
    application_icon    text                not null,
    application_version integer             not null,
    ios_version         integer,
    android_version     integer,
    ios_bundle          text,
    android_bundle      text,
    application_desc    text,
    create_time         timestamp default now(),
    update_time         timestamp default now(),
    disable_flag        smallint  default 0 not null,
    member_id           integer             not null,
    h5_action           text,
    entrance            text,
    properties          text,
    module              text,
    show_native_nav     boolean,
    nav_title           text,
    valid_platform      text,
    visible_platform    smallint,
    bundle_name         text,
    h5_action_ios       text,
    h5_action_android   text,
    delete_flag         smallint  default 0 not null,
    native_flag         smallint  default 0 not null,
    app_uuid            varchar(50)
);

comment on column find_application_table.id is '自增id';

comment on column find_application_table.application_type is '应用类型，2RN应用，3 H5应用';

comment on column find_application_table.visible_range is '可见性范围，空标识全员可见';

comment on column find_application_table.application_name is '应用名称';

comment on column find_application_table.application_class is '应用分类';

comment on column find_application_table.application_icon is '应用图标';

comment on column find_application_table.application_version is '应用版本号';

comment on column find_application_table.ios_version is 'ios版本号';

comment on column find_application_table.android_version is '安卓版本号';

comment on column find_application_table.ios_bundle is 'iosbundle包，h5应用的话对应的是h5的地址';

comment on column find_application_table.android_bundle is 'android的bundle包，h5应用对应的是地址';

comment on column find_application_table.application_desc is '应用描述';

comment on column find_application_table.create_time is '创建时间';

comment on column find_application_table.update_time is '更新时间';

comment on column find_application_table.disable_flag is '禁用标志位';

comment on column find_application_table.member_id is '在群组的id';

comment on column find_application_table.h5_action is 'h5页面地址';

comment on column find_application_table.entrance is 'RN应用的入口地址';

comment on column find_application_table.properties is '额外初始属性 map的json';

comment on column find_application_table.module is 'RN应用的程序入口';

comment on column find_application_table.show_native_nav is '是否显示导航';

comment on column find_application_table.nav_title is '导航title h5应用不生效';

comment on column find_application_table.valid_platform is '可适配的客户端类型，IOS Angroid PC';

comment on column find_application_table.visible_platform is 'ios|Android|pc(101):5';

comment on column find_application_table.bundle_name is 'bundle 包的文件名 不同于applicaName';

comment on column find_application_table.h5_action_ios is 'ios h5的页面地址';

comment on column find_application_table.h5_action_android is 'h5 android页面地址';

comment on column find_application_table.delete_flag is '删除标记位，1删除 0未删除';

comment on column find_application_table.native_flag is '原生应用标记0是自定义，1是原生应用禁止修改';

comment on column find_application_table.app_uuid is '应用的UUID';


create unique index find_application_table_id_uindex
    on find_application_table (id);

create index find_application_table_application_name_index
    on find_application_table (application_name);

create table find_class_table
(
    id         serial       not null
        constraint find_class_table_pk
            primary key,
    group_name varchar(200) not null,
    group_icon varchar(200) not null
);

comment on table find_class_table is '应用分类表';

comment on column find_class_table.id is '自增ID';

comment on column find_class_table.group_name is '分组名称';

comment on column find_class_table.group_icon is '分组封面';


create unique index find_class_table_group_name_uindex
    on find_class_table (group_name);

create unique index find_class_table_id_uindex
    on find_class_table (id);

create table startalk_dep_table
(
    id          serial                              not null
        constraint startalk_dep_table_pk
            primary key,
    dep_name    text                                not null,
    dep_level   integer                             not null,
    dep_vp      text,
    dep_hr      text,
    dep_visible text,
    dep_leader  text,
    parent_id   integer,
    delete_flag integer   default 0                 not null,
    dep_desc    text,
    create_time timestamp default now()             not null,
    update_time timestamp default CURRENT_TIMESTAMP not null
);

comment on table startalk_dep_table is '部门信息表';

comment on column startalk_dep_table.id is '自增ID';

comment on column startalk_dep_table.dep_name is '部门名称';

comment on column startalk_dep_table.dep_level is '部门层级';

comment on column startalk_dep_table.dep_vp is '部门领导';

comment on column startalk_dep_table.dep_hr is '部门HR';

comment on column startalk_dep_table.dep_visible is '部门可见性';

comment on column startalk_dep_table.parent_id is '父级部门的ID';

comment on column startalk_dep_table.delete_flag is '部门删除标记位,0是未删除 1是已删除';

comment on column startalk_dep_table.dep_desc is '部门信息备注';

comment on column startalk_dep_table.create_time is '创建时间';

comment on column startalk_dep_table.update_time is '更新时间';

CREATE UNIQUE INDEX startalk_dep_table_depName_uindex ON startalk_dep_table(dep_name);
create unique index startalk_dep_table_id_uindex
    on startalk_dep_table (id);
    
INSERT INTO startalk_dep_table (dep_name, dep_level)
VALUES('/管理员',1);

INSERT INTO startalk_dep_table (dep_name, dep_level)
VALUES('/智能服务助手',1);

create table startalk_role_class
(
    id             serial            not null
        constraint startalk_role_class_pk
            primary key,
    role_class     varchar(200)      not null,
    available_flag integer default 1 not null
);

comment on table startalk_role_class is '角色分组';

comment on column startalk_role_class.id is '自增id';

comment on column startalk_role_class.role_class is '角色分组';

comment on column startalk_role_class.available_flag is '可用标志 1是可用 0表示不可用';


create unique index startalk_role_class_id_uindex
    on startalk_role_class (id);

create unique index startalk_role_class_role_class_uindex
    on startalk_role_class (role_class);

create table startalk_user_role_table
(
    id             serial            not null
        constraint startalk_user_role_table_pk
            primary key,
    role_name      text              not null,
    available_flag integer default 1 not null,
    class_id       integer           not null,
    constraint startalk_user_role_table_pk_2
        unique (role_name, class_id)
);

comment on table startalk_user_role_table is 'startalk用户角色表';

comment on column startalk_user_role_table.id is '主键自增id';

comment on column startalk_user_role_table.role_name is '角色名';

comment on column startalk_user_role_table.available_flag is '可用标志位,1可用 0 不可用';

comment on column startalk_user_role_table.class_id is '角色所属组别的ID';


create unique index startalk_user_role_table_id_uindex
    on startalk_user_role_table (id);

insert into host_info (host, description, host_admin) values ('qtalk', 'qtalk', 'admin');
insert into host_users (host_id, user_id, user_name, department, dep1, pinyin, frozen_flag, version, user_type, hire_flag, gender, password, initialpwd, pwd_salt, ps_deptid) values ('1', 'admin', '管理员', '/管理员', '管理员', 'admin', '0', '1', 'U', '1', '1', 'CRY:fd540f073cc09aa98220bbb234153bd5', '1', 'qtalkadmin_pwd_salt_d2bf42081aab47f4ac00697d7dd32993', 'qtalk');
insert into vcard_version (username, version, profile_version, gender, host, url) values ('admin', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');
insert into host_users (host_id, user_id, user_name, department, dep1, pinyin, frozen_flag, version, user_type, hire_flag, gender, password, initialpwd, pwd_salt, ps_deptid) values ('1', 'file-transfer', '文件传输助手', '/智能服务助手', '智能服务助手', 'file-transfer', '1', '1', 'U', '1', '1', 'CRY:fd540f073cc09aa98220bbb234153bd5', '1', 'qtalkadmin_pwd_salt_d2bf42081aab47f4ac00697d7dd32993', 'qtalk');
insert into vcard_version (username, version, profile_version, gender, host, url) values ('file-transfer', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');


INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(33,'test','/qtalk_background_management/startalk/management/baseData',1,'{35}',1);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(34,'test','/qtalk_background_management/startalk/management/depList',1,'{}',0);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(35,'test','/qtalk_background_management/startalk/management/dayDataSearch',1,'{}',0);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(36,'test','/qtalk_background_management/startalk/management/dayMsgDataSearch',1,'{34,37}',1);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(37,'test','/qtalk_background_management/startalk/management/userMsgCount',1,'{}' ,0);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(38,'test','/qtalk_background_management/startalk/management/clientVersion',1,'{}' ,1);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(39,'test','/qtalk_background_management/startalk/management/getVersionList',1,'{}',0);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(40,'test','/qtalk_background_management/startalk/management/clickCount',1,'{41,42}',1);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(41,'test','/qtalk_background_management/startalk/management/selectList',1,'{}',0);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(42,'test','/qtalk_background_management/startalk/management/selectModel',1,'{}',0);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(43,'test','/qtalk_background_management/startalk/management/activity',1,'{}',1);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(44,'test','/qtalk_background_management/startalk/management/userOnline',1,'{45}',1);
INSERT INTO sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(45,'test','/qtalk_background_management/startalk/management/searchUserOnline',1,'{}',0);

alter sequence sys_permission_id_seq restart with 46;

INSERT into sys_role_permission(role_id, permission_id) VALUES (1,33),(1,34),(1,35),(1,36),(1,37),(1,38),(1,39),(1,40),(1,41),(1,42),(1,43),(1,44),(1,45);

-- auto-generated definition
CREATE TABLE data_board_day
(
  id                 SERIAL                    NOT NULL
    CONSTRAINT data_board_day_pkey
    PRIMARY KEY,
  activity           NUMERIC DEFAULT 0         NOT NULL,
  client_online_time JSONB                     NOT NULL,
  start_count        NUMERIC DEFAULT 0         NOT NULL,
  client_version     JSONB                     NOT NULL,
  day_msg_count      NUMERIC DEFAULT 0         NOT NULL,
  day_msg_average    NUMERIC DEFAULT 0         NOT NULL,
  department_data    JSONB                     NOT NULL,
  hire_type_data     JSONB                     NOT NULL,
  create_time        DATE DEFAULT CURRENT_DATE NOT NULL,
  platform_activity  JSONB,
  dep_activity       JSONB,
  hire_type_activity JSONB
);

CREATE INDEX idx_tb_data_board
  ON data_board_day (create_time);

COMMENT ON COLUMN data_board_day.activity IS '活跃数';

COMMENT ON COLUMN data_board_day.client_online_time IS '客户端在线时间';

COMMENT ON COLUMN data_board_day.start_count IS '启动次数';

COMMENT ON COLUMN data_board_day.client_version IS '客户端版本统计';

COMMENT ON COLUMN data_board_day.day_msg_count IS '每天消息量';

COMMENT ON COLUMN data_board_day.day_msg_average IS '每天平均消息量';

COMMENT ON COLUMN data_board_day.department_data IS '部门数据统计';

COMMENT ON COLUMN data_board_day.hire_type_data IS '人员类型统计';

COMMENT ON COLUMN data_board_day.create_time IS '创建时间';

COMMENT ON COLUMN data_board_day.platform_activity IS '平台活跃数';

COMMENT ON COLUMN data_board_day.dep_activity IS '部门活跃数';

COMMENT ON COLUMN data_board_day.hire_type_activity IS '人员类型活跃数';


-- auto-generated definition
CREATE TABLE client_upgrade
(
  id               SERIAL                                                NOT NULL
    CONSTRAINT client_upgrade_pkey
    PRIMARY KEY,
  client_type      VARCHAR(20)                                           NOT NULL,
  platform         VARCHAR(50)                                           NOT NULL,
  version          INTEGER DEFAULT 0                                     NOT NULL,
  copywriting      VARCHAR(500)                                          NOT NULL,
  grayscale_status INTEGER DEFAULT 0                                     NOT NULL,
  grayscale_value  INTEGER      DEFAULT 0,
  upgrade_status   INTEGER DEFAULT 0                                     NOT NULL,
  upgrade_url      VARCHAR(200)                                          NOT NULL,
  create_time      TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  update_time      TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  md5_key          VARCHAR(100) DEFAULT '' :: CHARACTER VARYING,
  stop_status      INTEGER      DEFAULT 0,
  stop_reason      VARCHAR(500) DEFAULT '' :: CHARACTER VARYING,
  updated_count    INTEGER DEFAULT 0                                     NOT NULL
);

CREATE UNIQUE INDEX client_upgrade_client_type_platform_version_uindex
  ON client_upgrade (client_type, platform, version);

COMMENT ON COLUMN client_upgrade.id IS '自增id';

COMMENT ON COLUMN client_upgrade.client_type IS '客户端类型 qtalk,qchat';

COMMENT ON COLUMN client_upgrade.platform IS '平台Android,ios';

COMMENT ON COLUMN client_upgrade.version IS '版本号';

COMMENT ON COLUMN client_upgrade.copywriting IS '更新文案';

COMMENT ON COLUMN client_upgrade.grayscale_status IS '灰度测试状态 0:否 1:是';

COMMENT ON COLUMN client_upgrade.grayscale_value IS '灰度量';

COMMENT ON COLUMN client_upgrade.upgrade_status IS '更新状态 0:强制更新 1:选择更新';

COMMENT ON COLUMN client_upgrade.upgrade_url IS '更新地址';

COMMENT ON COLUMN client_upgrade.create_time IS '创建时间';

COMMENT ON COLUMN client_upgrade.update_time IS '更新时间';

COMMENT ON COLUMN client_upgrade.md5_key IS '文件MD5';

COMMENT ON COLUMN client_upgrade.stop_status IS '是否停止更新,0:否,1:是';

COMMENT ON COLUMN client_upgrade.stop_reason IS '停止更新原因';

COMMENT ON COLUMN client_upgrade.updated_count IS '已更新量';




-- 原始日志数据表
create table if not exists t_client_log
(
   id bigserial primary key,
   u_id varchar(20),
   u_domain varchar(20),
   d_os varchar(10),
   d_brand varchar(100),
   d_model varchar(30),
   d_plat varchar(10),
   d_ip varchar(15),
   d_lat varchar(30),
   d_lgt varchar(40),
   l_type varchar(10),
   l_sub_type varchar(10),
   l_report_time varchar(50),
   l_data text,
   l_device_data jsonb,
   l_user_data jsonb,
   l_version_code varchar(50),
   l_version_name varchar(50),
   create_time timestamp default now(),
   l_client_event varchar(50),
   d_platform varchar(50),
   l_event_id varchar(500),
   l_current_page varchar(50)
);

comment on column t_client_log.id is '主键id';
comment on column t_client_log.u_id is '用户id';
comment on column t_client_log.u_domain is '域名 eg:ejabhost1';
-- 该字段从l_device_data中的os获取
comment on column t_client_log.d_os is '操作系统 LINUX、Android、Mac、iOS、PC64、android';
-- 该字段从l_device_data中的osBrand获取
comment on column t_client_log.d_brand is '客户端手机品牌';
-- 该字段从l_device_data中的osModel获取
comment on column t_client_log.d_model is '客户端机型';
-- 该字段从l_device_data中的plat字段获取
comment on column t_client_log.d_plat is '客户端平台 startalk';
comment on column t_client_log.d_ip is '客户端ip地址';
comment on column t_client_log.d_lat is '经纬度';
comment on column t_client_log.d_lgt is '经纬度';
comment on column t_client_log.l_type is '日志类型，CAT、COD、ACT、CRA、FIL';
comment on column t_client_log.l_sub_type is '日志子类型';
comment on column t_client_log.l_report_time is '上报时间';
comment on column t_client_log.l_data is '原始日志';
comment on column t_client_log.l_device_data is '设备日志';
comment on column t_client_log.l_user_data is '用户日志';
-- 该字段从l_device_data中的versionCode字段获取
comment on column t_client_log.l_version_code is '版本编号 221';
comment on column t_client_log.l_version_name is '版本名称 3.1.5';
comment on column t_client_log.create_time is '创建时间';
comment on column t_client_log.l_client_event is '事件名称 eg：搜索、拉取历史耗时';
comment on column t_client_log.d_platform is '所属平台，与d_os类型，只是经过了转小写处理, ios/linux/mac/pc32/pc64/android ';
comment on column t_client_log.l_event_id is '事件id';
comment on column t_client_log.l_current_page is '当前页';


-- 客户端品牌字典表
create table if not exists t_dict_client_brand
(
 id bigserial not null
  constraint t_dict_client_brand_pkey
   primary key,
 brand varchar(100)
  constraint uk_tbcb_unique_index_brand
   unique,
 platform varchar(100),
 del_flag integer default 0 not null,
 create_time timestamp with time zone default now() not null
);

create unique index if not exists t_dict_client_brand_brand_platform_idx on t_dict_client_brand (brand, platform);
comment on table t_dict_client_brand is '品牌渠道字典表';
comment on column t_dict_client_brand.id is '主键';
comment on column t_dict_client_brand.brand is '客户端手机品牌';
comment on column t_dict_client_brand.platform is '品牌所属平台';
comment on column t_dict_client_brand.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column t_dict_client_brand.create_time is '创建时间';


-- 客户端机型字典表
create table if not exists t_dict_client_model
(
 id bigserial not null
  constraint t_dict_client_model_pkey
   primary key,
 client_model varchar(100),
 client_brand varchar(100),
 platform varchar(100),
 del_flag integer default 0 not null,
 create_time timestamp with time zone default now() not null
);

create unique index if not exists t_dict_client_model_client_model_client_brand_platform_idx on t_dict_client_model (client_model, client_brand, platform);
comment on table t_dict_client_model is '机型字典表';
comment on column t_dict_client_model.id is '主键';
comment on column t_dict_client_model.client_model is '机型';
comment on column t_dict_client_model.client_brand is '品牌';
comment on column t_dict_client_model.platform is '所属平台';
comment on column t_dict_client_model.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column t_dict_client_model.create_time is '创建时间';


-- 客户端版本字典表
create table if not exists t_dict_client_version
(
 id bigserial not null
  constraint t_dict_client_version_pkey
   primary key,
 client_version varchar(100)
  constraint uk_tbcv_unique_index_version
   unique,
 platform varchar(100),
 del_flag integer default 0 not null,
 create_time timestamp with time zone default now() not null
)
;

create unique index if not exists t_dict_client_version_client_version_platform_idx on t_dict_client_version (client_version, platform);
comment on table t_dict_client_version is '客户端版本字典表';
comment on column t_dict_client_version.id is '主键';
comment on column t_dict_client_version.client_version is 'qtalk客户端版本';
comment on column t_dict_client_version.platform is '所属平台';
comment on column t_dict_client_version.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column t_dict_client_version.create_time is '创建时间';

-- 点击事件字典表
create table if not exists t_dict_client_event
(
 id bigserial not null
  constraint t_dict_client_event_pkey
   primary key,
 event varchar(100),
 del_flag integer default 0 not null,
 create_time timestamp with time zone default now() not null,
 platform varchar(100)
);

create unique index if not exists event_platform_unique_idx on t_dict_client_event (event, platform);
comment on table t_dict_client_event is '点击事件字典表';
comment on column t_dict_client_event.id is '主键';
comment on column t_dict_client_event.event is '事件';
comment on column t_dict_client_event.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column t_dict_client_event.create_time is '创建时间';


-- 点击数据统计表
create table if not exists statistic_qtalk_click_event
(
 id bigserial not null
  constraint statistic_qtalk_click_event_pkey
   primary key,
 client_platform varchar(100),
 client_version varchar(100),
 client_brand varchar(100),
 client_model varchar(100),
 click_event varchar(100),
 click_day date,
 click_cnt bigint,
 del_flag integer default 0 not null,
 create_time timestamp with time zone default now() not null
);

comment on table statistic_qtalk_click_event is '点击统计数据表';
comment on column statistic_qtalk_click_event.id is '主键';
comment on column statistic_qtalk_click_event.client_platform is '所属平台';
comment on column statistic_qtalk_click_event.client_version is '客户端版本号';
comment on column statistic_qtalk_click_event.client_brand is '客户端品牌';
comment on column statistic_qtalk_click_event.client_model is '客户端型号';
comment on column statistic_qtalk_click_event.click_event is '点击事件';
comment on column statistic_qtalk_click_event.click_day is '日期(天)';
comment on column statistic_qtalk_click_event.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column statistic_qtalk_click_event.create_time is '创建时间';
-- ldap 配置表
create table qtalk_config
(
    id  serial   primary key  not null,
    config_key   varchar(30)  not null,
    config_value varchar(500) not null,
    create_time  timestamp default now()
);
comment on column qtalk_config.config_key is '配置key';
comment on column qtalk_config.config_value is '配置值';
comment on column qtalk_config.create_time is '创建时间';
create unique index qtalk_config_id_uindex
    on qtalk_config (id);
create unique index qtalk_config_config_key_uindex
    on qtalk_config (config_key);



