--
-- PostgreSQL database cluster dump
--

-- Started on 2018-12-13 17:11:42 CST

SET default_transaction_read_only = off;

SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;


\connect ejabberd

SET default_transaction_read_only = off;

--
-- PostgreSQL database dump
--

-- Dumped from database version 10.5
-- Dumped by pg_dump version 10.5

-- Started on 2018-12-13 17:11:42 CST

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;
SET row_security = off;

SET default_tablespace = '';
SET default_with_oids = false;

--
-- TOC entry 203 (class 1259 OID 17229)
-- Name: admin_user; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.admin_user (
    username character varying(255) NOT NULL,
    priority text NOT NULL
);


ALTER TABLE public.admin_user OWNER TO ejabberd;


--
-- 
--

--
-- Name: login_data; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.login_data (
    id integer NOT NULL,
    username character varying(255) NOT NULL,
    host character varying(255) NOT NULL,
    resource character varying(255) NOT NULL,
    platform character varying(255) NOT NULL,
    ip inet,
    login_time timestamp with time zone DEFAULT now(),
    logout_at timestamp with time zone DEFAULT now(),
    record_type character varying(255) NOT NULL
);


ALTER TABLE public.login_data OWNER TO ejabberd;

--
-- Name: TABLE login_data; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.login_data IS 'qtalk登录记录';


--
-- Name: COLUMN login_data.id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.id IS '登录记录ID';


--
-- Name: COLUMN login_data.username; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.username IS '用户名';


--
-- Name: COLUMN login_data.host; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.host IS 'host';


--
-- Name: COLUMN login_data.resource; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.resource IS 'resource';


--
-- Name: COLUMN login_data.platform; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.platform IS '平台';


--
-- Name: COLUMN login_data.ip; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.ip IS '登录ip地址';


--
-- Name: COLUMN login_data.login_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.login_time IS '登录时间';


--
-- Name: COLUMN login_data.logout_at; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.logout_at IS '退出时间';


--
-- Name: COLUMN login_data.record_type; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.login_data.record_type IS '类型';


--
-- Name: login_data_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.login_data_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.login_data_id_seq OWNER TO ejabberd;

--
-- Name: login_data_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.login_data_id_seq OWNED BY public.login_data.id;


--
-- Name: login_data id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_data ALTER COLUMN id SET DEFAULT nextval('public.login_data_id_seq'::regclass);


--
-- Name: login_data login_data_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_data
    ADD CONSTRAINT login_data_pkey PRIMARY KEY (id);

--
-- Name: login_data_ip_idx; Type: INDEX; Schema: public; Owner: postgres
--

-- CREATE INDEX login_data_ip_idx ON public.login_data USING gist (ip inet_ops);

--
-- Name: login_data_login_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX login_data_login_time_idx ON public.login_data USING btree (login_time);


--
-- Name: login_data_logout_at_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX login_data_logout_at_idx ON public.login_data USING btree (logout_at);


--
-- Name: login_data_record_type_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX login_data_record_type_idx ON public.login_data USING btree (record_type);


--
-- Name: login_data_username_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX login_data_username_idx ON public.login_data USING btree (username);

--
-- TOC entry 207 (class 1259 OID 17249)
-- Name: client_config_sync; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.client_config_sync (
    id integer NOT NULL,
    username character varying(255) NOT NULL,
    host character varying(255) NOT NULL,
    configkey character varying(255) NOT NULL,
    subkey character varying(255) NOT NULL,
    configinfo text NOT NULL,
    version bigint DEFAULT 1,
    operate_plat character varying(50) NOT NULL,
    create_time timestamp with time zone DEFAULT now(),
    update_time timestamp with time zone DEFAULT now(),
    isdel smallint DEFAULT 0
);


ALTER TABLE public.client_config_sync OWNER TO ejabberd;

--
-- TOC entry 4876 (class 0 OID 0)
-- Dependencies: 207
-- Name: TABLE client_config_sync; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.client_config_sync IS '客户端漫游用户配置表';


--
-- TOC entry 4877 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.username; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.username IS '用户用户名';


--
-- TOC entry 4878 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.host; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.host IS '用户域名';


--
-- TOC entry 4879 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.configkey; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.configkey IS '漫游数据key';


--
-- TOC entry 4880 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.subkey; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.subkey IS '漫游数据子key';


--
-- TOC entry 4881 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.configinfo; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.configinfo IS '漫游数据';


--
-- TOC entry 4882 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.version; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.version IS '版本号';


--
-- TOC entry 4883 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.operate_plat; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.operate_plat IS '操作平台';


--
-- TOC entry 4884 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.create_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.create_time IS '创建时间';


--
-- TOC entry 4885 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.update_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.update_time IS '更新时间';


--
-- TOC entry 4886 (class 0 OID 0)
-- Dependencies: 207
-- Name: COLUMN client_config_sync.isdel; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.client_config_sync.isdel IS '是否删除或取消';


--
-- TOC entry 208 (class 1259 OID 17259)
-- Name: client_config_sync_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.client_config_sync_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.client_config_sync_id_seq OWNER TO ejabberd;

--
-- TOC entry 4887 (class 0 OID 0)
-- Dependencies: 208
-- Name: client_config_sync_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.client_config_sync_id_seq OWNED BY public.client_config_sync.id;


--
-- TOC entry 210 (class 1259 OID 17264)
-- Name: destroy_muc_info; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.destroy_muc_info (
    muc_name text NOT NULL,
    nick_name text,
    reason text,
    id bigint NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.destroy_muc_info OWNER TO ejabberd;

--
-- TOC entry 211 (class 1259 OID 17271)
-- Name: destroy_muc_info_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.destroy_muc_info_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.destroy_muc_info_id_seq OWNER TO ejabberd;

--
-- TOC entry 4888 (class 0 OID 0)
-- Dependencies: 211
-- Name: destroy_muc_info_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.destroy_muc_info_id_seq OWNED BY public.destroy_muc_info.id;


--
-- TOC entry 213 (class 1259 OID 17279)
-- Name: flogin_user; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.flogin_user (
      id serial, 
      username varchar(1000) not null,
      create_time timestamptz not null default now()
);


ALTER TABLE public.flogin_user OWNER TO ejabberd;


--
-- TOC entry 219 (class 1259 OID 17316)
-- Name: fresh_empl_entering; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.fresh_empl_entering (
    id bigint NOT NULL,
    user_id character varying(20) NOT NULL,
    user_name character varying(20) NOT NULL,
    hire_flag smallint DEFAULT 1 NOT NULL,
    join_date date NOT NULL,
    send_state smallint DEFAULT 0 NOT NULL,
    sn character varying(15) NOT NULL,
    manager character varying(20) NOT NULL,
    manager_mail character varying(50) NOT NULL,
    dep1 character varying(20) NOT NULL,
    dep2 character varying(20),
    dep3 character varying(20),
    dep4 character varying(20),
    dep5 character varying(20),
    job character varying(20),
    job_code character varying(20) NOT NULL,
    probation_date date NOT NULL,
    version smallint DEFAULT 0 NOT NULL,
    create_time timestamp with time zone DEFAULT now(),
    update_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.fresh_empl_entering OWNER TO ejabberd;

--
-- TOC entry 4890 (class 0 OID 0)
-- Dependencies: 219
-- Name: TABLE fresh_empl_entering; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.fresh_empl_entering IS '表名';


--
-- TOC entry 4891 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.id IS '唯一主键';


--
-- TOC entry 4892 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.user_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.user_id IS 'qtalkid';


--
-- TOC entry 4893 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.user_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.user_name IS '姓名';


--
-- TOC entry 4894 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.hire_flag; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.hire_flag IS '入职状态，0：已入职，1：未入职，2：推迟入职，3：待定入职';


--
-- TOC entry 4895 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.join_date; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.join_date IS '入职日期';


--
-- TOC entry 4896 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.send_state; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.send_state IS '消息发送状态，0：未发送，1：已发送';


--
-- TOC entry 4897 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.sn; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.sn IS '员工号';


--
-- TOC entry 4898 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.manager; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.manager IS '主管号';


--
-- TOC entry 4899 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.manager_mail; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.manager_mail IS '主管邮箱';


--
-- TOC entry 4900 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.dep1; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.dep1 IS '一级部门';


--
-- TOC entry 4901 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.job; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.job IS '岗位名称';


--
-- TOC entry 4902 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.job_code; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.job_code IS '岗位编码';


--
-- TOC entry 4903 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.probation_date; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.probation_date IS '转正日期';


--
-- TOC entry 4904 (class 0 OID 0)
-- Dependencies: 219
-- Name: COLUMN fresh_empl_entering.version; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.fresh_empl_entering.version IS '入职日期修改次数';


--
-- TOC entry 220 (class 1259 OID 17324)
-- Name: fresh_empl_entering_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.fresh_empl_entering_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.fresh_empl_entering_id_seq OWNER TO ejabberd;

--
-- TOC entry 4906 (class 0 OID 0)
-- Dependencies: 220
-- Name: fresh_empl_entering_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.fresh_empl_entering_id_seq OWNED BY public.fresh_empl_entering.id;


--
-- TOC entry 223 (class 1259 OID 17337)
-- Name: host_info; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.host_info (
    id bigint NOT NULL,
    host text NOT NULL,
    description text,
    create_time timestamp with time zone DEFAULT now() NOT NULL,
    host_type int not null default 0,
    host_qrcode varchar(1000),
    need_approve int not null default 0,
    host_admin text NOT NULL
);


ALTER TABLE public.host_info OWNER TO ejabberd;

--
-- TOC entry 224 (class 1259 OID 17344)
-- Name: host_info_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.host_info_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.host_info_id_seq OWNER TO ejabberd;

--
-- TOC entry 4910 (class 0 OID 0)
-- Dependencies: 224
-- Name: host_info_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.host_info_id_seq OWNED BY public.host_info.id;


--
-- TOC entry 225 (class 1259 OID 17346)
-- Name: host_users; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.host_users (
    id bigint NOT NULL,
    host_id bigint NOT NULL,
    user_id text NOT NULL,
    user_name text NOT NULL,
    department text NOT NULL,
    tel text,
    email text,
    dep1 text NOT NULL,
    dep2 text DEFAULT ''::text,
    dep3 text DEFAULT ''::text,
    dep4 text DEFAULT ''::text,
    dep5 text DEFAULT ''::text,
    pinyin text,
    frozen_flag smallint DEFAULT 0 NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    user_type character(1) DEFAULT 'U'::bpchar,
    hire_flag smallint DEFAULT 1 NOT NULL,
    gender smallint DEFAULT 0 NOT NULL,
    password text,
    initialpwd smallint DEFAULT 1 NOT NULL,
    pwd_salt character varying(200),
    leader character varying(200),
    hrbp character varying(200),
    user_role integer DEFAULT 0 NOT NULL,
    approve_flag int default 1,
    user_desc varchar(1024),
    user_origin int default 0,
    hire_type varchar(50) DEFAULT '未知',
    admin_flag varchar(20) DEFAULT '0',
    create_time timestamp DEFAULT (now())::timestamp(3),
    ps_deptid text DEFAULT 'QUNAR'::text
);


ALTER TABLE public.host_users OWNER TO ejabberd;

COMMENT ON COLUMN public.host_users.pwd_salt IS '密码salt';

--
-- TOC entry 226 (class 1259 OID 17363)
-- Name: host_users_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.host_users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.host_users_id_seq OWNER TO ejabberd;

--
-- TOC entry 4913 (class 0 OID 0)
-- Dependencies: 226
-- Name: host_users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.host_users_id_seq OWNED BY public.host_users.id;


--
-- TOC entry 229 (class 1259 OID 17385)
-- Name: invite_spool; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.invite_spool (
    username text NOT NULL,
    inviter text NOT NULL,
    body text NOT NULL,
    "timestamp" integer NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    host text DEFAULT 'ejabhost1'::text NOT NULL,
    ihost text DEFAULT 'ejabhost1'::text NOT NULL
);


ALTER TABLE public.invite_spool OWNER TO ejabberd;

--
-- TOC entry 230 (class 1259 OID 17394)
-- Name: iplimit; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.iplimit (
    ip text NOT NULL,
    created_at timestamp(6) without time zone DEFAULT now() NOT NULL,
    descriptions text DEFAULT now() NOT NULL,
    name text DEFAULT 'ALL'::text NOT NULL,
    priority text DEFAULT '1'::text NOT NULL,
    manager text
);


ALTER TABLE public.iplimit OWNER TO ejabberd;

--
-- TOC entry 231 (class 1259 OID 17404)
-- Name: irc_custom; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.irc_custom OWNER TO ejabberd;

--
-- TOC entry 232 (class 1259 OID 17411)
-- Name: last; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.last (
    username text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL
);


ALTER TABLE public.last OWNER TO ejabberd;

--
-- TOC entry 237 (class 1259 OID 17437)
-- Name: meeting_info; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.meeting_info (
    id integer NOT NULL,
    meeting_id character varying(255) NOT NULL,
    meeting_name character varying(255) NOT NULL,
    meeting_remarks text DEFAULT ''::text,
    meeting_intr text DEFAULT ''::text,
    meeting_locale character varying(255) NOT NULL,
    meeting_room character varying(255) NOT NULL,
    schedule_time timestamp with time zone DEFAULT now(),
    meeting_date date NOT NULL,
    begin_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone NOT NULL,
    inviter character varying(255) NOT NULL,
    member character varying(255) NOT NULL,
    mem_action integer DEFAULT 0,
    remind_flag integer DEFAULT 0,
    refuse_reason text,
    canceled boolean DEFAULT false
);


ALTER TABLE public.meeting_info OWNER TO ejabberd;

--
-- TOC entry 4918 (class 0 OID 0)
-- Dependencies: 237
-- Name: TABLE meeting_info; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.meeting_info IS '存储会议提醒信息表';


--
-- TOC entry 4919 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.meeting_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.meeting_id IS '会议ID';


--
-- TOC entry 4920 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.meeting_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.meeting_name IS '会议名称';


--
-- TOC entry 4921 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.meeting_remarks; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.meeting_remarks IS '会议备注';


--
-- TOC entry 4922 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.meeting_intr; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.meeting_intr IS '会议内容';


--
-- TOC entry 4923 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.meeting_locale; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.meeting_locale IS '会议地点';


--
-- TOC entry 4924 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.meeting_room; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.meeting_room IS '会议室名字';


--
-- TOC entry 4925 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.schedule_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.schedule_time IS '会议预约时间';


--
-- TOC entry 4926 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.meeting_date; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.meeting_date IS '会议日期';


--
-- TOC entry 4927 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.begin_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.begin_time IS '会议开始时间';


--
-- TOC entry 4928 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.end_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.end_time IS '会议结束时间';


--
-- TOC entry 4929 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.inviter; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.inviter IS '会议邀请者';


--
-- TOC entry 4930 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.member; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.member IS '会议被邀请者';


--
-- TOC entry 4931 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.mem_action; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.mem_action IS '参会人员反馈';


--
-- TOC entry 4932 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.remind_flag; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.remind_flag IS '提醒状态';


--
-- TOC entry 4933 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.refuse_reason; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.refuse_reason IS '参会者拒绝参会的原因';


--
-- TOC entry 4934 (class 0 OID 0)
-- Dependencies: 237
-- Name: COLUMN meeting_info.canceled; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.meeting_info.canceled IS '会议是否取消';


--
-- TOC entry 238 (class 1259 OID 17449)
-- Name: meeting_info_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.meeting_info_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.meeting_info_id_seq OWNER TO ejabberd;

--
-- TOC entry 4935 (class 0 OID 0)
-- Dependencies: 238
-- Name: meeting_info_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.meeting_info_id_seq OWNED BY public.meeting_info.id;


--
-- TOC entry 245 (class 1259 OID 17489)
-- Name: motd; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.motd (
    username text NOT NULL,
    xml text,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.motd OWNER TO ejabberd;

--
-- TOC entry 246 (class 1259 OID 17496)
-- Name: msg_history; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.msg_history (
    m_from character varying(255),
    m_to character varying(255),
    m_body text,
    create_time timestamp with time zone DEFAULT (now())::timestamp(3),
    id bigint NOT NULL,
    read_flag smallint DEFAULT 0,
    msg_id text,
    from_host text,
    to_host text,
    realfrom character varying(255),
    realto character varying(255),
    msg_type character varying(255),
    update_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.msg_history OWNER TO ejabberd;

--
-- TOC entry 247 (class 1259 OID 17505)
-- Name: msg_history_backup; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.msg_history_backup (
    m_from character varying(255),
    m_to character varying(255),
    m_body text,
    create_time timestamp with time zone DEFAULT (now())::timestamp(3),
    id bigint NOT NULL,
    read_flag smallint DEFAULT 0,
    msg_id text,
    from_host text,
    to_host text,
    realfrom character varying(255),
    realto character varying(255),
    msg_type character varying(255),
    update_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.msg_history_backup OWNER TO ejabberd;

--
-- TOC entry 248 (class 1259 OID 17514)
-- Name: msg_history_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.msg_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.msg_history_id_seq OWNER TO ejabberd;

--
-- TOC entry 4937 (class 0 OID 0)
-- Dependencies: 248
-- Name: msg_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.msg_history_id_seq OWNED BY public.msg_history.id;


--
-- TOC entry 253 (class 1259 OID 17533)
-- Name: msgview_old; Type: VIEW; Schema: public; Owner: postgres
--

CREATE VIEW public.msgview_old AS
 SELECT msg_history.m_from,
    msg_history.create_time AS s_date
   FROM public.msg_history;


ALTER TABLE public.msgview_old OWNER TO ejabberd;

--
-- TOC entry 254 (class 1259 OID 17537)
-- Name: muc_last; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_last (
    muc_name text NOT NULL,
    create_time text,
    last_msg_time text DEFAULT '0'::text
);


ALTER TABLE public.muc_last OWNER TO ejabberd;

--
-- TOC entry 255 (class 1259 OID 17544)
-- Name: muc_registered; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.muc_registered OWNER TO ejabberd;

--
-- TOC entry 256 (class 1259 OID 17551)
-- Name: muc_room; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.muc_room OWNER TO ejabberd;

--
-- TOC entry 257 (class 1259 OID 17558)
-- Name: muc_room_backup; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_room_backup (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.muc_room_backup OWNER TO ejabberd;

--
-- TOC entry 258 (class 1259 OID 17565)
-- Name: muc_room_history; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_room_history (
    muc_room_name character varying(255),
    nick character varying(255),
    packet text,
    have_subject boolean,
    size character varying(255),
    create_time timestamp with time zone DEFAULT (now())::timestamp(3),
    id bigint NOT NULL,
    host text DEFAULT 'ejabhost1'::text,
    msg_id text
);


ALTER TABLE public.muc_room_history OWNER TO ejabberd;

--
-- TOC entry 259 (class 1259 OID 17573)
-- Name: muc_room_history_backup; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.muc_room_history_backup (
    muc_room_name character varying(255),
    nick character varying(255),
    packet text,
    have_subject boolean,
    size character varying(255),
    create_time timestamp with time zone DEFAULT (now())::timestamp(3),
    id bigint NOT NULL,
    host text,
    msg_id text
);


ALTER TABLE public.muc_room_history_backup OWNER TO ejabberd;

--
-- TOC entry 260 (class 1259 OID 17580)
-- Name: muc_room_history_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.muc_room_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.muc_room_history_id_seq OWNER TO ejabberd;

--
-- TOC entry 4943 (class 0 OID 0)
-- Dependencies: 260
-- Name: muc_room_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.muc_room_history_id_seq OWNED BY public.muc_room_history.id;


--
-- TOC entry 261 (class 1259 OID 17582)
-- Name: muc_room_users_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.muc_room_users_id_seq
    START WITH 1542632
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.muc_room_users_id_seq OWNER TO ejabberd;

--
-- TOC entry 262 (class 1259 OID 17584)
-- Name: muc_room_users; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_room_users (
    muc_name character varying(200) NOT NULL,
    username character varying(200) NOT NULL,
    host character varying(200) NOT NULL,
    subscribe_flag text DEFAULT '0'::text NOT NULL,
    id bigint DEFAULT nextval('public.muc_room_users_id_seq'::regclass) NOT NULL,
    date bigint DEFAULT 0,
    login_date integer,
    domain text DEFAULT 'conference.ejabhost1'::text NOT NULL,
    update_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.muc_room_users OWNER TO ejabberd;

--
-- TOC entry 4944 (class 0 OID 0)
-- Dependencies: 262
-- Name: COLUMN muc_room_users.update_time; Type: COMMENT; Schema: public; Owner: ejabberd
--

COMMENT ON COLUMN public.muc_room_users.update_time IS '更新时间';



--
-- TOC entry 266 (class 1259 OID 17614)
-- Name: muc_user_mark; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_user_mark (
    muc_name text NOT NULL,
    user_name text NOT NULL,
    login_date integer NOT NULL,
    logout_date integer,
    id bigint NOT NULL
);


ALTER TABLE public.muc_user_mark OWNER TO ejabberd;

--
-- TOC entry 267 (class 1259 OID 17620)
-- Name: muc_user_mark_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.muc_user_mark_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.muc_user_mark_id_seq OWNER TO ejabberd;

--
-- TOC entry 4946 (class 0 OID 0)
-- Dependencies: 267
-- Name: muc_user_mark_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.muc_user_mark_id_seq OWNED BY public.muc_user_mark.id;


--
-- TOC entry 268 (class 1259 OID 17622)
-- Name: muc_vcard_info; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.muc_vcard_info (
    muc_name text NOT NULL,
    show_name text NOT NULL,
    muc_desc text,
    muc_title text,
    muc_pic text DEFAULT '/file/v2/download/eb574c5a1d33c72ba14fc1616cde3a42.png'::text,
    show_name_pinyin varchar(1000) DEFAULT 'xinjianqunliao|xjql',
    update_time timestamp with time zone DEFAULT (now())::timestamp(3) with time zone NOT NULL,
    version integer DEFAULT 1
);


ALTER TABLE public.muc_vcard_info OWNER TO ejabberd;


--
-- TOC entry 273 (class 1259 OID 17651)
-- Name: notice_history; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.notice_history (
    id bigint NOT NULL,
    msg_id text NOT NULL,
    m_from text NOT NULL,
    m_body text NOT NULL,
    create_time timestamp with time zone DEFAULT (now())::timestamp(3) NOT NULL,
    host text DEFAULT 'ejabhost1'::text NOT NULL
);


ALTER TABLE public.notice_history OWNER TO ejabberd;

--
-- TOC entry 274 (class 1259 OID 17659)
-- Name: notice_history_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.notice_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.notice_history_id_seq OWNER TO ejabberd;

--
-- TOC entry 4950 (class 0 OID 0)
-- Dependencies: 274
-- Name: notice_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.notice_history_id_seq OWNED BY public.notice_history.id;


--
-- TOC entry 282 (class 1259 OID 17715)
-- Name: privacy_default_list; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.privacy_default_list (
    username text NOT NULL,
    name text NOT NULL
);


ALTER TABLE public.privacy_default_list OWNER TO ejabberd;

--
-- TOC entry 283 (class 1259 OID 17721)
-- Name: privacy_list; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.privacy_list (
    username text NOT NULL,
    name text NOT NULL,
    id integer NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.privacy_list OWNER TO ejabberd;

--
-- TOC entry 284 (class 1259 OID 17728)
-- Name: privacy_list_data; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.privacy_list_data (
    id bigint,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord numeric NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
);


ALTER TABLE public.privacy_list_data OWNER TO ejabberd;

--
-- TOC entry 285 (class 1259 OID 17734)
-- Name: privacy_list_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.privacy_list_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.privacy_list_id_seq OWNER TO ejabberd;

--
-- TOC entry 4955 (class 0 OID 0)
-- Dependencies: 285
-- Name: privacy_list_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.privacy_list_id_seq OWNED BY public.privacy_list.id;


--
-- TOC entry 286 (class 1259 OID 17736)
-- Name: private_storage; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.private_storage (
    username text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.private_storage OWNER TO ejabberd;

--
-- TOC entry 287 (class 1259 OID 17743)
-- Name: pubsub_item; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.pubsub_item (
    nodeid bigint,
    itemid text,
    publisher text,
    creation text,
    modification text,
    payload text
);


ALTER TABLE public.pubsub_item OWNER TO ejabberd;

--
-- TOC entry 288 (class 1259 OID 17749)
-- Name: pubsub_node; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.pubsub_node (
    host text,
    node text,
    parent text,
    type text,
    nodeid integer NOT NULL
);


ALTER TABLE public.pubsub_node OWNER TO ejabberd;

--
-- TOC entry 289 (class 1259 OID 17755)
-- Name: pubsub_node_nodeid_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.pubsub_node_nodeid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pubsub_node_nodeid_seq OWNER TO ejabberd;

--
-- TOC entry 4956 (class 0 OID 0)
-- Dependencies: 289
-- Name: pubsub_node_nodeid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.pubsub_node_nodeid_seq OWNED BY public.pubsub_node.nodeid;


--
-- TOC entry 290 (class 1259 OID 17757)
-- Name: pubsub_node_option; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.pubsub_node_option (
    nodeid bigint,
    name text,
    val text
);


ALTER TABLE public.pubsub_node_option OWNER TO ejabberd;

--
-- TOC entry 291 (class 1259 OID 17763)
-- Name: pubsub_node_owner; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.pubsub_node_owner (
    nodeid bigint,
    owner text
);


ALTER TABLE public.pubsub_node_owner OWNER TO ejabberd;

--
-- TOC entry 292 (class 1259 OID 17769)
-- Name: pubsub_state; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.pubsub_state (
    nodeid bigint,
    jid text,
    affiliation character(1),
    subscriptions text,
    stateid integer NOT NULL
);


ALTER TABLE public.pubsub_state OWNER TO ejabberd;

--
-- TOC entry 293 (class 1259 OID 17775)
-- Name: pubsub_state_stateid_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.pubsub_state_stateid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pubsub_state_stateid_seq OWNER TO ejabberd;

--
-- TOC entry 4957 (class 0 OID 0)
-- Dependencies: 293
-- Name: pubsub_state_stateid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.pubsub_state_stateid_seq OWNED BY public.pubsub_state.stateid;


--
-- TOC entry 294 (class 1259 OID 17777)
-- Name: pubsub_subscription_opt; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.pubsub_subscription_opt (
    subid text,
    opt_name character varying(32),
    opt_value text
);


ALTER TABLE public.pubsub_subscription_opt OWNER TO ejabberd;

--
-- TOC entry 295 (class 1259 OID 17783)
-- Name: push_info; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.push_info (
    id integer NOT NULL,
    user_name character varying(255) NOT NULL,
    host character varying(255) NOT NULL,
    mac_key character varying(255),
    platname character varying(255),
    pkgname character varying(255),
    os character varying(50) NOT NULL,
    version character varying(50),
    create_time timestamp with time zone DEFAULT now(),
    update_at timestamp with time zone DEFAULT now(),
    show_content smallint DEFAULT 1,
    push_flag integer DEFAULT 0
);


ALTER TABLE public.push_info OWNER TO ejabberd;

--
-- TOC entry 4958 (class 0 OID 0)
-- Dependencies: 295
-- Name: TABLE push_info; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.push_info IS '存储用户推送信息表';


--
-- TOC entry 4959 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.user_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.user_name IS '用户用户名';


--
-- TOC entry 4960 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.host; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.host IS '用户域名';


--
-- TOC entry 4961 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.mac_key; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.mac_key IS '用户上传唯一key,给指定key发送push';


--
-- TOC entry 4962 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.platname; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.platname IS 'adr端平台名称，如mipush';


--
-- TOC entry 4963 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.pkgname; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.pkgname IS 'adr端应用包名';


--
-- TOC entry 4964 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.os; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.os IS '平台系统 ios或android';


--
-- TOC entry 4965 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.version; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.version IS '版本号';


--
-- TOC entry 4966 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.create_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.create_time IS '创建时间';


--
-- TOC entry 4967 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.update_at; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.update_at IS '更新时间';


--
-- TOC entry 4968 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.show_content; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.show_content IS '是否显示推送详细内容，默认值1：显示，0：不显示';


--
-- TOC entry 4969 (class 0 OID 0)
-- Dependencies: 295
-- Name: COLUMN push_info.push_flag; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.push_info.push_flag IS '是否push标志位';


--
-- TOC entry 296 (class 1259 OID 17790)
-- Name: push_info_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.push_info_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.push_info_id_seq OWNER TO ejabberd;

--
-- TOC entry 4970 (class 0 OID 0)
-- Dependencies: 296
-- Name: push_info_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.push_info_id_seq OWNED BY public.push_info.id;


--
-- TOC entry 297 (class 1259 OID 17792)
-- Name: qcloud_main; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.qcloud_main (
    id integer NOT NULL,
    q_user character varying(255),
    q_type integer,
    q_title text,
    q_introduce text,
    q_content text,
    q_time bigint,
    q_state integer DEFAULT 1
);


ALTER TABLE public.qcloud_main OWNER TO ejabberd;

--
-- TOC entry 4971 (class 0 OID 0)
-- Dependencies: 297
-- Name: TABLE qcloud_main; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.qcloud_main IS 'Evernote主列表存储表';


--
-- TOC entry 4972 (class 0 OID 0)
-- Dependencies: 297
-- Name: COLUMN qcloud_main.q_user; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main.q_user IS '信息所属用户';


--
-- TOC entry 4973 (class 0 OID 0)
-- Dependencies: 297
-- Name: COLUMN qcloud_main.q_type; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main.q_type IS '信息类型';


--
-- TOC entry 4974 (class 0 OID 0)
-- Dependencies: 297
-- Name: COLUMN qcloud_main.q_title; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main.q_title IS '标题';


--
-- TOC entry 4975 (class 0 OID 0)
-- Dependencies: 297
-- Name: COLUMN qcloud_main.q_introduce; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main.q_introduce IS '介绍';


--
-- TOC entry 4976 (class 0 OID 0)
-- Dependencies: 297
-- Name: COLUMN qcloud_main.q_content; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main.q_content IS '内容';


--
-- TOC entry 4977 (class 0 OID 0)
-- Dependencies: 297
-- Name: COLUMN qcloud_main.q_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main.q_time IS '最后修改时间';


--
-- TOC entry 4978 (class 0 OID 0)
-- Dependencies: 297
-- Name: COLUMN qcloud_main.q_state; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main.q_state IS '信息状态标记(正常、收藏、废纸篓、删除)';


--
-- TOC entry 298 (class 1259 OID 17799)
-- Name: qcloud_main_history; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.qcloud_main_history (
    id integer NOT NULL,
    q_id integer,
    qh_content text,
    qh_time bigint,
    qh_state integer DEFAULT 1
);


ALTER TABLE public.qcloud_main_history OWNER TO ejabberd;

--
-- TOC entry 4980 (class 0 OID 0)
-- Dependencies: 298
-- Name: TABLE qcloud_main_history; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.qcloud_main_history IS 'Evernote主列表操作历史';


--
-- TOC entry 4981 (class 0 OID 0)
-- Dependencies: 298
-- Name: COLUMN qcloud_main_history.q_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main_history.q_id IS '主列表ID';


--
-- TOC entry 4982 (class 0 OID 0)
-- Dependencies: 298
-- Name: COLUMN qcloud_main_history.qh_content; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main_history.qh_content IS '操作内容';


--
-- TOC entry 4983 (class 0 OID 0)
-- Dependencies: 298
-- Name: COLUMN qcloud_main_history.qh_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main_history.qh_time IS '操作时间';


--
-- TOC entry 4984 (class 0 OID 0)
-- Dependencies: 298
-- Name: COLUMN qcloud_main_history.qh_state; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_main_history.qh_state IS '状态';


--
-- TOC entry 299 (class 1259 OID 17806)
-- Name: qcloud_main_history_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.qcloud_main_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.qcloud_main_history_id_seq OWNER TO ejabberd;

--
-- TOC entry 4986 (class 0 OID 0)
-- Dependencies: 299
-- Name: qcloud_main_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.qcloud_main_history_id_seq OWNED BY public.qcloud_main_history.id;


--
-- TOC entry 300 (class 1259 OID 17808)
-- Name: qcloud_main_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.qcloud_main_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.qcloud_main_id_seq OWNER TO ejabberd;

--
-- TOC entry 4988 (class 0 OID 0)
-- Dependencies: 300
-- Name: qcloud_main_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.qcloud_main_id_seq OWNED BY public.qcloud_main.id;


--
-- TOC entry 301 (class 1259 OID 17810)
-- Name: qcloud_sub; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.qcloud_sub (
    id integer NOT NULL,
    q_id bigint NOT NULL,
    qs_user character varying(255),
    qs_type integer,
    qs_title text,
    qs_introduce text,
    qs_content text,
    qs_time bigint,
    qs_state integer DEFAULT 1
);


ALTER TABLE public.qcloud_sub OWNER TO ejabberd;

--
-- TOC entry 4990 (class 0 OID 0)
-- Dependencies: 301
-- Name: TABLE qcloud_sub; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.qcloud_sub IS 'Evernote子列表存储表';


--
-- TOC entry 4991 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.q_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.q_id IS '主列表ID';


--
-- TOC entry 4992 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.qs_user; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.qs_user IS '信息所属用户';


--
-- TOC entry 4993 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.qs_type; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.qs_type IS '信息类型';


--
-- TOC entry 4994 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.qs_title; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.qs_title IS '标题';


--
-- TOC entry 4995 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.qs_introduce; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.qs_introduce IS '介绍';


--
-- TOC entry 4996 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.qs_content; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.qs_content IS '内容';


--
-- TOC entry 4997 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.qs_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.qs_time IS '最后修改时间';


--
-- TOC entry 4998 (class 0 OID 0)
-- Dependencies: 301
-- Name: COLUMN qcloud_sub.qs_state; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub.qs_state IS '信息状态标记(正常、收藏、废纸篓、删除)';


--
-- TOC entry 302 (class 1259 OID 17817)
-- Name: qcloud_sub_history; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.qcloud_sub_history (
    id integer NOT NULL,
    qs_id integer,
    qh_content text,
    qh_time bigint,
    qh_state integer DEFAULT 1
);


ALTER TABLE public.qcloud_sub_history OWNER TO ejabberd;

--
-- TOC entry 5000 (class 0 OID 0)
-- Dependencies: 302
-- Name: TABLE qcloud_sub_history; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.qcloud_sub_history IS 'Evernote子列表操作历史';


--
-- TOC entry 5001 (class 0 OID 0)
-- Dependencies: 302
-- Name: COLUMN qcloud_sub_history.qs_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub_history.qs_id IS '主列表ID';


--
-- TOC entry 5002 (class 0 OID 0)
-- Dependencies: 302
-- Name: COLUMN qcloud_sub_history.qh_content; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub_history.qh_content IS '操作内容';


--
-- TOC entry 5003 (class 0 OID 0)
-- Dependencies: 302
-- Name: COLUMN qcloud_sub_history.qh_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub_history.qh_time IS '操作时间';


--
-- TOC entry 5004 (class 0 OID 0)
-- Dependencies: 302
-- Name: COLUMN qcloud_sub_history.qh_state; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qcloud_sub_history.qh_state IS '状态';


--
-- TOC entry 303 (class 1259 OID 17824)
-- Name: qcloud_sub_history_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.qcloud_sub_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.qcloud_sub_history_id_seq OWNER TO ejabberd;

--
-- TOC entry 5006 (class 0 OID 0)
-- Dependencies: 303
-- Name: qcloud_sub_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.qcloud_sub_history_id_seq OWNED BY public.qcloud_sub_history.id;


--
-- TOC entry 304 (class 1259 OID 17826)
-- Name: qcloud_sub_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.qcloud_sub_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.qcloud_sub_id_seq OWNER TO ejabberd;

--
-- TOC entry 5008 (class 0 OID 0)
-- Dependencies: 304
-- Name: qcloud_sub_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.qcloud_sub_id_seq OWNED BY public.qcloud_sub.id;


--
-- TOC entry 305 (class 1259 OID 17828)
-- Name: qtalk_user_comment; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.qtalk_user_comment (
    id bigint NOT NULL,
    from_user character varying(20) NOT NULL,
    to_user character varying(20) NOT NULL,
    create_time timestamp with time zone NOT NULL,
    comment character varying(500),
    grade boolean NOT NULL
);


ALTER TABLE public.qtalk_user_comment OWNER TO ejabberd;

--
-- TOC entry 5010 (class 0 OID 0)
-- Dependencies: 305
-- Name: COLUMN qtalk_user_comment.from_user; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qtalk_user_comment.from_user IS '点评人';


--
-- TOC entry 5011 (class 0 OID 0)
-- Dependencies: 305
-- Name: COLUMN qtalk_user_comment.to_user; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qtalk_user_comment.to_user IS '被点评人';


--
-- TOC entry 5012 (class 0 OID 0)
-- Dependencies: 305
-- Name: COLUMN qtalk_user_comment.create_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qtalk_user_comment.create_time IS '点评时间';


--
-- TOC entry 5013 (class 0 OID 0)
-- Dependencies: 305
-- Name: COLUMN qtalk_user_comment.comment; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qtalk_user_comment.comment IS '点评内容';


--
-- TOC entry 5014 (class 0 OID 0)
-- Dependencies: 305
-- Name: COLUMN qtalk_user_comment.grade; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.qtalk_user_comment.grade IS 'true 赞 fasle 扁';


--
-- TOC entry 306 (class 1259 OID 17831)
-- Name: qtalk_user_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.qtalk_user_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.qtalk_user_comment_id_seq OWNER TO ejabberd;

--
-- TOC entry 5015 (class 0 OID 0)
-- Dependencies: 306
-- Name: qtalk_user_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.qtalk_user_comment_id_seq OWNED BY public.qtalk_user_comment.id;


--
-- TOC entry 307 (class 1259 OID 17833)
-- Name: recv_msg_option; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.recv_msg_option (
    username text NOT NULL,
    rec_msg_opt smallint DEFAULT (2)::smallint,
    version integer
);


ALTER TABLE public.recv_msg_option OWNER TO ejabberd;

--
-- TOC entry 308 (class 1259 OID 17840)
-- Name: revoke_msg_history; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.revoke_msg_history (
    m_from text,
    m_to text,
    m_body text,
    id bigint NOT NULL,
    m_timestamp bigint DEFAULT date_part('epoch'::text, now()),
    msg_id text,
    create_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.revoke_msg_history OWNER TO ejabberd;

--
-- TOC entry 309 (class 1259 OID 17848)
-- Name: revoke_msg_history_backup; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.revoke_msg_history_backup (
    m_from text,
    m_to text,
    m_body text,
    id bigint NOT NULL,
    m_timestamp bigint DEFAULT date_part('epoch'::text, now()),
    msg_id text,
    create_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.revoke_msg_history_backup OWNER TO ejabberd;

--
-- TOC entry 310 (class 1259 OID 17856)
-- Name: revoke_msg_history_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.revoke_msg_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.revoke_msg_history_id_seq OWNER TO ejabberd;

--
-- TOC entry 5017 (class 0 OID 0)
-- Dependencies: 310
-- Name: revoke_msg_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.revoke_msg_history_id_seq OWNED BY public.revoke_msg_history.id;


--
-- TOC entry 320 (class 1259 OID 17929)
-- Name: s2s_mapped_host; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.s2s_mapped_host (
    domain character varying(255) NOT NULL,
    host character varying(255) NOT NULL,
    port integer,
    priority character varying(255) NOT NULL,
    weight character varying(255) NOT NULL
);


ALTER TABLE public.s2s_mapped_host OWNER TO ejabberd;

--
-- TOC entry 321 (class 1259 OID 17932)
-- Name: scheduling_info; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.scheduling_info (
    id integer NOT NULL,
    scheduling_id character varying(255) NOT NULL,
    scheduling_name character varying(255) NOT NULL,
    scheduling_type character varying(255) DEFAULT '0'::character varying,
    scheduling_remarks text DEFAULT ''::text,
    scheduling_intr text DEFAULT ''::text,
    scheduling_appointment text DEFAULT ''::text,
    scheduling_locale character varying(255) NOT NULL,
    scheduling_locale_id character varying(255) NOT NULL,
    scheduling_room character varying(255) NOT NULL,
    scheduling_room_id character varying(255) NOT NULL,
    schedule_time timestamp with time zone DEFAULT (now())::timestamp(3) with time zone,
    scheduling_date date NOT NULL,
    begin_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone NOT NULL,
    inviter character varying(255) NOT NULL,
    member character varying(255) NOT NULL,
    mem_action character varying(255) DEFAULT '0'::character varying,
    remind_flag character varying(255) DEFAULT '0'::character varying,
    action_remark text DEFAULT ''::text,
    canceled boolean DEFAULT false,
    update_time timestamp with time zone DEFAULT (now())::timestamp(3) with time zone
);


ALTER TABLE public.scheduling_info OWNER TO ejabberd;

--
-- TOC entry 5019 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_id IS '行程ID';


--
-- TOC entry 5020 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_name IS '行程名字';


--
-- TOC entry 5021 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_type; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_type IS '行程类型，1是会议，2是约会';


--
-- TOC entry 5022 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_remarks; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_remarks IS '行程备注';


--
-- TOC entry 5023 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_intr; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_intr IS '行程简介';


--
-- TOC entry 5024 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_appointment; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_appointment IS '行程介绍';


--
-- TOC entry 5025 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_locale; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_locale IS '行程地点';


--
-- TOC entry 5026 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_locale_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_locale_id IS '行程地点编号';


--
-- TOC entry 5027 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_room; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_room IS '行程房间';


--
-- TOC entry 5028 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_room_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_room_id IS '行程房间编号';


--
-- TOC entry 5029 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.schedule_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.schedule_time IS '行程约定的时间';


--
-- TOC entry 5030 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.scheduling_date; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.scheduling_date IS '行程日期';


--
-- TOC entry 5031 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.begin_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.begin_time IS '行程开始时间';


--
-- TOC entry 5032 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.end_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.end_time IS '行程结束时间';


--
-- TOC entry 5033 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.inviter; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.inviter IS '行程邀请者';


--
-- TOC entry 5034 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.member; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.member IS '行程参与者';


--
-- TOC entry 5035 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.mem_action; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.mem_action IS '行程参与者接受状态';


--
-- TOC entry 5036 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.remind_flag; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.remind_flag IS '行程参与者提醒标志，0未提醒，1开始前15分钟已提醒，2结束前15分钟已提醒';


--
-- TOC entry 5037 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.action_remark; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.action_remark IS '行程参与者接受时的备注';


--
-- TOC entry 5038 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.canceled; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.canceled IS '行程是否被取消';


--
-- TOC entry 5039 (class 0 OID 0)
-- Dependencies: 321
-- Name: COLUMN scheduling_info.update_time; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.scheduling_info.update_time IS '行程更新时间';


--
-- TOC entry 322 (class 1259 OID 17948)
-- Name: scheduling_info_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.scheduling_info_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.scheduling_info_id_seq OWNER TO ejabberd;

--
-- TOC entry 5041 (class 0 OID 0)
-- Dependencies: 322
-- Name: scheduling_info_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.scheduling_info_id_seq OWNED BY public.scheduling_info.id;


--
-- TOC entry 335 (class 1259 OID 18021)
-- Name: user_friends; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.user_friends (
    username text NOT NULL,
    friend text NOT NULL,
    relationship smallint,
    version integer,
    host text DEFAULT 'ejabhost1'::text NOT NULL,
    userhost character varying(255) DEFAULT 'ejabhost1'::character varying
);


ALTER TABLE public.user_friends OWNER TO ejabberd;


--
-- TOC entry 338 (class 1259 OID 18044)
-- Name: user_register_mucs; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.user_register_mucs (
    username text NOT NULL,
    muc_name text NOT NULL,
    domain character varying(200) NOT NULL,
    created_at timestamp with time zone DEFAULT (now())::timestamp(3) with time zone NOT NULL,
    registed_flag integer DEFAULT 1,
    host text DEFAULT 'ejabhost1'::text NOT NULL
);


ALTER TABLE public.user_register_mucs OWNER TO ejabberd;

--
-- TOC entry 339 (class 1259 OID 18053)
-- Name: user_register_mucs_backup; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.user_register_mucs_backup (
    username text NOT NULL,
    muc_name text NOT NULL,
    domain character varying(200) NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    registed_flag integer DEFAULT 1
);


ALTER TABLE public.user_register_mucs_backup OWNER TO ejabberd;

--
-- TOC entry 340 (class 1259 OID 18061)
-- Name: user_relation_opts; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.user_relation_opts (
    username text NOT NULL,
    rec_msg_opt smallint DEFAULT (2)::smallint,
    vld_friend_opt smallint DEFAULT (1)::smallint,
    validate_quetion text,
    validate_answer text,
    vesion smallint,
    userhost text DEFAULT 'ejabhost1'::text
);


ALTER TABLE public.user_relation_opts OWNER TO ejabberd;


--
-- TOC entry 344 (class 1259 OID 18085)
-- Name: vcard_version; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.vcard_version (
    username text NOT NULL,
    version integer DEFAULT 1,
    url text DEFAULT '/file/v2/download/8c9d42532be9316e2202ffef8fcfeba5.png'::text,
    uin character varying(30),
    id bigint NOT NULL,
    profile_version smallint DEFAULT (1)::smallint,
    mood text,
    gender integer DEFAULT 0 NOT NULL,
    host text DEFAULT 'ejabhost1'::text
);


ALTER TABLE public.vcard_version OWNER TO ejabberd;

--
-- TOC entry 345 (class 1259 OID 18095)
-- Name: vcard_version_id_seq; Type: SEQUENCE; Schema: public; Owner: ejabberd
--

CREATE SEQUENCE public.vcard_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.vcard_version_id_seq OWNER TO ejabberd;

--
-- TOC entry 5047 (class 0 OID 0)
-- Dependencies: 345
-- Name: vcard_version_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ejabberd
--

ALTER SEQUENCE public.vcard_version_id_seq OWNED BY public.vcard_version.id;


--
-- TOC entry 350 (class 1259 OID 18123)
-- Name: warn_msg_history; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.warn_msg_history (
    id bigint NOT NULL,
    m_from character varying(255),
    m_to character varying(255),
    read_flag smallint DEFAULT 0,
    msg_id text,
    from_host text,
    to_host text,
    m_body text,
    create_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.warn_msg_history OWNER TO ejabberd;

--
-- TOC entry 351 (class 1259 OID 18131)
-- Name: warn_msg_history_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.warn_msg_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.warn_msg_history_id_seq OWNER TO ejabberd;

--
-- TOC entry 5051 (class 0 OID 0)
-- Dependencies: 351
-- Name: warn_msg_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.warn_msg_history_id_seq OWNED BY public.warn_msg_history.id;


--
-- TOC entry 352 (class 1259 OID 18133)
-- Name: warn_msg_history_backup; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.warn_msg_history_backup (
    id bigint DEFAULT nextval('public.warn_msg_history_id_seq'::regclass) NOT NULL,
    m_from character varying(255),
    m_to character varying(255),
    read_flag smallint DEFAULT 0,
    msg_id text,
    from_host text,
    to_host text,
    m_body text,
    create_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.warn_msg_history_backup OWNER TO ejabberd;

--
-- TOC entry 353 (class 1259 OID 18142)
-- Name: white_list; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.white_list (
    username text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    single_flag text DEFAULT '1'::text
);


ALTER TABLE public.white_list OWNER TO ejabberd;


--
-- TOC entry 3842 (class 2604 OID 18166)
-- Name: client_config_sync id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_config_sync ALTER COLUMN id SET DEFAULT nextval('public.client_config_sync_id_seq'::regclass);


--
-- TOC entry 3844 (class 2604 OID 18167)
-- Name: destroy_muc_info id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.destroy_muc_info ALTER COLUMN id SET DEFAULT nextval('public.destroy_muc_info_id_seq'::regclass);


--
-- TOC entry 3856 (class 2604 OID 18169)
-- Name: fresh_empl_entering id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.fresh_empl_entering ALTER COLUMN id SET DEFAULT nextval('public.fresh_empl_entering_id_seq'::regclass);


--
-- TOC entry 3862 (class 2604 OID 18171)
-- Name: host_info id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.host_info ALTER COLUMN id SET DEFAULT nextval('public.host_info_id_seq'::regclass);


--
-- TOC entry 3874 (class 2604 OID 18172)
-- Name: host_users id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.host_users ALTER COLUMN id SET DEFAULT nextval('public.host_users_id_seq'::regclass);


--
-- TOC entry 3905 (class 2604 OID 18174)
-- Name: meeting_info id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.meeting_info ALTER COLUMN id SET DEFAULT nextval('public.meeting_info_id_seq'::regclass);


--
-- TOC entry 3927 (class 2604 OID 18175)
-- Name: msg_history id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.msg_history ALTER COLUMN id SET DEFAULT nextval('public.msg_history_id_seq'::regclass);


--
-- TOC entry 3939 (class 2604 OID 18177)
-- Name: muc_room_history id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_room_history ALTER COLUMN id SET DEFAULT nextval('public.muc_room_history_id_seq'::regclass);


--
-- TOC entry 3952 (class 2604 OID 18179)
-- Name: muc_user_mark id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_user_mark ALTER COLUMN id SET DEFAULT nextval('public.muc_user_mark_id_seq'::regclass);


--
-- TOC entry 3959 (class 2604 OID 18181)
-- Name: notice_history id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.notice_history ALTER COLUMN id SET DEFAULT nextval('public.notice_history_id_seq'::regclass);


--
-- TOC entry 3978 (class 2604 OID 18183)
-- Name: privacy_list id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.privacy_list ALTER COLUMN id SET DEFAULT nextval('public.privacy_list_id_seq'::regclass);


--
-- TOC entry 3980 (class 2604 OID 18184)
-- Name: pubsub_node nodeid; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_node ALTER COLUMN nodeid SET DEFAULT nextval('public.pubsub_node_nodeid_seq'::regclass);


--
-- TOC entry 3981 (class 2604 OID 18185)
-- Name: pubsub_state stateid; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_state ALTER COLUMN stateid SET DEFAULT nextval('public.pubsub_state_stateid_seq'::regclass);


--
-- TOC entry 3986 (class 2604 OID 18186)
-- Name: push_info id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.push_info ALTER COLUMN id SET DEFAULT nextval('public.push_info_id_seq'::regclass);


--
-- TOC entry 3988 (class 2604 OID 18187)
-- Name: qcloud_main id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_main ALTER COLUMN id SET DEFAULT nextval('public.qcloud_main_id_seq'::regclass);


--
-- TOC entry 3990 (class 2604 OID 18188)
-- Name: qcloud_main_history id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_main_history ALTER COLUMN id SET DEFAULT nextval('public.qcloud_main_history_id_seq'::regclass);


--
-- TOC entry 3992 (class 2604 OID 18189)
-- Name: qcloud_sub id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_sub ALTER COLUMN id SET DEFAULT nextval('public.qcloud_sub_id_seq'::regclass);


--
-- TOC entry 3994 (class 2604 OID 18190)
-- Name: qcloud_sub_history id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_sub_history ALTER COLUMN id SET DEFAULT nextval('public.qcloud_sub_history_id_seq'::regclass);


--
-- TOC entry 3995 (class 2604 OID 18191)
-- Name: qtalk_user_comment id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qtalk_user_comment ALTER COLUMN id SET DEFAULT nextval('public.qtalk_user_comment_id_seq'::regclass);


--
-- TOC entry 3999 (class 2604 OID 18192)
-- Name: revoke_msg_history id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.revoke_msg_history ALTER COLUMN id SET DEFAULT nextval('public.revoke_msg_history_id_seq'::regclass);


--
-- TOC entry 4032 (class 2604 OID 18193)
-- Name: scheduling_info id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scheduling_info ALTER COLUMN id SET DEFAULT nextval('public.scheduling_info_id_seq'::regclass);



--
-- TOC entry 4065 (class 2604 OID 18198)
-- Name: vcard_version id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.vcard_version ALTER COLUMN id SET DEFAULT nextval('public.vcard_version_id_seq'::regclass);


--
-- TOC entry 4074 (class 2604 OID 18199)
-- Name: warn_msg_history id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.warn_msg_history ALTER COLUMN id SET DEFAULT nextval('public.warn_msg_history_id_seq'::regclass);


--
-- TOC entry 4084 (class 2606 OID 18201)
-- Name: admin_user admin_user_pk; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.admin_user
    ADD CONSTRAINT admin_user_pk PRIMARY KEY (username);


--
-- TOC entry 4092 (class 2606 OID 18207)
-- Name: client_config_sync client_config_sync_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.client_config_sync
    ADD CONSTRAINT client_config_sync_pkey PRIMARY KEY (id);



--
-- TOC entry 4112 (class 2606 OID 18211)
-- Name: destroy_muc_info destroy_muc_info_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.destroy_muc_info
    ADD CONSTRAINT destroy_muc_info_pkey PRIMARY KEY (id);


--
-- TOC entry 4128 (class 2606 OID 18223)
-- Name: fresh_empl_entering fresh_empl_entering_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.fresh_empl_entering
    ADD CONSTRAINT fresh_empl_entering_pkey PRIMARY KEY (id);



--
-- TOC entry 4134 (class 2606 OID 18227)
-- Name: host_info host_info_host_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.host_info
    ADD CONSTRAINT host_info_host_key UNIQUE (host);


--
-- TOC entry 4136 (class 2606 OID 18229)
-- Name: host_info host_info_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.host_info
    ADD CONSTRAINT host_info_pkey PRIMARY KEY (id);


--
-- TOC entry 4140 (class 2606 OID 18231)
-- Name: host_users host_users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.host_users
    ADD CONSTRAINT host_users_pkey PRIMARY KEY (id);

--
-- TOC entry 4142 (class 2606 OID 18237)
-- Name: host_users host_users_user_id_host_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.host_users
    ADD CONSTRAINT host_users_user_id_host_id_key UNIQUE (user_id, host_id);


--
-- TOC entry 4162 (class 2606 OID 18239)
-- Name: last last_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.last
    ADD CONSTRAINT last_pkey PRIMARY KEY (username);


--
-- TOC entry 4185 (class 2606 OID 18243)
-- Name: meeting_info meeting_info_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.meeting_info
    ADD CONSTRAINT meeting_info_pkey PRIMARY KEY (id);


--
-- TOC entry 4193 (class 2606 OID 18249)
-- Name: motd motd_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.motd
    ADD CONSTRAINT motd_pkey PRIMARY KEY (username);


--
-- TOC entry 4222 (class 2606 OID 18251)
-- Name: msg_history_backup msg_history_backup_tmp_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.msg_history_backup
    ADD CONSTRAINT msg_history_backup_tmp_pkey PRIMARY KEY (id);


--
-- TOC entry 4230 (class 2606 OID 18255)
-- Name: muc_last muc_last_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_last
    ADD CONSTRAINT muc_last_pkey PRIMARY KEY (muc_name);


--
-- TOC entry 4250 (class 2606 OID 18257)
-- Name: muc_room_history_backup muc_room_history_2016_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.muc_room_history_backup
    ADD CONSTRAINT muc_room_history_2016_pkey PRIMARY KEY (id);


--
-- TOC entry 4247 (class 2606 OID 18259)
-- Name: muc_room_history muc_room_history_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_room_history
    ADD CONSTRAINT muc_room_history_pkey PRIMARY KEY (id);


--
-- TOC entry 4260 (class 2606 OID 18261)
-- Name: muc_room_users muc_room_users_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_room_users
    ADD CONSTRAINT muc_room_users_pkey PRIMARY KEY (id);

--
-- TOC entry 4284 (class 2606 OID 18265)
-- Name: muc_user_mark muc_user_mark_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_user_mark
    ADD CONSTRAINT muc_user_mark_pkey PRIMARY KEY (id);

--
-- TOC entry 4287 (class 2606 OID 18269)
-- Name: muc_vcard_info muc_vcard_info_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_vcard_info
    ADD CONSTRAINT muc_vcard_info_pkey PRIMARY KEY (muc_name);



--
-- TOC entry 4300 (class 2606 OID 18277)
-- Name: notice_history notice_history_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.notice_history
    ADD CONSTRAINT notice_history_pkey PRIMARY KEY (id);


--
-- TOC entry 4116 (class 2606 OID 18291)
-- Name: flogin_user pk_flogin_user; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.flogin_user
    ADD CONSTRAINT pk_flogin_user PRIMARY KEY (username);


--
-- TOC entry 4159 (class 2606 OID 18293)
-- Name: iplimit pk_iplimit; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.iplimit
    ADD CONSTRAINT pk_iplimit PRIMARY KEY (ip, name, priority);


--
-- TOC entry 4436 (class 2606 OID 18295)
-- Name: user_friends pk_user_friends; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_friends
    ADD CONSTRAINT pk_user_friends PRIMARY KEY (username, friend, host);

--
-- TOC entry 4329 (class 2606 OID 18301)
-- Name: privacy_default_list privacy_default_list_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.privacy_default_list
    ADD CONSTRAINT privacy_default_list_pkey PRIMARY KEY (username);


--
-- TOC entry 4333 (class 2606 OID 18303)
-- Name: privacy_list privacy_list_id_key; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.privacy_list
    ADD CONSTRAINT privacy_list_id_key UNIQUE (id);


--
-- TOC entry 4341 (class 2606 OID 18305)
-- Name: pubsub_node pubsub_node_nodeid_key; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_node
    ADD CONSTRAINT pubsub_node_nodeid_key UNIQUE (nodeid);


--
-- TOC entry 4347 (class 2606 OID 18307)
-- Name: pubsub_state pubsub_state_stateid_key; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_state
    ADD CONSTRAINT pubsub_state_stateid_key UNIQUE (stateid);


--
-- TOC entry 4350 (class 2606 OID 18309)
-- Name: push_info push_info_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.push_info
    ADD CONSTRAINT push_info_pkey PRIMARY KEY (id);


--
-- TOC entry 4360 (class 2606 OID 18311)
-- Name: qcloud_main_history qcloud_main_history_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_main_history
    ADD CONSTRAINT qcloud_main_history_pkey PRIMARY KEY (id);


--
-- TOC entry 4358 (class 2606 OID 18313)
-- Name: qcloud_main qcloud_main_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_main
    ADD CONSTRAINT qcloud_main_pkey PRIMARY KEY (id);


--
-- TOC entry 4368 (class 2606 OID 18315)
-- Name: qcloud_sub_history qcloud_sub_history_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_sub_history
    ADD CONSTRAINT qcloud_sub_history_pkey PRIMARY KEY (id);


--
-- TOC entry 4365 (class 2606 OID 18317)
-- Name: qcloud_sub qcloud_sub_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qcloud_sub
    ADD CONSTRAINT qcloud_sub_pkey PRIMARY KEY (id);


--
-- TOC entry 4372 (class 2606 OID 18319)
-- Name: qtalk_user_comment qtalk_user_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qtalk_user_comment
    ADD CONSTRAINT qtalk_user_comment_pkey PRIMARY KEY (id);


--
-- TOC entry 4376 (class 2606 OID 18321)
-- Name: recv_msg_option recv_msg_option_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.recv_msg_option
    ADD CONSTRAINT recv_msg_option_pkey PRIMARY KEY (username);


--
-- TOC entry 4383 (class 2606 OID 18323)
-- Name: revoke_msg_history_backup revoke_msg_history_backup_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.revoke_msg_history_backup
    ADD CONSTRAINT revoke_msg_history_backup_pkey PRIMARY KEY (id);


--
-- TOC entry 4380 (class 2606 OID 18325)
-- Name: revoke_msg_history revoke_msg_history_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.revoke_msg_history
    ADD CONSTRAINT revoke_msg_history_pkey PRIMARY KEY (id);


--
-- TOC entry 4405 (class 2606 OID 18337)
-- Name: s2s_mapped_host s2s_mapped_host_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.s2s_mapped_host
    ADD CONSTRAINT s2s_mapped_host_pkey PRIMARY KEY (domain, host);


--
-- TOC entry 4411 (class 2606 OID 18339)
-- Name: scheduling_info scheduling_info_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.scheduling_info
    ADD CONSTRAINT scheduling_info_pkey PRIMARY KEY (id);


--
-- TOC entry 4453 (class 2606 OID 18353)
-- Name: user_register_mucs_backup user_register_mucs_backup_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_register_mucs_backup
    ADD CONSTRAINT user_register_mucs_backup_pkey PRIMARY KEY (muc_name, username);


--
-- TOC entry 4444 (class 2606 OID 18355)
-- Name: user_register_mucs user_register_mucs_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_register_mucs
    ADD CONSTRAINT user_register_mucs_pkey PRIMARY KEY (muc_name, username);


--
-- TOC entry 4459 (class 2606 OID 18357)
-- Name: user_relation_opts user_relation_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_relation_opts
    ADD CONSTRAINT user_relation_pkey PRIMARY KEY (username);


--
-- TOC entry 4476 (class 2606 OID 18365)
-- Name: vcard_version vcard_version_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.vcard_version
    ADD CONSTRAINT vcard_version_pkey PRIMARY KEY (id);


--
-- TOC entry 4505 (class 2606 OID 18371)
-- Name: warn_msg_history_backup warn_msg_history_backup_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.warn_msg_history_backup
    ADD CONSTRAINT warn_msg_history_backup_pkey PRIMARY KEY (id);


--
-- TOC entry 4494 (class 2606 OID 18373)
-- Name: warn_msg_history warn_msg_history_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.warn_msg_history
    ADD CONSTRAINT warn_msg_history_pkey PRIMARY KEY (id);


--
-- TOC entry 4507 (class 2606 OID 18375)
-- Name: white_list white_list_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.white_list
    ADD CONSTRAINT white_list_pkey PRIMARY KEY (username);



--
-- TOC entry 4088 (class 1259 OID 18385)
-- Name: client_config_sync_configkey__idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_configkey__idx ON public.client_config_sync USING btree (configkey);


--
-- TOC entry 4089 (class 1259 OID 18386)
-- Name: client_config_sync_host__idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_host__idx ON public.client_config_sync USING btree (host);


--
-- TOC entry 4090 (class 1259 OID 18387)
-- Name: client_config_sync_operate_plat__idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_operate_plat__idx ON public.client_config_sync USING btree (operate_plat);


--
-- TOC entry 4093 (class 1259 OID 18388)
-- Name: client_config_sync_update_time__idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_update_time__idx ON public.client_config_sync USING btree (update_time);


--
-- TOC entry 4094 (class 1259 OID 18389)
-- Name: client_config_sync_username__idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username__idx ON public.client_config_sync USING btree (username);


--
-- TOC entry 4095 (class 1259 OID 18390)
-- Name: client_config_sync_username_host_configkey_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username_host_configkey_idx ON public.client_config_sync USING btree (username, host, configkey);


--
-- TOC entry 4096 (class 1259 OID 18391)
-- Name: client_config_sync_username_host_configkey_subkey_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX client_config_sync_username_host_configkey_subkey_idx ON public.client_config_sync USING btree (username, host, configkey, subkey);


--
-- TOC entry 4097 (class 1259 OID 18392)
-- Name: client_config_sync_username_host_configkey_subkey_version_i_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username_host_configkey_subkey_version_i_idx ON public.client_config_sync USING btree (username, host, configkey, subkey, version, isdel);


--
-- TOC entry 4098 (class 1259 OID 18393)
-- Name: client_config_sync_username_host_configkey_subkey_version_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username_host_configkey_subkey_version_idx ON public.client_config_sync USING btree (username, host, configkey, subkey, version);


--
-- TOC entry 4099 (class 1259 OID 18394)
-- Name: client_config_sync_username_host_configkey_subkey_version_isdel; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username_host_configkey_subkey_version_isdel ON public.client_config_sync USING btree (username, host, configkey, subkey, version, isdel);


--
-- TOC entry 4100 (class 1259 OID 18395)
-- Name: client_config_sync_username_host_configkey_version_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username_host_configkey_version_idx ON public.client_config_sync USING btree (username, host, configkey, version);


--
-- TOC entry 4101 (class 1259 OID 18396)
-- Name: client_config_sync_username_host_configkey_version_isdel_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username_host_configkey_version_isdel_idx ON public.client_config_sync USING btree (username, host, configkey, version, isdel);


--
-- TOC entry 4102 (class 1259 OID 18397)
-- Name: client_config_sync_username_host_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_username_host_idx ON public.client_config_sync USING btree (username, host);


--
-- TOC entry 4103 (class 1259 OID 18398)
-- Name: client_config_sync_version__idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX client_config_sync_version__idx ON public.client_config_sync USING btree (version);


--
-- TOC entry 4106 (class 1259 OID 18399)
-- Name: destroy_muc_info_created_at_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX destroy_muc_info_created_at_idx ON public.destroy_muc_info USING btree (created_at);


--
-- TOC entry 4107 (class 1259 OID 18400)
-- Name: destroy_muc_info_muc_index; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX destroy_muc_info_muc_index ON public.destroy_muc_info USING btree (muc_name);


--
-- TOC entry 4108 (class 1259 OID 18401)
-- Name: destroy_muc_info_muc_name_created_at_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX destroy_muc_info_muc_name_created_at_idx ON public.destroy_muc_info USING btree (muc_name, created_at);


--
-- TOC entry 4109 (class 1259 OID 18402)
-- Name: destroy_muc_info_nick_name_created_at_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX destroy_muc_info_nick_name_created_at_idx ON public.destroy_muc_info USING btree (nick_name, created_at);


--
-- TOC entry 4110 (class 1259 OID 18403)
-- Name: destroy_muc_info_nick_name_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX destroy_muc_info_nick_name_idx ON public.destroy_muc_info USING btree (nick_name);


--
-- TOC entry 4129 (class 1259 OID 18404)
-- Name: fresh_empl_entering_user_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX fresh_empl_entering_user_id_idx ON public.fresh_empl_entering USING btree (user_id);

--
-- TOC entry 4137 (class 1259 OID 18406)
-- Name: host_users_hire_flag_user_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX host_users_hire_flag_user_name_idx ON public.host_users USING btree (hire_flag, user_name);


--
-- TOC entry 4138 (class 1259 OID 18407)
-- Name: host_users_host_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX host_users_host_id_idx ON public.host_users USING btree (host_id);

--
-- TOC entry 4143 (class 1259 OID 18414)
-- Name: host_users_user_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX host_users_user_id_idx ON public.host_users USING btree (user_id);
-- CREATE INDEX host_users_host_id_idx ON public.host_id USING btree (host_id);


--
-- TOC entry 4144 (class 1259 OID 18415)
-- Name: host_users_user_id_user_name_pinyin_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX host_users_user_id_user_name_pinyin_idx ON public.host_users USING gin (user_id public.gin_trgm_ops, user_name public.gin_trgm_ops, pinyin public.gin_trgm_ops) WHERE (hire_flag = 1);


--
-- TOC entry 4145 (class 1259 OID 18416)
-- Name: host_users_user_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX host_users_user_name_idx ON public.host_users USING btree (user_name);
CREATE UNIQUE INDEX host_users_user_id_host_id_unique_index ON public.host_users USING btree (user_id, host_id);
CREATE UNIQUE INDEX host_users_tel_host_id_unique_index ON public.host_users USING btree (tel, host_id);
CREATE UNIQUE INDEX host_users_email_host_id__unique_index ON public.host_users USING btree (email, host_id);


--
-- TOC entry 4146 (class 1259 OID 18417)
-- Name: host_users_version_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX host_users_version_idx ON public.host_users USING btree (version);


--
-- TOC entry 4160 (class 1259 OID 18419)
-- Name: i_irc_custom_jid_host; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_irc_custom_jid_host ON public.irc_custom USING btree (jid, host);


--
-- TOC entry 4231 (class 1259 OID 18420)
-- Name: i_muc_registered_jid_host; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_muc_registered_jid_host ON public.muc_registered USING btree (jid, host);


--
-- TOC entry 4232 (class 1259 OID 18421)
-- Name: i_muc_registered_nick; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_muc_registered_nick ON public.muc_registered USING btree (nick);


--
-- TOC entry 4238 (class 1259 OID 18422)
-- Name: i_muc_room_history_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_muc_room_history_host_idx ON public.muc_room_history USING btree (host);


--
-- TOC entry 4233 (class 1259 OID 18423)
-- Name: i_muc_room_name_host; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_muc_room_name_host ON public.muc_room USING btree (name, host);


--
-- TOC entry 4330 (class 1259 OID 18424)
-- Name: i_privacy_list_username; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_privacy_list_username ON public.privacy_list USING btree (username);


--
-- TOC entry 4331 (class 1259 OID 18425)
-- Name: i_privacy_list_username_name; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_privacy_list_username_name ON public.privacy_list USING btree (username, name);


--
-- TOC entry 4334 (class 1259 OID 18426)
-- Name: i_private_storage_username; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_private_storage_username ON public.private_storage USING btree (username);


--
-- TOC entry 4335 (class 1259 OID 18427)
-- Name: i_private_storage_username_namespace; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_private_storage_username_namespace ON public.private_storage USING btree (username, namespace);


--
-- TOC entry 4336 (class 1259 OID 18428)
-- Name: i_pubsub_item_itemid; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_pubsub_item_itemid ON public.pubsub_item USING btree (itemid);


--
-- TOC entry 4337 (class 1259 OID 18429)
-- Name: i_pubsub_item_tuple; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_pubsub_item_tuple ON public.pubsub_item USING btree (nodeid, itemid);


--
-- TOC entry 4342 (class 1259 OID 18430)
-- Name: i_pubsub_node_option_nodeid; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_pubsub_node_option_nodeid ON public.pubsub_node_option USING btree (nodeid);


--
-- TOC entry 4343 (class 1259 OID 18431)
-- Name: i_pubsub_node_owner_nodeid; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_pubsub_node_owner_nodeid ON public.pubsub_node_owner USING btree (nodeid);


--
-- TOC entry 4338 (class 1259 OID 18432)
-- Name: i_pubsub_node_parent; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_pubsub_node_parent ON public.pubsub_node USING btree (parent);


--
-- TOC entry 4339 (class 1259 OID 18433)
-- Name: i_pubsub_node_tuple; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_pubsub_node_tuple ON public.pubsub_node USING btree (host, node);


--
-- TOC entry 4344 (class 1259 OID 18434)
-- Name: i_pubsub_state_jid; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_pubsub_state_jid ON public.pubsub_state USING btree (jid);


--
-- TOC entry 4345 (class 1259 OID 18435)
-- Name: i_pubsub_state_tuple; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_pubsub_state_tuple ON public.pubsub_state USING btree (nodeid, jid);


--
-- TOC entry 4348 (class 1259 OID 18436)
-- Name: i_pubsub_subscription_opt; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX i_pubsub_subscription_opt ON public.pubsub_subscription_opt USING btree (subid, opt_name);



--
-- TOC entry 4441 (class 1259 OID 18443)
-- Name: i_user_register_mucs_user_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX i_user_register_mucs_user_idx ON public.user_register_mucs USING btree (username);


--
-- TOC entry 4354 (class 1259 OID 18455)
-- Name: index_cloud_id_user; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX index_cloud_id_user ON public.qcloud_main USING btree (q_user, id);


--
-- TOC entry 4362 (class 1259 OID 18456)
-- Name: index_cloud_sub_user_time; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_cloud_sub_user_time ON public.qcloud_sub USING btree (q_id, qs_time);


--
-- TOC entry 4363 (class 1259 OID 18457)
-- Name: index_cloud_sub_user_time_type; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_cloud_sub_user_time_type ON public.qcloud_sub USING btree (q_id, qs_time, qs_type);


--
-- TOC entry 4355 (class 1259 OID 18458)
-- Name: index_cloud_user_time; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_cloud_user_time ON public.qcloud_main USING btree (q_user, q_time);


--
-- TOC entry 4356 (class 1259 OID 18459)
-- Name: index_cloud_user_time_type; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX index_cloud_user_time_type ON public.qcloud_main USING btree (q_user, q_time, q_type);


--
-- TOC entry 4157 (class 1259 OID 18460)
-- Name: invite_spool_username_inviter_host_ihost_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX invite_spool_username_inviter_host_ihost_idx ON public.invite_spool USING btree (username, inviter, host, ihost);



--
-- TOC entry 4182 (class 1259 OID 18478)
-- Name: meeting_info_meeting_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX meeting_info_meeting_id_idx ON public.meeting_info USING btree (meeting_id);


--
-- TOC entry 4183 (class 1259 OID 18479)
-- Name: meeting_info_meeting_id_inviter_member_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX meeting_info_meeting_id_inviter_member_idx ON public.meeting_info USING btree (meeting_id, inviter, member);



--
-- TOC entry 4210 (class 1259 OID 18481)
-- Name: msg_history_backup_msg_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX msg_history_backup_msg_id_idx ON public.msg_history_backup USING btree (msg_id) WHERE (create_time >= '2017-12-06 00:00:00+08'::timestamp with time zone);


--
-- TOC entry 4211 (class 1259 OID 18482)
-- Name: msg_history_backup_msg_id_idx1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_msg_id_idx1 ON public.msg_history_backup USING btree (msg_id);


--
-- TOC entry 4212 (class 1259 OID 18483)
-- Name: msg_history_backup_realfrom_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_realfrom_idx ON public.msg_history_backup USING btree (realfrom);


--
-- TOC entry 4213 (class 1259 OID 18484)
-- Name: msg_history_backup_realto_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_realto_idx ON public.msg_history_backup USING btree (realto);


--
-- TOC entry 4214 (class 1259 OID 18485)
-- Name: msg_history_backup_tmp_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_tmp_create_time_idx ON public.msg_history_backup USING btree (create_time);


--
-- TOC entry 4215 (class 1259 OID 18486)
-- Name: msg_history_backup_tmp_create_time_m_from_from_host_m_to_to_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_tmp_create_time_m_from_from_host_m_to_to_idx ON public.msg_history_backup USING btree (create_time, m_from, from_host, m_to, to_host);


--
-- TOC entry 4216 (class 1259 OID 18487)
-- Name: msg_history_backup_tmp_create_time_m_from_m_to_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_tmp_create_time_m_from_m_to_idx ON public.msg_history_backup USING btree (create_time, m_from, m_to);


--
-- TOC entry 4217 (class 1259 OID 18488)
-- Name: msg_history_backup_tmp_m_from_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_tmp_m_from_idx ON public.msg_history_backup USING btree (m_from);


--
-- TOC entry 4218 (class 1259 OID 18489)
-- Name: msg_history_backup_tmp_m_from_m_to_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_tmp_m_from_m_to_create_time_idx ON public.msg_history_backup USING btree (m_from, m_to, create_time);


--
-- TOC entry 4219 (class 1259 OID 18490)
-- Name: msg_history_backup_tmp_m_from_m_to_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX msg_history_backup_tmp_m_from_m_to_id_idx ON public.msg_history_backup USING btree (m_from, m_to, id);


--
-- TOC entry 4220 (class 1259 OID 18491)
-- Name: msg_history_backup_tmp_m_to_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX msg_history_backup_tmp_m_to_idx ON public.msg_history_backup USING btree (m_to);


--
-- TOC entry 4194 (class 1259 OID 18492)
-- Name: msg_history_create_time_idx1; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_create_time_idx1 ON public.msg_history USING btree (create_time);


--
-- TOC entry 4195 (class 1259 OID 18493)
-- Name: msg_history_create_time_m_from_m_to_idx1; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_create_time_m_from_m_to_idx1 ON public.msg_history USING btree (create_time, m_from, m_to);


--
-- TOC entry 4196 (class 1259 OID 18494)
-- Name: msg_history_from_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_from_host_idx ON public.msg_history USING btree (from_host);


--
-- TOC entry 4197 (class 1259 OID 18495)
-- Name: msg_history_id_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX msg_history_id_idx ON public.msg_history USING btree (id);


--
-- TOC entry 4198 (class 1259 OID 18496)
-- Name: msg_history_m_from_from_host_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_m_from_from_host_create_time_idx ON public.msg_history USING btree (m_from, from_host, create_time);


--
-- TOC entry 4199 (class 1259 OID 18497)
-- Name: msg_history_m_from_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_m_from_idx ON public.msg_history USING btree (m_from);


--
-- TOC entry 4200 (class 1259 OID 18498)
-- Name: msg_history_m_from_m_to_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_m_from_m_to_create_time_idx ON public.msg_history USING btree (m_from, m_to, create_time);


--
-- TOC entry 4201 (class 1259 OID 18499)
-- Name: msg_history_m_from_m_to_from_host_to_host_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_m_from_m_to_from_host_to_host_create_time_idx ON public.msg_history USING btree (m_from, m_to, from_host, to_host, create_time);


--
-- TOC entry 4202 (class 1259 OID 18500)
-- Name: msg_history_m_from_m_to_id_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX msg_history_m_from_m_to_id_create_time_idx ON public.msg_history USING btree (m_from, m_to, id, create_time);


--
-- TOC entry 4203 (class 1259 OID 18501)
-- Name: msg_history_m_from_m_to_id_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX msg_history_m_from_m_to_id_idx ON public.msg_history USING btree (m_from, m_to, id);


--
-- TOC entry 4204 (class 1259 OID 18502)
-- Name: msg_history_m_to_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_m_to_idx ON public.msg_history USING btree (m_to);


--
-- TOC entry 4205 (class 1259 OID 18503)
-- Name: msg_history_m_to_to_host_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_m_to_to_host_create_time_idx ON public.msg_history USING btree (m_to, to_host, create_time);


--
-- TOC entry 4206 (class 1259 OID 18504)
-- Name: msg_history_msg_id_idx3; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX msg_history_msg_id_idx3 ON public.msg_history USING btree (msg_id);


--
-- TOC entry 4207 (class 1259 OID 18505)
-- Name: msg_history_read_flag_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_read_flag_idx ON public.msg_history USING btree (read_flag);


--
-- TOC entry 4208 (class 1259 OID 18506)
-- Name: msg_history_realfrom_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_realfrom_idx ON public.msg_history USING btree (realfrom);


--
-- TOC entry 4209 (class 1259 OID 18507)
-- Name: msg_history_realto_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX msg_history_realto_idx ON public.msg_history USING btree (realto);



--
-- TOC entry 4236 (class 1259 OID 18512)
-- Name: muc_room_backup_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_backup_host_idx ON public.muc_room_backup USING btree (host);


--
-- TOC entry 4237 (class 1259 OID 18513)
-- Name: muc_room_backup_name_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX muc_room_backup_name_host_idx ON public.muc_room_backup USING btree (name, host);


--
-- TOC entry 4248 (class 1259 OID 18514)
-- Name: muc_room_history_2016_muc_room_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX muc_room_history_2016_muc_room_name_idx ON public.muc_room_history_backup USING btree (muc_room_name);


--
-- TOC entry 4251 (class 1259 OID 18515)
-- Name: muc_room_history_backup_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX muc_room_history_backup_create_time_idx ON public.muc_room_history_backup USING btree (create_time);


--
-- TOC entry 4252 (class 1259 OID 18516)
-- Name: muc_room_history_backup_create_time_muc_room_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX muc_room_history_backup_create_time_muc_room_name_idx ON public.muc_room_history_backup USING btree (create_time, muc_room_name);


--
-- TOC entry 4253 (class 1259 OID 18517)
-- Name: muc_room_history_backup_create_time_nick_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX muc_room_history_backup_create_time_nick_idx ON public.muc_room_history_backup USING btree (create_time, nick);


--
-- TOC entry 4254 (class 1259 OID 18518)
-- Name: muc_room_history_backup_msg_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX muc_room_history_backup_msg_id_idx ON public.muc_room_history_backup USING btree (msg_id) WHERE (create_time >= '2017-12-06 00:00:00+08'::timestamp with time zone);


--
-- TOC entry 4255 (class 1259 OID 18519)
-- Name: muc_room_history_backup_msg_id_idx1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX muc_room_history_backup_msg_id_idx1 ON public.muc_room_history_backup USING btree (msg_id);


--
-- TOC entry 4239 (class 1259 OID 18520)
-- Name: muc_room_history_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_history_create_time_idx ON public.muc_room_history USING btree (create_time);


--
-- TOC entry 4240 (class 1259 OID 18521)
-- Name: muc_room_history_create_time_msg_id_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_history_create_time_msg_id_idx ON public.muc_room_history USING btree (create_time, msg_id);


--
-- TOC entry 4241 (class 1259 OID 18522)
-- Name: muc_room_history_create_time_nick_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_history_create_time_nick_idx ON public.muc_room_history USING btree (create_time, nick);


--
-- TOC entry 4242 (class 1259 OID 18523)
-- Name: muc_room_history_msg_id_idx2; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX muc_room_history_msg_id_idx2 ON public.muc_room_history USING btree (msg_id);


--
-- TOC entry 4243 (class 1259 OID 18524)
-- Name: muc_room_history_muc_room_name_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_history_muc_room_name_create_time_idx ON public.muc_room_history USING btree (muc_room_name, create_time);


--
-- TOC entry 4244 (class 1259 OID 18525)
-- Name: muc_room_history_muc_room_name_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_history_muc_room_name_idx ON public.muc_room_history USING btree (muc_room_name);


--
-- TOC entry 4245 (class 1259 OID 18526)
-- Name: muc_room_history_nick_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_history_nick_idx ON public.muc_room_history USING btree (nick);


--
-- TOC entry 4234 (class 1259 OID 18527)
-- Name: muc_room_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_host_idx ON public.muc_room USING btree (host);


--
-- TOC entry 4235 (class 1259 OID 18528)
-- Name: muc_room_name_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX muc_room_name_idx ON public.muc_room USING btree (name);


--
-- TOC entry 4256 (class 1259 OID 18529)
-- Name: muc_room_users_date_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_date_idx ON public.muc_room_users USING btree (date);


--
-- TOC entry 4257 (class 1259 OID 18530)
-- Name: muc_room_users_muc_name_username_date_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_muc_name_username_date_idx ON public.muc_room_users USING btree (muc_name, username, date);


--
-- TOC entry 4258 (class 1259 OID 18531)
-- Name: muc_room_users_muc_name_username_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX muc_room_users_muc_name_username_idx ON public.muc_room_users USING btree (muc_name, username);


--
-- TOC entry 4261 (class 1259 OID 18532)
-- Name: muc_room_users_username_date_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_username_date_idx ON public.muc_room_users USING btree (username, date);


--
-- TOC entry 4262 (class 1259 OID 18533)
-- Name: muc_room_users_username_host_date_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_username_host_date_idx ON public.muc_room_users USING btree (username, host, date);


--
-- TOC entry 4263 (class 1259 OID 18534)
-- Name: muc_room_users_username_host_idx1; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_username_host_idx1 ON public.muc_room_users USING btree (username, host);


--
-- TOC entry 4264 (class 1259 OID 18535)
-- Name: muc_room_users_username_host_muc_name_domain_date_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX muc_room_users_username_host_muc_name_domain_date_idx ON public.muc_room_users USING btree (username, host, muc_name, domain, date);


--
-- TOC entry 4265 (class 1259 OID 18536)
-- Name: muc_room_users_username_host_muc_name_domain_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX muc_room_users_username_host_muc_name_domain_idx ON public.muc_room_users USING btree (username, host, muc_name, domain);


--
-- TOC entry 4266 (class 1259 OID 18537)
-- Name: muc_room_users_username_host_mucname_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_username_host_mucname_idx ON public.muc_room_users USING btree (username, host, muc_name);


--
-- TOC entry 4267 (class 1259 OID 18538)
-- Name: muc_room_users_username_host_update_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_username_host_update_time_idx ON public.muc_room_users USING btree (username, host, update_time);


--
-- TOC entry 4268 (class 1259 OID 18539)
-- Name: muc_room_users_username_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_room_users_username_idx ON public.muc_room_users USING btree (username);

--
-- TOC entry 4282 (class 1259 OID 18545)
-- Name: muc_user_mark_index; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_user_mark_index ON public.muc_user_mark USING btree (muc_name, user_name);


--
-- TOC entry 4285 (class 1259 OID 18546)
-- Name: muc_vcard_info_muc_name_show_name_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX muc_vcard_info_muc_name_show_name_idx ON public.muc_vcard_info USING gin (muc_name public.gin_trgm_ops, show_name public.gin_trgm_ops);

--
-- TOC entry 4295 (class 1259 OID 18551)
-- Name: notice_history_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX notice_history_create_time_idx ON public.notice_history USING btree (create_time);


--
-- TOC entry 4296 (class 1259 OID 18552)
-- Name: notice_history_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX notice_history_host_idx ON public.notice_history USING btree (host);


--
-- TOC entry 4297 (class 1259 OID 18553)
-- Name: notice_history_m_from_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX notice_history_m_from_create_time_idx ON public.notice_history USING btree (m_from, create_time);


--
-- TOC entry 4298 (class 1259 OID 18554)
-- Name: notice_history_m_from_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX notice_history_m_from_idx ON public.notice_history USING btree (m_from);


--
-- TOC entry 4351 (class 1259 OID 18568)
-- Name: push_info_user_name_host_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX push_info_user_name_host_idx ON public.push_info USING btree (user_name, host);


--
-- TOC entry 4352 (class 1259 OID 18569)
-- Name: push_info_user_name_host_os_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX push_info_user_name_host_os_idx ON public.push_info USING btree (user_name, host, os);


--
-- TOC entry 4353 (class 1259 OID 18570)
-- Name: push_info_user_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX push_info_user_name_idx ON public.push_info USING btree (user_name);


--
-- TOC entry 4361 (class 1259 OID 18571)
-- Name: qcloud_main_history_q_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX qcloud_main_history_q_id_idx ON public.qcloud_main_history USING btree (q_id);


--
-- TOC entry 4369 (class 1259 OID 18572)
-- Name: qcloud_sub_history_qs_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX qcloud_sub_history_qs_id_idx ON public.qcloud_sub_history USING btree (qs_id);


--
-- TOC entry 4366 (class 1259 OID 18573)
-- Name: qcloud_sub_q_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX qcloud_sub_q_id_idx ON public.qcloud_sub USING btree (q_id);


--
-- TOC entry 4370 (class 1259 OID 18574)
-- Name: qtalk_user_comment_from_user_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX qtalk_user_comment_from_user_idx ON public.qtalk_user_comment USING btree (from_user);


--
-- TOC entry 4373 (class 1259 OID 18575)
-- Name: qtalk_user_comment_to_user_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX qtalk_user_comment_to_user_idx ON public.qtalk_user_comment USING btree (to_user);


--
-- TOC entry 4374 (class 1259 OID 18576)
-- Name: qtalk_user_comment_uniq_key; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX qtalk_user_comment_uniq_key ON public.qtalk_user_comment USING btree (from_user, to_user, public.qto_char(create_time, 'YYYY-WW'::text));


--
-- TOC entry 4381 (class 1259 OID 18577)
-- Name: revoke_msg_history_backup_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX revoke_msg_history_backup_create_time_idx ON public.revoke_msg_history_backup USING btree (create_time);


--
-- TOC entry 4377 (class 1259 OID 18578)
-- Name: revoke_msg_history_create_time_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX revoke_msg_history_create_time_idx ON public.revoke_msg_history USING btree (create_time);


--
-- TOC entry 4378 (class 1259 OID 18579)
-- Name: revoke_msg_history_msg_id_idx1; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX revoke_msg_history_msg_id_idx1 ON public.revoke_msg_history USING btree (msg_id);


--
-- TOC entry 4406 (class 1259 OID 18581)
-- Name: scheduling_info_begin_time_end_time_member_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX scheduling_info_begin_time_end_time_member_idx ON public.scheduling_info USING btree (begin_time, end_time, member);


--
-- TOC entry 4407 (class 1259 OID 18582)
-- Name: scheduling_info_begin_time_mem_action_remind_flag_canceled_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX scheduling_info_begin_time_mem_action_remind_flag_canceled_idx ON public.scheduling_info USING btree (begin_time, mem_action, remind_flag, canceled);


--
-- TOC entry 4408 (class 1259 OID 18583)
-- Name: scheduling_info_end_time_mem_action_remind_flag_canceled_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX scheduling_info_end_time_mem_action_remind_flag_canceled_idx ON public.scheduling_info USING btree (end_time, mem_action, remind_flag, canceled);


--
-- TOC entry 4409 (class 1259 OID 18584)
-- Name: scheduling_info_inviter_member_scheduling_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX scheduling_info_inviter_member_scheduling_id_idx ON public.scheduling_info USING btree (inviter, member, scheduling_id);


--
-- TOC entry 4412 (class 1259 OID 18585)
-- Name: scheduling_info_scheduling_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX scheduling_info_scheduling_id_idx ON public.scheduling_info USING btree (scheduling_id);


--
-- TOC entry 4413 (class 1259 OID 18586)
-- Name: scheduling_info_scheduling_id_member_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX scheduling_info_scheduling_id_member_idx ON public.scheduling_info USING btree (scheduling_id, member);


--
-- TOC entry 4414 (class 1259 OID 18587)
-- Name: scheduling_info_update_time_member_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX scheduling_info_update_time_member_idx ON public.scheduling_info USING btree (update_time, member);

--
-- TOC entry 4437 (class 1259 OID 18594)
-- Name: user_friends_username_userhost_friend_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX user_friends_username_userhost_friend_host_idx ON public.user_friends USING btree (username, userhost, friend, host);



--
-- TOC entry 4451 (class 1259 OID 18596)
-- Name: user_register_mucs_backup_created_at_username_registed_flag_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_backup_created_at_username_registed_flag_idx ON public.user_register_mucs_backup USING btree (created_at, username, registed_flag);


--
-- TOC entry 4454 (class 1259 OID 18597)
-- Name: user_register_mucs_backup_username_domain_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_backup_username_domain_idx ON public.user_register_mucs_backup USING btree (username, domain);


--
-- TOC entry 4455 (class 1259 OID 18598)
-- Name: user_register_mucs_backup_username_domain_muc_name_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_backup_username_domain_muc_name_idx ON public.user_register_mucs_backup USING btree (username, domain, muc_name);


--
-- TOC entry 4456 (class 1259 OID 18599)
-- Name: user_register_mucs_backup_username_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_backup_username_idx ON public.user_register_mucs_backup USING btree (username);


--
-- TOC entry 4457 (class 1259 OID 18600)
-- Name: user_register_mucs_backup_username_registed_flag_domain_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_backup_username_registed_flag_domain_idx ON public.user_register_mucs_backup USING btree (username, registed_flag, domain);


--
-- TOC entry 4442 (class 1259 OID 18601)
-- Name: user_register_mucs_created_at_username_registed_flag_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_created_at_username_registed_flag_idx ON public.user_register_mucs USING btree (created_at, username, registed_flag);


--
-- TOC entry 4445 (class 1259 OID 18602)
-- Name: user_register_mucs_username_domain_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_username_domain_idx ON public.user_register_mucs USING btree (username, domain);


--
-- TOC entry 4446 (class 1259 OID 18603)
-- Name: user_register_mucs_username_domain_muc_name_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_username_domain_muc_name_idx ON public.user_register_mucs USING btree (username, domain, muc_name);


--
-- TOC entry 4447 (class 1259 OID 18604)
-- Name: user_register_mucs_username_host_muc_name_domain_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX user_register_mucs_username_host_muc_name_domain_idx ON public.user_register_mucs USING btree (username, host, muc_name, domain);


--
-- TOC entry 4448 (class 1259 OID 18605)
-- Name: user_register_mucs_username_host_registed_flag_created_at_idx1; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_username_host_registed_flag_created_at_idx1 ON public.user_register_mucs USING btree (username, host, registed_flag, created_at);


--
-- TOC entry 4449 (class 1259 OID 18606)
-- Name: user_register_mucs_username_host_registed_flag_idx1; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_username_host_registed_flag_idx1 ON public.user_register_mucs USING btree (username, host, registed_flag);


--
-- TOC entry 4450 (class 1259 OID 18607)
-- Name: user_register_mucs_username_registed_flag_domain_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX user_register_mucs_username_registed_flag_domain_idx ON public.user_register_mucs USING btree (username, registed_flag, domain);


--
-- TOC entry 4477 (class 1259 OID 18611)
-- Name: vcard_version_username_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX vcard_version_username_host_idx ON public.vcard_version USING btree (username, host);


--
-- TOC entry 4496 (class 1259 OID 18612)
-- Name: warn_msg_history_backup_create_time_m_to_m_from_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_backup_create_time_m_to_m_from_idx ON public.warn_msg_history_backup USING btree (create_time, m_to, m_from);


--
-- TOC entry 4497 (class 1259 OID 18613)
-- Name: warn_msg_history_backup_create_time_m_to_to_host_m_from_fro_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_backup_create_time_m_to_to_host_m_from_fro_idx ON public.warn_msg_history_backup USING btree (create_time, m_to, to_host, m_from, from_host);


--
-- TOC entry 4498 (class 1259 OID 18614)
-- Name: warn_msg_history_backup_m_from_from_host_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_backup_m_from_from_host_create_time_idx ON public.warn_msg_history_backup USING btree (m_from, from_host, create_time);


--
-- TOC entry 4499 (class 1259 OID 18615)
-- Name: warn_msg_history_backup_m_from_m_to_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_backup_m_from_m_to_create_time_idx ON public.warn_msg_history_backup USING btree (m_from, m_to, create_time);


--
-- TOC entry 4500 (class 1259 OID 18616)
-- Name: warn_msg_history_backup_m_to_id_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX warn_msg_history_backup_m_to_id_create_time_idx ON public.warn_msg_history_backup USING btree (m_to, id, create_time) WHERE (((m_from)::text = 'qunar-message'::text) AND (from_host = 'ejabhost1'::text) AND (to_host = 'ejabhost1'::text));


--
-- TOC entry 4501 (class 1259 OID 18617)
-- Name: warn_msg_history_backup_m_to_id_create_time_idx1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX warn_msg_history_backup_m_to_id_create_time_idx1 ON public.warn_msg_history_backup USING btree (m_to, id DESC, create_time);


--
-- TOC entry 4502 (class 1259 OID 18618)
-- Name: warn_msg_history_backup_m_to_to_host_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_backup_m_to_to_host_create_time_idx ON public.warn_msg_history_backup USING btree (m_to, to_host, create_time);


--
-- TOC entry 4503 (class 1259 OID 18619)
-- Name: warn_msg_history_backup_msg_id_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_backup_msg_id_idx ON public.warn_msg_history_backup USING btree (msg_id);


--
-- TOC entry 4485 (class 1259 OID 18620)
-- Name: warn_msg_history_create_time_m_to_m_from_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_create_time_m_to_m_from_idx ON public.warn_msg_history USING btree (create_time, m_to, m_from);


--
-- TOC entry 4486 (class 1259 OID 18621)
-- Name: warn_msg_history_create_time_m_to_to_host_m_from_from_host_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_create_time_m_to_to_host_m_from_from_host_idx ON public.warn_msg_history USING btree (create_time, m_to, to_host, m_from, from_host);


--
-- TOC entry 4487 (class 1259 OID 18622)
-- Name: warn_msg_history_m_from_from_host_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_m_from_from_host_create_time_idx ON public.warn_msg_history USING btree (m_from, from_host, create_time);


--
-- TOC entry 4488 (class 1259 OID 18623)
-- Name: warn_msg_history_m_from_m_to_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_m_from_m_to_create_time_idx ON public.warn_msg_history USING btree (m_from, m_to, create_time);


--
-- TOC entry 4489 (class 1259 OID 18624)
-- Name: warn_msg_history_m_to_id_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX warn_msg_history_m_to_id_create_time_idx ON public.warn_msg_history USING btree (m_to, id, create_time) WHERE (((m_from)::text = 'qunar-message'::text) AND (from_host = 'ejabhost1'::text) AND (to_host = 'ejabhost1'::text));


--
-- TOC entry 4490 (class 1259 OID 18625)
-- Name: warn_msg_history_m_to_id_create_time_idx1; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX warn_msg_history_m_to_id_create_time_idx1 ON public.warn_msg_history USING btree (m_to, id DESC, create_time);


--
-- TOC entry 4491 (class 1259 OID 18626)
-- Name: warn_msg_history_m_to_to_host_create_time_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_m_to_to_host_create_time_idx ON public.warn_msg_history USING btree (m_to, to_host, create_time);


--
-- TOC entry 4492 (class 1259 OID 18627)
-- Name: warn_msg_history_msg_id_idx2; Type: INDEX; Schema: public; Owner: postgres
--

CREATE UNIQUE INDEX warn_msg_history_msg_id_idx2 ON public.warn_msg_history USING btree (msg_id);


--
-- TOC entry 4495 (class 1259 OID 18628)
-- Name: warn_msg_history_read_flag_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX warn_msg_history_read_flag_idx ON public.warn_msg_history USING btree (read_flag) WHERE (read_flag <> '3'::smallint);

--
-- TOC entry 4513 (class 2606 OID 18630)
-- Name: privacy_list_data privacy_list_data_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.privacy_list_data
    ADD CONSTRAINT privacy_list_data_id_fkey FOREIGN KEY (id) REFERENCES public.privacy_list(id) ON DELETE CASCADE;


--
-- TOC entry 4514 (class 2606 OID 18635)
-- Name: pubsub_item pubsub_item_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_item
    ADD CONSTRAINT pubsub_item_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES public.pubsub_node(nodeid) ON DELETE CASCADE;


--
-- TOC entry 4515 (class 2606 OID 18640)
-- Name: pubsub_node_option pubsub_node_option_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_node_option
    ADD CONSTRAINT pubsub_node_option_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES public.pubsub_node(nodeid) ON DELETE CASCADE;


--
-- TOC entry 4516 (class 2606 OID 18645)
-- Name: pubsub_node_owner pubsub_node_owner_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_node_owner
    ADD CONSTRAINT pubsub_node_owner_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES public.pubsub_node(nodeid) ON DELETE CASCADE;


--
-- TOC entry 4517 (class 2606 OID 18650)
-- Name: pubsub_state pubsub_state_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_state
    ADD CONSTRAINT pubsub_state_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES public.pubsub_node(nodeid) ON DELETE CASCADE;



--
-- TOC entry 4905 (class 0 OID 0)
-- Dependencies: 219
-- Name: TABLE fresh_empl_entering; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.fresh_empl_entering FROM postgres;


--
-- TOC entry 4907 (class 0 OID 0)
-- Dependencies: 220
-- Name: SEQUENCE fresh_empl_entering_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.fresh_empl_entering_id_seq FROM postgres;


--
-- TOC entry 4909 (class 0 OID 0)
-- Dependencies: 223
-- Name: TABLE host_info; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.host_info FROM postgres;


--
-- TOC entry 4911 (class 0 OID 0)
-- Dependencies: 224
-- Name: SEQUENCE host_info_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.host_info_id_seq FROM postgres;


--
-- TOC entry 4912 (class 0 OID 0)
-- Dependencies: 225
-- Name: TABLE host_users; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.host_users FROM postgres;


--
-- TOC entry 4914 (class 0 OID 0)
-- Dependencies: 226
-- Name: SEQUENCE host_users_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.host_users_id_seq FROM postgres;

--
-- TOC entry 4936 (class 0 OID 0)
-- Dependencies: 247
-- Name: TABLE msg_history_backup; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.msg_history_backup FROM postgres;

--
-- TOC entry 4942 (class 0 OID 0)
-- Dependencies: 259
-- Name: TABLE muc_room_history_backup; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.muc_room_history_backup FROM postgres;



--
-- TOC entry 4979 (class 0 OID 0)
-- Dependencies: 297
-- Name: TABLE qcloud_main; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.qcloud_main FROM postgres;


--
-- TOC entry 4985 (class 0 OID 0)
-- Dependencies: 298
-- Name: TABLE qcloud_main_history; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.qcloud_main_history FROM postgres;


--
-- TOC entry 4987 (class 0 OID 0)
-- Dependencies: 299
-- Name: SEQUENCE qcloud_main_history_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.qcloud_main_history_id_seq FROM postgres;


--
-- TOC entry 4989 (class 0 OID 0)
-- Dependencies: 300
-- Name: SEQUENCE qcloud_main_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.qcloud_main_id_seq FROM postgres;


--
-- TOC entry 4999 (class 0 OID 0)
-- Dependencies: 301
-- Name: TABLE qcloud_sub; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.qcloud_sub FROM postgres;


--
-- TOC entry 5005 (class 0 OID 0)
-- Dependencies: 302
-- Name: TABLE qcloud_sub_history; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.qcloud_sub_history FROM postgres;


--
-- TOC entry 5007 (class 0 OID 0)
-- Dependencies: 303
-- Name: SEQUENCE qcloud_sub_history_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.qcloud_sub_history_id_seq FROM postgres;


--
-- TOC entry 5009 (class 0 OID 0)
-- Dependencies: 304
-- Name: SEQUENCE qcloud_sub_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.qcloud_sub_id_seq FROM postgres;


--
-- TOC entry 5016 (class 0 OID 0)
-- Dependencies: 309
-- Name: TABLE revoke_msg_history_backup; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.revoke_msg_history_backup FROM postgres;


--
-- TOC entry 5040 (class 0 OID 0)
-- Dependencies: 321
-- Name: TABLE scheduling_info; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.scheduling_info FROM postgres;


--
-- TOC entry 5042 (class 0 OID 0)
-- Dependencies: 322
-- Name: SEQUENCE scheduling_info_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.scheduling_info_id_seq FROM postgres;


--
-- TOC entry 5050 (class 0 OID 0)
-- Dependencies: 350
-- Name: TABLE warn_msg_history; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.warn_msg_history FROM postgres;


--
-- TOC entry 5052 (class 0 OID 0)
-- Dependencies: 351
-- Name: SEQUENCE warn_msg_history_id_seq; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON SEQUENCE public.warn_msg_history_id_seq FROM postgres;


--
-- TOC entry 5053 (class 0 OID 0)
-- Dependencies: 352
-- Name: TABLE warn_msg_history_backup; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE public.warn_msg_history_backup FROM postgres;

-- Completed on 2018-12-13 17:11:42 CST

--
-- PostgreSQL database dump complete
--


CREATE TABLE public.sys_role
(
  id          SERIAL                                                NOT NULL
    CONSTRAINT sys_role_pkey
    PRIMARY KEY,
  describe    VARCHAR(100)                                          NOT NULL,
  create_time TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  update_time TIMESTAMP(6) WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
  role_name   VARCHAR(100)
);

COMMENT ON COLUMN public.sys_role.id IS '自增id';

COMMENT ON COLUMN public.sys_role.describe IS '角色描述';

COMMENT ON COLUMN public.sys_role.create_time IS '创建时间';

COMMENT ON COLUMN public.sys_role.update_time IS '更新时间';

COMMENT ON COLUMN public.sys_role.role_name IS '角色名称';


INSERT INTO public.sys_role ( describe, create_time, update_time, role_name) VALUES ('所有权限', '2019-05-15 03:25:07.078138', '2019-05-15 03:25:07.078138', '超级管理员');



CREATE TABLE public.sys_role_permission
(
  id            SERIAL NOT NULL
    CONSTRAINT sys_role_permission_pkey
    PRIMARY KEY,
  role_id       INTEGER,
  permission_id INTEGER
);

COMMENT ON COLUMN public.sys_role_permission.id IS '自增id';

COMMENT ON COLUMN public.sys_role_permission.role_id IS '角色id';

COMMENT ON COLUMN public.sys_role_permission.permission_id IS '权限id';

INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 1);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 2);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 3);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 4);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 5);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 6);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 7);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 8);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 9);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 10);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 11);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 12);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 13);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 14);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 15);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 16);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 17);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 18);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 19);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 20);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 21);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 22);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 23);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 24);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 25);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 26);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 27);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 28);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 29);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 30);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 31);
INSERT INTO public.sys_role_permission (role_id, permission_id) VALUES (1, 32);


CREATE TABLE public.sys_permission
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

COMMENT ON COLUMN public.sys_permission.id IS '自增id';

COMMENT ON COLUMN public.sys_permission.url IS '权限地址';

COMMENT ON COLUMN public.sys_permission.describe IS '权限描述';

COMMENT ON COLUMN public.sys_permission.create_time IS '创建时间';

COMMENT ON COLUMN public.sys_permission.update_time IS '更新时间';

COMMENT ON COLUMN public.sys_permission.status IS '0:导航栏不显示,1:导航栏显示';

COMMENT ON COLUMN public.sys_permission.sub_permission_ids IS '子权限id列表';

COMMENT ON COLUMN public.sys_permission.navigation_flag IS '是否映射导航栏';

INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (1, '/qtalk_background_management/startalk/management/addUser/user', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (2, '/qtalk_background_management/startalk/management/download/template', '', '2019-05-15 09:56:18.308918', '2019-05-15 09:56:18.308918', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (3, '/qtalk_background_management/startalk/management/update/user', '', '2019-05-15 09:57:07.807083', '2019-05-15 09:57:07.807083', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (4, '/qtalk_background_management/startalk/management/delete/user', '', '2019-05-15 09:57:07.835557', '2019-05-15 09:57:07.835557', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (5, '/qtalk_background_management/startalk/management/getuserDetail', '', '2019-05-15 09:57:08.712239', '2019-05-15 09:57:08.712239', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (6, '/qtalk_background_management/startalk/management/organ/addDep', '', '2019-05-15 09:57:08.140355', '2019-05-15 09:57:08.140355', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (7, '/qtalk_background_management/startalk/management/organ/deleteDep', '', '2019-05-15 09:57:08.169407', '2019-05-15 09:57:08.169407', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (8, '/qtalk_background_management/startalk/management/organ/addRoleClass', '', '2019-05-15 09:57:08.226841', '2019-05-15 09:57:08.226841', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (9, '/qtalk_background_management/startalk/management/organ/addRole', '', '2019-05-15 09:57:08.252260', '2019-05-15 09:57:08.252260', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (10, '/qtalk_background_management/startalk/management/organ/updateRole', '', '2019-05-15 09:57:08.284820', '2019-05-15 09:57:08.284820', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (11, '/qtalk_background_management/startalk/management/organ/deleteRole', '', '2019-05-15 09:57:08.316238', '2019-05-15 09:57:08.316238', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (12, '/qtalk_background_management/startalk/management/organ/getRoleClass', '', '2019-05-15 09:57:08.359823', '2019-05-15 09:57:08.359823', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (13, '/qtalk_background_management/startalk/management/searchUser', '用户搜索', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (14, '/qtalk_background_management/startalk/management/unbindRole', '解绑用户', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (15, '/qtalk_background_management/startalk/management/bindRole', '绑定用户', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (16, '/qtalk_background_management/startalk/management/findAllRoles', '所有角色', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, '{18,19,20,21,22}', 1);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (17, '/qtalk_background_management/startalk/management/queryUserList', '用户列表', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, '{13,14,15,16}', 1);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (18, '/qtalk_background_management/startalk/management/findAllPermissions', '所有权限', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (19, '/qtalk_background_management/startalk/management/findPermissionByRoleId', '角色查找权限', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (20, '/qtalk_background_management/startalk/management/updateRolePermissions', '更新权限', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (21, '/qtalk_background_management/startalk/management/deleteRole', '删除角色', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (22, '/qtalk_background_management/startalk/management/addNewRole', '添加角色', '2019-05-17 07:11:11.408295', '2019-05-17 07:11:11.408295', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (23, '/qtalk_background_management/startalk/management/organ/getStructure', '', '2019-05-15 09:57:08.078643', '2019-05-15 09:57:08.078643', 1, '{6,7}', 1);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (24, '/qtalk_background_management/startalk/management/organ/getAllRole', '', '2019-05-15 09:57:08.501870', '2019-05-15 09:57:08.501870', 1, '{8,9,10,11,12}', 1);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (25, '/qtalk_background_management/startalk/management/search', '', '2019-05-15 09:57:08.662159', '2019-05-15 09:57:08.662159', 1, '{1,2,3,4,5}', 1);

INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (26, '/qtalk_background_management/startalk/management/find/add/application', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (27, '/qtalk_background_management/startalk/management/find/get/group', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (28, '/qtalk_background_management/startalk/management/find/update/app', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (29, '/qtalk_background_management/startalk/management/find/action/app', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (30, '/qtalk_background_management/startalk/management/find/add/group', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (31, '/qtalk_background_management/startalk/management/file/upload', '', '2019-05-15 09:56:18.263012', '2019-05-15 09:56:18.263012', 1, null, 0);
INSERT INTO public.sys_permission (id, url, describe, create_time, update_time, status, sub_permission_ids, navigation_flag) VALUES (32, '/qtalk_background_management/startalk/management/find/management', '', '2019-05-15 09:57:08.662159', '2019-05-15 09:57:08.662159', 1, '{26,27,28,29,30,31}', 1);

alter sequence public.sys_role_permission_id_seq restart with 33;


CREATE TABLE public.sys_user_role
(
  id      SERIAL      NOT NULL
    CONSTRAINT sys_user_role_pkey
    PRIMARY KEY,
  role_id INTEGER,
  user_id VARCHAR(60) NOT NULL
);

COMMENT ON COLUMN public.sys_user_role.id IS '自增id';

COMMENT ON COLUMN public.sys_user_role.role_id IS '角色id';

COMMENT ON COLUMN public.sys_user_role.user_id IS '用户id';

CREATE UNIQUE INDEX sys_user_role_user_id_uindex ON public.sys_user_role (user_id);
INSERT INTO public.sys_user_role (role_id, user_id) VALUES ( 1, 'admin');



CREATE TABLE public.persistent_logins
(
  username  VARCHAR(64) NOT NULL,
  series    VARCHAR(64) NOT NULL
    CONSTRAINT persistent_logins_pkey
    PRIMARY KEY,
  token     VARCHAR(64) NOT NULL,
  last_used TIMESTAMP   NOT NULL
);

create table public.find_application_table
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

comment on column public.find_application_table.id is '自增id';

comment on column public.find_application_table.application_type is '应用类型，2RN应用，3 H5应用';

comment on column public.find_application_table.visible_range is '可见性范围，空标识全员可见';

comment on column public.find_application_table.application_name is '应用名称';

comment on column public.find_application_table.application_class is '应用分类';

comment on column public.find_application_table.application_icon is '应用图标';

comment on column public.find_application_table.application_version is '应用版本号';

comment on column public.find_application_table.ios_version is 'ios版本号';

comment on column public.find_application_table.android_version is '安卓版本号';

comment on column public.find_application_table.ios_bundle is 'iosbundle包，h5应用的话对应的是h5的地址';

comment on column public.find_application_table.android_bundle is 'android的bundle包，h5应用对应的是地址';

comment on column public.find_application_table.application_desc is '应用描述';

comment on column public.find_application_table.create_time is '创建时间';

comment on column public.find_application_table.update_time is '更新时间';

comment on column public.find_application_table.disable_flag is '禁用标志位';

comment on column public.find_application_table.member_id is '在群组的id';

comment on column public.find_application_table.h5_action is 'h5页面地址';

comment on column public.find_application_table.entrance is 'RN应用的入口地址';

comment on column public.find_application_table.properties is '额外初始属性 map的json';

comment on column public.find_application_table.module is 'RN应用的程序入口';

comment on column public.find_application_table.show_native_nav is '是否显示导航';

comment on column public.find_application_table.nav_title is '导航title h5应用不生效';

comment on column public.find_application_table.valid_platform is '可适配的客户端类型，IOS Angroid PC';

comment on column public.find_application_table.visible_platform is 'ios|Android|pc(101):5';

comment on column public.find_application_table.bundle_name is 'bundle 包的文件名 不同于applicaName';

comment on column public.find_application_table.h5_action_ios is 'ios h5的页面地址';

comment on column public.find_application_table.h5_action_android is 'h5 android页面地址';

comment on column public.find_application_table.delete_flag is '删除标记位，1删除 0未删除';

comment on column public.find_application_table.native_flag is '原生应用标记0是自定义，1是原生应用禁止修改';

comment on column public.find_application_table.app_uuid is '应用的UUID';


create unique index find_application_table_id_uindex
    on public.find_application_table (id);

create index find_application_table_application_name_index
    on public.find_application_table (application_name);

create table public.find_class_table
(
    id         serial       not null
        constraint find_class_table_pk
            primary key,
    group_name varchar(200) not null,
    group_icon varchar(200) not null
);

comment on table public.find_class_table is '应用分类表';

comment on column public.find_class_table.id is '自增ID';

comment on column public.find_class_table.group_name is '分组名称';

comment on column public.find_class_table.group_icon is '分组封面';


create unique index find_class_table_group_name_uindex
    on public.find_class_table (group_name);

create unique index find_class_table_id_uindex
    on public.find_class_table (id);

create table public.startalk_dep_table
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

comment on table public.startalk_dep_table is '部门信息表';

comment on column public.startalk_dep_table.id is '自增ID';

comment on column public.startalk_dep_table.dep_name is '部门名称';

comment on column public.startalk_dep_table.dep_level is '部门层级';

comment on column public.startalk_dep_table.dep_vp is '部门领导';

comment on column public.startalk_dep_table.dep_hr is '部门HR';

comment on column public.startalk_dep_table.dep_visible is '部门可见性';

comment on column public.startalk_dep_table.parent_id is '父级部门的ID';

comment on column public.startalk_dep_table.delete_flag is '部门删除标记位,0是未删除 1是已删除';

comment on column public.startalk_dep_table.dep_desc is '部门信息备注';

comment on column public.startalk_dep_table.create_time is '创建时间';

comment on column public.startalk_dep_table.update_time is '更新时间';

CREATE UNIQUE INDEX startalk_dep_table_depName_uindex ON public.startalk_dep_table(dep_name);
create unique index startalk_dep_table_id_uindex
    on public.startalk_dep_table (id);
    
INSERT INTO public.startalk_dep_table (dep_name, dep_level)
VALUES('/管理员',1);

INSERT INTO public.startalk_dep_table (dep_name, dep_level)
VALUES('/智能服务助手',1);

create table public.startalk_role_class
(
    id             serial            not null
        constraint startalk_role_class_pk
            primary key,
    role_class     varchar(200)      not null,
    available_flag integer default 1 not null
);

comment on table public.startalk_role_class is '角色分组';

comment on column public.startalk_role_class.id is '自增id';

comment on column public.startalk_role_class.role_class is '角色分组';

comment on column public.startalk_role_class.available_flag is '可用标志 1是可用 0表示不可用';


create unique index startalk_role_class_id_uindex
    on public.startalk_role_class (id);

create unique index startalk_role_class_role_class_uindex
    on public.startalk_role_class (role_class);

create table public.startalk_user_role_table
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

comment on table public.startalk_user_role_table is 'startalk用户角色表';

comment on column public.startalk_user_role_table.id is '主键自增id';

comment on column public.startalk_user_role_table.role_name is '角色名';

comment on column public.startalk_user_role_table.available_flag is '可用标志位,1可用 0 不可用';

comment on column public.startalk_user_role_table.class_id is '角色所属组别的ID';


create unique index startalk_user_role_table_id_uindex
    on public.startalk_user_role_table (id);
insert into public.host_info (host, description, host_admin) values ('qtalk', 'qtalk', 'admin');
insert into public.host_users (host_id, user_id, user_name, department, dep1, pinyin, frozen_flag, version, user_type, hire_flag, gender, password, initialpwd, pwd_salt, ps_deptid) values ('1', 'admin', '管理员', '/管理员', '管理员', 'admin', '0', '1', 'U', '1', '1', 'CRY:fd540f073cc09aa98220bbb234153bd5', '1', 'qtalkadmin_pwd_salt_d2bf42081aab47f4ac00697d7dd32993', 'qtalk');
-- insert into public.vcard_version (username, version, profile_version, gender, host, url) values ('admin', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');
insert into public.host_users (host_id, user_id, user_name, department, dep1, pinyin, frozen_flag, version, user_type, hire_flag, gender, password, initialpwd, pwd_salt, ps_deptid) values ('1', 'file-transfer', '文件传输助手', '/智能服务助手', '智能服务助手', 'file-transfer', '1', '1', 'U', '1', '1', 'CRY:fd540f073cc09aa98220bbb234153bd5', '1', 'qtalkadmin_pwd_salt_d2bf42081aab47f4ac00697d7dd32993', 'qtalk');
-- insert into public.vcard_version (username, version, profile_version, gender, host, url) values ('file-transfer', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');
insert into public.vcard_version (username, version, profile_version, gender, host, url) values ('admin', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');
insert into public.vcard_version (username, version, profile_version, gender, host, url) values ('file-transfer', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(33,'test','/qtalk_background_management/startalk/management/baseData',1,'{35}',1);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(34,'test','/qtalk_background_management/startalk/management/depList',1,'{}',0);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(35,'test','/qtalk_background_management/startalk/management/dayDataSearch',1,'{}',0);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(36,'test','/qtalk_background_management/startalk/management/dayMsgDataSearch',1,'{34,37}',1);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(37,'test','/qtalk_background_management/startalk/management/userMsgCount',1,'{}' ,0);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(38,'test','/qtalk_background_management/startalk/management/clientVersion',1,'{}' ,1);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(39,'test','/qtalk_background_management/startalk/management/getVersionList',1,'{}',0);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(40,'test','/qtalk_background_management/startalk/management/clickCount',1,'{41,42}',1);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(41,'test','/qtalk_background_management/startalk/management/selectList',1,'{}',0);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(42,'test','/qtalk_background_management/startalk/management/selectModel',1,'{}',0);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(43,'test','/qtalk_background_management/startalk/management/activity',1,'{}',1);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(44,'test','/qtalk_background_management/startalk/management/userOnline',1,'{45}',1);
INSERT INTO public.sys_permission(id ,describe,url,status,sub_permission_ids, navigation_flag) VALUES(45,'test','/qtalk_background_management/startalk/management/searchUserOnline',1,'{}',0);

alter sequence public.sys_permission_id_seq restart with 46;

INSERT INTO public.sys_role_permission(role_id, permission_id) VALUES (1,33),(1,34),(1,35),(1,36),(1,37),(1,38),(1,39),(1,40),(1,41),(1,42),(1,43),(1,44),(1,45);

-- auto-generated definition
CREATE TABLE public.data_board_day
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
  ON public.data_board_day (create_time);

COMMENT ON COLUMN public.data_board_day.activity IS '活跃数';

COMMENT ON COLUMN public.data_board_day.client_online_time IS '客户端在线时间';

COMMENT ON COLUMN public.data_board_day.start_count IS '启动次数';

COMMENT ON COLUMN public.data_board_day.client_version IS '客户端版本统计';

COMMENT ON COLUMN public.data_board_day.day_msg_count IS '每天消息量';

COMMENT ON COLUMN public.data_board_day.day_msg_average IS '每天平均消息量';

COMMENT ON COLUMN public.data_board_day.department_data IS '部门数据统计';

COMMENT ON COLUMN public.data_board_day.hire_type_data IS '人员类型统计';

COMMENT ON COLUMN public.data_board_day.create_time IS '创建时间';

COMMENT ON COLUMN public.data_board_day.platform_activity IS '平台活跃数';

COMMENT ON COLUMN public.data_board_day.dep_activity IS '部门活跃数';

COMMENT ON COLUMN public.data_board_day.hire_type_activity IS '人员类型活跃数';


-- auto-generated definition
CREATE TABLE public.client_upgrade (
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
  ON public.client_upgrade (client_type, platform, version);

COMMENT ON COLUMN public.client_upgrade.id IS '自增id';

COMMENT ON COLUMN public.client_upgrade.client_type IS '客户端类型 qtalk,qchat';

COMMENT ON COLUMN public.client_upgrade.platform IS '平台Android,ios';

COMMENT ON COLUMN public.client_upgrade.version IS '版本号';

COMMENT ON COLUMN public.client_upgrade.copywriting IS '更新文案';

COMMENT ON COLUMN public.client_upgrade.grayscale_status IS '灰度测试状态 0:否 1:是';

COMMENT ON COLUMN public.client_upgrade.grayscale_value IS '灰度量';

COMMENT ON COLUMN public.client_upgrade.upgrade_status IS '更新状态 0:强制更新 1:选择更新';

COMMENT ON COLUMN public.client_upgrade.upgrade_url IS '更新地址';

COMMENT ON COLUMN public.client_upgrade.create_time IS '创建时间';

COMMENT ON COLUMN public.client_upgrade.update_time IS '更新时间';

COMMENT ON COLUMN public.client_upgrade.md5_key IS '文件MD5';

COMMENT ON COLUMN public.client_upgrade.stop_status IS '是否停止更新,0:否,1:是';

COMMENT ON COLUMN public.client_upgrade.stop_reason IS '停止更新原因';

COMMENT ON COLUMN public.client_upgrade.updated_count IS '已更新量';




-- 原始日志数据表
create table if not exists public.t_client_log
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

comment on column public.t_client_log.id is '主键id';
comment on column public.t_client_log.u_id is '用户id';
comment on column public.t_client_log.u_domain is '域名 eg:ejabhost1';
-- 该字段从l_device_data中的os获取
comment on column public.t_client_log.d_os is '操作系统 LINUX、Android、Mac、iOS、PC64、android';
-- 该字段从l_device_data中的osBrand获取
comment on column public.t_client_log.d_brand is '客户端手机品牌';
-- 该字段从l_device_data中的osModel获取
comment on column public.t_client_log.d_model is '客户端机型';
-- 该字段从l_device_data中的plat字段获取
comment on column public.t_client_log.d_plat is '客户端平台 startalk';
comment on column public.t_client_log.d_ip is '客户端ip地址';
comment on column public.t_client_log.d_lat is '经纬度';
comment on column public.t_client_log.d_lgt is '经纬度';
comment on column public.t_client_log.l_type is '日志类型，CAT、COD、ACT、CRA、FIL';
comment on column public.t_client_log.l_sub_type is '日志子类型';
comment on column public.t_client_log.l_report_time is '上报时间';
comment on column public.t_client_log.l_data is '原始日志';
comment on column public.t_client_log.l_device_data is '设备日志';
comment on column public.t_client_log.l_user_data is '用户日志';
-- 该字段从l_device_data中的versionCode字段获取
comment on column public.t_client_log.l_version_code is '版本编号 221';
comment on column public.t_client_log.l_version_name is '版本名称 3.1.5';
comment on column public.t_client_log.create_time is '创建时间';
comment on column public.t_client_log.l_client_event is '事件名称 eg：搜索、拉取历史耗时';
comment on column public.t_client_log.d_platform is '所属平台，与d_os类型，只是经过了转小写处理, ios/linux/mac/pc32/pc64/android ';
comment on column public.t_client_log.l_event_id is '事件id';
comment on column public.t_client_log.l_current_page is '当前页';


-- 客户端品牌字典表
create table if not exists public.t_dict_client_brand
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

create unique index if not exists t_dict_client_brand_brand_platform_idx on public.t_dict_client_brand (brand, platform);
comment on table public.t_dict_client_brand is '品牌渠道字典表';
comment on column public.t_dict_client_brand.id is '主键';
comment on column public.t_dict_client_brand.brand is '客户端手机品牌';
comment on column public.t_dict_client_brand.platform is '品牌所属平台';
comment on column public.t_dict_client_brand.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column public.t_dict_client_brand.create_time is '创建时间';


-- 客户端机型字典表
create table if not exists public.t_dict_client_model
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

create unique index if not exists t_dict_client_model_client_model_client_brand_platform_idx on public.t_dict_client_model (client_model, client_brand, platform);
comment on table public.t_dict_client_model is '机型字典表';
comment on column public.t_dict_client_model.id is '主键';
comment on column public.t_dict_client_model.client_model is '机型';
comment on column public.t_dict_client_model.client_brand is '品牌';
comment on column public.t_dict_client_model.platform is '所属平台';
comment on column public.t_dict_client_model.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column public.t_dict_client_model.create_time is '创建时间';


-- 客户端版本字典表
create table if not exists public.t_dict_client_version
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

create unique index if not exists t_dict_client_version_client_version_platform_idx on public.t_dict_client_version (client_version, platform);
comment on table public.t_dict_client_version is '客户端版本字典表';
comment on column public.t_dict_client_version.id is '主键';
comment on column public.t_dict_client_version.client_version is 'qtalk客户端版本';
comment on column public.t_dict_client_version.platform is '所属平台';
comment on column public.t_dict_client_version.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column public.t_dict_client_version.create_time is '创建时间';

-- 点击事件字典表
create table if not exists public.t_dict_client_event
(
 id bigserial not null
  constraint t_dict_client_event_pkey
   primary key,
 event varchar(100),
 del_flag integer default 0 not null,
 create_time timestamp with time zone default now() not null,
 platform varchar(100)
);

create unique index if not exists event_platform_unique_idx on public.t_dict_client_event (event, platform);
comment on table public.t_dict_client_event is '点击事件字典表';
comment on column public.t_dict_client_event.id is '主键';
comment on column public.t_dict_client_event.event is '事件';
comment on column public.t_dict_client_event.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column public.t_dict_client_event.create_time is '创建时间';


-- 点击数据统计表
create table if not exists public.statistic_qtalk_click_event
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

comment on table public.statistic_qtalk_click_event is '点击统计数据表';
comment on column public.statistic_qtalk_click_event.id is '主键';
comment on column public.statistic_qtalk_click_event.client_platform is '所属平台';
comment on column public.statistic_qtalk_click_event.client_version is '客户端版本号';
comment on column public.statistic_qtalk_click_event.client_brand is '客户端品牌';
comment on column public.statistic_qtalk_click_event.client_model is '客户端型号';
comment on column public.statistic_qtalk_click_event.click_event is '点击事件';
comment on column public.statistic_qtalk_click_event.click_day is '日期(天)';
comment on column public.statistic_qtalk_click_event.del_flag is '删除标识 0 - 未删除 1 - 删除';
comment on column public.statistic_qtalk_click_event.create_time is '创建时间';
-- ldap 配置表
create table public.qtalk_config
(
    id  serial   primary key  not null,
    config_key   varchar(30)  not null,
    config_value varchar(500) not null,
    create_time  timestamp default now()
);
comment on column public.qtalk_config.config_key is '配置key';
comment on column public.qtalk_config.config_value is '配置值';
comment on column public.qtalk_config.create_time is '创建时间';
create unique index qtalk_config_id_uindex
    on public.qtalk_config (id);
create unique index qtalk_config_config_key_uindex
    on public.qtalk_config (config_key);


