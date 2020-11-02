-- v1.00

\connect ejabberd
--
-- TOC entry 203 (class 1259 OID 17229)
-- Name: admin_user; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.admin_user (
    username character varying(255) NOT NULL,
    priority text NOT NULL
);


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
      id serial primary key, 
      username varchar(1000) not null,
      create_time timestamptz not null default now()
);


ALTER TABLE public.flogin_user OWNER TO ejabberd;



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
-- TOC entry 253 (class 1259 OID 17533)
-- Name: msgview_old; Type: VIEW; Schema: public; Owner: ejabberd
--

CREATE VIEW public.msgview_old AS
 SELECT msg_history.m_from,
    msg_history.create_time AS s_date
   FROM public.msg_history;


ALTER TABLE public.msgview_old OWNER TO ejabberd;

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
-- TOC entry 353 (class 1259 OID 18142)
-- Name: white_list; Type: TABLE; Schema: public; Owner: ejabberd
--

CREATE TABLE public.white_list (
    username text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    single_flag text DEFAULT '1'::text
);



--
-- TOC entry 3844 (class 2604 OID 18167)
-- Name: destroy_muc_info id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.destroy_muc_info ALTER COLUMN id SET DEFAULT nextval('public.destroy_muc_info_id_seq'::regclass);

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
-- TOC entry 3999 (class 2604 OID 18192)
-- Name: revoke_msg_history id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.revoke_msg_history ALTER COLUMN id SET DEFAULT nextval('public.revoke_msg_history_id_seq'::regclass);


--
-- TOC entry 4065 (class 2604 OID 18198)
-- Name: vcard_version id; Type: DEFAULT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.vcard_version ALTER COLUMN id SET DEFAULT nextval('public.vcard_version_id_seq'::regclass);


--
-- TOC entry 4112 (class 2606 OID 18211)
-- Name: destroy_muc_info destroy_muc_info_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.destroy_muc_info ADD CONSTRAINT destroy_muc_info_pkey PRIMARY KEY (id);

--
-- TOC entry 4162 (class 2606 OID 18239)
-- Name: last last_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.last ADD CONSTRAINT last_pkey PRIMARY KEY (username);

--
-- TOC entry 4193 (class 2606 OID 18249)
-- Name: motd motd_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.motd ADD CONSTRAINT motd_pkey PRIMARY KEY (username);

--
-- TOC entry 4230 (class 2606 OID 18255)
-- Name: muc_last muc_last_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_last ADD CONSTRAINT muc_last_pkey PRIMARY KEY (muc_name);

--
-- TOC entry 4247 (class 2606 OID 18259)
-- Name: muc_room_history muc_room_history_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_room_history ADD CONSTRAINT muc_room_history_pkey PRIMARY KEY (id);

--
-- TOC entry 4260 (class 2606 OID 18261)
-- Name: muc_room_users muc_room_users_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_room_users ADD CONSTRAINT muc_room_users_pkey PRIMARY KEY (id);

--
-- TOC entry 4284 (class 2606 OID 18265)
-- Name: muc_user_mark muc_user_mark_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_user_mark ADD CONSTRAINT muc_user_mark_pkey PRIMARY KEY (id);

--
-- TOC entry 4287 (class 2606 OID 18269)
-- Name: muc_vcard_info muc_vcard_info_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.muc_vcard_info ADD CONSTRAINT muc_vcard_info_pkey PRIMARY KEY (muc_name);


--
-- TOC entry 4300 (class 2606 OID 18277)
-- Name: notice_history notice_history_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.notice_history ADD CONSTRAINT notice_history_pkey PRIMARY KEY (id);

--
-- TOC entry 4116 (class 2606 OID 18291)
-- Name: flogin_user pk_flogin_user; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--
-- FIXED: 已有主键id
-- ALTER TABLE ONLY public.flogin_user ADD CONSTRAINT pk_flogin_user PRIMARY KEY (username);

--
-- TOC entry 4159 (class 2606 OID 18293)
-- Name: iplimit pk_iplimit; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.iplimit ADD CONSTRAINT pk_iplimit PRIMARY KEY (ip, name, priority);

--
-- TOC entry 4436 (class 2606 OID 18295)
-- Name: user_friends pk_user_friends; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_friends ADD CONSTRAINT pk_user_friends PRIMARY KEY (username, friend, host);

--
-- TOC entry 4329 (class 2606 OID 18301)
-- Name: privacy_default_list privacy_default_list_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.privacy_default_list ADD CONSTRAINT privacy_default_list_pkey PRIMARY KEY (username);

--
-- TOC entry 4333 (class 2606 OID 18303)
-- Name: privacy_list privacy_list_id_key; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.privacy_list ADD CONSTRAINT privacy_list_id_key UNIQUE (id);

--
-- TOC entry 4341 (class 2606 OID 18305)
-- Name: pubsub_node pubsub_node_nodeid_key; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_node ADD CONSTRAINT pubsub_node_nodeid_key UNIQUE (nodeid);

--
-- TOC entry 4347 (class 2606 OID 18307)
-- Name: pubsub_state pubsub_state_stateid_key; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.pubsub_state ADD CONSTRAINT pubsub_state_stateid_key UNIQUE (stateid);

--
-- TOC entry 4376 (class 2606 OID 18321)
-- Name: recv_msg_option recv_msg_option_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.recv_msg_option ADD CONSTRAINT recv_msg_option_pkey PRIMARY KEY (username);

--
-- TOC entry 4380 (class 2606 OID 18325)
-- Name: revoke_msg_history revoke_msg_history_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.revoke_msg_history ADD CONSTRAINT revoke_msg_history_pkey PRIMARY KEY (id);

--
-- TOC entry 4405 (class 2606 OID 18337)
-- Name: s2s_mapped_host s2s_mapped_host_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.s2s_mapped_host ADD CONSTRAINT s2s_mapped_host_pkey PRIMARY KEY (domain, host);

--
-- TOC entry 4453 (class 2606 OID 18353)
-- Name: user_register_mucs_backup user_register_mucs_backup_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_register_mucs_backup ADD CONSTRAINT user_register_mucs_backup_pkey PRIMARY KEY (muc_name, username);

--
-- TOC entry 4444 (class 2606 OID 18355)
-- Name: user_register_mucs user_register_mucs_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_register_mucs ADD CONSTRAINT user_register_mucs_pkey PRIMARY KEY (muc_name, username);

--
-- TOC entry 4459 (class 2606 OID 18357)
-- Name: user_relation_opts user_relation_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.user_relation_opts ADD CONSTRAINT user_relation_pkey PRIMARY KEY (username);

--
-- TOC entry 4476 (class 2606 OID 18365)
-- Name: vcard_version vcard_version_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.vcard_version ADD CONSTRAINT vcard_version_pkey PRIMARY KEY (id);

--
-- TOC entry 4507 (class 2606 OID 18375)
-- Name: white_list white_list_pkey; Type: CONSTRAINT; Schema: public; Owner: ejabberd
--

ALTER TABLE ONLY public.white_list ADD CONSTRAINT white_list_pkey PRIMARY KEY (username);


ALTER TABLE public.white_list OWNER TO ejabberd;




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
-- TOC entry 4157 (class 1259 OID 18460)
-- Name: invite_spool_username_inviter_host_ihost_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE INDEX invite_spool_username_inviter_host_ihost_idx ON public.invite_spool USING btree (username, inviter, host, ihost);

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
-- TOC entry 4437 (class 1259 OID 18594)
-- Name: user_friends_username_userhost_friend_host_idx; Type: INDEX; Schema: public; Owner: ejabberd
--

CREATE UNIQUE INDEX user_friends_username_userhost_friend_host_idx ON public.user_friends USING btree (username, userhost, friend, host);


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



-- -------------------------------------------
insert into public.vcard_version (username, version, profile_version, gender, host, url) values ('admin', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');

insert into public.vcard_version (username, version, profile_version, gender, host, url) values ('file-transfer', '1', '1', '1', 'yourhost', '/file/v2/download/214b6c4f070cf08a1ed27dbd73fdee5d.png');

