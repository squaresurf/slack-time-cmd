--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.5
-- Dumped by pg_dump version 11.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: slack_users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.slack_users (
    id bigint NOT NULL,
    slack_id character varying(255) NOT NULL,
    deleted boolean NOT NULL,
    is_bot boolean NOT NULL,
    tz_offset integer,
    tz_label character varying(255)
);


--
-- Name: slack_users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.slack_users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: slack_users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.slack_users_id_seq OWNED BY public.slack_users.id;


--
-- Name: slack_users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.slack_users ALTER COLUMN id SET DEFAULT nextval('public.slack_users_id_seq'::regclass);


--
-- Name: slack_users slack_users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.slack_users
    ADD CONSTRAINT slack_users_pkey PRIMARY KEY (id);


--
-- Name: slack_users_slack_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX slack_users_slack_id_index ON public.slack_users USING btree (slack_id);


--
-- PostgreSQL database dump complete
--

