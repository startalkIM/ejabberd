#!/usr/bin/env python
# -*- coding:utf-8 -*-
import re
import json

a_all_tables = {}
a_all_comments = {}
a_all_sets = {}
a_all_indexes = {}
b_all_tables = {}
b_all_comments = {}
b_all_sets = {}
b_all_indexes = {}

sql_a = 'testdump1.sql'
sql_b = 'testdump2.sql'


class Create:
    def handle_create(self, sql: str = ''):
        sql_parts = sql.split(' ')
        if sql_parts[1] == 'TABLE':
            self.handle_create_table(sql)
        elif sql_parts[1] in ['UNIQUE', 'INDEX']:
            self.handle_create_index(sql)

    def handle_create_table(self, sql: str = ''):
        schema_name_p = re.compile('TABLE (.*?) ')
        struct_p = re.compile('\((.*)\)', re.DOTALL)
        try:

            table_schema_name = schema_name_p.findall(sql)
            if not len(table_schema_name):
                print('TABLE: no name found')
                return None
            schema_name = table_schema_name[0]
            table_struct = struct_p.findall(sql)[0]
            table_struct = self.handle_table_stuct(table_struct)
        except Exception as e:
            print(e)
            exit(0)
        all_tables[schema_name] = table_struct
        return None

    def handle_create_index(self, sql: str = ''):
        is_unique = 'UNIQUE INDEX' in sql
        schema_name_p = re.compile('INDEX (.*?) ')
        index_p = re.compile('INDEX .*? ON (.*)', re.DOTALL)
        try:
            index_name = schema_name_p.findall(sql)
            if not len(index_name):
                print('INDEX: no name found')
                return None
            index_name = index_name[0]
            content_i = index_p.findall(sql)[0]
            content_i = self.handle_index(content_i)
        except Exception as e:
            print(e)
            exit(0)
        if is_unique:
            all_indexes[index_name] = ['UNIQUE'] + content_i
        else:
            all_indexes[index_name] = content_i

    @staticmethod
    def handle_table_stuct(struct: str = ''):
        table_struct = struct.split(',')
        res = []
        for column in table_struct:
            column = column.strip()
            res.append(column)
        return res

    @staticmethod
    def handle_index(i_struct: str = ''):
        index_struct = i_struct.split()
        res = []
        try:
            index_col = index_struct[0]
            p_index_type = re.compile('USING (.*?) \(.*\)')
            p_used_cols = re.compile('USING .*? \((.*)\)')
            index_type = p_index_type.findall(i_struct)[0]
            p_used_cols = p_used_cols.findall(i_struct)[0]

            res = [index_col, index_type, p_used_cols]
        except Exception as e:
            print(e)
        return res


class Comment:
    def handle_comment(self, sql: str = ''):
        schema_name_p = re.compile('COLUMN (.*?) ')
        comment_content = re.compile('IS (\'.*?\')')
        try:
            schema_name = schema_name_p.findall(sql)
            if not len(schema_name):
                return None
            schema_name = schema_name[0]
            comment = comment_content.findall(sql)[0]
        except Exception as e:
            print(e)
            exit(0)
        all_comments[schema_name] = comment
        return


class ALTER:
    def handle_alter(sql: str = ''):
        re.compile('')


# if __name__ == '__main__':
#     a = "CREATE INDEX destroy_muc_info_nick_name_created_at_idx ON public.destroy_muc_info USING btree (nick_name, created_at);"
#     c = Create()
#     c.handle_create_index(a)
#     print(all_indexes)
if __name__ == '__main__':
    create = Create()
    comment = Comment()
    # 操作包含 COMMENT SELECT SET CREATE ALTER 五大类
    with open(sql_a, 'r') as file_a:
        raw_file = file_a.read()
        raw_all = re.split('--\n-- Name:.*\n--', raw_file)  # 以注释分割每个段落
    for raw_phrase in raw_all:
        raw_phrase = re.sub('--.*?\n', '', raw_phrase)  # 清除注释
        raw_phrase_line = re.sub('\n', '', raw_phrase)
        raw_phrase_line = raw_phrase_line.split(';')
        for sql in raw_phrase_line:
            po = sql.split(' ')
            if po[0] == 'CREATE':
                create.handle_create(sql)
            elif po[0] == 'COMMENT':
                comment.handle_comment(sql)
            elif po[0] == 'SELECT':
                pass
            elif po[0] == 'SET':
                pass

    print(json.dumps(all_indexes, indent=4))
