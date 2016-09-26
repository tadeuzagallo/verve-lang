module Opcode where

data Opcode = Op_ret
            | Op_bind
            | Op_push
            | Op_call
            | Op_jz
            | Op_jmp
            | Op_create_closure
            | Op_load_string
            | Op_push_arg
            | Op_lookup
            | Op_exit
            | Op_create_lex_scope
            | Op_release_lex_scope
            | Op_put_to_scope
            | Op_alloc_obj
            | Op_alloc_list
            | Op_obj_store_at
            | Op_obj_tag_test
            | Op_obj_load
            | Op_stack_alloc
            | Op_stack_store
            | Op_stack_load
            | Op_stack_free
            deriving (Enum)
