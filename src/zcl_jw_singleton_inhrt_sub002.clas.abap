CLASS zcl_jw_singleton_inhrt_sub002 DEFINITION
  PUBLIC
  INHERITING FROM zcl_jw_singleton_inhrt_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_jw_singleton_inheritance~do_something
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JW_SINGLETON_INHRT_SUB002 IMPLEMENTATION.


  METHOD do_something.
    zrv_text = | returned from subclass TWO |.
  ENDMETHOD.
ENDCLASS.
