INTERFACE zif_jw_singleton_inheritance
  PUBLIC .


  TYPES:
    BEGIN OF zlty_instance,
      classname TYPE seoclsname,
      instance  TYPE REF TO zif_jw_singleton_inheritance,
    END OF zlty_instance .
  TYPES:
    zltty_instances TYPE SORTED TABLE OF zlty_instance WITH UNIQUE KEY classname .

  CLASS-DATA zgt_instances TYPE zltty_instances .

  CLASS-METHODS get_instance
    CHANGING
      !zcv_instance TYPE any
    RAISING
      zcx_simple_error .
  METHODS do_something
    RETURNING
      VALUE(zrv_text) TYPE string .
ENDINTERFACE.
