CLASS zcl_person_builder_how DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_person_builder_how.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      age  TYPE i,
      name TYPE string.
ENDCLASS.



CLASS zcl_person_builder_how IMPLEMENTATION.
  METHOD zif_person_builder_how~get_person.
    ro_object = NEW zcl_person_how( iv_age = me->age
                                    iv_name = me->name ).
  ENDMETHOD.

  METHOD zif_person_builder_how~set_age.
    me->age = iv_age.
    ro_object = me.
  ENDMETHOD.

  METHOD zif_person_builder_how~set_name.
    me->name = iv_name.
    ro_object = me.
  ENDMETHOD.

ENDCLASS.
