INTERFACE zif_person_builder_how
  PUBLIC .

  METHODS:
    set_name
      IMPORTING
        VALUE(iv_name)   TYPE string
      RETURNING
        VALUE(ro_object) TYPE REF TO zcl_person_builder_how,

    set_age
      IMPORTING
        VALUE(iv_age)    TYPE i
      RETURNING
        VALUE(ro_object) TYPE REF TO zcl_person_builder_how.

  METHODS:
    get_person
      RETURNING
        VALUE(ro_object) TYPE REF TO zcl_person_how.

ENDINTERFACE.
