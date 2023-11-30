INTERFACE zif_person_how
  PUBLIC .

  TYPES:
    BEGIN OF ty_person_details,
      name TYPE string,
      age  TYPE i,
    END OF ty_person_details.
ENDINTERFACE.
