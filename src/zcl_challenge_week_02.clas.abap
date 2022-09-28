CLASS zcl_challenge_week_02 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    TYPES:
      BEGIN OF alphatab_type,
        cola TYPE string,
        colb TYPE string,
        colc TYPE string,
      END OF alphatab_type,

      BEGIN OF numtab_type,
        col1 TYPE string,
        col2 TYPE string,
        col3 TYPE string,
      END OF numtab_type,

      BEGIN OF combined_data_type,
        colx TYPE string,
        coly TYPE string,
        colz TYPE string,
      END OF combined_data_type,

      alphas        TYPE STANDARD TABLE OF alphatab_type WITH EMPTY KEY,
      nums          TYPE STANDARD TABLE OF numtab_type WITH EMPTY KEY,
      combined_data TYPE STANDARD TABLE OF combined_data_type WITH EMPTY KEY.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      fill_alphas
        RETURNING
          VALUE(alphas) TYPE alphas,
      fill_nums
        RETURNING
          VALUE(nums) TYPE nums,
      perform_combination_other_soln
        IMPORTING
          !alphas              TYPE alphas
          !nums                TYPE nums
        RETURNING
          VALUE(combined_data) TYPE combined_data,
      perform_combination
        IMPORTING
          !alphas              TYPE alphas
          !nums                TYPE nums
        RETURNING
          VALUE(combined_data) TYPE combined_data.
ENDCLASS.



CLASS zcl_challenge_week_02 IMPLEMENTATION.

  METHOD fill_alphas.
    alphas = VALUE #( ( cola = 'A' colb = 'B' colc = 'C' )
                      ( cola = 'D' colb = 'E' colc = 'F' )
                      ( cola = 'G' colb = 'H' colc = 'I' ) ).
  ENDMETHOD.

  METHOD fill_nums.
    nums = VALUE #( ( col1 = '1' col2 = '2' col3 = '3' )
                    ( col1 = '4' col2 = '5' col3 = '6' )
                    ( col1 = '7' col2 = '8' col3 = '9' ) ).
  ENDMETHOD.

  METHOD perform_combination_other_soln.

*    combined_data = VALUE #( FOR <combined_data> IN alphas
*                             colx = <combined_data>-cola && nums[ sy-tabix ]-col1
*                             coly = <combined_data>-colb && nums[ sy-tabix ]-col2
*                             colz = <combined_data>-colc && nums[ sy-tabix ]-col3 (  ) ).

    "working
*    combined_data = VALUE #( FOR <alphas> IN alphas INDEX INTO index
*                             FOR <nums> IN nums FROM index TO index
*                             ( colx = <alphas>-cola && <nums>-col1
*                               coly = <alphas>-colb && <nums>-col2
*                               colz = <alphas>-colc && <nums>-col3 ) ).

*     combined_data = VALUE combined_data(
*      FOR ls_alpha IN alphas INDEX INTO lv_index
*      FOR ls_num IN nums FROM lv_index TO lv_index
*      LET ls_comb = VALUE combined_data_type(
*      colx = |{ ls_alpha-cola }| & |{ ls_num-col1 }|
*      coly = |{ ls_alpha-colb }| & |{ ls_num-col2 }|
*      colz = |{ ls_alpha-colc }| & |{ ls_num-col3 }| )
*      IN ( CORRESPONDING #( ls_comb ) ) ).

    "other solution
    combined_data = VALUE combined_data(
      FOR <alpha2> IN alphas INDEX INTO lv_index
      FOR <num2> IN nums FROM lv_index TO lv_index
      LET new_combined_data = VALUE combined_data_type(
      colx = |{ <alpha2>-cola }| & |{ <num2>-col1 }|
      coly = |{ <alpha2>-colb }| & |{ <num2>-col2 }|
      colz = |{ <alpha2>-colc }| & |{ <num2>-col3 }| )
      IN ( CORRESPONDING #( new_combined_data ) ) ).

    "
*    LOOP AT combined_data ASSIGNING FIELD-SYMBOL(<fs_comb>).
*      DATA(new_index) = sy-tabix.
*      <fs_comb>-colx = |{ <fs_comb>-colx }{ nums[ new_index ]-col1 }|.
*      <fs_comb>-coly = |{ <fs_comb>-coly }{ nums[ new_index ]-col2 }|.
*      <fs_comb>-colz = |{ <fs_comb>-colz }{ nums[ new_index ]-col3 }|.
*    ENDLOOP.

*    LOOP AT alphas ASSIGNING FIELD-SYMBOL(<alpha>).
*      TRY.
*        APPEND VALUE combined_data_type(
*          colx = <alpha>-cola && nums[ sy-tabix ]-col1
*          coly = <alpha>-colb && nums[ sy-tabix ]-col2
*          colz = <alpha>-colc && nums[ sy-tabix ]-col3
*        ) TO combined_data.
*      CATCH cx_sy_itab_line_not_found.
*      ENDTRY.
*    ENDLOOP.

    DATA: combined_struc TYPE combined_data_type.
    LOOP AT alphas REFERENCE INTO DATA(alpha).
*      combined_struc-colx = |{ alpha->cola }{ nums[ sy-tabix ]-col1 }|.
*      combined_struc-coly = |{ alpha->colb }{ nums[ sy-tabix ]-col2 }|.
*      combined_struc-colz = |{ alpha->colc }{ nums[ sy-tabix ]-col3 }|.

      APPEND VALUE #( colx = |{ alpha->cola }{ nums[ sy-tabix ]-col1 }|
                      coly = |{ alpha->colb }{ nums[ sy-tabix ]-col2 }|
                      colz = |{ alpha->colc }{ nums[ sy-tabix ]-col3 }| ) TO combined_data.
    ENDLOOP.
  ENDMETHOD.

  METHOD perform_combination.
    combined_data = VALUE #( FOR <alphas> IN alphas INDEX INTO index
                             FOR <nums> IN nums FROM index TO index
                             ( colx = <alphas>-cola && <nums>-col1
                               coly = <alphas>-colb && <nums>-col2
                               colz = <alphas>-colc && <nums>-col3 ) ).
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    DATA(alphas) = fill_alphas( ).
    DATA(nums) = fill_nums( ).

    DATA(combined_data) = perform_combination( alphas = alphas
                                               nums   = nums  ).

    DATA(test_data) = perform_combination_other_soln(
                        alphas = alphas
                        nums   = nums
                      ).
    out->write( alphas ).
    out->write( nums ).
    out->write( combined_data ).
  ENDMETHOD.
ENDCLASS.
