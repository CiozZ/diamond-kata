*&---------------------------------------------------------------------*
*& Report yfl_diamond_kata
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yfl_diamond_kata.

CLASS ltc_diamonds DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_letter,
             value TYPE c LENGTH 1,
           END OF ty_letter.

    TYPES tt_letters TYPE STANDARD TABLE OF ty_letter WITH EMPTY KEY.

    METHODS:
      _determine_letter_number
        IMPORTING iv_letter        TYPE c
        RETURNING VALUE(rv_result) TYPE i,
      _determine_letters_list
        IMPORTING
          iv_seed_letter   TYPE c
        RETURNING
          VALUE(rt_result) TYPE tt_letters.

    METHODS:
      determine_layers_for_a FOR TESTING,
      determine_layers_for_e FOR TESTING,
      determine_layers_for_g FOR TESTING,
      determine_layers_for_z FOR TESTING,
      determine_layers_for_1 FOR TESTING,
      determine_letters_list_till_e FOR TESTING.
ENDCLASS.


CLASS ltc_diamonds IMPLEMENTATION.

  METHOD _determine_letter_number.
    CHECK iv_letter CA sy-abcde.
    FIND iv_letter IN sy-abcde MATCH OFFSET rv_result.
    rv_result = rv_result + 1.
  ENDMETHOD.

  METHOD determine_layers_for_e.
    cl_abap_unit_assert=>assert_equals( act = _determine_letter_number( 'E' ) exp = 5 ).
  ENDMETHOD.

  METHOD determine_layers_for_g.
    cl_abap_unit_assert=>assert_equals( act = _determine_letter_number( 'G' ) exp = 7 ).
  ENDMETHOD.

  METHOD determine_layers_for_z.
    cl_abap_unit_assert=>assert_equals( act = _determine_letter_number( 'Z' ) exp = 26 ).
  ENDMETHOD.

  METHOD determine_layers_for_a.
    cl_abap_unit_assert=>assert_equals( act = _determine_letter_number( 'A' ) exp = 1 ).
  ENDMETHOD.

  METHOD determine_layers_for_1.
    cl_abap_unit_assert=>assert_equals( act = _determine_letter_number( '3' ) exp = 0 ).
  ENDMETHOD.

  METHOD determine_letters_list_till_e.
    DATA lt_letters_list TYPE tt_letters.
    lt_letters_list = VALUE #(
     ( |A| ) ( |B| ) ( |C| ) ( |D| ) ( |E| )
    ).

    cl_abap_unit_assert=>assert_equals( exp = lt_letters_list act = _determine_letters_list( |E| ) ).
  ENDMETHOD.

  METHOD _determine_letters_list.
    DATA(lv_position) = _determine_letter_number( iv_seed_letter ).
    rt_result = VALUE #( FOR i = 0 THEN i + 1 WHILE i < lv_position
                        ( value = substring( val = sy-abcde off = i len = 1 ) ) ).
  ENDMETHOD.

ENDCLASS.
