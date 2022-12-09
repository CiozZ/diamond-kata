*&---------------------------------------------------------------------*
*& Report yfl_diamond_hamburg_tt
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yfl_diamond_hamburg_tt.

CLASS diamond_kata DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_whitespaces,
             indentation TYPE i,
             spacing     TYPE i,
           END OF ty_whitespaces.

    TYPES: BEGIN OF ty_character,
             value TYPE c LENGTH 1,
           END OF ty_character.

    TYPES tt_whitespaces TYPE STANDARD TABLE OF ty_whitespaces WITH EMPTY KEY.
    TYPES tt_characters  TYPE STANDARD TABLE OF ty_character WITH EMPTY KEY.

    METHODS print
      IMPORTING seed          TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS determine_layers
      IMPORTING seed          TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS calculate_layer_whitespaces
      IMPORTING
        iv_number_of_layers TYPE i
      RETURNING
        VALUE(rt_result)    TYPE tt_whitespaces.

    METHODS generate_layer
      IMPORTING iv_character     TYPE string
                is_whitespaces   TYPE ty_whitespaces
      RETURNING VALUE(rv_result) TYPE string.

    METHODS generate_unique_layers
      IMPORTING it_characters    TYPE tt_characters
                it_whitespaces   TYPE tt_whitespaces
      RETURNING VALUE(rt_result) TYPE stringtab.

    METHODS build_diamond
      IMPORTING it_layers        TYPE stringtab
      RETURNING VALUE(rv_result) TYPE string.

  PRIVATE SECTION.

ENDCLASS.

CLASS diamond_kata IMPLEMENTATION.
  METHOD print.
    result = |A|.
  ENDMETHOD.

  METHOD determine_layers.
    FIND to_upper( seed ) IN sy-abcde MATCH OFFSET DATA(offset).
    result = substring( val = sy-abcde off = 0 len = offset + 1 ).
  ENDMETHOD.

  METHOD calculate_layer_whitespaces.
    rt_result = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = iv_number_of_layers ( indentation = iv_number_of_layers - i - 1
                                                                              spacing = 2 * i - 1 ) ).
  ENDMETHOD.

  METHOD generate_layer.
    rv_result = |{ iv_character ALIGN = RIGHT WIDTH = is_whitespaces-indentation + 1 }|.
    IF is_whitespaces-spacing <= 0.
      RETURN.
    ENDIF.
    rv_result = |{ rv_result }{ iv_character ALIGN = RIGHT WIDTH = is_whitespaces-spacing + 1 }|.
  ENDMETHOD.

  METHOD generate_unique_layers.
    DO lines( it_characters ) TIMES.
      DATA(generated) = generate_layer( iv_character = CONV #( it_characters[ sy-index ]-value ) is_whitespaces = it_whitespaces[ sy-index ] ).
      APPEND generated TO rt_result.
    ENDDO.
  ENDMETHOD.

  METHOD build_diamond.
    DATA(layers) = it_layers.
    LOOP AT layers INTO DATA(layer) .
      layer = |{ |\n| }{ layer }|.
      rv_result = |{ rv_result }{ layer }|.
    ENDLOOP.

    DATA(from) = lines( layers ) - 2.

    LOOP AT layers FROM from INTO layer TO lines( layers ).
      rv_result = |{ rv_result }{ |\n| }{ layer }|.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_diamond_kata DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO diamond_kata.

    METHODS setup.
    METHODS diamond_a               FOR TESTING.
    METHODS diamond_c               FOR TESTING.
    METHODS determine_layers_for_a  FOR TESTING.
    METHODS determine_layers_for_b  FOR TESTING.
    METHODS determine_layers_for_f  FOR TESTING.
    METHODS calc_layer_whitespace_3 FOR TESTING.
    METHODS generate_layers_first   FOR TESTING.
    METHODS generate_layers_non_first FOR TESTING.
    METHODS generate_layers FOR TESTING.
    METHODS build_diamond FOR TESTING.

ENDCLASS.


CLASS ltc_diamond_kata IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW diamond_kata( ).
  ENDMETHOD.

  METHOD diamond_a.
    DATA(result) = mo_cut->print( |A| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |A| ).
  ENDMETHOD.

  METHOD diamond_c.
    DATA(result) = mo_cut->print( |C| ).
    cl_abap_unit_assert=>assert_equals(
        act = result
        exp = |  A  \n| &&
              | B B \n| &&
              |C   C\n| &&
              | B B \n| &&
              |  A  | ).
  ENDMETHOD.

  METHOD determine_layers_for_a.
    DATA(result) = mo_cut->determine_layers( |a| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |A| ).
  ENDMETHOD.

  METHOD determine_layers_for_b.
    DATA(result) = mo_cut->determine_layers( |b| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |AB| ).
  ENDMETHOD.

  METHOD determine_layers_for_f.
    DATA(result) = mo_cut->determine_layers( |F| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |ABCDEF| ).
  ENDMETHOD.

  METHOD calc_layer_whitespace_3.
    cl_abap_unit_assert=>assert_equals( exp = VALUE diamond_kata=>tt_whitespaces( ( indentation = 2 spacing = -1 )
                                                                                  ( indentation = 1 spacing = 1 )
                                                                                  ( indentation = 0 spacing = 3 ) )
                                        act = mo_cut->calculate_layer_whitespaces( 3 ) ).
  ENDMETHOD.

  METHOD generate_layers_first.
    cl_abap_unit_assert=>assert_equals( exp = |  *|
                                        act = mo_cut->generate_layer( iv_character = |*| is_whitespaces = VALUE #( indentation = 2 spacing = -1 ) ) ).
  ENDMETHOD.

  METHOD generate_layers_non_first.
    cl_abap_unit_assert=>assert_equals( exp = |  *   *|
                                        act = mo_cut->generate_layer( iv_character = |*| is_whitespaces = VALUE #( indentation = 2 spacing = 3 ) ) ).
  ENDMETHOD.

  METHOD generate_layers.
    cl_abap_unit_assert=>assert_equals( exp = VALUE stringtab( ( | *| )
                                                               ( |  ! !| ) )
                                        act = mo_cut->generate_unique_layers( it_characters = VALUE #( ( |*| ) ( |!| ) )
                                                                              it_whitespaces = VALUE #( ( indentation = 1 spacing = -1 )
                                                                                                        ( indentation = 2 spacing = 1 ) ) ) ).
  ENDMETHOD.

  METHOD build_diamond.
    cl_abap_unit_assert=>assert_equals( exp = |A\nB\nC\nB\nA|
                                        act = mo_cut->build_diamond( it_layers = VALUE #( ( |A| ) ( |B| ) ( |C| ) ) ) ).
  ENDMETHOD.

ENDCLASS.
