CLASS zcl_challenge_week_04 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF artists_type,
             artist_id   TYPE string,
             artist_name TYPE string,
           END OF artists_type.
    TYPES: artists TYPE STANDARD TABLE OF artists_type WITH KEY artist_id.
    TYPES: BEGIN OF albums_type,
             artist_id  TYPE string,
             album_id   TYPE string,
             album_name TYPE string,
           END OF albums_type.
    TYPES: albums TYPE STANDARD TABLE OF albums_type WITH KEY artist_id album_id.
    TYPES: BEGIN OF songs_type,
             artist_id TYPE string,
             album_id  TYPE string,
             song_id   TYPE string,
             song_name TYPE string,
           END OF songs_type.
    TYPES: songs TYPE STANDARD TABLE OF songs_type WITH KEY artist_id album_id song_id.

    TYPES: BEGIN OF song_nested_type,
             song_id   TYPE string,
             song_name TYPE string,
           END OF song_nested_type.
    TYPES: BEGIN OF album_song_nested_type,
             album_id   TYPE string,
             album_name TYPE string,
             songs      TYPE STANDARD TABLE OF song_nested_type WITH KEY song_Id,
           END OF album_song_nested_type.
    TYPES: BEGIN OF artist_album_nested_type,
             artist_id   TYPE string,
             artist_name TYPE string,
             albums      TYPE STANDARD TABLE OF album_song_nested_type WITH KEY album_id,
           END OF artist_album_nested_type.
    TYPES: nested_data TYPE STANDARD TABLE OF artist_album_nested_type WITH KEY artist_id.

    INTERFACES: if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      fill_artist
        RETURNING
          VALUE(return_artist) TYPE artists,
      fill_albums
        RETURNING
          VALUE(return_album) TYPE albums,
      fill_songs
        RETURNING
          VALUE(return_song) TYPE songs,
      update_data
        IMPORTING
          !im_artists        TYPE artists
          !im_albums         TYPE albums
          !im_songs          TYPE songs
        RETURNING
          VALUE(update_data) TYPE nested_data.
ENDCLASS.



CLASS zcl_challenge_week_04 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(artist_content) = fill_artist( ).
    DATA(albums_content) = fill_albums( ).
    DATA(songs_content) = fill_songs( ).

    DATA(updated_nested_data) = update_data( im_artists = artist_content
                                             im_albums = albums_content
                                             im_songs = songs_content ).

    out->write( updated_nested_data ).
  ENDMETHOD.

  METHOD fill_artist.
    return_artist = VALUE #( ( artist_id = 1 artist_name = 'Godsmack' )
                      ( artist_id = 2 artist_name = 'Shinedown' ) ).
  ENDMETHOD.

  METHOD fill_albums.
    return_album = VALUE #( ( artist_id = 1 album_id = 1 album_name = 'Faceless' )
                     ( artist_id = 1 album_id = 2 album_name = 'When Legends Rise' )
                     ( artist_id = 2 album_id = 1 album_name = 'The Sound of Madness' )
                     ( artist_id = 2 album_id = 2 album_name = 'Planet Zero' ) ).
  ENDMETHOD.

  METHOD fill_songs.
    return_song = VALUE #( ( artist_id = 1 album_id = 1 song_id = 1 song_name = 'Straight Out of Line')
                           ( artist_id = 1 album_id = 1 song_id = 2 song_name = 'Changes')
                           ( artist_id = 1 album_id = 2 song_id = 1 song_name = 'Bullet Proof')
                           ( artist_id = 1 album_id = 2 song_id = 2 song_name = 'Under Your Scars')
                           ( artist_id = 2 album_id = 1 song_id = 1 song_name = 'Second Chance')
                           ( artist_id = 2 album_id = 1 song_id = 2 song_name = 'Breaking Inside')
                           ( artist_id = 2 album_id = 2 song_id = 1 song_name = 'Dysfunctional You')
                           ( artist_id = 2 album_id = 2 song_id = 2 song_name = 'Daylight')
                           ).
  ENDMETHOD.

  METHOD update_data.

    update_data = VALUE #( FOR <artists> IN im_artists ( artist_id = <artists>-artist_id
                                                         artist_name = <artists>-artist_name
                                                         albums = VALUE #( FOR <albums> IN im_albums
                                                                                     WHERE ( artist_id = <artists>-artist_id )
                                                                                           ( album_id = <albums>-album_id
                                                                                             album_name = <albums>-album_name
                                                                                             songs = VALUE #( FOR <songs> IN im_songs
                                                                                                                       WHERE ( artist_id = <artists>-artist_id
                                                                                                                         AND   album_id = <albums>-album_id )
                                                                                                                       ( song_id = <songs>-song_id
                                                                                                                         song_name = <songs>-song_name ) )
                                                                                                                        ) ) ) ).
*  LET new_album = VALUE artist_album_nested_type-albums
*    update_data = VALUE #( FOR <artists> IN im_artists  ( artist_id = <artists>-artist_id
*                                                         artist_name = <artists>-artist_name )
*                                                         albums = CORRESPONDING artist_album_nested_type-albums( im_albums[ artist_id = <artists>-artist_id ] ) ).

*     update_data = VALUE #(  )
*    update_data = VALUE #( FOR <artists> IN im_artists WHERE ( artist_id = <artists>-artist_id ) (  )
  ENDMETHOD.

ENDCLASS.
