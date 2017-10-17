package es.gob.afirma.server.webstart;

import java.io.IOException;


/**
 * <p>Encodes and decodes to and from Base64 notation.</p>
 * <p>Homepage: <a href="http://iharder.net/base64">http://iharder.net/base64</a>.</p>
 *
 * <p>Example:</p>
 *
 * <code>String encoded = Base64.encode( myByteArray );</code>
 * <br>
 * <code>byte[] myByteArray = Base64.decode( encoded );</code>
 *
 * <p>The <tt>options</tt> parameter, which appears in a few places, is used to pass
 * several pieces of information to the encoder. In the "higher level" methods such as
 * encodeBytes( bytes, options ) the options parameter can be used to indicate such
 * things as not inserting linefeeds,
 * and encoding using the URL-safe and Ordered dialects.</p>
 *
 * <p>Note, according to <a href="http://www.faqs.org/rfcs/rfc3548.html">RFC3548</a>,
 * Section 2.1, implementations should not add line feeds unless explicitly told
 * to do so. I've got Base64 set to this behavior now, although earlier versions
 * broke lines by default.</p>
 *
 * <p>The constants defined in Base64 can be OR-ed together to combine options, so you
 * might make a call like this:</p>
 *
 * <code>String encoded = Base64.encodeBytes( mybytes, Base64.DO_BREAK_LINES );</code>
 * <p>to compress the data before encoding it and then making the output have newline characters.</p>
 * <p>Also...</p>
 * <code>String encoded = Base64.encodeBytes( crazyString.getBytes() );</code>
 *
 * <p>
 * I am placing this code in the Public Domain. Do with it as you will.
 * This software comes with no guarantees or warranties but with
 * plenty of well-wishing instead!
 * Please visit <a href="http://iharder.net/base64">http://iharder.net/base64</a>
 * periodically to check for updates or to contribute improvements.
 * </p>
 *
 * @author Robert Harder
 * @author rob@iharder.net
 * @version 2.3.7
 */
final class Base64 {

/* ********  P U B L I C   F I E L D S  ******** */


    /** No options specified. Value is zero. */
    private static final int NO_OPTIONS = 0;

    /** Do break lines when encoding. Value is 8. */
    private static final int DO_BREAK_LINES = 8;

    /**
     * Encode using Base64-like encoding that is URL- and Filename-safe as described
     * in Section 4 of RFC3548:
     * <a href="http://www.faqs.org/rfcs/rfc3548.html">http://www.faqs.org/rfcs/rfc3548.html</a>.
     * It is important to note that data encoded this way is <em>not</em> officially valid Base64,
     * or at the very least should not be called Base64 without also specifying that is
     * was encoded using the URL- and Filename-safe dialect.
     */
     private static final int URL_SAFE = 16;



/* ********  P R I V A T E   F I E L D S  ******** */


    /** Maximum line length (76) of Base64 output. */
    private static final int MAX_LINE_LENGTH = 76;


    /** The equals sign (=) as a byte. */
    private static final byte EQUALS_SIGN = (byte)'=';

    /** The new line character (\n) as a byte. */
    private static final byte NEW_LINE = (byte)'\n';

    /** Preferred encoding. */
    private static final String PREFERRED_ENCODING = "US-ASCII"; //$NON-NLS-1$

    private static final byte WHITE_SPACE_ENC = -9; // Indicates white space in encoding
    private static final byte EQUALS_SIGN_ENC = -1; // Indicates equals sign in encoding


/* ********  S T A N D A R D   B A S E 6 4   A L P H A B E T  ******** */

    /** The 64 valid Base64 values. */
    /* Host platform me be something funny like EBCDIC, so we hardcode these values. */
    private static final byte[] STANDARD_ALPHABET = {
        (byte)'A', (byte)'B', (byte)'C', (byte)'D', (byte)'E', (byte)'F', (byte)'G',
        (byte)'H', (byte)'I', (byte)'J', (byte)'K', (byte)'L', (byte)'M', (byte)'N',
        (byte)'O', (byte)'P', (byte)'Q', (byte)'R', (byte)'S', (byte)'T', (byte)'U',
        (byte)'V', (byte)'W', (byte)'X', (byte)'Y', (byte)'Z',
        (byte)'a', (byte)'b', (byte)'c', (byte)'d', (byte)'e', (byte)'f', (byte)'g',
        (byte)'h', (byte)'i', (byte)'j', (byte)'k', (byte)'l', (byte)'m', (byte)'n',
        (byte)'o', (byte)'p', (byte)'q', (byte)'r', (byte)'s', (byte)'t', (byte)'u',
        (byte)'v', (byte)'w', (byte)'x', (byte)'y', (byte)'z',
        (byte)'0', (byte)'1', (byte)'2', (byte)'3', (byte)'4', (byte)'5',
        (byte)'6', (byte)'7', (byte)'8', (byte)'9', (byte)'+', (byte)'/'
    };

    /**
     * Translates a Base64 value to either its 6-bit reconstruction value
     * or a negative number indicating some other meaning.
     **/
    private static final byte[] STANDARD_DECODABET = {
        -9,-9,-9,-9,-9,-9,-9,-9,-9,                 // Decimal  0 -  8
        -5,-5,                                      // Whitespace: Tab and Linefeed
        -9,-9,                                      // Decimal 11 - 12
        -5,                                         // Whitespace: Carriage Return
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 14 - 26
        -9,-9,-9,-9,-9,                             // Decimal 27 - 31
        -5,                                         // Whitespace: Space
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,              // Decimal 33 - 42
        62,                                         // Plus sign at decimal 43
        -9,-9,-9,                                   // Decimal 44 - 46
        63,                                         // Slash at decimal 47
        52,53,54,55,56,57,58,59,60,61,              // Numbers zero through nine
        -9,-9,-9,                                   // Decimal 58 - 60
        -1,                                         // Equals sign at decimal 61
        -9,-9,-9,                                      // Decimal 62 - 64
        0,1,2,3,4,5,6,7,8,9,10,11,12,13,            // Letters 'A' through 'N'
        14,15,16,17,18,19,20,21,22,23,24,25,        // Letters 'O' through 'Z'
        -9,-9,-9,-9,-9,-9,                          // Decimal 91 - 96
        26,27,28,29,30,31,32,33,34,35,36,37,38,     // Letters 'a' through 'm'
        39,40,41,42,43,44,45,46,47,48,49,50,51,     // Letters 'n' through 'z'
        -9,-9,-9,-9,-9                              // Decimal 123 - 127
        ,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,       // Decimal 128 - 139
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 140 - 152
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 153 - 165
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 166 - 178
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 179 - 191
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 192 - 204
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 205 - 217
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 218 - 230
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 231 - 243
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9         // Decimal 244 - 255
    };


/* ********  U R L   S A F E   B A S E 6 4   A L P H A B E T  ******** */

    /**
     * Used in the URL- and Filename-safe dialect described in Section 4 of RFC3548:
     * <a href="http://www.faqs.org/rfcs/rfc3548.html">http://www.faqs.org/rfcs/rfc3548.html</a>.
     * Notice that the last two bytes become "hyphen" and "underscore" instead of "plus" and "slash."
     */
    private static final byte[] URL_SAFE_ALPHABET = {
      (byte)'A', (byte)'B', (byte)'C', (byte)'D', (byte)'E', (byte)'F', (byte)'G',
      (byte)'H', (byte)'I', (byte)'J', (byte)'K', (byte)'L', (byte)'M', (byte)'N',
      (byte)'O', (byte)'P', (byte)'Q', (byte)'R', (byte)'S', (byte)'T', (byte)'U',
      (byte)'V', (byte)'W', (byte)'X', (byte)'Y', (byte)'Z',
      (byte)'a', (byte)'b', (byte)'c', (byte)'d', (byte)'e', (byte)'f', (byte)'g',
      (byte)'h', (byte)'i', (byte)'j', (byte)'k', (byte)'l', (byte)'m', (byte)'n',
      (byte)'o', (byte)'p', (byte)'q', (byte)'r', (byte)'s', (byte)'t', (byte)'u',
      (byte)'v', (byte)'w', (byte)'x', (byte)'y', (byte)'z',
      (byte)'0', (byte)'1', (byte)'2', (byte)'3', (byte)'4', (byte)'5',
      (byte)'6', (byte)'7', (byte)'8', (byte)'9', (byte)'-', (byte)'_'
    };

    /**
     * Used in decoding URL- and Filename-safe dialects of Base64.
     */
    private static final byte[] URL_SAFE_DECODABET = {
      -9,-9,-9,-9,-9,-9,-9,-9,-9,                 // Decimal  0 -  8
      -5,-5,                                      // Whitespace: Tab and Linefeed
      -9,-9,                                      // Decimal 11 - 12
      -5,                                         // Whitespace: Carriage Return
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 14 - 26
      -9,-9,-9,-9,-9,                             // Decimal 27 - 31
      -5,                                         // Whitespace: Space
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,              // Decimal 33 - 42
      -9,                                         // Plus sign at decimal 43
      -9,                                         // Decimal 44
      62,                                         // Minus sign at decimal 45
      -9,                                         // Decimal 46
      -9,                                         // Slash at decimal 47
      52,53,54,55,56,57,58,59,60,61,              // Numbers zero through nine
      -9,-9,-9,                                   // Decimal 58 - 60
      -1,                                         // Equals sign at decimal 61
      -9,-9,-9,                                   // Decimal 62 - 64
      0,1,2,3,4,5,6,7,8,9,10,11,12,13,            // Letters 'A' through 'N'
      14,15,16,17,18,19,20,21,22,23,24,25,        // Letters 'O' through 'Z'
      -9,-9,-9,-9,                                // Decimal 91 - 94
      63,                                         // Underscore at decimal 95
      -9,                                         // Decimal 96
      26,27,28,29,30,31,32,33,34,35,36,37,38,     // Letters 'a' through 'm'
      39,40,41,42,43,44,45,46,47,48,49,50,51,     // Letters 'n' through 'z'
      -9,-9,-9,-9,-9                              // Decimal 123 - 127
      ,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 128 - 139
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 140 - 152
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 153 - 165
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 166 - 178
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 179 - 191
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 192 - 204
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 205 - 217
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 218 - 230
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     // Decimal 231 - 243
      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9         // Decimal 244 - 255
    };

/* ********  D E T E R M I N E   W H I C H   A L H A B E T  ******** */

    private static byte[] getAlphabet(final int options ) {
        if ((options & URL_SAFE) == URL_SAFE) {
            return URL_SAFE_ALPHABET;
        }
        return STANDARD_ALPHABET;
    }	// end getAlphabet

    private static byte[] getDecodabet(final int options) {
        if( (options & URL_SAFE) == URL_SAFE) {
            return URL_SAFE_DECODABET;
        }
        return STANDARD_DECODABET;
    }	// end getAlphabet


    private Base64(){
        /* Defeats instantiation. */
    }


/* ********  E N C O D I N G   M E T H O D S  ******** */

    private static byte[] encode3to4(final byte[] source,
    		                         final int srcOffset,
    		                         final int numSigBytes,
    		                         final byte[] destination,
    		                         final int destOffset,
    		                         final int options ) {

	final byte[] alphabet = getAlphabet( options );

        // Create buffer with zero-padding if there are only one or two
        // significant bytes passed in the array.
        // We have to shift left 24 in order to flush out the 1's that appear
        // when Java treats a value as negative that is cast from a byte to an int.
        final int inBuff =   ( numSigBytes > 0 ? source[ srcOffset     ] << 24 >>>  8 : 0 )
                     | ( numSigBytes > 1 ? source[ srcOffset + 1 ] << 24 >>> 16 : 0 )
                     | ( numSigBytes > 2 ? source[ srcOffset + 2 ] << 24 >>> 24 : 0 );

        switch( numSigBytes )
        {
            case 3:
                destination[ destOffset     ] = alphabet[ inBuff >>> 18        ];
                destination[ destOffset + 1 ] = alphabet[ inBuff >>> 12 & 0x3f ];
                destination[ destOffset + 2 ] = alphabet[ inBuff >>>  6 & 0x3f ];
                destination[ destOffset + 3 ] = alphabet[ inBuff & 0x3f ];
                return destination;

            case 2:
                destination[ destOffset     ] = alphabet[ inBuff >>> 18        ];
                destination[ destOffset + 1 ] = alphabet[ inBuff >>> 12 & 0x3f ];
                destination[ destOffset + 2 ] = alphabet[ inBuff >>>  6 & 0x3f ];
                destination[ destOffset + 3 ] = EQUALS_SIGN;
                return destination;

            case 1:
                destination[ destOffset     ] = alphabet[ inBuff >>> 18        ];
                destination[ destOffset + 1 ] = alphabet[ inBuff >>> 12 & 0x3f ];
                destination[ destOffset + 2 ] = EQUALS_SIGN;
                destination[ destOffset + 3 ] = EQUALS_SIGN;
                return destination;

            default:
                return destination;
        }   // end switch
    }   // end encode3to4

    /** Codifica un binario en Base64.
     * @param source Datos a convertir a Base64
     * @return Datos codificados como texto Base64 */
    public static String encode(final byte[] source) {
        final String encoded = encodeBytes(source, 0, source.length, NO_OPTIONS);
        assert encoded != null;
        return encoded;
    }

    /** Codifica un binario en Base64.
     * @param source Datos a convertir a Base64
     * @param urlSafe Si se establece a <code>true</code> indica que los datos se codificar&aacute;n con un alfabeto Base64
     *                susceptible de ser usado en URL, seg&uacute;n se indica en la seccti&oacute;n 4 de la RFC3548,
     *                si se establece a <code>false</code> los datos se codificar&aacute;n en Base64 normal
     * @return Datos codificados como texto Base64. */
    public static String encode(final byte[] source, final boolean urlSafe) {
        return encodeBytes( source, 0, source.length, urlSafe ? URL_SAFE : NO_OPTIONS);
    }

    /**
     * Encodes a byte array into Base64 notation.
     * <p>
     * Example: <code>encode( myData )</code> or
     * <p>
     * Example: <code>encodeBytes( myData, Base64.DO_BREAK_LINES )</code>
     *
     * @param source The data to convert
     * @param off Offset in array where conversion should begin
     * @param len Length of data to convert
     * @param options Specified options
     * @return The Base64-encoded data as a String
     * @throws IllegalArgumentException if source array, offset, or length are invalid
     * @since 2.0
     */
    private static String encodeBytes( final byte[] source, final int off, final int len, final int options ) {
        final byte[] encoded = encodeBytesToBytes( source, off, len, options );

        // Return value according to relevant encoding.
        try {
            return new String( encoded, PREFERRED_ENCODING );
        }   // end try
        catch (final java.io.UnsupportedEncodingException uue) {
            return new String( encoded );
        }   // end catch

    }   // end encodeBytes

    /**
     * Similar to {@link #encodeBytes(byte[], int, int, int)} but returns
     * a byte array instead of instantiating a String. This is more efficient
     * if you're working with I/O streams and have large data sets to encode.
     *
     *
     * @param source The data to convert
     * @param off Offset in array where conversion should begin
     * @param len Length of data to convert
     * @param options Specified options
     * @return The Base64-encoded data as a String
     * @throws IllegalArgumentException if source array, offset, or length are invalid
     * @since 2.3.1
     */
    private static byte[] encodeBytesToBytes(final byte[] source, final int off, final int len, final int options) {

        if( source == null ){
            throw new IllegalArgumentException("Cannot serialize a null array"); //$NON-NLS-1$
        }   // end if: null

        if( off < 0 ){
            throw new IllegalArgumentException("Cannot have negative offset: " + off ); //$NON-NLS-1$
        }   // end if: off < 0

        if( len < 0 ){
            throw new IllegalArgumentException("Cannot have length offset: " + len ); //$NON-NLS-1$
        }   // end if: len < 0

        if( off + len > source.length  ){
            throw new IllegalArgumentException(
        		String.format(
    				"Cannot have offset of %d and length of %d with array of length %d",  //$NON-NLS-1$
    				Integer.valueOf(off),
    				Integer.valueOf(len),
					Integer.valueOf(source.length)
				)
			);
        }   // end if: off < 0

        final boolean breakLines = (options & DO_BREAK_LINES) != 0;

        // Try to determine more precisely how big the array needs to be.
        // If we get it right, we don't have to do an array copy, and
        // we save a bunch of memory.
        int encLen = len / 3 * 4 + ( len % 3 > 0 ? 4 : 0 ); // Bytes needed for actual encoding
        if( breakLines ){
            encLen += encLen / MAX_LINE_LENGTH; // Plus extra newline characters
        }
        final byte[] outBuff = new byte[ encLen ];

        int d = 0;
        int e = 0;
        final int len2 = len - 2;
        int lineLength = 0;
        for( ; d < len2; d+=3, e+=4 ) {
            encode3to4( source, d+off, 3, outBuff, e, options );

            lineLength += 4;
            if( breakLines && lineLength >= MAX_LINE_LENGTH ) {
                outBuff[e+4] = NEW_LINE;
                e++;
                lineLength = 0;
            }   // end if: end of line
        }   // en dfor: each piece of array

        if( d < len ) {
            encode3to4( source, d+off, len - d, outBuff, e, options );
            e += 4;
        }   // end if: some padding needed


        // Only resize array if we didn't guess it right.
        if( e <= outBuff.length - 1 ){
            // If breaking lines and the last byte falls right at
            // the line length (76 bytes per line), there will be
            // one extra byte, and the array will need to be resized.
            // Not too bad of an estimate on array size, I'd say.
            final byte[] finalOut = new byte[e];
            System.arraycopy(outBuff,0, finalOut,0,e);
            return finalOut;
        }
        return outBuff;

        // end else: don't compress

    }   // end encodeBytesToBytes





/* ********  D E C O D I N G   M E T H O D S  ******** */


    /**
     * Decodes four bytes from array <var>source</var>
     * and writes the resulting bytes (up to three of them)
     * to <var>destination</var>.
     * The source and destination arrays can be manipulated
     * anywhere along their length by specifying
     * <var>srcOffset</var> and <var>destOffset</var>.
     * This method does not check to make sure your arrays
     * are large enough to accomodate <var>srcOffset</var> + 4 for
     * the <var>source</var> array or <var>destOffset</var> + 3 for
     * the <var>destination</var> array.
     * This method returns the actual number of bytes that
     * were converted from the Base64 encoding.
	 * <p>This is the lowest level of the decoding methods with
	 * all possible parameters.</p>
     *
     *
     * @param source the array to convert
     * @param srcOffset the index where conversion begins
     * @param destination the array to hold the conversion
     * @param destOffset the index where output will be put
	 * @param options alphabet type is pulled from this (standard, url-safe, ordered)
     * @return the number of decoded bytes converted
     * @throws IllegalArgumentException if srcOffset or destOffset are invalid
     *         or there is not enough room in the array.
     * @since 1.3
     */
    private static int decode4to3(final byte[] source,
    							  final int srcOffset,
    		                      final byte[] destination,
    		                      final int destOffset,
    		                      final int options) {

        // Lots of error checking and exception throwing
        if( source == null ){
            throw new IllegalArgumentException("Source array was null"); //$NON-NLS-1$
        }   // end if
        if( destination == null ){
            throw new IllegalArgumentException("Destination array was null"); //$NON-NLS-1$
        }   // end if
        if( srcOffset < 0 || srcOffset + 3 >= source.length ){
            throw new IllegalArgumentException(String.format(
        		"Source array with length %d cannot have offset of %d and still process four bytes",  //$NON-NLS-1$
        		Integer.valueOf(source.length),
        		Integer.valueOf(srcOffset)
    		));
        }   // end if
        if( destOffset < 0 || destOffset +2 >= destination.length ){
            throw new IllegalArgumentException(
        		String.format(
    				"Destination array with length %d cannot have offset of %d and still store three bytes.", //$NON-NLS-1$
    				Integer.valueOf(destination.length),
    				Integer.valueOf(destOffset)
				)
			);
        }   // end if


        final byte[] decodabet = getDecodabet( options );

        // Example: Dk==
        if( source[ srcOffset + 2] == EQUALS_SIGN ) {
            final int outBuff =   ( decodabet[ source[ srcOffset    ] ] & 0xFF ) << 18
                          | ( decodabet[ source[ srcOffset + 1] ] & 0xFF ) << 12;

            destination[ destOffset ] = (byte)( outBuff >>> 16 );
            return 1;
        }

        // Example: DkL=
        else if( source[ srcOffset + 3 ] == EQUALS_SIGN ) {
            final int outBuff =   ( decodabet[ source[ srcOffset     ] ] & 0xFF ) << 18
                          | ( decodabet[ source[ srcOffset + 1 ] ] & 0xFF ) << 12
                          | ( decodabet[ source[ srcOffset + 2 ] ] & 0xFF ) <<  6;

            destination[ destOffset     ] = (byte)( outBuff >>> 16 );
            destination[ destOffset + 1 ] = (byte)( outBuff >>>  8 );
            return 2;
        }

        // Example: DkLE
        else {
            final int outBuff =   ( decodabet[ source[ srcOffset     ] ] & 0xFF ) << 18
                          | ( decodabet[ source[ srcOffset + 1 ] ] & 0xFF ) << 12
                          | ( decodabet[ source[ srcOffset + 2 ] ] & 0xFF ) <<  6
                          | decodabet[ source[ srcOffset + 3 ] ] & 0xFF;


            destination[ destOffset     ] = (byte)( outBuff >> 16 );
            destination[ destOffset + 1 ] = (byte)( outBuff >>  8 );
            destination[ destOffset + 2 ] = (byte)outBuff;

            return 3;
        }
    }   // end decodeToBytes

    /** Descodifica datos en Base64.
     * @param source Datos codificados en Base64.
     * @param off    El &iacute;ndice inicial por el que empezar a descodificar.
     * @param len    N&uacute;mero de caracteres que descodificar.
     * @param urlSafe Si se establece a <code>true</code> indica que los datos est&aacute;n con un alfabeto Base64
     *                susceptible de ser usado en URL, seg&uacute;n se indica en la seccti&oacute;n 4 de la RFC3548,
     *                si se establece a <code>false</code> los datos deben estar en Base64 normal
     * @return Datos descodificados
     * @throws java.io.IOException si ocurre cualquier error */
    public static byte[] decode( final byte[] source, final int off, final int len, final boolean urlSafe) throws IOException {

    	int options = NO_OPTIONS;
    	if (urlSafe) {
    		options = URL_SAFE;
    	}

        // Lots of error checking and exception throwing
        if( source == null ){
            throw new IllegalArgumentException("Cannot decode null source array"); //$NON-NLS-1$
        }   // end if
        if( off < 0 || off + len > source.length ){
            throw new IllegalArgumentException(
        		String.format(
    				"Source array with length %d cannot have offset of %d and process %d bytes.", //$NON-NLS-1$
    				Integer.valueOf(source.length),
    				Integer.valueOf(off),
    				Integer.valueOf(len)
				)
			);
        }   // end if

        if( len == 0 ){
            return new byte[0];
        }
        else if( len < 4 ){
            throw new IllegalArgumentException(
        		"Base64-encoded string must have at least four characters, but length specified was " + len ); //$NON-NLS-1$
        }   // end if

        final byte[] decodabet = getDecodabet( options );

        final int    len34   = len * 3 / 4;       // Estimate on array size
        final byte[] outBuff = new byte[ len34 ]; // Upper limit on size of output
        int    outBuffPosn = 0;             // Keep track of where we're writing

        final byte[] b4        = new byte[4];     // Four byte buffer from source, eliminating white space
        int    b4Posn    = 0;               // Keep track of four byte input buffer
        int    i         = 0;               // Source array counter
        byte   sbiDecode = 0;               // Special value from DECODABET

        for( i = off; i < off+len; i++ ) {  // Loop through source

            sbiDecode = decodabet[ source[i]&0xFF ];

            // White space, Equals sign, or legit Base64 character
            // Note the values such as -5 and -9 in the
            // DECODABETs at the top of the file.
            if( sbiDecode >= WHITE_SPACE_ENC )  {
                if( sbiDecode >= EQUALS_SIGN_ENC ) {
                    b4[ b4Posn++ ] = source[i];         // Save non-whitespace
                    if( b4Posn > 3 ) {                  // Time to decode?
                        outBuffPosn += decode4to3( b4, 0, outBuff, outBuffPosn, options );
                        b4Posn = 0;

                        // If that was the equals sign, break out of 'for' loop
                        if( source[i] == EQUALS_SIGN ) {
                            break;
                        }   // end if: equals sign
                    }   // end if: quartet built
                }   // end if: equals sign or better
            }   // end if: white space, equals sign or better
            else {
                // There's a bad input character in the Base64 stream.
                throw new java.io.IOException(
            		String.format(
        				"Bad Base64 input character decimal %d in array position %d", //$NON-NLS-1$
        				Integer.valueOf(source[i]&0xFF),
        				Integer.valueOf(i)
    				)
				);
            }   // end else:
        }   // each input character

        final byte[] out = new byte[ outBuffPosn ];
        System.arraycopy( outBuff, 0, out, 0, outBuffPosn );
        return out;
    }   // end decode

    /** Descodifica datos en Base64.
     * @param str Cadena de caracteres en formato Base64
     * @return Datos descodificados
     * @throws java.io.IOException si ocurre cualquier error */
    public static byte[] decode(final String str) throws java.io.IOException {
        return decode(str, false);
    }

    /** Descodifica datos en Base64.
     * @param str Cadena de caracteres en formato Base64
     * @param urlSafe Si se establece a <code>true</code> indica que los datos est&aacute;n con un alfabeto Base64
     *                susceptible de ser usado en URL, seg&uacute;n se indica en la seccti&oacute;n 4 de la RFC3548,
     *                si se establece a <code>false</code> los datos deben estar en Base64 normal
     * @return Datos descodificados
     * @throws java.io.IOException si ocurre cualquier error */
    public static byte[] decode(final String str, final boolean urlSafe) throws java.io.IOException {
        if( str == null ){
            throw new IllegalArgumentException("Input string was null"); //$NON-NLS-1$
        }
        byte[] bytes;
        try {
            bytes = str.getBytes(PREFERRED_ENCODING);
        }
        catch(final java.io.UnsupportedEncodingException uee ) {
            bytes = str.getBytes();
        }
        return decode( bytes, 0, bytes.length, urlSafe);
    }

    /** Caracteres aceptados en una codificaci&oacute;n Base64 seg&uacute;n la
     * <a href="http://www.faqs.org/rfcs/rfc3548.html">RFC 3548</a>. Importante:
     * A&ntilde;adimos el car&aacute;cter &tilde; porque en ciertas
     * codificaciones de Base64 est&aacute; aceptado, aunque no es nada
     * recomendable */
    private static final String BASE_64_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz=_-\t\n+/0123456789\r~"; //$NON-NLS-1$

    /** Comprueba si un array de datos es una cadena en base 64.
     * @param data Datos a comprobar si podr&iacute;an o no ser Base64.
     * @return <code>true</code> si los datos proporcionado pueden ser una
     *         codificaci&oacute;n base64 de un original binario (que no tiene
     *         necesariamente porqu&eacute; serlo), <code>false</code> en caso
     *         contrario. */
    public static boolean isBase64(final byte[] data) {

        int count = 0;

        // Comprobamos que todos los caracteres de la cadena pertenezcan al
        // alfabeto base 64

        for (int i = 0; i < data.length; i++) {
        	final char b = (char) data[i];
        	if (BASE_64_ALPHABET.indexOf(b) == -1) {
        		return false;
        	}

        	// Solo puede aparecer el signo igual en como 2 ultimos caracteres de la cadena
        	if (b == '=' && i < data.length - 2) {
        		return false;
        	}

        	if (b != '\n' && b != '\r') {
        		count++;
        	}
        }

        // Comprobamos que la cadena (sin contar los saltos de linea) tenga una longitud multiplo de 4 caracteres
        return count % 4 == 0;
    }

    /** Comprueba si una cadena de texto es una cadena en base 64.
     * @param data Cadena de texto a comprobar si podr&iacute;an o no ser Base64.
     * @return <code>true</code> si los datos proporcionado pueden ser una
     *         codificaci&oacute;n base64 de un original binario (que no tiene
     *         necesariamente porqu&eacute; serlo), <code>false</code> en caso
     *         contrario. */
    public static boolean isBase64(final String data) {

        int count = 0;

        // Comprobamos que todos los caracteres de la cadena pertenezcan al
        // alfabeto base 64

        for (int i = 0; i < data.length(); i++) {
        	final char b = data.charAt(i);
        	if (BASE_64_ALPHABET.indexOf(b) == -1) {
        		return false;
        	}

        	// Solo puede aparecer el signo igual en como 2 ultimos caracteres de la cadena
        	if (b == '=' && i < data.length() - 2) {
        		return false;
        	}

        	if (b != '\n' && b != '\r') {
        		count++;
        	}
        }

        // Comprobamos que la cadena (sin contar los saltos de linea) tenga una longitud multiplo de 4 caracteres
        return count % 4 == 0;
    }

}   // end class Base64
