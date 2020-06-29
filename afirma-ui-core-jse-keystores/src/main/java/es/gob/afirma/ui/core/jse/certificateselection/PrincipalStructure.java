package es.gob.afirma.ui.core.jse.certificateselection;

import java.nio.charset.StandardCharsets;
import java.util.Locale;

import javax.security.auth.x500.X500Principal;

/**
 * Estructura del Principal de un certificado de la que se pueden extraer los distintos RDN.
 */
public class PrincipalStructure {

	/** RDN commonName. */
	static final String CN = "CN"; //$NON-NLS-1$

	/** RDN localityName. */
	static final String L = "L"; //$NON-NLS-1$

	/** RDN stateOrProvinceName. */
	static final String ST = "ST"; //$NON-NLS-1$

	/** RDN organizationName. */
	static final String O = "O"; //$NON-NLS-1$

	/** RDN organizationalUnitName. */
	static final String OU = "OU"; //$NON-NLS-1$

	/** RDN countryName. */
	static final String C = "C"; //$NON-NLS-1$

	/** RDN streetAddress. */
	static final String STREET = "STREET"; //$NON-NLS-1$

	/** RDN domainComponent. */
	static final String DC = "DC"; //$NON-NLS-1$

	/** RDN userid. */
	static final String UID = "UID"; //$NON-NLS-1$

	/** RDN serialNumber. */
	static final String SERIALNUMBER = "2.5.4.5"; //$NON-NLS-1$

	/** RDN surname. */
	static final String SURNAME = "2.5.4.4"; //$NON-NLS-1$

	/** RDN givenName. */
	static final String GIVENNAME = "2.5.4.42"; //$NON-NLS-1$

	/** RDN title. */
	static final String TITLE = "2.5.4.12"; //$NON-NLS-1$

	/** RDN pseudonym. */
	static final String PSEUDONYM = "2.5.4.65"; //$NON-NLS-1$

	/** RDN organizationUnit. */
	static final String ORGANIZATION_IDENTIFIER = "2.5.4.97"; //$NON-NLS-1$

	/** RDN description. */
	static final String DESCRIPTION = "2.5.4.13"; //$NON-NLS-1$

	private final String principal;

	public PrincipalStructure(final X500Principal x500Principal) {
		this.principal = x500Principal.getName(X500Principal.RFC2253);
	}

	/** Recupera el valor de un RDN (<i>Relative Distinguished Name</i>) del principal. El valor de retorno no incluye
     * el nombre del RDN, el igual, ni las posibles comillas que envuelvan el valor.
     * La funci&oacute;n no es sensible a la capitalizaci&oacute;n del RDN. Si no se encuentra, se devuelve {@code null}.
     * @param rdn RDN que deseamos encontrar.
     * @return Valor del RDN indicado o {@code null} si no se encuentra. */
    public String getRDNvalue(final String rdn) {

        int offset1 = 0;
        while ((offset1 = this.principal.toLowerCase(Locale.US).indexOf(rdn.toLowerCase(), offset1)) != -1) {

            if (offset1 > 0 && this.principal.charAt(offset1-1) != ',' && this.principal.charAt(offset1-1) != ' ') {
                offset1++;
                continue;
            }

            offset1 += rdn.length();
            while (offset1 < this.principal.length() && this.principal.charAt(offset1) == ' ') {
                offset1++;
            }

            if (offset1 >= this.principal.length()) {
                return null;
            }

            if (this.principal.charAt(offset1) != '=') {
                continue;
            }

            offset1++;
            while (offset1 < this.principal.length() && this.principal.charAt(offset1) == ' ') {
                offset1++;
            }

            if (offset1 >= this.principal.length()) {
                return ""; //$NON-NLS-1$
            }

            int offset2;
            if (this.principal.charAt(offset1) == ',') {
                return ""; //$NON-NLS-1$
            }
            else if (this.principal.charAt(offset1) == '"') {
                offset1++;
                if (offset1 >= this.principal.length()) {
                    return ""; //$NON-NLS-1$
                }

                offset2 = this.principal.indexOf('"', offset1);
                if (offset2 == offset1) {
                    return ""; //$NON-NLS-1$
                }
                else if (offset2 != -1) {
                    return recoverText(this.principal.substring(offset1, offset2));
                }
                else {
                    return recoverText(this.principal.substring(offset1));
                }
            }
            else {
                offset2 = this.principal.indexOf(',', offset1);
                if (offset2 != -1) {
                	return recoverText(this.principal.substring(offset1, offset2).trim());
                }
                return recoverText(this.principal.substring(offset1).trim());
            }
        }

        return null;
    }

    /**
     * Devuelve el texto de entrada o, si este se corresponde con un atributo BER
     * codificado en hexadecimal, el texto contenido en el mismo.
     * @param value Cadena de texto que hay que devolver directamente o decodificada
     * si se trataba de un atributo BER en hexadecimal.
     * @return El mismo texto de entrada o, si es un atributo BER codificado en hexadecimal,
     * el texto que este contiene.
     */
    private static String recoverText(final String value) {

    	// Los 5 primeros caracteres se tienen que corresponder con '#' y la cabecera
    	// del atributo BER. Si no, no es un valor valido para decodificar y se devuelve
    	// tal cual
    	if (value.length() < 5 || value.charAt(0) != '#') {
    		return value;
    	}

		return decodeHexadecimal(value.substring(5));
    }


	private static String decodeHexadecimal(final String hexadecimal) {
		final byte[] bytes = hexStringToByteArray(hexadecimal);
		return new String(bytes, StandardCharsets.UTF_8);
	}

	public static byte[] hexStringToByteArray(final String hex) {
	    final int l = hex.length();
	    final byte[] data = new byte[l/2];
	    for (int i = 0; i < l; i += 2) {
	        data[i/2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4)
	                             + Character.digit(hex.charAt(i+1), 16));
	    }
	    return data;
	}
}
