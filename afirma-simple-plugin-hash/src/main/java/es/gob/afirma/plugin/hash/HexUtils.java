package es.gob.afirma.plugin.hash;

public class HexUtils {

	private static final char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray(); //$NON-NLS-1$

	private static final String HEX_SUFFIX = "h"; //$NON-NLS-1$

	public static byte[] hexStringToByteArray(final String s) {
	    final int len = s.length() % 2 == 1 && s.endsWith(HEX_SUFFIX) ? s.length() - 1 : s.length();
	    final byte[] data = new byte[len / 2];
	    for (int i = 0; i < len; i += 2) {
	        data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i+1), 16));
	    }
	    return data;
	}

	public static String byteArrayToHexString(final byte[] data) {
	    return byteArrayToHexString(data, false);
	}

	public static String byteArrayToHexString(final byte[] data, final boolean suffixed) {
		final int stringLength = suffixed ? data.length * 2 + 1 : data.length * 2;
	    final char[] hexChars = new char[stringLength];
	    for (int j = 0; j < data.length; j++) {
	        final int v = data[j] & 0xFF;
	        hexChars[j * 2] = HEX_ARRAY[v >>> 4];
	        hexChars[j * 2 + 1] = HEX_ARRAY[v & 0x0F];
	    }
	    if (suffixed) {
	    	hexChars[hexChars.length - 1] = HEX_SUFFIX.charAt(0);
	    }
	    return new String(hexChars);
	}

	public static boolean isHexadecimal(final byte[] data) {
		if (data == null || data.length == 0) {
			return false;
		}
		final String strData = new String(data);

		final boolean suffixed = strData.endsWith("h"); //$NON-NLS-1$
		return	suffixed &&
					(data.length - 1) % 2 == 0 &&
					strData.substring(0, strData.length() - 1).matches("^[0-9a-fA-F]+$") || //$NON-NLS-1$
				!suffixed &&
					data.length % 2 == 0 &&
					strData.matches("^[0-9a-fA-F]+$"); //$NON-NLS-1$
    }
}
