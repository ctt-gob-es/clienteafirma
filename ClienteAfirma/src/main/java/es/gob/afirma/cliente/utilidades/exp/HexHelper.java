package es.gob.afirma.cliente.utilidades.exp;

class HexHelper {
	private static final HexHelper INSTANCE = new HexHelper();

	private static final char[] hexChars = new char[] { '0', '1', '2', '3',
			'4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

	private HexHelper() {
		super();
	}

	public static final HexHelper getInstance() {
		return INSTANCE;
	}

	public String toString(byte[] buff) {
		return toHex(buff);
	}

	private String toHex(byte b) {
		int v = (b < 0) ? (128 + (b & 127)) : b;
		int d0 = v % 16;
		int d1 = v / 16;

		String str = new String(new char[] { hexChars[d1], hexChars[d0] });

		// logger.debug("b= "+b+" v= "+v+" str= "+str);

		return str;
	}

	public String toHex(byte[] data) {
		final String hex;

		if (data.length > 0) {
			StringBuffer sb = new StringBuffer(data.length * 3);

			sb.append(toHex(data[0]));
			for (int i = 1; i < data.length; i++) {
				sb.append(':').append(toHex(data[i]));
			}

			hex = sb.toString();
		} else {
			hex = "";
		}

		return hex;
	}
}