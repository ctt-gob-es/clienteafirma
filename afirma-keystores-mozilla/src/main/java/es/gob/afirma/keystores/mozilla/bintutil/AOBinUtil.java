package es.gob.afirma.keystores.mozilla.bintutil;

/** M&eacute;todos generales de utilidad.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @version 0.3 */
final class AOBinUtil {

	private AOBinUtil() {}

	 static int getDWord(final byte[] d, final int off) {
		 int offset = off;
		 byte[] data = d.clone();
		 data = new byte[] {
			data[offset + 7],
			data[offset + 6],
			data[offset + 5],
			data[offset + 4],
			data[offset + 3],
			data[offset + 2],
			data[offset + 1],
			data[offset + 0]
		 };
		 offset = 0;
		 int l = 0;
		 for (int k = 0; k < 8; k++) {
			l = l << 8 | data[offset++] & 0xFF;
		}
		 return l;
	 }


	 static int getU2(final byte[] data, final int offset) {
		  return (data[offset + 1] & 0xff) << 8 | data[offset] & 0xff;
	 }

	 static int getInt(final byte[] d, final int off) {
		 int offset = off;
		 byte[] data = d.clone();
		 data = new byte[] { data[offset + 3], data[offset + 2], data[offset + 1], data[offset]};
		 offset = 0;
		 int l = 0;
		 for (int k = 0; k < 4; k++) {
			l = l << 8 | data[offset++] & 0xFF;
		 }
		 return l;
	 }
}
