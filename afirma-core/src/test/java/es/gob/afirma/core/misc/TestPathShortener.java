package es.gob.afirma.core.misc;

/** Pruebas de acortado de ruta.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestPathShortener {

	/** Main para prueba directa.
	 * @param args No se usa. */
	public static void main(final String args[]) {

		final String t = "C:\\Documents and Settings\\All Users\\Application Data\\Apple Computer\\iTunes\\SC Info\\SC Info.txt"; //$NON-NLS-1$

		System.out.println(AOFileUtils.pathLengthShortener(t, 60));
		System.out.println(AOFileUtils.pathLengthShortener("C:\\temp", 20)); //$NON-NLS-1$
		System.out.println(AOFileUtils.pathLengthShortener("C:\\1\\2\\3\\4\\5\\test.txt", 20)); //$NON-NLS-1$

		System.out.println(AOFileUtils.pathLengthShortener("C:/1/2/testfile.txt", 15)); //$NON-NLS-1$
		System.out.println(AOFileUtils.pathLengthShortener("C:/1/2/3/4/5/test.txt", 15)); //$NON-NLS-1$
		System.out.println(AOFileUtils.pathLengthShortener("\\\\server\\p1\\p2\\p3\\p4\\p5\\p6", 20)); //$NON-NLS-1$
		System.out.println(AOFileUtils.pathLengthShortener("http://www.rgagnon.com/p1/p2/p3/p4/p5/pb.html", 20)); //$NON-NLS-1$
	}

}
