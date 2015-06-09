package es.gob.afirma.triphase.server.document;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** Fachada simulada de gestor documental.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
final class FakeDocumentManager implements DocumentManager {

	private static final String PDF_DOC = "TEST_PDF.pdf"; //$NON-NLS-1$
	private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** {@inheritDoc} */
	@Override
	public byte[] getDocument(final String id, final X509Certificate cert, final Properties config) throws IOException {
		return AOUtil.getDataFromInputStream(this.getClass().getResourceAsStream(PDF_DOC));
	}

	/** {@inheritDoc} */
	@Override
	public String storeDocument(final String id, final X509Certificate cert, final byte[] data, final Properties config) throws IOException {
		final File tempFile = File.createTempFile("fakeDocumentRetriever-" + id, ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(tempFile);
			fos.write(data);
			fos.close();
		}
		catch (final IOException e) {
			LOGGER.severe("Error al almacenar los datos en el fichero '" + tempFile.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			if (fos != null) {
				try {
					fos.close();
				}
				catch (final IOException e2) {
					LOGGER.warning("El fichero queda sin cerrar"); //$NON-NLS-1$
				}
			}
			throw e;
		}
		LOGGER.info("Guardamos la firma generada en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
		return "id-fake-" + id; //$NON-NLS-1$
	}
}
