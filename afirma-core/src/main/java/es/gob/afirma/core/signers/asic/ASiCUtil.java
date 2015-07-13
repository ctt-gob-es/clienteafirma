package es.gob.afirma.core.signers.asic;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.MimeHelper;

/** Utilidades para firmas ASiC.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class ASiCUtil {

	private static final String ASIC_S_MIMETYPE = "application/vnd.etsi.asic-s+zip"; //$NON-NLS-1$

	private static final String DEFAULT_DATAOBJECT_EXTENSION = "bin"; //$NON-NLS-1$

	/** Nombre por defecto del fichero de firmas binarias. */
	public static final String ENTRY_NAME_BINARY_SIGNATURE = "META-INF/signature.p7s"; //$NON-NLS-1$

	/** Nombre por defecto del fichero de firmas XML. */
	public static final String ENTRY_NAME_XML_SIGNATURE = "META-INF/signatures.xml"; //$NON-NLS-1$

	private static final String ENTRY_NAME_MIMETYPE = "mimetype"; //$NON-NLS-1$

	private ASiCUtil() {
		// No instanciable
	}

	/** Obtiene el nombre por defecto a asignar al objeto de datos dentro de un contenedor ASiC-S.
	 * @param data Contenido del objeto de datos.
	 * @return Nombre por defecto del objeto de datos dentro de un contenedor ASiC-S. */
	public static String getASiCSDefaultDataFilename(final byte[] data) {
		final String extension = new MimeHelper(data).getExtension();
		return "dataobject." + (extension != null && !"".equals(extension) ? extension : DEFAULT_DATAOBJECT_EXTENSION); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Crea un contenedor ASiC-S.
	 * @param signature Objeto de firmas
	 * @param data Objeto de datos
	 * @param signatureFilename Nombre del objeto de firmas (debe contener la ruta, por ejemplo <code>"META-INF/signature.p7s"</code>).
	 * @param dataFilename Nombre Nombre del objeto de datos (no debe contener ninguna ruta). Si se proporciona <code>null</code> se usa el
	 *                     nombre por defecto.
	 * @return Contenedor ASiC-S.
	 * @throws IOException Si hay errores en el tratamiento de datos. */
	public static byte[] createSContainer(final byte[] signature,
			                              final byte[] data,
			                              final String signatureFilename,
			                              final String dataFilename) throws IOException {

		if (signature == null || signature.length < 1) {
			throw new IllegalArgumentException(
				"La firma no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}

		if (data == null || data.length < 1) {
			throw new IllegalArgumentException(
				"Los datos no pueden ser nulos ni vacios" //$NON-NLS-1$
			);
		}

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final ZipOutputStream zos = new ZipOutputStream(baos);
		zos.setComment("mimetype=" + ASIC_S_MIMETYPE); //$NON-NLS-1$

		// El MIME-Type
		ZipEntry ze = new ZipEntry(ENTRY_NAME_MIMETYPE);
		zos.putNextEntry(ze);
		zos.write(ASIC_S_MIMETYPE.getBytes());

		// Los datos
		final String entryName;
		if (dataFilename != null) {
			entryName = dataFilename;
		}
		else {
			entryName = getASiCSDefaultDataFilename(data);
		}
		ze = new ZipEntry(
			entryName
		);
		zos.putNextEntry(ze);
		zos.write(data);

		// La firma
		ze = new ZipEntry(signatureFilename != null ? signatureFilename : ENTRY_NAME_BINARY_SIGNATURE);
		zos.putNextEntry(ze);
		zos.write(data);

		zos.close();

		return baos.toByteArray();
	}

	/** Obtiene la firma de un contenedor CAdES ASiC-S.
	 * @param asic Contendor CAdES ASiC-S.
	 * @return Firma de un contenedor CAdES ASiC-S.
	 * @throws IOException Si hay alg&uacute;n error en el tratamiento de datos. */
	public static byte[] getASiCSBinarySignature(final byte[] asic) throws IOException {
		return getASiCSSignature(asic, ENTRY_NAME_BINARY_SIGNATURE);
	}

	/** Obtiene la firma de un contenedor XAdES ASiC-S.
	 * @param asic Contendor XAdES ASiC-S.
	 * @return Firma de un contenedor XAdES ASiC-S.
	 * @throws IOException Si hay alg&uacute;n error en el tratamiento de datos. */
	public static byte[] getASiCSXMLSignature(final byte[] asic) throws IOException {
		return getASiCSSignature(asic, ENTRY_NAME_XML_SIGNATURE);
	}

	/** Obtiene la firma de un contenedor ASiC-S.
	 * @param asic Contendor ASiC-S.
	 * @param signatureFilename Nombre de la entrada del ZIP con las firmas a obtener.
	 * @return Firma de un contenedor ASiC-S.
	 * @throws IOException Si hay alg&uacute;n error en el tratamiento de datos. */
	private static byte[] getASiCSSignature(final byte[] asic, final String signatureFilename) throws IOException {
		if (asic == null || asic.length < 1) {
			throw new IllegalArgumentException(
				"La firma ASiC proporcionada no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}
		if (signatureFilename == null) {
			throw new IllegalArgumentException(
				"La firma entrada de firma del ASiC no puede ser nula" //$NON-NLS-1$
			);
		}
		final ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(asic));
		ZipEntry entry;
		while((entry = zis.getNextEntry()) != null) {
			if (signatureFilename.equals(entry.getName())) {
				final byte[] sig = AOUtil.getDataFromInputStream(zis);
				zis.close();
				return sig;
			}
		}
		throw new IOException(
			"Los datos proporcionados no son una firma ASiC-S conteniendo la entrada " + signatureFilename //$NON-NLS-1$
		);
	}

	/** Obtiene los datos de un contenedor ASiC-S.
	 * @param asic Contendor ASiC-S.
	 * @return Datos firmados de un contenedor ASiC-S.
	 * @throws IOException Si hay alg&uacute;n error en el tratamiento de datos. */
	public static byte[] getASiCSData(final byte[] asic) throws IOException {
		if (asic == null || asic.length < 1) {
			throw new IllegalArgumentException(
				"La firma ASiC proporcionada no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}
		final ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(asic));
		ZipEntry entry;
		while((entry = zis.getNextEntry()) != null) {
			if (!ENTRY_NAME_BINARY_SIGNATURE.equals(entry.getName()) &&
				!ENTRY_NAME_XML_SIGNATURE.equals(entry.getName()) &&
				!ENTRY_NAME_MIMETYPE.equals(entry.getName())
			) {
				final byte[] data = AOUtil.getDataFromInputStream(zis);
				zis.close();
				return data;
			}
		}
		throw new IOException("Los datos proporcionados no son una firma ASiC-S"); //$NON-NLS-1$
	}

	/** Obtiene el nombre del objeto de datos de un contenedor ASiC-S.
	 * @param asic Contendor ASiC-S.
	 * @return Nombre del objeto de datos de un contenedor ASiC-S.
	 * @throws IOException Si hay alg&uacute;n error en el tratamiento de datos. */
	public static String getASiCSDataFilename(final byte[] asic) throws IOException {
		if (asic == null || asic.length < 1) {
			throw new IllegalArgumentException(
				"La firma ASiC proporcionada no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}
		final ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(asic));
		ZipEntry entry;
		while((entry = zis.getNextEntry()) != null) {
			final String entryName = entry.getName();
			if (!ENTRY_NAME_BINARY_SIGNATURE.equals(entryName) &&
				!ENTRY_NAME_XML_SIGNATURE.equals(entryName) &&
				!ENTRY_NAME_MIMETYPE.equals(entryName)
			) {
				return entryName;
			}
		}
		throw new IOException("Los datos proporcionados no son una firma ASiC-S"); //$NON-NLS-1$
	}
}
