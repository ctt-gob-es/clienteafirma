package es.gob.afirma.signers.cades.asic;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.MimeHelper;

final class ASiCUtil {

	private static final String ASIC_S_MIMETYPE = "application/vnd.etsi.asic-s+zip"; //$NON-NLS-1$

	private static final String DEFAULT_DATAOBJECT_EXTENSION = "bin"; //$NON-NLS-1$
	private static final String ENTRY_NAME_SIGNATURE = "META-INF/signature.p7s"; //$NON-NLS-1$
	private static final String ENTRY_NAME_MIMETYPE = "mimetype"; //$NON-NLS-1$

	private ASiCUtil() {
		// No instanciable
	}

	static byte[] createSContainer(final byte[] signature,
			                       final byte[] data,
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
			final String extension = new MimeHelper(data).getExtension();
			entryName = "dataobject." + (extension != null && !"".equals(extension) ? extension : DEFAULT_DATAOBJECT_EXTENSION); //$NON-NLS-1$ //$NON-NLS-2$
		}
		ze = new ZipEntry(
			entryName
		);
		zos.putNextEntry(ze);
		zos.write(data);

		// La firma
		ze = new ZipEntry(ENTRY_NAME_SIGNATURE);
		zos.putNextEntry(ze);
		zos.write(data);

		zos.close();

		return baos.toByteArray();
	}

	static byte[] getASiCSSignature(final byte[] asic) throws IOException {
		if (asic == null || asic.length < 1) {
			throw new IllegalArgumentException(
				"La firma ASiC proporcionada no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}
		final ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(asic));
		ZipEntry entry;
		while((entry = zis.getNextEntry()) != null) {
			if (ENTRY_NAME_SIGNATURE.equals(entry.getName())) {
				final byte[] sig = AOUtil.getDataFromInputStream(zis);
				zis.close();
				return sig;
			}
		}
		throw new IOException("Los datos proporcionados no son una firma ASiC-S"); //$NON-NLS-1$
	}

	static byte[] getASiCSData(final byte[] asic) throws IOException {
		if (asic == null || asic.length < 1) {
			throw new IllegalArgumentException(
				"La firma ASiC proporcionada no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}
		final ZipInputStream zis = new ZipInputStream(new ByteArrayInputStream(asic));
		ZipEntry entry;
		while((entry = zis.getNextEntry()) != null) {
			if (!ENTRY_NAME_SIGNATURE.equals(entry.getName()) && !ENTRY_NAME_MIMETYPE.equals(entry.getName())) {
				final byte[] data = AOUtil.getDataFromInputStream(zis);
				zis.close();
				return data;
			}
		}
		throw new IOException("Los datos proporcionados no son una firma ASiC-S"); //$NON-NLS-1$
	}


}
