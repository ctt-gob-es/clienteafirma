package es.fnmtrcm.ceres.batch.filewrapper;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashSet;
import java.util.Properties;
import java.util.UUID;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Creador de lotes de firmas en base a ficheros y directorios.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class BatchGenerator {

	private BatchGenerator() {
		// No instanciable
	}

	/** Genera un XML de defini&oacute;n de lote de firma.
	 * @param indir Directorio que contiene los
	 * @param outdir Directorio de salida para los documentos firmados.
	 * @param extension Extensi&oacute;n de los ficheros a firmar (no se distingue entre may&uacute;sculas
	 *                  y min&uacute;sculas).
	 * @param commonSignatureExtraParams Par&aacute;metros adicionales comunes a todas las firmas.
	 * @param signatureFormat
	 * @return XML de defini&oacute;n de lote de firma.
	 * @throws IOException Si hay problemas en el proceso. */
	public static String generateBatchXml(final String indir,
										  final String outdir,
			                              final String extension,
			                              final Properties commonSignatureExtraParams,
			                              final String signatureFormat) throws IOException {
		if (indir == null) {
			throw new IllegalArgumentException(
				"El directorio de entrada no puede ser nulo" //$NON-NLS-1$
			);
		}
		final File inFolder = new File(indir);
		if (!inFolder.isDirectory()) {
			throw new IllegalArgumentException(
				"El directorio de entrada no existe o no es un directorio" //$NON-NLS-1$
			);
		}
		if (!inFolder.canRead()) {
			throw new IllegalArgumentException(
				"No se tienen permisos de lectura sobre el directorio de entrada" //$NON-NLS-1$
			);
		}
		final File outFolder = new File(outdir);
		if (!outFolder.isDirectory()) {
			throw new IllegalArgumentException(
				"El directorio de salida no existe o no es un directorio" //$NON-NLS-1$
			);
		}
		if (!outFolder.canWrite()) {
			throw new IllegalArgumentException(
				"No se tienen permisos de escritura sobre el directorio de salida" //$NON-NLS-1$
			);
		}


		final Collection<File> filesToSign = new HashSet<File>();
		final String[] files = inFolder.list();
		final String ext = extension != null ? extension.toLowerCase() : ""; //$NON-NLS-1$
		for (final String cf : files) {
			if (cf.toLowerCase().endsWith(ext)) {
				final File tmpFile = new File(inFolder, cf);
				if (tmpFile.exists() && tmpFile.isFile() && tmpFile.canRead()) {
					filesToSign.add(tmpFile);
				}
			}
		}

		// Ya tenemos el listado de ficheros a firmar, creamos el lote

		final Properties extraParams = commonSignatureExtraParams != null ?
			commonSignatureExtraParams :
				new Properties();

		extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$

		final StringBuilder sb = new StringBuilder();
		sb.append(
			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" + //$NON-NLS-1$
			"<signbatch stoponerror=\"false\" algorithm=\"SHA256withRSA\">\n" //$NON-NLS-1$
		);

		for (final File f : filesToSign) {

			sb.append(" <singlesign Id=\""); //$NON-NLS-1$
			sb.append(UUID.randomUUID().toString());
			sb.append("\">\n  <datasource>\n   "); //$NON-NLS-1$
			sb.append(f.toURI());
			sb.append("\n  </datasource>\n  <format>\n   "); //$NON-NLS-1$
			sb.append(signatureFormat);
			sb.append("\n  </format>\n  <suboperation>sign</suboperation>\n  <extraparams>\n   "); //$NON-NLS-1$

			// Tratamos los ExtraParams
			Properties newProperties = new Properties();
			newProperties.putAll(extraParams);
			addXmlAttachment(newProperties, inFolder, f.getName());
			sb.append(AOUtil.properties2Base64(newProperties));

			sb.append("\n  </extraparams>\n  <signsaver>\n   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\n"); //$NON-NLS-1$
			sb.append("   <config>\n    "); //$NON-NLS-1$

			// Creamos la configuracion del SignSaver
			newProperties = new Properties();
			newProperties.put(
				"FileName", //$NON-NLS-1$
				new File(outFolder, f.getName()).getAbsolutePath()
			);
			sb.append(AOUtil.properties2Base64(newProperties));
			sb.append("\n   </config>\n  </signsaver>\n </singlesign>\n"); //$NON-NLS-1$

		}

		sb.append("</signbatch>"); //$NON-NLS-1$

		return sb.toString();
	}

	/** Modifica los par&aacute;metros adicionales de una firma PAdES para que, antes de firmarse un
	 * PDF, se adjunte si existe un fichero de metadatos XML.
	 * Este fichero de metadatos XML debe llamarse igual que el PDF a firmar solo que con el sufijo ".xml"
	 * y estar en el mismo directorio.
	 * As&iacute; si en el directorio de entrada encontramos el fichero "DOC.pdf" y adem&aacute;s
	 * "DOC.pdf.xml", este &uacute;ltimo se adjunta al PDF antes de que se firme.
	 * Si los par&aacute;metros adicionales conten&iacute;an ya la propiedad <code>attachFileName</code>
	 * esta no se modifica, y si no la conten&iacute;n se usa como <code>attachFileName</code> el mismo
	 * nombre de fichero (en este ejemplo ser&iacute;a ""DOC.pdf.xml".
	 * @param inProps Par&aacute;metros adicionales originales.
	 * @param inFolder Directorio de origen para PDF y XML.
	 * @param toBeSignedFilename Nombre del fichero PDF a firmar (sin ruta).
	 * @return Par&aacute;metros adicionales con los valores necesarios a&ntilde;adidos.
	 * @throws IOException Si hay problemas en la lectura del fichero de metadatos. */
	private static Properties addXmlAttachment(final Properties inProps,
			                                   final File inFolder,
			                                   final String toBeSignedFilename) throws IOException {

		final File atFile = new File(inFolder, toBeSignedFilename + ".xml"); //$NON-NLS-1$
		if (atFile.isFile() && atFile.canRead()) {
			final InputStream fis = new FileInputStream(atFile);
			final InputStream bis = new BufferedInputStream(fis);
			final byte[] data = AOUtil.getDataFromInputStream(bis);
			if (data != null && data.length > 0) {
				inProps.put("attach", Base64.encode(data)); //$NON-NLS-1$
				if (inProps.get("attachFileName") == null) { //$NON-NLS-1$
					inProps.put("attachFileName", atFile.getName()); //$NON-NLS-1$
				}
			}
			bis.close();
			fis.close();
		}
		return inProps;
	}

}
