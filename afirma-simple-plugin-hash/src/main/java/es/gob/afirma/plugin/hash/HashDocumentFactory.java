package es.gob.afirma.plugin.hash;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class HashDocumentFactory {

	static final String FORMAT_XML = "xml"; //$NON-NLS-1$

	static final String FORMAT_CSV = "csv"; //$NON-NLS-1$

	static final String FORMAT_TXT = "txt"; //$NON-NLS-1$

	private static final String EXT_HASHFILES_XML = "hashfiles"; //$NON-NLS-1$

	private static final String EXT_HASHFILES_TXT = "txthashfiles"; //$NON-NLS-1$

	private static final String CLASS_HASHDOCUMENT_XML = "es.gob.afirma.plugin.hash.XmlHashDocument"; //$NON-NLS-1$

	private static final String CLASS_HASHDOCUMENT_TXT = "es.gob.afirma.plugin.hash.TxtHashDocument"; //$NON-NLS-1$

	private static final String CLASS_HASHDOCUMENT_CSV = "es.gob.afirma.plugin.hash.CsvHashDocument"; //$NON-NLS-1$

	private static final String[] HASH_DOCUMENT_CLASSES = new String[] {
		CLASS_HASHDOCUMENT_XML,
		CLASS_HASHDOCUMENT_TXT
	};

	private static final Logger LOGGER = Logger.getLogger(HashDocumentFactory.class.getName());

	public static HashDocument getHashDocument(final String format) {

		if (format == null) {
			throw new IllegalArgumentException("El formato de documento no puede ser nulo"); //$NON-NLS-1$
		}

		HashDocument document;
		switch (format) {
		case FORMAT_XML:
			document = loadHashDocumentClass(CLASS_HASHDOCUMENT_XML);
			break;
		case FORMAT_TXT:
			document = loadHashDocumentClass(CLASS_HASHDOCUMENT_TXT);
			break;
		case FORMAT_CSV:
			document = loadHashDocumentClass(CLASS_HASHDOCUMENT_CSV);
			break;
		default:
			throw new IllegalArgumentException("El formato de documentos de hash '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return document;
	}

	static HashDocument loadDocument(final byte[] data, final String ext)
			throws DocumentException, CorruptedDocumentException {

		String defaultHashDocumentClass = null;
		switch (ext) {
		case FORMAT_XML:
		case EXT_HASHFILES_XML:
			defaultHashDocumentClass = CLASS_HASHDOCUMENT_XML;
			break;
		case FORMAT_TXT:
		case EXT_HASHFILES_TXT:
			defaultHashDocumentClass = CLASS_HASHDOCUMENT_TXT;
			break;
			default:
			break;
		}

		return loadHashDocument(data, defaultHashDocumentClass);
	}


	private static HashDocument loadHashDocument(final byte[] data, final String defaultHashDocumentClass)
			throws DocumentException, CorruptedDocumentException {

		// Si hay un formato por defecto seleccionado, probamos primero con ese
		if (defaultHashDocumentClass != null) {
			final HashDocument hashDocument = loadHashDocumentClass(defaultHashDocumentClass);
			try {
				hashDocument.load(data);
				return hashDocument;
			}
			catch (final CorruptedDocumentException e) {
				LOGGER.severe("Se ha identificado que el documento esta corrupto o ha sido manipulado"); //$NON-NLS-1$
				throw e;
			}
			catch (final IOException e) {
				LOGGER.warning("No se pudo cargar el documento. Se considerara que el formato no era el esperado y se continuara con el resto de formatos"); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning("El formato del documento de hashes de entrada no se corresponde con el que declara"); //$NON-NLS-1$
			}
		}

		// No habia formato por defecto o no se correspondia con el del fichero, asi
		// que probamos con el resto de formatos
		for (final String hashDocumentClassname : HASH_DOCUMENT_CLASSES) {
			if (hashDocumentClassname != defaultHashDocumentClass) {
				final HashDocument hashDocument = loadHashDocumentClass(defaultHashDocumentClass);
				try {

					return hashDocument;
				}
				catch (final Exception e) {
					LOGGER.log(Level.INFO, String.format("El manejador %s no soporta el fichero de hashes indicado", hashDocumentClassname)); //$NON-NLS-1$
				}
			}
		}

		// El documento no cumple con los requisitos de ningun formato soportado
		throw new DocumentException("El formato del documento de hashes seleccionado no esta soportado"); //$NON-NLS-1$
	}

	/**
	 * Crea una instancia de la clase indicada.
	 * @param hashDocumentClassname Nombre de la clase.
	 * @return Instancia de la clase.
	 */
	private static HashDocument loadHashDocumentClass(final String hashDocumentClassname) {
		try {
			final Class<?> hashDocumentClass = Class.forName(hashDocumentClassname);
			return (HashDocument) hashDocumentClass.getConstructor().newInstance();
		}
		catch (final Exception e) {
			// Esto nunca se deberia dar
			throw new RuntimeException("Falta una clase interna en la aplicacion", e); //$NON-NLS-1$
		}
	}
}
