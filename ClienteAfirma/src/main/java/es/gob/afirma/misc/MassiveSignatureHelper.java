/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URI;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.activation.MimeType;

import org.ietf.jgss.Oid;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.misc.AOSignConstants.CounterSignTarget;
import es.gob.afirma.misc.DirectorySignatureHelper.MassiveType;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;

/**
 * M&oacute;dulo para el soporte de multifirmas m&aacute;sivas. Permite
 * configurar una operaci&oacute;n de firma y ejecutarla sobre datos, hashes y
 * ficheros.</br> Se crea un log de la operaci&oacute;n masiva en donde cada
 * entrada se corresponde con el resultado de la ejecuci&oacute;n de una
 * operaci&oacute;n.
 */
public final class MassiveSignatureHelper {

	/** Configuracion de la operaci&oacute;n masiva. */
	private MassiveSignConfiguration massiveConfiguration = null;

	/** Logger de las operaciones de firma masiva. */
	private Vector<String> log = null;

	/** Manejador de firma para el formato configurado por defecto. */
	private AOSigner defaultSigner = null;

	/** Indica si el m&oacute;dulo est&aacute; inicializado. */
	private boolean isInitialized;

	/**
	 * Contruye el m&oacute;dulo de soporte para la multifirma masiva.
	 * 
	 * @param configuration
	 *            Configuracion de la operaci&oacute;n.
	 * @throws AOException
	 *             La configuraci&oacute;n introducida no es v&aacute;lida.
	 */
	public MassiveSignatureHelper(final MassiveSignConfiguration configuration)
			throws AOException {

		if (configuration == null) {
			throw new NullPointerException(
					"La configuracion de firma masiva no puede ser nula");
		}
		if (configuration.massiveOperation == null) {
			throw new NullPointerException(
					"La configuracion indicada no tiene establecida ninguna operacion masiva");
		}

		this.massiveConfiguration = configuration;

		// Creamos el manejador de firma por defecto
		this.defaultSigner = AOCryptoUtil
				.getSigner(this.massiveConfiguration.defaultFormat);
		if (this.defaultSigner == null) {
			throw new AOException("Formato de firma no soportado: "
					+ this.massiveConfiguration.defaultFormat);
		}

		this.isInitialized = true;
	}

	/**
	 * Comprueba si el modulo de firma masiva esta inicializado.
	 * 
	 * @return Devuelve <code>true</code> si est&aacute; inicializado,
	 *         <code>false</code> en caso contrario.
	 */
	public boolean isInitialized() {
		return this.isInitialized;
	}

	/**
	 * Establece el tipo de operaci&oacute;n (firma, cofirma, contrafirma del
	 * &aacute;rbol de firma o contrafirma de nodos hojas) que debe realizar el
	 * m&oacute;dulo de firma masiva. Si se indica <code>null</code> se
	 * establece la configuraci&oacute;n por defecto (firma).
	 * 
	 * @param massiveOperation
	 *            Tipo de operaci&oacute;n.
	 * @see MassiveType
	 */
	public void setMassiveOperation(MassiveType massiveOperation) {
		if (massiveOperation == null)
			massiveOperation = MassiveType.SIGN;
		this.massiveConfiguration.massiveOperation = massiveOperation;
	}

	/**
	 * Libera la configuraci&oacute;n de la operaci&oacute;n masiva e inhabilita
	 * su uso. El m&oacute;dulo aun conservar&aacute;a la informaci&oacute;n de
	 * log.
	 */
	public void release() {
		this.massiveConfiguration = null;
		this.defaultSigner = null;
		this.isInitialized = false;
	}

	/**
	 * Realiza la firma de datos.
	 * 
	 * @param b64Data
	 *            Datos en base 64 que se desean firmar.
	 * @return Resultado de la firma en Base64.
	 */
	public String signData(final String b64Data) {

		if (!this.isInitialized) {
			Logger.getLogger("es.gob.afirma").severe(
					"El modulo de firma masiva no ha sido inicializado");
			throw new NullPointerException(
					"El modulo de firma masiva no ha sido inicializado");
		}

		if (b64Data == null) {
			Logger.getLogger("es.gob.afirma").severe(
					"No se han introducido datos para firmar");
			this.addLog("Operaci\u00F3n sobre datos: No se han introducido datos para firmar");
			return null;
		}

		final Properties config = (Properties) massiveConfiguration.extraParams
				.clone(); // Configuracion
		config.setProperty("headLess", "true");
		final byte[] data = AOCryptoUtil.decodeBase64(b64Data); // Datos a
																// firmar
		String operation = null; // Para aclarar mensajes por consola
		byte[] signData = null; // Firma resultante

		// Ejecutamos la operacion que corresponda
		try {
			if (massiveConfiguration.massiveOperation == MassiveType.SIGN) { // Firma
				operation = "firmar";
				signData = signDataFromData(defaultSigner, data, null, config);
			} else if (massiveConfiguration.massiveOperation == MassiveType.COSIGN) { // Cofirma
				operation = "cofirmar";
				signData = cosign(defaultSigner, data, config);
			} else if (massiveConfiguration.massiveOperation == MassiveType.COUNTERSIGN_ALL) { // Contraforma
																								// del
																								// arbol
																								// completo
				operation = "contrafirmar el arbol de firmas de";
				signData = countersignTree(defaultSigner, data, config);
			} else { // Contrafirma de los nodos hoja
				operation = "contrafirmar los nodos hoja de";
				signData = countersignLeafs(defaultSigner, data, config);
			}
		} catch (final AOFormatFileException e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Los datos introducidos no tienen un formato v\u00E1lido: "
							+ e.getMessage());
			this.addLog("Operaci\u00F3n sobre datos: Los datos introducidos no tienen un formato v\u00E1lido");
			return null;
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Error al " + operation + " los datos introducidos: "
							+ e.getMessage());
			this.addLog("Operaci\u00F3n sobre datos: " + e.getMessage());
			return null;
		}

		this.addLog("Operaci\u00F3n sobre datos: Correcta");

		return AOCryptoUtil.encodeBase64(signData, false);
	}

	/**
	 * Realiza la firma de un hash. La cofirma y contrafirma de hashes no esta
	 * soportada.
	 * 
	 * @param b64Hash
	 *            Hash en base 64 que se desea firmar.
	 * @return Resultado de la firma en Base64.
	 */
	public String signHash(String b64Hash) {

		if (!this.isInitialized) {
			Logger.getLogger("es.gob.afirma").severe(
					"El modulo de firma masiva no ha sido inicializado");
			throw new NullPointerException(
					"El modulo de firma masiva no ha sido inicializado");
		}

		if (b64Hash == null) {
			Logger.getLogger("es.gob.afirma").severe(
					"No se ha introducido un hash para firmar");
			this.addLog("Operaci\u00F3n sobre hash: No se ha introducido un hash para firmar");
			return null;
		}

		// Transformamos los datos
		byte[] hash = AOCryptoUtil.decodeBase64(b64Hash);

		// Solo para aclarar los posibles mensajes por consola, almacenaremos
		String operation = "firmar";

		// Firma resultante
		byte[] signData = null;

		// Ejecutamos la operacion que corresponda
		Properties config = (Properties) massiveConfiguration.extraParams
				.clone();
		config.setProperty("headLess", "true");
		try {
			if (massiveConfiguration.massiveOperation == MassiveType.SIGN) { // Firma
				operation = "firmar";
				signData = signDataFromHash(defaultSigner, hash, config);
			} else if (massiveConfiguration.massiveOperation == MassiveType.COSIGN) { // Cofirma
				operation = "cofirmar";
				throw new UnsupportedOperationException(
						"La cofirma de un hash no es una operacion valida");
			} else { // Contrafirma
				operation = "contrafirmar";
				throw new UnsupportedOperationException(
						"La contrafirma de un hash no es una operacion valida");
			}
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Error al " + operation + " el hash '" + b64Hash + "': "
							+ e.getMessage());
			this.addLog("Operaci\u00F3n sobre hash: " + e.getMessage());
			return null;
		}

		this.addLog("Operaci\u00F3n sobre hash: Correcta");

		return AOCryptoUtil.encodeBase64(signData, false);
	}

	/**
	 * Realiza la operaci&oacute;n de multifirma sobre un fichero.
	 * 
	 * @param fileUri
	 *            Path del fichero que se desea firmar.
	 * @return Multifirma del fichero.
	 */
	public String signFile(String fileUri) {

		if (!this.isInitialized) {
			Logger.getLogger("es.gob.afirma").severe(
					"El modulo de firma masiva no ha sido inicializado");
			throw new NullPointerException(
					"El modulo de firma masiva no ha sido inicializado");
		}

		if (fileUri == null) {
			Logger.getLogger("es.gob.afirma").severe(
					"No se ha introducido un fichero para firmar");
			this.addLog("Operaci\u00F3n sobre fichero: No se ha introducido un fichero para firmar");
			return null;
		}

		// Creamos la URI del fichero
		URI uri = null;
		try {
			uri = AOUtil.createURI(fileUri);
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma")
					.severe("La URI '" + fileUri
							+ "' no posee un formato valido: " + e);
			this.addLog("Operaci\u00F3n sobre fichero: La URI '" + fileUri
					+ "' no posee un formato v\u00E1lido");
			return null;
		}

		// Creamos el flujo de datos del fichero
		InputStream is = null;
		try {
			is = AOUtil.loadFile(uri, null, false);
		} catch (FileNotFoundException e) {
			Logger.getLogger("es.gob.afirma").severe(
					"No ha sido posible encontrar el fichero '" + fileUri
							+ "': " + e);
			this.addLog("Operaci\u00F3n sobre fichero: No ha sido posible encontrar el fichero '"
					+ fileUri + "'");
			return null;
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"No es posible acceder al contenido del fichero '"
							+ fileUri + "': " + e);
			this.addLog("Operaci\u00F3n sobre fichero: No es posible acceder al contenido del fichero '"
					+ fileUri + "'");
			return null;
		}

		// Leemos el contenido del fichero
		byte[] data = null;
		try {
			data = AOUtil.getDataFromInputStream(is);
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"No es posible leer el contenido del fichero '" + fileUri
							+ "': " + e);
			this.addLog("Operaci\u00F3n sobre fichero: No es posible leer el contenido del fichero '"
					+ fileUri + "'");
			return null;
		}
		if (data == null) {
			Logger.getLogger("es.gob.afirma").severe(
					"El fichero '" + fileUri + "' esta vacio");
			this.addLog("Operaci\u00F3n sobre fichero: El fichero '" + fileUri
					+ "' esta vacio");
			return null;
		}

		// Liberamos el fichero de recursos
		try {
			is.close();
			is = null;
		} catch (Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
					"No se ha podido liberar el fichero '" + fileUri + "': "
							+ e);
		}

		// Ejecutamos la operacion que corresponda
		byte[] signData = null;
		final Properties config = (Properties) massiveConfiguration.extraParams
				.clone();
		config.setProperty("headLess", "true");

		try {
			if (massiveConfiguration.massiveOperation == MassiveType.SIGN) { // Firma
				signData = signDataFromData(defaultSigner, data, uri, config);
			} else if (massiveConfiguration.massiveOperation == MassiveType.COSIGN) { // Cofirma
				signData = cosign(defaultSigner, data, config);
			} else if (massiveConfiguration.massiveOperation == MassiveType.COUNTERSIGN_ALL) { // Contraforma
																								// del
																								// arbol
																								// completo
				signData = countersignTree(defaultSigner, data, config);
			} else { // Contraforma de los nodos hoja
				signData = countersignLeafs(defaultSigner, data, config);
			}
		} catch (final AOFormatFileException e) {
			Logger.getLogger("es.gob.afirma").severe(
					"El fichero '" + fileUri
							+ "' no tiene un formato v\u00E1lido: "
							+ e.getMessage());
			this.addLog("Operaci\u00F3n sobre fichero: El fichero '" + fileUri
					+ "' no tiene un formato v\u00E1lido");
			return null;
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe(
					"Error al realizar la operacion "
							+ massiveConfiguration.massiveOperation
							+ " sobre el fichero '" + fileUri + "': "
							+ e.getMessage());
			this.addLog("Operaci\u00F3n sobre fichero: " + e.getMessage());
			return null;
		}

		this.addLog("Operaci\u00F3n sobre fichero: Correcta");

		return AOCryptoUtil.encodeBase64(signData, false);
	}

	/**
	 * Firma datos con el signer indicado.
	 * 
	 * @param signer
	 *            Manejador con el que firmar los datos.
	 * @param data
	 *            Datos a firmar.
	 * @param uri
	 *            Uri de los datos a firmar (opcional seg&uacute;n formato de
	 *            firma).
	 * @param config
	 *            Configuraci&oacute;n general para la operaci&oacute;n.
	 * @return Firma electr&oacute;nica con el formato dado por el manejador de
	 *         firma.
	 * @throws AOException
	 *             Cuando ocurre un error durante la operaci&oacute;n de firma.
	 */
	private byte[] signDataFromData(final AOSigner signer, final byte[] data,
			final URI uri, final Properties config) throws AOException {

		// Configuramos y ejecutamos la operacion
		config.setProperty("mode", massiveConfiguration.mode);
		config.setProperty("format", massiveConfiguration.defaultFormat);
		if (uri != null)
			config.setProperty("uri", uri.toString());

		// Deteccion del MIMEType, solo para XAdES y XMLDSig
		if ((signer.getClass().getName()
				.equals("es.gob.afirma.signers.AOXAdESSigner"))
				|| (signer.getClass().getName()
						.equals("es.gob.afirma.signers.AOXMLDSigSigner"))) {
			final MimeHelper mimeHelper = new MimeHelper(data);
			final String mimeType = mimeHelper.getMimeType();
			if (mimeType != null) {
				try {
					signer.setDataObjectFormat(
							mimeHelper.getDescription(),
							new Oid(MimeHelper.transformMimeTypeToOid(mimeType)),
							new MimeType(mimeType), null);
				} catch (final Exception e) {
					Logger.getLogger("es.gob.afirma")
							.warning(
									"No se ha podido detectar el MIME-Type, se utilizara el por defecto y este aspecto no se indicara en el registro de firma masiva: "
											+ e);
				}
			}
		}

		final byte[] signData = signer.sign(data,
				massiveConfiguration.algorithm,
				massiveConfiguration.getKeyEntry(), config);

		if (signData == null)
			throw new AOException("No se generaron datos de firma");
		return signData;
	}

	/**
	 * Firma un hash con el signer indicado.
	 * 
	 * @param signer
	 *            Manejador con el que firmar el hash.
	 * @param data
	 *            Hash a firmar.
	 * @param config
	 *            Configuraci&oacute;n general para la operaci&oacute;n.
	 * @return Firma electr&oacute;nica con el formato configurado.
	 * @throws AOException
	 *             Cuando ocurre un error durante la operaci&oacute;n de firma.
	 */
	private byte[] signDataFromHash(final AOSigner signer, final byte[] data,
			final Properties config) throws AOException {

		int pos = massiveConfiguration.algorithm.indexOf("with");
		if (pos == -1)
			throw new AOException("El algoritmo '"
					+ massiveConfiguration.algorithm
					+ "' no esta soportado para la firma de huellas digitales");

		// Configuramos y ejecutamos la operacion
		config.setProperty("mode", massiveConfiguration.mode);
		config.setProperty("format", massiveConfiguration.defaultFormat);
		config.setProperty("precalculatedHashAlgorithm",
				massiveConfiguration.algorithm.substring(0, pos));

		// Introduccion MIMEType "hash/algo", solo para XAdES y XMLDSig
		if ((signer.getClass().getName()
				.equals("es.gob.afirma.signers.AOXAdESSigner"))
				|| (signer.getClass().getName()
						.equals("es.gob.afirma.signers.AOXMLDSigSigner"))) {
			final String mimeType = "hash/"
					+ massiveConfiguration.algorithm.substring(0, pos)
							.toLowerCase();
			try {
				signer.setDataObjectFormat("Huella digital precalculada",
						new Oid(MimeHelper.transformMimeTypeToOid(mimeType)),
						new MimeType(mimeType), null);
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma")
						.warning(
								"Error al indicar el MIME-Type, se utilizara el por defecto y este aspecto no se indicara en el registro de firma masiva: "
										+ e);
			}
		}

		final byte[] signData = signer.sign(data,
				massiveConfiguration.algorithm,
				massiveConfiguration.getKeyEntry(), config);
		if (signData == null)
			throw new AOException("No se generaron datos de firma");
		return signData;
	}

	/**
	 * Cofirma datos con el manejador configurado o con el m&aacute;s apropiado
	 * si se indic&oacute; que se buscase.
	 * 
	 * @param signer
	 *            Manejador con el que cofirmar los datos.
	 * @param sign
	 *            Firma con los datos a cofirmar.
	 * @param config
	 *            Configuraci&oacute;n general para la operaci&oacute;n.
	 * @return Firma electr&oacute;nica con el formato dado por el manejador de
	 *         firma.
	 * @throws AOException
	 *             Cuando ocurre un error durante la operaci&oacute;n de firma.
	 */
	private byte[] cosign(final AOSigner signer, final byte[] sign,
			final Properties config) throws AOException {

		// Tomamos el signer adecuado para la operacion o el obligatorio si se
		// especifico
		final AOSigner validSigner = this.getValidSigner(signer, sign);

		// Configuramos y ejecutamos la operacion
		config.setProperty("mode", massiveConfiguration.mode);

		final byte[] signData = validSigner.cosign(sign,
				massiveConfiguration.algorithm,
				massiveConfiguration.getKeyEntry(), config);
		if (signData == null)
			throw new AOException("No se generaron datos de firma");
		return signData;
	}

	/**
	 * Contrafirma todos los nodos de firma de los datos de firma introducidos
	 * usando el manejador configurado o el m&aacute;s apropiado si se
	 * indic&oacute; que se buscase.
	 * 
	 * @param signer
	 *            Manejador con el que contrafirmar.
	 * @param sign
	 *            Firma a contrafirmar.
	 * @param config
	 *            Configuraci&oacute;n general para la operaci&oacute;n.
	 * @return Firma electr&oacute;nica con el formato dado por el manejador de
	 *         firma.
	 * @throws AOException
	 *             Cuando ocurre un error durante la operaci&oacute;n de
	 *             contrafirma.
	 */
	private byte[] countersignTree(final AOSigner signer, final byte[] sign,
			final Properties config) throws AOException {
		return countersignOperation(signer, sign, CounterSignTarget.Tree,
				config);
	}

	/**
	 * Contrafirma las hojas de la estructura de firma introducida usando el
	 * manejador configurado o el m&aacute;s apropiado si se indic&oacute; que
	 * se buscase.
	 * 
	 * @param signer
	 *            Manejador con el que contrafirmar.
	 * @param sign
	 *            Firma a contrafirmar.
	 * @param config
	 *            Configuraci&oacute;n general para la operaci&oacute;n.
	 * @return Firma electr&oacute;nica con el formato dado por el manejador de
	 *         firma.
	 * @throws AOException
	 *             Cuando ocurre un error durante la operaci&oacute;n de
	 *             contrafirma.
	 */
	private byte[] countersignLeafs(final AOSigner signer, final byte[] sign,
			final Properties config) throws AOException {
		return countersignOperation(signer, sign, CounterSignTarget.Leafs,
				config);
	}

	/**
	 * Contrafirma los nodos indicados de una firma electr&oacute;nica usando el
	 * manejador configurado o el m&aacute;s apropiado si se indic&oacute; que
	 * se buscase.
	 * 
	 * @param signer
	 *            Manejador con el que contrafirmar.
	 * @param sign
	 *            Firma a contrafirmar.
	 * @param target
	 *            Nodos objetivos para la contrafirma.
	 * @param config
	 *            Configuraci&oacute;n general para la operaci&oacute;n.
	 * @return Firma electr&oacute;nica con el formato dado por el manejador de
	 *         firma.
	 * @throws AOException
	 *             Cuando ocurre un error durante la operaci&oacute;n de
	 *             contrafirma.
	 */
	private byte[] countersignOperation(final AOSigner signer,
			final byte[] sign, final CounterSignTarget target,
			final Properties config) throws AOException {

		// Tomamos el signer adecuado para la operacion o el obligatorio si se
		// especifico
		AOSigner validSigner = this.getValidSigner(signer, sign);

		byte[] signData = validSigner.countersign(sign,
				massiveConfiguration.algorithm, target, null,
				massiveConfiguration.getKeyEntry(), config);
		if (signData == null)
			throw new AOException("No se generaron datos de firma");
		return signData;
	}

	/**
	 * Recupera el signer apropiado para la cofirma o contrafirma de la firma
	 * introducida. Comprueba si se ha establecido que se respete el formato por
	 * defecto introducido o si se debe buscar el formato m&aacute;s apropiado
	 * seg&uacute;n el tipo de firma.
	 * 
	 * @param signer
	 *            Manejador de firma.
	 * @param signData
	 *            Firma para la que deseamos obtener un manejador.
	 * @return Manejador compatible con la firma introducida.
	 * @throws AOException
	 *             Si la firma introducida no se corresponde con ning&uacute;n
	 *             formato soportado o se obliga a usar el manejador por defecto
	 *             y este no la soporta.
	 */
	private AOSigner getValidSigner(final AOSigner signer, final byte[] signData)
			throws AOException {
		// Tomamos el signer adecuado para la operacion o el obligatorio si se
		// especifico
		AOSigner validSigner = signer;
		if (!this.massiveConfiguration.originalFormat) {
			if (!signer.isSign(signData)) {
				throw new AOException(
						"La firma introducida no se corresponde con el formato de firma especificado");
			}
		} else {
			validSigner = AOSignerFactory.getSigner(signData);
			if (validSigner == null) {
				throw new AOException(
						"La firma introducida no se corresponde con ning\u00FAn formato soportado");
			}
		}
		return validSigner;
	}

	/**
	 * Agrega una entrada al log de la operacion de multifirma masiva global.
	 * 
	 * @param message
	 *            Entrada del log.
	 */
	private void addLog(final String message) {
		if (this.log == null)
			this.log = new Vector<String>();
		this.log.add(message);
	}

	/**
	 * Recupera entrada del log correspondiente a la &uacute;ltima operacion de
	 * multifirma realizada.
	 * 
	 * @return Entrada de log.
	 */
	public String getCurrentLogEntry() {
		String lastEntry = "";
		if (this.log != null) {
			lastEntry = this.log.get(this.log.size() - 1);
		}
		return lastEntry;
	}

	/**
	 * Recupera todo el log de la operaci&oacute;n masiva.
	 * 
	 * @return Log de la operaci&oacute;n masiva completa.
	 */
	public String getAllLogEntries() {
		final StringBuilder buffer = new StringBuilder();
		if (this.log != null) {
			for (String logEntry : this.log) {
				buffer.append(logEntry).append("\r\n");
			}
		}
		return buffer.toString().trim();
	}

	/**
	 * Almacena los datos necesarios para realizar una operaci&oacute;n masiva
	 * de firma.
	 */
	public static class MassiveSignConfiguration {

		private X509Certificate certificate = null;
		private PrivateKeyEntry keyEntry = null;
		protected MassiveType massiveOperation = null;
		protected String algorithm = AOConstants.DEFAULT_SIGN_ALGO;
		protected String mode = AOConstants.DEFAULT_SIGN_MODE;
		protected String defaultFormat = AOConstants.DEFAULT_SIGN_FORMAT;
		protected boolean originalFormat = true;
		protected String selectedAlias = null;
		protected Properties extraParams = null;

		/**
		 * Crea un <i>JavaBean</i> con los par&aacute;metros necesarios para las
		 * operaciones de firma masiva.
		 * 
		 * @param keyEntry
		 *            Clave privada para las firmas
		 * @param certificate
		 *            Certificado X.509 firmante
		 */
		public MassiveSignConfiguration(final PrivateKeyEntry keyEntry,
				final X509Certificate certificate) {
			this.keyEntry = keyEntry;
			this.certificate = certificate;
			this.extraParams = new Properties();
		}

		/**
		 * Recupera la operaci&oacute;n masiva configurada.
		 * 
		 * @return Tipo de operaci&oacute;n masiva.
		 */
		public MassiveType getMassiveOperation() {
			return massiveOperation;
		}

		/**
		 * Establece la operaci&oacute;n masiva que deber&aacute; ejecutarse.
		 * 
		 * @param massiveOperation
		 *            Tipo de operaci&oacute;n masiva.
		 */
		public void setMassiveOperation(final MassiveType massiveOperation) {
			this.massiveOperation = massiveOperation;
		}

		/**
		 * Recupera el algoritmo de firma configurado.
		 * 
		 * @return Algoritmo de firma.
		 */
		public String getAlgorithm() {
			return algorithm;
		}

		/**
		 * Estable el algoritmo de firma.
		 * 
		 * @param algorithm
		 *            Algoritmo de firma.
		 */
		public void setAlgorithm(final String algorithm) {
			this.algorithm = algorithm;
		}

		/**
		 * Recupera el modo de firma configurado.
		 * 
		 * @return Modo de firma.
		 */
		public String getMode() {
			return mode;
		}

		/**
		 * Estable el modo de firma.
		 * 
		 * @param mode
		 *            Modo de firma.
		 */
		public void setMode(final String mode) {
			this.mode = mode;
		}

		/**
		 * Recupera el formato de firma configurado por defecto.
		 * 
		 * @return Formato de firma.
		 */
		public String getDefaultFormat() {
			return defaultFormat;
		}

		/**
		 * Estable el formato de firma por defecto (para cuando no se desee
		 * respetar el original o se realiza una firma masiva).
		 * 
		 * @param defaultFormat
		 *            Formato de firma.
		 */
		public void setDefaultFormat(final String defaultFormat) {
			this.defaultFormat = defaultFormat;
		}

		/**
		 * Indica si se ha configurado que las multifirmas respeten el formato
		 * de firma original.
		 * 
		 * @return Devuelve {@code true} si se ha configurado que se respete el
		 *         formato de firma, {@code false} en caso contrario.
		 */
		public boolean isOriginalFormat() {
			return originalFormat;
		}

		/**
		 * Estable si debe utilizarse un formato de firma original en le caso de
		 * las multifirmas masivas.
		 * 
		 * @param originalFormat
		 *            Respetar formato original de firma.
		 */
		public void setOriginalFormat(final boolean originalFormat) {
			this.originalFormat = originalFormat;
		}

		/**
		 * Recupera el certificado de firma.
		 * 
		 * @return Certificado de firma.
		 */
		public Certificate getCertificate() {
			return certificate;
		}

		/**
		 * Recupera entrada de la clave de firma.
		 * 
		 * @return Entrada de la clave de firma.
		 */
		public PrivateKeyEntry getKeyEntry() {
			return keyEntry;
		}

		/**
		 * Recupera el alias del certificado de firma.
		 * 
		 * @return Alias del certficado de firma.
		 */
		public String getSelectedAlias() {
			return selectedAlias;
		}

		/**
		 * Establece par&aacute;metros adicionales para la configuraci&oacute;n
		 * de la operaci&oacute;n masiva.
		 * 
		 * @param extraParams
		 *            Par&aacute;metros adicionales.
		 */
		public void setExtraParams(final Properties extraParams) {
			if (extraParams != null)
				this.extraParams = (Properties) extraParams.clone();
			else
				this.extraParams.clear();
		}

		/**
		 * Recupera los par&aacute;metros adicionales configurados para la
		 * operaci&oacute;n masiva.
		 * 
		 * @return Par&aacute;metros adicionales.
		 */
		public Properties getExtraParams() {
			return this.extraParams;
		}
	}

}
