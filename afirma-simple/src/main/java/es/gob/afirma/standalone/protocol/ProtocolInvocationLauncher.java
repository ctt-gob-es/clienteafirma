/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.misc.protocol.ParameterLocalAccessRequestedException;
import es.gob.afirma.core.misc.protocol.ParameterNeedsUpdatedVersionException;
import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParser;
import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParserUtil;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.core.misc.protocol.UrlParametersForBatch;
import es.gob.afirma.core.misc.protocol.UrlParametersToLoad;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.misc.protocol.UrlParametersToSelectCert;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.core.misc.protocol.UrlParametersToSignAndSave;
import es.gob.afirma.signers.batch.client.TriphaseDataParser;
import es.gob.afirma.standalone.JMulticardUtilities;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.protocol.ProtocolInvocationLauncherUtil.DecryptionException;
import es.gob.afirma.standalone.protocol.ProtocolInvocationLauncherUtil.InvalidEncryptedDataLengthException;
import es.gob.afirma.standalone.ui.AboutDialog;
import es.gob.afirma.standalone.ui.OSXHandler;

/**
 * Gestiona la ejecuci&oacute;n de Autofirma en una invocaci&oacute;n por
 * protocolo y bajo un entorno compatible <code>Swing</code>.
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class ProtocolInvocationLauncher {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String RESULT_OK = "OK"; //$NON-NLS-1$

    static final ProtocolVersion MAX_PROTOCOL_VERSION_SUPPORTED = ProtocolVersion.VERSION_4;

    private static final int MIN_JAVASCRIPT_VERSION_CODE_NEEDED = 1;

    private static final int DEFAULT_JAVASCRIPT_VERSION_CODE = 1;

    /** Par&aacute;metro de entrada con el identificador de sesi&oacute;. */
	private static final String IDSESSION_PARAM = "idsession"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con los puertos en los que se puede intentar abrir el socket. */
	private static final String PORTS_PARAM = "ports"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la versi&oacute;n del protocolo que se va a utilizar. */
	private static final String PROTOCOL_VERSION_PARAM = "v"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la versi&oacute;n del JavaScript de invocaci&oacute;n. */
	private static final String JAVASCRIPT_VERSION_CODE_PARAM = "jvc"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la versi&oacute;n m&iacute;nima de aplicaci&oacute;n cliente solicitada. */
	static final String MIN_REQUESTED_VERSION_PARAM = "mcv"; //$NON-NLS-1$

	/**
	 * Puerto a trav&eacute;s del que se realizar&aacute; la comunicaci&oacute;n por WebSocket
	 * cuando no se indique ninguno.
	 */
	private static final int DEFAULT_WEBSOCKET_PORT = 63117;

    /** Clave privada fijada para reutilizarse en operaciones sucesivas. */
	private static PrivateKeyEntry stickyKeyEntry = null;

	/**
	 * Hilo para solicitar activamente a traves del servidor intermedio que se
	 * espere a que termine de ejecutarse la aplicaci&oacute;n.
	 */
    private static Thread activeWaitingThread = null;

    /**
     * Versi&oacute;n del protocolo de comunicaci&oacute;n solicitada.
     */
    private static int requestedProtocolVersion = -1;

	/**
	 * Recupera la entrada con la clave y certificado prefijados para las
	 * operaciones con certificados.
	 *
	 * @return Entrada con el certificado y la clave prefijados.
	 */
	public static PrivateKeyEntry getStickyKeyEntry() {
		return stickyKeyEntry;
	}

	/**
	 * Establece una clave y certificado prefijados para las operaciones con
	 * certificados.
	 *
	 * @param stickyKeyEntry Entrada con el certificado y la clave prefijados.
	 */
	public static void setStickyKeyEntry(final PrivateKeyEntry stickyKeyEntry) {
		ProtocolInvocationLauncher.stickyKeyEntry = stickyKeyEntry;
	}

    @SuppressWarnings({ "unused", "static-method" })
	void showAbout(final EventObject event) {
    	AboutDialog.showAbout(null);
    }

	/**
	 * Lanza la aplicaci&oacute;n y realiza las acciones indicadas en la URL. Este
	 * m&eacute;todo usa siempre comunicaci&oacute;n mediante servidor intermedio,
	 * nunca localmente.
	 *
     * @param urlString URL de invocaci&oacute;n por protocolo.
	 * @return Resultado de la operaci&oacute;n.
	 */
    public static String launch(final String urlString)  {
        return launch(urlString, -1, false);
    }

	/**
	 * Lanza la aplicaci&oacute;n y realiza las acciones indicadas en la URL.
	 *
     * @param urlString URL de invocaci&oacute;n por protocolo.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n
	 *                        utilizada por el solicitante.
	 * @param bySocket        Si se establece a <code>true</code> se usa una
	 *                        comuicaci&oacute;n de vuelta mediante conexi&oacute;n
	 *                        HTTP local (a <code>localhost</code>), si se establece
	 *                        a <code>false</code> se usa un servidor intermedio
     *                 para esta comunicaci&oacute;n de vuelta.
	 * @return Resultado de la operaci&oacute;n.
	 */
    public static String launch(final String urlString, final int protocolVersion, final boolean bySocket)  {
        // En macOS sobrecargamos el "Acerca de..." del sistema operativo, que tambien
        // aparece en la invocacion por protocolo
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
	    	try {
				final Method aboutMethod = ProtocolInvocationLauncher.class.getDeclaredMethod("showAbout", //$NON-NLS-1$
						EventObject.class);
	    		OSXHandler.setAboutHandler(null, aboutMethod);
			} catch (final Exception e) {
	    		LOGGER.warning("No ha sido posible establecer el menu 'Acerca de...' de OS X: " + e); //$NON-NLS-1$
			}
        }

        if (urlString == null) {
            LOGGER.severe("No se ha proporcionado una URL para la invocacion"); //$NON-NLS-1$
            ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_NULL_URI);
			return ProtocolInvocationLauncherErrorManager
					.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_NULL_URI);
        }
        if (!urlString.startsWith("afirma://")) { //$NON-NLS-1$
            LOGGER.severe("La URL de invocacion no comienza por 'afirma://'"); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager
					.showError(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROTOCOL);
			return ProtocolInvocationLauncherErrorManager
					.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROTOCOL);
        }

		// Configuramos el uso de JMulticard segun lo establecido en el dialogo de
		// preferencias
		final boolean jMulticardEnabled = PreferencesManager
				.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ENABLED_JMULTICARD);
        JMulticardUtilities.configureJMulticard(jMulticardEnabled);

        // Por defecto, usaremos la version de protocolo proporcionada para la operacion,
        // aunque se extraera de la URL de llamada en caso de se una peticion de apertura de
        // servicio de sockets o websockets
        requestedProtocolVersion = protocolVersion;

        // Extraemos los parametros de la URL
        final Map<String, String> urlParams = extractParams(urlString);

        // Comprobamos la version de codigo declarada por el JavaScript y establecemos una
        // por defecto si no la declara o no es valida
        int jvc = DEFAULT_JAVASCRIPT_VERSION_CODE;
        if (urlParams.containsKey(JAVASCRIPT_VERSION_CODE_PARAM)) {
        	try {
        	jvc = Integer.parseInt(urlParams.get(JAVASCRIPT_VERSION_CODE_PARAM));
        	}
        	catch (final Exception e) {
        		jvc = DEFAULT_JAVASCRIPT_VERSION_CODE;
			}
        }

        // Si la version de codigo JavaScript es menor de la exigida, mostramos
        // una advertencia
        if (jvc < MIN_JAVASCRIPT_VERSION_CODE_NEEDED) {
        	JOptionPane.showMessageDialog(
        			null,												// Componente padre
        			ProtocolMessages.getString("ProtocolLauncher.51"),	// Mensaje //$NON-NLS-1$
        			ProtocolMessages.getString("ProtocolLauncher.52"),	// Titulo //$NON-NLS-1$
        			JOptionPane.WARNING_MESSAGE);						// Tipo de mensaje
        }

        //TODO: Mejorar toda la logica de comunicacion:
        // - La comunicacion por sockets/websockets no deberia utilizar URLs.
        // - Se utiliza la excepcion SocketOperationException para gestionar los errores
        //   cuando la comunicacion NO es por sockets (contrariamente a lo indicado en el
        //   javadoc de los metodos y la excepcion.
        // - Los errores en el proceso siempre deberian lanzar una excepcion y no devolver
        //   una cadena con el mensaje del error.

        // Se invoca la aplicacion para iniciar la comunicacion por socket
        if (urlString.startsWith("afirma://websocket?") || urlString.startsWith("afirma://websocket/?")) { //$NON-NLS-1$ //$NON-NLS-2$
        	 LOGGER.info("Se inicia el modo de comunicacion por websockets: " + urlString); //$NON-NLS-1$

        	 requestedProtocolVersion = getVersion(urlParams);
        	 final ChannelInfo channelInfo = getChannelInfo(urlParams);

        	 // Si no se indica ningun puerto, es que usamos el protocolo v3, segun el cual el puerto
        	 // a traves del que se establecera la conexion sera el
        	 if (channelInfo.getPorts() == null) {
        		 LOGGER.severe("Usando puerto por defecto para la comunicacion WebSocket"); //$NON-NLS-1$
        		 channelInfo.setPorts(new int[] { DEFAULT_WEBSOCKET_PORT });
        	 }

        	 try {
        		 AfirmaWebSocketServerManager.startService(channelInfo, requestedProtocolVersion);
			} catch (final UnsupportedProtocolException e) {
             	LOGGER.severe("La version del protocolo no esta soportada (" + e.getVersion() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
             	final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE;
             	ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
             	forceCloseApplication(0);
			}
        	catch (final SocketOperationException e) {
               	LOGGER.log(Level.SEVERE, "No se pudo abrir ninguno de los puertos proporcionados", e); //$NON-NLS-1$
 				final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_OPEN_SOCKET;
               	ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
               	forceCloseApplication(0);
            }
//        	 catch (final GeneralSecurityException | IOException e) {
//              	LOGGER.log(Level.SEVERE, "Ocurrio un error durante la carga del almacen de claves", e); //$NON-NLS-1$
//				final String errorCode = e instanceof IOException
//						? ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_FIND_SSL_KEYSTORE
//						: ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_ACCESS_SSL_KEYSTORE;
//              	ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
//              	return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
//              }

             return RESULT_OK;
        }
        // Se invoca la aplicacion para iniciar la comunicacion por socket
        else if (urlString.startsWith("afirma://service?") || urlString.startsWith("afirma://service/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se inicia el modo de comunicacion por sockets: " + urlString); //$NON-NLS-1$

       	 	requestedProtocolVersion = getVersion(urlParams);
       	 	final ChannelInfo channelInfo = getChannelInfo(urlParams);

       	 	// El listado de puertos de entre los que seleccionar uno es obligatorio
       	 	// en esta opcion
       	 	if (channelInfo.getPorts() == null) {
       	 		LOGGER.log(Level.SEVERE, "No se ha proporcionado el listado de puertos para la conexion"); //$NON-NLS-1$
       	 		final String errorCode = ProtocolInvocationLauncherErrorManager.ERROR_PARAMS;
       	 		ProtocolInvocationLauncherErrorManager.showError(errorCode);
       	 		return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
       	 	}

            try {
            	ServiceInvocationManager.startService(channelInfo, requestedProtocolVersion);
			} catch (final UnsupportedProtocolException e) {
            	LOGGER.severe("La version del protocolo no esta soportada (" + e.getVersion() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
				final String errorCode = e.isNewVersionNeeded()
						? ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE
						: ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE;
            	ProtocolInvocationLauncherErrorManager.showError(errorCode, e);
            	return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
            }

            return RESULT_OK;
        }
        // Se solicita una operacion de firma batch
        else if (urlString.startsWith("afirma://batch?") || urlString.startsWith("afirma://batch/?")) { //$NON-NLS-1$ //$NON-NLS-2$
        	LOGGER.info("Se invoca a la aplicacion para el procesado de un lote de firma"); //$NON-NLS-1$

        	try {
                UrlParametersForBatch params =
                		ProtocolInvocationUriParserUtil.getParametersToBatch(urlParams, !bySocket);

                if (requestedProtocolVersion == -1) {
               		requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
                }

				// Si se indica un identificador de fichero, es que el JSON o XML de definicion de lote
				// se tiene que
                // descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                    final byte[] batchDefinition;
                    try {
                    	batchDefinition = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
					} catch (final InvalidEncryptedDataLengthException e) {
                        LOGGER.log(Level.SEVERE, "No se pueden recuperar los datos del servidor: " + e, e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA);
					} catch (final DecryptionException e) {
                        LOGGER.severe("Error al descifrar los datos obtenidos: " + e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA);
                    }

                    final Map <String, String> paramsMap;

                    if (params.isJsonBatch()) {
                    	paramsMap = TriphaseDataParser.parseParamsListJson(batchDefinition);
                    } else {
                    	paramsMap = ProtocolInvocationUriParserUtil.parseXml(batchDefinition);
                    }
					params = ProtocolInvocationUriParserUtil.getParametersToBatch(paramsMap, !bySocket);
                }

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

				LOGGER.info(
						"Se inicia la operacion de firma de lote. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

                try {
                    return  ProtocolInvocationLauncherBatch.processBatch(params, requestedProtocolVersion, bySocket);
				} catch (final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de firma por lotes: " + e); //$NON-NLS-1$
                    final String msg = e.getErrorCode() == ProtocolInvocationLauncherBatch.getResultCancel()
                    		? e.getErrorCode()
                    		: URLEncoder.encode(
                    				ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode()),
                    				StandardCharsets.UTF_8.toString());
                    sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                    return ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode());
                }
			} catch (final ParameterException e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma por lote: " + e, e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showErrorDetail(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
			} catch (final Exception e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma por lote: " + e, e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
            }
        }
        // Se solicita una operacion de seleccion de certificado
        else if (urlString.startsWith("afirma://selectcert?") || urlString.startsWith("afirma://selectcert/?")) { //$NON-NLS-1$ //$NON-NLS-2$
        	LOGGER.info("Se invoca a la aplicacion para la seleccion de un certificado"); //$NON-NLS-1$

            try {
                UrlParametersToSelectCert params =
                		ProtocolInvocationUriParserUtil.getParametersToSelectCert(urlParams, !bySocket);

                if (requestedProtocolVersion == -1) {
               		requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
                }

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
					} catch (final InvalidEncryptedDataLengthException e) {
                    	LOGGER.log(Level.SEVERE, "No se pueden recuperar los datos del servidor: " + e, e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA);
					} catch (final DecryptionException e) {
                        LOGGER.severe("Error al descifrar: " + e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA);
                    }

                    params = ProtocolInvocationUriParser.getParametersToSelectCert(xmlData, true);
                }

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

				LOGGER.info("Se inicia la operacion de seleccion de certificado. Version de protocolo: " //$NON-NLS-1$
						+ requestedProtocolVersion);

                try {
					return ProtocolInvocationLauncherSelectCert.processSelectCert(params, requestedProtocolVersion,
							bySocket);
				} catch (final AOCancelledOperationException e) {
                	return ProtocolInvocationLauncherSelectCert.getResultCancel();
				} catch (final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de seleccion de certificados: " + e); //$NON-NLS-1$
                    final String msg = e.getErrorCode() == ProtocolInvocationLauncherSelectCert.getResultCancel()
                    		? e.getErrorCode()
                    		: URLEncoder.encode(
                    				ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode()),
                    				StandardCharsets.UTF_8.toString());
                    sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
					return ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode());
                }
			} catch (final ParameterException e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de seleccion de certificados: " + e, e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showErrorDetail(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
			} catch (final Exception e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de seleccion de certificados: " + e, e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
            }
        }
        // Se solicita una operacion de guardado
        else if (urlString.startsWith("afirma://save?") || urlString.startsWith("afirma://save/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para el guardado de datos"); //$NON-NLS-1$

            try {
                UrlParametersToSave params =
                		ProtocolInvocationUriParserUtil.getParametersToSave(urlParams, !bySocket);

                if (requestedProtocolVersion == -1) {
               		requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
                }

                LOGGER.info("Cantidad de datos a guardar: " + (params.getData() == null ? 0 : params.getData().length)); //$NON-NLS-1$

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {

                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
					} catch (final InvalidEncryptedDataLengthException e) {
                    	LOGGER.log(Level.SEVERE, "No se pueden recuperar los datos del servidor: " + e, e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA);
					} catch (final DecryptionException e) {
                        LOGGER.severe("Error al descifrar: " + e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA);
                    }

                    params = ProtocolInvocationUriParser.getParametersToSave(xmlData, true);
                }

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

                LOGGER.info("Se inicia la operacion de guardado. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

                try {
                	return  ProtocolInvocationLauncherSave.processSave(params, requestedProtocolVersion, bySocket);
                }
				// solo entra en la excepcion en el caso de que haya que devolver errores a
				// traves del servidor intermedio
                catch (final SocketOperationException e) {
                    LOGGER.log(Level.SEVERE, "Error en la operacion de guardado: " + e, e); //$NON-NLS-1$
                    final String msg = e.getErrorCode() == ProtocolInvocationLauncherSave.getResultCancel()
                    		? e.getErrorCode()
                    		: URLEncoder.encode(
                    				ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode()),
                    				StandardCharsets.UTF_8.toString());
                    sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                    return ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode());
                }
			} catch (final ParameterNeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP);
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED);
			} catch (final ParameterException e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de guardado", e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showErrorDetail(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
			} catch (final Exception e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de guardado", e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
            }
        }

		// Se solicita una operacion de firma/multifirma seguida del guardado del
		// resultado
        else if (urlString.startsWith("afirma://signandsave?") || urlString.startsWith("afirma://signandsave/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para la firma/multifirma y el guardado del resultado"); //$NON-NLS-1$

            try {
                UrlParametersToSignAndSave params =
                		ProtocolInvocationUriParserUtil.getParametersToSignAndSave(urlParams, !bySocket);

                if (requestedProtocolVersion == -1) {
               		requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
                }

				LOGGER.info("Cantidad de datos a firmar y guardar: " //$NON-NLS-1$
						+ (params.getData() == null ? 0 : params.getData().length));

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {

                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
					} catch (final InvalidEncryptedDataLengthException e) {
                    	LOGGER.log(Level.SEVERE, "No se pueden recuperar los datos del servidor", e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA);
					} catch (final DecryptionException e) {
                        LOGGER.severe("Error al descifrar: " + e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA);
                    }

                    params = ProtocolInvocationUriParser.getParametersToSignAndSave(xmlData, true);
                }

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

				LOGGER.info("Se inicia la operacion de firma y guardado. Version de protocolo: " //$NON-NLS-1$
						+ requestedProtocolVersion);

				StringBuilder dataToSend;
                try {
                	dataToSend =  ProtocolInvocationLauncherSignAndSave.processSign(params, requestedProtocolVersion);
                }
                // Llegara aqui siempre que tratemos con un error controlado. En caso de estar en
                // la comunicacion por servidor intermedio, el mensaje de error al servidor
                // intermedio y despues revolveremos el error. En caso de estar en la comunicacion
                // por sockets, directamente devolveremos el error.
                catch(final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de firma: " + e); //$NON-NLS-1$
                    String msg;
                    final String errorCode = e.getErrorCode();
                    if (ProtocolInvocationLauncherSignAndSave.RESULT_CANCEL.equals(errorCode)) {
                    	msg = errorCode;
                    // Si el mensaje es igual al codigo, es que no se establecio mensaje
                    //TODO: Comprobar si realmente no tiene mensaje, en lugar de si el mensaje y el codigo son distintos
                    } else if (!errorCode.equals(e.getMessage())) {
                    	msg = errorCode + ": " + e.getMessage(); //$NON-NLS-1$
                    } else {
                    	msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
                    }
                    if (!bySocket) {
                    	msg = URLEncoder.encode(msg, StandardCharsets.UTF_8.toString());
                        sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                    }
                    return msg;
                }

                // Si no es por sockets, se devuelve el resultado al servidor y detenemos la
                // espera activa si se encontraba vigente
                if (!bySocket) {
                	LOGGER.info("Enviamos el resultado de la operacion de firma y guardado al servidor intermedio"); //$NON-NLS-1$
                	sendDataToServer(dataToSend.toString(), params.getStorageServletUrl().toString(), params.getId());
                }

                return dataToSend.toString();
			} catch (final ParameterNeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP);
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED);
			} catch (final ParameterException e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma y guardado: " + e, e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showErrorDetail(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
			} catch (final Exception e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma y guardado: " + e, e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
            }
        }

        // Se solicita una operacion de firma/cofirma/contrafirma
        else if (urlString.startsWith("afirma://sign?")        || urlString.startsWith("afirma://sign/?") || //$NON-NLS-1$ //$NON-NLS-2$
                 urlString.startsWith("afirma://cosign?")      || urlString.startsWith("afirma://cosign/?") || //$NON-NLS-1$ //$NON-NLS-2$
                 urlString.startsWith("afirma://countersign?") || urlString.startsWith("afirma://countersign/?") //$NON-NLS-1$ //$NON-NLS-2$
        ) {
            LOGGER.info("Se invoca a la aplicacion para realizar una operacion de firma/multifirma"); //$NON-NLS-1$

            try {
                UrlParametersToSign params =
                		ProtocolInvocationUriParserUtil.getParametersToSign(urlParams, !bySocket);

                if (requestedProtocolVersion == -1) {
               		requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
                }

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {

                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
					} catch (final InvalidEncryptedDataLengthException | IOException e) {
                    	LOGGER.log(Level.SEVERE, "No se pueden recuperar los datos del servidor", e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA);
					} catch (final DecryptionException e) {
                        LOGGER.log(Level.SEVERE, "Error al descifrar", e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA);
                    }
                    params = ProtocolInvocationUriParser.getParametersToSign(xmlData, true);
                }

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

                LOGGER.info("Se inicia la operacion de firma. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

                StringBuilder dataToSend;
                try {
                	dataToSend = ProtocolInvocationLauncherSign.processSign(params, requestedProtocolVersion, null);
                }
                // Llegara aqui siempre que tratemos con un error controlado. En caso de estar en
                // la comunicacion por servidor intermedio, el mensaje de error al servidor
                // intermedio y despues revolveremos el error. En caso de estar en la comunicacion
                // por sockets, directamente devolveremos el error.
                catch(final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de firma: " + e); //$NON-NLS-1$
                    String msg;
                    final String errorCode = e.getErrorCode();
                    if (ProtocolInvocationLauncherSign.RESULT_CANCEL.equals(errorCode)) {
                    	msg = errorCode;
                    // Si el mensaje es igual al codigo, es que no se establecio mensaje
                    //TODO: Comprobar si realmente no tiene mensaje, en lugar de si el mensaje y el codigo son distintos
                    } else if (!errorCode.equals(e.getMessage())) {
                    	msg = errorCode + ": " + e.getMessage(); //$NON-NLS-1$
                    } else {
                    	msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
                    }
                    if (!bySocket) {
                    	msg = URLEncoder.encode(msg, StandardCharsets.UTF_8.toString());
						sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                    }
                    return msg;
                }

                // Si no es por sockets, se devuelve el resultado al servidor y detenemos la
                // espera activa si se encontraba vigente
                if (!bySocket) {
                	LOGGER.info("Enviamos el resultado de la operacion de firma al servidor intermedio"); //$NON-NLS-1$
                	sendDataToServer(dataToSend.toString(), params.getStorageServletUrl().toString(), params.getId());
                }

                return dataToSend.toString();
            }
            catch(final ParameterNeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP);
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED);
			} catch (final ParameterException e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de firma", e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showErrorDetail(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
			} catch (final Exception e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de firma", e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
            }
        }

        // Se solicita una operacion de carga de ficheros
        else if (urlString.startsWith("afirma://load?") || urlString.startsWith("afirma://load/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para realizar una operacion de carga de uno o varios ficheros"); //$NON-NLS-1$

            try {
                UrlParametersToLoad params =
                		ProtocolInvocationUriParserUtil.getParametersToLoad(urlParams);

                if (requestedProtocolVersion == -1) {
               		requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
                }

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {

                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
					} catch (final InvalidEncryptedDataLengthException | IOException e) {
                    	LOGGER.log(Level.SEVERE, "No se pueden recuperar los datos del servidor", e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_DATA);
					} catch (final DecryptionException e) {
                        LOGGER.log(Level.SEVERE, "Error al descifrar", e); //$NON-NLS-1$
						ProtocolInvocationLauncherErrorManager
								.showError(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA, e);
						return ProtocolInvocationLauncherErrorManager
								.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_DECRYPTING_DATA);
                    }
                    params = ProtocolInvocationUriParser.getParametersToLoad(xmlData);
                }

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

                LOGGER.info("Se inicia la operacion de carga. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

                try {
                    return ProtocolInvocationLauncherLoad.processLoad(params, requestedProtocolVersion, bySocket);
                }
				// solo entra en la excepcion en el caso de que haya que devolver errores a
				// traves del servidor intermedio
                catch(final SocketOperationException e) {
                    LOGGER.severe("La operacion indicada no esta soportada: " + e); //$NON-NLS-1$
                    final String msg = e.getErrorCode() == ProtocolInvocationLauncherLoad.getResultCancel()
                    		? e.getErrorCode()
                    		: URLEncoder.encode(
                    				ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode()),
                    				StandardCharsets.UTF_8.toString());
                    sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                    return ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode());
                }
			} catch (final ParameterNeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_OBSOLETE_APP);
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showError(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_LOCAL_ACCESS_BLOCKED);
			} catch (final ParameterException e) {
                LOGGER.severe("Error en los parametros de carga: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager
						.showErrorDetail(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
			} catch (final Exception e) {
                LOGGER.severe("Error en los parametros de carga: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS, e);
				return ProtocolInvocationLauncherErrorManager
						.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_PARAMS);
            }
        }

		LOGGER.severe("La operacion indicada en la URL no esta soportada: " + //$NON-NLS-1$
    				urlString.substring(0, Math.min(30, urlString.length())) + "..." //$NON-NLS-1$
		);
		ProtocolInvocationLauncherErrorManager
				.showError(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_OPERATION);
		return ProtocolInvocationLauncherErrorManager
				.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_OPERATION);
    }

    /**
	 * Inicia el proceso de solicitud de espera activa a trav&eacute;s del servidor
	 * intermedio.
	 *
	 * @param storageServletUrl URL del servicio de guardado en el servidor
	 *                          intermedio.
	 * @param id                Identificador de la transacci&oacute;n para la que
	 *                          se le solicita la espera.
     */
    private static void requestWait(final URL storageServletUrl, final String id) {
    	try {
	    	activeWaitingThread = new ActiveWaitingThread(storageServletUrl.toString(), id);
	    	activeWaitingThread.start();
		} catch (final Exception e) {
			LOGGER.warning("Se ha interrumpido la espera activa para la conexion con servidor intermedio: " + e); //$NON-NLS-1$
		}
	}

	/** Env&iacute;a datos al servidor intermedio e interrumpe la espera declarada en
	 * este servidor.
     * @param data Cadena de texto.
     * @param serviceUrl URL del servicio de env&iacute;o de datos.
	 * @param id         Identificador del mensaje en el servidor.
	 */
	private static void sendDataToServer(final String data, final String serviceUrl, final String id) {
		// Detenemos la espera activa
		final Thread waitingThread = getActiveWaitingThread();
		if (waitingThread != null) {
			waitingThread.interrupt();
		}
		// Esperamos a que termine cualquier otro envio al servidor para que no se pisen
		synchronized (IntermediateServerUtil.getUniqueSemaphoreInstance()) {
			try {
				IntermediateServerUtil.sendData(data, serviceUrl, id);
			} catch (final IOException e) {
				LOGGER.log(Level.SEVERE, "Error al enviar los datos al servidor intermedio: " + e, e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(
						ProtocolInvocationLauncherErrorManager.ERROR_SENDING_RESULT, e);
			}
		}
	}

	/**
	 * Obtiene el hilo encargado de solicitar a trav&eacute;s del servidor
	 * intermedio que se realice una espera activa del resultado de la
	 * operaci&oacute;n actual.
	 *
	 * @return Hilo que solicita reiteradamente la espera o {@code null} si no se
	 *         inici&oacute; la espera activa.
	 */
	public static Thread getActiveWaitingThread() {
		return activeWaitingThread;
	}

	/**
	 * Parsea la cadena con la versi&oacute;n del protocolo de comunicacion
	 * solicitada.
	 *
	 * @param version Declarada del protocolo.
	 * @return Version de protocolo o {@code 1} si no era una cadena v&aacute;lida.
	 */
	private static int parseProtocolVersion(final String version) {
		int protocolVersion;
    	try {
    		protocolVersion = Integer.parseInt(version);
		} catch (final Exception e) {
    		protocolVersion = 1;
		}
    	return protocolVersion;
	}


	/**
	 * Obtiene el valor asignado al par&aacute;metro de versi&oacute;n de una URL.
	 * @param params Par&acute;metros declarados en una URL.
	 * @return Valor del par&aacute;metro de versi&oacute;n ('v') o el valor '1' si no est&aacute; definido.
	 */
	private static int getVersion(final Map<String, String> params) {

		// Si se encuentra el parametro con la version, se devuelve. Si no, se devuelve
		// 1.
		int protocolVersion = 1;
		final String protocolId = params.get(PROTOCOL_VERSION_PARAM);
		if (protocolId != null) {
			try {
				protocolVersion = Integer.parseInt(protocolId.trim());
			} catch (final Exception e) {
				LOGGER.info("El ID de protocolo indicado no es un numero entero (" + protocolId + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}

		return protocolVersion;
	}

	/**
	 * Extrae los parametros declarados en una URL con sus valores asignados.
	 * @param url URL de la que extraer los par&aacute;metros.
	 * @return Conjunto de par&aacute;metros con sus valores.
	 */
	private static Map<String, String> extractParams(final String url) {

		final Map<String, String> params = new HashMap<>();

		final int initPos = url.indexOf('?') + 1;
		final String[] urlParams = url.substring(initPos).split("&"); //$NON-NLS-1$
		for (final String param : urlParams) {
			final int equalsPos = param.indexOf('=');
			if (equalsPos > 0) {
				try {
					params.put(
							param.substring(0, equalsPos),
							URLDecoder.decode(param.substring(equalsPos + 1), StandardCharsets.UTF_8.toString()));
				} catch (final UnsupportedEncodingException e) {
					LOGGER.warning("No se pudo decodificar el valor del parametro '" + param.substring(0, equalsPos) + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}

		return params;
	}


	/** Obtiene los puertos que se deben probar para la conexi&oacute;n externa.
	 * Asigna cual es la clave.
	 * @param urlParams Par&aacute;metros de la URL de entre los que obtener los puertos.
	 * @return Listados de puertos. */
	private static ChannelInfo getChannelInfo(final Map<String, String> urlParams) {

		int[] ports = null;
		final String ps = urlParams.get(PORTS_PARAM);
		if (ps != null) {
			final String[] portsText = ps.split(","); //$NON-NLS-1$
			ports = new int[portsText.length];
			for (int i = 0; i < portsText.length; i++) {
				try {
					ports[i] = Math.abs(Integer.parseInt(portsText[i]));
				}
				catch(final Exception e) {
					throw new IllegalArgumentException(
						"El parametro 'ports' de la URI de invocacion contiene valores no numericos: " + e //$NON-NLS-1$
					, e);
				}
			}
		}

		String idSession = urlParams.get(IDSESSION_PARAM);
		if (idSession != null && !idSession.isEmpty()){
		    LOGGER.info("Se ha recibido un id de sesion: " + idSession); //$NON-NLS-1$
		    // El ID de sesion solo puede estar conformado por numeros. Usar otra cadena nos expondria
		    // a una injeccion de codigo en los AppleScripts que se ejecuten con el
		    boolean valid = true;
		    for (final char c : idSession.toCharArray()) {
		    	if (!Character.isLetterOrDigit(c)) {
		    		valid = false;
		    		break;
		    	}
		    }
		    if (!valid) {
		    	LOGGER.info("No se ha proporcionado un id de sesion valido"); //$NON-NLS-1$
		    	idSession = null;
		    }
		}
		else {
            LOGGER.info("No se utilizara id para la sesion"); //$NON-NLS-1$
        }

		return new ChannelInfo(idSession, ports);
	}

	/**
	 * Cierra la aplicaci&oacute;n.
	 *
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
	 *                 indica error y cero indica salida normal.
	 */
    public static void forceCloseApplication(final int exitCode) {
       	Runtime.getRuntime().halt(exitCode);
    }
}
