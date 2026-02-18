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
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.SSLHandshakeException;
import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.InvalidDomainSSLCertificateException;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.misc.protocol.ParameterLocalAccessRequestedException;
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
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.protocol.ProtocolInvocationLauncherUtil.DecryptionException;
import es.gob.afirma.standalone.ui.AboutDialog;
import es.gob.afirma.standalone.ui.OSXHandler;
import es.gob.afirma.standalone.ui.tasks.LoadKeystoreTask;

/**
 * Gestiona la ejecuci&oacute;n de Autofirma en una invocaci&oacute;n por
 * protocolo y bajo un entorno compatible <code>Swing</code>.
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class ProtocolInvocationLauncher {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String OK_RESPONSE = "OK"; //$NON-NLS-1$

    private static final ProtocolVersion[] SUPPORTED_PROTOCOLS = {
    		ProtocolVersion.getInstance(ProtocolVersion.VERSION_4_1),
    		ProtocolVersion.getInstance(ProtocolVersion.VERSION_3),
    		ProtocolVersion.getInstance(ProtocolVersion.VERSION_2),
    		ProtocolVersion.getInstance(ProtocolVersion.VERSION_1)
    };

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
    private static ProtocolVersion requestedProtocolVersion = null;

    private static LoadKeystoreTask loadKeyStoreTask = null;

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
        return launch(urlString, null, false);
    }

    /**
     * Edita la version de protocolo si es necesario para ajustar el funcionamiento para la compatibilidad
     * con versiones antiguas que no inclu&iacute;an la versi&oacute;n menor dentro de la versi&oacute;n
     * de protocolo.
     * @param protocolVersion Versi&oacute;n de protocolo declarada.
     * @param scriptVesionCode Versi&oacute;n de c&oacute;digo del script de invocaci&oacute;n.
     */
    private static void reviewProtocolVersion(final ProtocolVersion protocolVersion, final int scriptVesionCode) {
		// Hacemos aqui un arreglo para incorporar las mejoras de Autoscript 1.10 sin forzar a
    	// cambiar de version de protocolo de la llamada, ya que esto romperia compatibilidad
    	// entre Autoscript 1.10 y Autofirma 1.9 (obligaria a los usuarios a actualizar a Autofirma
    	// 1.10). En futuras versiones esto se podra hacer con el uso de la MinorVersion del
    	// protocolo, pero Autofirma 1.9 aun no lo admit&iacute;a, as&iacute; que no se puede usar
    	// en Autoscript 1.10 para mantener la compatibilidad).
		// Vamos a editar la version del protocolo recibido cuando recibamos una versi&oacute;n
		// de c&oacute;digo mayor a 3 (la de Autoscript 1.10), pero el protocolo siga siendo la
		// versi&oacute;n 4 (la de de Autoscript 1.9 y 1.10). Estableceremos que esta es la version
    	// de protocolo 4.1, y as&iacute; sabremos que podemos usar las mejoras de Autoscript y
    	// Autofirma 1.10.
		if (protocolVersion.getMajorVersion() == 4 && scriptVesionCode > 3) {
			requestedProtocolVersion = ProtocolVersion.getInstance(ProtocolVersion.VERSION_4_1);
		}
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
    public static String launch(final String urlString, final ProtocolVersion protocolVersion, final boolean bySocket)  {

    	LOGGER.info("Se recibe una llamada de operacion"); //$NON-NLS-1$

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
            final ErrorCode error = SimpleErrorCode.Request.REQUEST_URI_NOT_FOUND;
            ProtocolInvocationLauncherErrorManager.showError(protocolVersion, error);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(protocolVersion, error);
        }
        if (!urlString.startsWith("afirma://") && !urlString.startsWith("getresult?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.severe("La URL de invocacion no comienza por 'afirma://'"); //$NON-NLS-1$
            final ErrorCode error = SimpleErrorCode.Request.UNSUPPORTED_REQUEST_SCHEME;
            ProtocolInvocationLauncherErrorManager.showError(protocolVersion, error);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(protocolVersion, error);
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
        	LOGGER.info("Se inicia el modo de comunicacion por websockets"); //$NON-NLS-1$

        	requestedProtocolVersion = getVersion(urlParams);
        	reviewProtocolVersion(requestedProtocolVersion, jvc);

        	final ChannelInfo channelInfo = getChannelInfo(urlParams);

        	// Si no se indica ningun puerto, es que usamos el protocolo v3, segun el cual el puerto
        	// a traves del que se establecera la conexion sera el
        	if (channelInfo.getPorts() == null) {
        		LOGGER.severe("Usando puerto por defecto para la comunicacion WebSocket"); //$NON-NLS-1$
        		channelInfo.setPorts(new int[] { DEFAULT_WEBSOCKET_PORT });
        	}

        	try {

        		// A partir de Autoscript 1.10 se admite la llamada asincrona a traves de Websockets
        		final boolean asynchronous = jvc > 3;

        		AfirmaWebSocketServerManager.startService(channelInfo, requestedProtocolVersion, asynchronous);
        	} catch (final UnsupportedProtocolException e) {
        		LOGGER.log(Level.SEVERE, "La version del protocolo no esta soportada (" + e.getVersion() + "). Se cerrara la aplicacion" , e); //$NON-NLS-1$ //$NON-NLS-2$
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
        		forceCloseApplication(0);
        	}
        	catch (final SocketOperationException e) {
        		LOGGER.log(Level.SEVERE, "No se pudo abrir ninguno de los puertos proporcionados. Se cerrara la aplicacion", e); //$NON-NLS-1$
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, SimpleErrorCode.Internal.SOCKET_INITIALIZING_ERROR);
        		forceCloseApplication(0);
        	} catch (final SllKeyStoreException e) {
        		LOGGER.log(Level.SEVERE, "No se ha encontrado o no ha podido cargarse el almacen del certificado SSL. Se cerrara la aplicacion", e); //$NON-NLS-1$
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, SimpleErrorCode.Internal.LOADING_SSL_KEYSTORE_ERROR);
        		forceCloseApplication(0);
			}

        	return OK_RESPONSE;
        }
		if (urlString.startsWith("afirma://service?") || urlString.startsWith("afirma://service/?")) { //$NON-NLS-1$ //$NON-NLS-2$
        	LOGGER.info("Se inicia el modo de comunicacion por sockets"); //$NON-NLS-1$

        	requestedProtocolVersion = getVersion(urlParams);
        	reviewProtocolVersion(requestedProtocolVersion, jvc);

        	final ChannelInfo channelInfo = getChannelInfo(urlParams);

        	// El listado de puertos de entre los que seleccionar uno es obligatorio
        	// en esta opcion
        	if (channelInfo.getPorts() == null) {
        		LOGGER.log(Level.SEVERE, "No se ha proporcionado el listado de puertos para la conexion"); //$NON-NLS-1$
        		final ErrorCode errorCode = SimpleErrorCode.Request.PORTS_NOT_FOUND;
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
        		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
        	}

        	try {
        		ServiceInvocationManager.startService(channelInfo, requestedProtocolVersion);
        	} catch (final UnsupportedProtocolException e) {
        		LOGGER.severe("La version del protocolo no esta soportada (" + e.getVersion() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
        		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
        	} catch (final SllKeyStoreException e) {
        		LOGGER.log(Level.SEVERE, "No se ha encontrado o no ha podido cargarse el almacen del certificado SSL. Se cerrara la aplicacion", e); //$NON-NLS-1$
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, SimpleErrorCode.Internal.LOADING_SSL_KEYSTORE_ERROR);
        		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final IOException e) {
        		LOGGER.log(Level.SEVERE, "No se ha podido abrir el socket o se ha cerrado durante la operacion. Se cierra la aplicacion", e); //$NON-NLS-1$
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, SimpleErrorCode.Internal.SOCKET_INITIALIZING_ERROR);
        		forceCloseApplication(0);
			}

        	return OK_RESPONSE;
        }
		if (urlString.startsWith("afirma://batch?") || urlString.startsWith("afirma://batch/?")) { //$NON-NLS-1$ //$NON-NLS-2$
        	LOGGER.info("Se invoca a la aplicacion para el procesado de un lote de firma"); //$NON-NLS-1$

        	try {
                UrlParametersForBatch params =
                		ProtocolInvocationUriParserUtil.getParametersToBatch(urlParams, !bySocket);

				// Si se indica un identificador de fichero, es que el JSON o XML de definicion de lote
				// se tiene que
                // descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                	try {
                		final byte[] batchDefinition;
                		try {
                			batchDefinition = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                		} catch (final DecryptionException e) {
                			throw new IntermediateServerErrorSendedException("Error al descifrar los datos obtenidos", e); //$NON-NLS-1$
                		} catch (final SocketTimeoutException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Se excedio el tiempo de espera de la llamada al servico de recuperacion del servidor intermedio cuando se trataron de obtener los datos para la firma de lote", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_BATCH_TIMEOUT);
                		} catch (final IOException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al recuperar los datos enviados por el cliente a traves del servidor intermedio", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_BATCH_OPERATION);
                		}

                		final Map <String, String> paramsMap;

                		if (params.isJsonBatch()) {
                			paramsMap = TriphaseDataParser.parseParamsListJson(batchDefinition);
                		} else {
                			paramsMap = ProtocolInvocationUriParserUtil.parseXml(batchDefinition);
                		}
                		params = ProtocolInvocationUriParserUtil.getParametersToBatch(paramsMap, !bySocket);
                	} catch (final IntermediateServerErrorSendedException e) {
                		LOGGER.log(Level.SEVERE, "Se obtuvo un error al descargar los datos del servidor intermedio", e); //$NON-NLS-1$
                		processIntermediateServiceError(e.getMessage(), e.getErrorCode(), params.getStorageServletUrl(), params.getId());
                		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                	}
                }

                // Si la peticion no se hizo a traves de socket/websocket, la version de protocolo se indica en la propia operacion
                if (requestedProtocolVersion == null) {
               		requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
                	reviewProtocolVersion(requestedProtocolVersion, jvc);
                }

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

				LOGGER.info(
						"Se inicia la operacion de firma de lote. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

				String msg;
                try {
                    msg = ProtocolInvocationLauncherBatch.processBatch(params, requestedProtocolVersion);
				}
                catch(final AOCancelledOperationException e) {
                	LOGGER.severe("Operacion de firma por lotes cancelada por el usuario"); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }
				// solo entra en la excepcion en el caso de que haya que devolver errores a
				// traves del servidor intermedio
                catch (final SocketOperationException e) {
                    LOGGER.log(Level.SEVERE, "Error durante la operacion de firma por lotes", e); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }

                // Si no es por sockets, se devuelve el resultado al servidor y detenemos la
                // espera activa si se encontraba vigente
                if (!bySocket) {
                	LOGGER.info("Enviamos el resultado de la operacion de firma por lotes al servidor intermedio"); //$NON-NLS-1$
                	sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                }

                return msg;
        	} catch (final ParameterException e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma por lotes: " + e, e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final Exception e) {
                LOGGER.log(Level.SEVERE, "Error en la operacion de firma por lotes: " + e, e); //$NON-NLS-1$
				final ErrorCode errorCode = SimpleErrorCode.Internal.UNKNOWN_BATCH_ERROR;
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
            }
        }
		if (urlString.startsWith("afirma://selectcert?") || urlString.startsWith("afirma://selectcert/?")) { //$NON-NLS-1$ //$NON-NLS-2$
        	LOGGER.info("Se invoca a la aplicacion para la seleccion de un certificado"); //$NON-NLS-1$

        	try {
        		UrlParametersToSelectCert params =
        				ProtocolInvocationUriParserUtil.getParametersToSelectCert(urlParams, !bySocket);

        		// Si se indica un identificador de fichero, es que la configuracion de la
        		// operacion
        		// se tiene que descargar desde el servidor intermedio
        		if (params.getFileId() != null) {
        			try {
        				final byte[] xmlData;
        				try {
        					xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
        				} catch (final DecryptionException e) {
        					throw new IntermediateServerErrorSendedException(
        							"Error al descifrar los datos obtenidos", e); //$NON-NLS-1$
        				} catch (final SocketTimeoutException e) {
        					throw new IntermediateServerErrorSendedException(
        							"Se excedio el tiempo de espera de la llamada al servico de recuperacion del servidor intermedio cuando se trataron de obtener los datos para la seleccion de certificado", e, //$NON-NLS-1$
        							SimpleErrorCode.Communication.RECIVING_DATA_OF_CERT_TIMEOUT);
        				} catch (final IOException e) {
        					throw new IntermediateServerErrorSendedException(
        							"Error al recuperar los datos enviados por el cliente a traves del servidor intermedio", e, //$NON-NLS-1$
        							SimpleErrorCode.Communication.RECIVING_DATA_OF_CERT_OPERATION);
        				}

        				params = ProtocolInvocationUriParser.getParametersToSelectCert(xmlData, true);
        			} catch (final IntermediateServerErrorSendedException e) {
        				LOGGER.log(Level.SEVERE, "Se obtuvo un error al descargar los datos del servidor intermedio", e); //$NON-NLS-1$
        				processIntermediateServiceError(e.getMessage(), e.getErrorCode(), params.getStorageServletUrl(), params.getId());
        				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
        			}
        		}

        		// Si la peticion no se hizo a traves de socket/websocket, la version de protocolo se indica en la propia operacion
        		if (requestedProtocolVersion == null) {
        			requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
        			reviewProtocolVersion(requestedProtocolVersion, jvc);
        		}

        		// En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
        		// que se espere activamente hasta el fin de la tarea
        		if (!bySocket && params.isActiveWaiting()) {
        			requestWait(params.getStorageServletUrl(), params.getId());
        		}

        		LOGGER.info("Se inicia la operacion de seleccion de certificado. Version de protocolo: " //$NON-NLS-1$
        				+ requestedProtocolVersion);

        		String msg;
        		boolean errorConnectingServer = false;
        		try {
        			msg = ProtocolInvocationLauncherSelectCert.processSelectCert(params, requestedProtocolVersion);
        		}
        		catch (final SSLHandshakeException e) {
					LOGGER.log(Level.SEVERE, "Error al realizar una conexion segura con el servidor", e); //$NON-NLS-1$
					final ErrorCode errorCode = SimpleErrorCode.Communication.SENDING_RESULT_OPERATION;
					ProtocolInvocationLauncherErrorManager.showError(protocolVersion, errorCode);
					msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(protocolVersion, errorCode);
					errorConnectingServer = true;
				}
        		catch(final AOCancelledOperationException e) {
        			LOGGER.severe("Operacion de seleccion de certificado cancelada por el usuario"); //$NON-NLS-1$
        			msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
        		}
        		// solo entra en la excepcion en el caso de que haya que devolver errores a
        		// traves del servidor intermedio
        		catch (final SocketOperationException e) {
        			LOGGER.log(Level.SEVERE, "Error durante la operacion de seleccion de certificado", e); //$NON-NLS-1$
        			msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
        		}

        		// Si no es por sockets, y si no hay errores de conexion con el servidor,
        		// se devuelve el resultado al servidor y detenemos la
        		// espera activa si se encontraba vigente
        		if (!bySocket && !errorConnectingServer) {
        			LOGGER.info("Enviamos el resultado de la operacion de seleccion de certificado al servidor intermedio"); //$NON-NLS-1$
        			sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
        		}

        		return msg;
        	} catch (final ParameterException e) {
        		LOGGER.log(Level.SEVERE, "Error en los parametros de seleccion de certificados: " + e, e); //$NON-NLS-1$
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
        		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
        	} catch (final Exception e) {
        		LOGGER.log(Level.SEVERE, "Error en los parametros de seleccion de certificados: " + e, e); //$NON-NLS-1$
        		final ErrorCode errorCode = SimpleErrorCode.Internal.UNKNOWN_SELECTING_CERT_ERROR;
        		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
        		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
        	}
        }
		if (urlString.startsWith("afirma://save?") || urlString.startsWith("afirma://save/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para el guardado de datos"); //$NON-NLS-1$

            try {
                UrlParametersToSave params =
                		ProtocolInvocationUriParserUtil.getParametersToSave(urlParams, !bySocket);

                LOGGER.info("Cantidad de datos a guardar: " + (params.getData() == null ? 0 : params.getData().length)); //$NON-NLS-1$

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                	try {
                		final byte[] xmlData;
                		try {
                			xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                		} catch (final DecryptionException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al descifrar los datos obtenidos", e); //$NON-NLS-1$
                		} catch (final SocketTimeoutException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Se excedio el tiempo de espera de la llamada al servico de recuperacion del servidor intermedio cuando se trataron de obtener los datos para el guardado de datos", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_SAVE_TIMEOUT);
                		} catch (final IOException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al recuperar los datos enviados por el cliente a traves del servidor intermedio", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_SAVE_OPERATION);
                		}

                		params = ProtocolInvocationUriParser.getParametersToSave(xmlData, true);
                	} catch (final IntermediateServerErrorSendedException e) {
                		LOGGER.log(Level.SEVERE, "Se obtuvo un error al descargar los datos del servidor intermedio", e); //$NON-NLS-1$
                		processIntermediateServiceError(e.getMessage(), e.getErrorCode(), params.getStorageServletUrl(), params.getId());
                		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                	}
                }

        		// Si la peticion no se hizo a traves de socket/websocket, la version de protocolo se indica en la propia operacion
        		if (requestedProtocolVersion == null) {
        			requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
        			reviewProtocolVersion(requestedProtocolVersion, jvc);
        		}

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

                LOGGER.info("Se inicia la operacion de guardado. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

                String msg;
                try {
                	msg = ProtocolInvocationLauncherSave.processSave(params, requestedProtocolVersion);
                }
                catch(final AOCancelledOperationException e) {
                    LOGGER.severe("Operacion de guardado de datos cancelada por el usuario"); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }
				// solo entra en la excepcion en el caso de que haya que devolver errores a
				// traves del servidor intermedio
                catch (final SocketOperationException e) {
                	LOGGER.log(Level.SEVERE, "Error en la operacion de guardado: " + e, e); //$NON-NLS-1$
                	msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }

                // Si no es por sockets, se devuelve el resultado al servidor y detenemos la
                // espera activa si se encontraba vigente
                if (!bySocket) {
                	LOGGER.info("Enviamos el resultado de la operacion de guardado al servidor intermedio"); //$NON-NLS-1$
                	sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                }

                return msg;

			} catch (final NeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterException e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de guardado", e); //$NON-NLS-1$
            	ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final Exception e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de guardado", e); //$NON-NLS-1$
				final ErrorCode errorCode = SimpleErrorCode.Internal.UNKNOWN_SAVING_DATA_ERROR;
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
            }
        }
		if (urlString.startsWith("afirma://signandsave?") || urlString.startsWith("afirma://signandsave/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para la firma/multifirma y el guardado del resultado"); //$NON-NLS-1$

            try {
                UrlParametersToSignAndSave params =
                		ProtocolInvocationUriParserUtil.getParametersToSignAndSave(urlParams, !bySocket);

				LOGGER.info("Cantidad de datos a firmar y guardar: " //$NON-NLS-1$
						+ (params.getData() == null ? 0 : params.getData().length));

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                	try {
                		final byte[] xmlData;
                		try {
                			xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                		} catch (final DecryptionException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al descifrar los datos obtenidos", e); //$NON-NLS-1$
                		} catch (final SocketTimeoutException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Se excedio el tiempo de espera de la llamada al servico de recuperacion del servidor intermedio cuando se trataron de obtener los datos para la firma y guardado", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_SIGN_AND_SAVE_TIMEOUT);
                		} catch (final IOException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al recuperar los datos enviados por el cliente a traves del servidor intermedio", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_SIGN_AND_SAVE_OPERATION);
                		}

                		params = ProtocolInvocationUriParser.getParametersToSignAndSave(xmlData, true);
                	} catch (final IntermediateServerErrorSendedException e) {
                		LOGGER.log(Level.SEVERE, "Se obtuvo un error al descargar los datos del servidor intermedio", e); //$NON-NLS-1$
                		processIntermediateServiceError(e.getMessage(), e.getErrorCode(), params.getStorageServletUrl(), params.getId());
                		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                	}
                }

        		// Si la peticion no se hizo a traves de socket/websocket, la version de protocolo se indica en la propia operacion
        		if (requestedProtocolVersion == null) {
        			requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
        			reviewProtocolVersion(requestedProtocolVersion, jvc);
        		}

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

				LOGGER.info("Se inicia la operacion de firma y guardado. Version de protocolo: " //$NON-NLS-1$
						+ requestedProtocolVersion);

				String msg;
                try {
                	final StringBuilder dataToSend =  ProtocolInvocationLauncherSignAndSave.processSign(params, requestedProtocolVersion);
                	msg = dataToSend.toString();
                }
                catch(final AOCancelledOperationException e) {
                    LOGGER.severe("Operacion de firma y guardado cancelada por el usuario"); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }
				// solo entra en la excepcion en el caso de que haya que devolver errores a
				// traves del servidor intermedio
                catch(final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de firma y guardado: " + e); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }

                // Si no es por sockets, se devuelve el resultado al servidor y detenemos la
                // espera activa si se encontraba vigente
                if (!bySocket) {
                	LOGGER.info("Enviamos el resultado de la operacion de firma y guardado al servidor intermedio"); //$NON-NLS-1$
                	sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                }

                return msg;
			} catch (final NeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterException e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma y guardado: " + e, e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final Exception e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma y guardado: " + e, e); //$NON-NLS-1$
                final ErrorCode errorCode = ErrorCode.Internal.UNKNOWN_SIGNING_ERROR;
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
            }
        }
		if (urlString.startsWith("afirma://sign?")        || urlString.startsWith("afirma://sign/?") || //$NON-NLS-1$ //$NON-NLS-2$
                 urlString.startsWith("afirma://cosign?")      || urlString.startsWith("afirma://cosign/?") || //$NON-NLS-1$ //$NON-NLS-2$
                 urlString.startsWith("afirma://countersign?") || urlString.startsWith("afirma://countersign/?") //$NON-NLS-1$ //$NON-NLS-2$
        ) {
            LOGGER.info("Se invoca a la aplicacion para realizar una operacion de firma/multifirma"); //$NON-NLS-1$

            try {
                UrlParametersToSign params =
                		ProtocolInvocationUriParserUtil.getParametersToSign(urlParams, !bySocket);

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                	try {
                		LOGGER.info("Se descargan los datos del servidor intermedio"); //$NON-NLS-1$

                		final byte[] xmlData;
                		try {
                			xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                		} catch (final DecryptionException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al descifrar los datos obtenidos", e); //$NON-NLS-1$
                		} catch (final SocketTimeoutException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Se excedio el tiempo de espera de la llamada al servico de recuperacion del servidor intermedio cuando se trataron de obtener los datos para la firma", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_SIGN_TIMEOUT);
                		} catch (final IOException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al recuperar los datos enviados por el cliente a traves del servidor intermedio", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_SIGN_OPERATION);
                		}

                		LOGGER.info("Fin de la descarga de los datos. Se carga la configuracion de firma de la peticion"); //$NON-NLS-1$

                		params = ProtocolInvocationUriParser.getParametersToSign(xmlData, true);
                	} catch (final IntermediateServerErrorSendedException e) {
                		LOGGER.log(Level.SEVERE, "Se obtuvo un error al descargar los datos del servidor intermedio", e); //$NON-NLS-1$
                		processIntermediateServiceError(e.getMessage(), e.getErrorCode(), params.getStorageServletUrl(), params.getId());
                		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                	}
                }

        		// Si la peticion no se hizo a traves de socket/websocket, la version de protocolo se indica en la propia operacion
        		if (requestedProtocolVersion == null) {
        			requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
        			reviewProtocolVersion(requestedProtocolVersion, jvc);
        		}

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

                LOGGER.info("Se inicia la operacion de firma. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

                String msg;
                try {
                	final StringBuilder dataToSend = ProtocolInvocationLauncherSign.processSign(params, requestedProtocolVersion, null);
                	msg = dataToSend.toString();
                }
                catch(final AOCancelledOperationException e) {
                    LOGGER.severe("Operacion de firma cancelada por el usuario"); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }
				// solo entra en la excepcion en el caso de que haya que devolver errores a
				// traves del servidor intermedio
                catch(final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de firma: " + e); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }

                // Si no es por sockets, se devuelve el resultado al servidor y detenemos la
                // espera activa si se encontraba vigente
                if (!bySocket) {
                	LOGGER.info("Enviamos el resultado de la operacion de firma al servidor intermedio"); //$NON-NLS-1$
                	sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                }

                return msg;
            } catch (final NeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterException e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de firma", e); //$NON-NLS-1$
            	ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final Exception e) {
            	LOGGER.log(Level.SEVERE, "Error en los parametros de firma", e); //$NON-NLS-1$
            	final ErrorCode errorCode = ErrorCode.Internal.UNKNOWN_SIGNING_ERROR;
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
            }
        }
		if (urlString.startsWith("afirma://load?") || urlString.startsWith("afirma://load/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para realizar una operacion de carga de uno o varios ficheros"); //$NON-NLS-1$

            try {
                UrlParametersToLoad params =
                		ProtocolInvocationUriParserUtil.getParametersToLoad(urlParams);

				// Si se indica un identificador de fichero, es que la configuracion de la
				// operacion
                // se tiene que descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                	try {
                		final byte[] xmlData;
                		try {
                			xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                		} catch (final DecryptionException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al descifrar los datos obtenidos", e); //$NON-NLS-1$
                		} catch (final SocketTimeoutException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Se excedio el tiempo de espera de la llamada al servico de recuperacion del servidor intermedio cuando se trataron de obtener los datos para la carga de fichero", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_LOAD_TIMEOUT);
                		} catch (final IOException e) {
                			throw new IntermediateServerErrorSendedException(
                					"Error al recuperar los datos enviados por el cliente a traves del servidor intermedio", e, //$NON-NLS-1$
                					SimpleErrorCode.Communication.RECIVING_DATA_OF_LOAD_OPERATION);
                		}

                		params = ProtocolInvocationUriParser.getParametersToLoad(xmlData);
                	} catch (final IntermediateServerErrorSendedException e) {
                		LOGGER.log(Level.SEVERE, "Se obtuvo un error al descargar los datos del servidor intermedio", e); //$NON-NLS-1$
                		processIntermediateServiceError(e.getMessage(), e.getErrorCode(), params.getStorageServletUrl(), params.getId());
                		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                	}
                }

        		// Si la peticion no se hizo a traves de socket/websocket, la version de protocolo se indica en la propia operacion
        		if (requestedProtocolVersion == null) {
        			requestedProtocolVersion = parseProtocolVersion(params.getMinimumProtocolVersion());
        			reviewProtocolVersion(requestedProtocolVersion, jvc);
        		}

                // En caso de comunicacion por servidor intermedio, solicitamos, si corresponde,
                // que se espere activamente hasta el fin de la tarea
                if (!bySocket && params.isActiveWaiting()) {
                	requestWait(params.getStorageServletUrl(), params.getId());
                }

                LOGGER.info("Se inicia la operacion de carga. Version de protocolo: " + requestedProtocolVersion); //$NON-NLS-1$

                String msg;
                try {
                    msg = ProtocolInvocationLauncherLoad.processLoad(params, requestedProtocolVersion);
                }
                catch(final AOCancelledOperationException e) {
                    LOGGER.severe("Operacion de carga de datos cancelada por el usuario"); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }
				// solo entra en la excepcion en el caso de que haya que devolver errores a
				// traves del servidor intermedio
                catch(final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de carga de fichero: " + e); //$NON-NLS-1$
                    msg = ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
                }

                // Si no es por sockets, se devuelve el resultado al servidor y detenemos la
                // espera activa si se encontraba vigente
                if (!bySocket) {
                	LOGGER.info("Enviamos el resultado de la operacion de carga de fichero al servidor intermedio"); //$NON-NLS-1$
                	sendDataToServer(msg, params.getStorageServletUrl().toString(), params.getId());
                }

                return msg;
			} catch (final NeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de Autofirma para procesar la peticion: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final ParameterException e) {
                LOGGER.severe("Error en los parametros de carga: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, e);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, e.getErrorCode());
			} catch (final Exception e) {
                LOGGER.severe("Error desconocido en la operacion de carga: " + e); //$NON-NLS-1$
                final ErrorCode errorCode = SimpleErrorCode.Internal.UNKNOWN_LOADING_DATA_ERROR;
                ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
            }
        }

		LOGGER.severe("La operacion indicada en la URL no esta soportada: " + //$NON-NLS-1$
    				urlString.substring(0, Math.min(30, urlString.length())) + "..." //$NON-NLS-1$
		);

		final ErrorCode errorCode = SimpleErrorCode.Request.UNSUPPORTED_OPERATION;
		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
		return ProtocolInvocationLauncherErrorManager.getErrorMessage(requestedProtocolVersion, errorCode);
    }

    /**
     * Envia un error al servidor intermedio o, si no proporcionan los datos necesarios, se muestra al usuario.
     * @param msg Mensaje de error que enviar.
     * @param errorCode C&oacute;digo de error.
     * @param storageServerUrl URL del servicio de guardado del servidor intermedio.
     * @param id Identificador para el guardado en el servidor intermedio.
     */
    public static void processIntermediateServiceError(final String msg, final ErrorCode errorCode, final URL storageServerUrl, final String id) {
    	if (storageServerUrl != null && id != null) {
    		sendDataToServer(msg, storageServerUrl.toString(), id);
    	} else {
    		ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, errorCode);
    	}
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
				SimpleAfirma.getSSLContextConfigurationTask().join();
			} catch (final InterruptedException e) {
				LOGGER.warning("No se ha podido configurar correctamente el contexto SSL: " + e); //$NON-NLS-1$
			}
			try {
				IntermediateServerUtil.sendData(data, serviceUrl, id);
			} catch (final SocketTimeoutException e) {
				LOGGER.log(Level.SEVERE, "Se excedio el tiempo de espera maximo en la llamada al servicio de guardado del servidor intermedio", e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, SimpleErrorCode.Communication.SENDING_RESULT_TIMEOUT);
			}
			catch (final InvalidDomainSSLCertificateException e) {
				LOGGER.log(Level.SEVERE, "El certificado SSL no esta expedido para el dominio al que pertenece el servidor: " + e, e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, ErrorCode.Communication.INVALID_DOMAIN_SSL_CERTIFICATE_ERROR, e.getHost());
			}catch (final Exception e) {
				LOGGER.log(Level.SEVERE, "Error al enviar los datos al servidor intermedio: " + e, e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(requestedProtocolVersion, SimpleErrorCode.Communication.SENDING_RESULT_OPERATION);
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
	private static ProtocolVersion parseProtocolVersion(final String version) {

		ProtocolVersion protocolVersion;
		if (version != null) {
			try {
				protocolVersion = ProtocolVersion.getInstance(version);
			} catch (final Exception e) {
				LOGGER.warning("Se ha proporcionado en la llamada un identificador de version del protocolo con formato no soportado: " + e); //$NON-NLS-1$
				protocolVersion = ProtocolVersion.getInstance(ProtocolVersion.VERSION_0);
			}
		}
		else {
			protocolVersion = ProtocolVersion.getInstance(ProtocolVersion.VERSION_0);
		}
    	return protocolVersion;
	}

	/**
	 * Obtiene el valor asignado al par&aacute;metro de versi&oacute;n de una URL.
	 * @param params Par&acute;metros declarados en una URL.
	 * @return Valor del par&aacute;metro de versi&oacute;n ('v') o el valor '1' si no est&aacute; definido.
	 */
	private static ProtocolVersion getVersion(final Map<String, String> params) {

		// Si se encuentra el parametro con la version, se devuelve. Si no, se devuelve
		// 1.
		ProtocolVersion protocolVersion;
		final String protocolId = params.get(PROTOCOL_VERSION_PARAM);
		if (protocolId != null) {
			try {
				protocolVersion = ProtocolVersion.getInstance(protocolId.trim());
			} catch (final Exception e) {
				LOGGER.log(Level.WARNING, "El ID de protocolo indicado no es valido (" //$NON-NLS-1$
						+ LoggerUtil.getTrimStr(protocolId) + ")", e); //$NON-NLS-1$
				protocolVersion = ProtocolVersion.getInstance(ProtocolVersion.VERSION_0);
			}
		}
		else {
			protocolVersion = ProtocolVersion.getInstance(ProtocolVersion.VERSION_0);
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

    /**
     * Comprueba si la aplicaci&oacute;n es compatible con la versi&oacute;n de protocolo
     * solicitada.
     * @param protocolVersion Versi&oacute;n de protocolo.
     * @return {@code true} si se es compatible con la versi&oacute;n indicada, {@code false}
     * en caso contrario.
     */
	public static boolean isCompatibleWith(final ProtocolVersion protocolVersion) {
		for (final ProtocolVersion supportedVersion : SUPPORTED_PROTOCOLS) {
			if (supportedVersion.isCompatibleWith(protocolVersion)) {
				return true;
			}
		}
		return false;
	}


	/**
	 * Inicia en segundo plano la tarea para cargar del almac&eacute;n de claves por defecto
	 * si no se hab&iacute;a hecho antes. El almac&eacute;n cargado puede ser el del sistema
	 * o el configurado en Autofirma para la invocaci&oacute;n por protocolo, siempre que
	 * no sea un almac&eacute;n en fichero o tarjeta.
	 */
	public static void initLoadKeyStoreTask() {
		if (loadKeyStoreTask == null) {
			loadKeyStoreTask = new LoadKeystoreTask();
			loadKeyStoreTask.start();
		}
	}

	/**
	 * Devuelve la tarea de carga del almac&eacute;n de claves en segundo plano.
	 * @return Tarea de carga del almac&eacute;n o {@code null} si no se defini&oacute;.
	 */
	public static LoadKeystoreTask getLoadKeyStoreTask() {
		return loadKeyStoreTask;
	}
}
