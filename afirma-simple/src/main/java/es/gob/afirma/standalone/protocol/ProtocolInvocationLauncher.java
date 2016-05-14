package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.ParameterLocalAccessRequestedException;
import es.gob.afirma.core.misc.protocol.ParameterNeedsUpdatedVersionException;
import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParser;
import es.gob.afirma.core.misc.protocol.UrlParametersForBatch;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.misc.protocol.UrlParametersToSelectCert;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.core.misc.protocol.UrlParametersToSignAndSave;
import es.gob.afirma.standalone.protocol.ProtocolInvocationLauncherUtil.DecryptionException;
import es.gob.afirma.standalone.protocol.ProtocolInvocationLauncherUtil.InvalidEncryptedDataLengthException;
import es.gob.afirma.standalone.ui.MainMenu;

/** Gestiona la ejecuci&oacute;n del Cliente Afirma en una invocaci&oacute;n
 * por protocolo y bajo un entorno compatible <code>Swing</code>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ProtocolInvocationLauncher {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String RESULT_OK = "OK"; //$NON-NLS-1$
    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private ProtocolInvocationLauncher(){
        // No instanciable
    }

    /** Lanza la aplicaci&oacute;n y realiza las acciones indicadas en la URL.
     * Este m&eacute;todo usa siempre comunicaci&oacute;n mediante servidor intermedio, nunca localmente.
     * @param urlString URL de invocaci&oacute;n por protocolo.
     * @return Resultado de la operaci&oacute;n. */
    public static String launch(final String urlString)  {
        return launch(urlString, false);
    }

    /** Lanza la aplicaci&oacute;n y realiza las acciones indicadas en la URL.
     * @param urlString URL de invocaci&oacute;n por protocolo.
     * @param bySocket Si se establece a <code>true</code> se usa una comuicaci&oacute;n de vuelta mediante conexi&oacute;n
     *                 HTTP local (a <code>localhost</code>), si se establece a <code>false</code> se usa un servidor intermedio
     *                 para esta comunicaci&oacute;n de vuelta.
     * @return Resultado de la operaci&oacute;n. */
    public static String launch(final String urlString, final boolean bySocket)  {
        // En OS X sobrecargamos el "Acerca de..." del sistema operativo, que tambien
        // aparece en la invocacion por protocolo
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
        	try {
	            com.apple.eawt.Application.getApplication().setAboutHandler(
	                 ae -> MainMenu.showAbout(null)
	            );
        	}
        	catch(final Exception | Error e) {
        		LOGGER.warning("No ha sido posible establecer el menu 'Acerca de...' de OS X: " + e); //$NON-NLS-1$
        	}
        }

        if (urlString == null) {
            LOGGER.severe("No se ha proporcionado una URL para la invocacion"); //$NON-NLS-1$
            ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_01);
            return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_01);
        }
        if (!urlString.startsWith("afirma://")) { //$NON-NLS-1$
            LOGGER.severe("La URL de invocacion no comienza por 'afirma://'"); //$NON-NLS-1$
            ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_02);
            return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_02);
        }

        // Se invoca la aplicacion para iniciar la comunicacion por socket
        if (urlString.startsWith("afirma://service?") || urlString.startsWith("afirma://service/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se inicia la invocacion por servicio: " + urlString); //$NON-NLS-1$
            try {
            	ServiceInvocationManager.startService(urlString);
            }
            catch(final UnsupportedProtocolException e) {
            	LOGGER.severe("La version del protocolo no esta soportada (" + e.getVersion() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
            	final String errorCode = e.isNewVersionNeeded() ?
            			ProtocolInvocationLauncherErrorManager.SAF_21 : ProtocolInvocationLauncherErrorManager.SAF_22;
            	ProtocolInvocationLauncherErrorManager.showError(errorCode);
            	return ProtocolInvocationLauncherErrorManager.getErrorMessage(errorCode);
            }

            return RESULT_OK;
        }
        // Se solicita una operacion de firma batch
        else if (urlString.startsWith("afirma://batch?") || urlString.startsWith("afirma://batch/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            try {

                UrlParametersForBatch params = ProtocolInvocationUriParser.getParametersForBatch(urlString);

                // Si se indica un identificador de fichero, es que el XML de definicion de lote se tiene que
                // descargar desde el servidor intermedio
                if (params.getFileId() != null) {
                    final byte[] xmlBatchDefinition;
                    try {
                        xmlBatchDefinition = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                    }
                    catch(final InvalidEncryptedDataLengthException e) {
                        LOGGER.severe("No se pueden recuperar los datos del servidor: " + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_16);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_16);
                    }
                    catch(final DecryptionException e) {
                        LOGGER.severe("Error al descifrar los datos obtenidos: " + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_15);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_15);
                    }

                    params = ProtocolInvocationUriParser.getParametersForBatch(xmlBatchDefinition);
                }
                try {
                    return  ProtocolInvocationLauncherBatch.processBatch(params, bySocket);
                }
                catch(final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de firma por lotes: " + e); //$NON-NLS-1$
                    if (e.getErrorCode() == ProtocolInvocationLauncherBatch.getResultCancel()){
                        ProtocolInvocationLauncherBatch.sendErrorToServer(e.getErrorCode(), params);
                    }
                    else {
                        ProtocolInvocationLauncherBatch.sendErrorToServer(ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode()), params);
                    }
                }
            }
            catch(final Exception e) {
                LOGGER.log(Level.SEVERE, "Error en los parametros de firma por lote: " + e, e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_03);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_03);
            }
        }
        // Se solicita una operacion de seleccion de certificado
        else if (urlString.startsWith("afirma://selectcert?") || urlString.startsWith("afirma://selectcert/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            try {
                final UrlParametersToSelectCert params = ProtocolInvocationUriParser.getParametersToSelectCert(urlString);
                try {
                    return ProtocolInvocationLauncherSelectCert.processSelectCert(params);
                }
                catch (final AOCancelledOperationException e) {
                	return ProtocolInvocationLauncherSelectCert.getResultCancel();
                }
                catch (final SocketOperationException e) {
                    LOGGER.severe("Error durante la operacion de seleccion de certificados: " + e); //$NON-NLS-1$
                   	return ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode());
                }
            }
            catch (final Exception e) {
                LOGGER.severe("Error en los parametros de seleccion de certificados: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_03);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_03);
            }
        }
        // Se solicita una operacion de guardado
        else if (urlString.startsWith("afirma://save?") || urlString.startsWith("afirma://save/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para el guardado de datos"); //$NON-NLS-1$

            try {
                UrlParametersToSave params = ProtocolInvocationUriParser.getParametersToSave(urlString);
                LOGGER.info("Parametros de la llamada = " + urlString); //$NON-NLS-1$

                // Si se indica un identificador de fichero, es que la configuracion se tiene que
                // descargar desde el servidor intermedio
                if (params.getFileId() != null) {

                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                    }
                    catch(final InvalidEncryptedDataLengthException e) {
                        LOGGER.severe("No se pueden recuperar los datos del servidor: " + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_16);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_16);
                    }
                    catch(final DecryptionException e) {
                        LOGGER.severe("Error al descifrar: " + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_15);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_15);
                    }

                    params = ProtocolInvocationUriParser.getParametersToSave(xmlData);
                }
                return  ProtocolInvocationLauncherSave.processSave(params);
            }
            catch(final ParameterNeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de AutoFirma para procesar la peticion: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_14);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_14);
            }
            catch(final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_13);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_13);
            }
            catch (final Exception e) {
                LOGGER.severe("Error en los parametros de guardado: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_03);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_03);
            }
        }

        // Se solicita una operacion de firma/multifirma seguida del guardado del resultado
        else if (urlString.startsWith("afirma://signandsave?") || urlString.startsWith("afirma://signandsave/?")) { //$NON-NLS-1$ //$NON-NLS-2$
            LOGGER.info("Se invoca a la aplicacion para la firma/multifirma y el guardado del resultado"); //$NON-NLS-1$

            try {
                UrlParametersToSignAndSave params = ProtocolInvocationUriParser.getParametersToSignAndSave(urlString);
                LOGGER.info("Parametros de la llamada = " + urlString); //$NON-NLS-1$

                // Si se indica un identificador de fichero, es que la configuracion se tiene que
                // descargar desde el servidor intermedio
                if (params.getFileId() != null) {

                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                    }
                    catch(final InvalidEncryptedDataLengthException e) {
                        LOGGER.severe("No se pueden recuperar los datos del servidor: " + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_16);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_16);
                    }
                    catch(final DecryptionException e) {
                        LOGGER.severe("Error al descifrar: " + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_15);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_15);
                    }

                    params = ProtocolInvocationUriParser.getParametersToSignAndSave(xmlData);
                }
                return  ProtocolInvocationLauncherSignAndSave.process(params, bySocket);
            }
            catch(final ParameterNeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de AutoFirma para procesar la peticion: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_14);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_14);
            }
            catch(final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_13);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_13);
            }
            catch (final Exception e) {
                LOGGER.severe("Error en los parametros de la peticion: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_03);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_03);
            }
        }

        // Se solicita una operacion de firma/cofirma/contrafirma
        else if (urlString.startsWith("afirma://sign?")        || urlString.startsWith("afirma://sign/?") || //$NON-NLS-1$ //$NON-NLS-2$
                 urlString.startsWith("afirma://cosign?")      || urlString.startsWith("afirma://cosign/?") || //$NON-NLS-1$ //$NON-NLS-2$
                 urlString.startsWith("afirma://countersign?") || urlString.startsWith("afirma://countersign/?") //$NON-NLS-1$ //$NON-NLS-2$
        ) {
            LOGGER.info("Se invoca a la aplicacion para realizar una operacion de firma/multifirma"); //$NON-NLS-1$

            try {
                UrlParametersToSign params = ProtocolInvocationUriParser.getParametersToSign(urlString);

                // Si se indica un identificador de fichero, es que la configuracion se tiene que
                // descargar desde el servidor intermedio
                if (params.getFileId() != null) {

                    final byte[] xmlData;
                    try {
                        xmlData = ProtocolInvocationLauncherUtil.getDataFromRetrieveServlet(params);
                    }
                    catch(final InvalidEncryptedDataLengthException e) {
                        LOGGER.severe("No se pueden recuperar los datos del servidor" + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_16);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_16);
                    }
                    catch(final DecryptionException e) {
                        LOGGER.severe("Error al descifrar" + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_15);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_15);
                    }
                    catch (final IOException e) {
                        LOGGER.severe("No se pueden recuperar los datos del servidor" + e); //$NON-NLS-1$
                        ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_16);
                        return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_16);
                    }

                    params = ProtocolInvocationUriParser.getParametersToSign(xmlData);
                }

                LOGGER.info("Se inicia la operacion de firma"); //$NON-NLS-1$

                try {
                    return ProtocolInvocationLauncherSign.processSign(params, bySocket);
                }
                // solo entra en la excepcion en el caso de que haya que devolver errores en el servidor en el envio por servicio web
                catch(final SocketOperationException e){
                    LOGGER.severe("La operacion indicada no esta soportada: " + e); //$NON-NLS-1$
                    if (e.getErrorCode() == ProtocolInvocationLauncherSign.getResultCancel()){
                        ProtocolInvocationLauncherSign.sendErrorToServer(e.getErrorCode(), params);
                    }
                    else {
                        ProtocolInvocationLauncherSign.sendErrorToServer(ProtocolInvocationLauncherErrorManager.getErrorMessage(e.getErrorCode()), params);
                    }

                }

            }
            catch(final ParameterNeedsUpdatedVersionException e) {
                LOGGER.severe("Se necesita una version mas moderna de AutoFirma para procesar la peticion: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_14);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_14);
            }
            catch(final ParameterLocalAccessRequestedException e) {
                LOGGER.severe("Se ha pedido un acceso a una direccion local (localhost o 127.0.0.1): " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_13);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_13);
            }
            catch (final Exception e) {
                LOGGER.severe("Error en los parametros de firma: " + e); //$NON-NLS-1$
                ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_03);
                return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_03);
            }
        }

        else {
        	LOGGER.severe(
        			"La operacion indicada en la URL no esta soportada: " +  //$NON-NLS-1$
        					urlString.substring(0, Math.min(30, urlString.length())) + "..."); //$NON-NLS-1$
            ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_04);
            return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_04);
        }
        throw new IllegalStateException("Estado no permitido"); //$NON-NLS-1$
    }
}
