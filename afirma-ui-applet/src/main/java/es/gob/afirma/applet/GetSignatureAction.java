package es.gob.afirma.applet;

import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URI;
import java.security.PrivilegedExceptionAction;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.ui.AOUIFactory;

/**
 * Recupera la firma generada o seleccionada por el usuario. Si no hay una firma generada
 * puede mostrar un di&aacute;logo de selecci&oacute;n para que el usuario seleccione
 * la firma que desee. Si no se solicitase mostrar este di&aacute;logo, se devolver&iacute;a
 * {@code null}.
 */
class GetSignatureAction implements PrivilegedExceptionAction<byte[]> {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final byte[] signData;
	private URI electronicSignatureFile;
	private boolean selectSignature;
	private String signFormat;
	private Component parent;

	/**
	 * Crea la acci&oacute;n para recuperar la firma.
	 * @param signData Datos de firma establecidos.
	 * @param electronicSignatureFile Fichero de firma establecido.
	 */
	GetSignatureAction(final byte[] signData, final URI electronicSignatureFile) {
		this.signData = signData == null ? null : signData.clone();
		this.electronicSignatureFile = electronicSignatureFile;
		this.selectSignature = false;

	}

	/**
	 * Establece si debe mostrarse o no un di&aacute;logo de selecci&oacute;n para la
	 * elecci&oacute;n de la firma si no hay ninguna generada o ya seleccionada.
	 * Si no se utiliza este m&eacute;todo no se mostrar&aacute; ning&uacute;n di&aacute;logo
	 * de selecci&oacute;n.
	 * @param select Permite que el usuario seleccione una firma si no hab&iacute;a
	 * 				 ninguna seleccionada o generada.
	 * @param format Formato de la firma que se dese cargar (s&oacute;lo cuando se
	 * 				 desea cargar desde un fichero).
	 * @param parent Componente sobre el que se muestra el di&aacute;logo de selecci&oacute;n.
	 * 				(s&oacute;lo cuando se desea cargar desde un fichero)
	 */
	void setSelectFile(final boolean select, final String format, final Component parent) {
		this.selectSignature = select;
		this.signFormat = format;
		this.parent = parent;
	}

	/** {@inheritDoc} */
	@Override
	public byte[] run() throws AOException  {
		final byte[] originalSign;
        if (this.signData != null) {
            originalSign = this.signData;
        }
        else if (this.electronicSignatureFile == null && !this.selectSignature) {
            originalSign = null;
        }
        else {
            if (this.electronicSignatureFile == null) {
                final String fileName = selectSignFile(this.signFormat, this.parent);
                try {
                	this.electronicSignatureFile = AOUtil.createURI(fileName);
                }
                catch (final Exception e) {
                	LOGGER.severe("La URI proporcionada no es valida (" + fileName + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    throw new AOException("El nombre de fichero '" + fileName + "' no es valido ", e); //$NON-NLS-1$ //$NON-NLS-2$
                }
            } // Fin 'else': Si no habia fichero seleccionado

            // Cargamos el fichero que estaba seleccionado o recien elegido por
            // el usuario
            try {
                final InputStream is = AOUtil.loadFile(this.electronicSignatureFile);
                originalSign = AOUtil.getDataFromInputStream(is);
                is.close();
            }
            catch (final FileNotFoundException e) {
            	LOGGER.severe("No se encuentra el fichero de firma '" + this.electronicSignatureFile.getPath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                throw new AOException("No se encuentra el fichero de firma '" + this.electronicSignatureFile.getPath() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
            }
            catch (final Exception e) {
            	LOGGER.severe("Error tratando de leer el fichero de firma original (" + this.electronicSignatureFile.getPath() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                throw new AOException("Error tratando de leer el fichero de firma '" + this.electronicSignatureFile.getPath() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }

        return originalSign;
	}

	/**
	 * Recupera el fichero de firma seleccionado. Esto s&oacute;lo tiene utilidad cuando se
	 * ha solicitado que se muestre el di&aacute;logo de selecci&oacute;n de fichero de firma.
	 * @return Ruta del fichero seleccionado.
	 * @see #setSelectFile(boolean, String, Component)
	 */
	URI getSelectedSignatureFile() {
		return this.electronicSignatureFile;
	}

	/** Muestra un di&aacute;logo para la selecci&oacute;n de un fichero de
     * firma. En caso de que se indique un formato, se usar&aacute;n filtros para las
     * extensiones predeterminadas para el formato de firma concreto. Si no se
     * selecciona ningun fichero, se devolver&aacute; {@code null}.
     * @param signFormat
     *        Formato de la firma que se desea seleccionar.
     * @param parent
     * 		  Componente padre sobre el que se muestra el di&aacute;logo de selecci&oacute;n.
     * @return Fichero de firma. */
	private static String selectSignFile(final String signFormat, final Component parent) {

        String[] exts = null;
        String desc = null;

        if (signFormat != null) {
            if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CMS)) {
                exts = new String[] {
                        "csig", "p7s", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.29"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
                exts = new String[] {
                        "csig", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$
                desc = AppletMessages.getString("SignApplet.26"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED) || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
                     || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
                     || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
                     || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)
                     || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
                exts = new String[] {
                        "xsig", "sig", "xml"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.27"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_PKCS1)) {
                exts = new String[] {
                    "sig"}; //$NON-NLS-1$
                desc = AppletMessages.getString("SignApplet.318"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_PDF)) {
                exts = new String[] {
                    "pdf"}; //$NON-NLS-1$
                desc = AppletMessages.getString("SignApplet.28"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_ODF)) {
                exts = new String[] {
                        "odt", "ods", "odp"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.32"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_OOXML)) {
                exts = new String[] {
                        "docx", "xlsx", "pptx"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.30"); //$NON-NLS-1$
            }
        }

        return AOUIFactory.getLoadFiles(
    		AppletMessages.getString("SignApplet.163"), //$NON-NLS-1$
    		null,
    		null,
            exts,
            desc,
            false,
            false,
            null,
            parent
        )[0].getAbsolutePath();
    }
}
