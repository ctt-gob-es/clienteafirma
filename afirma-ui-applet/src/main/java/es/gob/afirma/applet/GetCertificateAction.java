package es.gob.afirma.applet;

import java.security.cert.X509Certificate;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerException;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;

/**
 * Recupera un certificado de un almac&eacute;n de claves. Se deber&aacute; indicar
 * tanto el alias del certificado como la configuraci&oacute;n con el repositorio
 * activo. 
 */
public class GetCertificateAction extends BasicPrivilegedAction<X509Certificate, Void> {

	private String alias;
	
	private KeyStoreConfigurationManager ksConfigManager;
	
	/**
	 * Construye la accui&oacute;n para la recuperaci&oacute;n del certificado de usuario.
	 * @param alias Alias del certificado.
	 * @param ksConfigManager Configuraci&oacute;n con el repositorio de certificados activo.
	 */
	public GetCertificateAction(final String alias, final KeyStoreConfigurationManager ksConfigManager) {
		this.alias = alias;
		this.ksConfigManager = ksConfigManager;
	}

	public X509Certificate run() {
		try {
            return (X509Certificate) this.ksConfigManager.getCertificate(this.alias);
        }
        catch (final AOCancelledOperationException e) {
            setError(AppletMessages.getString("SignApplet.68"), e); //$NON-NLS-1$
            return null;
        }
        catch (final AOKeyStoreManagerException e) {
            setError(AppletMessages.getString("SignApplet.6"), e); //$NON-NLS-1$
            return null;
        }
        catch (final AOKeystoreAlternativeException e) {
        	setError(AppletMessages.getString("SignApplet.6"), e); //$NON-NLS-1$
        	return null;
        }
	}
}
