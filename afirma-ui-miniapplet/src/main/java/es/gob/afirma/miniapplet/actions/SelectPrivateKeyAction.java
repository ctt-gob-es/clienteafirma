package es.gob.afirma.miniapplet.actions;

import java.awt.Component;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivilegedExceptionAction;
import java.util.List;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform.BROWSER;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.keystores.common.AOKeyStore;
import es.gob.afirma.keystores.common.AOKeyStoreManager;
import es.gob.afirma.keystores.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.common.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.common.KeyStoreUtilities;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.miniapplet.CertFilterManager;

/**
 * Acci&oacute;n privilegiada para la selecci&oacute;n de una clave privada
 * de firma por el usuario.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class SelectPrivateKeyAction implements PrivilegedExceptionAction<PrivateKeyEntry> {

	private final AOKeyStore keyStore;
	private final Component parent;
	private final CertFilterManager filterManager;
	
	/**
	 * Crea la acci&oacute;n para la seleccion de la clave privada de un certificado.
	 * @param os Sistema operativo actual.
	 * @param browser Navegador web actual.
	 * @param filterManager Manejador de filtros de certificados.
	 * @param parent Componente padre sobre el que se montanlos di&aacute;logos que se
	 * visualizan como parte de la acci&oacute;n.
	 */
	public SelectPrivateKeyAction(final OS os, 
	                              final BROWSER browser, 
	                              final CertFilterManager filterManager, 
	                              final Component parent) {
		if (os == OS.WINDOWS) {
			if (browser == BROWSER.FIREFOX) {
				this.keyStore = AOKeyStore.MOZ_UNI;
			} 
			else {
				this.keyStore = AOKeyStore.WINDOWS;
			}
		} 
		else if (os == OS.LINUX || os == OS.SOLARIS) {
			this.keyStore = AOKeyStore.MOZ_UNI;
		} 
		else if (os == OS.MACOSX) {
			this.keyStore = AOKeyStore.APPLE;
		} 
		else {
			this.keyStore = AOKeyStore.PKCS12; 	
		}
		this.filterManager = filterManager;
		this.parent = parent;
	}
	
	public PrivateKeyEntry run() throws KeyException, AOKeystoreAlternativeException, AOException {
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			this.keyStore, 
			null, 
			null,
			KeyStoreUtilities.getPreferredPCB(this.keyStore, this.parent), 
			this.parent
		);
		
		boolean mandatoryCertificate = false;
		List<CertificateFilter> filters = null;
		if (this.filterManager != null) {
			filters = this.filterManager.getFilters();
			mandatoryCertificate = this.filterManager.isMandatoryCertificate();
		}
		
		final String selectedAlias = KeyStoreUtilities.showCertSelectionDialog(
			ksm.getAliases(),
			ksm.getKeyStores(),
			this,
			true,
			true,
			true,
			filters,
			mandatoryCertificate
		);
    	
    	return ksm.getKeyEntry(selectedAlias,
    			KeyStoreUtilities.getCertificatePC(this.keyStore, this.parent));
	}

	
}
