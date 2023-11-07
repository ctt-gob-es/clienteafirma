/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedExceptionAction;
import java.security.UnrecoverableEntryException;
import java.util.List;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Platform.BROWSER;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.filters.CertFilterManager;


/** Acci&oacute;n privilegiada para la selecci&oacute;n de una clave privada
 * de firma por el usuario.
 * @author Carlos Gamuci Mill&aacute;n. */
final class SelectPrivateKeyAction implements PrivilegedExceptionAction<PrivateKeyEntry> {

	private final AOKeyStore keyStore;
	private final Component parent;
	private final CertFilterManager filterManager;

	private String library = null;

	/** Crea la acci&oacute;n para la selecci&oacute;n de la clave privada de un certificado.
     * @param type Tipo de almac&eacute;n de certificados y claves privadas a usar.
     * @param lib Fichero asociado al almac&aacute;n (biblioteca din&aacute;mica en el caso de PKCS#11,
     *            fichero PFX en el caso de PKCS#12, archivo de llavero en el caso de llavero de
     *            Mac OS X, etc.
     * @param filterManager Manejador de filtros de certificados.
     * @param parent Componente padre para los di&aacute;logos que se
     * visualizan como parte de la acci&oacute;n. */
	SelectPrivateKeyAction(final AOKeyStore type,
	                       final String lib,
	                       final CertFilterManager filterManager,
                           final Component parent) {
	    if (type == null) {
	        throw new IllegalArgumentException("El tipo de almacen no puede ser nulo"); //$NON-NLS-1$
	    }
	    this.keyStore = type;
	    this.filterManager = filterManager;
	    this.parent = parent;
	    this.library = lib;
	}

	/** Crea la acci&oacute;n para la selecci&oacute;n de la clave privada de un certificado.
	 * @param os Sistema operativo actual.
	 * @param browser Navegador web actual.
	 * @param filterManager Manejador de filtros de certificados.
	 * @param parent Componente padre para los di&aacute;logos que se
	 * visualizan como parte de la acci&oacute;n. */
	SelectPrivateKeyAction(final OS os,
	                       final BROWSER browser,
	                       final CertFilterManager filterManager,
	                       final Component parent) {
        if (browser == BROWSER.FIREFOX) {
            this.keyStore = AOKeyStore.MOZ_UNI;
        }
        else if (os == OS.WINDOWS) {
			this.keyStore = AOKeyStore.WINDOWS;
		}
		else if (os == OS.LINUX) {
			this.keyStore = AOKeyStore.SHARED_NSS;
		}
		else if (os == OS.SOLARIS) {
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

	/** {@inheritDoc} */
	@Override
	public PrivateKeyEntry run() throws AOKeystoreAlternativeException,
	                                    AOException,
	                                    UnrecoverableEntryException,
	                                    NoSuchAlgorithmException,
	                                    KeyStoreException,
	                                    IOException {
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			this.keyStore,
			this.library,
			null,
			this.keyStore.getStorePasswordCallback(this.parent),
			this.parent
		);

		boolean mandatoryCertificate = false;
		List<CertificateFilter> filters = null;
		if (this.filterManager != null) {
			filters = this.filterManager.getFilters();
			mandatoryCertificate = this.filterManager.isMandatoryCertificate();
		}
		String libName = null;
		if (this.library != null) {
			final File file = new File(this.library);
			libName = file.getName();
		}
		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
			ksm,
			this,
			true,
			true, // showExpiredCertificates
			true, // checkValidity
			filters,
			mandatoryCertificate,
			libName
		);
		dialog.allowOpenExternalStores(
			this.filterManager.isExternalStoresOpeningAllowed()
		);
		dialog.show();

		ksm.setParentComponent(this.parent);
		return ksm.getKeyEntry(dialog.getSelectedAlias());
	}


}
