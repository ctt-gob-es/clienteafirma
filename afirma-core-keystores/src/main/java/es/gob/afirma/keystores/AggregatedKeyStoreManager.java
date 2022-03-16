/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import es.gob.afirma.core.misc.AOUtil;

/** Gestor de claves consistente a su vez en un agregado de varios gestores, que se tratan y manejan como
 * si fuese un gestor normal de un &uacute;nico almac&eacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class AggregatedKeyStoreManager extends AOKeyStoreManager {

	private final List<AOKeyStoreManager> ksms = new ArrayList<>();

	AggregatedKeyStoreManager(final AOKeyStoreManager mainKsm) {
		if (mainKsm == null) {
			throw new IllegalArgumentException("El gestor principal de almacenes no puede ser nulo"); //$NON-NLS-1$
		}
		addKeyStoreManager(mainKsm);
		setKeyStoreType(mainKsm.getType());
	}

	/** Constructor. */
	protected AggregatedKeyStoreManager() {
		// Vacio
	}

    /** Devuelve el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves. */
    @Override
	public AOKeyStore getType() {
    	AOKeyStore type = super.getType();
    	if (type == null && this.ksms != null && this.ksms.size() > 0) {
    		type = this.ksms.get(0).getType();
    	}
    	return type;
    }

    /** Devuelve el tipo de almac&eacute;n de claves para un alias determinado.
     * @param alias Alias de la entrada para la cual se desea conocer su tipo de almac&eacute;n.
     * @return Tipo de almac&eacute;n de claves para el alias indicado. */
    @Override
	protected AOKeyStore getType(final String alias) {
    	for (final AOKeyStoreManager ksm : this.ksms) {
    		if (ksm.getCertificate(alias) != null) {
    			return ksm.getType(alias);
    		}
    	}
    	LOGGER.warning(
			"Se ha pedido el tipo de almacen de un alias no contenido en este gestor, se devolvera el tipo por defecto" //$NON-NLS-1$
		);
    	return getType();
    }

	/** Contruye un gestor de claves consistente a su vez en un agregado de varios gestores,
	 * a partir de un almac&acute;n principal.
	 * @param ksm Gestor de claves principal */
	public final void addKeyStoreManager(final AOKeyStoreManager ksm) {
		if (ksm != null) {
			// Si es preferente hay que eliminar antes de anadir los posibles duplicados que hubiese
			if (ksm.isPreferred()) {
				final String[] newAliases = ksm.getAliases();
				MessageDigest md = null;
				try {
					md = MessageDigest.getInstance("SHA1"); //$NON-NLS-1$
				}
				catch (final NoSuchAlgorithmException e) {
					LOGGER.warning(
						"No se ha podido instanciar el generador de huellas digitales SHA1, pueden aparecer duplicados en la lista de certificados: " + e //$NON-NLS-1$
					);
				}
				if (md != null) {
					for (final String alias : newAliases) {
						final String currentThumbprint;
						try {
							currentThumbprint = AOUtil.hexify(
								md.digest(ksm.getCertificate(alias).getEncoded()),
								false
							);
						}
						catch (final CertificateEncodingException e) {
							LOGGER.severe(
								"No se ha podido obtener la huella del certificado con numero de serie '" + ksm.getCertificate(alias).getSerialNumber() + "', pueden aparecer duplicados en la lista de certificados: " + e //$NON-NLS-1$ //$NON-NLS-2$
							);
							continue;
						}
						LOGGER.info(
							"El certificado de huella '" + currentThumbprint + "' se tomara unicamente del almacen preferente" //$NON-NLS-1$ //$NON-NLS-2$
						);
						for (final AOKeyStoreManager currentKsm : this.ksms) {
							currentKsm.deactivateEntry(currentThumbprint);
						}
					}
				}
			}
			this.ksms.add(ksm);
		}
	}

	@Override
	public String[] getAliases() {
		final List<String> aliases = new ArrayList<>();
		for (final AOKeyStoreManager ksm : this.ksms) {
			try {
				aliases.addAll(Arrays.asList(ksm.getAliases()));
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudieron obtener los alias del almacen " + ksm.getType() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		return aliases.toArray(new String[0]);
	}

	@Override
	public X509Certificate getCertificate(final String alias) {
		for (final AOKeyStoreManager ksm : this.ksms) {
			List<String> listAlias;
			try {
				listAlias = Arrays.asList(ksm.getAliases());
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudieron obtener los alias del almacen " + ksm.getType() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			if (listAlias.contains(alias)) {
				return ksm.getCertificate(alias);
			}
		}
		LOGGER.warning(
			"El almacen no contiene ningun certificado con el alias especificado, se devolvera null" //$NON-NLS-1$
		);
		return null;
	}

	@Override
	public KeyStore.PrivateKeyEntry getKeyEntry(final String alias) throws KeyStoreException,
                                                                           NoSuchAlgorithmException,
                                                                           UnrecoverableEntryException {
		for (final AOKeyStoreManager ksm : this.ksms) {
			List<String> listAlias;
			try {
				listAlias = Arrays.asList(ksm.getAliases());
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudieron obtener los alias del almacen " + ksm.getType() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			if (listAlias.contains(alias)) {
				return ksm.getKeyEntry(alias);
			}
		}
		LOGGER.warning(
			"El almacen no contiene ninguna clave el alias especificado, se devolvera null" //$NON-NLS-1$
		);
		return null;
	}

	@Override
	public X509Certificate[] getCertificateChain(final String alias) {
		for (final AOKeyStoreManager ksm : this.ksms) {
			List<String> listAlias;
			try {
				listAlias = Arrays.asList(ksm.getAliases());
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudieron obtener los alias del almacen " + ksm.getType() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			if (listAlias.contains(alias)) {
				return ksm.getCertificateChain(alias);
			}
		}
		LOGGER.warning(
			"El almacen no contiene ninguna cadena de certificados con el alias especificado, se devolvera null" //$NON-NLS-1$
		);
		return null;
	}

	@Override
	public void refresh() throws IOException {
		for (final AOKeyStoreManager ksm : this.ksms) {
			try {
				ksm.refresh();
			}
			catch (final Exception e) {
				ksm.setKeyStore(null);
				LOGGER.warning("Error al actualizar el almacen de tipo " + ksm.getType() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}

	@Override
	protected boolean lacksKeyStores() {
		return this.ksms.isEmpty();
	}

	@Override
	public boolean isKeyEntry(final String alias) throws KeyStoreException {
		for (final AOKeyStoreManager ksm : this.ksms) {
			List<String> listAlias;
			try {
				listAlias = Arrays.asList(ksm.getAliases());
			}
			catch (final Exception e) {
				LOGGER.warning("No se pudieron obtener los alias del almacen " + ksm.getType() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				continue;
			}
			if (listAlias.contains(alias)) {
				return ksm.isKeyEntry(alias);
			}
		}
		throw new KeyStoreException(
			"Se ha pedido comprobar la clave privada de un certificado no contenido en este gestor" //$NON-NLS-1$
		);
	}

	@Override
	public void deactivateEntry(final String certificateThumbprint) {
		for (final AOKeyStoreManager ksm : this.ksms) {
			ksm.deactivateEntry(certificateThumbprint);
		}
	}

	/** Recupera la lista (no mutable) de almacenes del almac&eacute;n agregado.
	 * @return Lista de almacenes. */
	List<AOKeyStoreManager> getKeyStoreManagers() {
		return (List<AOKeyStoreManager>) ((ArrayList<AOKeyStoreManager>)this.ksms).clone();
	}

	/** Elimina todos los almacenes del de claves del almac&eacute;n agregado. */
	public void removeAll() {
		this.ksms.clear();
		setKeyStoreType(null);
	}
}
