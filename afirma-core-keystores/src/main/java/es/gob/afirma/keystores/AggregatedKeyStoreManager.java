/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import es.gob.afirma.core.misc.AOUtil;

import java.io.IOException;
import java.security.*;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.*;
import java.util.logging.Level;

/**
 * Gestor de claves consistente a su vez en un agregado de varios gestores, que se tratan y manejan como
 * si fuese un gestor normal de un &uacute;nico almac&eacute;n. Los almacenes se ordenan de mayor a menor
 * prioridad, de forma que si un certificado est&aacute; presente en varios almacenes, se tomar&aacute; el
 * del almac&eacute;n de mayor prioridad. Es posible insertar los almacenes indicando la posici&oacute;n para
 * alterar esta prioridad.
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public class AggregatedKeyStoreManager extends AOKeyStoreManager {

    private static MessageDigest md = null;

    static {
        try {
            md = MessageDigest.getInstance("SHA1"); //$NON-NLS-1$
        } catch (final NoSuchAlgorithmException e) {
            LOGGER.warning(
                    "No se ha podido instanciar el generador de huellas digitales SHA1, pueden aparecer duplicados en la lista de certificados: " + e //$NON-NLS-1$
            );
        }
    }

    private final List<AOKeyStoreManager> ksms = new ArrayList<>();

    protected boolean smartCardAdded;

    /**
     * Referencias a los certificados de los almacenes. Se cargar&aacute;n al listar el contenido de los almacenes y
     * se borrar&aacute;n si hay cambios en el almac&eacute;n (agregar almacenes, eliminar almacenes, refrescar almacenes,
     * etc.) hasta que se vuelva a listar su contenido.
     */
    private HashMap<String, CertificateReference> loadedReferences = null;

    private boolean initializationError = false;

    AggregatedKeyStoreManager(final AOKeyStoreManager mainKsm) {
        if (mainKsm == null) {
            throw new IllegalArgumentException("El gestor principal de almacenes no puede ser nulo"); //$NON-NLS-1$
        }
        if (mainKsm instanceof AggregatedKeyStoreManager && ((AggregatedKeyStoreManager) mainKsm).isSmartCardAdded()) {
            this.smartCardAdded = true;
        }
        addKeyStoreManager(mainKsm);
    }

    /**
     * Constructor.
     */
    protected AggregatedKeyStoreManager() {
        // Vacio
    }

    /**
     * Devuelve el tipo de almac&eacute;n de claves.
     *
     * @return Tipo de almac&eacute;n de claves.
     */
    @Override
    public final AOKeyStore getType() {
        AOKeyStore type = super.getType();
        if (type == null && !this.ksms.isEmpty()) {
            type = this.ksms.get(0).getType();
        }
        return type;
    }

    /**
     * Devuelve el tipo de almac&eacute;n de claves al que pertenece el certificado con el alias indicado.
     *
     * @param alias Alias de la entrada para la cual se desea conocer su tipo de almac&eacute;n.
     * @return Tipo de almac&eacute;n de claves del certificado.
     */
    @Override
    protected final AOKeyStore getType(final String alias) {

        // Si tenemos las referencias cargadas, buscamos el alias en ellas y devolvemos el
        // tipo del almacén correspondiente
        if (this.loadedReferences != null && this.loadedReferences.containsKey(alias)) {
            return this.loadedReferences.get(alias).getKsm().getType();
        } else {
            LOGGER.warning("Se obtendra el tipo del almacen del alias indicado sin haber cargado previamente el listado de referencias (getAliases), se buscara el alias en todos los almacenes"); //$NON-NLS-1$
        }

        // Si no las teniamos cargadas, las buscamos por prioridad
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

    /**
     * Agrega un nuevo gestor de almac&eacute;n al conjunto actual asignandole menor priodidad que al resto.
     *
     * @param ksm Nuevo gestor de almac&eacute;n.
     */
    public final void addKeyStoreManager(final AOKeyStoreManager ksm) {
        if (ksm == null) {
            return;
        }

        // Reseteamos las referencias cargadas
        this.loadedReferences = null;

        this.ksms.add(ksm);
    }

    /**
     * Agrega un nuevo gestor de almac&eacute;n estableci&eacute;ndole un orden de prioridad. Cuando menor sea el
     * n&uacute;mero de posici&oacute;n, mayor prioridad tendr&aacute; el almac&eacute;n.
     *
     * @param position Posici&oacute;n en la que se desea agregar el almac&eacute;n. Debe ser un n&uacute;mero entre 0 y
     *                 el n&uacute;mero de almacenes agregados actualmente.
     * @param ksm      Nuevo gestor de almac&eacute;n.
     *
     */
    public final void addKeyStoreManager(int position, final AOKeyStoreManager ksm) {
        if (ksm == null) {
            return;
        }

        // Reseteamos las referencias cargadas
        this.loadedReferences = null;

        // Agregamos el almacen al listado
        try {
            this.ksms.add(position, ksm);
        } catch (IndexOutOfBoundsException e) {
            LOGGER.warning("Se indico una prioridad no valida para el almacen. Se anaddira con la menor prioridad");
            this.ksms.add(ksm);
        }
    }

    private static String getThumbprint(X509Certificate certificate) {

        String thumbprint = null;
        if (md != null) {
            try {
                thumbprint = AOUtil.hexify(md.digest(certificate.getEncoded()), false);
            } catch (final CertificateEncodingException e) {
                LOGGER.severe(
                        "No se ha podido obtener la huella del certificado con numero de serie '" + certificate.getSerialNumber() + "', pueden aparecer duplicados en la lista de certificados: " + e //$NON-NLS-1$ //$NON-NLS-2$
                );
            }
        }

        if (thumbprint == null) {
            thumbprint = certificate.getSerialNumber().toString() + "_" + certificate.getIssuerX500Principal().getName();
        }

        return thumbprint;
    }

    @Override
    public final String[] getAliases() {

        if (this.loadedReferences == null) {
           loadReferences();
        }
        return this.loadedReferences.keySet().toArray(new String[0]);
    }

    private void loadReferences() {

        this.initializationError = false;

        Set<String> indexedCertificates = new HashSet<>();

        this.loadedReferences = new HashMap<>();

        // Por cada almacen, en orden, se listan los certificados, se comprueba que no esten ya entre los
        // certificados referenciados y, de no estarlo, se agrega la referencia cuidando que no haya ninguna
        // con el mismo alias
        for (final AOKeyStoreManager ksm : this.ksms) {

            // Listamos los certificados
            String[] ksmAliases;
            try {
                ksmAliases = ksm.getAliases();
                // Comprobamos si es un almacen agregado que no se inicializo correctamente, en cuyo caso trasladamos el error
                if (ksmAliases.length == 0 && ksm instanceof AggregatedKeyStoreManager
                        && ((AggregatedKeyStoreManager) ksm).isInitializationFailed()) {
                    throw new IllegalStateException("El almacen agregado interno " + ksm.getType() + " no se inicializo correctamente"); //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
            catch (final IllegalStateException e) {
                LOGGER.log(Level.SEVERE, "El almacen " + ksm.getType() + " no se inicializo correctamente", e); //$NON-NLS-1$ //$NON-NLS-2$
                initializationError = true;
                continue;
            }
            catch (final Exception e) {
                LOGGER.log(Level.WARNING, "No se pudieron obtener los alias del almacen " + ksm.getType(), e); //$NON-NLS-1$ //$NON-NLS-2$
                continue;
            }

            for (String alias : ksmAliases) {

                // Comprobamos que el certificado no exista ya en un almacen de mayor prioridad
                X509Certificate certificate = ksm.getCertificate(alias);
                if (certificate == null) {
                    LOGGER.warning("No se pudo recuperar el certificado con alias '" + alias + "' del almacen " + ksm.getType()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
                    continue;
                }

                String certThumbprint = getThumbprint(certificate);
                if (indexedCertificates.contains(certThumbprint)) {
                    LOGGER.info("El certificado con numero de serie '" + certificate.getSerialNumber() + "' emitido por '" + certificate.getIssuerX500Principal() + "' ya se encuentra en un almacen con mayor prioridad. Se omitira el del almacen " + ksm.getType()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
                    continue;
                }

                // Asociamos al certificado una referencia unica
                String reference = getAvailableReference(alias);
                this.loadedReferences.put(reference, new CertificateReference(alias, ksm));

                // Agregamos la huella del certificado a la lista de certificados ya indexados
                // para que no se repita en los siguientes almacenes
                indexedCertificates.add(certThumbprint);
            }
        }
    }

    /**
     * Obtiene una referencia que no exista en el listado de referencias cargadas. Si es posible, usar&aacute; el propio
     * alias proporcionado.
     *
     * @param alias Alias del certificado.
     * @return Referencia disponible para el certificado.
     */
    private String getAvailableReference(String alias) {

        if (!this.loadedReferences.containsKey(alias)) {
            return alias;
        }

        int i = 1;
        String references;
        do {
            references = alias + "_" + i; //$NON-NLS-1$
            i++;
        } while (this.loadedReferences.containsKey(references));

        return references;
    }


    @Override
    public final X509Certificate getCertificate(final String alias) {

        // Si tenemos las referencias cargadas, buscamos el alias en ellas y devolvemos el
        // certificado del almacén correspondiente
        if (this.loadedReferences != null && this.loadedReferences.containsKey(alias)) {
            CertificateReference reference = this.loadedReferences.get(alias);
            return reference.getKsm().getCertificate(reference.getAlias());
        } else {
            LOGGER.warning("Se recupera el certificado del almacen sin haber cargado previamente el listado de referencias (getAliases), se buscara el alias en todos los almacenes"); //$NON-NLS-1$
        }

        // Si no estaban cargadas, las buscamos por prioridad
        for (final AOKeyStoreManager ksm : this.ksms) {
            List<String> listAlias;
            try {
                listAlias = Arrays.asList(ksm.getAliases());
            } catch (final Exception e) {
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
    public final KeyStore.PrivateKeyEntry getKeyEntry(final String alias) throws KeyStoreException,
            NoSuchAlgorithmException,
            UnrecoverableEntryException {
        // Si tenemos las referencias cargadas, buscamos el alias en ellas y devolvemos la
        // entrada del almacén correspondiente
        if (this.loadedReferences != null && this.loadedReferences.containsKey(alias)) {
            CertificateReference reference = this.loadedReferences.get(alias);
            return reference.getKsm().getKeyEntry(reference.getAlias());
        } else {
            LOGGER.warning("Se recuperan entradas del almacen sin haber cargado previamente el listado de referencias (getAliases), se buscara el alias en todos los almacenes"); //$NON-NLS-1$
        }

        // Si no estaban cargadas, las buscamos por prioridad

        for (final AOKeyStoreManager ksm : this.ksms) {
            List<String> listAlias;
            try {
                listAlias = Arrays.asList(ksm.getAliases());
            } catch (final Exception e) {
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
    public final X509Certificate[] getCertificateChain(final String alias) {

        // Si tenemos las referencias cargadas, buscamos el alias en ellas y devolvemos la
        // cadena de certificacion desde el almacén correspondiente
        if (this.loadedReferences != null && this.loadedReferences.containsKey(alias)) {
            CertificateReference reference = this.loadedReferences.get(alias);
            return reference.getKsm().getCertificateChain(reference.getAlias());
        } else {
            LOGGER.warning("Se recupera la cadena de certificados desde almacen sin haber cargado previamente el listado de referencias (getAliases), se buscara el alias en todos los almacenes"); //$NON-NLS-1$
        }

        // Si no estaban cargadas, las buscamos por prioridad
        for (final AOKeyStoreManager ksm : this.ksms) {
            List<String> listAlias;
            try {
                listAlias = Arrays.asList(ksm.getAliases());
            } catch (final Exception e) {
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

        // Borramos las referencias cargadas para asegurarnos de que no quedan desactualizadas tras el refresco
        // de los almacenes
        this.loadedReferences = null;

        // Refrescamos cada almacen individual
        for (final AOKeyStoreManager ksm : this.ksms) {
            try {
                ksm.setParentComponent(getParentComponent());
                ksm.refresh();
            } catch (final Exception e) {
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
    public final boolean isKeyEntry(final String alias) throws KeyStoreException {

        // Si tenemos las referencias cargadas, buscamos el alias en ellas y devolvemos
        // certificado del almacén correspondiente
        if (this.loadedReferences != null && this.loadedReferences.containsKey(alias)) {
            return true;
        } else {
            LOGGER.warning("Se comprueba la existencia de una entrada del almacen sin haber cargado previamente el listado de referencias (getAliases), se buscara el alias en todos los almacenes"); //$NON-NLS-1$
        }

        // Si no estaban cargadas, las buscamos por prioridad
        for (final AOKeyStoreManager ksm : this.ksms) {
            List<String> listAlias;
            try {
                listAlias = Arrays.asList(ksm.getAliases());
            } catch (final Exception e) {
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

    /**
     * Recupera la lista (no mutable) de almacenes del almac&eacute;n agregado.
     *
     * @return Lista de almacenes.
     */
    List<AOKeyStoreManager> getKeyStoreManagers() {
        return (List<AOKeyStoreManager>) ((ArrayList<AOKeyStoreManager>) this.ksms).clone();
    }

    /**
     * Elimina todos los almacenes del de claves del almac&eacute;n agregado.
     */
    public void removeAll() {
        this.ksms.clear();
        this.loadedReferences = null;
    }

    public boolean isSmartCardAdded() {
        return this.smartCardAdded;
    }

    public void setSmartCardAdded(boolean smartCardAdded) {
        this.smartCardAdded = smartCardAdded;
    }

    /**
     * Indica si se produjo un error al inicializar el almac&eacute;n agregado.
     * @return {@code true} si se produjo un error al inicializar el almac&eacute;n agregado,
     * {@code false} en caso contrario.
     */
    public boolean isInitializationFailed() {
        return initializationError;
    }

    /**
     * Clase que representa una referencia a un certificado mediante su alias y el almac&eacute;n en el que se
     * encuentra.
     */
    private static class CertificateReference {
        private final String alias;
        private final AOKeyStoreManager ksm;

        CertificateReference(final String alias, final AOKeyStoreManager ksm) {
            this.alias = alias;
            this.ksm = ksm;
        }

        String getAlias() {
            return this.alias;
        }

        AOKeyStoreManager getKsm() {
            return this.ksm;
        }
    }
}
