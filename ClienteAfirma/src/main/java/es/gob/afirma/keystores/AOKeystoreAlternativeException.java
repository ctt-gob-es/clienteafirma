/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.keystores;

import es.gob.afirma.misc.AOConstants;

/** Indica que ocurri&oacute; un error intentando obtener un <code>KeyStore</code>, pero que se obtuvo uno de un tipo alternativo.
 * @version 0.1 */
public final class AOKeystoreAlternativeException extends Exception {

    private static final long serialVersionUID = -1536411480952188376L;

    private final AOConstants.AOKeyStore alternativeKs;

    /** Crea la excepci&oacute;n con un mensaje determinado y un almac&eacute;n
     * alternativo que podr&iacute;a usarse como sustituto.
     * @param ks
     *        Almac&eacute;n de claves y certificados alternativo
     * @param desc
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param e
     *        Excepci&oacute;n que ha causado el lanzamiento de esta. */
    public AOKeystoreAlternativeException(final AOConstants.AOKeyStore ks, final String desc, final Exception e) {
        super(desc, e);
        if (ks == null) {
            throw new IllegalArgumentException("Es necesario proporcionar un AOConstants.AOKeyStore alternativo");
        }
        alternativeKs = ks;
    }

    /** Crea la excepci&oacute;n con un mensaje determinado y un almac&eacute;n
     * alternativo que podr&iacute;a usarse como sustituto.
     * @param ks
     *        Almac&eacute;n de claves y certificados alternativo
     * @param desc
     *        Mensaje descriptivo de la excepci&oacute;n. */
    public AOKeystoreAlternativeException(final AOConstants.AOKeyStore ks, final String desc) {
        super(desc);
        if (ks == null) {
            throw new IllegalArgumentException("Es necesario proporcionar un AOConstants.AOKeyStore alternativo");
        }
        alternativeKs = ks;
    }

    /** Obtiene el almac&eacute;n alternativo que podr&iacute;a usarse como
     * sustituto.
     * @return Almac&eacute;n de claves y certificados alternativo */
    public AOConstants.AOKeyStore getAlternativeKsm() {
        return alternativeKs;
    }

}
