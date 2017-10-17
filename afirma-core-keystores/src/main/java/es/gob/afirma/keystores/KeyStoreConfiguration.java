/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;


/** Agrupaci&oacute;n de los tres atributos principales de un almac&eacute;n de
 * claves: tipo, descripci&oacute;n y biblioteca PKCS#11 */
public final class KeyStoreConfiguration {

    private final AOKeyStore type;
    private final String name;
    private final String lib;

    /** Crea una configuraci&oacute;n para un almac&eacute;n de claves.
     * @param t
     *        Typo de almac&eacute;n de claves
     * @param n
     *        Nombre del almac&eacute;n de claves
     * @param l
     *        Biblioteca PKCS#11 correspondiente al almac&eacute;n de claves
     *        (&uacute;nicamente en almacenes tipo PKCS#11) */
    public KeyStoreConfiguration(final AOKeyStore t, final String n, final String l) {
        this.type = t;
        if (t == null) {
            throw new IllegalArgumentException("Es necesario indicar el tipo de almacen"); //$NON-NLS-1$
        }
        this.name = (n != null) ? n : t.getName();
        this.lib = l;
    }

    /** Obtiene el tipo de almac&eacute;n de claves.
     * @return Tipo de almac&eacute;n de claves */
    public AOKeyStore getType() {
        return this.type;
    }

    /** Obtiene la biblioteca PKCS#11 correspondiente al almac&eacute;n de
     * claves. Este m&eacute;todo aplica &uacute;nicamente en almacenes tipo
     * PKCS#11
     * @return Biblioteca PKCS#11 correspondiente al almac&eacute;n de claves */
    public String getLib() {
        return this.lib;
    }

    @Override
    public String toString() {
        return this.name;
    }
}
