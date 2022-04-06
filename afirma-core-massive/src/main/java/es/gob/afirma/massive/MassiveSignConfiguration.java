/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.massive;

import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import es.gob.afirma.core.signers.AOSignConstants;

/** Almacena los datos necesarios para realizar una operaci&oacute;n masiva
 * de firma. */
public final class MassiveSignConfiguration {

    private final PrivateKeyEntry keyEntry;

    private MassiveType massiveOperation = MassiveType.SIGN;
    private String algorithm = AOSignConstants.DEFAULT_SIGN_ALGO;
    private String mode = AOSignConstants.DEFAULT_SIGN_MODE;
    private String defaultFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;
    private String signatureFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;
    private boolean originalFormat = true;
    private Properties extraParams;

    /** Crea un <i>JavaBean</i> con los par&aacute;metros necesarios para las
     * operaciones de firma masiva.
     * @param keyEntry
     *        Clave privada para las firmas */
    public MassiveSignConfiguration(final PrivateKeyEntry keyEntry) {
        this.keyEntry = keyEntry;
        this.extraParams = new Properties();
    }

	/** Recupera la operaci&oacute;n masiva configurada.
     * @return Tipo de operaci&oacute;n masiva. */
    public MassiveType getMassiveOperation() {
        return this.massiveOperation;
    }

    /** Establece la operaci&oacute;n masiva que deber&aacute; ejecutarse.
     * Si se introduce {@code null} se reestablecer&aacute; la operaci&oacute;n
     * por defecto.
     * @param massiveOperation
     *        Tipo de operaci&oacute;n masiva. */
    public void setMassiveOperation(final MassiveType massiveOperation) {
        this.massiveOperation = massiveOperation != null ?
        		massiveOperation : MassiveType.SIGN;
    }

    /** Recupera el algoritmo de firma configurado.
     * @return Algoritmo de firma. */
    public String getAlgorithm() {
        return this.algorithm;
    }

    /** Estable el algoritmo de firma. Si se introduce {@code null} se
     * reestablecer&aacute; el algoritmo por defecto.
     * @param algorithm
     *        Algoritmo de firma. */
    public void setAlgorithm(final String algorithm) {
        this.algorithm = algorithm != null ?
        		algorithm : AOSignConstants.DEFAULT_SIGN_ALGO;
    }

    /** Recupera el modo de firma configurado.
     * @return Modo de firma. */
    public String getMode() {
        return this.mode;
    }

    /** Estable el modo de firma. Si se introduce {@code null} se
     * reestablecer&aacute; el modo por defecto.
     * @param mode
     *        Modo de firma. */
    public void setMode(final String mode) {
        this.mode = mode != null ? mode : AOSignConstants.DEFAULT_SIGN_MODE;
    }

    /** Recupera el formato de firma configurado por defecto.
     * @return Formato de firma. */
    public String getDefaultFormat() {
        return this.defaultFormat;
    }

    /** Estable el formato de firma por defecto (para cuando no se desee
     * respetar el original o se realiza una firma masiva). Este valor sustituye
     * al formato inicial para firmas masivas.<br>
     * Si se introduce {@code null} se reestablecer&aacute; el formato por defecto.
     * @param defaultFormat
     *        Formato de firma. */
    public void setDefaultFormat(final String defaultFormat) {
        this.defaultFormat = defaultFormat != null ?
        		defaultFormat : AOSignConstants.DEFAULT_SIGN_FORMAT;
        this.signatureFormat = this.defaultFormat;
    }

    /** Recupera el formato de firma utilizado para las operaciones de firma masiva.
     * @return Formato de firma utilizado en las operaciones de firma masiva. */
    public String getSignatureFormat() {
		return this.signatureFormat;
	}

    /** Establece el formato de firma para la operaci&oacute;n de firma masiva que, a diferencia
     * del resto de operaciones, permite ser cambiado durante el proceso de firma masiva.<br>
     * Si se establece {@code null} se configura el formato de firma establecido por defecto.
     * Al inicio del proceso de firma masiva este formato siempre tendr&aacute; el mismo valor
     * que el formato por defecto configurado.
     * @param signatureFormat Formato de firma. */
    public void setSignatureFormat(final String signatureFormat) {
		this.signatureFormat = signatureFormat != null ?
				signatureFormat : this.defaultFormat;
	}

    /** Indica si se ha configurado que las multifirmas respeten el formato
     * de firma original.
     * @return Devuelve {@code true} si se ha configurado que se respete el
     *         formato de firma, {@code false} en caso contrario. */
    public boolean isOriginalFormat() {
        return this.originalFormat;
    }

    /** Estable si debe utilizarse un formato de firma original en le caso de
     * las multifirmas masivas.
     * @param originalFormat
     *        Respetar formato original de firma. */
    public void setOriginalFormat(final boolean originalFormat) {
        this.originalFormat = originalFormat;
    }

    /** Recupera entrada de la clave de firma.
     * @return Entrada de la clave de firma. */
    public PrivateKeyEntry getKeyEntry() {
        return this.keyEntry;
    }

    /** Establece par&aacute;metros adicionales para la configuraci&oacute;n
     * de la operaci&oacute;n masiva. Si se introduce {@code null} se
     * reestablecer&aacute; la configuraci&oacute;n por defecto.
     * @param extraParams
     *        Par&aacute;metros adicionales. */
    public void setExtraParams(final Properties extraParams) {
        if (extraParams != null) {
            this.extraParams = (Properties) extraParams.clone();
        }
        else {
            this.extraParams.clear();
        }
    }

    /** Recupera los par&aacute;metros adicionales configurados para la
     * operaci&oacute;n masiva.
     * @return Par&aacute;metros adicionales. */
    public Properties getExtraParams() {
    	return this.extraParams != null ? (Properties) this.extraParams.clone() : null;
    }


}
