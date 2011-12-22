/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

/** Clase que guarda una configuracion generica de firma */
public class GeneralSignConfig {

    private String signAlgorithm = null;
    private String signatureProductionPlace = "";
    private String signContact = "";

    private String signReason = "";

    private boolean useAlgorithmInternally = false;

    /** Establece una configuracion generica de firma.
     * @param algorithm Algoritmo de firma. */
    public GeneralSignConfig(final String algorithm) {
        this.signAlgorithm = algorithm;
    }

    public String getSignAlgorithm() {
        return this.signAlgorithm;
    }

    public String getSignatureProductionPlace() {
        return this.signatureProductionPlace;
    }

    public String getSignContact() {
        return this.signContact;
    }

    public String getSignReason() {
        return this.signReason;
    }

    public boolean isUseAlgorithmInternally() {
        return this.useAlgorithmInternally;
    }

    public void setSignAlgorithm(final String signAlgorithm) {
        this.signAlgorithm = signAlgorithm;
    }

    public void setSignatureProductionPlace(final String signatureProductionPlace) {
        this.signatureProductionPlace = signatureProductionPlace;
    }

    public void setSignContact(final String signContact) {
        this.signContact = signContact;
    }

    public void setSignReason(final String signReason) {
        this.signReason = signReason;
    }

    public void setUseAlgorithmInternally(final boolean useAlgorithmInternally) {
        this.useAlgorithmInternally = useAlgorithmInternally;
    }
}
