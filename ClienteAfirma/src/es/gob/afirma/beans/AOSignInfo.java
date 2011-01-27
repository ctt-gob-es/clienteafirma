/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.beans;

/** Objeto que contiene la informacion general de un objeto de firma. Ya que un objeto de firma puede
 * contener muchas firmas, se considera informaci&oacute;n general la que aplica a todo el
 * objeto. Esto es:
 *  <ul><li>Formato de firma: Formato general de la firma (p.e. CAdES, XAdES,...)</li>
 *  <li>Variante: Variante del formato de firma (p.e. Enveloped, Detached,...)</li>
 *  <li>URL de firma: URL desde donde descargar el fichero de firma</li>
 *  <li>URL de datos: URL desde donde descargar el fichero de datos</li>
 *  <li>Codigo de verificacion: Codigo en base64 con la informacion de la firma</li></ul>
 *  Todos los campos, salvo el "Formato de firma" son opcionales.<br/><br/>
 */
public final class AOSignInfo {

    /** Formato de firma. */
    private String format = null;
    
    /** Variante del formato de firma. */
    private String variant = null;
    
    /** URL desde la que descargar el objeto de firma. */
    private String urlSignObject = null;
    
    /** URL desde la que descargar el objeto de datos. */
    private String urlSignedData = null;
    
    /** C&oacute;digo de verificaci&oacute;n de la firma en Base64. */
    private String b64VerificationCode = null;
    
    /**
     * Construye un objeto de informaci&oacute;n de firma. Si no se especifica
     * un formato de firma se establece el formato "Desconocido".
     * @param signFormat Formato general de firma.
     */
    public AOSignInfo(String signFormat) {
        this.format = (signFormat != null ? signFormat : "Desconocido");
    }
    
    /**
     * Recupera la variante de formato a la que pertene e objeto de firma.
     * @return Nombre de la variante.
     */
    public String getVariant() {
        return variant;
    }

    public void setVariant(final String variant) {
        this.variant = variant;
    }

    public String getUrlSignObject() {
        return urlSignObject;
    }

    public void setUrlSignObject(final String urlSignObject) {
        this.urlSignObject = urlSignObject;
    }

    public String getUrlSignedData() {
        return urlSignedData;
    }

    public void setUrlSignedData(String urlSignedData) {
        this.urlSignedData = urlSignedData;
    }

    public String getB64VerificationCode() {
        return b64VerificationCode;
    }

    public void setB64VerificationCode(String b64VerificationCode) {
        this.b64VerificationCode = b64VerificationCode;
    }

    public String getFormat() {
        return format;
    }
}
