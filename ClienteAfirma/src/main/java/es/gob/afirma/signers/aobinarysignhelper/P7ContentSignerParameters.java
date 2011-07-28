/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.net.URI;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import es.gob.afirma.misc.AOConstants;

/** Clase auxiliar que prepara los par&aacute;metros para cada una de las formas
 * de firma. */
public final class P7ContentSignerParameters {

    private final byte[] data;
    private final String sigAlgo;
    private final X509Certificate[] certChain;
    private final byte[] signature;
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");

    /** M&eacute;todo que asigna los datos pasados por par&aacute;metro a las
     * propiedades de la clase.
     * @param data2
     *        Archivo de firmas.
     * @param signatureAlgorithm
     *        Tipo de datos.
     * @param cChain
     *        Certificados del firmante */
    public P7ContentSignerParameters(final byte[] data2, String signatureAlgorithm, X509Certificate[] cChain) {

        data = data2.clone();

        if (signatureAlgorithm == null || signatureAlgorithm.length() < 1) {
            LOGGER.warning("No se especifico algoritmo para la firma CADES, se utilizara " + AOConstants.DEFAULT_SIGN_ALGO);
            signatureAlgorithm = AOConstants.DEFAULT_SIGN_ALGO;
        }
        if (cChain == null) {
            LOGGER.warning("No se ha proporcionado una cadena de certificados para la construccion de los parametros de firma CADES, se utilizara una cadena vacia");
            certChain = new X509Certificate[0];
        }
        else {
            certChain = cChain.clone();
        }

        sigAlgo = signatureAlgorithm;
        signature = new byte[0]; // la firma se realizara despues

    }

    /** M&eacute;todo que devuelve el contenido
     * @return el contenido */
    public byte[] getContent() {
        return data.clone();
    }

    /** M&eacute;todo que devuelve la firma.
     * @return la firma. */
    public byte[] getSignature() {
        return signature.clone();
    }

    /** M&eacute;todo que devuelve el tipo
     * @return el tipo */
    public String getSignatureAlgorithm() {
        return sigAlgo;
    }

    /** M&eacute;todo que devuelve los certificados del firmante
     * @return Array de certificados. */
    public X509Certificate[] getSignerCertificateChain() {
        return certChain.clone();
    }

    /** Obtiene la URL de la autoridad de sellado de tiempo.
     * @return URL de la autoridad de sellado de tiempo */
    public URI getTimestampingAuthority() {
        LOGGER.info("Se ha solicitado la URI de la autoridad de sellado de tiempo, pero como es una caracteristica aun no soportada se devolvio null");
        return null;
    }

    /** Obtiene el certificado de la autoridad de sellado de tiempo.
     * @return Certificado de la autoridad de sellado de tiempo */
    public X509Certificate getTimestampingAuthorityCertificate() {
        LOGGER.info("Se ha solicitado el certificado de la autoridad de sellado de tiempo, pero como es una caracteristica aun no soportada se devolvio null");
        return null;
    }
}
