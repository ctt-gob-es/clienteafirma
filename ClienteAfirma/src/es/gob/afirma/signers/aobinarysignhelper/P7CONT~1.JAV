/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers.aobinarysignhelper;

import java.net.URI;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import es.gob.afirma.misc.AOConstants;

/**
 * Clase auxiliar que prepara los par&aacute;metros para cada una de las formas de firma.
 *
 */
public final class P7ContentSignerParameters {

    private byte[] data;
    private String sigTipo;
    private X509Certificate[] certChain;
    private byte[] signature;

    /**
     * M&eacute;todo que asigna los datos pasados por par&aacute;metro a las propiedades de la clase.
     *
     * @param data2 Archivo de firmas.
     * @param tipo Tipo de datos.
     * @param cChain Certificados del firmante
     */
    public P7ContentSignerParameters(final byte[] data2, String tipo, X509Certificate[] cChain) {

    	data = data2;

        if (tipo == null || tipo.length() < 1) {
            Logger.getLogger("es.gob.afirma").warning(
                "No se especifico algoritmo para la firma CADES, se utilizara " + AOConstants.DEFAULT_SIGN_ALGO
            );
            tipo = AOConstants.DEFAULT_SIGN_ALGO;
        }
        if (cChain == null) {
            Logger.getLogger("es.gob.afirma").warning(
                "No se ha proporcionado una cadena de certificados para la construccion de los " +
                "parametros de firma CADES, se utilizara una cadena vacia"
            );
            cChain = new X509Certificate[0];
        }

        sigTipo = tipo;
        certChain = cChain;
        signature =  new byte[0]; // la firma se realizara despues

    }

    /**
     * M&eacute;todo que devuelve el contenido
     * @return el contenido
     */
    public byte[] getContent() {
        return data;
    }

    /**
     * M&eacute;todo que devuelve la firma.
     * @return la firma.
     */
    public byte[] getSignature() {
        return signature;
    }

    /**
     * M&eacute;todo que devuelve el tipo
     * @return  el tipo
     */
    public String getSignatureAlgorithm() {
        return sigTipo;
    }

    /**
     * M&eacute;todo que devuelve los certificados del firmante
     * @return  Array de certificados.
     */
    public X509Certificate[] getSignerCertificateChain() {
        return certChain;
    }

    public URI getTimestampingAuthority() {
        Logger.getLogger("es.gob.afirma").info(
            "Se ha solicitado la URI de la autoridad de sellado de tiempo, pero como es una caracteristica " +
            "aun no soportada se devolvio null"
        );
        return null;
    }

    public X509Certificate getTimestampingAuthorityCertificate() {
        Logger.getLogger("es.gob.afirma").info(
            "Se ha solicitado el certificado de la autoridad de sellado de tiempo, pero como es una caracteristica " +
            "aun no soportada se devolvio null"
        );
        return null;
    }
}