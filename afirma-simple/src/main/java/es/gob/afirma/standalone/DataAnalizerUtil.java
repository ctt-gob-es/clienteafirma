/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.io.ByteArrayInputStream;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;

/** Utilidad para el an&aacute;lisis de ficheros de datos.
 * @author Carlos Gamuci. */
public final class DataAnalizerUtil {

    private DataAnalizerUtil() {
        // No permitimos la instanciacion
    }

    /** Identifica si los datos proporcionados son un certificado X509, devolvi&eacute;ndolo en ese caso.
     * @param data Datos a analizar.
     * @return Certificado X509 si los datos lo eran.
     * @throws CertificateException Cuando los datos proporcionados no son un certificado X509. */
    public static X509Certificate isCertificate(final byte[] data) throws CertificateException {

    	if (data == null || data.length < 1) {
    		throw new CertificateException(
				"Los datos eran nulos o vacios" //$NON-NLS-1$
			);
    	}

    	final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$

    	// Antes de nada un intento directo
    	try {
	        return (X509Certificate) cf.generateCertificate(
	            new ByteArrayInputStream(
	                 data
	             )
	        );
    	}
        catch(final Exception e) {
            // Ignoramos los errores
        }

    	// Despues, intento en Base64 directo sin cabeceras y con posibilidad de URLEncoding
    	try {
	        return (X509Certificate) cf.generateCertificate(
	            new ByteArrayInputStream(
	                 Base64.decode(
	                   new String(data).replace("%0A", "").replace("%2F", "/").replace("%2B", "+").replace("%3D", "=") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
	                 )
	             )
	        );
    	}
        catch(final Exception e) {
            // Ignoramos los errores
        }

    	throw new CertificateException(
    		"Los datos proporcionados no son un certificado X.509" //$NON-NLS-1$
		);
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero XML.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son XML. */
    public static boolean isXML(final byte[] data) {
        return AOFileUtils.isXML(data);
    }

    /** Comprueba si los datos introducidos se corresponden a una firma XML soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma XML soportada. */
    public static boolean isSignedXML(final byte[] data) {
        try {
            return new AOXAdESSigner().isSign(data) || new AOXMLDSigSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero PDF.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son un PDF. */
    public static boolean isPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a un fichero PDF firmado.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son un PDF firmado.
     */
    public static boolean isSignedPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a una firma binaria soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma binaria soportada.
     */
    public static boolean isSignedBinary(final byte[] data) {

        try {
            return new AOCMSSigner().isSign(data) || new AOCAdESSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Indica si los datos son una factura electr&oacute;nica.
     * @param file Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    public static boolean isFacturae(final byte[] file) {
        try {
            return new AOFacturaESigner().isValidDataFile(file);
        }
        catch(final Exception e) {
            return false;
        }
    }

	/**
     * Comprueba si los datos introducidos se corresponden con una factura
     * electr&oacute;nica firmada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma XML soportada.
     */
    public static boolean isSignedFacturae(final byte[] data) {

        try {
            return new AOFacturaESigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un documento ODF.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son ODF. */
    public static boolean isODF(final byte[] data) {
        try {
            return new AOODFSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a una firma ODF soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma ODF soportada.
     */
    public static boolean isSignedODF(final byte[] data) {
        try {
            return new AOODFSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un documento OOXML.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son OOXML. */
    public static boolean isOOXML(final byte[] data) {
        try {
            return new AOOOXMLSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /**
     * Comprueba si los datos introducidos se corresponden a una firma OOXML soportada.
     * @param data Datos a analizar.
     * @return Devuelve {@code true} si los datos son una firma OOXML soportada.
     */
    public static boolean isSignedOOXML(final byte[] data) {
        try {
            return new AOOOXMLSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }
}
