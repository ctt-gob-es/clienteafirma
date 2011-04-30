/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Signature;
import java.util.List;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.BERSet;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.exceptions.AOException;



/**
 *  Clase que contiene una serie de m&eacute;todos utilizados por GenSignedData,
 *  GenCadesSignedData, CoSigner y CounterSigner.
 */
final class SigUtils {

    /**
     * M&eacute;todo que devuelve el Identificador del algoritmo.
     *
     * @param oid OID del algoritmo a idenfiticar
     * @param params par&aacute;metros que identifican el algoritmo en si
     * @return El identificador del algoritmo formateado y listo para introducir
     * en el cms.
     * @throws java.io.IOException
     */
    static AlgorithmIdentifier makeAlgId(String oid, byte[] params) throws IOException {
        if (params != null) 
        	return new AlgorithmIdentifier(new DERObjectIdentifier(oid), makeObj(params));
        return new AlgorithmIdentifier(new DERObjectIdentifier(oid), new DERNull());
    }

    /**
     * Genera un objeto formateado de tipo ASN1 especial para insertarlo en el CMS.
     * Devuelve <code>null</code> si le llega una codificaci&oacute;n nula
     * @param encoding  Lo codificado
     * @return  Un objeto formateado de tipo DER
     * @throws java.io.IOException
     */
    static DERObject makeObj(byte[] encoding) throws IOException {
        if (encoding == null) {
        	Logger.getLogger("es.gob.afirma").warning("La codificacion era nula, se devolvera null");
            return null;
        }
        return new ASN1InputStream(new ByteArrayInputStream(encoding)).readObject();
    }

    /**
     * Genera un estructura de tipo SET de formato ASN1.
     * @param derObjects Una lista con los objetos a obtener el tipo SET
     * @return  Un SET de ASN1 con los elementos de la lista introducida.
     */
    static ASN1Set createBerSetFromList(List<DEREncodable> derObjects) {
        ASN1EncodableVector v = new ASN1EncodableVector();
        for (DEREncodable d : derObjects) v.add(d);
//		  Version del blucle para Java 1.4
//        for (Iterator it = derObjects.iterator(); it.hasNext();) {
//            v.add((DEREncodable) it.next());
//        }
        return new BERSet(v);
    }

    /**
     * Genera un atributo de un SET en formato DER
     * @param attr Atributo a formatear.
     * @return SET en formato DER del atributo.
     */
    static ASN1Set getAttributeSet(AttributeTable attr) {
        if (attr != null) return new DERSet(attr.toASN1EncodableVector());
    	Logger.getLogger("es.gob.afirma").warning("Los atributos eran nulos, se devolvera null");
        return null;
    }

    /**
     * Genera un estructura de tipo SET de formato ASN1.
     * @param derObjects Una lista con los objetos a obtener el tipo SET
     * @return  Un SET de ASN1 con los elementos de la lista introducida.
     */
    static ASN1Set fillRestCerts(final List<DEREncodable> derObjects, final ASN1EncodableVector v) {
        for (DEREncodable d : derObjects) v.add(d);
        return new BERSet(v);
    }

    /**
	 * Firma un fichero (o un conjunto de datos binarios) en formato PKCS#1.
	 * @param file Datos binarios a firmar
	 * @param algorithm Algoritmo a usar para la firma
	 * @param keyEntry Clave a usar para la firma
	 * @return Firma en formato binario sin ning&uacute;n tipo de a&ntilde;adido
	 * @throws AOException Cuando ocurre cualquier error en la firma PKCS#1
	 */
	static byte[] signData(final InputStream file, 
			                      final String algorithm, 
			                      final PrivateKeyEntry keyEntry) throws AOException {

		Signature sig = null;
		try {
			sig = Signature.getInstance(algorithm);
		}
		catch (final Throwable e) {
			throw new AOException(
				"Error obteniendo la clase de firma para el algoritmo " + algorithm, e
			);
		}
		try {
			sig.initSign(keyEntry.getPrivateKey());
		}
		catch (final Throwable e) {
            throw new AOException(
        		"Error al inicializar la firma con la clave privada", e
			);
		}
		BufferedInputStream bufin = new BufferedInputStream(file);
		byte[] buffer = new byte[1024];
		int len;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

		try {
			while (bufin.available() != 0) {
			    len = bufin.read(buffer);
			    sig.update(buffer, 0, len);
                baos.write(buffer, 0, len);
			}
		}
		catch (final Throwable e) {
             Logger.getLogger("es.gob.afirma").severe(
					"Error al leer los datos a firmar: " + e
			);
		}
		try {
			bufin.close();
		}
		catch (final Throwable e) {
			Logger.getLogger("es.gob.afirma").warning(
				"Error al cerrar el fichero de datos, el proceso de firma continuara: "+e
			);
		}
		byte[] realSig;
		try {
			realSig = sig.sign();
		}
		catch (final Throwable e) {
			throw new AOException("Error durante el proceso de firma", e);
		}

		return realSig;
	}

}
