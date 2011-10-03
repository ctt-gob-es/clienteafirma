/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.ietf.jgss.Oid;


import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.DigestedData;

/** Clase base para la implementaci&oacute;n del tipo DigestedData La Estructura
 * del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *  DigestedData ::= SEQUENCE {
 *        version CMSVersion,
 *        digestAlgorithm DigestAlgorithmIdentifier,
 *        encapContentInfo EncapsulatedContentInfo,
 *        digest Digest }
 *
 *  Digest ::= OCTET STRING
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje DigestedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */
final class CMSDigestedData {

    /** Genera una estructura de tipo digestedData.
     * @param content
     *        Contenido original
     * @param digestAlgorithm
     *        Algoritmo de huella digital (<i>digest</i>) a usar
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @return Mensaje firmado en tipo Digested Data.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos */
    byte[] genDigestedData(final byte[] content, final String digestAlgorithm, final Oid dataType) throws NoSuchAlgorithmException,
                                                                                                         IOException {

        // Obtenemos el algoritmo para hacer el digest
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(
                AOAlgorithmID.getOID(AOSignConstants.getDigestAlgorithmName(digestAlgorithm)));
        
        // indicamos el tipo de contenido
        final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType.toString());
        final ContentInfo encInfo = new ContentInfo(contentTypeOID, null);

        // digest
        final DEROctetString digest = new DEROctetString(MessageDigest.getInstance(digestAlgorithm).digest(content));

        // construimos el digestedData.
        return new ContentInfo(PKCSObjectIdentifiers.digestedData, new DigestedData(digAlgId, encInfo, digest)).getDEREncoded();
    }
}
