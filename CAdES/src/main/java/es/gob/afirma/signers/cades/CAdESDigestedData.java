/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.cades;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.DigestedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Clase base para la implementaci&oacute;n del tipo DigestedData en CADES
 * basado en CMS. La Estructura del mensaje es la siguiente:<br>
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

final class CAdESDigestedData {

    /** M&eacute;todo que genera la firma de tipo digestedData.
     * @param parameters
     *        Par&aacute;metros necesarios para la generaci&oacute;n de este
     *        tipo.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @return Mensaje firmado en tipo Digested Data.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos */
    byte[] genDigestedData(final P7ContentSignerParameters parameters, final String dataType) throws NoSuchAlgorithmException, IOException {
        if (parameters == null) {
            throw new IllegalArgumentException("Los parametros no pueden ser nulos"); //$NON-NLS-1$
        }
        // Obtenemos el algoritmo para "digestear"
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(parameters.getSignatureAlgorithm());
        final AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
        org.bouncycastle.asn1.x509.AlgorithmIdentifier digAlgId;
        try {
            digAlgId = SigUtils.makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());
        }
        catch (final Exception e) {
            throw new IOException((new StringBuilder()).append("Error de codificacion: ").append(e).toString()); //$NON-NLS-1$
        }
        ContentInfo encInfo = null;

        // indicamos el tipo de contenido
        final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType.toString());
        encInfo = new ContentInfo(contentTypeOID, null);

        final byte data[] = parameters.getContent();

        // digest
        final DEROctetString digest = new DEROctetString(MessageDigest.getInstance(digestAlgorithm.toString()).digest(data));

        // construimos el digestedData.
        return (new ContentInfo(PKCSObjectIdentifiers.digestedData, new DigestedData(digAlgId, encInfo, digest))).getDEREncoded();
    }

}
