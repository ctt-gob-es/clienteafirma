package es.gob.afirma.crypto.handwritten;

import java.util.ArrayList;
import java.util.List;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.DigestInfo;

import es.gob.afirma.core.signers.AOSignConstants;

/** Estructura ASN.1 que combina datos biom&eacute;tricos con huella digital del documento firmado.
 * <pre>
 *  BioDataStructure ::= SEQUENCE {
 *    isoBioData	OctetctStream,
 *    rawBioData	OctectStream	OPTIONAL,
 *    digestInfo	DigestInfo
 *  }
 * </pre>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class BioDataStructure extends DERSequence {

	/** Crea una estructura ASN.1 que combina datos biom&eacute;tricos con huella digital
	 * del documento firmado.
	 * @param isoBioData Datos biom&eacute;tricos en formato ISO 19794-7.
	 * @param rawBioData Datos biom&eacute;tricos en el formato nativo de la tableta.
	 * @param docDigest Huellla digital original del documento firmado.
	 * @param digestAlgorithmName Algoritmo usado para la huella digital. */
	public BioDataStructure(final byte[] isoBioData,
			                final byte[] rawBioData,
			                final byte[] docDigest,
			                final String digestAlgorithmName) {

		super(getEncodables(isoBioData, rawBioData, docDigest, digestAlgorithmName));
	}

	private static ASN1Encodable[] getEncodables(final byte[] isoBioData,
                                                 final byte[] rawBioData,
                                                 final byte[] docDigest,
                                                 final String digestAlgorithmName) {
		if (isoBioData == null) {
			throw new IllegalArgumentException(
				"Los datos biometricos en formato ISO no pueden ser nulos" //$NON-NLS-1$
			);
		}
		if (docDigest == null || digestAlgorithmName == null) {
			throw new IllegalArgumentException(
				"La huella del documento y el algoritmo de huella usados no pueden ser nulos" //$NON-NLS-1$
			);
		}
		final List<ASN1Encodable> encs = new ArrayList<ASN1Encodable>();
		encs.add(new DEROctetString(isoBioData));
		if (rawBioData != null) {
			encs.add(new DEROctetString(rawBioData));
		}
		encs.add(
			new DigestInfo(
				new AlgorithmIdentifier(getAlgId(digestAlgorithmName)),
				docDigest
			)
		);

		return encs.toArray(new ASN1Encodable[0]);

	}

	private static ASN1ObjectIdentifier getAlgId(final String algName) {
		final String realName = AOSignConstants.getDigestAlgorithmName(algName);
		if ("SHA1".equals(realName)) { //$NON-NLS-1$
			return new ASN1ObjectIdentifier("1.3.14.3.2.26"); //$NON-NLS-1$
		}
		if ("SHA-256".equals(realName)) { //$NON-NLS-1$
			return new ASN1ObjectIdentifier("2.16.840.1.101.3.4.2.1"); //$NON-NLS-1$
		}
		if ("SHA-384".equals(realName)) { //$NON-NLS-1$
			return new ASN1ObjectIdentifier("2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$
		}
		if ("SHA-384".equals(realName)) { //$NON-NLS-1$
			return new ASN1ObjectIdentifier("2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$
		}
		if ("SHA-512".equals(realName)) { //$NON-NLS-1$
			return new ASN1ObjectIdentifier("2.16.840.1.101.3.4.2.3"); //$NON-NLS-1$
		}
		throw new IllegalArgumentException(
			"Algoritmo de huella digital no soportado: " + algName //$NON-NLS-1$
		);
	}

}
