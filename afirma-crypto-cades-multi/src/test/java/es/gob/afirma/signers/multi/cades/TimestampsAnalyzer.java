package es.gob.afirma.signers.multi.cades;

import java.io.ByteArrayInputStream;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.cert.X509CertificateHolder;
import org.spongycastle.cms.CMSException;
import org.spongycastle.cms.CMSSignedData;
import org.spongycastle.cms.SignerInformation;
import org.spongycastle.tsp.TimeStampToken;

import es.gob.afirma.core.signers.AOTimestampInfo;

/** Analizador de sellos de tiempo en firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class TimestampsAnalyzer {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Obtiene los sellos de una firma CMS.
	 * @param sign Firma.
	 * @return Sellos de una firma CMS. */
	static List<AOTimestampInfo> getCmsTimestamps(final byte[] sign) {
		final CMSSignedData signedData;
		try {
			signedData = new CMSSignedData(sign);
		}
		catch (final CMSException e) {
			LOGGER.log(Level.SEVERE,
				"La firma proporcionada no es un SignedData compatible CMS, se devolvera una lista de sellos vacia: " + e, //$NON-NLS-1$
				e
			);
			return new ArrayList<>(0);
		}
		return getCmsTimestamps(signedData);
	}

	private static List<AOTimestampInfo> getCmsTimestamps(final CMSSignedData signedData) {
		if (signedData == null) {
			return new ArrayList<>(0);
		}

		final List<AOTimestampInfo> ret = new ArrayList<>();

		final Iterator<SignerInformation> i = signedData.getSignerInfos().getSigners().iterator();
		while (i.hasNext()) {

			final SignerInformation signerInformation = i.next();
			final AttributeTable at = signerInformation.getUnsignedAttributes();

			if (at != null) {

				final Attribute att = at.get(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken);

				if (att != null) {

					// Tiene sello de tiempo
					final TimeStampToken tst;
					final X509Certificate cert;

					try {

						final ASN1Encodable dob = att.getAttrValues().getObjectAt(0);
				        final CMSSignedData sd = new CMSSignedData(dob.toASN1Primitive().getEncoded());
				        tst = new TimeStampToken(sd);

						final Collection<X509CertificateHolder> col = sd.getCertificates().getMatches(null);
						if (!col.isEmpty()) {
								final org.spongycastle.asn1.x509.Certificate c = ((X509CertificateHolder)col.toArray()[0]).toASN1Structure();
								cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
									new ByteArrayInputStream(
										c.getEncoded()
									)
								);
						}
						else {
							continue;
						}

					}
					catch (final Exception e) {
						LOGGER.log(
							Level.SEVERE,
							"Error extrayendo los sellos de tiempo de la firma CMS, se continuara con la siguiente: " + e, //$NON-NLS-1$
							e
						);
						continue;
					}

					ret.add(
						new AOTimestampInfo(
							cert,
							tst.getTimeStampInfo().getGenTime()
						)
					);

				}
			}

		}
		return ret;
	}

}
