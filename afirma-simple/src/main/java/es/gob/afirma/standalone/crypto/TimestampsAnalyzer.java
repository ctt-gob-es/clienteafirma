package es.gob.afirma.standalone.crypto;

import java.io.ByteArrayInputStream;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
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

import com.aowagie.text.exceptions.BadPasswordException;
import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.signers.AOTimestampInfo;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.PdfUtil;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Analizador de sellos de tiempo en firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TimestampsAnalyzer {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	private static final PdfName PDFNAME_ETSI_RFC3161 = new PdfName("ETSI.RFC3161"); //$NON-NLS-1$
	private static final PdfName PDFNAME_DOCTIMESTAMP = new PdfName("DocTimeStamp"); //$NON-NLS-1$

	/** Obtiene informaci&oacute;n de los sellos de tiempo de una firma.
	 * @param sign Firma.
	 * @return Informaci&oacute;n de los sellos de tiempo de la firma proporcionada. */
	public static List<AOTimestampInfo> getTimestamps(final byte[] sign) {
		if (sign == null) {
			return new ArrayList<>(0);
		}
		if (new AOPDFSigner().isSign(sign)) {
			return getPdfTimestamps(sign);
		}
		try {
			if (new AOCAdESSigner().isSign(sign) || new AOCMSSigner().isSign(sign)) {
				return getCmsTimestamps(sign);
			}
		}
		catch(final Exception e) {
			LOGGER.warning(
				"Error comprobando si la firma es CMS: " + e //$NON-NLS-1$
			);
		}
		return new ArrayList<>(0);
	}

	/** Obtiene informaci&oacute;n de los sellos de tiempo de una firma.
	 * @param sign Firma.
	 * @param params Par&aacute;metros de firma.
	 * @return Informaci&oacute;n de los sellos de tiempo de la firma proporcionada. */
	public static List<AOTimestampInfo> getTimestamps(final byte[] sign, final Properties params) {
		if (sign == null) {
			return new ArrayList<>(0);
		}
		if (new AOPDFSigner().isSign(sign, params)) {
			return getPdfTimestamps(sign, params);
		}
		try {
			if (new AOCAdESSigner().isSign(sign) || new AOCMSSigner().isSign(sign)) {
				return getCmsTimestamps(sign);
			}
		}
		catch(final Exception e) {
			LOGGER.warning(
				"Error comprobando si la firma es CMS: " + e //$NON-NLS-1$
			);
		}
		return new ArrayList<>(0);
	}

	private static List<AOTimestampInfo> getCmsTimestamps(final byte[] sign) {
		final CMSSignedData signedData;
		try {
			signedData = new CMSSignedData(sign);
		}
		catch (final CMSException e) {
			LOGGER.severe(
				"La firma proporcionada no es un SignedData compatible CMS, se devolvera una lista de sellos vacia: " + e //$NON-NLS-1$
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

						final Collection<?> col = sd.getCertificates().getMatches(null);
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
						LOGGER.warning(
							"Error extrayendo los sellos de tiempo de la firma CMS, se continuara con la siguiente: " + e //$NON-NLS-1$
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

	private static List<AOTimestampInfo> getPdfTimestamps(final byte[] sign) {
    	PdfReader pdfReader;
    	try {
    		pdfReader = new PdfReader(sign);
    	}
    	catch (final BadPasswordException e) {
    		try {
    			pdfReader = new PdfReader(
					sign,
					new String(
						AOUIFactory.getPassword(
							SimpleAfirmaMessages.getString("TimestampsAnalyzer.0"), //$NON-NLS-1$
							null
						)
					).getBytes()
				);
    		}
    		catch (final BadPasswordException e2) {
    			LOGGER.severe("La contrasena del PDF no es valida, se devolvera una lista de sellos vacia: " + e2); //$NON-NLS-1$
    			return new ArrayList<>(0);
    		}
    		catch (final Exception e3) {
    			LOGGER.severe("No se ha podido leer el PDF con contrasena, se devolvera una lista de sellos vacia: " + e3); //$NON-NLS-1$
    			return new ArrayList<>(0);
    		}
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se ha podido leer el PDF, se devolvera una lista de sellos vacia: " + e); //$NON-NLS-1$
    		return new ArrayList<>(0);
    	}

    	final AcroFields af;
    	try {
    		af = pdfReader.getAcroFields();
    	}
    	catch (final Exception e) {
    		LOGGER.severe(
				"No se ha podido obtener la informacion de los sellos del PDF, se devolvera una lista de sellos vacia: " + e //$NON-NLS-1$
			);
    		return new ArrayList<>(0);
    	}

    	final List<String> names = af.getSignatureNames();

    	final List<AOTimestampInfo> ret = new ArrayList<>();

    	for (final String signatureName : names) {
    		final PdfDictionary pdfDictionary = af.getSignatureDictionary(signatureName);

			final byte[] ts = pdfDictionary.getAsString(PdfName.CONTENTS).getOriginalBytes();

			final CMSSignedData signedData;
			try {
				signedData = new CMSSignedData(ts);
			}
			catch (final CMSException e) {
				LOGGER.severe(
					"La firma encontrada no es compatible CMS, se continua con las siguientes: " + e //$NON-NLS-1$
				);
				continue;
			}

			ret.addAll(getCmsTimestamps(signedData));

    		if (PDFNAME_ETSI_RFC3161.equals(pdfDictionary.get(PdfName.SUBFILTER)) || PDFNAME_DOCTIMESTAMP.equals(pdfDictionary.get(PdfName.SUBFILTER))) {

				final TimeStampToken tst;
				try {
					tst = new TimeStampToken(signedData);
				}
				catch (final Exception e) {
					LOGGER.warning(
						"El sello encontrado no es compatible, se continua con los siguientes: " + e //$NON-NLS-1$
					);
					continue;
				}

	    		final PdfPKCS7 pcks7;
	    		try {
	    			pcks7 = af.verifySignature(signatureName);
	    		}
	    		catch(final Exception e) {
	    			LOGGER.warning(
						"El PDF contiene una firma corrupta o con un formato desconocido (" + //$NON-NLS-1$
							signatureName +
								"), se continua con las siguientes si las hubiese: " + e //$NON-NLS-1$
					);
	    			continue;
	    		}

				ret.add(
					new AOTimestampInfo(
						pcks7.getSigningCertificate(),
						tst.getTimeStampInfo().getGenTime()
					)
				);

			}
    	}

    	return ret;
	}

	private static List<AOTimestampInfo> getPdfTimestamps(final byte[] sign, final Properties params) {
    	PdfReader pdfReader;
    	try {
    		final boolean headless = params != null
    				? Boolean.parseBoolean(params.getProperty(PdfExtraParams.HEADLESS))
    				: false;
    		pdfReader = PdfUtil.getPdfReader(sign, params, headless);
    	}
    	catch (final BadPasswordException e) {
    		try {
    			pdfReader = new PdfReader(
					sign,
					new String(
						AOUIFactory.getPassword(
							SimpleAfirmaMessages.getString("TimestampsAnalyzer.0"), //$NON-NLS-1$
							null
						)
					).getBytes()
				);
    		}
    		catch (final BadPasswordException e2) {
    			LOGGER.severe("La contrasena del PDF no es valida, se devolvera una lista de sellos vacia: " + e2); //$NON-NLS-1$
    			return new ArrayList<>(0);
    		}
    		catch (final Exception e3) {
    			LOGGER.severe("No se ha podido leer el PDF, se devolvera una lista de sellos vacia: " + e3); //$NON-NLS-1$
    			return new ArrayList<>(0);
    		}
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se ha podido leer el PDF, se devolvera una lista de sellos vacia: " + e); //$NON-NLS-1$
    		return new ArrayList<>(0);
    	}

    	final AcroFields af;
    	try {
    		af = pdfReader.getAcroFields();
    	}
    	catch (final Exception e) {
    		LOGGER.severe(
				"No se ha podido obtener la informacion de los sellos del PDF, se devolvera una lista de sellos vacia: " + e //$NON-NLS-1$
			);
    		return new ArrayList<>(0);
    	}

    	final List<String> names = af.getSignatureNames();

    	final List<AOTimestampInfo> ret = new ArrayList<>();

    	for (final String signatureName : names) {
    		final PdfDictionary pdfDictionary = af.getSignatureDictionary(signatureName);

			final byte[] ts = pdfDictionary.getAsString(PdfName.CONTENTS).getOriginalBytes();

			final CMSSignedData signedData;
			try {
				signedData = new CMSSignedData(ts);
			}
			catch (final CMSException e) {
				LOGGER.severe(
					"La firma encontrada no es compatible CMS, se continua con las siguientes: " + e //$NON-NLS-1$
				);
				continue;
			}

			ret.addAll(getCmsTimestamps(signedData));

    		if (PDFNAME_ETSI_RFC3161.equals(pdfDictionary.get(PdfName.SUBFILTER)) || PDFNAME_DOCTIMESTAMP.equals(pdfDictionary.get(PdfName.SUBFILTER))) {

				final TimeStampToken tst;
				try {
					tst = new TimeStampToken(signedData);
				}
				catch (final Exception e) {
					LOGGER.warning(
						"El sello encontrado no es compatible, se continua con los siguientes: " + e //$NON-NLS-1$
					);
					continue;
				}

	    		final PdfPKCS7 pcks7;
	    		try {
	    			pcks7 = af.verifySignature(signatureName);
	    		}
	    		catch(final Exception e) {
	    			LOGGER.warning(
						"El PDF contiene una firma corrupta o con un formato desconocido (" + //$NON-NLS-1$
							signatureName +
								"), se continua con las siguientes si las hubiese: " + e //$NON-NLS-1$
					);
	    			continue;
	    		}

				ret.add(
					new AOTimestampInfo(
						pcks7.getSigningCertificate(),
						tst.getTimeStampInfo().getGenTime()
					)
				);

			}
    	}

    	return ret;
	}

}
