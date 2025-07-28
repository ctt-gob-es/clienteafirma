package es.gob.afirma.signvalidation;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.CMSObjectIdentifiers;
import org.spongycastle.asn1.esf.ESFAttributes;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.cert.X509CRLHolder;
import org.spongycastle.cms.CMSSignedData;
import org.spongycastle.cms.SignerInformation;
import org.spongycastle.cms.SignerInformationStore;
import org.spongycastle.util.CollectionStore;

import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfReader;

public class SignatureFormatDetectorPadesCades implements ISignatureFormatDetector {

	/**
	 * Constant attribute that represents the OID of the
	 * <code>attribute-certificate-references</code> attribute.
	 */
	private static final ASN1ObjectIdentifier ID_ATTRIBUTE_CERTIFICATE_REFERENCES = new ASN1ObjectIdentifier(
			"1.2.840.113549.1.9.16.2.44"); //$NON-NLS-1$

	/**
	 * Constant attribute that represents the OID of the
	 * <code>attribute-revocation-references</code> attribute.
	 */
	private static final ASN1ObjectIdentifier ID_ATTRIBUTE_REVOCATION_REFERENCES = new ASN1ObjectIdentifier(
			"1.2.840.113549.1.9.16.2.45"); //$NON-NLS-1$

	/**
	 * Constant attribute that represents the OID of the
	 * <code>long-term-validation</code> attribute.
	 */
	private static final ASN1ObjectIdentifier ID_LONG_TERM_VALIDATION = new ASN1ObjectIdentifier("0.4.0.1733.2.2"); //$NON-NLS-1$

	/**
	 * Constant attribute that represents the OID of the
	 * <code>archive-time-stamp-v3</code> attribute.
	 */
	private static final ASN1ObjectIdentifier ID_ARCHIVE_TIME_STAMP_V3 = new ASN1ObjectIdentifier("0.4.0.1733.2.4"); //$NON-NLS-1$

	/**
	 * Constant attribute that represents the value to identify the <i>DSS</i> entry
	 * in a PDF's Catalog.
	 */
	public static final PdfName DSS_DICTIONARY_NAME = new PdfName("DSS"); //$NON-NLS-1$

	/**
	 * Constant attribute that represents the value for the key <i>SubFilter</i> of
	 * the signature dictionary for a PAdES Enhanced signature.
	 */
	public static final PdfName CADES_SUBFILTER_VALUE = new PdfName("ETSI.CAdES.detached"); //$NON-NLS-1$

	/**
	 * Constant attribute that represents the value for the key <i>SubFilter</i> of
	 * a Document Time-stamp dictionary.
	 */
	public static final PdfName TST_SUBFILTER_VALUE = new PdfName("ETSI.RFC3161"); //$NON-NLS-1$

	/**
	 * Constant attribute that represents the value to identify the type of a
	 * Document Time-stamp.
	 */
	public static final PdfName DOC_TIME_STAMP_DICTIONARY_NAME = new PdfName("DocTimeStamp"); //$NON-NLS-1$

	/**
	 * Constant attribute that identifies CAdES-BES signature format.
	 */
	static String FORMAT_CADES_BES = "CAdES-BES"; //$NON-NLS-1$

	/**
	 * Constant attribute that identifies CMS signature format.
	 */
	static String FORMAT_CMS = "CMS"; //$NON-NLS-1$

	/**
	 * Constant attribute that identifies CMS-T signature format.
	 */
	static String FORMAT_CMS_T = "CMS-T"; //$NON-NLS-1$

	/**
	 * Method that indicates if a signature dictionary refers to a PAdES B-Level
	 * profile (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @return a boolean that indicates if a signature dictionary refers to a PAdES
	 *         B-Level profile (true) or not (false).
	 */
	private static boolean isPAdESBLevel(final PDFSignatureDictionary signatureDictionary) {
		/*
		 * Consideramos que una firma es PAdES B-Level si: > El nucleo de firma CAdES
		 * contiene el elemento SignedData.certificates con, al menos, un certificado
		 * (el firmante). > Contiene la entrada /M en el diccionario de firma. > La
		 * entrada /SubFilter del diccionario de firma posee el valor
		 * 'ETSI.CAdES.detached'.
		 */
		final PdfDictionary pdfDic = signatureDictionary.getDictionary();

		// Accedemos a la entrada /SubFilter
		final PdfName subFilterValue = signatureDictionary.getDictionary().getAsName(PdfName.SUBFILTER);

		// Comprobamos que la entrada /SubFilter posee el valor
		// 'ETSI.CAdES.detached' y que se encuentra la entrada /M en el
		// diccionario de firma
		if (subFilterValue.equals(CADES_SUBFILTER_VALUE) && pdfDic.get(PdfName.M) != null) {
			try {
				// Obtenemos los datos firmados
				final CMSSignedData signedData = getCMSSignature(signatureDictionary);

				// Accedemos al elemento SignedData.certificates y comprobamos
				// que posee al menos un elemento
				final Iterator<SignerInformation> it = signedData.getSignerInfos().getSigners().iterator();
				while (it.hasNext()) {
					final SignerInformation si = it.next();
					final AttributeTable signedAttrs = si.getSignedAttributes();
					if(signedAttrs.get(PKCSObjectIdentifiers.id_aa_signingCertificate) != null) {
						return true;
					}
				}
			} catch (final Exception e) {
				return false;
			}
		}
		return false;
	}

	/**
	 * Method that indicates if a signature dictionary refers to a PAdES B-B-Level
	 * profile (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @return a boolean that indicates if a signature dictionary refers to a PAdES
	 *         B-B-Level profile (true) or not (false).
	 */
	private static boolean isPAdESBBLevel(final PDFSignatureDictionary signatureDictionary) {
		/*
		 * Consideramos que una firma es PAdES B-Level si: > El nucleo de firma CAdES
		 * contiene el elemento SignedData.certificates con, al menos, un certificado
		 * (el firmante). > Contiene la entrada /M en el diccionario de firma. > La
		 * entrada /SubFilter del diccionario de firma posee el valor
		 * 'ETSI.CAdES.detached'.
		 */
		final PdfDictionary pdfDic = signatureDictionary.getDictionary();

		// Accedemos a la entrada /SubFilter
		final PdfName subFilterValue = signatureDictionary.getDictionary().getAsName(PdfName.SUBFILTER);

		// Comprobamos que la entrada /SubFilter posee el valor
		// 'ETSI.CAdES.detached' y que se encuentra la entrada /M en el
		// diccionario de firma
		if (subFilterValue.equals(CADES_SUBFILTER_VALUE) && pdfDic.get(PdfName.M) != null) {
			try {
				// Obtenemos los datos firmados
				final CMSSignedData signedData = getCMSSignature(signatureDictionary);

				// Accedemos al elemento SignedData.certificates y comprobamos
				// que posee al menos un elemento
				final Iterator<SignerInformation> it = signedData.getSignerInfos().getSigners().iterator();
				while (it.hasNext()) {
					final SignerInformation si = it.next();
					final AttributeTable signedAttrs = si.getSignedAttributes();
					if(signedAttrs.get(PKCSObjectIdentifiers.id_aa_signingCertificateV2) != null) {
						return true;
					}
				}
			} catch (final Exception e) {
				return false;
			}
		}
		return false;
	}

	/**
	 * Method that indicates if a signature dictionary refers to a PAdES T-Level
	 * profile (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @param reader              Parameter that allows to read the PDF document.
	 * @return a boolean that indicates if a signature dictionary refers to a PAdES
	 *         T-Level profile (true) or not (false).
	 */
	private static boolean isPAdESTLevel(final PDFSignatureDictionary signatureDictionary, final PdfReader reader) {
		/*
		 * Consideramos que una firma es PAdES T-Level si: > El nucleo de firma CAdES
		 * contiene un unico elemento signature-time-stamp y el documento PDF no
		 * contiene ningun diccionario de sello de tiempo o > El nucleo de firma CAdES
		 * no contiene ningun elemento signature-time-stamp y el documento PDF contiene
		 * un unico diccionario de sello de tiempo
		 */
		try {
			int signatureTimeStampNumber = 0;
			int documentTimeStampDictionariesNumber = 0;

			// Obtenemos los datos firmados
			final CMSSignedData signedData = getCMSSignature(signatureDictionary);
			if (!signedData.getCertificates().getMatches(null).isEmpty()) {
				// Obtenemos la lista con todos los firmantes contenidos en la
				// firma
				final SignerInformationStore signerInformationStore = signedData.getSignerInfos();
				final List<SignerInformation> listSignersSignature = (List<SignerInformation>) signerInformationStore
						.getSigners();

				// Accedemos al primer firmante
				final SignerInformation signerInfo = listSignersSignature.get(0);

				// Obtenemos el numero de atributos signature-time-stamp
				if (signerInfo.getUnsignedAttributes() != null && signerInfo.getUnsignedAttributes()
						.getAll(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken) != null) {
					signatureTimeStampNumber = signerInfo.getUnsignedAttributes()
							.getAll(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken).size();
				}

				// Obtenemos el numero de diccionarios de sello de tiempo
				documentTimeStampDictionariesNumber = countDocumentTimeStampDictionaries(reader);

				if (signatureTimeStampNumber == 1 && documentTimeStampDictionariesNumber == 0
						|| signatureTimeStampNumber == 0 && documentTimeStampDictionariesNumber == 1) {
					return true;
				}
			}
			return false;
		} catch (final Exception e) {
			return false;
		}
	}

	/**
	 * Method that indicates if a signature dictionary refers to a PAdES LT-Level
	 * profile (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @param reader              Parameter that allows to read the PDF document.
	 * @return a boolean that indicates if a signature dictionary refers to a PAdES
	 *         LT-Level profile (true) or not (false).
	 */
	private static boolean isPAdESLTLevel(final PDFSignatureDictionary signatureDictionary, final PdfReader reader) {
		/*
		 * Consideramos que una firma es PAdES LT-Level si: > El documento PDF contiene
		 * al menos un diccionario DSS > El nucleo de firma CAdES contiene un unico
		 * elemento signature-time-stamp y el documento PDF no contiene ningun
		 * diccionario de sello de tiempo o > El nucleo de firma CAdES no contiene
		 * ningun elemento signature-time-stamp y el documento PDF contiene un unico
		 * diccionario de sello de tiempo
		 */
		if (isPAdESTLevel(signatureDictionary, reader)) {
			if (reader.getCatalog().get(DSS_DICTIONARY_NAME) != null) {
				return true;
			}
			// Instanciamos un objeto para leer las firmas
			final AcroFields af = reader.getAcroFields();

			// Obtenemos la lista de firmas del documento PDF
			final List<String> listSignatures = af.getSignatureNames();

			for (int i = 0; i < listSignatures.size(); i++) {
				// Metemos en una variable el nombre de la firma
				final String signatureName = listSignatures.get(i);

				try {
					// Obtenemos el PdfReader asociado a la revision que
					// estamos procesando
					final PdfReader revisionReader = new PdfReader(af.extractRevision(signatureName));

					if (revisionReader.getCatalog().getAsDict(DSS_DICTIONARY_NAME) != null) {
						return true;
					}
				} catch (final IOException e) {
					return false;
				}
			}
		}
		return false;
	}

	/**
	 * Method that indicates if a signature dictionary refers to a PAdES LTA-Level
	 * profile (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @param reader              Parameter that allows to read the PDF document.
	 * @return a boolean that indicates if a signature dictionary refers to a PAdES
	 *         LTA-Level profile (true) or not (false).
	 */
	private static boolean isPAdESLTALevel(final PDFSignatureDictionary signatureDictionary, final PdfReader reader) {
		/*
		 * Consideramos que una firma es PAdES LTA-Level si: > El documento PDF contiene
		 * al menos un diccionario DSS > El documento PDF contiene al menos un
		 * diccionario de sello de tiempo > El nucleo de firma CAdES contiene al menos
		 * un atributo signature-time-stamp
		 */
		int signatureTimeStampNumber = 0;
		int documentTimeStampDictionariesNumber = 0;

		try {

			// Obtenemos los datos firmados
			final CMSSignedData signedData = getCMSSignature(signatureDictionary);

			if (!signedData.getCertificates().getMatches(null).isEmpty()) {
				// Obtenemos la lista con todos los firmantes contenidos en la
				// firma
				final SignerInformationStore signerInformationStore = signedData.getSignerInfos();
				final List<SignerInformation> listSignersSignature = (List<SignerInformation>) signerInformationStore
						.getSigners();

				// Accedemos al primer firmante
				final SignerInformation signerInfo = listSignersSignature.get(0);

				// Obtenemos el numero de atributos signature-time-stamp
				if (signerInfo.getUnsignedAttributes() != null && signerInfo.getUnsignedAttributes()
						.getAll(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken) != null) {
					signatureTimeStampNumber = signerInfo.getUnsignedAttributes()
							.getAll(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken).size();
				}

				// Obtenemos el numero de diccionarios de sello de tiempo
				documentTimeStampDictionariesNumber = countDocumentTimeStampDictionaries(reader);

				if (signatureTimeStampNumber > 1 || documentTimeStampDictionariesNumber > 1) {
					if (reader.getCatalog().get(DSS_DICTIONARY_NAME) != null) {
						return true;
					}
					// Instanciamos un objeto para leer las firmas
					final AcroFields af = reader.getAcroFields();

					// Obtenemos la lista de firmas del documento PDF
					final List<String> listSignatures = af.getSignatureNames();

					for (int i = 0; i < listSignatures.size(); i++) {
						// Metemos en una variable el nombre de la firma
						final String signatureName = listSignatures.get(i);

						// Obtenemos el PdfReader asociado a la revision que
						// estamos procesando
						final PdfReader revisionReader = new PdfReader(af.extractRevision(signatureName));

						if (revisionReader.getCatalog().getAsDict(DSS_DICTIONARY_NAME) != null) {
							return true;
						}
					}
				}
			}
			return false;
		} catch (final Exception e) {
			return false;
		}
	}

	/**
	 * Method that obtains the number of Document Time-stamp dictionaries included
	 * into the PDF document.
	 *
	 * @param reader Parameter that allows to read the PDF document.
	 * @return the number of Document Time-stamp dictionaries included into the PDF
	 *         document.
	 */
	private static int countDocumentTimeStampDictionaries(final PdfReader reader) {
		int documentTimeStampDictionaries = 0;

		// Instanciamos un objeto para leer las firmas
		final AcroFields af = reader.getAcroFields();

		// Obtenemos la lista de firmas del documento PDF
		final List<String> listSignatures = af.getSignatureNames();

		for (int i = 0; i < listSignatures.size(); i++) {
			// Metemos en una variable el nombre de la firma
			final String signatureName = listSignatures.get(i);

			// Obtenemos el diccionario de firma asociado
			final PdfDictionary signatureDictionary = af.getSignatureDictionary(signatureName);

			// Determinamos el tipo de diccionario obtenido
			String pdfType = null;
			if (signatureDictionary.get(PdfName.TYPE) != null) {
				pdfType = signatureDictionary.get(PdfName.TYPE).toString();
			}

			final String pdfSubFilter = signatureDictionary.get(PdfName.SUBFILTER).toString();

			// Comprobamos si existe al menos un diccionario de firma de
			// tipo Document Time-stamp
			if (pdfSubFilter.equalsIgnoreCase(TST_SUBFILTER_VALUE.toString())
					&& (pdfType == null || pdfType.equals(DOC_TIME_STAMP_DICTIONARY_NAME.toString()))) {
				documentTimeStampDictionaries++;
			}
		}
		return documentTimeStampDictionaries;
	}

	/**
	 * Method that obtains the concrete signature format of a PDF document.
	 *
	 * @param pdfDocument Parameter that represents the PDF document.
	 * @return the signature format. The format will have one of these values:
	 *         <ul>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_LTA_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_LT_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_T_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_B_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_LTV}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_EPES}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_BES}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_BASIC}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PDF}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_UNRECOGNIZED}.</li>
	 *         </ul>
	 */
	public static String resolvePDFFormat(final byte[] pdfDocument) {
		// Por defecto establecemos el formato como no reconocido
		String format = FORMAT_UNRECOGNIZED;

		try {
			// Leemos el documento PDF
			final PdfReader reader = new PdfReader(pdfDocument);

			// El formato de firma sera determinado por la firma con mayor
			// revision
			final PDFSignatureDictionary signatureDictionary = obtainLatestSignatureFromPDF(reader);

			if (isPAdESBLevel(signatureDictionary)) {
				format = FORMAT_PADES_B_LEVEL;
				if (isPAdESLTALevel(signatureDictionary, reader)) {
					format = FORMAT_PADES_LTA_LEVEL;
				} else if (isPAdESLTLevel(signatureDictionary, reader)) {
					format = FORMAT_PADES_LT_LEVEL;
				} else if (isPAdESTLevel(signatureDictionary, reader)) {
					format = FORMAT_PADES_T_LEVEL;
				}
			}
			else if (isPAdESBBLevel(signatureDictionary)) {
				format = FORMAT_PADES_B_B_LEVEL;
				if (isPAdESLTALevel(signatureDictionary, reader)) {
					format = FORMAT_PADES_LTA_LEVEL;
				} else if (isPAdESLTLevel(signatureDictionary, reader)) {
					format = FORMAT_PADES_LT_LEVEL;
				} else if (isPAdESTLevel(signatureDictionary, reader)) {
					format = FORMAT_PADES_T_LEVEL;
				}
			}
			else{
				return getFormatOfPAdESSignature(signatureDictionary, reader);
			}
		} catch (final Exception e) {
			format = FORMAT_UNRECOGNIZED;
		}
		return format;
	}

	/**
	 * Method that obtains the concrete signature format of a PDF document when the
	 * concrete signature format hasn't Baseline form.
	 *
	 * @param signatureDictionary Parameter that represents the most recent
	 *                            signature dictionary.
	 * @param reader              Parameter that allows to read the PDF document.
	 * @return the signature format. The format will have one of these values:
	 *         <ul>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_LTV}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_EPES}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_BES}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PADES_BASIC}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_PDF}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_UNRECOGNIZED}.</li>
	 *         </ul>
	 */
	private static String getFormatOfPAdESSignature(final PDFSignatureDictionary signatureDictionary,
			final PdfReader reader) {
		// Por defecto establecemos el formato como no reconocido
		String format = FORMAT_UNRECOGNIZED;

		if (isPAdESLTV(reader)) {
			format = FORMAT_PADES_LTV;
		} else {
			// Comprobamos el formato especifico de la firma
			if (isPAdESEPES(signatureDictionary)) {
				format = FORMAT_PADES_EPES;
			} else if (isPAdESBES(signatureDictionary)) {
				format = FORMAT_PADES_BES;
			} else if (isPAdESBasic(signatureDictionary)) {
				format = FORMAT_PADES_BASIC;
			} else if (isPDF(signatureDictionary)) {
				format = FORMAT_PDF;
			}
		}

		return format;
	}

	/**
	 * Method that indicates whether a signature dictionary has PAdES-EPES signature
	 * format (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @return a boolean that indicates whether a signature dictionary has
	 *         PAdES-EPES signature format (true) or not (false).
	 */
	public static boolean isPAdESEPES(final PDFSignatureDictionary signatureDictionary) {
		// Segun ETSI TS 102 778-3 una firma PAdES-EPES debe tener
		// el valor 'ETSI.CAdES.detached' en el campo /SubFilter
		// y politica de firma asociada, es decir, el atributo
		// signature-policy-identifier debe ser un atributo firmado.
		final PdfName subFilterValue = signatureDictionary.getDictionary().getAsName(PdfName.SUBFILTER);
		if (subFilterValue.equals(CADES_SUBFILTER_VALUE)) {
			try {
				// Obtenemos los datos firmados
				final CMSSignedData signedData = getCMSSignature(signatureDictionary);

				// Obtenemos la lista con todos los firmantes contenidos en la
				// firma
				final SignerInformationStore signerInformationStore = signedData.getSignerInfos();
				final List<SignerInformation> listSignersSignature = (List<SignerInformation>) signerInformationStore
						.getSigners();

				// Comprobamos si la firma contenida es CAdES-EPES
				return isCAdESEPES(listSignersSignature);
			} catch (final Exception e) {
				return false;
			}
		}
		return false;
	}

	/**
	 * Method that indicates whether a signature dictionary has PAdES-BES signature
	 * format (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @return a boolean that indicates whether a signature dictionary has PAdES-BES
	 *         signature format (true) or not (false).
	 */
	public static boolean isPAdESBES(final PDFSignatureDictionary signatureDictionary) {
		// Segun ETSI TS 102 778-3 una firma PAdES-BES debe tener
		// el valor 'ETSI.CAdES.detached' en el campo /SubFilter
		// y no tener politica de firma asociada, es decir, el atributo
		// signature-policy-identifier no debe ser un atributo firmado.
		final PdfName subFilterValue = signatureDictionary.getDictionary().getAsName(PdfName.SUBFILTER);
		if (subFilterValue.equals(CADES_SUBFILTER_VALUE)) {
			try {
				// Obtenemos los datos firmados
				final CMSSignedData signedData = getCMSSignature(signatureDictionary);

				// Obtenemos la lista con todos los firmantes contenidos en la
				// firma
				final SignerInformationStore signerInformationStore = signedData.getSignerInfos();
				final List<SignerInformation> listSignersSignature = (List<SignerInformation>) signerInformationStore
						.getSigners();

				// Accedemos al primer firmante
				final SignerInformation signerInfo = listSignersSignature.get(0);

				// Una firma PAdES-BES no tiene politica de firma asociada, es
				// decir, ela tributo signature-policy-identifier no debe sesr
				// un atributo firmado.
				if (!hasSignaturePolicyIdentifier(signerInfo)) {
					return true;
				}

			} catch (final Exception e) {
				return false;
			}
		}
		return false;
	}

	/**
	 * Method that indicates whether a signature dictionary has PAdES-Basic
	 * signature format (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @return a boolean that indicates whether a signature dictionary has
	 *         PAdES-Basic signature format (true) or not (false).
	 */
	public static boolean isPAdESBasic(final PDFSignatureDictionary signatureDictionary) {
		// Segun ETSI TS 102 778-2 una firma PAdES basica debe haber sido
		// codificada en PKCS#7
		// y la clave /SubFilter solo puede tener 2 valores:
		// 'adbe.pkcs7.detached' o 'adbe.pkcs7.sha1'
		final PdfName subFilterValue = (PdfName) signatureDictionary.getDictionary().get(PdfName.SUBFILTER);
		if (subFilterValue.equals(PdfName.ADBE_PKCS7_DETACHED) || subFilterValue.equals(PdfName.ADBE_PKCS7_SHA1)) {
			return true;
		}
		return false;
	}

	/**
	 * Method that indicates whether a signature dictionary has PDF signature format
	 * (true) or not (false).
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @return a boolean that indicates whether a signature dictionary has PDF
	 *         signature format (true) or not (false).
	 */
	public static boolean isPDF(final PDFSignatureDictionary signatureDictionary) {
		// Consideramos formato PDF si la clave /SubFilter del diccionario de
		// firma no posee algun valor valido para PAdES-BES, PAdES-EPES
		// o PAdES-Basic
		final PdfName subFilterValue = (PdfName) signatureDictionary.getDictionary().get(PdfName.SUBFILTER);
		if (!subFilterValue.equals(PdfName.ADBE_PKCS7_DETACHED) && !subFilterValue.equals(PdfName.ADBE_PKCS7_SHA1)
				&& !subFilterValue.equals(CADES_SUBFILTER_VALUE)) {
			return true;
		}
		return false;
	}

	/**
	 * Method that indicates whether a PDF document has PAdES-LTV signature format
	 * (true) or not (false).
	 *
	 * @param reader Parameter that allows to access to all of the elements of the
	 *               PDF document.
	 * @return a boolean that indicates whether a PDF document has PAdES-LTV
	 *         signature format (true) or not (false).
	 */
	private static boolean isPAdESLTV(final PdfReader reader) {
		// Un documento PDF tendra el formato de firma PAdES-LTV si posee un
		// diccionario DSS y/o posee al menos un diccionario de firma de tipo
		// Document Time-stamp.
		if (reader.getCatalog().get(DSS_DICTIONARY_NAME) != null) {
			return true;
		}
		return countDocumentTimeStampDictionaries(reader) > 0;
	}

	/**
	 * Method that obtains the <code>SignedData</code> contained inside of a
	 * signature dictionary of a PDF document.
	 *
	 * @param signatureDictionary Parameter that represents the signature
	 *                            dictionary.
	 * @return an object that represents the <code>SignedData</code>.
	 * @throws Exception If the method fails.
	 */
	public static CMSSignedData getCMSSignature(final PDFSignatureDictionary signatureDictionary) throws Exception {

		// Metemos en una variable el contenido de la clave
		// /Contents, o
		// lo que es lo mismo, la firma
		final byte[] contents = signatureDictionary.getDictionary().getAsString(PdfName.CONTENTS).getOriginalBytes();
		// Obtenemos los datos firmados
		CMSSignedData signedData = null;
		try {
			signedData = new CMSSignedData(contents);
			// Comprobamos que la firma tiene al menos un firmante
			if (signedData.getSignerInfos().getSigners().size() == 0) {
				throw new Exception("No existen firmantes en el documento"); //$NON-NLS-1$
			}
		} catch (final Exception e) {
			throw e;
		}

		// Devolvemos los datos firmados
		return signedData;
	}

	/**
	 * Method that obtains the signature dictionary with major review.
	 *
	 * @param reader Parameter that represents the reader for the PDF document.
	 * @return the signature dictionary with major review.
	 */
	public static PDFSignatureDictionary obtainLatestSignatureFromPDF(final PdfReader reader) {
		// Instanciamos la variable a devolver
		PDFSignatureDictionary dictionary = null;
		try {
			// Instanciamos un contador de revisión
			int revision = -1;
			// Instanciamos un objeto para leer las firmas
			final AcroFields af = reader.getAcroFields();
			// Obtenemos la lista de firmas del documento PDF
			final List<String> listSignatures = af.getSignatureNames();
			// Recorremos la lista de firmas obtenidas
			for (int i = 0; i < listSignatures.size(); i++) {
				// Metemos en una variable el nombre de la firma
				final String signatureName = listSignatures.get(i);
				// Obtenemos el diccionario de firma asociado
				final PdfDictionary signatureDictionary = af.getSignatureDictionary(signatureName);
				// Determinamos el tipo de diccionario obtenido
				String pdfType = null;
				if (signatureDictionary.get(PdfName.TYPE) != null) {
					pdfType = signatureDictionary.get(PdfName.TYPE).toString();
				}
				final String pdfSubFilter = signatureDictionary.get(PdfName.SUBFILTER).toString();
				// Si el tipo de diccionario obtenido es un diccionario de firma
				// y
				// no un diccionario de tipo Document Time-stamp
				if (!pdfSubFilter.equalsIgnoreCase(new PdfName("ETSI.RFC3161").toString()) //$NON-NLS-1$
						&& (pdfType == null || pdfType.equalsIgnoreCase(PdfName.SIG.toString()))) {
					// Comparamos el número de revisión de la firma con el que
					// tenemos, si es mayor, actualizamos variables
					final int actuallyRevision = af.getRevision(signatureName);
					if (actuallyRevision > revision) {
						revision = actuallyRevision;
						dictionary = new PDFSignatureDictionary(actuallyRevision, signatureDictionary, signatureName);
					}
				}
			}

		} catch (final Exception e) {
			throw e;
		}
		return dictionary;
	}

	/**
	 * Method that checks whether an ASN.1 signature has CAdES-EPES format.
	 *
	 * @param listSignersSignature Parameter that represents the signers list of the
	 *                             signature.
	 * @return a boolean that indicates whether an ASN.1 signature has CAdES-EPES
	 *         format.
	 */
	private static boolean isCAdESEPES(final List<SignerInformation> listSignersSignature) {
		if (listSignersSignature != null && listSignersSignature.size() > 0) {
			// Recorremos la lista de firmantes
			for (final SignerInformation signerInformation : listSignersSignature) {
				// Comprobamos si el firmante tiene formato CAdES-EPES
				if (isCAdESEPES(signerInformation)) {
					return true;
				}
				// Si el firmante posee contrafirmas comprobamos si alguna de
				// ellas es CAdES-EPES
				final SignerInformationStore counterSignatures = signerInformation.getCounterSignatures();
				if (counterSignatures != null && counterSignatures.size() > 0
						&& isCAdESEPES((List<SignerInformation>) counterSignatures.getSigners())) {
					return true;
				}
			}
			return false;
		}
		return false;
	}

	/**
	 * Method that checks whether a signer has CAdES-EPES format.
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @return a boolean that indicates whether the signer has CAdES-EPES format.
	 */
	private static boolean isCAdESEPES(final SignerInformation signerInformation) {
		// Accedemos al conjunto de atributos firmados
		final AttributeTable signedAttrs = signerInformation.getSignedAttributes();
		// Se considera una firma con formato CAdES-EPES si posee el
		// atributo firmado
		// id-aa-sigPolicyId
		if (signedAttrs != null && signedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId) != null) {
			return true;
		}
		return false;
	}

	/**
	 * Method that indicates whether a signature is ASN.1 (true) or not (false).
	 *
	 * @param signature Parameter that represents the signature to check.
	 * @return a boolean that indicates whether a signature is ASN.1 (true) or not
	 *         (false).
	 */
	public static boolean isASN1Format(final byte[] signature) {
		try {
			final CMSSignedData signedData = new CMSSignedData(signature);
			signedData.getSignedContent().getContentType();
			if (signedData.getSignedContent() != null
					&& !CMSObjectIdentifiers.signedData.equals(signedData.getSignedContent().getContentType())) {
				// Es una estructura ASN.1, pero no es una firma
				return false;
			}
			return true;
		} catch (final Exception e) {
			return false;
		}
	}

	/**
	 * Method that obtains the concrete format of an ASN.1 signature.
	 * @param signedData ASN.1 signature.
	 * @param signature Parameter that represents the ASN.1 signature.
	 * @return the format of the signature. The format will have one of these
	 *         values:
	 *         <ul>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_LTA_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_LT_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_T_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_B_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_A}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_XL2}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_XL1}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_X2}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_X1}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_T}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_C}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_BES}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_EPES}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CMS}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CMS_T}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_UNRECOGNIZED}.</li>
	 *         </ul>
	 */
	public static String resolveASN1Format(final CMSSignedData signedData, final SignerInformation signature) {
		// Inicialmente definidos que el formato no está reconocido
		String format = FORMAT_UNRECOGNIZED;
		try {
			// Comprobamos si la firma es CMS
			if (isCMS(signature)) {
				format = FORMAT_CMS;
				// Comprobamos si la firma contiene sello de tiempo, y por lo
				// tanto, si es CMS-T
				if (isCAdEST(signature)) {
					format = FORMAT_CMS_T;
				}
			}
			// Comprobamos si la firma es CAdES-EPES
			else if (isCAdESEPES(signature)) {
				format = resolveFormatOfCAdESEPESSignature(signedData, signature);
			}
			// Comprobamos si la firma es CAdES-BES
			else if (isCAdESBES(signature)) {
				// Establecemos el formato a CAdES-EPES
				format = FORMAT_CADES_BES;
				// Comprobamos si la firma es CAdES B-Level, esto es, si tiene
				// el atributo firmado signing-time
				if (isCAdESBLevel(signature)) {
					// Establecemos el formato a CAdES B-Level
					format = FORMAT_CADES_B_LEVEL;
					// Comprobamos si la firma posee signature-time-stamp en
					// cuyo caso será CAdES T-Level
					if (isCAdEST(signature)) {
						// Establecemos el formato a CAdES T-Level
						format = FORMAT_CADES_T_LEVEL;
						// Comprobamos si la firma es LT-Level
						if (isCAdESLTLevel(signature, signedData)) {
							format = FORMAT_CADES_LT_LEVEL;
							// Comprobamos si la firma es LTA-Level
							if (isCAdESLTALevel(signature)) {
								format = FORMAT_CADES_LTA_LEVEL;
							}
						} else {
							format = resolveCAdESNoBaselineFormat(format, signature);
						}
					}
					// Si la firma no es CAdES B-Level
					else {
						// Comprobamos si la firma es CAdES-T, CAdES-C, CAdES-X1,
						// CAdES-X2, CAdES-XL1, CAdES-XL2 o CAdES-A
						format = resolveCAdESNoBaselineFormat(format, signature);
					}
				}
			}
			else if (isCAdESBBLevel(signature)) {
				format = FORMAT_CADES_B_B_LEVEL;
				if (isCAdEST(signature)) {
					// Establecemos el formato a CAdES T-Level
					format = FORMAT_CADES_T_LEVEL;
					// Comprobamos si la firma es LT-Level
					if (isCAdESLTLevel(signature, signedData)) {
						format = FORMAT_CADES_LT_LEVEL;
						// Comprobamos si la firma es LTA-Level
						if (isCAdESLTALevel(signature)) {
							format = FORMAT_CADES_LTA_LEVEL;
						}
					} else {
						format = resolveCAdESNoBaselineFormat(format, signature);
					}
				}
				// Si la firma no es CAdES B-Level
				else {
					// Comprobamos si la firma es CAdES-T, CAdES-C, CAdES-X1,
					// CAdES-X2, CAdES-XL1, CAdES-XL2 o CAdES-A
					format = resolveCAdESNoBaselineFormat(format, signature);
				}
			}
		} catch (final Exception e) {
			format = FORMAT_UNRECOGNIZED;
		}
		return format;
	}

	/**
	 * Method that checks whether a signer has CMS format.
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @return a boolean that indicates whether the signer has CMS format.
	 */
	private static boolean isCMS(final SignerInformation signerInformation) {
		// Accedemos al conjunto de atributos firmados
		final AttributeTable signedAttrs = signerInformation.getSignedAttributes();
		// Se considera una firma con formato CMS si no posee ningún
		// atributo firmado
		// id_aa_signingCertificate, id_aa_signingCertificateV2, ni
		// id_aa_ets_otherSigCert
		return signedAttrs.get(PKCSObjectIdentifiers.id_aa_signingCertificate) == null
				&& signedAttrs.get(PKCSObjectIdentifiers.id_aa_signingCertificateV2) == null
				&& signedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_otherSigCert) == null;
	}

	/**
	 * Method that checks whether a signer has CAdES-T format.
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @return a boolean that indicates whether the signer has CAdES-T format.
	 */
	private static boolean isCAdEST(final SignerInformation signerInformation) {
		// Accedemos al conjunto de atributos no firmados
		final AttributeTable unsignedAttrs = signerInformation.getUnsignedAttributes();
		// Se considera una firma con formato CAdES-T si posee el
		// atributo no firmado
		// id-aa-timeStampToken
		if (unsignedAttrs != null && unsignedAttrs.get(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken) != null) {
			return true;
		}
		return false;
	}

	/**
	 * Method that obtains the concrete format of an ASN.1 signature which has, al
	 * least, CAdES-EPES form.
	 *
	 * @param signedData           Parameter that represents the signed data.
	 * @param listSignersSignature Parameter that represents the list of signers of
	 *                             the signature.
	 * @return the format of the signature. The format will have one of these
	 *         values:
	 *         <ul>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_LTA_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_LT_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_T_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_B_LEVEL}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_A}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_XL2}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_XL1}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_X2}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_X1}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_T}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_C}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_EPES}.</li>
	 *         </ul>
	 */
	private static String resolveFormatOfCAdESEPESSignature(final CMSSignedData signedData,
			final SignerInformation signature) {
		// Establecemos el formato a CAdES-EPES
		String format = FORMAT_CADES_EPES;
		// Comprobamos si la firma es CAdES B-Level, esto es, si tiene
		// el atributo firmado signing-time
		if (isCAdESBLevel(signature)) {
			// Establecemos el formato a CAdES B-Level
			format = FORMAT_CADES_B_LEVEL;
			// Comprobamos si la firma posee signature-time-stamp en
			// cuyo caso será CAdES T-Level
			if (isCAdEST(signature)) {
				// Establecemos el formato a CAdES T-Level
				format = FORMAT_CADES_T_LEVEL;
				// Comprobamos si la firma es LT-Level
				if (isCAdESLTLevel(signature, signedData)) {
					format = FORMAT_CADES_LT_LEVEL;
					// Comprobamos si la firma es LTA-Level
					if (isCAdESLTALevel(signature)) {
						format = FORMAT_CADES_LTA_LEVEL;
					}
				}
			}
		}
		// Si la firma no es CAdES B-Level
		else {
			// Comprobamos si la firma es CAdES-T, CAdES-C, CAdES-X1,
			// CAdES-X2, CAdES-XL1, CAdES-XL2 o CAdES-A
			format = resolveCAdESNoBaselineFormat(format, signature);
		}
		return format;
	}

	/**
	 * Method that obtains the format of a CAdES signature without Baseline form.
	 *
	 * @param temporalFormat       Parameter that represents the current format of
	 *                             the CAdES signature. It must have one of these
	 *                             values:
	 *                             <ul>
	 *                             <li>{@link ISignatureFormatDetector#FORMAT_CADES_BES}.</li>
	 *                             <li>{@link ISignatureFormatDetector#FORMAT_CADES_EPES}.</li>
	 *                             </ul>
	 * @param listSignersSignature Parameter that represents the signers list of the
	 *                             signature.
	 * @return the format of the CAdES signature without Baseline form. The format
	 *         will have one of these values:
	 *         <ul>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_A}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_XL2}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_XL1}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_X2}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_X1}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_T}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_C}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_BES}.</li>
	 *         <li>{@link ISignatureFormatDetector#FORMAT_CADES_EPES}.</li>
	 *         </ul>
	 */
	private static String resolveCAdESNoBaselineFormat(final String temporalFormat,
			final SignerInformation signature) {
		// Primero establecemos el formato a devolver como el formato temporal
		// que posee la firma
		String format = temporalFormat;

		// Si la firma no es CAdES-T comprobamos si pudiera serlo
		if (!format.equals(FORMAT_CADES_T_LEVEL) && isCAdEST(signature)) {
			format = FORMAT_CADES_T;
		}

		return format;
	}


	/**
	 * Method that checks whether an ASN.1 signature has
	 * <code>SignaturePolicyIdentifier</code> element.
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @return a boolean that indicates whether the ASN.1 signature has
	 *         <code>SignaturePolicyIdentifier</code> element (true) or not (false).
	 */
	public static boolean hasSignaturePolicyIdentifier(final SignerInformation signerInformation) {
		final AttributeTable signedAttrs = signerInformation.getSignedAttributes();
		if (signedAttrs != null && signedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId) != null) {
			return true;
		}
		return false;
	}

	/**
	 * Method that checks whether a signer has CAdES-BES format.
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @return a boolean that indicates whether the signer has CAdES-BES format.
	 */
	private static boolean isCAdESBES(final SignerInformation signerInformation) {
		// Accedemos al conjunto de atributos firmados
		final AttributeTable signedAttrs = signerInformation.getSignedAttributes();
		// Se considera una firma con formato CAdES-BES si posee los
		// atributos firmados
		// id_aa_signingCertificate, o id_aa_signingCertificateV2, o
		// id_aa_ets_otherSigCert
		return signedAttrs.get(PKCSObjectIdentifiers.id_aa_signingCertificate) != null
				|| signedAttrs.get(PKCSObjectIdentifiers.id_aa_signingCertificateV2) != null
				|| signedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_otherSigCert) != null;
	}

	/**
	 * Method that checks whether a signer has CAdES-B-B-Level format.
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @return a boolean that indicates whether the signer has CAdES-B-B-Level format.
	 */
	private static boolean isCAdESBBLevel(final SignerInformation signerInformation) {
		// Accedemos al conjunto de atributos firmados
		final AttributeTable signedAttrs = signerInformation.getSignedAttributes();
		// Se considera una firma con formato CAdES-B-B-Level si posee el
		// atributo firmado id_aa_signingCertificateV2
		return signedAttrs.get(PKCSObjectIdentifiers.id_aa_signingCertificateV2) != null;
	}


	/**
	 * Method that checks if the signature has <code>signing-time</code> attribute.
	 *
	 * @param signature Parameter that represents the signer of the
	 *                             signature.
	 * @return a boolean that indicates if the signature has
	 *         <code>signing-time</code> attribute (true) or not (false).
	 */
	private static boolean isCAdESBLevel(final SignerInformation signature) {
		// Si la firma posee firmantes

		// Accedemos al conjunto de atributos firmados
		final AttributeTable signedAttrs = signature.getSignedAttributes();

		// Comprobamos si tiene el atributo signing-time
		if (signedAttrs.get(PKCSObjectIdentifiers.pkcs_9_at_signingTime) != null) {
			return true;
		}

		return false;
	}

	/**
	 * Method that checks if a signer has CAdES LT-Level format, it contains at
	 * least one revocation value into the signed data, and it doesn't contain any
	 * of the following unsigned attributes:
	 * <ul>
	 * <li>complete-certificate-references.</li>
	 * <li>complete-revocation-references.</li>
	 * <li>attribute-certificate-references.</li>
	 * <li>attribute-revocation-references.</li>
	 * <li>CAdES-C-time-stamp.</li>
	 * <li>time-stamped-certs-crls-references.</li>
	 * <li>certificate-values.</li>
	 * <li>revocation-values.</li>
	 * <li>archive-time-stamp.</li>
	 * <li>archive-time-stampv2.</li>
	 * <li>long-term-validation.</li>
	 * </ul>
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @param cmsSignedData     Parameter that represents the signed data.
	 * @return a boolean that indicates if the signer has CAdES LT-Level (true) or
	 *         not (false).
	 */
	private static boolean isCAdESLTLevel(final SignerInformation signerInformation,
			final CMSSignedData cmsSignedData) {
		// Accedemos al conjunto de atributos no firmados
		final AttributeTable unsignedAttrs = signerInformation.getUnsignedAttributes();

		/*
		 * Comprobamos que la firma no contenga ninguno de los atributos no firmados: >
		 * complete-certificate-references > complete-revocation-references >
		 * attribute-certificate-references > attribute-revocation-references >
		 * CAdES-C-time-stamp > time-stamped-certs-crls-references > certificate-values
		 * > revocation-values > archive-time-stamp > archive-time-stampv2 >
		 * long-term-validation
		 *
		 * y que contenga al menos un elemento de revocación dentro de SignedData.crl
		 */
		final CollectionStore<X509CRLHolder> crlStore = (CollectionStore<X509CRLHolder>) cmsSignedData.getCRLs();
		if (checkUnsignedAttributesForCAdESLTLevel(unsignedAttrs) && crlStore != null) {
			final Iterator<X509CRLHolder> it = crlStore.iterator();
			if (it.hasNext()) {
				return true;
			}
		}
		return false;
	}


	/**
	 * Method that checks if a signature doesn't contain any of the next unsigned
	 * attributes:
	 * <ul>
	 * <li>complete-certificate-references.</li>
	 * <li>complete-revocation-references.</li>
	 * <li>attribute-certificate-references.</li>
	 * <li>attribute-revocation-references.</li>
	 * <li>CAdES-C-time-stamp.</li>
	 * <li>time-stamped-certs-crls-references.</li>
	 * <li>certificate-values.</li>
	 * <li>revocation-values.</li>
	 * <li>archive-time-stamp.</li>
	 * <li>archive-time-stampv2.</li>
	 * <li>long-term-validation.</li>
	 * </ul>
	 *
	 * @param unsignedAttrs Parameter that represents the unsigned attributes of the
	 *                      signature.
	 * @return a boolean that indicates if the signature contains at least one of
	 *         the attributes (false) or none (true).
	 */
	private static boolean checkUnsignedAttributesForCAdESLTLevel(final AttributeTable unsignedAttrs) {
		if (unsignedAttrs != null) {
			// complete-certificate-references
			if (unsignedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_certificateRefs) != null) {
				return false;
			}

			// complete-revocation-references
			if (unsignedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_revocationRefs) != null) {
				return false;
			}

			// attribute-certificate-references
			if (unsignedAttrs.get(ID_ATTRIBUTE_CERTIFICATE_REFERENCES) != null) {
				return false;
			}

			// attribute-revocation-references
			if (unsignedAttrs.get(ID_ATTRIBUTE_REVOCATION_REFERENCES) != null) {
				return false;
			}

			// CAdES-C-time-stamp
			if (unsignedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_escTimeStamp) != null) {
				return false;
			}

			// time-stamped-certs-crls-references
			if (unsignedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_certCRLTimestamp) != null) {
				return false;
			}

			// certificate-values
			if (unsignedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_certValues) != null) {
				return false;
			}

			return checkUnsignedAttributesForCAdESLTLevelAux(unsignedAttrs);
		}
		return true;
	}

	/**
	 * Method that checks if a signature doesn't contain any of the next unsigned
	 * attributes:
	 * <ul>
	 * <li>revocation-values.</li>
	 * <li>archive-time-stamp.</li>
	 * <li>archive-time-stampv2.</li>
	 * <li>long-term-validation.</li>
	 * </ul>
	 *
	 * @param unsignedAttrs Parameter that represents the unsigned attributes of the
	 *                      signature.
	 * @return a boolean that indicates if the signature contains at least one of
	 *         the attributes (false) or none (true).
	 */
	private static boolean checkUnsignedAttributesForCAdESLTLevelAux(final AttributeTable unsignedAttrs) {
		// revocation-values
		if (unsignedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_revocationValues) != null) {
			return false;
		}

		// archive-time-stamp
		if (unsignedAttrs.get(ESFAttributes.archiveTimestamp) != null) {
			return false;
		}

		// archive-time-stamp-v2
		if (unsignedAttrs.get(ESFAttributes.archiveTimestampV2) != null) {
			return false;
		}

		// long-term-validation
		if (unsignedAttrs.get(ID_LONG_TERM_VALIDATION) != null) {
			return false;
		}
		return true;
	}

	/**
	 * Method that checks if a signer contains the
	 * <code>archive-time-stamp-v3</code> attribute.
	 *
	 * @param signerInformation Parameter that represents the information about the
	 *                          signer.
	 * @return a boolean that indicates if the signer contains the
	 *         <code>archive-time-stamp-v3</code> attribute (true) or not (false).
	 */
	private static boolean isCAdESLTALevel(final SignerInformation signerInformation) {
		// Accedemos al conjunto de atributos no firmados
		final AttributeTable unsignedAttrs = signerInformation.getUnsignedAttributes();

		// Comprobamos si tiene el atributo archive-time-stamp-v3
		if (unsignedAttrs != null && unsignedAttrs.get(ID_ARCHIVE_TIME_STAMP_V3) != null) {
			return true;
		}
		return false;
	}


}
