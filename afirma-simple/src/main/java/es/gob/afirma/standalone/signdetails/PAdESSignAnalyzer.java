package es.gob.afirma.standalone.signdetails;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.esf.SignaturePolicyId;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.cms.CMSSignedData;
import org.spongycastle.cms.SignerInformation;
import org.spongycastle.cms.SignerInformationStore;

import com.aowagie.text.exceptions.BadPasswordException;
import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfArray;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfObject;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfString;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.signvalidation.SignatureFormatDetectorPadesCades;
import es.gob.afirma.signvalidation.ValidatePdfSignature;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class PAdESSignAnalyzer implements SignAnalyzer {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	List <SignDetails> signDetailsList;
	final AOTreeNode signersTree = new AOTreeNode("Datos"); //$NON-NLS-1$

	public static final String PADES = "PAdES"; //$NON-NLS-1$s


	public PAdESSignAnalyzer(final byte [] data) throws Exception {
    	try {
    		this.signDetailsList = new ArrayList<>();
    		createSignDetails(data);
    	}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}
	}

	@Override
	public AOTreeModel getSignersTree() {
		return new AOTreeModel(this.signersTree);
	}

	@Override
	public List<SignDetails> getAllSignDetails() {
		return this.signDetailsList;
	}

	@Override
	public String getSignFormat() {
		return PADES;
	}

	@Override
	public String getDataLocation() {
        return null;
	}

	/**
	 * Crea los detalles de cada firma a partir del array indicado por par&aacute;metro.
	 * @param data Datos a mapear.
	 * @throws Exception Error transformando los datos.
	 */
	private void createSignDetails(final byte [] data) throws Exception {
    	try {

        	PdfReader pdfReader = null;
        	try {
        		pdfReader = new PdfReader(data);
        	}
        	catch (final BadPasswordException e) {
        		LOGGER.info(
    				"El PDF necesita contrasena. Se devolvera el arbol vacio: " + e //$NON-NLS-1$
    			);
        		throw e;
        	}
        	catch (final Exception e) {
        		LOGGER.severe("No se ha podido leer el PDF, se devolvera un arbol vacio: " + e); //$NON-NLS-1$
        		throw e;
        	}

        	AcroFields af = null;
        	try {
        		af = pdfReader.getAcroFields();
        	}
        	catch (final Exception e) {
        		LOGGER.severe("No se ha podido obtener la informacion de los firmantes del PDF, se devolvera un arbol vacio: " + e); //$NON-NLS-1$
        		throw e;
        	}

        	final int certLevel = pdfReader.getCertificationLevel();

			final String signProfile = SignatureFormatDetectorPadesCades.resolvePDFFormat(data);

			for (final String signatureName : af.getSignatureNames()) {

				// Comprobamos si es una firma o un sello
				final PdfDictionary pdfDictionary = af.getSignatureDictionary(signatureName);
				if (AOPDFSigner.PDFNAME_ETSI_RFC3161.equals(pdfDictionary.get(PdfName.SUBFILTER))
						|| AOPDFSigner.PDFNAME_DOCTIMESTAMP.equals(pdfDictionary.get(PdfName.SUBFILTER))) {
					// Ignoramos los sellos
					continue;
				}

				final SignDetails signDetails = buildSignDetails(signatureName, pdfDictionary, af, signProfile, certLevel);

				//Se obtiene la politica en caso de que exista
				final PdfString contents = pdfDictionary.getAsString(PdfName.CONTENTS);
				if (contents != null) {
					final byte[] signBytes = pdfDictionary.getAsString(PdfName.CONTENTS).getOriginalBytes();
					try {
						final CMSSignedData cmsSignedData = new CMSSignedData(signBytes);
						final SignerInformationStore signerInformationStore = cmsSignedData.getSignerInfos();
						final List<SignerInformation> listSignersSignature = (List<SignerInformation>) signerInformationStore
								.getSigners();
						for (final SignerInformation si : listSignersSignature) {
							final SignaturePolicy policy = analyzePolicy(si);
							if (policy != null) {
								signDetails.setPolicy(policy);
								break;
							}
						}
					} catch (final Exception e) {
						LOGGER.log(Level.SEVERE, "No se ha podido obtener la informacion de la politica correctamente", e); //$NON-NLS-1$
					}
				}

				this.signDetailsList.add(signDetails);
			}

		}
    	catch (final Exception e) {
    		throw new AOInvalidFormatException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
    	}
	}

	/**
	 * Construye los detalles de la firma que se indica por par&aacute;metro.
	 * @param signName Nombre de firma.
	 * @param signPdfDictionary Datos de la firma.
	 * @param af Campos con informaci&oacute;n sobre la firma.
	 * @param signProfile Perfil de firma.
	 * @return Detalles de la firma.
	 * @throws Exception Error transformando los datos.
	 */
	private SignDetails buildSignDetails(final String signName, final PdfDictionary signPdfDictionary, final AcroFields af, final String signProfile, final int certLevel) throws Exception {

		final SignDetails padesSignDetails = new SignDetails();

		padesSignDetails.setSignProfile(signProfile);

		padesSignDetails.setCertificationLevel(certLevel);

		// Comprobamos si la firma es la que certifica el documento
		if (signPdfDictionary.get(PdfName.REFERENCE) != null) {
			final PdfArray reference = (PdfArray) signPdfDictionary.get(PdfName.REFERENCE);
			final ArrayList<PdfObject> p = reference.getArrayList();
			final PdfDictionary dictionaryReference = (PdfDictionary) p.get(0);
			padesSignDetails.setCertificationSign(dictionaryReference.get(PdfName.TRANSFORMMETHOD) != null);
		}
		else {
			padesSignDetails.setCertificationSign(Boolean.FALSE);
		}

		PdfPKCS7 pkcs7 = null;
		try {
			pkcs7 = af.verifySignature(signName);
		} catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "El PDF contiene una firma corrupta o con un formato desconocido (" + //$NON-NLS-1$
					signName + ")", //$NON-NLS-1$
					e);
			throw e;
		}

		// Signing time
		if (pkcs7.getSignDate() != null) {
			padesSignDetails.setSigningTime(pkcs7.getSignDate().getTime());
		}

		// Obtenemos el algoritmo de firma
		final String digestAlgorithm = pkcs7.getDigestAlgorithm();
		if (digestAlgorithm != null) {
			padesSignDetails.setAlgorithm(digestAlgorithm);
		}

		// Obtenemos el firmante y lo agregamos al arbol
		this.signersTree.add(new AOTreeNode(AOUtil.getCN(pkcs7.getSigningCertificate()) + " (" + DateFormat.getDateTimeInstance(DateFormat.DEFAULT, DateFormat.SHORT).format(pkcs7.getSignDate().getTime()) + ")")); //$NON-NLS-1$ //$NON-NLS-2$

		// Detalles del certificado
		final CertificateDetails certDetails = new CertificateDetails(pkcs7.getSigningCertificate());
		padesSignDetails.setSigner(certDetails);

		// Metadatos
		final Map<String, String> metadataMap = new HashMap<>();
		final PdfString reason = signPdfDictionary.getAsString(PdfName.REASON);
		if (reason != null) {
			metadataMap.put(SignDetailsFormatter.SIGN_REASON_METADATA, reason.toString());
		}
		final PdfString location = signPdfDictionary.getAsString(PdfName.LOCATION);
		if (location != null) {
			metadataMap.put(SignDetailsFormatter.LOCATION_METADATA, location.toString());
		}
		final PdfString contactInfo = signPdfDictionary.getAsString(PdfName.CONTACTINFO);
		if (contactInfo != null) {
			metadataMap.put(SignDetailsFormatter.CONTACT_INFO_METADATA, contactInfo.toString());
		}
		padesSignDetails.setMetadata(metadataMap);

		// Validamos la firma
		final List<SignValidity> listValidity = ValidatePdfSignature.validateSign(signName, af, signProfile, false);
		padesSignDetails.setValidityResult(listValidity);

		// Cuando la firma sea certificada de tipo 1, solo la ultima firma sera valida.
		if(PdfExtraParams.CERTIFICATION_LEVEL_VALUE_TYPE_1.equals(Integer.toString(certLevel))
				&& padesSignDetails.getValidityResult().get(0).getValidity().equals(SIGN_DETAIL_TYPE.OK)){

			final int rev = af.getRevision(signName);
			if(!(rev == af.getTotalRevisions())){
			    final List<SignValidity> listValidityCertifiedSign = padesSignDetails.getValidityResult();
				listValidityCertifiedSign.clear();
				listValidityCertifiedSign.add(new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CERTIFIED_SIGN_REVISION));
				padesSignDetails.setValidityResult(listValidityCertifiedSign);
				padesSignDetails.setLastRevisionSign(Boolean.FALSE);
			}
			else{
				padesSignDetails.setLastRevisionSign(Boolean.TRUE);
			}
		}

		return padesSignDetails;
	}

    /**
     * Analiza la pol&iacute;tica y la agrega a los detalles de la firma.
     * @param si Datos sobre la firma.
     */
    private static SignaturePolicy analyzePolicy(final SignerInformation si) {
    	final AttributeTable signedAttrs = si.getSignedAttributes();
    	final Attribute policyAttr = signedAttrs != null ? signedAttrs.get(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId): null;
		if (policyAttr != null && policyAttr.getAttrValues() != null && policyAttr.getAttrValues().size() > 0) {
			final SignaturePolicyId sigPolId = SignaturePolicyId.getInstance(policyAttr.getAttrValues().getObjectAt(0));
			final String polId = sigPolId.getSigPolicyId().toString();

			if (polId.equals(SignDetails.POLICY_PADES_AGE_1_9.getPolicyIdentifier().substring(8))) {
				return new SignaturePolicy(SimpleAfirmaMessages.getString("PreferencesPanel.73"), SignDetails.POLICY_PADES_AGE_1_9); //$NON-NLS-1$
			}

			final String identifierHash = sigPolId.getSigPolicyHash().getHashValue().toString();
			final String identifierHashAlgorithm = sigPolId.getSigPolicyHash().getHashAlgorithm().getAlgorithm().toString();
			final String qualifier = sigPolId.getSigPolicyQualifiers().getInfoAt(0).getSigQualifier().toString();
			final AdESPolicy newPolicy = new AdESPolicy(polId, identifierHash.substring(1), identifierHashAlgorithm, qualifier);
			return new SignaturePolicy("", newPolicy); //$NON-NLS-1$
		}
		return null;
    }

}
