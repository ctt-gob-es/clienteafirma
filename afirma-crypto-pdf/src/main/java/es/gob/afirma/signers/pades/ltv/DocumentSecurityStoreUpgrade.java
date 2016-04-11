
package es.gob.afirma.signers.pades.ltv;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import com.aowagie.text.pdf.PdfArray;
import com.aowagie.text.pdf.PdfDictionary;
import com.aowagie.text.pdf.PdfIndirectReference;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfStamper;
import com.aowagie.text.pdf.PdfStream;
import com.aowagie.text.pdf.PdfWriter;

/** Utilidad para la actualizaci&oacute;n del DSS de un PDF. */
public final class DocumentSecurityStoreUpgrade {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

//    public DocumentSecurityStoreUpgrade(CoreContext coreContext) throws SignatureManagerException {
//        super(coreContext, -1, -1, "UPGRADE");
//        this.configuration = (CryptoConfiguration)this.context.getProperty("configuration");
//        if (this.configuration == null) {
//            throw new SignatureManagerException(Language.getResCoreSign((String)"logDSSU01"));
//        }
//    }

//    public void doTask(PdfStamper pdfStamper, byte[] arrby) {
//
//        PdfDocumentSecurityStore pdfDocumentSecurityStore = new PdfDocumentSecurityStore();
//        AcroFields acroFields = pdfStamper.getReader().getAcroFields();
//
//        pdfStamper.getReader().getCatalog().remove(new PdfName("DSS")); //$NON-NLS-1$
//
//        List<String> arrayList = acroFields.getSignatureNames();
//        for (int i = 0; i < arrayList.size(); ++i) {
//            X509Certificate x509Certificate;
//            String string3;
//            byte[] arrby2;
//            TimeStampToken timeStampToken;
//            String string4 = (String)arrayList.get(i);
//            PdfDictionary pdfDictionary = acroFields.getSignatureDictionary(string4);
//            String string5 = null;
//            if (pdfDictionary.get(PdfName.TYPE) != null) {
//                string5 = pdfDictionary.get(PdfName.TYPE).toString();
//            }
//            if ((string3 = pdfDictionary.get(PdfName.SUBFILTER).toString()).equalsIgnoreCase(new PdfName("ETSI.RFC3161").toString()) && (string5 == null || string5.equals(new PdfName("DocTimeStamp").toString()))) { //$NON-NLS-1$
//                arrby2 = pdfDictionary.getAsString(PdfName.CONTENTS).getOriginalBytes();
//                timeStampToken = new TimeStampToken(arrby2);
//                x509Certificate = new X509Certificate(timeStampToken.getSigningCertificate().getEncoded());
//                PadesUtils.actualizeDSSStructure(x509Certificate, string, string2, pdfDocumentSecurityStore, arrby2);
//                continue;
//            }
//            if (!string5.equals(PdfName.SIG.toString())) continue;
//            arrby2 = pdfDictionary.getAsString(PdfName.CONTENTS).getOriginalBytes();
//            timeStampToken = (SignedData)PadesUtils.getCMSSignature(arrby2, pdfDictionary.getAsArray(PdfName.BYTERANGE), arrby);
//            x509Certificate = timeStampToken.getCertificate(timeStampToken.getSignerInfos()[0].getSignerIdentifier());
//            PadesUtils.actualizeDSSStructure(x509Certificate, string, string2, pdfDocumentSecurityStore, arrby2);
//        }
//        this.addDocumentSecurityStore(pdfStamper, pdfDocumentSecurityStore);
//    }

    private static void addDocumentSecurityStore(final PdfStamper pdfStamper,
    		                                     final PdfDocumentSecurityStore pdfDocumentSecurityStore) throws IOException {

    	final PdfWriter pdfWriter = pdfStamper.getWriter();
        final PdfDictionary pdfDictionary = new PdfDictionary(new PdfName("DSS")); //$NON-NLS-1$

        // Registramos los certificados
        final List<PdfIndirectReference> certificateIndirectReferences = new ArrayList<PdfIndirectReference>();
        for (final byte[] certificateBytes : pdfDocumentSecurityStore.getCertificates()) {
            certificateIndirectReferences.add(
        		pdfWriter.addToBody(
    				new PdfStream(
		        		certificateBytes
		    		),
    				false
				).getIndirectReference()
    		);
        }
        final PdfArray certificatesPdfArray = new PdfArray();
        for (final PdfIndirectReference certificateIndirectReference : certificateIndirectReferences) {
            certificatesPdfArray.add(certificateIndirectReference);
        }
        if (certificatesPdfArray.size() > 0) {
            pdfDictionary.put(new PdfName("Certs"), certificatesPdfArray); //$NON-NLS-1$
        }

        // Registramos las respuestas OCSP
        final List<PdfIndirectReference> ocspIndirectReferences = new ArrayList<PdfIndirectReference>();
        for (final byte[] ocspBytes : pdfDocumentSecurityStore.getOcsps()) {
            ocspIndirectReferences.add(
        		pdfWriter.addToBody(
    				new PdfStream(
						ocspBytes
					),
					false
				).getIndirectReference()
    		);
        }
        final PdfArray ocspResponsesPdfArray = new PdfArray();
        for (final PdfIndirectReference ocspIndirectReference : ocspIndirectReferences) {
        	ocspResponsesPdfArray.add(ocspIndirectReference);
        }
        if (ocspResponsesPdfArray.size() > 0) {
            pdfDictionary.put(new PdfName("OCSPs"), ocspResponsesPdfArray); //$NON-NLS-1$
        }

        // Registramos las CRL
        final List<PdfIndirectReference> crlIndirectReferences = new ArrayList<PdfIndirectReference>();
        for (final byte[] crlBytes : pdfDocumentSecurityStore.getCrls()) {
        	crlIndirectReferences.add(
    			pdfWriter.addToBody(
					new PdfStream(
						crlBytes
					),
    				false
    			).getIndirectReference()
			);
        }
        final PdfArray crlsPdfArray = new PdfArray();
        for (final PdfIndirectReference crlIndirectReference : crlIndirectReferences) {
        	crlsPdfArray.add(crlIndirectReference);
        }
        if (crlsPdfArray.size() > 0) {
            pdfDictionary.put(new PdfName("CRLs"), crlsPdfArray); //$NON-NLS-1$
        }

//        // Y anadimos todo al diccionario general
//        if (pdfDocumentSecurityStore.getSignatures().size() > 0) {
//            final PdfDictionary vriDictionary = new PdfDictionary(new PdfName("VRI")); //$NON-NLS-1$
//            for (final String signatureKey : pdfDocumentSecurityStore.getSignatures().keySet()) {
//                int[] arrn;
//                final PdfDocumentSecurityStore.ValidationInformation validationInformation = pdfDocumentSecurityStore.getSignatures().get(signatureKey);
//                final PdfDictionary pdfDictionary2 = new PdfDictionary();
//                final int[] arrn2 = validationInformation.getCertId();
//                if (arrn2 != null && arrn2.length > 0) {
//                    final PdfArray array6 = new PdfArray();
//                    for (final int element : arrn2) {
//                        array6.add(certificateIndirectReferences.get(element));
//                    }
//                    pdfDictionary2.put(new PdfName("Cert"), array6); //$NON-NLS-1$
//                }
//                int[] intArray6;
//                if ((intArray6 = validationInformation.getOcspId()) != null && intArray6.length > 0) {
//                    final PdfArray pdfArray2 = new PdfArray();
//                    for (final int element : intArray6) {
//                        //pdfArray2.add(map0.get(element));
//                    }
//                    pdfDictionary2.put(new PdfName("OCSP"), pdfArray2); //$NON-NLS-1$
//                }
//                if ((arrn = validationInformation.getCrlId()) != null && arrn.length > 0) {
//                    final PdfArray pdfArray31 = new PdfArray();
//                    for (final int element : arrn) {
//                        //pdfArray31.add(map2.get(element));
//                    }
//                    pdfDictionary2.put(new PdfName("CRL"), pdfArray31); //$NON-NLS-1$
//                }
//                vriDictionary.put(new PdfName(signatureKey), pdfDictionary2);
//              }
//              pdfDictionary.put(
//        		  new PdfName("VRI"), //$NON-NLS-1$
//        		  pdfWriter.addToBody(vriDictionary, false).getIndirectReference()
//    		  );
//          }
//          final PdfDictionary pdfStream = pdfStamper.getReader().getCatalog();
//          pdfStream.put(new PdfName("DSS"), pdfWriter.addToBody(pdfDictionary, false).getIndirectReference()); //$NON-NLS-1$
//          pdfStamper.getAcroFields().markUsed(pdfStream);
    }

}

