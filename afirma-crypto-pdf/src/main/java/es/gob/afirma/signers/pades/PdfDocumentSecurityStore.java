package es.gob.afirma.signers.pades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.DEREnumerated;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERTaggedObject;
import org.bouncycastle.asn1.ocsp.OCSPObjectIdentifiers;

import com.lowagie.text.pdf.PRStream;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfString;

/** Clase que representa un diccionario DSS.
 * @author <a href="mailto:jgutierrez@accv.es">Jos&eacute; Manuel Guti&eacute;rrez N&uacute;&ntilde;ez</a> */
final class PdfDocumentSecurityStore {

    private static final String DEFAULT_DIGEST_ALGORITHM = "SHA1"; //$NON-NLS-1$

    /** Clase que representa una estructura VRI (Validation Related Information). */
    static class ValidationInformation {

    	/** Obtiene la clave como la codificaci&oacute;n Base16 en may&uacute;sculas
    	 * del la huella digital SHA-1 de la firma
    	 * @param digest Huella digital SHA-1 de la firma
    	 * @return Codificaci&oacute;n Base16 en may&uacute;sculas de la huella digital */
        static String getKey(final byte digest[]) {
            final StringBuilder buf = new StringBuilder();
            for (final byte element : digest) {
                final int b = element & 0xff;
                if(b >= 0 && b <= 15) {
                    buf.append("0"); //$NON-NLS-1$
                }
                buf.append(Integer.toHexString(b).toUpperCase(Locale.ENGLISH));
            }
            return buf.toString();
        }

        String getKey() {
            return getKey(this.digest);
        }

        byte[] getDigest() {
            return this.digest;
        }

        int[] getOcspId() {
            return this.ocspId.clone();
        }

        int[] getCrlId() {
            return this.crlId.clone();
        }

        int[] getCertId() {
            return this.certId.clone();
        }

        Calendar getDate() {
            return this.date;
        }

        private final byte digest[];
        private final int ocspId[];
        private final int crlId[];
        private final int certId[];
        private final Calendar date;

        ValidationInformation(final byte val[], final int certId[], final int ocspId[], final int crlId[], final Calendar date) {
        	byte[] digestBytes = null;
        	try {
        		digestBytes = MessageDigest.getInstance(DEFAULT_DIGEST_ALGORITHM).digest(val);
        	}
        	catch(final Exception e) {
        		Logger.getLogger("es.gob.afirma").severe("No se ha posido calcular la huella digital: " + e);  //$NON-NLS-1$//$NON-NLS-2$
        	}
        	this.digest = digestBytes;
            this.ocspId = ocspId.clone();
            this.crlId = crlId.clone();
            this.certId = certId.clone();
            this.date = date;
        }

        ValidationInformation(final PdfName key, final int certId[], final int ocspId[], final int crlId[], final Calendar date) {
            this.digest = key.getBytes();
            this.ocspId = ocspId.clone();
            this.crlId = crlId.clone();
            this.certId = certId.clone();
            this.date = date;
        }
    }

    private final Map<String, ValidationInformation> signatures = new HashMap<String, ValidationInformation>();
    private final Map<Integer, byte[]> certificates = new HashMap<Integer, byte[]>();
    private final Map<Integer, byte[]> ocsps = new HashMap<Integer, byte[]>();
    private final Map<Integer, byte[]> crls = new HashMap<Integer, byte[]>();

    private static final String PDF_NAME_CERTS = "Certs"; //$NON-NLS-1$
    private static final String PDF_NAME_OCSPS = "OCSPs"; //$NON-NLS-1$
    private static final String PDF_NAME_CRLS = "CRLs"; //$NON-NLS-1$
    private static final String PDF_NAME_VRI = "VRI"; //$NON-NLS-1$
    private static final String PDF_NAME_CERT = "Cert"; //$NON-NLS-1$
    private static final String PDF_NAME_OCSP = "OCSP"; //$NON-NLS-1$
    private static final String PDF_NAME_CRL = "CRL"; //$NON-NLS-1$

    /** Constructor por defecto. */
    PdfDocumentSecurityStore() {}

    /** Constructor para inicializar la clase con una estructura DSS ya creada.
     * @param dss Diccionario DSS
     * @throws IOException */
    PdfDocumentSecurityStore(final PdfDictionary dss) throws IOException {
        int i = 0;
        PdfArray arrayCerts = dss.getAsArray(new PdfName(PDF_NAME_CERTS));
        if(arrayCerts != null) {
            for(final Iterator<PdfObject> iterator = arrayCerts.listIterator(); iterator.hasNext();) {
                final PdfIndirectReference reference = (PdfIndirectReference)iterator.next();
                this.certificates.put(Integer.valueOf(i), getContentBytesFromContentObject(PdfReader.getPdfObject(reference)));
                i++;
            }

        }
        PdfArray arrayOcsps = dss.getAsArray(new PdfName(PDF_NAME_OCSPS));
        if(arrayOcsps != null) {
            i = 0;
            for(final Iterator<PdfObject> iterator = arrayOcsps.listIterator(); iterator.hasNext();) {
                final PdfIndirectReference reference = (PdfIndirectReference)iterator.next();
                this.ocsps.put(Integer.valueOf(i), getContentBytesFromContentObject(PdfReader.getPdfObject(reference)));
                i++;
            }

        }
        PdfArray arrayCrls = dss.getAsArray(new PdfName(PDF_NAME_CRLS));
        if(arrayCrls != null) {
            i = 0;
            for(final Iterator<PdfObject> iterator = arrayCrls.listIterator(); iterator.hasNext();) {
                final PdfIndirectReference reference = (PdfIndirectReference)iterator.next();
                this.crls.put(Integer.valueOf(i), getContentBytesFromContentObject(PdfReader.getPdfObject(reference)));
                i++;
            }

        }
        final PdfDictionary vri = dss.getAsDict(new PdfName(PDF_NAME_VRI));
        if (vri != null) {
	        PdfName key;
	        ValidationInformation val;
	        for(final Iterator<PdfName> iterator = vri.getKeys().iterator(); iterator.hasNext(); this.signatures.put(key.toString().substring(1), val)) {
	            key = iterator.next();
	            final PdfDictionary vriEntry = vri.getAsDict(key);
	            arrayCerts = vriEntry.getAsArray(new PdfName(PDF_NAME_CERT));
	            int certId[];
	            if(arrayCerts != null) {
	                certId = new int[arrayCerts.size()];
	                for(i = 0; i < arrayCerts.size(); i++) {
	                    final PdfIndirectReference reference = (PdfIndirectReference)arrayCerts.getPdfObject(i);
	                    final byte referenceBytes[] = getContentBytesFromContentObject(PdfReader.getPdfObject(reference));
	                    final Iterator<Integer> iteratorKeys = this.certificates.keySet().iterator();
	                    do {
	                        if(!iteratorKeys.hasNext()) {
								break;
							}
	                        final int index = iteratorKeys.next().intValue();
	                        if(Arrays.equals(referenceBytes, this.certificates.get(Integer.valueOf(index)))) {
								certId[i] = index;
							}
	                    } while(true);
	                }

	            }
	            else {
	                certId = new int[0];
	            }
	            arrayOcsps = vriEntry.getAsArray(new PdfName(PDF_NAME_OCSP));
	            int ocspId[];
	            if(arrayOcsps != null) {
	                ocspId = new int[arrayOcsps.size()];
	                i = 0;
	                for(final Iterator<PdfObject> iteratorOcsps = arrayOcsps.listIterator(); iteratorOcsps.hasNext();) {
	                    final PdfIndirectReference reference = (PdfIndirectReference)iteratorOcsps.next();
	                    final byte referenceBytes[] = getContentBytesFromContentObject(PdfReader.getPdfObject(reference));
	                    final Iterator<Integer> iteratorKeys = this.ocsps.keySet().iterator();
	                    do {
	                        if(!iteratorKeys.hasNext()) {
								break;
							}
	                        final int index = iteratorKeys.next().intValue();
	                        if(Arrays.equals(referenceBytes, this.ocsps.get(Integer.valueOf(index)))) {
								ocspId[i] = index;
							}
	                    } while(true);
	                    i++;
	                }

	            }
	            else {
	                ocspId = new int[0];
	            }
	            arrayCrls = vriEntry.getAsArray(new PdfName(PDF_NAME_CRL));
	            int crlId[];
	            if(arrayCrls != null) {
	                crlId = new int[arrayCrls.size()];
	                i = 0;
	                for(final Iterator<PdfObject> iteratorCRLs = arrayCrls.listIterator(); iteratorCRLs.hasNext();) {
	                    final PdfIndirectReference reference = (PdfIndirectReference)iteratorCRLs.next();
	                    final byte referenceBytes[] = getContentBytesFromContentObject(PdfReader.getPdfObject(reference));
	                    final Iterator<Integer> iteratorKeys = this.crls.keySet().iterator();
	                    do {
	                        if(!iteratorKeys.hasNext()) {
								break;
							}
	                        final int index = iteratorKeys.next().intValue();
	                        if(Arrays.equals(referenceBytes, this.crls.get(Integer.valueOf(index)))) {
								crlId[i] = index;
							}
	                    } while(true);
	                    i++;
	                }

	            }
	            else {
	                crlId = new int[0];
	            }
	            Calendar date = null;
	            if(vriEntry.get(PdfName.TU) != null) {
	                if(vriEntry.get(PdfName.TU) instanceof PdfDate) {
						date = PdfDate.decode(((PdfDate)vriEntry.get(PdfName.TU)).getEncoding());
					}
	                if(vriEntry.get(PdfName.TU) instanceof PdfString) {
						date = PdfDate.decode(vriEntry.getAsString(PdfName.TU).getEncoding());
					}
	            }
	            val = new ValidationInformation(key, certId, ocspId, crlId, date);
	        }
        }
    }

    Map<String, ValidationInformation> getSignatures() {
        return this.signatures;
    }

    Map<Integer, byte[]> getCertificates() {
        return this.certificates;
    }

    Map<Integer, byte[]> getOcsps() {
        return this.ocsps;
    }

    Map<Integer, byte[]> getCrls() {
        return this.crls;
    }

    /** Registra una firma (crear&aacute; una estructura VRI dentro del DSS).
     * @param pkcs7 Firma
     * @param certId Array de IDs de certificados ubicados en el DSS
     * @param ocspId Array de IDs de respuestas OCSP ubicadas en el DSS
     * @param crlId Array de IDs de CRLs ubicadas en el DSS
     * @param date Fecha de la firma */
    void registerSignature(final byte pkcs7[], final int certId[], final int ocspId[], final int crlId[], final Calendar date) {
        final ValidationInformation val = new ValidationInformation(pkcs7, certId, ocspId, crlId, date);
        this.signatures.put(val.getKey(), val);
    }

    void registerSignature(final byte pkcs7[], final int certId[], final int ocspId[], final int crlId[]) {
        registerSignature(pkcs7, certId, ocspId, crlId, new GregorianCalendar());
    }

    void registerSignature(final byte pkcs7[]) {
        registerSignature(pkcs7, null, null, null, new GregorianCalendar());
    }

    /** Registra un certificado en el DSS y devuelve el ID que se le ha asignado.
     * @param cert Certificado
     * @return ID asignado al certificado (luego servir&aacute; para registrar la firma completa
     * 	en un VRI) */
    synchronized int registerCertificate(final byte cert[]) {
        final int nextId = this.certificates.size() + 1;
        this.certificates.put(Integer.valueOf(nextId), cert);
        return nextId;
    }

    /** Registra una respuesta OCSP y devuelve el ID que se le ha asignado.
     * @param ocsp Respuesta OCSP
     * @return ID asignado a la respuesta OCSP  (luego servir&aacute; para registrar la firma completa
     * 	en un VRI) */
    synchronized int registerOcspResp(final byte ocsp[]) {
        final int nextId = this.ocsps.size() + 1;
        this.ocsps.put(Integer.valueOf(nextId), ocsp);
        return nextId;
    }

    synchronized int registerOcspBasicResp(final byte basicResp[]) throws IOException {
        final ASN1EncodableVector v2 = new ASN1EncodableVector();
        v2.add(OCSPObjectIdentifiers.id_pkix_ocsp_basic);
        v2.add(new DEROctetString(basicResp));
        final ASN1EncodableVector v3 = new ASN1EncodableVector();
        v3.add(new DEREnumerated(0));
        v3.add(new DERTaggedObject(true, 0, new DERSequence(v2)));
        return registerOcspResp(new DERSequence(v3).getEncoded());
    }

    /** Registra una CRL y devuelve el ID que se le ha asignado.
     * @param crl CRL
     * @return ID asignado a la CRL  (luego servir&aacute; para registrar la firma completa
     * 	en un VRI) */
    synchronized int registerCrl(final byte crl[]) {
        final int nextId = this.crls.size() + 1;
        this.crls.put(Integer.valueOf(nextId), crl);
        return nextId;
    }

    /** Devuelve el VRI pas&aacute;ndole como par&aacute;metro la firma que representa.
     * @param pkcs7 Firma PKCS#7
     * @return VRI que representa la firma
     * @throws NoSuchAlgorithmException */
    ValidationInformation getValidationInformation(final byte pkcs7[]) throws NoSuchAlgorithmException {
        final MessageDigest dg = MessageDigest.getInstance(DEFAULT_DIGEST_ALGORITHM);
        return this.signatures.get(ValidationInformation.getKey(dg.digest(pkcs7)));
    }

    private static byte[] getContentBytesFromContentObject(final PdfObject contentObject) throws IOException {
        switch (contentObject.type()) {
            case PdfObject.INDIRECT:
                return getContentBytesFromContentObject(PdfReader.getPdfObject(contentObject));
            case PdfObject.STREAM:
                return PdfReader.getStreamBytes((PRStream) PdfReader.getPdfObject(contentObject));
            case PdfObject.ARRAY:
                final ByteArrayOutputStream allBytes = new ByteArrayOutputStream();
                final ListIterator<PdfObject> iter = ((PdfArray) contentObject).listIterator();
                while (iter.hasNext()) {
                    allBytes.write(getContentBytesFromContentObject(iter.next()));
                    allBytes.write((byte)' ');
                }
                return allBytes.toByteArray();
            default:
                throw new IllegalStateException("No se soporta contenido del tipo: " + contentObject.getClass()); //$NON-NLS-1$
        }
    }

}
