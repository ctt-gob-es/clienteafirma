package es.gob.afirma.signers.pades.ltv;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Enumerated;
import org.spongycastle.asn1.ASN1OutputStream;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERSequence;
import org.spongycastle.asn1.DERTaggedObject;
import org.spongycastle.asn1.ocsp.OCSPObjectIdentifiers;

/** Implementaci&oacute;n del DSS de PDF. */
public final class PdfDocumentSecurityStore {

    private final List<ValidationInformation> signatures = new ArrayList<ValidationInformation>();

    List<ValidationInformation> getSignatures() {
        return this.signatures;
    }

    List<byte[]> getCertificates() {
    	final List<byte[]> ret = new ArrayList<byte[]>();
    	for(final ValidationInformation vi : getSignatures()) {
    		ret.addAll(vi.getCertificates());
    	}
        return ret;
    }

    List<byte[]> getOcsps() {
    	final List<byte[]> ret = new ArrayList<byte[]>();
    	for(final ValidationInformation vi : getSignatures()) {
    		ret.addAll(vi.getOcspResponses());
    	}
        return ret;
    }

    /** Obtiene las CRL del DSS.
     * @return CRL del DSS. */
    public List<byte[]> getCrls() {
    	final List<byte[]> ret = new ArrayList<byte[]>();
    	for(final ValidationInformation vi : getSignatures()) {
    		ret.addAll(vi.getCrls());
    	}
        return ret;
    }

    /** Registra una respuesta b&aacute;sica OCSP en el DSS.
     * @param arrby Respuesta b&aacute;sica OCSP.
     * @throws IOException Si on puede registrarse la respuesta b&aacute;sica OCSP. */
    public static void registerOcspBasicResp(final byte[] arrby) throws IOException {
        final DEROctetString dEROctetString = new DEROctetString(arrby);
        final ASN1EncodableVector aSN1EncodableVector = new ASN1EncodableVector();
        aSN1EncodableVector.add(OCSPObjectIdentifiers.id_pkix_ocsp_basic);
        aSN1EncodableVector.add(dEROctetString);
        final ASN1Enumerated dEREnumerated = new ASN1Enumerated(0);
        final ASN1EncodableVector aSN1EncodableVector2 = new ASN1EncodableVector();
        aSN1EncodableVector2.add(dEREnumerated);
        aSN1EncodableVector2.add(new DERTaggedObject(true, 0, new DERSequence(aSN1EncodableVector)));
        final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        final ASN1OutputStream aSN1OutputStream = new ASN1OutputStream(byteArrayOutputStream);
        aSN1OutputStream.writeObject(new DERSequence(aSN1EncodableVector2));
        aSN1OutputStream.close();
        final byte[] arrby2 = byteArrayOutputStream.toByteArray();
//        this.registerOcspResp(arrby2);
//        this.registerSignature(arrby2);
        byteArrayOutputStream.close();
    }

    static class ValidationInformation {

    	private final List<byte[]> certificates;
    	private final List<byte[]> ocspResponses;
    	private final List<byte[]> crl;

    	ValidationInformation(final List<byte[]> certs,
    			              final List<byte[]> ocsps,
    			              final List<byte[]> crls) {
    		this.certificates = certs != null ? certs : new ArrayList<byte[]>(0);
    		this.ocspResponses = ocsps != null ? ocsps : new ArrayList<byte[]>(0);
    		this.crl = crls != null ? crls : new ArrayList<byte[]>(0);
    	}

    	List<byte[]> getCertificates() {
    		return this.certificates;
    	}

    	List<byte[]> getOcspResponses() {
    		return this.ocspResponses;
    	}

    	List<byte[]> getCrls() {
    		return this.crl;
    	}

    }

}

