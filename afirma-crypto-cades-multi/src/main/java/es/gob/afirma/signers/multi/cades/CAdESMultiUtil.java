package es.gob.afirma.signers.multi.cades;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.SignerInfo;

import es.gob.afirma.signers.pkcs7.SigUtils;

final class CAdESMultiUtil {

	private CAdESMultiUtil() {
		// No instanciable
	}

	static SignerInfo getCounterSignerForMultipleSignerInfos(final ASN1EncodableVector signerInfosU,
			                                                 final SignerInfo signerInfo,
			                                                 final ASN1EncodableVector contexExpecific) {
        for (int i = 0; i < signerInfosU.size(); i++) {
            if (signerInfosU.get(i) instanceof Attribute) {
                contexExpecific.add(signerInfosU.get(i));
            }
            else {
                contexExpecific.add(
            		new Attribute(
        				CMSAttributes.counterSignature,
        				new DERSet(signerInfosU.get(i))
    				)
        		);
            }
        }
        return new SignerInfo(
    		signerInfo.getSID(),
            signerInfo.getDigestAlgorithm(),
            signerInfo.getAuthenticatedAttributes(),
            signerInfo.getDigestEncryptionAlgorithm(),
            signerInfo.getEncryptedDigest(),
            SigUtils.getAttributeSet(new AttributeTable(contexExpecific)) // unsignedAttr
        );

	}

}
