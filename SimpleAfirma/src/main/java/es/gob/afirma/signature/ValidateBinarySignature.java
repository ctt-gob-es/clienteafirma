package es.gob.afirma.signature;

import java.util.logging.Logger;

import es.gob.afirma.signature.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signature.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.signers.AOCAdESSigner;
import es.gob.afirma.signers.AOCMSSigner;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.standalone.SimpleAfirma;

/**
 * Validador de firmas Adobe PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class ValidateBinarySignature {
	
    /**
     * Valida una firma binaria (CMS/CAdES).
     * @param sign Firma binaria
     * @return <code>true</code> si la firma es v&aacute;lida, <code>false</code> en caso contrario
     * @throws Exception Si ocurre cualquier problema durante la validaci&oacute;n
     */
    public static SignValidity validate(final byte[] sign) {
    	if (sign == null) {
    		throw new NullPointerException("La firma a validar no puede ser nula"); //$NON-NLS-1$
    	}
    	
    	AOSigner signer = new AOCMSSigner();
    	if (!signer.isSign(sign)) {
    	    signer = new AOCAdESSigner();
    	    if (!signer.isSign(sign)) {
    	        return new SignValidity(SIGN_DETAIL_TYPE.KO, null);
    	    }
    	}
    	
    	try {
    	    byte[] data = signer.getData(sign);
    	    if (data == null) {
    	        throw new Exception("La firma no contiene los datos firmados");
    	    }
    	} catch (Exception e) {
    	    if (SimpleAfirma.DEBUG) {
    	        Logger.getLogger("es.gob.afirma").info("No se ha podido extraer los datos de la firma: " + e);
    	    }
    	    return new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.NO_DATA);
        }
    	
    	return new SignValidity(SIGN_DETAIL_TYPE.OK, null);
    	
//    	//FIXME: Hay que extraer el algoritmo de la propia firma
//        Signature signature = Signature.getInstance("SHA1withRSA");
//        
//        boolean result = true;
//        TreeModel tree = signer.getSignersStructure(sign, true);
//        for (int i = 0; result && i < tree.getChildCount(tree.getRoot()); i++) {
//            try {
//                if (!verifySignNode(signature, data, tree, (TreeNode) tree.getChild(tree.getRoot(), i))) {
//                    result = false;
//                }
//            } catch (Exception e) {
//                result = false;
//            }
//        }
//        
//    	return result;
    }
    
//    private static boolean verifySignNode(Signature signature, byte[] data, TreeModel tree, TreeNode node) throws InvalidKeyException, SignatureException {
//        boolean result = verifySign(signature, data, (AOSimpleSignInfo) node.getUserObject());
//        for (int i = 0; result && i < tree.getChildCount(node); i++) {
//            if (!verifySignNode(signature, data, tree, (TreeNode) tree.getChild(node, i))) {
//                result = false;
//            }
//        }
//        return result;
//    }
//    
//    private static boolean verifySign(Signature signature, byte[] data, AOSimpleSignInfo signInfo) throws InvalidKeyException, SignatureException {
//        signature.initVerify(signInfo.getCerts()[0].getPublicKey());
//        signature.update(data);
//        return signature.verify(signInfo.getPkcs1());
//    }
    
//    public static void main(String[] args) throws Exception {
//        
//        
//        SignValidity result = ValidateBinarySignature.validate(
//                AOUtil.getDataFromInputStream(AOUtil.loadFile(
//                        AOUtil.createURI("C:\\Users\\A122466\\Desktop\\Pendiente_txt.csig"), null, false)));
//        
//        System.out.println(result.getValidity());
//        System.out.println("-----");
//    }
}
