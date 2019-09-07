package es.gob.afirma.triphase.signer.xades;

import java.security.InvalidAlgorithmParameterException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.spec.ECGenParameterSpec;
import java.security.spec.ECParameterSpec;
import java.util.Dictionary;
import java.util.Enumeration;
import java.util.Hashtable;

import org.spongycastle.asn1.x9.ECNamedCurveTable;
import org.spongycastle.asn1.x9.X9ECParameters;
import org.spongycastle.jcajce.provider.asymmetric.util.EC5Util;
import org.spongycastle.jce.provider.BouncyCastleProvider;

final class KeyHelperEcdsa implements KeyHelper {

	static {
		final Provider p = Security.getProvider(BouncyCastleProvider.PROVIDER_NAME);
		if (p == null) {
			Security.addProvider(new BouncyCastleProvider());
		}
	}

	private static final String ECDSA = "ECDSA"; //$NON-NLS-1$

	private static final Dictionary<String, PrivateKey> KEYS = new Hashtable<>();

	KeyHelperEcdsa() {
		// Vacio y 'package protected'
	}

	@Override
	public PrivateKey getPrivateKey(final PublicKey puK) throws NoSuchAlgorithmException {
		if (puK == null) {
			throw new IllegalArgumentException(
				"La clave publica no puede ser nula" //$NON-NLS-1$
			);
		}
		final String curveName = deriveCurveName(puK);
		PrivateKey ret = KEYS.get(curveName);
		if (ret == null) {
			ret = generatePrivateKey(curveName);
			KEYS.put(curveName, ret);
		}
		return ret;
	}

	private static PrivateKey generatePrivateKey(final String curveName) throws NoSuchAlgorithmException {
		final ECGenParameterSpec ecGenSpec = new ECGenParameterSpec(curveName);
		final KeyPairGenerator g = KeyPairGenerator.getInstance(ECDSA);
		try {
			g.initialize(ecGenSpec, new SecureRandom());
		}
		catch (final InvalidAlgorithmParameterException e) {
			throw new NoSuchAlgorithmException(
				"Error inicializando el generador de claves con el nombre de curva '" + curveName + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		final KeyPair pair = g.generateKeyPair();
		return pair.getPrivate();
	}

	private static final String deriveCurveName(final PublicKey publicKey) throws NoSuchAlgorithmException {
	    if(publicKey instanceof java.security.interfaces.ECPublicKey){
	        final java.security.interfaces.ECPublicKey pk = (java.security.interfaces.ECPublicKey) publicKey;
	        final ECParameterSpec params = pk.getParams();
	        return deriveCurveName(EC5Util.convertSpec(params, false));
	    }
	    else if(publicKey instanceof org.spongycastle.jce.interfaces.ECPublicKey) {
	        final org.spongycastle.jce.interfaces.ECPublicKey pk = (org.spongycastle.jce.interfaces.ECPublicKey) publicKey;
	        return deriveCurveName(pk.getParameters());
	    }
	    else {
	    	throw new IllegalArgumentException(
				"Solo se admiten claves de curva eliptica, pero se ha recibido una de tipo: " + publicKey.getClass().getName() //$NON-NLS-1$
			);
		}
	}

	private static final String deriveCurveName(final org.spongycastle.jce.spec.ECParameterSpec ecParameterSpec) throws NoSuchAlgorithmException {
	    for (final Enumeration names = ECNamedCurveTable.getNames(); names.hasMoreElements();) {
	        final String name = (String) names.nextElement();
	        final X9ECParameters params = ECNamedCurveTable.getByName(name);
	        if (params.getN().equals(ecParameterSpec.getN())         &&
	        	params.getH().equals(ecParameterSpec.getH())         &&
	        	params.getCurve().equals(ecParameterSpec.getCurve()) &&
	        	params.getG().equals(ecParameterSpec.getG())) {
	            	return name;
	        }
	    }
	    throw new NoSuchAlgorithmException(
    		"No se ha podido determinar el nombre de la curva eliptica de la clave" //$NON-NLS-1$
		);
	}

}
