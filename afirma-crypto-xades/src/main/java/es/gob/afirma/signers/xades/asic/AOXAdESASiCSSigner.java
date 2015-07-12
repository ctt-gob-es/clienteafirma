package es.gob.afirma.signers.xades.asic;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.xades.AOXAdESSigner;

/** Manejador de firmas XML XAdES ASiC-S.
 * Todas las firmas XAdES ASiC-S se generan de forma <i>externally detached</i> con MANIFEST para evitar problemas de resoluci&oacute;n en
 * las referencias, y todas contienen el nodo <code>KeyName</code> de la secci&oacute;n <code>KeyInfo</code>.
 * <p>
 *  Debido a errores en algunas versiones del entorno de ejecuci&oacute;n de Java, esta clase puede generar ocasionalmente
 *  mensajes en consola del tipo: <code>[Fatal Error] :1:1: Content is not allowed in prolog.</code>. Estos
 *  deben ignorarse, ya que no indican ninguna condici&oacute;n de error ni malfuncionamiento.
 * </p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOXAdESASiCSSigner implements AOSigner {

	@Override
	public byte[] sign(final byte[] data,
					   final String algorithm,
					   final PrivateKey key,
					   final Certificate[] certChain,
					   final Properties xParams) throws AOException,
					                                    IOException {

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final String dataFilename = extraParams.getProperty("asicsFilename") != null ? //$NON-NLS-1$
										extraParams.getProperty("asicsFilename") : //$NON-NLS-1$
											ASiCUtil.getASiCSDefaultDataFilename(data);

		// Siempre con MANIFEST
		extraParams.put("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$

		// Aprovechamos para anadir atributos utiles
		extraParams.put("addKeyInfoKeyName", "true"); //$NON-NLS-1$ //$NON-NLS-2$

		// La URI de referencia es el nombre de fichero dentro del ASiC
		extraParams.put("uri", dataFilename); //$NON-NLS-1$

		// Siempre <i>Externally Detached</i>
		extraParams.put("format", AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED); //$NON-NLS-1$

		final byte[] xadesSignature = new AOXAdESSigner().sign(data, algorithm, key, certChain, extraParams);

		System.out.println(new String(xadesSignature));

		throw new UnsupportedOperationException("Aun no implementado"); //$NON-NLS-1$
	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException,
			                                                  IOException {
		throw new UnsupportedOperationException("Aun no implementado"); //$NON-NLS-1$
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException,
			                                                  IOException {
		throw new UnsupportedOperationException("Aun no implementado"); //$NON-NLS-1$
	}

	@Override
	public byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties extraParams) throws AOException,
			                                                       IOException {
		throw new UnsupportedOperationException("Aun no implementado"); //$NON-NLS-1$
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign,
			                               final boolean asSimpleSignInfo) throws AOInvalidFormatException,
			                                                                      IOException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isSign(final byte[] is) throws IOException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isValidDataFile(final byte[] is) throws IOException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public byte[] getData(final byte[] signData) throws AOException, IOException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOException,
			IOException {
		// TODO Auto-generated method stub
		return null;
	}

}
