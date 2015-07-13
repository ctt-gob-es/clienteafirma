package es.gob.afirma.signers.xades.asic;

import java.io.IOException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.Properties;
import java.util.logging.Logger;

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

	private static final Logger LOGGER = Logger.getLogger("es.agob.afirma"); //$NON-NLS-1$

	@Override
	public byte[] sign(final byte[] data,
					   final String algorithm,
					   final PrivateKey key,
					   final Certificate[] certChain,
					   final Properties xParams) throws AOException,
					                                    IOException {

		final Properties extraParams = setASiCProperties(xParams, data);

		final byte[] xadesSignature = new AOXAdESSigner().sign(
			data,
			algorithm,
			key,
			certChain,
			extraParams
		);

		return ASiCUtil.createSContainer(
			xadesSignature,
			data,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			extraParams.getProperty("asicsFilename") //$NON-NLS-1$
		);

	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties extraParams) throws AOException,
			                                                  IOException {
		return cosign(sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException,
			                                                  IOException {
		// Extraemos firma y datos del ASiC
		final byte[] packagedData = ASiCUtil.getASiCSData(sign);
		final byte[] packagedSign = ASiCUtil.getASiCSXMLSignature(sign);

		final Properties extraParams = setASiCProperties(xParams, packagedData);

		// Creamos la contrafirma
		final byte[] newCoSign = new AOXAdESSigner().cosign(
			packagedData,
			packagedSign,
			algorithm,
			key,
			certChain,
			extraParams
		);

		return ASiCUtil.createSContainer(
			newCoSign,
			packagedData,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			ASiCUtil.getASiCSDataFilename(sign)
		);
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
		// Extraemos firma y datos del ASiC
		final byte[] packagedData = ASiCUtil.getASiCSData(sign);
		final byte[] packagedSign = ASiCUtil.getASiCSXMLSignature(sign);

		// Creamos la contrafirma
		final byte[] newCounterSign = new AOXAdESSigner().countersign(
			packagedSign,
			algorithm,
			targetType,
			targets,
			key,
			certChain,
			extraParams
		);

		return ASiCUtil.createSContainer(
			newCounterSign,
			packagedData,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			ASiCUtil.getASiCSDataFilename(sign)
		);

	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign,
			                               final boolean asSimpleSignInfo) throws AOInvalidFormatException,
			                                                                      IOException {
		return new AOXAdESSigner().getSignersStructure(
			ASiCUtil.getASiCSXMLSignature(sign),
			asSimpleSignInfo
		);
	}

	@Override
	public boolean isSign(final byte[] is) throws IOException {
		final byte[] sign;
		try {
			sign = ASiCUtil.getASiCSXMLSignature(is);
		}
		catch(final Exception e) {
			LOGGER.info("La firma proporcionada no es XAdES ASiC-S: " + e); //$NON-NLS-1$
			return false;
		}
		return new AOXAdESSigner().isSign(
			ASiCUtil.getASiCSXMLSignature(sign)
		);
	}

	@Override
	public boolean isValidDataFile(final byte[] data) throws IOException {
		if (data == null) {
			LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".asics"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public byte[] getData(final byte[] signData) throws AOException, IOException {
		return ASiCUtil.getASiCSData(signData);
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] signData) throws AOException,
			                                                    IOException {
		return new AOXAdESASiCSSigner().getSignInfo(ASiCUtil.getASiCSXMLSignature(signData));
	}

	private static Properties setASiCProperties(final Properties xParams, final byte[] data) {

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

		// Raiz de ASiC
		extraParams.put("RootXmlNodeName", "asic:XAdESSignatures"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.put("RootXmlNodeNamespace", "http://uri.etsi.org/02918/v1.2.1#"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.put("RootXmlNodeNamespacePrefix", "xmlns:asic"); //$NON-NLS-1$ //$NON-NLS-2$

		return extraParams;

	}

}
