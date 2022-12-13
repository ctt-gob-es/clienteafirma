package es.gob.afirma.signers.multi.cades;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Iterator;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.cms.CMSSignedData;
import org.spongycastle.cms.SignerInformation;
import org.spongycastle.cms.SignerInformationStore;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESExtraParams;

public class TestCosignMimeType {

	private static final String ATTR_MIMETYPE_OID = "0.4.0.1733.2.1"; //$NON-NLS-1$

	/** MimeType de los datos. */
	private static final String MIMETYPE_JPEG = "image/jpeg"; //$NON-NLS-1$


	private static final String OID_PDF = "1.2.840.10003.5.109.1"; //$NON-NLS-1$
	private static final String MIMETYPE_PDF = "application/pdf"; //$NON-NLS-1$

	private static final String OID_PNG = "1.2.840.10003.5.109.7"; //$NON-NLS-1$
	private static final String MIMETYPE_PNG = "image/png"; //$NON-NLS-1$
	private static final String DESCRIPTION_PNG = "Imagen PNG"; //$NON-NLS-1$

	private static final String DEFAULT_MIMETYPE = "application/octet-stream"; //$NON-NLS-1$

	private static final String FILENAME_JPG = "rubric.jpg"; //$NON-NLS-1$
	private static final String FILENAME_UNKNOWN = "txt"; //$NON-NLS-1$

	private static final String SIGN_CERT_PATH = "PruebaEmpleado4Activo.p12"; //$NON-NLS-1$
    private static final String SIGN_CERT_PASS = "Giss2016"; //$NON-NLS-1$
    private static final String SIGN_CERT_ALIAS = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$

    private static final String CERT_PATH_TO_COSIGN = "00_empleado_publico-hsm.p12"; //$NON-NLS-1$
    private static final String CERT_PASS_TO_COSIGN = "12345"; //$NON-NLS-1$
    private static final String CERT_ALIAS_TO_COSIGN = "nombre apellido1 apellido2 - dni 12345678z"; //$NON-NLS-1$

	private byte[] jpegData;
	private byte[] unknownData;

	private byte[] signatureImplicitWithMimeTypePng;
	private byte[] signatureImplicitWithoutMimeType;
	private byte[] signatureExplicitWithoutMimeType;
	private byte[] signatureExplicitWithContentTypePng;
	private byte[] signatureExplicitWithContentTypeAndMimeTypePng;
	private byte[] signatureImplicitWithoutMimeTypeUnknownData;

	private String signatureSignerSid;

	private PrivateKeyEntry signPke;
	private PrivateKeyEntry cosignPke;

	@Before
	public void loadData() throws IOException {
		this.jpegData = loadFileData(FILENAME_JPG);
		this.unknownData = loadFileData(FILENAME_UNKNOWN);
	}

	private static byte[] loadFileData(final String filename) throws IOException {

		byte[] data;
		try (InputStream is = ClassLoader.getSystemResourceAsStream(filename)) {
			data = AOUtil.getDataFromInputStream(is);
		}
		return data;
	}

	@Before
	public void loadPrivateKeyEntries() throws Exception {

		KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(SIGN_CERT_PATH)
		) {
			ks.load(is, SIGN_CERT_PASS.toCharArray());
		}
		this.signPke = (PrivateKeyEntry) ks.getEntry(SIGN_CERT_ALIAS, new KeyStore.PasswordProtection(SIGN_CERT_PASS.toCharArray()));

		ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH_TO_COSIGN)
		) {
			ks.load(is, CERT_PASS_TO_COSIGN.toCharArray());
		}
		this.cosignPke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS_TO_COSIGN, new KeyStore.PasswordProtection(CERT_PASS_TO_COSIGN.toCharArray()));
	}

	@Before
	public void generateSignatures() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.MODE, AOSignConstants.SIGN_MODE_IMPLICIT);
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_MIME_TYPE, MIMETYPE_PNG);
		this.signatureImplicitWithMimeTypePng = sign(this.jpegData, extraParams);

		// Identificamos el ID del firmante que se usara en todas las firmas
		final CMSSignedData s = new CMSSignedData(this.signatureImplicitWithMimeTypePng);
		final SignerInformationStore signersInfo = s.getSignerInfos();
		final SignerInformation signerInfo = signersInfo.iterator().next();
		this.signatureSignerSid = signerInfo.getSID().getSerialNumber().toString();

		extraParams.clear();
		extraParams.setProperty(CAdESExtraParams.MODE, AOSignConstants.SIGN_MODE_IMPLICIT);
		this.signatureImplicitWithoutMimeType = sign(this.jpegData, extraParams);

		extraParams.clear();
		extraParams.setProperty(CAdESExtraParams.MODE, AOSignConstants.SIGN_MODE_EXPLICIT);
		this.signatureExplicitWithoutMimeType = sign(this.jpegData, extraParams);

		extraParams.clear();
		extraParams.setProperty(CAdESExtraParams.MODE, AOSignConstants.SIGN_MODE_EXPLICIT);
		extraParams.setProperty(CAdESExtraParams.CONTENT_TYPE_OID, OID_PNG);
		extraParams.setProperty(CAdESExtraParams.CONTENT_DESCRIPTION, DESCRIPTION_PNG);
		this.signatureExplicitWithContentTypePng = sign(this.jpegData, extraParams);

		extraParams.clear();
		extraParams.setProperty(CAdESExtraParams.MODE, AOSignConstants.SIGN_MODE_EXPLICIT);
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_MIME_TYPE, MIMETYPE_PNG);
		extraParams.setProperty(CAdESExtraParams.CONTENT_TYPE_OID, OID_PNG);
		extraParams.setProperty(CAdESExtraParams.CONTENT_DESCRIPTION, DESCRIPTION_PNG);
		this.signatureExplicitWithContentTypeAndMimeTypePng = sign(this.jpegData, extraParams);

		extraParams.clear();
		extraParams.setProperty(CAdESExtraParams.MODE, AOSignConstants.SIGN_MODE_IMPLICIT);
		this.signatureImplicitWithoutMimeTypeUnknownData = sign(this.unknownData, extraParams);
	}

	/**
	 * Comprueba que una firma CAdES sin configuraci&oacute;n relacionada con
	 * el mimetype de los datos no incluye su MimeType.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesPorDefecto() throws Exception {

		final Properties extraParams = new Properties();
		final byte[] cosignature = cosign(this.signatureImplicitWithMimeTypePng, extraParams);
		final String mimeType = getMimeType(cosignature);

		Assert.assertNull("No se debe agregar el mimetype a las cofirmas CAdES por defecto", mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una cofirma CAdES en la que se indica que se incluya el
	 * MimeType y se indica uno, incluye ese MimeType.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeIndicandolo() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_MIME_TYPE, MIMETYPE_PDF);
		final byte[] signature = cosign(this.signatureImplicitWithMimeTypePng, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType indicado", MIMETYPE_PDF, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una cofirma CAdES en la que se indica que se incluya el
	 * MimeType y no se indica uno, pero si se indica el OID de los datos,
	 * incluye el MimeType correspondiente al OID.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeIndicandoOid() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_TYPE_OID, OID_PDF);
		final byte[] signature = cosign(this.signatureImplicitWithMimeTypePng, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType del OID indicado", MIMETYPE_PDF, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una cofirma CAdES en la que se indica que se incluya el
	 * MimeType, pero no se indica ni el MimeType ni el OID de los datos,
	 * incluye el MimeType de la firma original.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicarYFirmaConEl() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = cosign(this.signatureImplicitWithMimeTypePng, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType de los datos", MIMETYPE_PNG, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una cofirma CAdES en la que se indica que se incluya el
	 * MimeType, pero no se indica ni el MimeType ni el OID de los datos,
	 * y en el que la firma original tenida un OID,  incluye el MimeType
	 * correspondiente al OID de de la firma.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicarYFirmaConOid() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = cosign(this.signatureExplicitWithContentTypePng, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType de los datos", MIMETYPE_PNG, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una cofirma CAdES en la que se indica que se incluya el
	 * MimeType, pero no se indica ni el MimeType ni el OID de los datos,
	 * incluye el MimeType y el OID de la firma original.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicarYFirmaConMimeTypeyOid() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = cosign(this.signatureExplicitWithContentTypeAndMimeTypePng, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType de los datos", MIMETYPE_PNG, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una cofirma CAdES en la que se indica que se incluya el
	 * MimeType y no se indica ni el MimeType ni el OID de los datos ni se
	 * puede determinar el tipo a trav&eacute;s de los propios datos,
	 * incluye el MimeType gen&eacute;rico.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicarYFirmaImplicitaSinTipo() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = cosign(this.signatureImplicitWithoutMimeTypeUnknownData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType por defecto", DEFAULT_MIMETYPE, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que se incluya el
	 * MimeType y no se indica ni el MimeType ni el OID de los datos, ni se
	 * encuentra informaci&oacute;n en la firma previa, ni contiene esta los
	 * datos firmados, incluye el MimeType gen&eacute;rico.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicarYFirmaDatosDesconocidosSinTipo() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = cosign(this.signatureImplicitWithoutMimeTypeUnknownData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType por defecto", DEFAULT_MIMETYPE, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que se incluya el
	 * MimeType y no se indica ni el MimeType ni el OID de los datos, cuando
	 * la firma no incluye los datos ni el mimetype, pero si el OID, incluye el
	 * MimeType correspondiente al OID del Content Type.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicarYFirmaSinDatosNiMimeType() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = cosign(this.signatureExplicitWithoutMimeType, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType por defecto", MIMETYPE_JPEG, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que se incluya el
	 * MimeType y no se indica ni el MimeType ni el OID de los datos ni se
	 * puede determinar el tipo a trav&eacute;s de los propios datos,
	 * incluye el MimeType gen&eacute;rico.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void contrafirmaConMimeType() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_MIME_TYPE, MIMETYPE_JPEG);
		final byte[] signature = countersign(this.signatureImplicitWithMimeTypePng, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertNull("Se ha agregado un MimeType a la contrafirma", mimeType); //$NON-NLS-1$
	}

	/**
	 * Genera una firma CAdES de los datos con la configuraci&oacute;n de firma indicada.
	 * @param data Datos a firmar.
	 * @param extraParams Configuraci&oacute;n de firma.
	 * @return Firma CAdES.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	private byte[] sign(final byte[] data, final Properties extraParams) throws Exception {

		final AOSigner signer = new AOCAdESSigner();
		final byte[] signature = signer.sign(
				data,
				"SHA512withRSA", //$NON-NLS-1$
				this.signPke.getPrivateKey(),
				this.signPke.getCertificateChain(),
				extraParams);

		return signature;
	}

	/**
	 * Genera una cofirma CAdES con la configuraci&oacute;n de firma indicada.
	 * @param signature Firma a cofirmar.
	 * @param extraParams Configuraci&oacute;n para la cofirma.
	 * @return Cofirma CAdES.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	private byte[] cosign(final byte[] signature, final Properties extraParams) throws Exception {

		final AOSigner signer = new AOCAdESSigner();
		final byte[] cosignature = signer.cosign(
				signature,
				"SHA512withRSA", //$NON-NLS-1$
				this.cosignPke.getPrivateKey(),
				this.cosignPke.getCertificateChain(),
				extraParams);

		return cosignature;
	}

	/**
	 * Genera una cofirma CAdES con la configuraci&oacute;n de firma indicada.
	 * @param signature Firma a cofirmar.
	 * @param extraParams Configuraci&oacute;n para la cofirma.
	 * @return Cofirma CAdES.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	private byte[] countersign(final byte[] signature, final Properties extraParams) throws Exception {

		final AOSigner signer = new AOCAdESSigner();
		final byte[] cosignature = signer.countersign(
				signature,
				"SHA512withRSA", //$NON-NLS-1$
				CounterSignTarget.LEAFS,
				null,
				this.cosignPke.getPrivateKey(),
				this.cosignPke.getCertificateChain(),
				extraParams);

		return cosignature;
	}

	/**
	 * Extrae el atributo firmado id_aa_ets_mimeType de una cofirma CAdES
	 * asegurandose de no extraer el de la firma original.
	 * @param signature Cofirma CAdES.
	 * @return Valor del atributo firmado o {@code null} si no se encuentra.
	 * @throws Exception Cuando la firma no es CMS/CAdES o no esta bien formada.
	 */
	private String getMimeType(final byte[] signature) throws Exception {

		final CMSSignedData s = new CMSSignedData(signature);
		final SignerInformationStore signersInfo = s.getSignerInfos();
		final Iterator<SignerInformation> signersIt = signersInfo.iterator();

		while (signersIt.hasNext()) {
			final SignerInformation signerInfo = signersIt.next();
			if (!signerInfo.getSID().getSerialNumber().toString().equals(this.signatureSignerSid)) {
				final AttributeTable signedAttributes = signerInfo.getSignedAttributes();
				final Attribute mimetypeAttr = signedAttributes.get(new ASN1ObjectIdentifier(ATTR_MIMETYPE_OID));
				if (mimetypeAttr == null) {
					return null;
				}
				final ASN1Encodable asn1Value = mimetypeAttr.getAttributeValues()[0];
				return asn1Value.toString();
			}
		}
		return null;
	}

}

