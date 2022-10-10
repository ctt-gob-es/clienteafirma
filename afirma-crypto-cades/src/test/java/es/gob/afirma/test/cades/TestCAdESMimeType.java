package es.gob.afirma.test.cades;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
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
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESExtraParams;

public class TestCAdESMimeType {

	private static final String ATTR_MIMETYPE_OID = "0.4.0.1733.2.1"; //$NON-NLS-1$

	/** MimeType de los datos. */
	private static final String MIMETYPE_JPEG = "image/jpeg"; //$NON-NLS-1$

	/** OID correspondiente a datos JPEG. */
	private static final String OID_PDF = "1.2.840.10003.5.109.1"; //$NON-NLS-1$
	private static final String MIMETYPE_PDF = "application/pdf"; //$NON-NLS-1$

	private static final String DEFAULT_MIMETYPE = "application/octet-stream"; //$NON-NLS-1$

	private static final String FILENAME_JPG = "rubric.jpg"; //$NON-NLS-1$
	private static final String FILENAME_UNKNOWN = "txt"; //$NON-NLS-1$

	private static final String CERT_PATH = "PruebaEmpleado4Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "Giss2016"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$

//	private static final String CERT_PATH = "EIDAS_CERTIFICADO_PRUEBAS___99999999R.p12"; //$NON-NLS-1$
//    private static final String CERT_PASS = "1234"; //$NON-NLS-1$
//    private static final String CERT_ALIAS = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

	private byte[] jpegData;

	private byte[] unknownData;

	private PrivateKeyEntry pke;

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
	public void loadPrivateKeyEntry() throws Exception {

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
	}

	/**
	 * Comprueba que una firma CAdES sin configuraci&oacute;n relacionada con
	 * el mimetype de los datos no incluye su MimeType.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesPorDefecto() throws Exception {

		final Properties extraParams = new Properties();
		final byte[] signature = sign(this.jpegData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertNull("No se debe agregar el mimetype a las firmas CAdES por defecto", mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que no se indica que se incluya el
	 * mimetype, pero en el que s&iacute; se declara, no incluye el MimeType.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesPorDefectoIndicandoMimeType() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.CONTENT_MIME_TYPE, MIMETYPE_JPEG);
		final byte[] signature = sign(this.jpegData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertNull("No se debe agregar el mimetype a las firmas CAdES que no pidan incluirlo", mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que no se incluya el
	 * MimeType y no se indica, no incluye el MimeType.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesSinMimetype() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.FALSE.toString());
		final byte[] signature = sign(this.jpegData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertNull("No se debe agregar el mimetype a las firmas CAdES que no pidan incluirlo", mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que no se incluya el
	 * MimeType pero s&iacute; se indica uno, no incluye el MimeType.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesSinMimeTypeIndicandolo() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.FALSE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_MIME_TYPE, MIMETYPE_JPEG);
		final byte[] signature = sign(this.jpegData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertNull("No se debe agregar el mimetype a las firmas CAdES que no pidan incluirlo", mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que se incluya el
	 * MimeType y se indica uno, incluye ese MimeType.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeIndicandolo() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_MIME_TYPE, MIMETYPE_JPEG);
		final byte[] signature = sign(this.jpegData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType indicado", MIMETYPE_JPEG, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que se incluya el
	 * MimeType y no se indica uno, pero si se indica el OID de los datos,
	 * incluye el MimeType correspondiente al OID.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeIndicandoOid() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		extraParams.setProperty(CAdESExtraParams.CONTENT_TYPE_OID, OID_PDF);
		final byte[] signature = sign(this.jpegData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType del OID indicado", MIMETYPE_PDF, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que se incluya el
	 * MimeType, pero no se indica ni el MimeType ni el OID de los datos,
	 * incluye el MimeType correspondiente al an&aacute;lisis de los datos.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicar() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = sign(this.jpegData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType de los datos", MIMETYPE_JPEG, mimeType); //$NON-NLS-1$
	}

	/**
	 * Comprueba que una firma CAdES en la que se indica que se incluya el
	 * MimeType y no se indica ni el MimeType ni el OID de los datos ni se
	 * puede determinar el tipo a trav&eacute;s de los propios datos,
	 * incluye el MimeType gen&eacute;rico.
	 * @throws Exception Cuando ocurre un error en la firma.
	 */
	@Test
	public void cadesConMimeTypeSinIndicarYDatosNoReconocidos() throws Exception {
		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.INCLUDE_MIMETYPE_ATTRIBUTE, Boolean.TRUE.toString());
		final byte[] signature = sign(this.unknownData, extraParams);
		final String mimeType = getMimeType(signature);

		Assert.assertEquals("No se ha agregado el MimeType por defecto", DEFAULT_MIMETYPE, mimeType); //$NON-NLS-1$
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
				this.pke.getPrivateKey(),
				this.pke.getCertificateChain(),
				extraParams);

		return signature;
	}

	/**
	 * Extrae el atributo firmado id_aa_ets_mimeType de una firma CAdES.
	 * @param signature Firma CAdES.
	 * @return Valor del atributo firmado o {@code null} si no se encuentra.
	 * @throws Exception Cuando la firma no es CMS/CAdES o no esta bien formada.
	 */
	private static String getMimeType(final byte[] signature) throws Exception {

		final CMSSignedData s = new CMSSignedData(signature);
		final SignerInformationStore signersInfo = s.getSignerInfos();
		final SignerInformation signerInfo = signersInfo.iterator().next();

		final AttributeTable signedAttributes = signerInfo.getSignedAttributes();
		final Attribute mimetypeAttr = signedAttributes.get(new ASN1ObjectIdentifier(ATTR_MIMETYPE_OID));
		if (mimetypeAttr == null) {
			return null;
		}

		final ASN1Encodable asn1Value = mimetypeAttr.getAttributeValues()[0];

		return asn1Value.toString();
	}
}
