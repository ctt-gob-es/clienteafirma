package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.xadestri.server.XAdESTriPhaseSignerServerSide;
import es.gob.afirma.signers.xadestri.server.XmlPreSignException;
import es.gob.afirma.signers.xadestri.server.XmlPreSignResult;

final class XAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	private static enum Op {
		SIGN,
		COSIGN
	}

	/** Nombre de la propiedad del PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Nombre de la propiedad de los datos a firmar. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Nombre de la propiedad de los sesi&oacute;n necesarios para completar la firma. */
	private static final String PROPERTY_NAME_SESSION_DATA = "SESSION"; //$NON-NLS-1$

	@Override
	public byte[] preProcessPreSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {
		return preProcessPre(data, algorithm, cert, extraParams, Op.SIGN);
	}

	@Override
	public byte[] preProcessPreCoSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {
		return preProcessPre(data, algorithm, cert, extraParams, Op.COSIGN);
	}

	private static byte[] preProcessPre(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final Op op) throws IOException, AOException {

		XmlPreSignResult preSignature;
		try {
			switch (op) {
			case SIGN:
				preSignature = XAdESTriPhaseSignerServerSide.preSign(data, algorithm, new X509Certificate[] { cert }, extraParams, XAdESTriPhaseSignerServerSide.Op.SIGN);
				break;
			case COSIGN:
				preSignature = XAdESTriPhaseSignerServerSide.preSign(data, algorithm, new X509Certificate[] { cert }, extraParams, XAdESTriPhaseSignerServerSide.Op.COSIGN);
				break;
			default:
				throw new IllegalStateException("No se puede dar una operacion no contemplada en el enumerado de operaciones"); //$NON-NLS-1$
			}
		}
		catch (final InvalidKeyException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final SignatureException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final SAXException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final ParserConfigurationException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final MarshalException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final XMLSignatureException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final XmlPreSignException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}

		// Ahora pasamos al cliente 2 cosas:
		// 1.- La prefirma (datos a firmar) para que haga el PKCS#1
		// 2.- Los datos de sesion que coinciden con la firma XML sin PKCS#1

		final StringBuilder builder = new StringBuilder();
		builder.append(PROPERTY_NAME_PRESIGN).append("=").append(preSignature.getSignedInfoBase64()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		builder.append(PROPERTY_NAME_SESSION_DATA).append("=").append(preSignature.getXmlSignBase64()); //$NON-NLS-1$

		//Logger.getLogger("es.gob.afirma").info("Respuesta prefirma:\n" + builder.toString());

		return builder.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws NoSuchAlgorithmException,
			AOException,
			IOException {
		return preProcessPost(extraParams);
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws NoSuchAlgorithmException,
			AOException,
			IOException {
		return preProcessPost(extraParams);
	}

	private static byte[] preProcessPost(final Properties extraParams) throws IOException {
		if (extraParams == null) {
			throw new IllegalArgumentException("La propiedades adicionales no pueden ser nulas en la postfirma"); //$NON-NLS-1$
		}

		final String pkcs1Base64Sign = extraParams.getProperty(PROPERTY_NAME_PKCS1_SIGN);
		if (pkcs1Base64Sign == null) {
			throw new IllegalArgumentException("La propiedades adicionales no contienen la firma PKCS#1"); //$NON-NLS-1$
		}

		return new String(Base64.decode(extraParams.getProperty(PROPERTY_NAME_SESSION_DATA))).replace(
				XAdESTriPhaseSignerServerSide.REPLACEMENT_STRING, pkcs1Base64Sign.trim()).getBytes();
	}

	@Override
	public byte[] preProcessPreCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams,
			final CounterSignTarget targets) throws IOException, AOException {
		throw new UnsupportedOperationException("La operacion de contrafirma trifasica no esta soportada."); //$NON-NLS-1$
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams,
			final CounterSignTarget targets) throws NoSuchAlgorithmException,
			AOException, IOException {
		throw new UnsupportedOperationException("La operacion de contrafirma trifasica no esta soportada."); //$NON-NLS-1$
	}

}
