package es.gob.afirma.keyone;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.cert.signvalidation.SignValiderFactory;
import es.gob.afirma.cert.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.SmartCardException;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.keystores.filters.PolicyIdFilter;
import es.gob.afirma.signers.pades.BadPdfPasswordException;
import es.gob.afirma.signers.pades.PdfHasUnregisteredSignaturesException;
import es.gob.afirma.signers.pades.PdfIsCertifiedException;
import es.gob.afirma.signers.pades.PdfUtil;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

public final class KeyOneUtil {

	private static final String SEPARATOR = ","; //$NON-NLS-1$
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	public static String enumSignatureFieldNames(final String filePath) throws PdfException {
		final StringBuilder sb = new StringBuilder();
        try ( final InputStream fis = new FileInputStream(new File(filePath)) ) {
        	final byte[] data = AOUtil.getDataFromInputStream(fis);
        	final List<SignatureField> fields = PdfUtil.getPdfEmptySignatureFields(data);
        	for (final SignatureField field : fields) {
        		sb.append(field.getName());
        		sb.append(SEPARATOR);
        	}
        	return sb.toString();
        }
        catch (final Exception e) {
        	LOGGER.severe("Error recuperando los nombres de campos de firma del PDF: " + e); //$NON-NLS-1$
        	throw new PdfException("Error recuperando los nombres de campos de firma del pdf: " + e, e); //$NON-NLS-1$
        }
	}

	public static void addBlankPage(final String filePath) throws PdfException {
        try ( final ByteArrayOutputStream baos = new ByteArrayOutputStream() ) {
        	final PdfReader pdfReader = new PdfReader(filePath);
        	final Calendar cal = Calendar.getInstance();
        	final PdfStamper stp = new PdfStamper(pdfReader, baos, cal);
        	stp.insertPage(pdfReader.getNumberOfPages() + 1, pdfReader.getPageSizeWithRotation(1));
        	stp.close(cal);
        	pdfReader.close();
        	final FileOutputStream os = new FileOutputStream(new File(filePath));
        	os.write(baos.toByteArray());
        	os.close();
        }
        catch(final Exception e) {
        	LOGGER.severe("Error anadiendo pagina en blanco al PDF: " + e); //$NON-NLS-1$
        	throw new PdfException("Error a&ntilde;adiendo pagina en blanco al documento PDF: " + e, e); //$NON-NLS-1$
        }
	}

	public static void pdfSign(final String originalPath,
								   final String destinyPath,
								   final String policyIdentifier,
								   final String fieldName,
								   final String tsaName,
								   final String xmlLook) throws PdfException,
																XMLException,
																AOCertificatesNotFoundException,
																BadPdfPasswordException,
																PdfIsCertifiedException,
																PdfHasUnregisteredSignaturesException {

		final AOSigner signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_PADES);

		byte[] data = null;
		try ( final InputStream fis = new FileInputStream(new File(originalPath)); ) {
        	data = AOUtil.getDataFromInputStream(fis);
		}
		catch (final Exception e) {
			LOGGER.severe("Error leyendo fichero de entrada: " + e); //$NON-NLS-1$
			throw new PdfException("Error leyendo fichero de entrada: " + e, e); //$NON-NLS-1$
		}
		SignatureField field = null;
		if (fieldName != null && !fieldName.isEmpty()) {
			final List<SignatureField> list = PdfUtil.getPdfEmptySignatureFields(data);
			for (final SignatureField sf : list) {
				if (sf.getName().equals(fieldName)) {
					field = sf;
				}
			}
		}

		final Properties p = new Properties();

		PolicyIdFilter policyFilter = null;
		if (policyIdentifier != null && !policyIdentifier.isEmpty()) {
			policyFilter = new PolicyIdFilter(policyIdentifier);
			p.setProperty("policyIdentifier", policyIdentifier); //$NON-NLS-1$
		}
		if (tsaName != null && !tsaName.isEmpty()) {
			p.setProperty("tsaPolicy", tsaName); //$NON-NLS-1$
		}

		ArrayList<CertificateFilter> filters = null;
		if (policyFilter != null) {
			filters = new ArrayList<>();
			filters.add(policyFilter);
		}
		final PrivateKeyEntry pke;
        try {
            pke = getPrivateKeyEntry(filters);
        }
        catch (final AOCancelledOperationException e) {
        	throw new AOCancelledOperationException("Cancelado por el usuario: " + e, e); //$NON-NLS-1$
        }
        catch(final AOCertificatesNotFoundException e) {
        	LOGGER.severe("El almacen no contiene ningun certificado que se pueda usar para firmar: " + e); //$NON-NLS-1$
        	throw new AOCertificatesNotFoundException("El almacen no contiene ningun certificado que se pueda usar para firmar: " + e); //$NON-NLS-1$
        }
        catch (final Exception e) {
        	LOGGER.severe("Ocurrio un error al extraer la clave privada del certificiado seleccionado: " + e); //$NON-NLS-1$
        	throw new PdfException("Ocurrio un error al extraer la clave privada del certificiado seleccionado: " + e, e); //$NON-NLS-1$
    	}

        final String signatureAlgorithm = PreferencesManager.get(
    		PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, "SHA512withRSA" //$NON-NLS-1$
		);

        new XMLLookParser(xmlLook, field, p, pke).parse();

        final byte[] signResult;
        try {
            signResult = signer.sign(
        		data,
        		signatureAlgorithm,
        		pke.getPrivateKey(),
                pke.getCertificateChain(),
                p
            );
            final FileOutputStream os = new FileOutputStream(new File(destinyPath));
        	os.write(signResult);
        	os.close();
        }
        catch(final AOCancelledOperationException e) {
        	throw new AOCancelledOperationException("Cancelado por el usuario: " + e, e); //$NON-NLS-1$
        }
        catch(final PdfIsCertifiedException e) {
        	LOGGER.severe("PDF no firmado por estar certificado: " + e); //$NON-NLS-1$
        	throw new PdfIsCertifiedException();
        }
        catch(final BadPdfPasswordException e) {
        	LOGGER.severe("PDF protegido con contrasena mal proporcionada: " + e); //$NON-NLS-1$
        	throw new BadPdfPasswordException(e);
        }
        catch(final PdfHasUnregisteredSignaturesException e) {
        	LOGGER.severe("PDF con firmas no registradas: " + e); //$NON-NLS-1$
        	throw new PdfHasUnregisteredSignaturesException();
        }
        catch(final OutOfMemoryError ooe) {
            LOGGER.severe("Falta de memoria en el proceso de firma: " + ooe); //$NON-NLS-1$
            throw new OutOfMemoryError("Falta de memoria en el proceso de firma: " + ooe); //$NON-NLS-1$
        }
        catch(final Exception e) {
            LOGGER.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
            throw new PdfException("Error durante el proceso de firma: " + e, e); //$NON-NLS-1$
        }
	}

	public static boolean verifySignature(final String filePath) throws PdfException {
		byte[] sign = null;
		try ( final FileInputStream fis = new FileInputStream(new File(filePath)) ) {
			sign = AOUtil.getDataFromInputStream(fis);
			return SignValiderFactory.getSignValider(sign).validate(sign).getValidity().equals(SIGN_DETAIL_TYPE.OK);
		}
		catch(final Exception e) {
			LOGGER.severe("Error validando la firma del PDF: " + e); //$NON-NLS-1$
			throw new PdfException("Error validando la firma del PDF: " + e, e); //$NON-NLS-1$
		}
	}

	public static PrivateKeyEntry getPrivateKeyEntry(final List<? extends CertificateFilter> filters) throws UnrecoverableEntryException,
																											  AOKeyStoreManagerException,
																											  AOCertificatesNotFoundException,
																											  KeyStoreException,
																											  NoSuchAlgorithmException
	{
		final AOKeyStoreManager ksm = SimpleKeyStoreManager.getKeyStore(false, null);
		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(
				ksm,
				null,
				true,             // Comprobar claves privadas
				false,            // Mostrar certificados caducados
				true,             // Comprobar validez temporal del certificado
				filters, 				// Filtros
				false             // mandatoryCertificate
			);
    	dialog.show();
    	ksm.setParentComponent(null);
    	return ksm.getKeyEntry(
			dialog.getSelectedAlias()
		);
	}

	public static String cnTarjeta() throws SmartCardException{
		AOKeyStoreManager ksm = null;
		try {
			ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			    AOKeyStore.TEMD, // Store
			    null, // Lib
			    "TEMD (Tarjeta del Ministerio de Defensa)", // Description //$NON-NLS-1$
				null, // PasswordCallback
				null // Parent
			);
		}
		catch (final Exception e) {
			LOGGER.severe("Error recuperando el almacen de tarjetas del Ministerio de Defensa: " + e); //$NON-NLS-1$
			throw new SmartCardException("Error recuperando el almacen de tarjetas del Ministerio de Defensa: " + e, e); //$NON-NLS-1$
		}
		final String[] aliases = ksm.getAliases();
		for (final String al : aliases) {
			System.out.println(al);
		}
		if (aliases.length != 1) {
			LOGGER.severe("Hay mas de una tarjeta de defensa insertada"); //$NON-NLS-1$
			throw new SmartCardException("Hay mas de una tarjeta de defensa insertada"); //$NON-NLS-1$
		}
		return AOUtil.getCN(ksm.getCertificate(aliases[0]));
	}

}
