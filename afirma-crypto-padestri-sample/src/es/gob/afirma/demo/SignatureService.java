package es.gob.afirma.demo;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.PDFEnhancer;
import es.gob.afirma.signers.padestri.server.DocumentManager;
import es.gob.afirma.signers.padestri.server.PAdESTriPhaseSignerServerSide;
import es.gob.afirma.signers.padestri.server.PAdESTriPhaseSignerServerSide.PdfPreSignResult;
import es.gob.afirma.signers.padestri.server.SignEnhancer;


/**
 * Servlet implementation class SignatureService
 */
@WebServlet("/SignatureService")
public final class SignatureService extends HttpServlet {

	//TODO: Cambiar por una referencia al gestor documental de verdad
	private static final DocumentManager DOC_MANAGER = new FakeDocumentManager();

	private static final long serialVersionUID = 1L;

	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	// Parametros que necesitamos para la prefirma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$


	private static final String OPERATION_PRESIGN = "0"; //$NON-NLS-1$
	private static final String OPERATION_POSTSIGN = "1"; //$NON-NLS-1$

    /** Indicador de finalizaci&oacute;n correcta de proceso. */
    private static final String SUCCESS = "OK"; //$NON-NLS-1$

	// Nombres de las propiedades intercambiadas con el servidor como Properties

	/** Prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Algoritmo de firma. */
	private static final String PROPERTY_NAME_SIGN_ALGORITHM = "ALG"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Identificador interno del PDF. */
	private static final String PROPERTY_NAME_PDF_UNIQUE_ID = "PID"; //$NON-NLS-1$

	/** Momento de la firma, establecido en el servidor. */
	private static final String PROPERTY_NAME_SIGN_TIME = "TIME"; //$NON-NLS-1$


	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {
		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		if (operation == null) {
			response.getOutputStream().print("ERR-1: No se ha indicado la operacion a realizar");
			return;
		}

		final String docId = request.getParameter(PARAMETER_NAME_DOCID);
		if (docId == null) {
			response.getOutputStream().print("ERR-2: No se ha indicado el identificador del documento");
			return;
		}

		final String algorithm = request.getParameter(PARAMETER_NAME_ALGORITHM);
		if (algorithm == null) {
			response.getOutputStream().print("ERR-3: No se ha indicado el algoritmo de firma");
			return;
		}

		final String format = request.getParameter(PARAMETER_NAME_FORMAT);
		if (format == null) {
			response.getOutputStream().print("ERR-4: No se ha indicado el formato de firma");
			return;
		}

		final String cert = request.getParameter(PARAMETER_NAME_CERT);
		if (cert == null) {
			response.getOutputStream().print("ERR-5: No se ha indicado el certificado de usuario");
			return;
		}

		final Properties extraParams = new Properties();
		try {
		if (request.getParameter(PARAMETER_NAME_EXTRA_PARAM) != null) {

			Logger.getLogger("es.gob.afirma").log(Level.FINER, "ExtraParams: " + new String(Base64.decode(request.getParameter(PARAMETER_NAME_EXTRA_PARAM), Base64.URL_SAFE))); //$NON-NLS-1$ //$NON-NLS-2$

			extraParams.load(
					new ByteArrayInputStream(
						Base64.decode(request.getParameter(PARAMETER_NAME_EXTRA_PARAM), Base64.URL_SAFE)
					)
				);
		}
		} catch (final Exception e) {
			response.getOutputStream().print("ERR-6: El formato de los parametros adiciones suministrados es erroneo");
			return;
		}

		try {
			final PrintWriter out = response.getWriter();
			if (OPERATION_PRESIGN.equals(operation)) {

				final X509Certificate signerCert;
				try {
					signerCert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
						new ByteArrayInputStream(Base64.decode(cert, Base64.URL_SAFE))
					);
				}
				catch (final CertificateException e) {
					e.printStackTrace();
					out.println(e);
					out.flush();
					out.close();
					return;
				}

				// Obtenemos el PDF del gestor documental a partir del ID que nos llega por URL
				final byte[] pdfBytes = DOC_MANAGER.getDocument(docId);

				final GregorianCalendar signTime = new GregorianCalendar();

		        // Primera fase (servidor)
				final PdfPreSignResult preSignature;
		        try {
					preSignature = PAdESTriPhaseSignerServerSide.preSign(
					     AOSignConstants.getDigestAlgorithmName(algorithm),
					     pdfBytes,
					     new X509Certificate[] { signerCert },
					     signTime,
					     extraParams
					);
				}
		        catch (final AOException e) {
					out.println(e);
					out.flush();
					out.close();
					return;
				}

		        // AHora pasamos al cliente tres cosas:
		        // 1.- La prefirma para que haga el PKCS#1
		        // 2.- La fecha generada en el servidor para reutilizarla en la postfirma
		        // 3.- El ID de PDF para reutilizarlo en la postfirma
		        final Properties pre = new Properties();
		        pre.put(PROPERTY_NAME_PRESIGN, Base64.encode(preSignature.getPreSign()));
		        pre.put(PROPERTY_NAME_SIGN_TIME, Long.toString(signTime.getTimeInMillis()));
		        pre.put(PROPERTY_NAME_PDF_UNIQUE_ID, preSignature.getFileID());

		        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		        pre.store(baos, ""); //$NON-NLS-1$

				out.println(Base64.encodeBytes(baos.toByteArray(), Base64.URL_SAFE));
			}
			else if (OPERATION_POSTSIGN.equals(operation)) {

				// Obtenemos el certificado del firmante
				final X509Certificate signerCert;
				try {
					signerCert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
						new ByteArrayInputStream(Base64.decode(cert, Base64.URL_SAFE))
					);
				}
				catch (final CertificateException e) {
					e.printStackTrace();
					out.println(e);
					out.flush();
					out.close();
					return;
				}

				// Obtenemos el PDF del gestor documental a partir del ID que nos llega por URL
				final byte[] pdfBytes = DOC_MANAGER.getDocument(docId);

				// Preparo la fecha de firma
				final Calendar cal = Calendar.getInstance();
				try {
					cal.setTimeInMillis(Long.parseLong(extraParams.getProperty(PROPERTY_NAME_SIGN_TIME)));
				} catch (final NumberFormatException e) {
					Logger.getLogger("es.gob.afirma").warning("La hora de firma indicada no es valida: " + e.toString()); //$NON-NLS-1$ //$NON-NLS-2$
				}

				//TODO: Descomentar para el embellecimiento de las firmas PAdES
				//final SignEnhancer enhancer = new PDFEnhancer();
				final SignEnhancer enhancer = null;

				final Properties enhancerConfig = new Properties();

				//TODO: Externalizar opciones de configuracion
				enhancerConfig.setProperty(PDFEnhancer.APPLICATION_NAME_OPTION, "dipucr.sigem"); //$NON-NLS-1$
				enhancerConfig.setProperty(PDFEnhancer.SIGN_TYPE_OPTION, "A"); //$NON-NLS-1$

				// Ya con todos los datos hacemos la postfirma
				final byte[] signedPdf;
				try {
					signedPdf = PAdESTriPhaseSignerServerSide.postSign(
						AOSignConstants.getDigestAlgorithmName(algorithm),
						pdfBytes,
						new X509Certificate[] { signerCert },
						extraParams,
						Base64.decode(extraParams.getProperty(PROPERTY_NAME_PKCS1_SIGN)),
						Base64.decode(extraParams.getProperty(PROPERTY_NAME_PRESIGN)),
						extraParams.getProperty(PROPERTY_NAME_PDF_UNIQUE_ID),
						cal,
						enhancer,
						enhancerConfig
					);
				}
				catch (final Exception e) {
					e.printStackTrace();
					out.println(e);
					out.flush();
					out.close();
					return;
				}

				// Devolvemos al servidor documental el documento firmado
				DOC_MANAGER.storeDocument(docId, signedPdf);

				out.println(SUCCESS);
			}
			else {
				out.println("ERR-11: Operacion desconocida");
			}
			out.flush();
			out.close();
		} catch (final IOException e) {
			response.getOutputStream().println("ERR-12: Error al guardar o recuperar los datos temporales");

		}
	}
}
