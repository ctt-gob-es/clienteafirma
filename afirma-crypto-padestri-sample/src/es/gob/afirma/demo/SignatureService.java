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
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_SECONDPHASE = "second"; //$NON-NLS-1$


	private static final String OPERATION_PRESIGN = "0"; //$NON-NLS-1$
	private static final String OPERATION_POSTSIGN = "1"; //$NON-NLS-1$

	private static final String SIGN_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

    private final static Properties p1 = new Properties();
    static {
        try {
			p1.load(SignatureService.class.getResourceAsStream("signature.properties")); //$NON-NLS-1$
		}
        catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se han podido cargar las propiedades de firma, se usaran los valores por defecto: " + e //$NON-NLS-1$
			);
		}
    }

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

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doGet(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {
		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		try (final PrintWriter out = response.getWriter()) {
			if (OPERATION_PRESIGN.equals(operation)) {

				final String docId = request.getParameter(PARAMETER_NAME_DOCID);

				final X509Certificate signerCert;
				try {
					signerCert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
						new ByteArrayInputStream(Base64.decode(request.getParameter(PARAMETER_NAME_CERT), Base64.URL_SAFE))
					);
				}
				catch (final CertificateException e) {
					out.println(e);
					e.printStackTrace();
					return;
				}

				// Obtenemos el PDF del gestor documental a partir del ID que nos llega por URL
				final byte[] pdfBytes = DOC_MANAGER.getDocument(docId);

				final GregorianCalendar signTime = new GregorianCalendar();

		        // Primera fase (servidor)
				final PdfPreSignResult preSignature;
		        try {
					preSignature = PAdESTriPhaseSignerServerSide.preSign(
					     AOSignConstants.getDigestAlgorithmName(SIGN_ALGORITHM),
					     pdfBytes,
					     new X509Certificate[] { signerCert },
					     signTime,
					     p1
					);
				}
		        catch (final AOException e) {
					out.println(e);
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
		        pre.put(PROPERTY_NAME_SIGN_ALGORITHM, SIGN_ALGORITHM);

		        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		        pre.store(baos, ""); //$NON-NLS-1$

				out.println(Base64.encodeBytes(baos.toByteArray(), Base64.URL_SAFE));
			}
			else if (OPERATION_POSTSIGN.equals(operation)) {

				// Obtenemos el certificado del firmante
				final X509Certificate signerCert;
				try {
					signerCert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
						new ByteArrayInputStream(Base64.decode(request.getParameter(PARAMETER_NAME_CERT), Base64.URL_SAFE))
					);
				}
				catch (final CertificateException e) {
					out.println(e);
					e.printStackTrace();
					return;
				}

				// Obtenemos el identificador del documento a firmar
				final String docId = request.getParameter(PARAMETER_NAME_DOCID);

				// Obtenemos el PDF del gestor documental a partir del ID que nos llega por URL
				final byte[] pdfBytes = DOC_MANAGER.getDocument(docId);

				final Properties secPhaseResult = new Properties();
				secPhaseResult.load(
					new ByteArrayInputStream(
						Base64.decode(request.getParameter(PARAMETER_NAME_SECONDPHASE), Base64.URL_SAFE)
					)
				);

				// Preparo la feca de firma
				final Calendar cal = Calendar.getInstance();
				cal.setTimeInMillis(Long.parseLong(secPhaseResult.getProperty(PROPERTY_NAME_SIGN_TIME)));

				final SignEnhancer enhancer = new PDFEnhancer();

				final Properties enhancerConfig = new Properties();

				//TODO: Externalizar opciones de configuracion
				enhancerConfig.setProperty(PDFEnhancer.APPLICATION_NAME_OPTION, "dipucr.sigem"); //$NON-NLS-1$
				enhancerConfig.setProperty(PDFEnhancer.SIGN_TYPE_OPTION, "A"); //$NON-NLS-1$

				// Ya con todos los datos hacemos la postfirma
				final byte[] signedPdf;
				try {
					signedPdf = PAdESTriPhaseSignerServerSide.postSign(
						AOSignConstants.getDigestAlgorithmName(SIGN_ALGORITHM),
						pdfBytes,
						new X509Certificate[] { signerCert },
						p1,
						Base64.decode(secPhaseResult.getProperty(PROPERTY_NAME_PKCS1_SIGN)),
						Base64.decode(secPhaseResult.getProperty(PROPERTY_NAME_PRESIGN)),
						secPhaseResult.getProperty(PROPERTY_NAME_PDF_UNIQUE_ID),
						cal,
						enhancer,
						enhancerConfig
					);
				}
				catch (final Exception e) {
					out.println(e);
					e.printStackTrace();
					return;
				}

				// Devolvemos al servidor documental el documento firmado
				DOC_MANAGER.storeDocument(docId, signedPdf);

				out.println(SUCCESS);
			}
			else {
				out.println("Operacion desconocida"); //$NON-NLS-1$
			}
		}
	}


}
