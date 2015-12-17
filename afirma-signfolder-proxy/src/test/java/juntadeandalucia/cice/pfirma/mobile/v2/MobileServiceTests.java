package juntadeandalucia.cice.pfirma.mobile.v2;

import javax.activation.DataHandler;
import javax.jws.WebService;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileApplication;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileApplicationList;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileDocSignInfoList;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileDocument;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileDocumentList;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileRequest;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileRequestFilterList;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileRequestList;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileSignFormat;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileSignLine;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileSignLineList;
import juntadeandalucia.cice.pfirma.mobile.type.v2.MobileStringList;
import juntadeandalucia.cice.pfirma.mobile.type.v2.ObjectFactory;

/**
 * WebService para la emulacion de los servicios del portafirmas Web con objeto de pruebas.
 */
@WebService(
		serviceName = "MobileService",
		targetNamespace = "urn:juntadeandalucia:cice:pfirma:mobile:v2.0",
		portName = "MobileServicePort",
		wsdlLocation = "MobileService.wsdl",
		endpointInterface = "juntadeandalucia.cice.pfirma.mobile.v2.MobileService")
public class MobileServiceTests implements MobileService {

	@Override
	public MobileRequestList queryRequestList(final byte[] certificate, final String state,
			final String initPage, final String pageSize, final MobileStringList signFormats,
			final MobileRequestFilterList filters) throws MobileException {

		System.out.println("Llamada al WS queryRequestList"); //$NON-NLS-1$

		/* =========== XML de respuesta =============
		<?xml version="1.0" encoding="UTF-8"?>
		<list n='5'>
			<rqt id="P97mQHaP6R" priority="1" workflow="false" forward="false" type="FIRMA">
				<subj>Firma PDF</subj>
				<snder>Usuario Prueba Portafirmasmobile</snder>
				<view>LEIDO</view>
				<date>01/12/2015</date>
				<docs>
					<doc docid="dt0yVh3PIO">
						<nm>Entrada.pdf</nm>
						<sz>0</sz>
						<mmtp>application/pdf</mmtp>
						<sigfrmt>PDF</sigfrmt>
						<mdalgo>SHA1</mdalgo>
						<params></params>
					</doc>
				</docs>
			</rqt><rqt id="5t6VDh78B5" priority="1" workflow="false" forward="false" type="FIRMA"><subj>Paralelo</subj><snder>Domingo Sánchez Pina</snder><view>LEIDO</view><date>19/11/2015</date><docs><doc docid="dt0yVh3PIO"><nm>Documento 1.pdf</nm><sz>0</sz><mmtp>application/pdf</mmtp><sigfrmt>PDF</sigfrmt><mdalgo>SHA1</mdalgo><params></params></doc></docs></rqt><rqt id="LeXBi4CIw1" priority="1" workflow="true" forward="false" type="FIRMA"><subj>Comentarios con 2 firmantes</subj><snder>Carlos Muñoz López</snder><view>LEIDO</view><date>13/11/2015</date><docs><doc docid="3pCO4mptrd"><nm>maestras_v4.docx</nm><sz>0</sz><mmtp>application/vnd.openxmlformats-officedocument.wordprocessingml.document</mmtp><sigfrmt>PDF</sigfrmt><mdalgo>SHA1</mdalgo><params></params></doc></docs></rqt><rqt id="V30GBTFJ7m" priority="1" workflow="false" forward="false" type="FIRMA"><subj>Firma PDF</subj><snder>Usuario Prueba Portafirmasmobile</snder><view>LEIDO</view><date>05/11/2015</date><docs><doc docid="BtgtOb09Jf"><nm>Entrada.pdf</nm><sz>0</sz><mmtp>application/pdf</mmtp><sigfrmt>PDF</sigfrmt><mdalgo>SHA1</mdalgo><params></params></doc></docs></rqt><rqt id="0fQCpKFJx1" priority="1" workflow="false" forward="false" type="FIRMA"><subj>Firma PDF cascada</subj><snder>Usuario Prueba Portafirmasmobile</snder><view>LEIDO</view><date>05/11/2015</date><docs><doc docid="DBEZKLyxMR"><nm>Entrada.pdf</nm><sz>0</sz><mmtp>application/pdf</mmtp><sigfrmt>PDF</sigfrmt><mdalgo>SHA1</mdalgo><params></params></doc></docs></rqt></list>
		 */

		final ObjectFactory f = new ObjectFactory();

		// ============== Peticion 1 ==============
		final MobileRequest req = f.createMobileRequest();
		req.setRequestTagId("P97mQHaP6R"); //$NON-NLS-1$
		req.setIdentifier(f.createMobileRequestIdentifier("P97mQHaP6R")); //$NON-NLS-1$
		req.setImportanceLevel(f.createMobileRequestImportanceLevel("1")); //$NON-NLS-1$
		req.setWorkflow(f.createMobileRequestWorkflow(Boolean.FALSE));
		req.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req.setRequestType("FIRMA"); //$NON-NLS-1$

		req.setSubject(f.createMobileRequestSubject("Firma PDF")); //$NON-NLS-1$

		final MobileStringList senders1 = f.createMobileStringList();
		senders1.getStr().add("Usuario Prueba Portafirmasmobile"); //$NON-NLS-1$
		req.setSenders(senders1);
		req.setView("LEIDO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal1 = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2015, 11, 1, 60);
			req.setFentry(f.createMobileRequestFentry(cal1));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}
		final MobileDocument doc = f.createMobileDocument();
		doc.setIdentifier("dt0yVh3PIO"); //$NON-NLS-1$
		doc.setName("Entrada.pdf"); //$NON-NLS-1$
		doc.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc.setMime("application/pdf"); //$NON-NLS-1$
		doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.PDF));
		doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		final MobileDocumentList docList = f.createMobileDocumentList();
		docList.getDocument().add(doc);

		req.setDocumentList(docList);


		// ============== Peticion 2 ==============
		final MobileRequest req2 = f.createMobileRequest();
		req2.setRequestTagId("P97mQHaP6R"); //$NON-NLS-1$
		req2.setIdentifier(f.createMobileRequestIdentifier("P97mQHaP6R")); //$NON-NLS-1$
		req2.setImportanceLevel(f.createMobileRequestImportanceLevel("1")); //$NON-NLS-1$
		req2.setWorkflow(f.createMobileRequestWorkflow(Boolean.FALSE));
		req2.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req2.setRequestType("FIRMA"); //$NON-NLS-1$

		req2.setSubject(f.createMobileRequestSubject("Firma PDF")); //$NON-NLS-1$

		final MobileStringList senders2 = f.createMobileStringList();
		senders2.getStr().add("Usuario Prueba Portafirmasmobile"); //$NON-NLS-1$
		req2.setSenders(senders2);
		req2.setView("LEIDO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal1 = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2015, 11, 1, 60);
			req2.setFentry(f.createMobileRequestFentry(cal1));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}
		final MobileDocument doc2 = f.createMobileDocument();
		doc2.setOperationType("countersign"); //$NON-NLS-1$
		doc2.setIdentifier("dt0yVh3XAd"); //$NON-NLS-1$
		doc2.setName("XAdES.xsig"); //$NON-NLS-1$
		doc2.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc2.setMime("text/xml"); //$NON-NLS-1$
		doc2.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_ENVELOPING));
		doc2.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc2.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		final MobileDocumentList docList2 = f.createMobileDocumentList();
		docList2.getDocument().add(doc2);

		req2.setDocumentList(docList);

		final MobileRequestList list = f.createMobileRequestList();
		// Numero total de peticiones en este estado (no solo las listadas)
		list.setSize(Integer.valueOf(5));
		// Agregamos los elementos creados
		list.getRequestList().add(req);
		list.getRequestList().add(req2);

		System.out.println("Tamano listado: " + list.getRequestList().size()); //$NON-NLS-1$

		return list;
	}

	@Override
	public MobileRequest queryRequest(final byte[] certificate, final String requestId)
			throws MobileException {

		System.out.println("Llamada al WS queryRequest"); //$NON-NLS-1$

		/* =========== XML de ejemplo ===========
		 <?xml version="1.0" encoding="UTF-8"?>
		 <dtl id="5t6VDh78B5" priority="1" workflow="false" forward="false" type="FIRMA">
		 	<subj>Paralelo</subj>
		 	<snders>
		 		<snder>Domingo Sánchez Pina</snder>
		 	</snders>
		 	<date>19/11/2015  13:33</date>
		 	<app>PORTAFIRMAS</app>
		 	<ref></ref>
		 	<sgnlines>
		 		<sgnline>
		 			<rcvr>Domingo Sánchez Pina</rcvr>
		 		</sgnline>
		 		<sgnline>
		 			<rcvr>Usuario Prueba Portafirmasmobile</rcvr>
		 		</sgnline>
		 	</sgnlines>
		 	<docs>
		 		<doc docid="dt0yVh3PIO">
		 			<nm>Documento 1.pdf</nm>
		 			<sz>82500</sz>
		 			<mmtp>application/pdf</mmtp>
		 			<sigfrmt>PDF</sigfrmt>
		 			<mdalgo>SHA1</mdalgo>
		 			<params></params>
		 		</doc>
		 	</docs>
		 </dtl>
		 */

		final ObjectFactory f = new ObjectFactory();
		final MobileRequest req = f.createMobileRequest();

		req.setIdentifier(f.createMobileRequestIdentifier("P97mQHaP6R")); //$NON-NLS-1$
		req.setImportanceLevel(f.createMobileRequestImportanceLevel("1")); //$NON-NLS-1$
		req.setWorkflow(f.createMobileRequestWorkflow(Boolean.FALSE));
		req.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req.setRequestType("FIRMA"); //$NON-NLS-1$

		req.setSubject(f.createMobileRequestSubject("Firma PDF")); //$NON-NLS-1$

		final MobileStringList senders1 = f.createMobileStringList();
		senders1.getStr().add("Usuario Prueba Portafirmasmobile"); //$NON-NLS-1$
		req.setSenders(senders1);
		req.setView("LEIDO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal1 = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2015, 11, 1, 60);
			req.setFentry(f.createMobileRequestFentry(cal1));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}

		req.setApplication(f.createMobileRequestApplication("PORTAFIRMAS")); //$NON-NLS-1$
		req.setRef(f.createMobileRequestRef("x3OJB0Spt0")); //$NON-NLS-1$

		final MobileSignLineList signLineList = f.createMobileSignLineList();
		final MobileSignLine line = f.createMobileSignLine();
		final MobileStringList signLine = f.createMobileStringList();
		signLine.getStr().add("Domingo Sánchez Pina"); //$NON-NLS-1$
		line.setMobileSignerList(f.createMobileSignLineMobileSignerList(signLine));
		signLineList.getMobileSignLine().add(line);

		req.setSignLineList(signLineList);

		final MobileDocumentList docList = f.createMobileDocumentList();

		final MobileDocument doc = f.createMobileDocument();
		doc.setOperationType("sign"); //$NON-NLS-1$
		doc.setIdentifier("dt0yVh3PIO"); //$NON-NLS-1$
		doc.setName("Entrada.pdf"); //$NON-NLS-1$
		doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
		doc.setMime("application/pdf"); //$NON-NLS-1$
		doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.PDF));
		doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		docList.getDocument().add(doc);
		req.setDocumentList(docList);

		return req;
	}

	@Override
	public MobileDocument documentPreview(final byte[] certificate, final String documentId)
			throws MobileException {

		System.out.println("Llamada al WS documentPreview"); //$NON-NLS-1$

		final ObjectFactory f = new ObjectFactory();
		final MobileDocument doc = f.createMobileDocument();
		doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/Entrada.pdf")))); //$NON-NLS-1$
		doc.setOperationType("sign"); //$NON-NLS-1$
		doc.setIdentifier("dt0yVh3PIO"); //$NON-NLS-1$
		doc.setName("Entrada.pdf"); //$NON-NLS-1$
		doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
		doc.setMime("application/pdf"); //$NON-NLS-1$
		doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.PDF));
		doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		return doc;
	}

	@Override
	public MobileDocumentList getDocumentsToSign(final byte[] certificate,
			final String requestTagId) throws MobileException {

		System.out.println("Llamada al WS getDocumentsToSign"); //$NON-NLS-1$

		final ObjectFactory f = new ObjectFactory();

		final MobileDocumentList docList = f.createMobileDocumentList();

		final MobileDocument doc = f.createMobileDocument();
		doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/Entrada.pdf")))); //$NON-NLS-1$
		doc.setOperationType("sign"); //$NON-NLS-1$
		doc.setIdentifier("dt0yVh3PIO"); //$NON-NLS-1$
		doc.setName("Entrada.pdf"); //$NON-NLS-1$
		doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
		doc.setMime("application/pdf"); //$NON-NLS-1$
		doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.PDF));
		doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		docList.getDocument().add(doc);

		return docList;
	}

	@Override
	public String saveSign(final byte[] certificate, final String requestTagId,
			final MobileDocSignInfoList docSignInfoList) throws MobileException {

		System.out.println("Llamada al WS saveSign"); //$NON-NLS-1$

		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String rejectRequest(final byte[] certificate, final String requestId,
			final String textRejection) throws MobileException {

		System.out.println("Llamada al WS rejectRequest"); //$NON-NLS-1$

		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public MobileApplicationList queryApplicationsMobile(final byte[] certificate) throws MobileException {

		System.out.println("Llamada al WS queryApplicationsMobile"); //$NON-NLS-1$

		/* ================ XML de respuesta ===================
		 <?xml version="1.0" encoding="UTF-8"?>
		 <appConf>
		 	<app id="ACCEDA">Aplicación Acceda</app>
		 	<app id="APLIFICTICIA">Aplicación Ficticia</app>
		 	<app id="AURA">Aplicación AURA</app>
		 	<app id="BADARAL">Aplicación Badaral</app>
		 	<app id="CONVOCA">Aplicación CONVOCA</app>
		 	<app id="DROGAS">Aplicación Drogas</app>
		 	<app id="EXTRANJERIA">Aplicación Extranjería</app>
		 	<app id="FUNCIONA">Aplicación Funciona</app>
		 	<app id="GEXCO">Aplicación GEXCO</app>
		 	<app id="HABILITADOS">Habilitados</app>
		 	<app id="HABCERTIFICADOS">Aplicación que recibe los certificados enviados por la aplicación de Habilitados</app>
		 	<app id="INFORMES_DTIC">Usuario Informes DTIC</app>
		 	<app id="JURADOS">Aplicación Jurados</app>
		 	<app id="JURADOS_PDF">Aplicación Jurados PDF</app>
		 	<app id="MANIFESTACIONES">Aplicación Manifestaciones</app>
		 	<app id="PFIRMA">PORTAFIRMAS</app>
		 	<app id="PFIRMA_CADES">PORTAFIRMA CADES</app>
		 	<app id="PFIRMA_PADES">PORTAFIRMA PADES</app>
		 	<app id="PFIRMA_XADES">PORTAFIRMA XADES</app>
		 	<app id="PFIRMA_XADES_ENVELOPED">PORTAFIRMA XADES ENVELOPED</app>
		 	<app id="PF_DGA">Aplicación Portafirma Delegación del Gobierno en Andalucía</app>
		 	<app id="PORTALCECIR">Aplicación PORTALCECIR</app>
		 	<app id="REGTELEMATICO">Aplicación Registro telemático</app>
		 	<app id="RGECO">Aplicación RGECO</app>
		 	<app id="SANCIONES_APP_DSIC">Aplicación Sanciones</app>
		 	<app id="SIRES">Aplicación SIRES</app>
		 	<app id="TASAS">Aplicación Tasas</app>
		 	<app id="TRIBUNALES">Aplicación TRIBUNALES</app>
		 </appConf>
		 */

		final String[][] apps = new String[][] {
				{"ACCEDA", "Aplicación Acceda"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"APLIFICTICIA", "Aplicación Ficticia"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"AURA", "Aplicación AURA"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"BADARAL", "Aplicación Badaral"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"CONVOCA", "Aplicación CONVOCA"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"DROGAS", "Aplicación Drogas"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"EXTRANJERIA", "Aplicación Extranjería"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"FUNCIONA", "Aplicación Funciona"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"GEXCO", "Aplicación GEXCO"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"HABILITADOS", "Habilitados"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"HABCERTIFICADOS", "Aplicación que recibe los certificados enviados por la aplicación de Habilitados"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"INFORMES_DTIC", "Usuario Informes DTIC"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"JURADOS", "Aplicación Jurados"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"JURADOS_PDF", "Aplicación Jurados PDF"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"MANIFESTACIONES", "Aplicación Manifestaciones"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"PFIRMA", "PORTAFIRMAS"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"PFIRMA_CADES", "PORTAFIRMA CADES"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"PFIRMA_PADES", "PORTAFIRMA OADES"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"PFIRMA_XADES", "PORTAFIRMA XADES"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"PFIRMA_XADES_ENVELOPED", "PORTAFIRMA XADES ENVELOPED"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"PF_DGA", "Aplicación Portafirma Delegación del Gobierno en Andalucía"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"PORTALCECIR", "Aplicación PORTALCECIR"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"REGTELEMATICO", "Aplicación Registro telemático"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"RGECO", "Aplicación RGECO"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"SANCIONES_APP_DSIC", "Aplicación Sanciones"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"SIRES", "Aplicación SIRES"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"TASAS", "Aplicación Tasas"}, //$NON-NLS-1$ //$NON-NLS-2$
				{"TRIBUNALES", "Aplicación TRIBUNALES"} //$NON-NLS-1$ //$NON-NLS-2$
		};

		final MobileApplicationList list = new MobileApplicationList();
		for (final String[] app : apps) {
			list.getApplicationList().add(newMobileApplication(app[0], app[1]));
		}
		return list;
	}

	private static MobileApplication newMobileApplication(final String id, final String name) {
		final MobileApplication app = new MobileApplication();
		app.setId(id);
		app.setName(name);
		return app;
	}

	@Override
	public String approveRequest(final byte[] certificate, final String requestTagId)
			throws MobileException {

		System.out.println("Llamada al WS approveRequest"); //$NON-NLS-1$

		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public MobileDocument signPreview(final byte[] certificate, final String documentId)
			throws MobileException {

		System.out.println("Llamada al WS signPreview"); //$NON-NLS-1$

		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public MobileDocument reportPreview(final byte[] certificate, final String documentId)
			throws MobileException {

		System.out.println("Llamada al WS reportPreview"); //$NON-NLS-1$

		// TODO Auto-generated method stub
		return null;
	}

}
