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

	/**
	 * Listado con el identificador de las peticiones existentes y los nombres de los ficheros asignados.
	 */
	private static final String[][] requestIds = new String[][] {
		{ "P97mQHaP6R", "Entrada.pdf" },
		{ "y7fT5o93NU", "XAdES_enveloping.xsig" },
		{ "PjZn4Qiz8G", "text.txt" },
		{ "P97mQHay25", "text.txt"},
		{ "eRM8vOTbzx", "firma_MainMenu.java.xsig"},
		{ "LbRuDmEhGI", "Firma_MainMenu.csig"},
		{ "P11111aP6R", "Entrada.xml" }
	};

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
		req2.setRequestTagId("y7fT5o93NU"); //$NON-NLS-1$
		req2.setIdentifier(f.createMobileRequestIdentifier("7GTy5L18r")); //$NON-NLS-1$
		req2.setImportanceLevel(f.createMobileRequestImportanceLevel("1")); //$NON-NLS-1$
		req2.setWorkflow(f.createMobileRequestWorkflow(Boolean.FALSE));
		req2.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req2.setRequestType("FIRMA"); //$NON-NLS-1$

		req2.setSubject(f.createMobileRequestSubject("Firma XML")); //$NON-NLS-1$

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
		doc2.setName("XAdES_Enveloping.xsig"); //$NON-NLS-1$
		doc2.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc2.setMime("text/xml"); //$NON-NLS-1$
		doc2.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_ENVELOPING));
		doc2.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc2.setSignatureParameters(f.createMobileDocumentSignatureParameters("format=XAdES Enveloping")); //$NON-NLS-1$

		final MobileDocumentList docList2 = f.createMobileDocumentList();
		docList2.getDocument().add(doc2);

		req2.setDocumentList(docList2);

		// ============== Peticion 3 ==============
		final MobileRequest req3 = f.createMobileRequest();
		req3.setRequestTagId("PjZn4Qiz8G"); //$NON-NLS-1$
		req3.setIdentifier(f.createMobileRequestIdentifier("PjZn4Qiz8G")); //$NON-NLS-1$
		req3.setImportanceLevel(f.createMobileRequestImportanceLevel("3")); //$NON-NLS-1$
		req3.setWorkflow(f.createMobileRequestWorkflow(Boolean.TRUE));
		req3.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req3.setRequestType("FIRMA"); //$NON-NLS-1$

		req3.setSubject(f.createMobileRequestSubject("Prueba XAdES")); //$NON-NLS-1$

		final MobileStringList senders3 = f.createMobileStringList();
		senders3.getStr().add("Usuario Prueba Portafirmasmobile"); //$NON-NLS-1$
		req3.setSenders(senders3);
		req3.setView("NUEVO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2015, 11, 22, 60);
			req3.setFentry(f.createMobileRequestFentry(cal));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}
		final MobileDocument doc3 = f.createMobileDocument();
		doc3.setOperationType("sign"); //$NON-NLS-1$
		doc3.setIdentifier("pV9YxbSXq5"); //$NON-NLS-1$
		doc3.setName("text.txt"); //$NON-NLS-1$
		doc3.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc3.setMime("text/plain"); //$NON-NLS-1$
		doc3.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_IMPLICITO));
		doc3.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc3.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		final MobileDocumentList docList3 = f.createMobileDocumentList();
		docList3.getDocument().add(doc3);

		req3.setDocumentList(docList3);

		// ============== Peticion 4 ==============
		final MobileRequest req4 = f.createMobileRequest();
		req4.setRequestTagId("P97mQHay25"); //$NON-NLS-1$
		req4.setIdentifier(f.createMobileRequestIdentifier("P97mQHay25")); //$NON-NLS-1$
		req4.setImportanceLevel(f.createMobileRequestImportanceLevel("2")); //$NON-NLS-1$
		req4.setWorkflow(f.createMobileRequestWorkflow(Boolean.FALSE));
		req4.setForward(f.createMobileRequestForward(Boolean.TRUE));
		req4.setRequestType("FIRMA"); //$NON-NLS-1$

		req4.setSubject(f.createMobileRequestSubject("Firma CAdES")); //$NON-NLS-1$

		final MobileStringList senders4 = f.createMobileStringList();
		senders4.getStr().add("Usuario Prueba Portafirmasmobile"); //$NON-NLS-1$
		req4.setSenders(senders4);
		req4.setView("LEIDO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal1 = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2015, 11, 1, 60);
			req4.setFentry(f.createMobileRequestFentry(cal1));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}
		final MobileDocument doc4_1 = f.createMobileDocument();
		doc4_1.setIdentifier("pV9YxbSXq5"); //$NON-NLS-1$
		doc4_1.setName("text.txt"); //$NON-NLS-1$
		doc4_1.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc4_1.setMime("text/plain"); //$NON-NLS-1$
		doc4_1.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.CADES));
		doc4_1.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc4_1.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		final MobileDocumentList docList4 = f.createMobileDocumentList();
		docList4.getDocument().add(doc4_1);

		req4.setDocumentList(docList4);

		// ============== Peticion 5 ==============
		final MobileRequest req5 = f.createMobileRequest();
		req5.setRequestTagId("eRM8vOTbzx"); //$NON-NLS-1$
		req5.setIdentifier(f.createMobileRequestIdentifier("eRM8vOTbzx")); //$NON-NLS-1$
		req5.setImportanceLevel(f.createMobileRequestImportanceLevel("1")); //$NON-NLS-1$
		req5.setWorkflow(f.createMobileRequestWorkflow(Boolean.TRUE));
		req5.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req5.setRequestType("FIRMA"); //$NON-NLS-1$

		req5.setSubject(f.createMobileRequestSubject("Cofirma XAdES")); //$NON-NLS-1$

		final MobileStringList senders5 = f.createMobileStringList();
		senders5.getStr().add("Carlos Gamuci"); //$NON-NLS-1$
		req5.setSenders(senders5);
		req5.setView("LEIDO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal1 = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2015, 11, 1, 60);
			req5.setFentry(f.createMobileRequestFentry(cal1));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}
		final MobileDocument doc5_1 = f.createMobileDocument();
		doc5_1.setIdentifier("IPExQMO8j0"); //$NON-NLS-1$
		doc5_1.setName("MainMenu.java"); //$NON-NLS-1$
		doc5_1.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc5_1.setMime("application/octet-stream"); //$NON-NLS-1$
		doc5_1.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_IMPLICITO));
		doc5_1.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc5_1.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		final MobileDocumentList docList5 = f.createMobileDocumentList();
		docList5.getDocument().add(doc5_1);

		req5.setDocumentList(docList5);

		// ============== Peticion 6 ==============

//		<rqt type="FIRMA" forward="false" workflow="true" priority="1" id="LbRuDmEhGI">
//			<subj>Contrafirma CAdES</subj>
//			<snder>Carlos Gamuci</snder>
//			<view>NUEVO</view>
//			<date>19/01/2016</date>
//			<docs>
//				<doc docid="ScDSJ93yx2">
//					<nm>MainMenu.java</nm>
//					<sz>0</sz>
//					<mmtp>application/octet-stream</mmtp>
//					<sigfrmt>CADES</sigfrmt>
//					<mdalgo>SHA1</mdalgo>
//					<params/>
//				</doc>
//			</docs>
//		</rqt>

		final MobileRequest req6 = f.createMobileRequest();
		req6.setRequestTagId("LbRuDmEhGI"); //$NON-NLS-1$
		req6.setIdentifier(f.createMobileRequestIdentifier("LbRuDmEhGI")); //$NON-NLS-1$
		req6.setImportanceLevel(f.createMobileRequestImportanceLevel("1")); //$NON-NLS-1$
		req6.setWorkflow(f.createMobileRequestWorkflow(Boolean.TRUE));
		req6.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req6.setRequestType("FIRMA"); //$NON-NLS-1$

		req6.setSubject(f.createMobileRequestSubject("Contrafirma CAdES")); //$NON-NLS-1$

		final MobileStringList senders6 = f.createMobileStringList();
		senders6.getStr().add("Carlos Gamuci"); //$NON-NLS-1$
		req6.setSenders(senders6);
		req6.setView("NUEVO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal1 = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2016, 1, 19, 60);
			req6.setFentry(f.createMobileRequestFentry(cal1));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}
		final MobileDocument doc6_1 = f.createMobileDocument();
		doc6_1.setIdentifier("ScDSJ93yx2"); //$NON-NLS-1$
		doc6_1.setName("Firma_MainMenu.csig"); //$NON-NLS-1$
		doc6_1.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc6_1.setMime("application/octet-stream"); //$NON-NLS-1$
		doc6_1.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.CADES));
		doc6_1.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc6_1.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		final MobileDocumentList docList6 = f.createMobileDocumentList();
		docList6.getDocument().add(doc6_1);

		req6.setDocumentList(docList6);

		// ============== Peticion 7 ==============
		final MobileRequest req7 = f.createMobileRequest();
		req7.setRequestTagId("P11111aP6R"); //$NON-NLS-1$
		req7.setIdentifier(f.createMobileRequestIdentifier("P11111aP6R")); //$NON-NLS-1$
		req7.setImportanceLevel(f.createMobileRequestImportanceLevel("1")); //$NON-NLS-1$
		req7.setWorkflow(f.createMobileRequestWorkflow(Boolean.FALSE));
		req7.setForward(f.createMobileRequestForward(Boolean.FALSE));
		req7.setRequestType("FIRMA"); //$NON-NLS-1$

		req7.setSubject(f.createMobileRequestSubject("XAdES Enveloped")); //$NON-NLS-1$

		final MobileStringList senders7 = f.createMobileStringList();
		senders7.getStr().add("Usuario Prueba Portafirmasmobile"); //$NON-NLS-1$
		req7.setSenders(senders7);
		req7.setView("LEIDO"); //$NON-NLS-1$

		try {
			final XMLGregorianCalendar cal1 = DatatypeFactory.newInstance().newXMLGregorianCalendarDate(2015, 11, 1, 60);
			req7.setFentry(f.createMobileRequestFentry(cal1));
		} catch (final DatatypeConfigurationException e) {
			e.printStackTrace();
		}
		final MobileDocument doc7_1 = f.createMobileDocument();
		doc7_1.setIdentifier("dt0yV11111"); //$NON-NLS-1$
		doc7_1.setName("Entrada.xml"); //$NON-NLS-1$
		doc7_1.setSize(f.createMobileDocumentSize(Integer.valueOf(0)));
		doc7_1.setMime("plain/xml"); //$NON-NLS-1$
		doc7_1.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_ENVELOPED));
		doc7_1.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
		doc7_1.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$

		final MobileDocumentList docList7 = f.createMobileDocumentList();
		docList7.getDocument().add(doc7_1);

		req7.setDocumentList(docList7);

		// ========= Listado de peticiones =========

		final MobileRequestList list = f.createMobileRequestList();
		// Numero total de peticiones en este estado (no solo las listadas)
		list.setSize(Integer.valueOf(6));
		// Agregamos los elementos creados
		list.getRequestList().add(req);
		list.getRequestList().add(req2);
		list.getRequestList().add(req3);
		list.getRequestList().add(req4);
		list.getRequestList().add(req5);
		list.getRequestList().add(req6);
		list.getRequestList().add(req7);

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

		String[] docFileIds =  null;
		for (final String[] requestList : requestIds) {
			if (requestTagId.equals(requestList[0])) {
				docFileIds = new String[requestList.length - 1];
				for (int i = 1; i < requestList.length; i++) {
					docFileIds[i - 1] = requestList[i];
				}
				break;
			}
		}

		final ObjectFactory f = new ObjectFactory();

		final MobileDocumentList docList = f.createMobileDocumentList();


		for (final String docId : docFileIds) {

			final MobileDocument doc = f.createMobileDocument();

			if (docId.equals("Entrada.pdf")) {
				doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/Entrada.pdf")))); //$NON-NLS-1$
				doc.setOperationType("sign"); //$NON-NLS-1$
				doc.setIdentifier("dt0yVh3PIO"); //$NON-NLS-1$
				doc.setName("Entrada.pdf"); //$NON-NLS-1$
				doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
				doc.setMime("application/pdf"); //$NON-NLS-1$
				doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.PDF));
				doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
				doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$
			}
			else if (docId.equals("XAdES_enveloping.xsig")) {
				doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/XAdES_enveloping.xsig")))); //$NON-NLS-1$
				doc.setOperationType("countersign"); //$NON-NLS-1$
				doc.setIdentifier("dt0yVh3XAd"); //$NON-NLS-1$
				doc.setName("XAdES_enveloping.xsig"); //$NON-NLS-1$
				doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
				doc.setMime("text/xml"); //$NON-NLS-1$
				doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_ENVELOPING));
				doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
				doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("format=XAdES Enveloping")); //$NON-NLS-1$
			}
			else if (docId.equals("text.txt")) {
				doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/text.txt")))); //$NON-NLS-1$
				doc.setOperationType("sign"); //$NON-NLS-1$
				doc.setIdentifier("pV9YxbSXq5"); //$NON-NLS-1$
				doc.setName("text.txt"); //$NON-NLS-1$
				doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
				doc.setMime("text/plain"); //$NON-NLS-1$
				doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_IMPLICITO));
				doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
				doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$
			}
			else if (docId.equals("firma_MainMenu.java.xsig")) {
				doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/firma_MainMenu.java.xsig")))); //$NON-NLS-1$
				doc.setOperationType("cosign"); //$NON-NLS-1$
				doc.setIdentifier("IPExQMO8j0"); //$NON-NLS-1$
				doc.setName("firma_MainMenu.java.xsig"); //$NON-NLS-1$
				doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
				doc.setMime("application/octet-stream"); //$NON-NLS-1$
				doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_IMPLICITO));
				doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
				doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("mode=implicit")); //$NON-NLS-1$
			}
			else if (docId.equals("Firma_MainMenu.csig")) {
				doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/" + docId)))); //$NON-NLS-1$
				doc.setOperationType("countersign"); //$NON-NLS-1$
				doc.setIdentifier("ScDSJ93yx2"); //$NON-NLS-1$
				doc.setName(docId);
				doc.setSize(f.createMobileDocumentSize(Integer.valueOf(82500)));
				doc.setMime("application/octet-stream"); //$NON-NLS-1$
				doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.CADES));
				doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
				doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("mode=implicit")); //$NON-NLS-1$
			}
			else if (docId.equals("Entrada.xml")) {
				doc.setData(f.createMobileDocumentData(new DataHandler(this.getClass().getResource("/" + docId)))); //$NON-NLS-1$
				doc.setOperationType("sign"); //$NON-NLS-1$
				doc.setIdentifier("dt0yV11111"); //$NON-NLS-1$
				doc.setName(docId);
				doc.setSize(f.createMobileDocumentSize(Integer.valueOf(227)));
				doc.setMime("plain/xml"); //$NON-NLS-1$
				doc.setSignatureType(f.createMobileDocumentSignatureType(MobileSignFormat.XADES_ENVELOPED));
				doc.setSignAlgorithm(f.createMobileDocumentSignAlgorithm("SHA1")); //$NON-NLS-1$
				doc.setSignatureParameters(f.createMobileDocumentSignatureParameters("")); //$NON-NLS-1$
			}

			docList.getDocument().add(doc);
		}
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
