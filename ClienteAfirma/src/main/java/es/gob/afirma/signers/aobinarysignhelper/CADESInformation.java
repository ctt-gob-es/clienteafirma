/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.InputStream;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

/**
 * Clase que obtiene la informaci&oacute;n de los distintos tipos de firma para
 * CADES a partir de un fichero pasado por par&aacute;metro.
 * 
 * La informaci&oacute;n es para los tipo:
 * 
 * <ul>
 * <li>Data</li>
 * <li>Signed Data</li>
 * <li>Digested Data</li>
 * <li>Encrypted Data</li>
 * <li>Enveloped Data</li>
 * <li>Signed and Enveloped Data</li>
 * </ul>
 */
public final class CADESInformation {

	/**
	 * M&eacute;todo principal que obtiene la informaci&oacute;n a partir de un
	 * fichero firmado de tipo CADES.
	 * 
	 * @param data
	 *            Fichero firmado
	 * @return Informaci&oacute;n del fichero firmado
	 */
	public String getInformation(InputStream data) {
		String datos = "";

		try {
			ASN1InputStream is = new ASN1InputStream(data);
			// LEEMOS EL FICHERO QUE NOS INTRODUCEN
			ASN1Sequence dsq = (ASN1Sequence) is.readObject();
			Enumeration<?> e = dsq.getObjects();
			// Elementos que contienen los elementos OID Data
			DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
			// Contenido a obtener informacion
			ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
			if (doi.equals(PKCSObjectIdentifiers.data)) {
				datos = Utils.getFromData();
			} else if (doi.equals(PKCSObjectIdentifiers.digestedData)) {
				datos = getFromDigestedData(doj);
			} else if (doi.equals(PKCSObjectIdentifiers.encryptedData)) {
				datos = Utils.extractData(doj, "5", "Tipo: Encrypted\n",
						"CADES");
			} else if (doi.equals(PKCSObjectIdentifiers.signedData)) {
				datos = Utils.extractData(doj, "4", "Tipo: SignedData\n",
						"CADES");
			} else if (doi.equals(PKCSObjectIdentifiers.envelopedData)) {
				datos = Utils.extractData(doj, "0", "Tipo: EnvelopedData\n",
						"CADES");
			} else if (doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)) {
				datos = Utils.extractData(doj, "3",
						"Tipo: SignedAndEnvelopedData\n", "CADES");
			} else {
				Logger.getLogger("es.gob.afirma").warning(
						"No se reconoce el tipo");
			}
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
					"Error intentando reconocer el tipo: " + e);
		}
		return datos;
	}

	/**
	 * Obtiene la informaci&oacute;n de un tipo Digested Data.
	 * 
	 * @return Representaci&oacute;n de los datos.
	 */
	private String getFromDigestedData(ASN1TaggedObject doj) {
		String detalle = "";
		detalle = detalle + "Tipo: DigestedData\n";

		// obtenemos el digestedData
		DigestedData dd = new DigestedData((ASN1Sequence) doj.getObject());

		// obtenemos la version
		detalle = detalle + "Version: " + dd.getVersion() + "\n";

		// obtenemos el algoritmo
		final AlgorithmIdentifier ai = dd.getDigestAlgorithm();
		detalle = detalle + "Algoritmo de firma: " + ai.getAlgorithm() + "\n";

		// obtenemos el tipo de contenido
		detalle = detalle + "Tipo de Contenido: "
				+ dd.getContentInfo().getContentType() + "\n";

		return detalle;
	}

}