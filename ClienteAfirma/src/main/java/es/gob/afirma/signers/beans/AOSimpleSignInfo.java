/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.beans;

import java.security.cert.X509Certificate;
import java.text.DateFormat;
import java.util.Date;

import es.gob.afirma.misc.AOUtil;

/**
 * Clase dise&ntilde;ada para almacenar la informaci&oacute;n m&iacute;nima
 * extraida de un objeto simple de firma.
 */
public final class AOSimpleSignInfo {

	/** Cadena de certificaci&oacute;n. */
	private X509Certificate[] certs = null;

	/** Algoritmo de firma. */
	private String signAlgorithm = null;

	/** Formato de firma. */
	private String signFormat = null;

	/** Momento de la firma segundo el dispositivo que la realiz&oacute;. */
	private Date signingTime = null;

	/** Momento de la firma seg&uacte;n un sello uan TSA. */
	private Date[] timestampingTime = null;

	/** Cadena binaria con el PKCS#1 de la firma individual. */
	private byte[] pkcs1 = null;

	/**
	 * Construye un objeto con la informaci&oacute;n b&aacute;sica:
	 * <ul>
	 * <li>La cadena de certificaci&oacute;n (obligatoria).</li>
	 * <li>La fecha de firma (opcional).</li>
	 * </ul>
	 * Si no se dispone de la cadena de certificaci&oacute;n completa se
	 * indicar&aacute; al menos el certificado usado para la firma.
	 * 
	 * @param chainCert
	 *            Cadena de certificaci&oacute;n.
	 * @param signingTime
	 *            Momento de la firma.
	 */
	public AOSimpleSignInfo(final X509Certificate[] chainCert,
			final Date signingTime) {

		if (chainCert == null || chainCert.length == 0 || chainCert[0] == null) {
			throw new NullPointerException(
					"No se ha introducido la cadena de certificaci&oacute;n.");
		}

		this.certs = chainCert;
		this.signingTime = signingTime;
	}

	public String getSignAlgorithm() {
		return signAlgorithm;
	}

	public void setSignAlgorithm(String algorithm) {
		this.signAlgorithm = algorithm;
	}

	public String getSignFormat() {
		return signFormat;
	}

	public void setSignFormat(final String format) {
		this.signFormat = format;
	}

	public Date[] getTimestampingTime() {
		return timestampingTime;
	}

	public void setTimestampingTime(final Date[] timestampingTime) {
		this.timestampingTime = timestampingTime;
	}

	public X509Certificate[] getCerts() {
		return certs;
	}

	public Date getSigningTime() {
		return signingTime;
	}

	/**
	 * Indica si la firma dispone de un sello de tiempo.
	 * 
	 * @return Devuelve <code>true</code> si la firma tiene un sello de tiempo,
	 *         <code>false</code> en caso contrario.
	 */
	public boolean isTimeStamped() {
		return this.timestampingTime != null
				&& this.timestampingTime.length > 0
				&& this.timestampingTime[0] != null;
	}

	/**
	 * Recupera el PKCS#1 de la firma en cuesti&oacute;n. Devuelve {@code null}
	 * si no se preestablecio.
	 * 
	 * @return PKCS#1 de la firma.
	 */
	public byte[] getPkcs1() {
		return pkcs1;
	}

	/**
	 * Establece el PKCS#1 de la firma.
	 * 
	 * @param pkcs1
	 *            PKCS#1 que gener&oacute; la firma.
	 */
	public void setPkcs1(byte[] pkcs1) {
		this.pkcs1 = pkcs1;
	}
	
	@Override
	public String toString() {
	    String desc = AOUtil.getCN(certs[0]); 
	    if (timestampingTime != null && timestampingTime.length > 0 && timestampingTime[0] != null)
	        desc += " (TimeStamp: " + DateFormat.getDateInstance(DateFormat.DEFAULT).format(signingTime) + ")";
	    else if (signingTime != null)
	        desc += " (" + DateFormat.getDateInstance(DateFormat.DEFAULT).format(signingTime) + ")";  

	    return desc;
	}
}
