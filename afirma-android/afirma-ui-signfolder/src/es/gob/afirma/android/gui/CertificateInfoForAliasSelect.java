package es.gob.afirma.android.gui;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/** Clase que contiene la informaci&oacute;n del certificado.
 * Nombre com&uacute;n, fecha comienzo validez y fecha expiraci&oacute;n.
 * @author Astrid Idoate
 *
 */
public class CertificateInfoForAliasSelect implements Serializable {


	/** Serial ID. */
	private static final long serialVersionUID = 1L;

	String commonName;
	Date notAfterDate;
	Date notBeforeDate;
	String alias;
	SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy", new Locale("ES")); //$NON-NLS-1$ //$NON-NLS-2$
	String issuer;


	/**
	 * @param commonName Nombre com&uacute;n del certificado.
	 * @param notBeforeDate Fecha en la que comienza la validez del certificado.
	 * @param notAfterDate Fecha de expiraci&oacute;n del certificado.
	 * @param alias
	 * @param issuer Emisor del certificado.
	 */
	public CertificateInfoForAliasSelect(final String commonName,final Date notBeforeDate, final Date notAfterDate, final String alias, final String issuer) {
		this.commonName = commonName;
		this.notBeforeDate = notBeforeDate;
		this.notAfterDate = notAfterDate;
		this.alias = alias;
		this.issuer = issuer;
	}


	/** Devuelve la fecha de expiraci&oacute;n del certificado.
	 * @return notAfterDate fecha de expiraci&oacute;n del certificado.
	 */
	public String getNotAfterDate(){
		return this.sdf.format(this.notAfterDate);
	}


	/** Devuelve la fecha de comienzo de validez del certificado.
	 * @return notBeforeDate fecha en la que comienza la validez del certificado.
	 */
	public String getNotBeforeDate(){
		return this.sdf.format(this.notBeforeDate);
	}

	/** Devuelve el nombre com&uacute;n del certificado
	 * @return commonName nombre com&uacute;n del certificado.
	 */
	public String getCommonName(){
		return this.commonName;
	}

	/** Devuelve el alias del certificado.
	 * @return alias Alias del certificado.
	 */
	public String getAlias(){
		return this.alias;
	}

	/** Devuelve el nombre del emisor del certificado.
	 * @return issuer Emisor del certificado.
	 */
	public String getIssuer(){
		return this.issuer;
	}

	/**
	 * @param date fecha de expiraci&oacute;n del certificado.
	 */
	public void setNotAfterDate(final Date date){
		this.notAfterDate = date;
	}

	/**
	 * @param date fecha comienzo de validez del certificado.
	 */
	public void setNotBeforeDate(final Date date){
		this.notBeforeDate = date;
	}

	/**
	 * @param name nombre com&uacute;n del certificado.
	 */
	public void setCommonName(final String name){
		this.commonName=name;
	}

	/**
	 * @param alias Alias del certificado.
	 */
	public void setAlias(final String alias){
		this.alias = alias;
	}

	/**
	 * @param issuer Emisor del certificado.
	 */
	public void setIssuer(final String issuer){
		this.issuer = issuer;
	}

}
