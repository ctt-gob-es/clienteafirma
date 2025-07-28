/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades;

/** Par&aacute;metros adicionales aceptados para las firmas CAdES. */
public final class CAdESExtraParams {

    /** Si se establece a <code>true</code> omite la inclusi&oacute;n de la
     * pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>,
     * si se establece a <code>false</code> se incluye siempre que el certificado
     * la declare. */
    public static final String DO_NOT_INCLUDE_POLICY_ON_SIGNING_CERTIFICATE = "doNotIncludePolicyOnSigningCertificate"; //$NON-NLS-1$

    /** Algoritmo de huella digital (a usar para la firma) cuando esta se proporciona pre-calculada.
     * Cuando se usan modos de firma <i>expl&iacute;citos</i>, en los que los datos no se incluyen en la firma,
     * es posible trabajar sin proporcionarlos, indicando &uacute;nicamente su huella digital en el par&aacute;metro
     * <code>data</code> y el algoritmo usado para su c&aacute;lculo.<br>
     * <b>Siempre que se de valor a este par&aacute;metro se supondr&aacute; que los datos proporcionados en el
     * par&aacute;metro <code>data</code> son la huella digital de los datos a firmar, y no los datos a firmar en
     * s&iacute;.</b> */
    public static final String PRECALCULATED_HASH_ALGORITHM = "precalculatedHashAlgorithm"; //$NON-NLS-1$

    /** Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo
     *  <i>Signing Certificate</i> de CAdES. Si no se establece un valor para este par&aacute;metro
     *  se utilizar&aacute; la versi&oacute;n 1 con las firmas realizadas con algoritmos SHA1 y
     *  la versi&oacute;n 2 con las firmas realizadas con cualquier otro algoritmo. */
    public static final String SIGNING_CERTIFICATE_V2 = "signingCertificateV2"; //$NON-NLS-1$

    /**
     * Modo de firma a usar. El valor explicit indica que no se incluyen los datos firmados, sino una referencia
     * a estos, mientras que el valor implicit indica que s&iacute; se incluir&aacute;n dentro de la propia firma los datos firmados.
     */
    public static final String MODE = "mode"; //$NON-NLS-1$

    /**
     * Perfil de firma que se desea generar.
     */
	public static final String PROFILE = "profile"; //$NON-NLS-1$

    /**
     * Descripci&oacute;n textual del tipo de contenido a firmar. Necesita que se establezca tambi&eacute;n la propiedad <code>contentTypeOid</code>.
     */
    public static final String CONTENT_DESCRIPTION = "contentDescription"; //$NON-NLS-1$

    /**
     * Si se establece a <code>true</code> se incluye en la firma &uacute;nicamente el certificado del firmante (y no la cadena de certificaci&oacute;n completa).
     * Si no se establece o se establece a <code>false</code> se incluir&aacute; toda la cadena de certificaci&oacute;n.
     */
    public static final String INCLUDE_ONLY_SIGNNING_CERTIFICATE = "includeOnlySignningCertificate"; //$NON-NLS-1$

    /**
     *  <code>true</code> para incluir el atributo <i>SigningTime</i> de PKCS#9 (OID:1.2.840.113549.1.9.5),
     *  <code>false</code> para no incluirlo.<br><br>
     *  Esta propiedad s&Oacute;lo deber&iacute;a utilizarse para generar firmas PAdES para aptas para ser
     *  incluidas como para de una firma PAdES.
     */
    public static final String INCLUDE_SIGNING_TIME_ATTRIBUTE = "includeSigningTimeAttribute"; //$NON-NLS-1$

    /**
     *  Si se indica <code>false</code>, no se incluir&aacute; el atributo <i>content-hint</i>
     *  definido en PKCS#9 (OID:1.2.840.113549.1.9.16.2.4). Con cualquier otro valor se
     *  incluir&aacute; el atributo, ya sea con los valores calculados autom&aacute;ticamente a
     *  partir de los datos o con los valores establecidos en las propiedades
     *  {@code #CONTENT_TYPE_OID} y {@code #CONTENT_DESCRIPTION}.<br><br>
     *  Esta propiedad s&oacute;lo deber&iacute;a utilizarse para generar firmas aptas para ser
     *  incluidas como parte de una firma PAdES.
     */
    public static final String INCLUDE_CONTENT_HINT_ATTRIBUTE = "includeContentHintAttribute"; //$NON-NLS-1$

    /**
     *  OID que identifica el tipo de datos a firmar.
     *  <br>Propiedad compartida con XAdES, no aplica a contrafirmas
     */
    public static final String CONTENT_TYPE_OID = "contentTypeOid"; //$NON-NLS-1$

    /**
     *  Si se indica <code>true</code>, se incluir&aacute; el atributo <i>mime-type</i>
     *  (OID:0.4.0.1733.2.1) seg&uacute;n las reglas definidas en la IETF RFC 2045. El
     *  valor del mimetype se tomar&acute; de la propiedad {@code #CONTENT_MIME_TYPE}.
     *  Si esta no se definiese, se identificar&aacute; el mimetype correspondiente al
     *  OID establecido en la propiedad {@code #CONTENT_TYPE_OID}. Si esta tampoco se
     *  definiese, se usar&aacute; el mimetype identificado a ra&iacute;z del
     *  an&aacute;lisis de los datos firmados o el gen&eacute;rico si no se identificase.
     *  En el caso de cofirmas, se copiar&acute; el mimetype de la firma original si lo
     *  tuviese. Si no, se seguir&iacute;a con la cadena de comprobaciones ya indicada.
     *  En las contrafirmas no se incluir&aacute; este atributo.
     */
    public static final String INCLUDE_MIMETYPE_ATTRIBUTE = "includeMimeTypeAttribute"; //$NON-NLS-1$

    /** MIME-Type de los datos a firmar. Se utilizar&aacute; s&oacute;lo cuando tambi&eacute;se
     * utilice la propiedad {@code #INCLUDE_MIMETYPE_ATTRIBUTE}. */
    public static final String CONTENT_MIME_TYPE = "mimeType"; //$NON-NLS-1$

    /**
     *  Identificador de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique
     *  &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *  <br> Propiedad compartida con XAdES y PAdES)
     */
    public static final String POLICY_IDENTIFIER = "policyIdentifier";//$NON-NLS-1$

    /**
     *   Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *   Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible
     *   universalmente se lanzar&aacute; una Excepci&oacute;n, mientras que si no se indica una huella digital pero el par&aacute;metro
     *   <code>policyIdentifier</code> es una URL accesible universalmente, se descargar&aacute; el fichero apuntado por la URL para calcular la huella
     *   digital <i>al vuelo</i>.
     */
    public static final String POLICY_IDENTIFIER_HASH = "policyIdentifierHash";//$NON-NLS-1$

    /**
     *  Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro policyIdentifierHash.
     *  Es obligatorio indicarlo cuando se proporciona una huella digital distinta de 0.
     */
    public static final String POLICY_IDENTIFIER_HASH_ALGORITHM = "poliyIdentifierHashAlgorithm";//$NON-NLS-1$

    /**
     *  URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *  <br> Propiedad compartida con XAdES y PAdES
     */
    public static final String POLICY_QUALIFIER = "policyQualifier";//$NON-NLS-1$

    /**
     * Lista de cargos atribuidos al firmante separados por el car&uacute;cter
     * '|'. Los cargos de la lista no pueden contener el car&uacute;cter '|' (ya
     * que este se usa como separador).
     */
    public static final String SIGNER_CLAIMED_ROLES = "signerClaimedRoles";//$NON-NLS-1$

    /**
     *  Ciudad en la que se realiza la firma.
     *  <br> Propiedad compartida con XAdES y PAdES.
     */
    public static final String SIGNATURE_PRODUCTION_CITY = "signatureProductionCity";//$NON-NLS-1$

    /**
     * C&oacute;digo postal en el que se realiza la firma.
     * <br> Propiedad compartida con XAdES.
     */
    public static final String SIGNATURE_PRODUCTION_POSTAL_CODE = "signatureProductionPostalCode";//$NON-NLS-1$

    /**
     *  Pa&iacute;s en el que se realiza la firma.
     */
    public static final String SIGNATURE_PRODUCTION_COUNTRY = "signatureProductionCountry";//$NON-NLS-1$

    /**
     *  N&uacute;mero de <i>CommitmentTypeIndications</i> a a&ntilde;adir a la firma.<br>
     *  En los par&aacute;metros siguientes, los <i>CommitmentTypeIndications</i> se numeran a partir de 0 (cero).
     */
    public static final String COMMITMENT_TYPE_INDICATIONS = "commitmentTypeIndications";//$NON-NLS-1$

    /**
     * Prefijo de las claves con las que se indican las propiedades de los <i>Commitment
     * Type Indications</i>. Se utilizar&aacute; este prefijo, seguido el n&uacute;mero del
     * commitmentTypeIndication al que queramos referirnos y la clave de la propiedad en
     * cuesti&oacute;n. As&iacute; pues, los par&aacute;metros son:
     * <ul>
     *  <li>commitmentTypeIndication<i>n</i>Identifier</li>
     *  <li>commitmentTypeIndication<i>n</i>CommitmentTypeQualifiers</li>
     * </ul>
     */
    public static final String COMMITMENT_TYPE_INDICATION_PREFIX = "commitmentTypeIndication";//$NON-NLS-1$
    /**
     * Lista de OID separados por el caracter '<i>|</i>' que se aportan como calificadores adicionales del
     * <i>CommitmentTypeIndication</i> n&uacute;mero <i>n</i> (contando desde cero). Atributo opcional.
     */
    public static final String COMMITMENT_TYPE_INDICATION_QUALIFIERS = "CommitmentTypeQualifiers";//$NON-NLS-1$
    /**
     *  Tipo de <i>CommitmentTypeIndication</i> para el <i>CommitmentTypeIndication</i> n&uacute;mero <i>n</i>
     *  (contando desde cero). Atributo obligatorio. Valores:
     *  <ul>
     *  <li><i>1</i> = Prueba de origen</li>
     *  <li><i>2</i> = Prueba de recepci&oacute;n</li>
     *  <li><i>3</i> = Prueba de entrega</li>
     *  <li><i>4</i> = Prueba de env&iacute;o</li>
     *  <li><i>5</i> = Prueba de aprobaci&oacute;n</li>
     *  <li><i>6</i> = Prueba de creaci&oacute;n</li>
     *  </ul>
     */
    public static final String COMMITMENT_TYPE_INDICATION_IDENTIFIER = "Identifier";//$NON-NLS-1$

    /**
     *  URL de la autoridad de sello de tiempo (si no se indica no se a&ntilde;ade sello de tiempo).
     */
    public static final String TSA_URL = "tsaURL";//$NON-NLS-1$

    /**
     *  Pol&iacute;tica de sellado de tiempo (obligatoria si se indica tsaURL).
     */
    public static final String TSA_POLICY = "tsaPolicy";//$NON-NLS-1$

    /**
     *  Algoritmo de huella digital a usar para el sello de tiempo (si no se establece se usa SHA-1).
     */
    public static final String TSA_HASH_ALGORITHM = "tsaHashAlgorithm";//$NON-NLS-1$

    /**
     * <code>true</code> si se requiere el certificado de la TSA, false en caso contrario (si no se establece se asume <code>true</code>).
     */
    public static final String TSA_REQUIRE_CERT = "tsaRequireCert";//$NON-NLS-1$

    /**
     *  Nombre de usuario de la TSA.
     */
    public static final String TSA_USR = "tsaUsr";//$NON-NLS-1$

    /** Contrase&ntilde;a del usuario de la TSA. Se ignora si no se ha establecido
     * adem&aacute;s <code>tsaUsr</code>. */
    public static final String TSA_PWD_KEY = "tsaPwd";//$NON-NLS-1$

    /** Nombre del fichero PKCS#12 que contiene el certificado SSL cliente que pedir&aacute;
     * la TSA al establecer la conexi&oacute;n HTTPS.*/
    public static final String TSA_SSL_PKCS12_FILE = "tsaSslPkcs12File";//$NON-NLS-1$

    /** Contrase&ntilde;a del fichero PKCS#12 que contiene el certificado SSL cliente para
     * las conexiones HTTPS. */
    public static final String TSA_SSL_PKCS12_FILE_PASSWORD_KEY = "tsaSslPkcs12FilePassword";//$NON-NLS-1$

    /**
     *  Evita cualquier interacci&oacute;n con el usuario si se establece a <code>true</code>, si no se establece o se establece a <code>false</code>
     *  act&uacute;a normalmente (puede mostrar di&aacute;logos, por ejemplo, para solicitar las contrase&ntilde;as de los PDF cifrados). &Uacute;til para
     *  los procesos desatendidos y por lotes.
     */
    public static final String HEADLESS = "headless";//$NON-NLS-1$

    /**
     * Par&aacute;metro que permite configurar si se permite la multifirma de firmas de archivo aunque
     * queden invalidadas.
     */
	public static final String ALLOW_SIGN_LTS_SIGNATURES = "allowSignLTSignature"; //$NON-NLS-1$


    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private CAdESExtraParams(){
        // No instanciable
    }
}
