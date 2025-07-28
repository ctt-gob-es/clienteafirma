/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades.common;

/** Par&aacute;metros adicionales para las firmas PAdES. */
public final class PdfExtraParams {

	/**
	 * Si se establece a <code>true</code> se evita usar la cadena de certificados
	 * del firmante en la inclusi&oacute;n en el diccionario PDF y en la estructura
	 * de apariencia (PdfSignatureAppearance). Esto permite trabajar en modo
	 * trif&aacute;sico sin que la infraestructura PDF conozca la identidad del
	 * firmante, que quedar&iacute;a encapsulada en la parte CAdES.
	 */
	public static final String DO_NOT_USE_CERTCHAIN_ON_POSTSIGN = "doNotUseCertChainOnPostSign"; //$NON-NLS-1$

	/**
	 * Indica si debe rotarse el campo de firma. Si se indica <code>true</code> el
	 * texto de la firma se rota 90 grados en sentido positivo. Si se indica
	 * <code>false</code> o no se indica, no se rota nada. Se respetan las
	 * posiciones del campo de firma (no se rota el propio campo, solo su texto).
	 * Este campo solo aplica a las firmas PDF visibles. Si se rota una firma se
	 * pierde la imagen de r&uacute;brica que se pudiese haber establecido.
	 */
	public static final String SIGNATURE_ROTATION = "signatureRotation"; //$NON-NLS-1$

	/**
	 * Si se establece a <code>true</code> omite la inclusi&oacute;n de la
	 * pol&iacute;tica de certificaci&oacute;n en el <i>SigningCertificate</i>, si
	 * se establece a <code>false</code> se incluye siempre que el certificado la
	 * declare.
	 */
	public static final String DO_NOT_INCLUDE_POLICY_ON_SIGNING_CERTIFICATE = "doNotIncludePolicyOnSigningCertificate"; //$NON-NLS-1$

	/**
	 * Si se establece a <code>true</code> se incluye en la firma &uacute;nicamente
	 * el certificado del firmante (y no la cadena de certificaci&oacute;n
	 * completa). Si no se establece o se establece a <code>false</code> se
	 * incluir&aacute; toda la cadena de certificaci&oacute;n (propiedad compartida
	 * con XAdES y CAdES).
	 */
	public static final String INCLUDE_ONLY_SIGNNING_CERTIFICATE = "includeOnlySignningCertificate";//$NON-NLS-1$

	/**
	 * Establece la fecha y hora indicadas como momento de la firma. Debe
	 * establecerse en el formato <code>yyyy:MM:dd:HH:mm:ss</code>. Por ejemplo, la
	 * cadena <code>2010:12:25:12:30:01</code> indica el venticinco de diciembre de
	 * 2010 a las 12 horas, treinta minutos y un segundo. Se toma siempre como zona
	 * horaria la establecida en la m&aacute;quina donde se ejecuta el firmador.
	 */
	public static final String SIGN_TIME = "signTime";//$NON-NLS-1$

	/** Perfil de firma que se desea generar. */
	public static final String PROFILE = "profile"; //$NON-NLS-1$

	/**
	 * Si se establece a <code>true</code> se permite firmar un PDF incluso si este
	 * contiene firmas previas no registradas dentro de campos Acrobat (AcroFields).
	 * El firmar un PDF que contenga firmas no registradas puede dar como resultado
	 * que se invaliden estas firmas previas, por lo que no se recomienda
	 * permitirlo. Si no se establece o se establece a <code>false</code>, al
	 * intentar firmar un PDF que contenga firmas no registradas se lanza una
	 * excepci&oacute;n de tipo<code> PdfHasUnregisteredSignaturesException</code>.
	 */
	public static final String ALLOW_COSIGNING_UNREGISTERED_SIGNATURES = "allowCosigningUnregisteredSignatures";//$NON-NLS-1$

	/**
	 * Nombre del sub-filtro en el diccionario PDF para indicar el tipo de la firma.
	 * Si no se indica este par&aacute;metro por defecto se usa
	 * <code>adbe.pkcs7.detached</code> (firma PAdES b&aacute;sica). Es posible
	 * indicar <code>ETSI.CAdES.detached</code> para generar una firma PAdES-BES, si
	 * bien el hacerlo puede causar que al a&ntilde;adir firmas adicionales al PDF
	 * se invaliden las ya existentes.
	 */
	public static final String SIGNATURE_SUBFILTER = "signatureSubFilter";//$NON-NLS-1$

	/**
	 * Si se establece a <code>false</code> no se comprime el PDF resultante. Si no
	 * se establece o se establece a cualquier otro valor distinto de
	 * <code>false</code>, el PDF de salida (firmado) se comprime para que ocupe
	 * menos tama&ntilde;o. Esta propiedad s&oacute;lo se aplica si se trata de un
	 * PDF v4 o superior, en versiones anteriores nunca se comprimen los PDF.
	 */
	public static final String COMPRESS_PDF = "compressPdf";//$NON-NLS-1$

	/**
	 * Si se establece a <code>true</code> siempre crea una revisi&oacute;n del PDF
	 * incluso cuando el documento no contiene ninguna firma previa.<br>
	 * Esto requiere que los documentos de entrada cumplan estrictamente la
	 * especificaci&oacute;n PDF 1.7 (ISO 32000-1:2008), y puede crear
	 * incompatibilidades con documentos PDF acordes a la especificaci&oacute;n 1.3
	 * creados con bibliotecas antiguas, como por ejemplo
	 * <a href="http://qpdf.sourceforge.net/">QPDF</a>.<br>
	 * Si se establece a <code>false</code>, no crea revisiones en documentos que no
	 * contengan firmas previas y s&iacute; las crea en documentos que ya contengan
	 * alguna firma.<br>
	 * En los documentos cifrados <u>siempre</u> se crea una revisi&oacute;n, sea
	 * cual sea el valor de este par&aacute;metro.
	 */
	public static final String ALWAYS_CREATE_REVISION = "alwaysCreateRevision";//$NON-NLS-1$

	/**
	 * Imagen que se desea insertar en el PDF antes de que este sea firmado. La
	 * imagen debe proporcionarse en formato JPEG codificado en Base64. Si el
	 * documento ya contiene firmas es posible que se invaliden, por lo que conviene
	 * usarlo &uacute;nicamente en documentos sin firmas previas.
	 */
	public static final String IMAGE = "image";//$NON-NLS-1$

	/**
	 * P&aacute;gina donde desea insertarse la imagen indicada mediante el
	 * par&aacute;metro <code>image</code>. La numeraci&oacute;n de las
	 * p&aacute;ginas comienza en uno.<br>
	 * Si se indica <i>-1</i> como n&uacute;mero de p&aacute;gina se inserta la
	 * imagen en la &uacute;ltima p&aacute;gina del documento. Si se indica <i>0</i>
	 * como n&uacute;mero de p&aacute;gina se inserta la imagen en todas las
	 * p&aacute;ginas del documento. Este par&aacute;metro es obligatorio, si no se
	 * indica una p&aacute;gina v&aacute;lida no se insertar&aacute; la imagen.
	 */
	public static final String IMAGE_PAGE = "imagePage";//$NON-NLS-1$

	/**
	 * Coordenada horizontal inferior izquierda de la posici&oacute;n de la imagen
	 * (indicada mediante el par&aacute;metro <code>image</code>) dentro de la
	 * p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas de la imagen mediante los
	 * par&aacute;metros <code>imagePositionOnPageLowerLeftY</code>,
	 * <code>imagePositionOnPageUpperRightX</code> e
	 * <code>imagePositionOnPageUpperRightY</code>.<br>
	 * Es necesario indicar tambi&eacute;n una p&aacute;gina de inserci&oacute;n en
	 * el par&aacute;metro <code>imagePage</code>.
	 */
	public static final String IMAGE_POSITION_ON_PAGE_LOWER_LEFTX = "imagePositionOnPageLowerLeftX";//$NON-NLS-1$

	/**
	 * Coordenada vertical inferior izquierda de la posici&oacute;n de la imagen
	 * (indicada mediante el par&aacute;metro <code>image</code>) dentro de la
	 * p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas de la imagen mediante los
	 * par&aacute;metros <code>imagePositionOnPageLowerLeftX</code>,
	 * <code>imagePositionOnPageUpperRightX</code> e
	 * <code>imagePositionOnPageUpperRightY</code>.<br>
	 * Es necesario indicar tambi&eacute;n una p&aacute;gina de inserci&oacute;n en
	 * el par&aacute;metro <code>imagePage</code>.
	 */
	public static final String IMAGE_POSITION_ON_PAGE_LOWER_LEFTY = "imagePositionOnPageLowerLeftY";//$NON-NLS-1$

	/**
	 * Coordenada horizontal superior derecha de la posici&oacute;n de la imagen
	 * (indicada mediante el par&aacute;metro <code>image</code>) dentro de la
	 * p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas de la imagen mediante los
	 * par&aacute;metros <code>imagePositionOnPageLowerLeftX</code>,
	 * <code>imagePositionOnPageLowerLeftY</code> e
	 * <code>imagePositionOnPageUpperRightY</code>.<br>
	 * Es necesario indicar tambi&eacute;n una p&aacute;gina de inserci&oacute;n en
	 * el par&aacute;metro <code>imagePage</code>.
	 */
	public static final String IMAGE_POSITION_ON_PAGE_UPPER_RIGHTX = "imagePositionOnPageUpperRightX";//$NON-NLS-1$

	/**
	 * Coordenada vertical superior derecha de la posici&oacute;n de la imagen
	 * (indicada mediante el par&aacute;metro <code>image</code>) dentro de la
	 * p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas de la imagen mediante los
	 * par&aacute;metros <code>imagePositionOnPageLowerLeftX</code>,
	 * <code>imagePositionOnPageLowerLeftY</code> e
	 * <code>imagePositionOnPageUpperRightX</code>.<br>
	 * Es necesario indicar tambi&eacute;n una p&aacute;gina de inserci&oacute;n en
	 * el par&aacute;metro <code>imagePage</code>.
	 */
	public static final String IMAGE_POSITION_ON_PAGE_UPPER_RIGHTY = "imagePositionOnPageUpperRightY";//$NON-NLS-1$

	/**
	 * Contenido a a&ntilde;adir como adjunto al PDF, en formato Base64 (el adjunto
	 * ser&aacute; el binario decodificado). Este par&aacute;metro requiere que se
	 * haya establecido tambi&eacute;n el par&aacute;metro
	 * <code>attachFileName</code>.
	 */
	public static final String ATTACH = "attach";//$NON-NLS-1$

	/**
	 * Nombre de fichero para adjuntar el contenido binario indicado mediante
	 * <code>attach</code>. Este par&aacute;metro requiere que se haya establecido
	 * tambi&eacute;n el par&aacute;metro <code>attach</code>.
	 */
	public static final String ATTACH_FILENAME = "attachFileName";//$NON-NLS-1$

	/**
	 * Descripci&oacute;n del contenido binario indicado mediante
	 * <code>attach</code>.
	 */
	public static final String ATTACH_DESCRIPTION = "attachDescription";//$NON-NLS-1$

	/**
	 * <p>
	 * Configura si debe realizarse una firma de aprobaci&oacute;n (por defecto) o
	 * certificada.
	 * </p>
	 * <p>
	 * Una firma de aprobaci&oacute;n o de formulario se realiza sobre un campo de
	 * firma de formulario del documento (preexistente o creado
	 * autom&aacute;ticamente en el momento de la firma). Un documento puede
	 * contener tantas firmas de aprobaci&oacute;n como necesite. Esta es la
	 * opci&oacute;n com&uacute;n de firma.
	 * </p>
	 * <p>
	 * Una firma certificada o de documento se aplica sobre un campo de firma
	 * identificado como de documento (preexistente o creado autom&aacute;ticamente
	 * en el momento de la firma). Un documento puede contener un &uacute;nico campo
	 * de este tipo y por tanto una &uacute;nica firma certificada. En caso de
	 * agregarse una firma certificada al documento, esta debe ser la primera que se
	 * agregue. Si hubiese alguna firma previa el resultado no ser&iacute;a
	 * v&aacute;lido.
	 * </p>
	 * <p>
	 * Independientemente de sus nombres, ambos tipos de firma aplican a todo el
	 * documento (lo firman por completo), s&oacute;lo cambia la designaci&oacute;n
	 * del campo en el que se almacenan.
	 * </p>
	 * <p>
	 * Una firma certificada restringe modificaciones posteriores sobre el
	 * documento. Seg&uacute;n el nivel de certificaci&oacute;n de esta firma se
	 * podr&aacute;n hacer unos cambios u otros. El Cliente @firma permite
	 * configurar el nivel de certificaci&oacute;n de una firma por medio del
	 * par&aacute;metro certificationLevel y un valor num&eacute;rico:
	 * </p>
	 * <ul>
	 * <li><i>0</i> = Firma sin certificar. Esta ser&iacute;a una firma de
	 * aprobaci&oacute;n. Es el valor por defecto.</li>
	 * <li><i>1</i> = Firma certificada de autor. Tras este tipo de firma
	 * certificada, no se permite ning&uacute;n cambio posterior en el documento (no
	 * se pueden agregar firmas, ni rellenar formularios).</li>
	 * <li><i>2</i> = Firma certificada de autor para formularios. Tras este tipo de
	 * firma certificada, s&oacute;lo se permite el relleno de los campos del
	 * formulario (no se pueden agregar firmas).</li>
	 * <li><i>3</i> = Firma certificada com&uacute;n. Tras este tipo de firma
	 * certificada, s&oacute;lo se permite el relleno de los campos del formulario y
	 * la creaci&oacute;n de firmas de aprobaci&oacute;n.</li>
	 * </ul>
	 */
	public static final String CERTIFICATION_LEVEL = "certificationLevel";//$NON-NLS-1$


	/** Valor para indicar el nivel 0 de firma certificada de PDF (sin certificar). */
	public static final String CERTIFICATION_LEVEL_VALUE_TYPE_0 = "0"; //$NON-NLS-1$

	/** Valor para indicar el nivel 1 de firma certificada de PDF (certificada de autor). */
	public static final String CERTIFICATION_LEVEL_VALUE_TYPE_1 = "1"; //$NON-NLS-1$

	/** Valor para indicar el nivel 2 de firma certificada de PDF (certificada de autor para formularios). */
	public static final String CERTIFICATION_LEVEL_VALUE_TYPE_2 = "2"; //$NON-NLS-1$

	/** Valor para indicar el nivel 3 de firma certificada de PDF (certificada com&uacute;n). */
	public static final String CERTIFICATION_LEVEL_VALUE_TYPE_3 = "3"; //$NON-NLS-1$

	/**
	 * Versi&oacute;n del PDF de salida:
	 * <ul>
	 * <li><i>2</i> = PDF 1.2</li>
	 * <li><i>3</i> = PDF 1.3</li>
	 * <li><i>4</i> = PDF 1.4</li>
	 * <li><i>5</i> = PDF 1.5</li>
	 * <li><i>6</i> = PDF 1.6</li>
	 * <li><i>7</i> = PDF 1.7</li>
	 * </ul>
	 */
	public static final String PDF_VERSION = "pdfVersion";//$NON-NLS-1$

	/**
	 * Nombre del campo en donde insertar la firma. Si el documento PDF tiene ya un
	 * campo de firma pre-creado es posible utilizarlo para insertar la firma
	 * generada, referenci&aacute;ndolo por su nombre.<br>
	 * Si se indica un nombre de campo de firma que no exista en el documento PDF
	 * proporcionado, se generar&aacute; una excepci&oacute;n.
	 */
	public static final String SIGNATURE_FIELD = "signatureField";//$NON-NLS-1$

	/**
	 * Bandera que indica si la firma debe ser visible en el documento PDF.
	 */
	public static final String VISIBLE_SIGNATURE = "visibleSignature"; //$NON-NLS-1$

	/** Valor para indicar que el usuario debe poder seleccionar el &aacute;rea de firma. */
	public static final String VISIBLE_SIGNATURE_VALUE_OPTIONAL = "optional"; //$NON-NLS-1$

	/** Valor para indicar que el usuario debe seleccionar obligatoriamente el &aacute;rea de firma. */
	public static final String VISIBLE_SIGNATURE_VALUE_WANT = "want"; //$NON-NLS-1$

	/**
	 * Bandera que indica si se le permitir&aacute; al usuario establecer una visualizaci&oacute;n
	 * personalizada de la firma en el documento PDF.
	 */
	public static final String VISIBLE_APPEARANCE = "visibleAppearance"; //$NON-NLS-1$

	/** Valor para indicar que el usuario debe poder seleccionar la apariencia de la firma. */
	public static final String VISIBLE_APPEARANCE_VALUE_CUSTOM = "custom"; //$NON-NLS-1$

	/**
	 * P&aacute;gina del documento PDF donde insertar la firma.
	 * Se podr&aacute;n seleccionar todas las p&aacute;ginas usando la palabra clave {@code all}.
	 * Se podr&aacute;n seleccionar p&aacute;ginas individuales list&aacute;ndolas separadas por comas (,).
	 * Por ejemplo, para mostrar la firma en las p&aacute;ginas 1, 2, 5 y 7, se usar&aacute;: 1,2,5,7
	 * Se podr&aacute;n seleccionar rangos de p&aacute;ginas separadas por guion (-), en donde en las dos p&aacute;ginas
	 * que limitan el rango tambi&eacute;n se imprimir&aacute; la r&uacute;brica.
	 * Por ejemplo, para mostrar la firma en las p&aacute;ginas 2, 3, 4 y 5, se usar&aacute;: 2-5
	 * Se podr&aacute;n seleccionar p&aacute;ginas contando desde el final del documento utilizando valores negativos.
	 * Por ejemplo, para mostrar la firma en la pen&uacute;ltima p&aacute;gina, se usar&aacute;: -2
	 * Estos mecanismos podr&aacute;n usarse de forma conjunta, a excepci&oacute;n de la palabra clave {@code all}
	 * que ya implica la firma en todas las p&aacute;ginas.
	 * Por ejemplo, para mostrar la firma en las tres primeras p&aacute;ginas y en las tres &uacute;ltimas, se usar&aacute;: 1-3,-3--1
	 * <code>signaturePositionOnPageLowerLeftX</code>,
	 * <code>signaturePositionOnPageLowerLeftY</code>,
	 * <code>signaturePositionOnPageUpperRightX</code> y
	 * <code>signaturePositionOnPageUpperRightY</code>.
	 */
	public static final String SIGNATURE_PAGE = "signaturePage";//$NON-NLS-1$

	/**
	 * Tiene el mismo funcionamiento que el par&aacute;metro signaturePage.
	 * Se admite este parametro para evitar confusiones por parte de los integradores.
	 */
	public static final String SIGNATURE_PAGES = "signaturePages";//$NON-NLS-1$

	/**
	 * Coordenada horizontal inferior izquierda de la posici&oacute;n del recuadro
	 * visible de la firma dentro de la p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas del recuadro mediante los
	 * par&aacute;metros <code>signaturePositionOnPageLowerLeftY</code>,
	 * <code>signaturePositionOnPageUpperRightX</code> y
	 * <code>signaturePositionOnPageUpperRightY</code>.<br>
	 * Si no se indica una p&aacute;gina en el par&aacute;metro
	 * <code>signaturePage</code> la firma se inserta en la &uacute;ltima
	 * p&aacute;gina del documento.
	 */
	public static final String SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX = "signaturePositionOnPageLowerLeftX";//$NON-NLS-1$

	/**
	 * Coordenada vertical inferior izquierda de la posici&oacute;n del recuadro
	 * visible de la firma dentro de la p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas del recuadro mediante los
	 * par&aacute;metros <code>signaturePositionOnPageLowerLeftX</code>,
	 * <code>signaturePositionOnPageUpperRightX</code> y
	 * <code>signaturePositionOnPageUpperRightY</code>.<br>
	 * Si no se indica una p&aacute;gina en el par&aacute;metro
	 * <code>signaturePage</code> la firma se inserta en la &uacute;ltima
	 * p&aacute;gina del documento.
	 */
	public static final String SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY = "signaturePositionOnPageLowerLeftY";//$NON-NLS-1$

	/**
	 * Coordenada horizontal superior derecha de la posici&oacute;n del recuadro
	 * visible de la firma dentro de la p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas del recuadro mediante los
	 * par&aacute;metros <code>signaturePositionOnPageLowerLeftX</code>,
	 * <code>signaturePositionOnPageLowerLeftY</code> y
	 * <code>signaturePositionOnPageUpperRightY</code>.<br>
	 * Si no se indica una p&aacute;gina en el par&aacute;metro
	 * <code>signaturePage</code> la firma se inserta en la &uacute;ltima
	 * p&aacute;gina del documento.
	 */
	public static final String SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX = "signaturePositionOnPageUpperRightX";//$NON-NLS-1$

	/**
	 * Coordenada vertical superior derecha de la posici&oacute;n del recuadro
	 * visible de la firma dentro de la p&aacute;gina.<br>
	 * Es necesario indicar el resto de coordenadas del recuadro mediante los
	 * par&aacute;metros <code>signaturePositionOnPageLowerLeftX</code>,
	 * <code>signaturePositionOnPageLowerLeftY</code> y
	 * <code>signaturePositionOnPageUpperRightX</code>.<br>
	 * Si no se indica una p&aacute;gina en el par&aacute;metro
	 * <code>signaturePage</code> la firma se inserta en la &uacute;ltima
	 * p&aacute;gina del documento.
	 */
	public static final String SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY = "signaturePositionOnPageUpperRightY";//$NON-NLS-1$

	/**
	 * Imagen JPEG codificada en Base64 de la r&uacute;brica de la firma manuscrita
	 * que se desea aparezca como firma visible en el PDF.
	 */
	public static final String SIGNATURE_RUBRIC_IMAGE = "signatureRubricImage";//$NON-NLS-1$

	/**
	 * <p>
	 * Texto a escribir dentro de la "capa 2" de la firma visible.<br>
	 * Este texto se escribe &uacute;nicamente si no se ha especificado una imagen
	 * de r&uacute;brica, y necesita que se indique la p&aacute;gina y la
	 * situaci&oacute;n d&oacute;nde mostrar el recuadro de firma mediante los
	 * par&aacute;metros <code>signaturePositionOnPageLowerLeftX</code>,
	 * <code>signaturePositionOnPageLowerLeftY</code>,
	 * <code>signaturePositionOnPageUpperRightX</code>,
	 * <code>signaturePositionOnPageUpperRightY</code> y <code>signaturePage</code>.
	 * </p>
	 * <p>
	 * Este texto puede incluir una serie de palabras clave que ser&aacute;n
	 * sustituidas por los textos apropiados del titular o emisor del certificado de
	 * firma:
	 * <dl>
	 * <dt><i><b>$$SUBJECTCN$$</b></i></dt>
	 * <dd>Nombre com&uacute;n (CN, <i>Common Name</i>) dentro del
	 * <code>X.500 Principal</code> del titular del certificado de firma.</dd>
	 * <dt><i><b>$$SUBJECTDN$$</b></i></dt>
	 * <dd>Nombre diferenciado (DN, <i>Distinguished Name</i>) dentro del
	 * <code>X.500 Principal</code> del titular del certificado de firma.</dd>
	 * <dt><i><b>$$ISSUERCN$$</b></i></dt>
	 * <dd>Nombre com&uacute;n (CN, <i>Common Name</i>) dentro del
	 * <code>X.500 Principal</code> del emisor del certificado de firma.</dd>
	 * <dt><i><b>$$CERTSERIAL$$</b></i></dt>
	 * <dd>N&uacute;mero de serie del certificado de firma.</dd>
	 * <dt><i><b>$$SIGNDATE=<code>PATR&Oacute;N</code>$$</b></i></dt>
	 * <dd>Fecha de la firma, donde <code>PATR&Oacute;N</code> debe indicar el
	 * formato en el que debe mostrarse la fecha, siguiendo el esquema definido por
	 * Oracle para la clase <a href=
	 * "http://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html">SimpleDateFormat</a>.
	 * </dd>
	 * <dt><i><b>$$GIVENNAME$$</b></i></dt>
	 * <dd>Nombre Propio (GIVENNAME) dentro del <code>X.500 Principal</code> del
	 * titular del certificado de firma.</dd>
	 * <dt><i><b>$$SURNAME$$</b></i></dt>
	 * <dd>Apellido (SURNAME) dentro del <code>X.500 Principal</code> del titular
	 * del certificado de firma.</dd>
	 * <dt><i><b>$$SUBJECTCN$$</b></i></dt>
	 * <dd>Organizaci&oacute;n (O, <i>Organzation</i>) dentro del
	 * <code>X.500 Principal</code> del titular del certificado de firma.</dd>
	 * <dt><i><b>$$REASON$$</b></i></dt>
	 * <dd>Razón de la firma, definida en las preferencias de Firmas PAdES</dd>
	 * <dt><i><b>$$LOCATION$$</b></i></dt>
	 * <dd>Ciudad de creación de la firma, definida en las preferencias de Firmas
	 * PAdES</dd>
	 * <dt><i><b>$$CONTACT$$</b></i></dt>
	 * <dd>Información de contacto, definida en las preferencias de Firmas
	 * PAdES</dd>
	 * </dl>
	 * As&iacute;, por ejemplo, el texto
	 * "<code>Firmado por $$SUBJECTCN$$ el d&iacute;a $$SIGNDATE=dd/MM/yyyy$$.</code>"
	 * resultar&aacute; finalmente en el PDF como
	 * "<code>Firmado por Tom&aacute;s Garc&iacute;a-Mer&aacute;s el d&iacute;a 04/01/2016.</code>"
	 * suponiendo que el CN del titular del certificado de firma es
	 * <code>Tom&aacute;s Garc&iacute;a-Mer&aacute;s</code> y que la firma se
	 * realiza el 04/01/2016.
	 */
	public static final String LAYER2_TEXT = "layer2Text";//$NON-NLS-1$

	/**
	 * Tipo de letra a usar en el texto de la "capa 2" de la firma visible. Este
	 * par&aacute;metro requiere que se haya establecido tambi&eacute;n el
	 * par&aacute;metro <code>layer2Text</code>.<br>
	 * Los valores admitidos son num&eacute;ricos, correspondiendo:
	 * <ul>
	 * <li><i>0</i> = Courier (tipo por defecto)</li>
	 * <li><i>1</i> = Helv&eacute;tica</li>
	 * <li><i>2</i> = Times Roman</li>
	 * <li><i>3</i> = Symbol</li>
	 * <li><i>4</i> = ZapfDingBats</li>
	 * </ul>
	 */
	public static final String LAYER2_FONTFAMILY = "layer2FontFamily";//$NON-NLS-1$

	/**
	 * Tama&ntilde;o de letra a usar en el texto de la "capa 2" de la firma visible.
	 * Este par&aacute;metro requiere que se haya establecido tambi&eacute;n el
	 * par&aacute;metro <code>layer2Text</code>.<br>
	 * Los valores admitidos son num&eacute;ricos (y el valor por defecto es 12).
	 */
	public static final String LAYER2_FONTSIZE = "layer2FontSize";//$NON-NLS-1$

	/**
	 * Estilo del tipo de letra a usar en el texto de la "capa 2" de la firma
	 * visible. Este par&aacute;metro requiere que se haya establecido
	 * tambi&eacute;n el par&aacute;metro <code>layer2Text</code>.<br>
	 * Los valores admitidos son num&eacute;ricos, correspondiendo:
	 * <ul>
	 * <li><i>0</i> = Normal (estilo por defecto)</li>
	 * <li><i>1</i> = Negrita</li>
	 * <li><i>2</i> = Cursiva</li>
	 * <li><i>3</i> = Negrita y cursiva</li>
	 * <li><i>4</i> = Subrayado</li>
	 * <li><i>8</i> = Tachado</li>
	 * </ul>
	 * Es posible combinar estilos aplicando la operaci&oacute;n l&oacute;gica
	 * <cite>o</cite> sobre los valores num&eacute;ricos a combinar.
	 */
	public static final String LAYER2_FONTSTYLE = "layer2FontStyle";//$NON-NLS-1$

	/**
	 * Par&aacute;metro que indica si se debe ofuscar los datos sensibles del
	 * certificado de usuario en la firma visible PDF.
	 */
	public static final String OBFUSCATE_CERT_DATA = "obfuscateCertText";//$NON-NLS-1$

	/**
	 * Par&aacute;metro con la mascara de ofuscaci&oacute;n a aplicar. La cadena con
	 * la m&aacute;scara debe tener la forma:<br>
	 * {@code caracterSustitutivo;longitudDigitos;posiciones;desplazamiento}<br>
	 * Aqu&iacute;:
	 * <ul>
	 * <li>{@code caracterSustitutivo}: Es el car&aacute;cter que debemos usar para
	 * ofuscar caracteres.</li>
	 * <li>{@code longitudDigitos}: Es el n&uacute;mero m&iacute;nimo de
	 * d&iacute;gitos que debe tener una cadena de texto para que se considere que
	 * debe ofuscarse.</li>
	 * <li>{@code posiciones}: Es el listado de posiciones que indica qu&eacute;
	 * caracteres deben mostrarse. El listado se expresar&aacute; por una
	 * sucesi&oacute;n {@code true}/{@code false} separados por comas (','), en
	 * donde {@code true} indica que el car&aacute;cter debe mostrarse y
	 * {@code false} que no. Las posiciones a ofucar al final del patr&oacute;n se
	 * sobreentender&aacute;n.</li>
	 * <li>{@code desplazamiento}: Indica si se admite el desplazamiento de
	 * posiciones de la m&aacute;scara para mostrar todos los caracteres indicados
	 * ({@code true}) o si esta debe respetarse ({@code false}).</li>
	 * </ul>
	 */
	public static final String OBFUSCATION_MASK = "obfuscationMask";//$NON-NLS-1$

	/**
	 * Color del texto de la "capa 2" de la firma visible. Este par&aacute;metro
	 * requiere que se haya establecido tambi&eacute;n el par&aacute;metro
	 * <code>layer2Text</code>.<br>
	 * Los valores admitidos son textuales (se ignora entre may&uacute;sculas y
	 * min&uacute;sculas), soport&aacute;ndose:
	 * <ul>
	 * <li><i>black</i> = Negro (color por defecto)</li>
	 * <li><i>white</i> = Blanco</li>
	 * <li><i>gray</i> = Gris</li>
	 * <li><i>lightGray</i> = Gris claro</li>
	 * <li><i>darkGray</i> = Gris oscuro</li>
	 * <li><i>red</i> = Rojo</li>
	 * <li><i>pink</i> = Rosa</li>
	 * </ul>
	 */
	public static final String LAYER2_FONTCOLOR = "layer2FontColor";//$NON-NLS-1$

	/**
	 * Raz&oacute;n por la que se realiza la firma (este dato se a&ntilde;ade al
	 * diccionario PDF, y no a la propia firma).
	 */
	public static final String SIGN_REASON = "signReason";//$NON-NLS-1$

	/**
	 * Ciudad en la que se realiza la firma (este dato se a&ntilde;ade al
	 * diccionario PDF, y no a la propia firma).
	 */
	public static final String SIGNATURE_PRODUCTION_CITY = "signatureProductionCity";//$NON-NLS-1$

	/**
	 * Contacto del firmante, usualmente una direcci&oacute;n de correo
	 * electr&oacute;nico (este dato se a&ntilde;ade al diccionario PDF, y no a la
	 * propia firma).
	 */
	public static final String SIGNER_CONTACT = "signerContact";//$NON-NLS-1$

	/**
	 * Identificador de la pol&iacute;tica de firma. Debe ser un OID (o una URN de
	 * tipo OID) que identifique un&iacute;vocamente la pol&iacute;tica en formato
	 * ASN.1 procesable. (propiedad compartida con XAdES y CAdES)
	 */
	public static final String POLICY_IDENTIFIER = "policyIdentifier";//$NON-NLS-1$

	/**
	 * Huella digital del documento de pol&iacute;tica de firma (normalmente del
	 * mismo fichero en formato ASN.1 procesable). Si no se indica una huella
	 * digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL
	 * accesible universalmente se lanzar&aacute; una Excepci&oacute;n, mientras que
	 * si no se indica una huella digital pero el par&aacute;metro
	 * <code>policyIdentifier</code> es una URL accesible universalmente, se
	 * descargara el fichero apuntado por la URL para calcular la huella digital
	 * <i>al vuelo</i>.
	 */
	public static final String POLICY_IDENTIFIER_HASH = "policyIdentifierHash";//$NON-NLS-1$

	/**
	 * Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el
	 * par&aacute;metro <code>policyIdentifierHash</code>. Es obligatorio indicarlo
	 * cuando se proporciona una huella digital distinta de <code>0</code>.
	 * (propiedad compartida con XAdES y CAdES).
	 */
	public static final String POLICY_IDENTIFIER_HASH_ALGORITHM = "policyIdentifierHashAlgorithm";//$NON-NLS-1$

	/**
	 * URL que apunta al documento descriptivo de la pol&iacute;tica de firma
	 * (normalmente un documento PDF con una descripci&oacute;n textual) (propiedad
	 * compartida con XAdES y CAdES).
	 */
	public static final String POLICY_QUALIFIER = "policyQualifier";//$NON-NLS-1$

	/**
	 * Lista de cargos atribuidos al firmante separados por el car&uacute;cter '|'.
	 * Los cargos de la lista no pueden contener el car&uacute;cter '|' (ya que este
	 * se usa como separador).
	 */
	public static final String SIGNER_CLAIMED_ROLES = "signerClaimedRoles";//$NON-NLS-1$

	/**
	 * Contrase&ntilde;a de apertura del PDF (contrase&ntilde;a del propietario) si
	 * este estaba cifrado.
	 */
	public static final String OWNER_PASSWORD_STRING = "ownerPassword";//$NON-NLS-1$

	/**
	 * Evita cualquier interacci&oacute;n con el usuario si se establece a
	 * <code>true</code>, si no se establece o se establece a <code>false</code>
	 * act&uacute;a normalmente (puede mostrar di&aacute;logos, por ejemplo, para
	 * solicitar las contrase&ntilde;as de los PDF cifrados). &Uacute;til para los
	 * procesos desatendidos y por lotes.
	 */
	public static final String HEADLESS = "headless";//$NON-NLS-1$

	/**
	 * Si se establece a <code>true</code> permite la firma o cofirma de PDF
	 * certificados sin consultarlo al usuario, si se establece a <code>false</code>
	 * o cualquier otro valor se lanza una excepci&oacute;n en caso de intentar
	 * firmar o cofirmar un PDF certificado y si no se establece se mostrar&aacute;
	 * un di&aacute;logo al usuario para que confirme que desea realizar la firma a
	 * pesar de que el resultado ser&aacute;n una firma no v&aacute;lida.<br>
	 * <b>Si el par&aacute;metro <code>headless</code> est&aacute; establecido a
	 * <code>true</code>, no podr&aacute; mostrar el di&aacute;logo de
	 * confirmaci&oacute;n as&iacute; que llegados a este punto se lanzar&aacute;
	 * una excepci&oacute;n.</b><br>
	 * No se soporta el cifrado de documentos PDF con certificados o con algoritmo
	 * AES256.
	 */
	public static final String ALLOW_SIGNING_CERTIFIED_PDFS = "allowSigningCertifiedPdfs";//$NON-NLS-1$

	/**
	 * Tipo de sello de tiempo a aplicar:
	 * <ul>
	 * <li>1 = Solo sello a nivel de firma.</li>
	 * <li>2 = Solo sello a nivel de documento.</li>
	 * <li>3 = Dos sellos, uno a nivel de firma y otro a nivel de documento.</li>
	 * </ul>
	 */
	public static final String TS_TYPE = "tsType";//$NON-NLS-1$

	/**
	 * URL de la autoridad de sello de tiempo (si no se indica no se a&ntilde;ade
	 * sello de tiempo).
	 */
	public static final String TSA_URL = "tsaURL";//$NON-NLS-1$

	/** Pol&iacute;tica de sellado de tiempo. */
	static final String TSA_POLICY = "tsaPolicy";//$NON-NLS-1$

	/**
	 * Algoritmo de huella digital a usar para el sello de tiempo (si no se
	 * establece se usa SHA-1).
	 */
	static final String TSA_HASH_ALGORITHM = "tsaHashAlgorithm";//$NON-NLS-1$

	/**
	 * <code>true</code> si se requiere el certificado de la TSA, false en caso
	 * contrario (si no se establece se asume <code>true</code>).
	 */
	static final String TSA_REQUIRE_CERT = "tsaRequireCert";//$NON-NLS-1$

	/** Nombre de usuario de la TSA. */
	static final String TSA_USR = "tsaUsr";//$NON-NLS-1$

	/**
	 * Contrase&ntilde;a del usuario de la TSA. Se ignora si no se ha establecido
	 * adem&aacute;s <code>tsaUsr</code>.
	 */
	static final String TSA_PWD_KEY = "tsaPwd";//$NON-NLS-1$

	/**
	 * OID de la extensi&oacute;n a a&ntilde;adir a la petici&oacute;n al servidor
	 * de sello de tiempo (opcional). Solo se permite indicar una extensi&oacute;n.
	 */
	static final String TSA_EXTENSION_OID = "tsaExtensionOid";//$NON-NLS-1$

	/**
	 * Valor, en binario convertido a Base64, de la extensi&oacute;n a a&ntilde;adir
	 * a la petici&oacute;n al servidor de sello de tiempo. Se ignora si no se ha
	 * establecido adem&aacute;s <code>tsaUsr</code>.
	 */
	static final String TSA_EXTENSION_VALUE_BASE64 = "tsaExtensionValueBase64";//$NON-NLS-1$

	/**
	 * Indica si la extensi&oacute;n indicada en <code>tsaExtensionOid</code> es
	 * cr&iacute;tica (valor <code>true</code>) o no (valor <code>false</code>). Se
	 * ignora si no se ha establecido adem&aacute;s <code>tsaUsr</code>.
	 */
	static final String TSA_EXTENSION_CRITICAL = "tsaExtensionCritical";//$NON-NLS-1$

	/**
	 * Almac&eacute;n de claves codificado en Base64 que contiene el certificado SSL
	 * cliente que pedir&aacute; la TSA al establecer la conexi&oacute;n HTTPS.
	 */
	static final String TSA_SSL_KEYSTORE = "tsaSslKeyStore";//$NON-NLS-1$

	/**
	 * Contrase&ntilde;a del almac&eacute;n de claves que contiene el certificado
	 * SSL cliente para las conexiones HTTPS.
	 */
	static final String TSA_SSL_KEYSTORE_PASSWORD_PARAM = "tsaSslKeyStorePassword";//$NON-NLS-1$

	/**
	 * Tipo del almac&eacute;n de claves que contiene el certificado SSL cliente
	 * para las conexiones HTTPS.<br>
	 * El formato <code>JKS</code> puede no funcionar adecuadamente en Android.
	 */
	static final String TSA_SSL_KEYSTORE_TYPE = "tsaSslKeyStoreType";//$NON-NLS-1$

	/**
	 * Almac&eacute;n de confianza en base64 que contiene los certificados emisores
	 * de confianza del certificado SSL de la TSA.
	 */
	static final String TSA_SSL_TRUST_STORE = "tsaSslTrustStore";//$NON-NLS-1$

	/**
	 * Contrase&ntilde;a del almac&eacute;n de confianza que contiene los
	 * certificados emisores del certificado SSL de la TSA.
	 */
	static final String TSA_SSL_TRUST_STORE_PASSWORD_PARAM = "tsaSslTrustStorePassword";//$NON-NLS-1$

	/**
	 * Tipo del almac&eacute;n de confianza que contiene los certificados emisores
	 * del certificado SSL de la TSA.<br>
	 * El formato <code>JKS</code> puede no funcionar adecuadamente en Android.
	 */
	static final String TSA_SSL_TRUST_STORE_TYPE = "tsaSslTrustStoreType";//$NON-NLS-1$

	/**
	 * Si se indica a <code>true</code> se utilizar&aacute; SigningCertificateV2, si
	 * se indica cualquier otra cosa SigningCertificateV1. Si no se indica nada, se
	 * utilizar&aacute; V1 para las firmas SHA1 y V2 para el resto (propiedad
	 * compartida con CAdES).
	 */
	static final String SIGNING_CERTIFICATE_V2 = "signingCertificateV2";//$NON-NLS-1$

	/**
	 * N&uacute;mero de <i>CommitmentTypeIndications</i> a a&ntilde;adir a la
	 * firma.<br>
	 * En los par&aacute;metros siguientes, los <i>CommitmentTypeIndications</i> se
	 * numeran a partir de 0 (cero).
	 */
	public static final String COMMITMENT_TYPE_INDICATIONS = "commitmentTypeIndications";//$NON-NLS-1$

	/**
	 * Prefijo de las claves con las que se indican las propiedades de los
	 * <i>Commitment Type Indications</i>. Se utilizar&aacute; este prefijo, seguido
	 * el n&uacute;mero del commitmentTypeIndication al que queramos referirnos y la
	 * clave de la propiedad en cuesti&oacute;n. As&iacute; pues, los
	 * par&aacute;metros son:
	 * <ul>
	 * <li>commitmentTypeIndication<i>n</i>Identifier</li>
	 * <li>commitmentTypeIndication<i>n</i>CommitmentTypeQualifiers</li>
	 * </ul>
	 */
	public static final String COMMITMENT_TYPE_INDICATION_PREFIX = "commitmentTypeIndication";//$NON-NLS-1$
	/**
	 * Lista de OID separados por el caracter '<i>|</i>' que se aportan como
	 * calificadores adicionales del <i>CommitmentTypeIndication</i> n&uacute;mero
	 * <i>n</i> (contando desde cero). Atributo opcional.
	 */
	public static final String COMMITMENT_TYPE_INDICATION_QUALIFIERS = "CommitmentTypeQualifiers";//$NON-NLS-1$
	/**
	 * Tipo de <i>CommitmentTypeIndication</i> para el
	 * <i>CommitmentTypeIndication</i> n&uacute;mero <i>n</i> (contando desde cero).
	 * Atributo obligatorio. Valores:
	 * <ul>
	 * <li><i>1</i> = Prueba de origen</li>
	 * <li><i>2</i> = Prueba de recepci&oacute;n</li>
	 * <li><i>3</i> = Prueba de entrega</li>
	 * <li><i>4</i> = Prueba de env&iacute;o</li>
	 * <li><i>5</i> = Prueba de aprobaci&oacute;n</li>
	 * <li><i>6</i> = Prueba de creaci&oacute;n</li>
	 * </ul>
	 */
	public static final String COMMITMENT_TYPE_INDICATION_IDENTIFIER = "Identifier";//$NON-NLS-1$

	/**
	 * Texto a escribir dentro de la "capa 4" de la firma visible.<br>
	 * Este texto se escribe &uacute;nicamente si no se ha especificado una imagen
	 * de r&uacute;brica.
	 */
	public static final String LAYER4_TEXT = "layer4Text";//$NON-NLS-1$

	/**
	 * Contrase&ntilde;a del pdf en caso de se encuentre protegido contra
	 * modificaciones y/o aperturas.
	 */
	public static final String USER_PASSWORD_STRING = "userPassword";//$NON-NLS-1$

	/**
	 * Si se indica a {@code true}, se mostrar&aacute; una marca junto a la firma
	 * visible que el lector de PDF podr&aacute; utilizar para indicar si la firma
	 * es v&aacute;lida o no.
	 */
	public static final String INCLUDE_QUESTION_MARK = "includeQuestionMark";//$NON-NLS-1$

	/**
	 * Indica el tama&ntilde;o que se desea reservar para la firma en el PDF.
	 */
	public static final String SIGN_RESERVED_SIZE = "signReservedSize";//$NON-NLS-1$

	/**
	 * Indica el si se permiten o no ataques PDF Shadow Attacks
	 */
	public static final String ALLOW_SHADOW_ATTACK = "allowShadowAttack"; //$NON-NLS-1$

	/**
	 * Indica si se permite firmar un PDF del que se han cambiado valores de los formularios que
	 * contiene despu&eacute;s de haberlo firmado.
	 */
	public static final String ALLOW_SIGN_MODIFIED_FORM = "allowModifiedForm"; //$NON-NLS-1$

	/**
	 * Indica la cantidad de p&aacute;ginas en las que comprobar un PDF Shadow Attack
	 */
	public static final String PAGES_TO_CHECK_PSA = "pagesToCheckShadowAttack"; //$NON-NLS-1$

	/**
	 * Valor que indica que se deben comprobar todas las p&aacute;ginas de un documento
	 */
	public static final String PAGES_TO_CHECK_PSA_VALUE_ALL = "all"; //$NON-NLS-1$

	/**
	 * Indica si se debe comprobar o no la validez de los certificados.
	 */
	public static final String CHECK_CERTIFICATES = "checkCertificates"; //$NON-NLS-1$

	/**
	 * Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya
	 * que es est&aacute;tico.
	 */
	private PdfExtraParams() {
		// No instanciable
	}
}
