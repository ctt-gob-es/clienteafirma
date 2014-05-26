using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Security.Cryptography.X509Certificates;
using es.gob.afirma.core.signers;
using es.gob.afirma.signers.pkcs7;

namespace es.gob.afirma.signers.cades
{
    class AOCAdESSigner
    {

    /** Firma datos en formato CAdES.<br/>
     * @param data Datos que deseamos firmar.
     * @param algorithm Algoritmo a usar para la firma.
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA1withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA256withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA384withRSA</i></li>
     *  <li>&nbsp;&nbsp;&nbsp;<i>SHA512withRSA</i></li>
     * </ul>
     * @param keyEntry Entrada que apunta a la clave privada a usar para firmar
     * @param xParams Par&aacute;metros adicionales para la firma.
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>includeOnlySignningCertificate</i></b></dt>
	 *   <dd>
	 *    Si se establece a {@code true} se incluye en la firma &uacute;nicamente el certificado del firmante (y no la cadena de certificaci&oacute;n completa).
	 *    Si no se establece o se establece a o {@code false} se incluir&aacute; toda la cadena de certificaci&oacute;n.
	 *   </dd>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. El valor <code>explicit</code> indica que no se incluyen los datos firmados, sino una
     *    referencia a estos, mientras que el valor <code>implicit</code> indica que s&iacute; se incluir&aacute;n dentro de
     *    la propia firma los datos firmados
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificador de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique
     *    &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *   </dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *    Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible
     *    universalmente se usar&aacute; <code>0</code>, mientras que si no se indica una huella digital pero el par&aacute;metro
     *    <code>policyIdentifier</code> es una URL accesible universalmente, se descargara el fichero apuntado por la URL para calcular la huella
     *    digital <i>al vuelo</i>.
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>.
     *    Es obligario indicarlo cuando se proporciona una huella digital distinta de <code>0</code>.
     *   </dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>
     *    URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *   </dd>
     *  <dt><b><i>precalculatedHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo de huella digital (a usar para la firma) cuando esta se proporciona precalculada. Cuando se usan modos de firma
     *    <i>expl&iacute;citos</i>, en los que los datos no se incluyen en la firma, es posible trabajar sin proporcionarlos, indicando
     *    &uacute;nicamente su huella digital en el par&aacute;metro <code>data</code> y el algoritmo usado para su c&aacute;lculo.<br>
     *    <b>
     *     Siempre que se de valor a este par&aacute;metro se supondr&aacute; que los datos proporcionados en el par&aacute;metro
     *     <code>data</code> son la huella digital de los datos a firmar, y no los datos a firmar en si.
     *    </b>
     *   </dd>
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>
     *    Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo
     *    <i>Signing Certificate</i> de CAdES. Si no se establece un valor para este par&aacute;metro
     *    se utilizar&aacute;a la versi&oacute;n 1 con las firmas realizadas con algoritmos SHA1 y
     *    la versi&oacute;n 2 con las firmas realizadas con cualquier otro algoritmo.
     *   </dd>
     * </dl>
     * @return Firma en formato CAdES
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    
	public byte[] sign(byte[] data,
                       String algorithm,
                       X509Certificate2[] keyEntry,
                       Dictionary<string,string> xParams) {

        //Aunque está implementado, omitimos el checkeo de BC. Razon: mirar dentro de la clase BCChecker().
    	//new BCChecker().checkBouncyCastle();

        Dictionary<string,string> extraParams = xParams != null ? xParams : new Dictionary<string,string>();

        string precalculatedDigest = null;
        extraParams.TryGetValue("precalculatedHashAlgorithm", out precalculatedDigest);
            
        byte[] messageDigest = null;

        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        bool signingCertificateV2 = false;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        }
        else if (extraParams.ContainsKey("signingCertificateV2")) { 
            string signing = null;
            extraParams.TryGetValue("signingCertificateV2", out signing);
            if(signing!=null)
       		    signingCertificateV2 = Boolean.Parse(signing);
        }
        else {
        	signingCertificateV2 = !"SHA1".Equals(AOSignConstants.GetDigestAlgorithmName(algorithm));	 
        }
        string mode = AOSignConstants.DEFAULT_SIGN_MODE;
        extraParams.TryGetValue("mode", out mode);

        //string includeOnlySignningCertificate = null;
        //extraParams.TryGetValue("includeOnlySignningCertificate", out includeOnlySignningCertificate);
        //if(includeOnlySignningCertificate!= null){
        //    bool include = Boolean.Parse(includeOnlySignningCertificate);
        //    if(include){
            
        //    }
        //}

        P7ContentSignerParameters csp = new P7ContentSignerParameters(
    		data,
    		algorithm,
    		keyEntry
		);

        try {
            bool omitContent = false;
            if (mode.Equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null) {
                omitContent = true;
            }

            String contentTypeOid = AOSignConstants.DEFAULT_CONTENT_OID_DATA;
            String contentDescription = AOSignConstants.DEFAULT_CONTENT_DESCRIPTION;

            // DE MOMENTO NO VAMOS A CALCULAR EL MIMETYPE. MANTENDREMOS SIEMPRE POR DEFECTO LOS MISMOS
            //if (data != null) {
            //    try {
            //        MimeHelper mimeHelper = new MimeHelper(data);
            //        contentDescription = mimeHelper.getDescription();
            //        contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
            //    } catch (Exception e) {
            //        Console.WriteLine("No se han podido cargar las librerias para identificar el tipo de dato firmado: " + e); 
            //    }
            //}

            bool padesMode = false;
            string sPadesMode = null;
            extraParams.TryGetValue("padesMode", out sPadesMode);
            if(sPadesMode!=null){
                padesMode = Boolean.Parse(sPadesMode);
            }
            
			return GenCAdESEPESSignedData.generateSignedData(
                   csp,
                   omitContent,
                   new AdESPolicy(extraParams),
                   signingCertificateV2,
                   keyEntry[0], //cogemos el elemento 0, que es el que contiene el certificado raiz
                   messageDigest,
                   padesMode, 
                   contentTypeOid,
                   contentDescription
            );
        }
        catch (Exception e) {
            throw new Exception("Error generando la firma CAdES: " + e, e); 
        }
    }
    }
}
