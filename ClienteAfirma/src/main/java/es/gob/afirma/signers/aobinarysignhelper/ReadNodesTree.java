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

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.asn1.x509.X509Name;
import org.bouncycastle.asn1.x509.X509ObjectIdentifiers;
import org.bouncycastle.jce.provider.X509CertificateObject;

import es.gob.afirma.misc.tree.TreeModel;
import es.gob.afirma.misc.tree.TreeNode;
import es.gob.afirma.signers.beans.AOSimpleSignInfo;

/** Clase que genera las listas de nodos o de firmantes que existe en un fichero. */
public final class ReadNodesTree {

    private String StringRetorn = "";
    private TreeNode raiz;
    private TreeNode rama;
    private TreeNode rama2;
    private int seleccionados[];
    private final List<String> lista = new ArrayList<String>();
    private final List<X509Certificate[]> listaCert = new ArrayList<X509Certificate[]>();

    int[] getSeleccionados() {
        return seleccionados;
    }

    void setSeleccionados(int[] seleccionados) {
        this.seleccionados = seleccionados;
    }

    // DefaultTreeModel modelo;

    String getStringRetorn() {
        return StringRetorn;
    }

    void setStringRetorn(String StringRetorn) {
        this.StringRetorn = StringRetorn;
    }

    /** Genera el &aacute;rbol que representa las firmas.
     * @param data
     *        Archivo que contiene la firma.
     * @param asSimpleSignInfo
     *        Indica si deben extraerse informacion b&aacute;sica de la
     *        firma o solo los nombres.
     * @return Un modelo de &aacute;rbol.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos */
    public TreeModel readNodesTree(byte[] data, boolean asSimpleSignInfo) throws IOException {

        ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        ASN1Sequence dsq = null;
        dsq = (ASN1Sequence) is.readObject();
        Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        e.nextElement();
        // Contenido de SignedData
        ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // SignedData

        // Raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del SignedData
        ASN1Set signerInfosSd = null;
        ASN1Set certificates = null;
        try {
            SignedData sd = new SignedData(contentSignedData);
            signerInfosSd = sd.getSignerInfos();
            certificates = sd.getCertificates();
        }
        catch (Exception exception) {
            try {
                Logger.getLogger("es.gob.afirma").info("No es signedData, probamos con SignedAndEnvelopedData.");
                SignedAndEnvelopedData sd = new SignedAndEnvelopedData(contentSignedData);
                signerInfosSd = sd.getSignerInfos();
                certificates = sd.getCertificates();
            }
            catch (Exception e2) {
                Logger.getLogger("es.gob.afirma").severe(e2.toString());
            }
        }

        // Para la creacion del arbol
        raiz = new TreeNode("Datos");

        // introducimos el nuevo SignerInfo del firmante actual.

        if (asSimpleSignInfo && signerInfosSd != null) {
            for (int i = 0; i < signerInfosSd.size(); i++) {
                ASN1Sequence atribute = (ASN1Sequence) signerInfosSd.getObjectAt(i);
                IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atribute.getObjectAt(1));
                X509Certificate[] nameSigner = searchCert(certificates, issuerSerial.getSerialNumber());
                SignerInfo si = new SignerInfo(atribute);
                Date signingTime = getSigningTime(si);
                AOSimpleSignInfo aossi = new AOSimpleSignInfo(nameSigner, signingTime);
                aossi.setPkcs1(si.getEncryptedDigest().getOctets());
                rama = new TreeNode(aossi);
                listaCert.add(nameSigner);
                getUnsignedAtributesWithCertificates(si.getUnauthenticatedAttributes(), rama, certificates);

                raiz.add(rama);
            }
        }
        else if (signerInfosSd != null) {
            for (int i = 0; i < signerInfosSd.size(); i++) {
                ASN1Sequence atribute = (ASN1Sequence) signerInfosSd.getObjectAt(i);
                IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atribute.getObjectAt(1));
                String nameSigner = searchName(certificates, issuerSerial.getSerialNumber());
                SignerInfo si = new SignerInfo(atribute);
                rama = new TreeNode(nameSigner);
                lista.add(nameSigner);
                getUnsignedAtributes(si.getUnauthenticatedAttributes(), rama, certificates);

                raiz.add(rama);
            }
        }

        return new TreeModel(raiz);
    }

    /** M&eacute;todo para obtener las contrafirmas.
     * @param signerInfouAtrib
     *        Atributos en los que puede estar la contrafirma.
     * @param ramahija
     *        Rama hija donde buscar los siguientes nodos.
     * @param certificates
     *        Certificados. */
    private void getUnsignedAtributesWithCertificates(ASN1Set signerInfouAtrib, TreeNode ramahija, ASN1Set certificates) {

        if (signerInfouAtrib != null) {
            Enumeration<?> eAtributes = signerInfouAtrib.getObjects();
            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    ASN1Set setInto = data.getAttrValues();
                    Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            ASN1Sequence atrib = (ASN1Sequence) obj;
                            IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atrib.getObjectAt(1));
                            SignerInfo si = new SignerInfo(atrib);
                            X509Certificate[] nameSigner = searchCert(certificates, issuerSerial.getSerialNumber());
                            Date signingTime = getSigningTime(si);
                            AOSimpleSignInfo aossi = new AOSimpleSignInfo(nameSigner, signingTime);
                            aossi.setPkcs1(si.getEncryptedDigest().getOctets());
                            rama2 = new TreeNode(aossi);
                            listaCert.add(nameSigner);
                            ramahija.add(rama2);
                            getUnsignedAtributesWithCertificates(si.getUnauthenticatedAttributes(), rama2, certificates);
                        }
                    }
                }
            }

        }
    }

    /** M&eacute;todo para obtener las contrafirmas.
     * @param signerInfouAtrib
     *        Atributos en los que puede estar la contrafirma.
     * @param ramahija
     *        Rama hija donde buscar los siguientes nodos.
     * @param certificates
     *        Certificados. */
    private void getUnsignedAtributes(ASN1Set signerInfouAtrib, TreeNode ramahija, ASN1Set certificates) {

        if (signerInfouAtrib != null) {
            Enumeration<?> eAtributes = signerInfouAtrib.getObjects();
            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    ASN1Set setInto = data.getAttrValues();
                    Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            ASN1Sequence atrib = (ASN1Sequence) obj;
                            IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atrib.getObjectAt(1));
                            SignerInfo si = new SignerInfo(atrib);
                            String nameSigner = searchName(certificates, issuerSerial.getSerialNumber());
                            rama2 = new TreeNode(nameSigner);
                            lista.add(nameSigner);
                            ramahija.add(rama2);
                            getUnsignedAtributes(si.getUnauthenticatedAttributes(), rama2, certificates);
                        }
                    }
                }
            }

        }
    }

    /** Lee los nodos pertenecientes a un firmante.
     * @param signers
     *        Firmante del que se buscan los nodos.
     * @param data
     *        Fichero que representa la firma.
     * @return Los nodos que tiene ese firmante.
     * @throws java.io.IOException
     *         Si hay problemas en la lectura de datos */
    public int[] readNodesFromSigners(final String[] signers, final byte[] data) throws IOException {
        int[] solucion;

        readNodesTree(data, false);
        List<String> listaComp = lista;
        int[] nodesToSign = new int[listaComp.size()];
        int cont = 0;
        for (int i = 0; i < listaComp.size(); i++) {
            for (int j = 0; j < signers.length; j++) {
                String aux = listaComp.get(i);
                String aux2 = signers[j];
                if (aux.equals(aux2)) {
                    nodesToSign[cont] = i;
                    cont++;

                }
            }
        }

        solucion = new int[cont];
        for (int b = 0; b < cont; b++) {
            solucion[b] = nodesToSign[b];
        }
        solucion = simplyArray(solucion);
        Arrays.sort(solucion);// de mayor a menor

        return solucion;

    }

    /** Simplifica un array quitando los elementos repetidos.
     * @param nodes
     *        array con posibles repetidos.
     * @return array sin repetidos. */
    public int[] simplyArray(int[] nodes) {
        List<Integer> devolver = new ArrayList<Integer>();

        for (int i = 0; i < nodes.length; i++) {
            if (!devolver.contains(nodes[i])) {
                devolver.add(Integer.valueOf(nodes[i]));
            }
        }
        int[] simplificado = new int[devolver.size()];
        for (int i = 0; i < devolver.size(); i++) {
            simplificado[i] = devolver.get(i);
        }
        return simplificado;
    }

    /** M&eacute;todo que, apartir de un numero de serie de un certificado,
     * devuelve su nombre com&uacute;n (CN). De no existir el CN,
     * devolver&aacute; el nombre de la unidad organizativa.
     * @param certificates
     *        Certificados de los firmantes.
     * @param serialNumber
     *        N&uacute;mero de serie del certificado a firmar.
     * @return El nombre com&uacute;n. */
    private String searchName(ASN1Set certificates, DERInteger serialNumber) {
        String nombre = "";
        Enumeration<?> certSet = certificates.getObjects();
        while (certSet.hasMoreElements()) {
            ASN1Sequence atrib2 = (ASN1Sequence) certSet.nextElement();
            TBSCertificateStructure atrib = TBSCertificateStructure.getInstance(atrib2.getObjectAt(0));
            if (serialNumber.toString().equals(atrib.getSerialNumber().toString())) {
                X509Name name = atrib.getSubject();
                nombre = name.getValues(X509ObjectIdentifiers.commonName).toString();
                if (nombre != null && !nombre.equals("") && !nombre.equals("[]")) {
                    nombre = nombre.substring(1, nombre.length() - 1);
                }
                else {
                    nombre = name.getValues(X509ObjectIdentifiers.organizationalUnitName).toString();
                    nombre = nombre.substring(1, nombre.length() - 1);
                }
            }
        }
        return nombre;
    }

    /** A partir de un numero de serie de un certificado, devuelve un array con
     * el certificado y su cadena de confianza.
     * @param certificates
     *        Certificados de los firmantes.
     * @param serialNumber
     *        N&uacute;mero de serie del certificado a firmar.
     * @return El certificado (en la posici&oacute;n 0 y su cadena de confianza
     *         en orden). */
    private X509Certificate[] searchCert(ASN1Set certificates, DERInteger serialNumber) {
        Enumeration<?> certSet = certificates.getObjects();
        ASN1Sequence atrib2;
        while (certSet.hasMoreElements()) {
            atrib2 = (ASN1Sequence) certSet.nextElement();
            // atrib2 es ya en sí un único certificado.
            // se compone de 3 elementos:
            // tbsCert =
            // TBSCertificateStructure.getInstance(seq.getObjectAt(0));
            // sigAlgId = AlgorithmIdentifier.getInstance(seq.getObjectAt(1));
            // sig = DERBitString.getInstance(seq.getObjectAt(2));
            if (atrib2.size() < 1) continue;
            TBSCertificateStructure atrib = TBSCertificateStructure.getInstance(atrib2.getObjectAt(0));
            if (serialNumber.toString().equals(atrib.getSerialNumber().toString())) {
                // Es el certificado bueno, creamos el array con un único
                // elemento y lo devolvemos
                X509Certificate[] ret = new X509Certificate[1];
                try {
                    ret[0] = new X509CertificateObject(new X509CertificateStructure(atrib2));
                }
                catch (Exception e) {
                    Logger.getLogger("es.gob.afirma")
                          .severe("No se pudieron recuperar los certificados de la lista, se devolvera un array vacio: " + e);
                    return new X509Certificate[0];
                }
                return ret;
            }
        }
        Logger.getLogger("es.gob.afirma").severe("El certificados pedido no estaba en la lista, se devolvera un array vacio");
        return new X509Certificate[0];
    }

    private Date getSigningTime(SignerInfo si) {
        Date returnDate = null;

        if (si.getAuthenticatedAttributes() != null) {
            Enumeration<?> eAtributes = si.getAuthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (data.getAttrType().equals(CMSAttributes.signingTime)) {
                    ASN1Set time = data.getAttrValues();
                    DERUTCTime d = (DERUTCTime) time.getObjectAt(0);
                    try {
                        returnDate = d.getDate();
                    }
                    catch (ParseException ex) {
                        Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha");
                    }
                }
            }
        }

        return returnDate;
    }
}
