/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.pkcs7;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateFactory;
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

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.beans.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;


/** Clase que genera las listas de nodos o de firmantes que existe en un fichero. */
public final class ReadNodesTree {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    private String StringRetorn = ""; //$NON-NLS-1$
    private AOTreeNode raiz;
    private AOTreeNode rama;
    private AOTreeNode rama2;
    private int seleccionados[];
    private final List<String> lista = new ArrayList<String>();
    private final List<X509Certificate[]> listaCert = new ArrayList<X509Certificate[]>();

    int[] getSeleccionados() {
        return this.seleccionados;
    }

    void setSeleccionados(final int[] seleccionados) {
        this.seleccionados = seleccionados.clone();
    }

    // DefaultTreeModel modelo;

    String getStringRetorn() {
        return this.StringRetorn;
    }

    void setStringRetorn(final String StringRetorn) {
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
    public AOTreeModel readNodesTree(final byte[] data, final boolean asSimpleSignInfo) throws IOException {

        final ASN1InputStream is = new ASN1InputStream(data);

        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> contentsData = dsq.getObjects();
        // Elementos que contienen los elementos OID SignedData
        contentsData.nextElement();
        // Contenido de SignedData
        final ASN1TaggedObject doj = (ASN1TaggedObject) contentsData.nextElement();
        final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido
                                                                        // del
                                                                        // SignedData

        // Raiz de la secuencia de SignerInfo
        // Obtenemos los signerInfos del SignedData
        ASN1Set signerInfosSd = null;
        ASN1Set certificates = null;
        try {
            final SignedData sd = new SignedData(contentSignedData);
            signerInfosSd = sd.getSignerInfos();
            certificates = sd.getCertificates();
        }
        catch (final Exception e) {
            LOGGER.severe("Error obteniendo los SignerInfos del SignedData: " + e); //$NON-NLS-1$
        }

        // Para la creacion del arbol
        this.raiz = new AOTreeNode("Datos"); //$NON-NLS-1$

        // introducimos el nuevo SignerInfo del firmante actual.

        if (asSimpleSignInfo && signerInfosSd != null) {
            for (int i = 0; i < signerInfosSd.size(); i++) {
                final ASN1Sequence atribute = (ASN1Sequence) signerInfosSd.getObjectAt(i);
                final IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atribute.getObjectAt(1));
                final X509Certificate[] nameSigner = searchCert(certificates, issuerSerial.getSerialNumber());
                final SignerInfo si = new SignerInfo(atribute);
                final Date signingTime = getSigningTime(si);
                final AOSimpleSignInfo aossi = new AOSimpleSignInfo(nameSigner, signingTime);
                aossi.setPkcs1(si.getEncryptedDigest().getOctets());
                this.rama = new AOTreeNode(aossi);
                this.listaCert.add(nameSigner);
                getUnsignedAtributesWithCertificates(si.getUnauthenticatedAttributes(), this.rama, certificates);

                this.raiz.add(this.rama);
            }
        }
        else if (signerInfosSd != null) {
            for (int i = 0; i < signerInfosSd.size(); i++) {
                final ASN1Sequence atribute = (ASN1Sequence) signerInfosSd.getObjectAt(i);
                final IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atribute.getObjectAt(1));
                final String nameSigner = searchName(certificates, issuerSerial.getSerialNumber());
                final SignerInfo si = new SignerInfo(atribute);
                this.rama = new AOTreeNode(nameSigner);
                this.lista.add(nameSigner);
                getUnsignedAtributes(si.getUnauthenticatedAttributes(), this.rama, certificates);

                this.raiz.add(this.rama);
            }
        }

        return new AOTreeModel(this.raiz);
    }

    /** M&eacute;todo para obtener las contrafirmas.
     * @param signerInfouAtrib
     *        Atributos en los que puede estar la contrafirma.
     * @param ramahija
     *        Rama hija donde buscar los siguientes nodos.
     * @param certificates
     *        Certificados. */
    private void getUnsignedAtributesWithCertificates(final ASN1Set signerInfouAtrib, final AOTreeNode ramahija, final ASN1Set certificates) {

        if (signerInfouAtrib != null) {
            final Enumeration<?> eAtributes = signerInfouAtrib.getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atrib.getObjectAt(1));
                            final SignerInfo si = new SignerInfo(atrib);
                            final X509Certificate[] nameSigner = searchCert(certificates, issuerSerial.getSerialNumber());
                            final Date signingTime = getSigningTime(si);
                            final AOSimpleSignInfo aossi = new AOSimpleSignInfo(nameSigner, signingTime);
                            aossi.setPkcs1(si.getEncryptedDigest().getOctets());
                            this.rama2 = new AOTreeNode(aossi);
                            this.listaCert.add(nameSigner);
                            ramahija.add(this.rama2);
                            getUnsignedAtributesWithCertificates(si.getUnauthenticatedAttributes(), this.rama2, certificates);
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
    private void getUnsignedAtributes(final ASN1Set signerInfouAtrib, final AOTreeNode ramahija, final ASN1Set certificates) {

        if (signerInfouAtrib != null) {
            final Enumeration<?> eAtributes = signerInfouAtrib.getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (!data.getAttrType().equals(PKCSObjectIdentifiers.id_aa_signatureTimeStampToken)) {
                    final ASN1Set setInto = data.getAttrValues();
                    final Enumeration<?> eAtributesData = setInto.getObjects();
                    while (eAtributesData.hasMoreElements()) {
                        final Object obj = eAtributesData.nextElement();
                        if (obj instanceof ASN1Sequence) {
                            final ASN1Sequence atrib = (ASN1Sequence) obj;
                            final IssuerAndSerialNumber issuerSerial = new IssuerAndSerialNumber((ASN1Sequence) atrib.getObjectAt(1));
                            final SignerInfo si = new SignerInfo(atrib);
                            final String nameSigner = searchName(certificates, issuerSerial.getSerialNumber());
                            this.rama2 = new AOTreeNode(nameSigner);
                            this.lista.add(nameSigner);
                            ramahija.add(this.rama2);
                            getUnsignedAtributes(si.getUnauthenticatedAttributes(), this.rama2, certificates);
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
        final List<String> listaComp = this.lista;
        final int[] nodesToSign = new int[listaComp.size()];
        int cont = 0;
        for (int i = 0; i < listaComp.size(); i++) {
            for (final String aux2 : signers) {
                final String aux = listaComp.get(i);
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
    public int[] simplyArray(final int[] nodes) {
        final List<Integer> devolver = new ArrayList<Integer>();

        for (int i = 0; i < nodes.length; i++) {
            if (!devolver.contains(Integer.valueOf(nodes[i]))) {
                devolver.add(Integer.valueOf(nodes[i]));
            }
        }
        final int[] simplificado = new int[devolver.size()];
        for (int i = 0; i < devolver.size(); i++) {
            simplificado[i] = devolver.get(i).intValue();
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
    private String searchName(final ASN1Set certificates, final DERInteger serialNumber) {
        final Enumeration<?> certSet = certificates.getObjects();
        while (certSet.hasMoreElements()) {
            final X509Certificate c;
            try {
                c = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(new ByteArrayInputStream(((ASN1Sequence) certSet.nextElement()).getEncoded())); //$NON-NLS-1$
            }
            catch(final Exception e) {
                LOGGER.severe("Error extrayendo los certificados del Set ASN.1, puede que se haya omitido un elemento valido" + e); //$NON-NLS-1$
                continue;
            }
            if (c.getSerialNumber().equals(serialNumber.getValue())) {
                return AOUtil.getCN(c);
            }
        }
        LOGGER.info("No se ha encontrado el certificado indicado, se devolvera una cadena vacia"); //$NON-NLS-1$
        return ""; //$NON-NLS-1$
    }

    /** A partir de un numero de serie de un certificado, devuelve un array con
     * el certificado y su cadena de confianza.
     * @param certificates
     *        Certificados de los firmantes.
     * @param serialNumber
     *        N&uacute;mero de serie del certificado a firmar.
     * @return El certificado (en la posici&oacute;n 0 y su cadena de confianza
     *         en orden). */
    private X509Certificate[] searchCert(final ASN1Set certificates, final DERInteger serialNumber) {
        final Enumeration<?> certSet = certificates.getObjects();
        while (certSet.hasMoreElements()) {
            final X509Certificate c;
            try {
                c = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate(new ByteArrayInputStream(((ASN1Sequence) certSet.nextElement()).getEncoded())); //$NON-NLS-1$
            }
            catch(final Exception e) {
                LOGGER.severe("Error extrayendo los certificados del Set ASN.1, puede que se haya omitido un elemento valido" + e); //$NON-NLS-1$
                continue;
            }
            if (c.getSerialNumber().equals(serialNumber.getValue())) {
                return new X509Certificate[] { c };
            }
        }
        LOGGER.severe("El certificados pedido no estaba en la lista, se devolvera un array vacio"); //$NON-NLS-1$
        return new X509Certificate[0];
    }

    private Date getSigningTime(final SignerInfo si) {
        Date returnDate = null;

        if (si.getAuthenticatedAttributes() != null) {
            final Enumeration<?> eAtributes = si.getAuthenticatedAttributes().getObjects();
            while (eAtributes.hasMoreElements()) {
                final Attribute data = new Attribute((ASN1Sequence) eAtributes.nextElement());
                if (data.getAttrType().equals(CMSAttributes.signingTime)) {
                    final ASN1Set time = data.getAttrValues();
                    final DERUTCTime d = (DERUTCTime) time.getObjectAt(0);
                    try {
                        returnDate = d.getDate();
                    }
                    catch (final ParseException ex) {
                        LOGGER.warning("No es posible convertir la fecha"); //$NON-NLS-1$
                    }
                }
            }
        }

        return returnDate;
    }
}
