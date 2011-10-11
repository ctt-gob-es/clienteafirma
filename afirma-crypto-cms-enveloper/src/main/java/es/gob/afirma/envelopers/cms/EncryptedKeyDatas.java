/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cms;

import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

/** Clase utilizada en Utils en la funcion fetchEncryptedKeys */
final class EncryptedKeyDatas {

    private byte[] encryptedKey = null;
    private AlgorithmIdentifier algEncryptedKey = null;

    byte[] getEncryptedKey() {
        return this.encryptedKey;
    }

    void setEncryptedKey(final byte[] encryptedKey) {
        this.encryptedKey = encryptedKey.clone();
    }

    AlgorithmIdentifier getAlgEncryptedKey() {
        return this.algEncryptedKey;
    }

    void setAlgEncryptedKey(final AlgorithmIdentifier algEncryptedKey) {
        this.algEncryptedKey = algEncryptedKey;
    }
}
