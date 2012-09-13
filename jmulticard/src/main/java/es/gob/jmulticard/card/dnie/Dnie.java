/*
 * Controlador Java de la Secretaria de Estado de Administraciones PÃºblicas
 * para el DNI electrÃ³nico.
 *
 * El Controlador Java para el DNI electrÃ³nico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electrÃ³nico en aplicaciones Java de terceros
 * para la realizaciÃ³n de procesos de autenticaciÃ³n, firma electrÃ³nica y validaciÃ³n
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electrÃ³nico, asi como la realizaciÃ³n
 * de operaciones criptogrÃ¡ficas de firma con el DNI electrÃ³nico. El Controlador ha
 * sido diseÃ±ado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 DirecciÃ³n General de ModernizaciÃ³n Administrativa, Procedimientos
 * e Impulso de la AdministraciÃ³n ElectrÃ³nica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podrÃ¡n elegir bajo cual de las
 * licencias desean utilizar el cÃ³digo fuente. Su elecciÃ³n deberÃ¡ reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinarÃ¡
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los tÃ©rminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la versiÃ³n 2.1 de la Licencia, o en una versiÃ³n posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los tÃ©rminos de la
 * European Union Public License publicada por la ComisiÃ³n Europea,
 * tanto en la versiÃ³n 1.1 de la Licencia, o en una versiÃ³n posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consÃºltelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consÃºltelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea Ãºtil, pero
 * SIN NINGUNA GARANTÃ�A; incluso sin la garantia implicita de comercializaciÃ³n
 * o idoneidad para un propÃ³sito particular.
 */
package es.gob.jmulticard.card.dnie;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPrivateKey;
import java.util.logging.Logger;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

import javax.security.auth.callback.PasswordCallback;

import es.gob.jmulticard.CryptoHelper;
import es.gob.jmulticard.HexUtils;
import es.gob.jmulticard.apdu.CommandApdu;
import es.gob.jmulticard.apdu.ResponseApdu;
import es.gob.jmulticard.apdu.connection.ApduConnection;
import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.apdu.connection.CardNotPresentException;
import es.gob.jmulticard.apdu.connection.LostChannelException;
import es.gob.jmulticard.apdu.connection.NoReadersFoundException;
import es.gob.jmulticard.apdu.connection.cwa14890.Cwa14890OneConnection;
import es.gob.jmulticard.apdu.connection.cwa14890.SecureChannelException;
import es.gob.jmulticard.apdu.dnie.GetChipInfoApduCommand;
import es.gob.jmulticard.apdu.iso7816eight.PsoSignHashApduCommand;
import es.gob.jmulticard.apdu.iso7816four.ExternalAuthenticateApduCommand;
import es.gob.jmulticard.apdu.iso7816four.InternalAuthenticateApduCommand;
import es.gob.jmulticard.apdu.iso7816four.MseSetAuthenticationKeyApduCommand;
import es.gob.jmulticard.apdu.iso7816four.MseSetSignatureKeyApduCommand;
import es.gob.jmulticard.asn1.der.pkcs1.DigestInfo;
import es.gob.jmulticard.asn1.der.pkcs15.Cdf;
import es.gob.jmulticard.asn1.der.pkcs15.PrKdf;
import es.gob.jmulticard.card.Atr;
import es.gob.jmulticard.card.CryptoCard;
import es.gob.jmulticard.card.CryptoCardException;
import es.gob.jmulticard.card.InvalidCardException;
import es.gob.jmulticard.card.Location;
import es.gob.jmulticard.card.PrivateKeyReference;
import es.gob.jmulticard.card.cwa14890.Cwa14890Card;
import es.gob.jmulticard.card.iso7816eight.Iso7816EightCard;
import es.gob.jmulticard.card.iso7816four.FileNotFoundException;
import es.gob.jmulticard.card.iso7816four.Iso7816FourCardException;
import es.gob.jmulticard.ui.passwordcallback.CancelledOperationException;
import es.gob.jmulticard.ui.passwordcallback.DialogBuilder;

/**
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class Dnie extends Iso7816EightCard implements CryptoCard, Cwa14890Card {

    /** Identificador del fichero del certificado de componente del DNIe. */
    private static final byte[] CERT_ICC_FILE_ID = new byte[] {
            (byte) 0x60, (byte) 0x1F
    };

    /** Nombre del Master File del DNIe. */
    private static final String MASTER_FILE_NAME = "Master.File"; //$NON-NLS-1$

    private static final String AUTH_CERT_ALIAS = "CertAutenticacion"; //$NON-NLS-1$
    private static final String SIGN_CERT_ALIAS = "CertFirmaDigital"; //$NON-NLS-1$
    private static final String INTERMEDIATE_CA_CERT_ALIAS = "CertCAIntermediaDGP"; //$NON-NLS-1$

    private static final String AUTH_KEY_LABEL = "KprivAutenticacion"; //$NON-NLS-1$
    private static final String SIGN_KEY_LABEL = "KprivFirmaDigital"; //$NON-NLS-1$

    private static final Location CDF_LOCATION = new Location("50156004"); //$NON-NLS-1$

    private static final Location PRKDF_LOCATION = new Location("50156001"); //$NON-NLS-1$

    /** Propiedad del sistema que determina si el "Modo r&aacute;pido" est&aacute;a activado. */
    private static final String FAST_MODE_PROPERTY = "es.gob.jmulticard.fastmode"; //$NON-NLS-1$

    private X509Certificate authCert;
    private X509Certificate signCert;
    private X509Certificate intermediateCaCert;

    private Location authCertPath;
    private Location signCertPath;

    private DniePrivateKeyReference authKeyRef;
    private DniePrivateKeyReference signKeyRef;

    private boolean needsRealCertificates = false;

    /** Manejador de funciones criptograficas. */
    private CryptoHelper cryptoHelper = null;

    private static final byte[] ATR_MASK = new byte[] {
            (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
            (byte) 0xFF, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xFF, (byte) 0xFF
    };

    private static final Atr ATR = new Atr(new byte[] {
            (byte) 0x3B, (byte) 0x7F, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x6A, (byte) 0x44, (byte) 0x4E, (byte) 0x49,
            (byte) 0x65, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x90, (byte) 0x00
    }, ATR_MASK);

    private static final Atr BURNED_DNI_ATR = new Atr(new byte[] {
            (byte) 0x3B, (byte) 0x7F, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x6A, (byte) 0x44, (byte) 0x4E, (byte) 0x49,
            (byte) 0x65, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x65, (byte) 0x81
    }, ATR_MASK);

    private final PasswordCallback passwordCallback;

    /** Si se establece a <code>true</code> se recrean los certificados y los alias a partir del CDF, fichero
     * p&uacute;blico que se puede leer sin PIN. Estos certificados son ficticios, y no recrean una estructura
     * X.509 */
    private final boolean fastMode;

    /** Conecta con el lector del sistema que tenga un DNIe insertado. */
    private void connect(final ApduConnection conn) throws BurnedDnieCardException, InvalidCardException, ApduConnectionException {
    	if (conn == null) {
    		throw new IllegalArgumentException("La conexion no puede ser nula"); //$NON-NLS-1$
    	}
    	byte[] responseAtr;
    	Atr actualAtr;
    	InvalidCardException invalidCardException = null;
    	CardNotPresentException cardNotPresentException = null;
    	final long[] terminals = conn.getTerminals(false);
    	if (terminals.length < 1) {
    	    throw new NoReadersFoundException();
    	}
    	for(int i=0;i<terminals.length;i++) {
    		conn.setTerminal((int) terminals[i]);
    		try {
    			responseAtr = conn.reset();
    		}
    		catch(final CardNotPresentException e) {
    			cardNotPresentException = e;
    			continue;
    		}
    		actualAtr = new Atr(responseAtr, ATR_MASK);
    		if (BURNED_DNI_ATR.equals(actualAtr)) {
                throw new BurnedDnieCardException();
            }
    		if (!ATR.equals(actualAtr)) { // La tarjeta encontrada no es un DNIe
    			invalidCardException = new InvalidCardException(getCardName(), ATR, responseAtr);
    			continue;
    		}
    		return;
    	}
    	if (invalidCardException != null) {
    		throw invalidCardException;
    	}
    	if (cardNotPresentException != null) {
    		throw cardNotPresentException;
    	}
    	throw new ApduConnectionException("No se ha podido conectar con ningun lector de tarjetas"); //$NON-NLS-1$
    }

    /** Construye una clase que representa un DNIe.
     * @param conn Conexi&oacute;n con la tarjeta
     * @param pwc <i>PasswordCallback</i> para obtener el PIN del DNIe
     * @param cryptoHelper Funcionalidades criptogr&aacute;ficas de utilidad que pueden variar entre m&aacute;quinas virtuales
     * @throws ApduConnectionException Si la conexi&oacute;n con la tarjeta se proporciona cerrada y no es posible abrirla
     * @throws es.gob.jmulticard.card.InvalidCardException Si la tarjeta conectada no es un DNIe
     * @throws BurnedDnieCardException Si la tarjeta conectada es un DNIe con la memoria vol&aacute;til borrada */
    public Dnie(final ApduConnection conn, final PasswordCallback pwc, final CryptoHelper cryptoHelper) throws ApduConnectionException,
                                                                                                       InvalidCardException,
                                                                                                       BurnedDnieCardException {
        super((byte) 0x00, conn);
        connect(conn);

        this.passwordCallback = pwc;
        if (cryptoHelper == null) {
            throw new IllegalArgumentException("El CryptoHelper no puede ser nula"); //$NON-NLS-1$
        }
        this.cryptoHelper = cryptoHelper;

        this.fastMode = Boolean.getBoolean(FAST_MODE_PROPERTY);

        // Cargamos la informacion publica de los certificados y el certificado
        // de CA intermedia de los certificados de firma y autenticacion
        preloadCertificates();

        // Cargamos la informacion publica con la referencia a las claves
        loadKeyReferences();
    }

    /** Carga la informaci&oacute;n p&uacute;blica con la referencia a las claves de firma. */
    private void loadKeyReferences() {
        final PrKdf prKdf = new PrKdf();
        try {
            prKdf.setDerValue(selectFileByLocationAndRead(PRKDF_LOCATION));
        }
        catch (final Exception e) {
            throw new IllegalStateException("No se ha podido cargar el PrKDF de la tarjeta: " + e.toString()); //$NON-NLS-1$
        }

        for (int i = 0; i < prKdf.getKeyCount(); i++) {
            if (AUTH_KEY_LABEL.equals(prKdf.getKeyName(i))) {
                this.authKeyRef = new DniePrivateKeyReference(this, prKdf.getKeyIdentifier(i), new Location(prKdf.getKeyPath(i)), AUTH_KEY_LABEL);
            }
            else if (SIGN_KEY_LABEL.equals(prKdf.getKeyName(i))) {
                this.signKeyRef = new DniePrivateKeyReference(this, prKdf.getKeyIdentifier(i), new Location(prKdf.getKeyPath(i)), SIGN_KEY_LABEL);
            }
        }
    }

    /** Recupera el n&uacute;mero de serie de un DNIe
     * @return Un array de bytes que contiene el n&uacute;mero de serie del DNIe
     * @throws ApduConnectionException Si la conexi&oacute;n con la tarjeta se proporciona cerrada y no es posible abrirla */
    public byte[] getSerialNumber() throws ApduConnectionException {
        final ResponseApdu response = this.getConnection().transmit(new GetChipInfoApduCommand());
        if (response.isOk()) {
        	return response.getData();
        }
        throw new ApduConnectionException("Respuesta invalida en la obtencion del numero de serie con el codigo: " + response.getStatusWord()); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
	public String getCardName() {
        return "DNIe"; //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public String[] getAliases() {
        return new String[] {
                AUTH_CERT_ALIAS, SIGN_CERT_ALIAS
        };
    }

    /**
     * Carga certificados impostados para el uso b&aacute;sico de la tarjeta en el modo r&aacute;pido
     * y la CA intermedia de los certificados reales.
     */
    private void preloadCertificates() {
        final Cdf cdf = new Cdf();
        try {
        	selectMasterFile();
            cdf.setDerValue(selectFileByLocationAndRead(CDF_LOCATION));
        }
        catch (final Exception e) {
            throw new IllegalStateException("No se ha podido cargar el CDF de la tarjeta: " + e.toString()); //$NON-NLS-1$
        }

        X509Certificate tmpCert;
        for (int i = 0; i < cdf.getCertificateCount(); i++) {
            tmpCert =
                    new FakeX509Certificate(cdf.getCertificateSubjectPrincipal(i),
                                            cdf.getCertificateIssuerPrincipal(i),
                                            cdf.getCertificateSerialNumber(i),
                                            AUTH_CERT_ALIAS.equals(cdf.getCertificateAlias(i)));
            if (AUTH_CERT_ALIAS.equals(cdf.getCertificateAlias(i))) {
                this.authCert = tmpCert;
                this.authCertPath = new Location(cdf.getCertificatePath(i));
            }
            else if (SIGN_CERT_ALIAS.equals(cdf.getCertificateAlias(i))) {
                this.signCert = tmpCert;
                this.signCertPath = new Location(cdf.getCertificatePath(i));
            }
            else {
            	try {
            		final byte[] intermediateCaCertEncoded = deflate(selectFileByLocationAndRead(new Location(cdf.getCertificatePath(i))));
            		this.intermediateCaCert =
            			(X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
            					new ByteArrayInputStream(intermediateCaCertEncoded));
            	}
            	catch (final Exception e) {
            		Logger.getLogger("es.gob.jmulticard").warning( //$NON-NLS-1$
            				"No se ha podido cargar el certificado de la autoridad intermedia de la DGP: " + e.toString()); //$NON-NLS-1$
            		this.intermediateCaCert = null;
            	}
            }
        }
    }

    /** Carga los certificados autenticos de la tarjeta.
     * @throws CryptoCardException Cuando se produce un error en la operaci&oacute;n con la tarjeta
     * @throws es.gob.jmulticard.card.AuthenticationModeLockedException Cuando el DNIe est&aacute; bloqueado
     * @throws es.gob.jmulticard.ui.passwordcallback.CancelledOperationException Cuando se ha cancelado
     *         la inserci&oacute;n del PIN */
    private void loadCertificates() throws CryptoCardException {
        if (this.isSecurityChannelOpen()) {
            return;
        }
        verifyAndLoadCertificates();
    }

    /** {@inheritDoc} */
    public X509Certificate getCertificate(final String alias) throws CryptoCardException {

        // Si no estamos en Modo Rapido, nos aseguramos de que este cargados
        // los certificados de verdad
        if (this.needsRealCertificates || (this.authCert instanceof FakeX509Certificate && !this.fastMode)) {
            loadCertificates();
        }

        if (AUTH_CERT_ALIAS.equals(alias)) {
            return this.authCert;
        }
        if (SIGN_CERT_ALIAS.equals(alias)) {
            return this.signCert;
        }
        if (INTERMEDIATE_CA_CERT_ALIAS.equals(alias)) {
            return this.intermediateCaCert;
        }
        return null;
    }

    /** {@inheritDoc} */
    public void verifyCaIntermediateIcc() throws CertificateException, IOException {
        // No se comprueba
    }

    /** {@inheritDoc} */
    public void verifyIcc() throws CertificateException, IOException {
        // No se comprueba
    }

    /** {@inheritDoc} */
    public byte[] getIccCertEncoded() throws IOException {
        byte[] iccCertEncoded;
        try {
            iccCertEncoded = this.selectFileByIdAndRead(CERT_ICC_FILE_ID);
        }
        catch (final ApduConnectionException e) {
            throw new IOException("Error en el envio de APDU para la seleccion del certificado de componente de la tarjeta: " + e); //$NON-NLS-1$
        }
        catch (final Iso7816FourCardException e) {
            throw new IOException("Error en la seleccion del certificado de componente de la tarjeta: " + e); //$NON-NLS-1$
        }
        return iccCertEncoded;
    }

    /** {@inheritDoc} */
    public void verifyIfdCertificateChain() throws ApduConnectionException {

        // Seleccionamos en la tarjeta la clave publica de la CA raiz del controlador
        try {
            this.setPublicKeyToVerification(DnieCwa14890Constants.REF_C_CV_CA_PUBLIC_KEY);
        }
        catch (final SecureChannelException e) {
            throw new SecureChannelException("Error al seleccionar para verificacion la " //$NON-NLS-1$
                                             + "clave publica de la CA raiz de los certificados verificables por la tarjeta", e); //$NON-NLS-1$
        }

        // Verificamos la CA intermedia del controlador. La clave publica queda almacenada en memoria
        try {
            this.verifyCertificate(DnieCwa14890Constants.C_CV_CA);
        }
        catch (final SecureChannelException e) {
            throw new SecureChannelException("Error en la verificacion del certificado de la CA intermedia de Terminal", e); //$NON-NLS-1$
        }

        // Seleccionamos a traves de su CHR la clave publica del certificado recien cargado en memoria
        // (CA intermedia de Terminal) para su verificacion
        try {
            this.setPublicKeyToVerification(DnieCwa14890Constants.CHR_C_CV_CA);
        }
        catch (final SecureChannelException e) {
            throw new SecureChannelException("Error al establecer la clave publica del certificado de CA intermedia " //$NON-NLS-1$
                                             + "de Terminal para su verificacion en tarjeta", e); //$NON-NLS-1$
        }

        // Enviamos el certificado de Terminal (C_CV_IFD) para su verificacion por la tarjeta
        try {
            this.verifyCertificate(DnieCwa14890Constants.C_CV_IFD);
        }
        catch (final SecureChannelException e) {
            throw new SecureChannelException("Error en la verificacion del certificado de Terminal", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    public byte[] getRefIccPrivateKey() {
        return DnieCwa14890Constants.REF_ICC_PRIVATE_KEY;
    }

    /** {@inheritDoc} */
    public byte[] getChrCCvIfd() {
        return DnieCwa14890Constants.CHR_C_CV_IFD;
    }

    /** {@inheritDoc} */
    public RSAPrivateKey getIfdPrivateKey() {
        return DnieCwa14890Constants.IFD_PRIVATE_KEY;
    }

    /** {@inheritDoc} */
    public void setKeysToAuthentication(final byte[] refPublicKey, final byte[] refPrivateKey) throws ApduConnectionException {
        final CommandApdu apdu = new MseSetAuthenticationKeyApduCommand((byte) 0x00, refPublicKey, refPrivateKey);
        final ResponseApdu res = this.getConnection().transmit(apdu);
        if (!res.isOk()) {
            throw new SecureChannelException("Error durante el establecimiento de las claves publica y privada " + //$NON-NLS-1$
                                             "para atenticacion (error: " + HexUtils.hexify(res.getBytes(), true) + ")" //$NON-NLS-1$ //$NON-NLS-2$
            );
        }
    }

    /** {@inheritDoc} */
    public byte[] getInternalAuthenticateMessage(final byte[] randomIfd, final byte[] chrCCvIfd) throws ApduConnectionException {
        final CommandApdu apdu = new InternalAuthenticateApduCommand((byte) 0x00, randomIfd, chrCCvIfd);
        final ResponseApdu res = this.getConnection().transmit(apdu);
        if (res.isOk()) {
        	return res.getData();
        }
        throw new ApduConnectionException(
    		"Respuesta invalida en la obtencion del mensaje de autenticacion interna con el codigo: " + res.getStatusWord() //$NON-NLS-1$
		);
    }

    /** {@inheritDoc} */
    public boolean externalAuthentication(final byte[] extAuthenticationData) throws ApduConnectionException {
        final CommandApdu apdu = new ExternalAuthenticateApduCommand((byte) 0x00, extAuthenticationData);
        return this.getConnection().transmit(apdu).isOk();
    }

    /** {@inheritDoc} */
    public PrivateKeyReference getPrivateKey(final String alias) {
        this.needsRealCertificates = true;
        if (AUTH_CERT_ALIAS.equals(alias)) {
            return this.authKeyRef;
        }
        else if (SIGN_CERT_ALIAS.equals(alias)) {
            return this.signKeyRef;
        }
        return null;
    }

    /** {@inheritDoc} */
    public byte[] sign(final byte[] data, final String algorithm, final PrivateKeyReference privateKeyReference) throws CryptoCardException {

        if (!(privateKeyReference instanceof DniePrivateKeyReference)) {
            throw new IllegalArgumentException("La referencia a la clave privada tiene que ser de tipo DniePrivateKeyReference"); //$NON-NLS-1$
        }

        if (DialogBuilder.showSignatureConfirmDialog(null, !AUTH_KEY_LABEL.equals(((DniePrivateKeyReference) privateKeyReference).toString())) == 1) {
            throw new CancelledOperationException("Operacion de firma no autorizada por el usuario"); //$NON-NLS-1$
        }

        return signOperation(data, algorithm, privateKeyReference);
    }

    /** Realiza la operaci&oacute;n de firma.
     * @param data Datos que se desean firmar.
     * @param algorithm Algoritmo de firma.
     * @param privateKeyReference Referencia a la clave privada para la firma.
     * @return Firma de los datos.
     * @throws CryptoCardException Cuando se produce un error durante la operaci&oacute;n de firma.
     * @throws es.gob.jmulticard.card.AuthenticationModeLockedException Cuando el DNIe est&aacute; bloqueado. */
    private byte[] signOperation(final byte[] data, final String algorithm, final PrivateKeyReference privateKeyReference) throws CryptoCardException {

        if (!this.isSecurityChannelOpen()) {
            this.verifyAndLoadCertificates();
        }

        ResponseApdu res;
        try {
            CommandApdu apdu = new MseSetSignatureKeyApduCommand((byte) 0x00, ((DniePrivateKeyReference) privateKeyReference).getKeyPath().getLastFilePath());

            res = this.getConnection().transmit(apdu);
            if (!res.isOk()) {
                throw new DnieCardException("Error en el establecimiento de las variables de entorno para firma", res.getStatusWord()); //$NON-NLS-1$
            }

            // TODO: Modificar esta llamada y la clase DigestInfo para que reciba el algoritmo
            // de digest directamente

            final byte[] digestInfo;
            try {
                digestInfo = DigestInfo.encode(algorithm, data, this.cryptoHelper);
            }
            catch (final IOException e) {
                throw new DnieCardException("Error en el calculo del hash para firmar", e); //$NON-NLS-1$
            }

            apdu = new PsoSignHashApduCommand((byte) 0x00, digestInfo);
            res = this.getConnection().transmit(apdu);
            if (!res.isOk()) {
                throw new DnieCardException("Error durante la operacion de firma", res.getStatusWord()); //$NON-NLS-1$
            }
        }
        catch(final LostChannelException e) {
            try {
                this.getConnection().close();
                if (this.getConnection() instanceof Cwa14890OneConnection) {
                    this.setConnection(((Cwa14890OneConnection) this.getConnection()).getSubConnection());
                }
            }
            catch (final Exception ex) {
                throw new DnieCardException("No se pudo recuperar el canal seguro para firmar: " + ex, ex); //$NON-NLS-1$
            }
            return signOperation(data, algorithm, privateKeyReference);
        }
        catch (final ApduConnectionException e) {
            throw new DnieCardException("Error en la transmision de comandos a la tarjeta", e); //$NON-NLS-1$
        }

        return res.getData();
    }

    /** Abre el canal seguro de la tarjeta solicitando el PIN al usuario y carga los
     * certificados de verdad para utilizarlos cuando se desee.
     * @throws CryptoCardException Cuando se produce un error en la operaci&oacute;n con la tarjeta
     * @throws es.gob.jmulticard.card.AuthenticationModeLockedException Cuando el DNIe est&aacute; ha bloqueado
     * @throws es.gob.jmulticard.ui.passwordcallback.CancelledOperationException Cuando se ha cancelado la inserci&oacute;n del PIN */
    private void verifyAndLoadCertificates() throws CryptoCardException {

        // Abrimos el canal seguro si no lo esta ya
        if (!this.isSecurityChannelOpen()) {
            if (!(this.getConnection() instanceof Cwa14890OneConnection)) {
                final Cwa14890OneConnection secureConnection = new Cwa14890OneConnection(this, this.getConnection(), this.cryptoHelper);
                try {
                    this.setConnection(secureConnection);
                }
                catch (final ApduConnectionException e) {
                    throw new CryptoCardException("Error en el establecimiento del canal seguro", e); //$NON-NLS-1$
                }
            }
            try {
                verifyPin(this.passwordCallback);
                if (this.passwordCallback != null) {
                	this.passwordCallback.clearPassword();
                }
            }
            catch (final ApduConnectionException e) {
                throw new CryptoCardException("Error en la apertura del canal seguro: " + e, e); //$NON-NLS-1$
            }
        }

        // Cargamos certificados
        try {
            final CertificateFactory certFactory = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$

            final byte[] authCertEncoded = deflate(selectFileByLocationAndRead(this.authCertPath));
            this.authCert = (X509Certificate) certFactory.generateCertificate(new ByteArrayInputStream(authCertEncoded));

            final byte[] signCertEncoded = deflate(selectFileByLocationAndRead(this.signCertPath));
            this.signCert = (X509Certificate) certFactory.generateCertificate(new ByteArrayInputStream(signCertEncoded));
        }
        catch (final CertificateException e) {
            throw new CryptoCardException(
        		"Error al cargar los certificados reales del DNIe, no es posible obtener una factoria de certificados X.509", e //$NON-NLS-1$
    		);
        }
        catch (final IOException e) {
            throw new CryptoCardException(
        		"Error al cargar los certificados reales del DNIe, error en la descompresion de los datos", e //$NON-NLS-1$
    		);
		}
        catch (final Iso7816FourCardException e) {
            throw new CryptoCardException(
        		"Error al cargar los certificados reales del DNIe, no es posible obtener una factoria de certificados X.509", e //$NON-NLS-1$
    		);
		}
    }

	protected void selectMasterFile() throws ApduConnectionException, FileNotFoundException {
    	selectFileByName(MASTER_FILE_NAME);
    }

    /** Descomprime un certificado contenido en el DNIe.
     * @param compressedCertificate Certificado comprimido en ZIP a partir del 9 byte.
     * @return Certificado codificado.
     * @throws IOException Cuando se produce un error en la descompresion del certificado. */
    private static byte[] deflate(final byte[] compressedCertificate) throws IOException {
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        final Inflater decompressor = new Inflater();
        decompressor.setInput(compressedCertificate, 8, compressedCertificate.length - 8);
        final byte[] buf = new byte[1024];
        try {
            // Descomprimimos los datos
            while (!decompressor.finished()) {
                final int count = decompressor.inflate(buf);
                if (count == 0) {
                    throw new DataFormatException();
                }
                buffer.write(buf, 0, count);
            }
            // Obtenemos los datos descomprimidos
            return buffer.toByteArray();
        }
        catch (final DataFormatException ex) {
            throw new IOException("Error al descomprimir el certificado: " + ex); //$NON-NLS-1$
        }
    }

    private boolean isSecurityChannelOpen() {
        // Si estan cargados los certificados de verdad entonces ya se abrio
        // el canal seguro
        return this.getConnection() instanceof Cwa14890OneConnection && this.getConnection().isOpen()
               && !(this.authCert instanceof FakeX509Certificate);
    }
}