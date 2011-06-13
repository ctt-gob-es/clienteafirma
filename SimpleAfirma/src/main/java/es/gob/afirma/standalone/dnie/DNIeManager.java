/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.dnie;

import java.awt.Frame;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.logging.Logger;

import javax.smartcardio.Card;
import javax.smartcardio.CardException;
import javax.smartcardio.CardTerminal;
import javax.smartcardio.TerminalFactory;

import es.gob.afirma.standalone.SimpleAfirma;

/** Gestor de almac&eacute;n (<code>KeyStore</code>) DNIe v&iacute;a PKCS#11 y JSR-268.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DNIeManager {

    /** Evento que indica que se ha insertado un DNIe. */
    public static final String DNI_INSERTED = "DniInserted";

    /** Evento que indica ha ocurrido un error relacionado con <code>SmartCardIO</code>. */
    public static final String CARD_EXCEPTION = "CardException";

    /** Evento que indica que se ha insertado una tarjeta que no es un DNIe. */
    public static final String NOT_DNI_INSERTED = "NotDniInterted";

    /** Evento que indica que se ha insertado un DNIe con su memoria vol&aacute;til borrada. */
    public static final String BLOWN_DNI_INSERTED = "BlownDniInserted";

    private final List<CardTerminal> terminals;

    final PropertyChangeListener pcListener;

    /** Construye un gestor de almac&eacute;n (<code>KeyStore</code>) <DNIe v&iacute;a PKCS#11 y JSR-268.
     * @param pcl Clase a la que se notificar&aacute;n los eventos relacionados con la inserci&oacute;n y extracci&oacute;n de tarjetas en los
     *        lectores del sistema
     * @throws DNIeManagerException cuando ocurre alg&uacute;n problema relacionado con <code>SmartCardIO</code> */
    public DNIeManager(final PropertyChangeListener pcl) throws DNIeManagerException {
        this.pcListener = pcl;
        try {
            this.terminals = TerminalFactory.getDefault().terminals().list();
        }
        catch (final CardException e) {
            throw new DNIeManagerException("No se ha podido obtener la lista de lectores de tarjetas", e);
        }
        if (this.terminals.isEmpty()) throw new DNIeManagerException("No se ha detectado ningun lector de tarjetas");
        for (final CardTerminal terminal : this.terminals) {
            Logger.getLogger("es.gob.afirma").info("Detectado lector de tarjetas: " + terminal.getName());
        }
    }

    /** Espera a la inserci&oacute;n de un DNIe en alguno de los lectores del sistema.
     * La espera termina cuando se inserta un DNIe, se detecta un DNIe con su memoria vol&aacute;til borrada
     * o ocurre alg&uacute;n error relacionado con <code>SmartCardIO</code> */
    public void waitForDnie() {
        if (this.pcListener == null) return;
        for (final CardTerminal cardTerminal : this.terminals) {
            new CardTerminalMonitor(cardTerminal, this.pcListener).startMonitoring();
        }
    }

    /** Comprueba si el ATR proporcionado corresponde a un DNIe.
     * @param atr ATR que se dese comprobar si pertenece a un DNIe
     * @return <code>true</code> si el ATR proporcionado es de un DNIe, <code>false</code> si no lo es */
    private static boolean itsDNIe(final byte[] atr) throws BlownDNIeException {

        if (atr == null) return false;

        // Si el ATR termina en 65-81 es que el DNIe esta estropeado
        if (atr.length > 1 && atr[atr.length - 2] == (byte) 0x65 && atr[atr.length - 1] == (byte) 0x81) {
            throw new BlownDNIeException();
        }

        // Mascaras de ATR del DNIe:
        // 00 01 06 07 08 09 10 18 19
        // 3b,7f,00,00,00,00,6a,44,4e,49,65,00,00,00,00,00,00,00,90,00
        // ff,ff,00,ff,ff,ff,ff,ff,ff,ff,ff,00,00,00,00,00,00,00,ff,ff

        if (atr.length != 20) return false;
        if (atr[0] == (byte) 0x3B && atr[1] == (byte) 0x7F
            && atr[6] == (byte) 0x6A
            && atr[7] == (byte) 0x44
            && atr[8] == (byte) 0x4E
            && atr[9] == (byte) 0x49
            && atr[10] == (byte) 0x65
            && atr[18] == (byte) 0x90
            && atr[19] == (byte) 0x00) return true;

        return false;
    }

    private static final class CardTerminalMonitor extends Frame {

        private static final long serialVersionUID = 3311217290591616359L;

        private final CardTerminal cardTerminal;

        CardTerminalMonitor(final CardTerminal terminal, PropertyChangeListener pcl) {
            if (pcl != null) this.addPropertyChangeListener(pcl);
            this.cardTerminal = terminal;
        }

        void startMonitoring() {
            if (this.cardTerminal == null) return;
            Card card;
            while (true) {
                try {
                    this.cardTerminal.waitForCardPresent(0);
                    if (SimpleAfirma.DEBUG) {
                        System.out.println("Tarjeta insertada en el lector '" + this.cardTerminal.getName() + "'");
                    }
                    card = this.cardTerminal.connect("*");
                }
                catch (final CardException e) {
                    if (SimpleAfirma.DEBUG) {
                        e.printStackTrace();
                    }
                    firePropertyChange(CARD_EXCEPTION, "", this.cardTerminal.getName());
                    return;
                }
                // firePropertyChange("CardInserted", false, true);
                try {
                    if (!itsDNIe(card.getATR().getBytes())) {
                        if (SimpleAfirma.DEBUG) {
                            System.out.println("Detectada tarjeta extraña");
                        }
                        firePropertyChange(NOT_DNI_INSERTED, "", this.cardTerminal.getName());
                        try {
                            this.cardTerminal.waitForCardAbsent(0);
                        }
                        catch (final CardException e) {
                            firePropertyChange(CARD_EXCEPTION, "", this.cardTerminal.getName());
                            return;
                        }
                    }
                    else {
                        if (SimpleAfirma.DEBUG) {
                            System.out.println("Detectado DNIe");
                        }
                        firePropertyChange(DNI_INSERTED, "", this.cardTerminal.getName());
                        return;
                    }
                }
                catch (final BlownDNIeException e) {
                    firePropertyChange("BlownDniInserted", false, true);
                    return;
                }
            }
        }

    }

}
