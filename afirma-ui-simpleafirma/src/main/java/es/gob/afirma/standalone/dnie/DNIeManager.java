/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
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

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.SimpleAfirma;

/** Gestor de almac&eacute;n (<code>KeyStore</code>) DNIe v&iacute;a PKCS#11 y JSR-268.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DNIeManager {

    /** Evento que indica que se ha insertado un DNIe. */
    public static final String DNI_INSERTED = "DniInserted"; //$NON-NLS-1$

    /** Evento que indica ha ocurrido un error relacionado con <code>SmartCardIO</code>. */
    public static final String CARD_EXCEPTION = "CardException"; //$NON-NLS-1$

    /** Evento que indica que se ha insertado una tarjeta que no es un DNIe. */
    public static final String NOT_DNI_INSERTED = "NotDniInterted"; //$NON-NLS-1$

    /** Evento que indica que se ha insertado un DNIe con su memoria vol&aacute;til borrada. */
    public static final String BLOWN_DNI_INSERTED = "BlownDniInserted"; //$NON-NLS-1$

    private final List<CardTerminal> terminals;

    private final PropertyChangeListener pcListener;

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
            throw new DNIeManagerException("No se ha podido obtener la lista de lectores de tarjetas", e); //$NON-NLS-1$
        }
        if (this.terminals.isEmpty()) {
        	throw new DNIeManagerException("No se ha detectado ningun lector de tarjetas"); //$NON-NLS-1$
        }
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
        	throw new DNIeManagerException("No se gestiona directamente DNIe en Mac OS X"); //$NON-NLS-1$
        }
        for (final CardTerminal terminal : this.terminals) {
            Logger.getLogger("es.gob.afirma").info("Detectado lector de tarjetas: " + terminal.getName());  //$NON-NLS-1$//$NON-NLS-2$
        }
    }

    /** Espera a la inserci&oacute;n de un DNIe en alguno de los lectores del sistema.
     * La espera termina cuando se inserta un DNIe, se detecta un DNIe con su memoria vol&aacute;til borrada
     * o ocurre alg&uacute;n error relacionado con <code>SmartCardIO</code> */
    public void waitForDnie() {
        if (this.pcListener == null) {
        	return;
        }
        if (this.terminals == null || this.terminals.size() < 1) {
        	return;
        }
        // El error de Java http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6963006 puede hace que no funcione
        // la monitorizacion de multi-hilo en JRE anteriores a la 6u25.
        for (final CardTerminal cardTerminal : this.terminals) {
            new CardTerminalMonitor(cardTerminal, this.pcListener).startMonitoring();
        }
    }

    /** Comprueba si el ATR proporcionado corresponde a un DNIe.
     * @param atr ATR que se dese comprobar si pertenece a un DNIe
     * @return <code>true</code> si el ATR proporcionado es de un DNIe, <code>false</code> si no lo es */
    static boolean itsDNIe(final byte[] atr) throws BlownDNIeException {

        if (atr == null) {
            return false;
        }

        // Si el ATR termina en 65-81 es que el DNIe esta estropeado
        if (atr.length > 1 && atr[atr.length - 2] == (byte) 0x65 && atr[atr.length - 1] == (byte) 0x81) {
            throw new BlownDNIeException();
        }

        // Mascaras de ATR del DNIe:
        // 00 01 06 07 08 09 10 18 19
        // 3b,7f,00,00,00,00,6a,44,4e,49,65,00,00,00,00,00,00,00,90,00
        // ff,ff,00,ff,ff,ff,ff,ff,ff,ff,ff,00,00,00,00,00,00,00,ff,ff

        if (atr.length != 20) {
            return false;
        }
        if (atr[0] == (byte) 0x3B && atr[1] == (byte) 0x7F
            && atr[6] == (byte) 0x6A
            && atr[7] == (byte) 0x44
            && atr[8] == (byte) 0x4E
            && atr[9] == (byte) 0x49
            && atr[10] == (byte) 0x65
            && atr[18] == (byte) 0x90
            && atr[19] == (byte) 0x00) {
            return true;
        }

        return false;
    }

    private static final class CardTerminalMonitor extends Frame {

        private static final long serialVersionUID = 3311217290591616359L;

        private final CardTerminal cardTerminal;

        CardTerminalMonitor(final CardTerminal terminal, final PropertyChangeListener pcl) {
            if (pcl != null) {
                this.addPropertyChangeListener(pcl);
            }
            this.cardTerminal = terminal;
        }

        void startMonitoring() {
            if (this.cardTerminal == null) {
                return;
            }
            Card card = null;
            boolean validCard = true;
            while (true) {
                do {
                    try {
                        this.cardTerminal.waitForCardPresent(0);
                        if (SimpleAfirma.DEBUG) {
                            System.out.println("Tarjeta insertada en el lector '" + this.cardTerminal.getName() + "'"); //$NON-NLS-1$ //$NON-NLS-2$
                        }
                        card = this.cardTerminal.connect("*"); //$NON-NLS-1$
                        validCard = true;
                    }
                    catch (final CardException e) {
                        if (SimpleAfirma.DEBUG) {
                            e.printStackTrace();
                        }
                        validCard = false;
                        firePropertyChange(CARD_EXCEPTION, "", this.cardTerminal.getName()); //$NON-NLS-1$

                        try {
                            this.cardTerminal.waitForCardAbsent(0);
                        }
                        catch (final CardException ce) { /* Ignoramos los errores */ }
                        //return;
                    }
                } while (!validCard);
                // firePropertyChange("CardInserted", false, true);
                try {
                    if (card != null && (!itsDNIe(card.getATR().getBytes()))) {
                        if (SimpleAfirma.DEBUG) {
                            System.out.println("Detectada tarjeta extrana"); //$NON-NLS-1$
                        }
                        firePropertyChange(NOT_DNI_INSERTED, "", this.cardTerminal.getName()); //$NON-NLS-1$
                        try {
                            this.cardTerminal.waitForCardAbsent(0);
                        }
                        catch (final CardException e) {
                            firePropertyChange(CARD_EXCEPTION, "", this.cardTerminal.getName()); //$NON-NLS-1$
                            return;
                        }
                    }
                    else {
                        if (SimpleAfirma.DEBUG) {
                            System.out.println("Detectado DNIe"); //$NON-NLS-1$
                        }
                        firePropertyChange(DNI_INSERTED, "", this.cardTerminal.getName()); //$NON-NLS-1$
                        return;
                    }
                }
                catch (final BlownDNIeException e) {
                    firePropertyChange(BLOWN_DNI_INSERTED, false, true);
                    return;
                }
            }
        }

    }

}
