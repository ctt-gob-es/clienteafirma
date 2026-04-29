/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.util.Map;

import es.gob.afirma.core.misc.protocol.ProtocolInvocationUriParser;
import es.gob.afirma.standalone.SimpleAfirma;

/** Clase que gestiona los mensajes mostrados por el di&aacute;logo de progreso
 * y que muestra o esconde el di&aacute;logo.*/

public final class ProgressInfoDialogManager {

	private static ProgressInfoDialog infoDialog;
	
	private static boolean showProgressDialog = true;
	
	/** Par&aacute;metro de entrada para mostrar o no el di&aacute;logo de espera. */
	protected static final String SHOW_LOADING_DIALOG_PARAM = "dlgload"; //$NON-NLS-1$
	
	public static void init(String [] args) {
		if (args != null && args.length > 0 
	    		 && args[0].toLowerCase().startsWith(SimpleAfirma.PROTOCOL_URL_START_LOWER_CASE)) {
			Map <String,String> paramsMap = ProtocolInvocationUriParser.parserUri(args[0]);
			if (paramsMap.containsKey(SHOW_LOADING_DIALOG_PARAM)) {
				showProgressDialog = Boolean.parseBoolean(paramsMap.get(SHOW_LOADING_DIALOG_PARAM));
			}
		} else {
			// Para la aplicacion de escritorio no se mostrara
			showProgressDialog = false;
		}
	}
	
	/** Elimina el di&aacute;logo de la vista 
	 * @param message Mensaje del di&aacute;logo. */
	public static void showProgressDialog(final String message) {
		
	    if (!showProgressDialog) {
			return;
		}

	    if (infoDialog == null) {
	        infoDialog = new ProgressInfoDialog(null);
	    }

	    if (infoDialog.isEnabledDialog()) {
	    	infoDialog.setMessage(message);
	    	infoDialog.setVisible(true);
	    }
	    
	}

	/** Elimina el di&aacute;logo de la vista */
	public static void hideProgressDialog() {
		if (infoDialog != null) {
			infoDialog.setVisible(false);
		}	
	}

}
