package es.gob.afirma.crypto.handwritten.wacom;

import com.WacomGSS.STU.STUException;

interface PadButtonsListener {
	void pressOkButton();
	void pressClearButton() throws STUException;
	void pressCancelButton();
}
