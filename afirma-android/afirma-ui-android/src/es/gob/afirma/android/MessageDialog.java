package es.gob.afirma.android;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;

/** Di&aacute;logo modal con el que mostrar al usuario un mensaje y un bot&oacute;n para ocultar el
 * di&aacute;logo y, opcionalmente, realizar una acci&oacute;n. */
@SuppressLint("ValidFragment")
class MessageDialog extends DialogFragment {

	private String message = null;

	private DialogInterface.OnClickListener listener = null;

	private final AlertDialog.Builder dialogBuilder;

	/**
	 * Construye el di&aacute;logo.
	 * @param message Mensaje que mostrar al usuario.
	 * @param listener Manejador con la acci&oacute;n a realizar al cerrar el di&aacute;logo.
	 */
	@SuppressLint("ValidFragment")
	MessageDialog(final String message, final DialogInterface.OnClickListener listener, final Activity activity) {
		this.message = message;
		this.listener = listener;

		if (activity == null) {
			this.dialogBuilder = new AlertDialog.Builder(getActivity());
		}
		else {
			this.dialogBuilder = new AlertDialog.Builder(activity);
		}
	}

	void setMessage(final String message) {
		this.message = message;
	}

	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {
		this.dialogBuilder.setMessage(this.message);
		if (this.listener != null) {
			this.dialogBuilder.setPositiveButton(android.R.string.ok, this.listener);
		}
		return this.dialogBuilder.create();
	}
}
