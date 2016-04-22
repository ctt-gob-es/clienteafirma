package es.gob.afirma.android.signfolder;

interface DialogFragmentListener {

	void onDialogPositiveClick(int dialogId, String reason);

	void onDialogNegativeClick(int dialogId);
}
