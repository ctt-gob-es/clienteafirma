import old.es.gob.afirma.crypto.handwritten.wacom.WacomSignaturePadManager;
import es.gob.afirma.crypto.handwritten.SignaturePadManager;


public final class Demo {

	/** Main
	 * @param args
	 * @throws Exception */
	public static void main(final String[] args) throws Exception {
		final SignaturePadManager padManager = new WacomSignaturePadManager(WacomSignaturePadManager.getSigningDevicesNames()[0]);
		System.out.println(padManager.getDeviceScreenWidth());
		System.out.println(padManager.getDeviceScreenHeight());
	}

}
