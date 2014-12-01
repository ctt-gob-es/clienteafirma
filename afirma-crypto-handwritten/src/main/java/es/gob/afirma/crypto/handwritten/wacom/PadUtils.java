package es.gob.afirma.crypto.handwritten.wacom;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.util.logging.Logger;
import java.util.zip.Deflater;

import com.WacomGSS.STU.NotConnectedException;
import com.WacomGSS.STU.STUException;
import com.WacomGSS.STU.Tablet;
import com.WacomGSS.STU.UsbDevice;
import com.WacomGSS.STU.Protocol.Capability;
import com.WacomGSS.STU.Protocol.EncodingMode;
import com.WacomGSS.STU.Protocol.InkingMode;
import com.WacomGSS.STU.Protocol.PenData;
import com.WacomGSS.STU.Protocol.ProtocolHelper;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.crypto.handwritten.Rectangle;
import es.gob.afirma.crypto.handwritten.SignaturePadConnectionException;
import es.gob.afirma.crypto.handwritten.SignaturePadException;

/** Utilidades para la implementaci&oacute;n de la interfaz de firma en tableta.*/
public class PadUtils {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Tiempo a esperar para reintentar la reconexi&oacute;n USB si esta falla. */
	private static final int RECONNECT_WAIT_TIME_MILIS = 500;

	/** N&uacute;mero m&aacute;ximo re reintentos de reconexi&oacute;n USB. */
	private static final int RECONNECT_MAX_ATTEMPS = 3;

	static {
		setLibraryPath();
	}

	private PadUtils() {
		// No instanciable
	}

	static boolean fitsInto(final Rectangle sigArea, final Dimension availableScreenSize) {
		if (sigArea == null || availableScreenSize == null) {
			return false;
		}
		if (sigArea.x + sigArea.width < availableScreenSize.width && sigArea.y + sigArea.height < availableScreenSize.height) {
			return true;
		}
		return false;
	}

	static byte[] penDataArrayToIso19794(final PenData[] penDataArray) {
		if (penDataArray == null) {
			throw new IllegalArgumentException("El array de muestras no puede ser nulo"); //$NON-NLS-1$
		}

		return compress(Iso197947Helper.createIsoXml(penDataArray).getBytes());
	}

	static byte[] penDataArrayToRaw(final PenData[] penDataArray) {
		if (penDataArray == null) {
			throw new IllegalArgumentException("El array de muestras no puede ser nulo"); //$NON-NLS-1$
		}
		StringBuilder sb = new StringBuilder();
		sb.append('[');
		for (final PenData pd : penDataArray) {
			sb.append('[');
			sb.append(Integer.toString(pd.getX()));
			sb.append(',');
			sb.append(pd.getY());
			sb.append(',');
			sb.append(pd.getPressure());
			sb.append(']');
		}
		sb.append(']');
		return compress(sb.toString().getBytes());
	}

	private static byte[] compress(final byte[] in) {
		try {
		     byte[] output = new byte[in.length];
		     Deflater compresser = new Deflater(Deflater.BEST_COMPRESSION);
		     compresser.setInput(in);
		     compresser.finish();
		     final int compressedDataLength = compresser.deflate(output);
		     compresser.end();
		     final byte[] ret = new byte[compressedDataLength];
		     for (int i=0; i<compressedDataLength;i++) {
		    	 ret[i] = output[i];
		     }
		     return ret;
		}
		catch(Exception e) {
			LOGGER.warning("No se ha podido comprimir el penData, se introducira sin comprimir: " + e); //$NON-NLS-1$
		}
		return in;
	}

	static void setLibraryPath() {
		final String dir = Platform.getUserHome() + File.separator + ".afirma" + File.separator +  "handwritten" + File.separator + "wacom" + File.separator + "lib" + File.separator + Platform.getOS() + File.separator + Platform.getJavaArch(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		File f = new File(dir);
		if (!f.exists()) {
			f.mkdirs();
		}
		if (!f.isDirectory()) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"La ruta '" + dir + "' existe, pero no es un directorio" //$NON-NLS-1$ //$NON-NLS-2$
			);
			return;
		}
		if (!f.canRead()) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"La ruta '" + dir + "' existe, pero no hay permisos de lectura" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		final String lib;
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			lib = "wgssSTU.dll"; //$NON-NLS-1$
		}
		else if (Platform.OS.LINUX.equals(Platform.getOS())) {
			lib = "libwgssSTU.so.2.1.0"; //$NON-NLS-1$
		}
		else {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Sistema operativo no soportado: "  + Platform.getOS() //$NON-NLS-1$
			);
			return;
		}

		f = new File(dir + File.separator + lib);
		if (!f.exists()) {
			try {
				final byte[] libBytes = AOUtil.getDataFromInputStream(
					PadUtils.class.getResourceAsStream(
						"/wacom/lib/" + Platform.getOS() + "/" + Platform.getJavaArch() + "/" + lib //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					)
				);
				final OutputStream fos = new FileOutputStream(f);
				fos.write(libBytes);
				fos.close();
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"Fallo la escritura de la biblioteca Wacom: "  + e //$NON-NLS-1$
				);
			}
		}

		System.setProperty("java.library.path", dir); //$NON-NLS-1$

		try {

		    final Field sysPathsField = ClassLoader.class.getDeclaredField("sys_paths"); //$NON-NLS-1$
		    sysPathsField.setAccessible(true);
		    sysPathsField.set(null, null);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error estableciendo el directorio de carga de DLL Wacom" //$NON-NLS-1$
			);
		}
	}

	/** Obtiene la tableta conectada con la que se va a proceder a firmar.
	 * @return Tableta conectada con la que se va a proceder a firmar.
	 * @throws SignaturePadException Excepción genérica de un dispositivo de captura de firmas. */
	public static Tablet getTablet() throws SignaturePadException {
		// Buscamos la primera tableta conectada al sistema y la inicializamos
		final com.WacomGSS.STU.UsbDevice[] usbDevices = UsbDevice.getUsbDevices();
		final UsbDevice usbDevice;
		if (usbDevices != null && usbDevices.length > 0) {
			usbDevice = usbDevices[0];
		}
		else {
			throw new SignaturePadConnectionException(
				"No hay tabletas de firma conectadas al sistema" //$NON-NLS-1$
			);
		}
		return initializeTablet(usbDevice);
	}

	static Capability getTabletCapability(final Tablet tablet) throws SignaturePadException {
		try {
			return tablet.getCapability();
		}
		catch (final Exception e) {
			tablet.disconnect();
			throw new SignaturePadException(
				"Error obteniendo las capacidades de la tableta de captura: " + e, e //$NON-NLS-1$
			);
		}
	}

	static void disposeTablet(final Tablet tablet, final boolean encrypted) {
		if (tablet != null) {
			try {
				tablet.setInkingMode(InkingMode.Off);
				if (encrypted) {
					tablet.endCapture();
				}
				tablet.setClearScreen();
			}
			catch (final STUException t) {
				LOGGER.warning("Error cerrando el dipositivo de captura de firmas: " + t); //$NON-NLS-1$
			}
			try {
				tablet.reset();
			}
			catch(final Exception e) {
				LOGGER.warning("Error reiniciando la tableta: " + e); //$NON-NLS-1$
			}
			tablet.disconnect();
			LOGGER.info("Tableta desconectada y lista para la proxima captura"); //$NON-NLS-1$
		}
	}

	static void setTabletSignatureArea(final Tablet tablet,
			                           final Rectangle sigArea) throws SignaturePadException {
		if (sigArea != null) {
			try {
				tablet.setHandwritingDisplayArea(
					new com.WacomGSS.STU.Protocol.Rectangle(
							sigArea.x,
							sigArea.y,
							sigArea.x + sigArea.width,
							sigArea.y + sigArea.height
					)
				);
			}
			catch (final STUException e) {
				throw new SignaturePadException(
					"Error estableciendo el rectangulo de firma en la tableta de captura: " + e, e //$NON-NLS-1$
				);
			}
		}
	}

	private static Tablet initializeTablet(final UsbDevice usbDevice) throws SignaturePadConnectionException {
		final Tablet tab = new Tablet();
		tab.setEncryptionHandler(new MyEncryptionHandler());
		tab.setEncryptionHandler2(new MyEncryptionHandler2());
		int retries = RECONNECT_MAX_ATTEMPS;
		while (tab.usbConnect(usbDevice, true) != 0) {
			try {
				Thread.sleep(RECONNECT_WAIT_TIME_MILIS);
			}
			catch (final InterruptedException e1) {
				// Se ignora
			}
			if (retries-- < 0) {
				throw new SignaturePadConnectionException();
			}
		}
		return tab;
	}

	static boolean getColorEnabled(final Tablet tablet,
			                       final Capability capability) throws SignaturePadConnectionException {
		try {
			return ProtocolHelper.encodingFlagSupportsColor(
				ProtocolHelper.simulateEncodingFlag(
					tablet.getProductId(),
					capability.getEncodingFlag()
				)
			) && tablet.supportsWrite();
		}
		catch (final NotConnectedException e) {
			tablet.disconnect();
			throw new SignaturePadConnectionException(
				"Error determinando si la tableta debe usar o no color: " + e, e //$NON-NLS-1$
			);
		}
	}

	static EncodingMode getTabletEncodingMode(final boolean useColor,
			                                  final Tablet tablet) throws SignaturePadConnectionException {
		final boolean supportsWrite;
		try {
			supportsWrite = tablet.supportsWrite();
		}
		catch (final NotConnectedException e) {
			tablet.disconnect();
			throw new SignaturePadConnectionException(
				"Error intentando determinar si la tableta soporta escritura de alta velocidad: " + e, e //$NON-NLS-1$
			);
		}
		if (useColor) {
			if (supportsWrite) {
				return EncodingMode.EncodingMode_16bit_Bulk;
			}
			return EncodingMode.EncodingMode_16bit;
		}
		return EncodingMode.EncodingMode_1bit;
	}

	static byte[] convertBitmapToWacomNative(final BufferedImage bitmap, final boolean useColor) {
		return ProtocolHelper.flatten(
			bitmap,
			bitmap.getWidth(),
			bitmap.getHeight(),
			useColor
		);
	}

	/** Obtiene el nombre del modelo de la tableta sin espacios en blanco.
	 * @param tablet Tableta conectada.
	 * @return Nombre del modelo de la tableta sin espacios en blanco. */
	public static String getTabletModel(final Tablet tablet) {
		// Modelo de la tableta
		String model;
		try {
			model = tablet.getInformation().getModelName().replace(" ", "_");  //$NON-NLS-1$//$NON-NLS-2$
		}
		catch(final Exception e) {
			LOGGER.warning("Error en la obtecion del modelo de la tableta."); //$NON-NLS-1$
			return "UNKNOWN"; //$NON-NLS-1$
		}

		return model;
	}
}
