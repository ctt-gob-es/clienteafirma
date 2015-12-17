package es.gob.afirma.keystores.mozilla.bintutil;

import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** Analizador de ficheros MS-PE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s Capote. */
public class MsPortableExecutable {

	private static final byte PE_SIGNATURE_OFFSET = (byte) 0x3c;

	private final String description;
	private final PeMachineType machine;

	/** Obtiene el tipo de m&aacute;quina requerido para ejecutar este PE.
	 * @return Tipo de m&aacute;quina requerido para ejecutar este PE. */
	public PeMachineType getPeMachineType() {
		return this.machine;
	}

	@Override
	public String toString() {
		return this.description;
	}

	/** Fichero PE de Microsoft.
	 * @param peFile Fichero PE (<i>Portable Executable</i>) de Microsoft.
	 * @throws PEParserException Si ocurre cualquier problema durante el an&aacute;lisis del PE. */
	public MsPortableExecutable(final byte[] peFile) throws PEParserException {

		final int coffOffset = AOBinUtil.getU2(peFile, PE_SIGNATURE_OFFSET) + 4;
		if (!"PE\0\0".equals( //$NON-NLS-1$
			new String(new byte[] {
				peFile[coffOffset -4],
				peFile[coffOffset -3],
				peFile[coffOffset -2],
				peFile[coffOffset -1]
			})
		)) {
			throw new PEParserException(
				"Cabecera PE\\0\\0 no encontrada en el offset " + AOUtil.hexify(new byte[] { PE_SIGNATURE_OFFSET }, false) //$NON-NLS-1$
			);
		}

		final StringBuilder sb = new StringBuilder("Fichero Microsoft PE"); //$NON-NLS-1$

		sb.append("\n Cabecera PE\\0\\0 encontrada en el offset " + AOUtil.hexify(new byte[] { PE_SIGNATURE_OFFSET }, false)); //$NON-NLS-1$

		this.machine = PeMachineType.getPeMachineType(
			AOUtil.hexify(
				new byte[] {
					peFile[coffOffset+1], // +0
					peFile[coffOffset],   // +1
				},
				true
			)
		);

		sb.append("\n Tipo de maquina: " + this.machine);  //$NON-NLS-1$

		final int numberOfSections = AOBinUtil.getU2(peFile, coffOffset+2); // +2, +3

		sb.append("\n Numero de secciones de la tabla de secciones: " + numberOfSections); //$NON-NLS-1$

		//int timeDateStamp = AOBinUtil.getInt(peFile, coffOffset+4); // +4, +5, +6, +7

        if (AOBinUtil.getInt(peFile, coffOffset+8) != 0) { // +8, +9, +10, +11
        	Logger.getLogger("es.gob.afirma").warning("El puntero a la informacion de depuracion del COFF no es cero (se implementa una caracteristica deprecada)"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        if (AOBinUtil.getInt(peFile, coffOffset+12) != 0) { // +12, +13, +14, +15
        	Logger.getLogger("es.gob.afirma").warning("El numero de entradas de la tabla de simbolos no es cero (se implementa una caracteristica deprecada)"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final int sizeOfOptionalHeader = AOBinUtil.getU2(peFile, coffOffset+16); // +16, +17

        if (sizeOfOptionalHeader == 0) {
			sb.append(
				" El fichero es un objeto (no tiene cabecera opcional)" //$NON-NLS-1$
			);
		}
        else {
			sb.append(
				" El fichero es ejecutable (tamano de la cabecera opcional: " + sizeOfOptionalHeader + ")" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

        final byte[] characteristics = new byte[] {peFile[coffOffset+19], peFile[coffOffset+18]}; // +18, +19

        if (0 != (byte) (characteristics[1] & (byte) 0x01)) {
			sb.append("\n El fichero no contiene relocalizaciones de base"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x02)) {
			sb.append("\n Imagen valida"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x04)) {
			Logger.getLogger("es.gob.afirma").warning("Numeros de linea de la imagen COFF eliminados (se implementa una caracteristica deprecada"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x08)) {
			Logger.getLogger("es.gob.afirma").warning("Entradas de la tabla de simbolos de la imagen COFF para simbolos locales eliminadas (se implementa una caracteristica deprecada)"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x10)) {
			Logger.getLogger("es.gob.afirma").warning("Conjunto trucando agresivamente (se implementa una caracteristica deprecada para Windows 2000 y posteriores)"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x20)) {
			sb.append("\n La aplicacion puede gestionar direccionamiento superior a 2GB"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x40)) {
			Logger.getLogger("es.gob.afirma").warning("Se hace uso de un bit reservado para uso futuro"); //$NON-NLS-1$ //$NON-NLS-2$
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x80)) {
			sb.append("\n Arquitectura Little Endian"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x01)) {
			sb.append("\n Arquitectura con palabras de 32 bits"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x02)) {
			sb.append("\n Informacion de depuracion eliminada de la imagen"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x04)) {
			sb.append("\n Si la imagen se encuentra en almacenamiento extraible, debe cargarse completamente y copiarse al fichero de intercambio"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x08)) {
			sb.append("\n Si la imagen se encuantra en almacenamiento en red, debe cargarse completamente y copiarse al fichero de intercambio"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x10)) {
			sb.append("\n La imagen es un fichero de sistema, no un programa de usuario"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x20)) {
			sb.append("\n La imagen es una biblioteca de enlace dinamico"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x40)) {
			sb.append("\n La imagen debe ejecutarse en una maquina con un unico procesador"); //$NON-NLS-1$
		}
        if (0 != (byte) (characteristics[0] & (byte) 0x20)) {
			sb.append("\n Arquitectura Big Endian"); //$NON-NLS-1$
		}

        //sb.append("\n Bits de caracteristicas: " + AOBinUtil.hexify(characteristics, true));

        if (sizeOfOptionalHeader > 0) {

        	final int optionalHeaderOffset = 20;

        	boolean pe32Plus = false;

        	// Magic +0, +1
        	if (peFile[coffOffset+optionalHeaderOffset+1] == (byte) 0x01) {
        		if (peFile[coffOffset+optionalHeaderOffset+0] == (byte) 0x0B) {
					sb.append("\n La imagen es un ejecutable normal (PE32)"); //$NON-NLS-1$
				}
        		else if (peFile[coffOffset+optionalHeaderOffset+0] == (byte) 0x07) {
					sb.append("\n La imagen es una ROM"); //$NON-NLS-1$
				}
        		else {
					Logger.getLogger("es.gob.afirma").warning("El fichero de imagen es de un tipo desconocido (Magic: " + AOUtil.hexify(new byte[] {peFile[coffOffset+21], peFile[coffOffset+20]}, true) + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
        	}
        	else if (peFile[coffOffset+optionalHeaderOffset+1] == (byte) 0x02 && peFile[coffOffset+20] == (byte) 0x0B) {
        		sb.append("\n La imagen es un ejecutable PE32+"); //$NON-NLS-1$
        		pe32Plus = true;
        	}
        	else {
				Logger.getLogger("es.gob.afirma").warning("El fichero de imagen es de un tipo desconocido (Magic: " + AOUtil.hexify(new byte[] {peFile[coffOffset+21], peFile[coffOffset+20]}, true) + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}

        	// Linker, +2, +3
        	sb.append("\n Version del enlazador usado para crear la imagen: " + peFile[coffOffset+optionalHeaderOffset+2] + "." + peFile[coffOffset+optionalHeaderOffset+3]); //$NON-NLS-1$ //$NON-NLS-2$

        	// Version del sistema operativo +40, +41, +42, +43 - WINDOWS SPECIFIC
        	sb.append("\n Version requerida del sistema operativo: " + //$NON-NLS-1$
        		AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+40) +
	        	"." + //$NON-NLS-1$
	        	AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+42)
        	);

//        	// Version de la imagen +44, +45, +46, +47
//        	sb.append("\n Version de la imagen: " +
//        		AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+44) +
//	        	"." +
//	        	AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+46)
//        	);

//        	// Checksum +64 +65 +66 +67
//        	sb.append("\n Checksum de la imagen: " +
//        		AOBinUtil.hexify(new byte[] {
//    				peFile[coffOffset+optionalHeaderOffset+64],
//    				peFile[coffOffset+optionalHeaderOffset+65],
//    				peFile[coffOffset+optionalHeaderOffset+66],
//    				peFile[coffOffset+optionalHeaderOffset+67]
//        		}, true)
//        	);

        	// Subsistema Windows - WINDOWS SPECIFIC
        	sb.append("\n Tipo de subsistema Windows: "); //$NON-NLS-1$
        	switch (AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+68)) {
			case 0:
				sb.append("\n Desconocido"); //$NON-NLS-1$
				break;
			case 1:
				sb.append("\n Controlador de dispositiovo o proceso nativo Windows"); //$NON-NLS-1$
				break;
			case 2:
				sb.append("\n Windows en modo grafico (GUI)"); //$NON-NLS-1$
				break;
			case 3:
				sb.append("\n Windows en modo texto"); //$NON-NLS-1$
				break;
			case 7:
				sb.append("\n POSIX en modo texto"); //$NON-NLS-1$
				break;
			case 9:
				sb.append("\n Windows CE"); //$NON-NLS-1$
				break;
			case 10:
				sb.append("\n Aplicacion EFI (Extensible Firmware Iterface)"); //$NON-NLS-1$
				break;
			case 11:
				sb.append("\n Controlador EFI (Extensible Firmware Iterface) con servicios de arranque"); //$NON-NLS-1$
				break;
			case 12:
				sb.append("\n Controlador EFI (Extensible Firmware Iterface) con servicios de ejecucion"); //$NON-NLS-1$
				break;
			case 13:
				sb.append("\n Imagen ROM EFI (Extensible Firmware Iterface)"); //$NON-NLS-1$
				break;
			case 14:
				sb.append("\n XBOX"); //$NON-NLS-1$
				break;
			default:
				sb.append("\n No valido"); //$NON-NLS-1$
				break;
			}

        	int resourceTablePointerOffset = 112;
        	if (pe32Plus) {
				resourceTablePointerOffset = 128;
			}

        	final int resourceTableOffset = AOBinUtil.getDWord(peFile,coffOffset+optionalHeaderOffset+resourceTablePointerOffset); // 112, 113, 114, 115 (PE32)
        	final int resourceTableSize = AOBinUtil.getDWord(peFile,coffOffset+optionalHeaderOffset+resourceTablePointerOffset+4); // 116, 117, 118, 119 (PE32)

        	sb.append("\n Tamano (en memoria) de la tabla de recursos: " + resourceTableSize + " (" + AOUtil.hexify(new byte[] { peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset+7], peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset+6], peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset+5],peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset+4] }, false) + "h)"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        	sb.append("\n Offset (virtual) de la tabla de recursos: " + resourceTableOffset + " (" + AOUtil.hexify(new byte[] { peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset+3], peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset+2], peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset+1],peFile[coffOffset+optionalHeaderOffset+resourceTablePointerOffset] }, false) + "h)"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        }

    	this.description = sb.toString();
	}

}
