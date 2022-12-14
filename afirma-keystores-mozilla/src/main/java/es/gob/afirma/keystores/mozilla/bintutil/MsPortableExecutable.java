/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla.bintutil;

import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** Analizador de ficheros MS-PE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s Capote. */
public final class MsPortableExecutable {

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
        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"El puntero a la informacion de depuracion del COFF no es cero (se implementa una caracteristica deprecada)" //$NON-NLS-1$
			);
        }

        if (AOBinUtil.getInt(peFile, coffOffset+12) != 0) { // +12, +13, +14, +15
        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"El numero de entradas de la tabla de simbolos no es cero (se implementa una caracteristica deprecada)" //$NON-NLS-1$
			);
        }

        final int sizeOfOptionalHeader = AOBinUtil.getU2(peFile, coffOffset+16); // +16, +17

        if (sizeOfOptionalHeader == 0) {
			sb.append(
				"\n El fichero es un objeto (no tiene cabecera opcional)" //$NON-NLS-1$
			);
		}
        else {
			sb.append(
				"\n El fichero es ejecutable (tamano de la cabecera opcional: " + sizeOfOptionalHeader + ")" //$NON-NLS-1$ //$NON-NLS-2$
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
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Numeros de linea de la imagen COFF eliminados (se implementa una caracteristica deprecada" //$NON-NLS-1$
			);
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x08)) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Entradas de la tabla de simbolos de la imagen COFF para simbolos locales eliminadas (se implementa una caracteristica deprecada)" //$NON-NLS-1$
			);
		}
        if (0 != (byte) (characteristics[1] & (byte) 0x10)) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Conjunto truncando agresivamente (se implementa una caracteristica deprecada para Windows 2000 y posteriores)" //$NON-NLS-1$
			);
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
        if (0 != (byte) (characteristics[0] & (byte) 0x80)) {
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
					Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"El fichero de imagen es de un tipo desconocido (Magic: " + AOUtil.hexify(new byte[] {peFile[coffOffset+21], peFile[coffOffset+20]}, true) + ")" //$NON-NLS-1$ //$NON-NLS-2$
					);
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

        	// Version de la imagen +44, +45, +46, +47
        	sb.append("\n Version de la imagen: " + //$NON-NLS-1$
        		AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+44) +
	        	"." + //$NON-NLS-1$
	        	AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+46)
        	);

        	// Version del subsistema +48, +49, +50, +51
        	sb.append("\n Version del subsistema: " + //$NON-NLS-1$
        		AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+48) +
	        	"." + //$NON-NLS-1$
	        	AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+50)
        	);

        	// Tamano de la imagen en memoria +56, +57, +58, +59
        	sb.append("\n Tamano de la imagen en memoria: " + //$NON-NLS-1$
        		AOUtil.hexify(new byte[] {
    				peFile[coffOffset+optionalHeaderOffset+56],
    				peFile[coffOffset+optionalHeaderOffset+57],
    				peFile[coffOffset+optionalHeaderOffset+58],
    				peFile[coffOffset+optionalHeaderOffset+59]
        		}, false)
        	);
        	sb.append('h');

        	// Tamano de las cabeceras +60, +61, +62, +63
        	sb.append("\n Tamano de las cabeceras (DOS Stub, cabecera PE y cabeceras de seccion, en valor redondeado a un multiplo de 'FileAlignment'): " + //$NON-NLS-1$
        		AOUtil.hexify(new byte[] {
    				peFile[coffOffset+optionalHeaderOffset+60],
    				peFile[coffOffset+optionalHeaderOffset+61],
    				peFile[coffOffset+optionalHeaderOffset+62],
    				peFile[coffOffset+optionalHeaderOffset+63]
        		}, false)
        	);
        	sb.append('h');

        	// Checksum +64 +65 +66 +67
        	sb.append("\n Checksum de la imagen: " + //$NON-NLS-1$
        		AOUtil.hexify(new byte[] {
    				peFile[coffOffset+optionalHeaderOffset+64],
    				peFile[coffOffset+optionalHeaderOffset+65],
    				peFile[coffOffset+optionalHeaderOffset+66],
    				peFile[coffOffset+optionalHeaderOffset+67]
        		}, false)
        	);

        	// Subsistema Windows - WINDOWS SPECIFIC +68
        	sb.append("\n Tipo de subsistema Windows: "); //$NON-NLS-1$
        	switch (AOBinUtil.getU2(peFile, coffOffset+optionalHeaderOffset+68)) {
			case 0:
				sb.append(" Desconocido"); //$NON-NLS-1$
				break;
			case 1:
				sb.append(" Controlador de dispositiovo o proceso nativo Windows"); //$NON-NLS-1$
				break;
			case 2:
				sb.append(" Windows en modo grafico (GUI)"); //$NON-NLS-1$
				break;
			case 3:
				sb.append(" Windows en modo texto"); //$NON-NLS-1$
				break;
			case 7:
				sb.append(" POSIX en modo texto"); //$NON-NLS-1$
				break;
			case 9:
				sb.append(" Windows CE"); //$NON-NLS-1$
				break;
			case 10:
				sb.append(" Aplicacion EFI (Extensible Firmware Interface)"); //$NON-NLS-1$
				break;
			case 11:
				sb.append(" Controlador EFI (Extensible Firmware Interface) con servicios de arranque"); //$NON-NLS-1$
				break;
			case 12:
				sb.append(" Controlador EFI (Extensible Firmware Interface) con servicios de ejecucion"); //$NON-NLS-1$
				break;
			case 13:
				sb.append(" Imagen ROM EFI (Extensible Firmware Interface)"); //$NON-NLS-1$
				break;
			case 14:
				sb.append(" XBOX"); //$NON-NLS-1$
				break;
			default:
				sb.append(" No valido"); //$NON-NLS-1$
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
