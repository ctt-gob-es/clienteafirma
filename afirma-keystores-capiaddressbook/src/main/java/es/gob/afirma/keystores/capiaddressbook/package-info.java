/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

/** M&oacute;dulo del Cliente Afirma de gesti&oacute;n de almacenes de certificados "Libreta de Direcciones" y "Autoridades de Certificaci&oacute;n" de Windows (CAPI).<br>
 * Se trata realmente de un proveedor criptogr&aacute;fico JCE que a&ntilde;ade dos nuevos tipos de <code>KeyStore</code> con los nombres <code>Windows-ADDRESSBOOK</code>
 * (Libreta de Direcciones) y <code>Windows-CA</code> (Autoridades de Certificaci&oacute;n).
 * Este puede ser usado como si fuese cualquier otro <code>KeyStore</code> del sistema.
 * <p>
 *  Este m&oacute;dulo no presenta ninguna dependencia externa. Se incluyen clases implementadas en el paquete <code>sun.security.mscapi</code> para
 *  poder realizar llamadas a <i>sunmscapi.dll</i>.
 * </p>
 * <p>
 *  Este m&oacute;dulo es compatible con cualquier entorno JSE 1.6 o superior que incorporen el proveedor de seguridad SunMSCAPI, siempre en entornos MS-Windows.<br>
 * </p>
 * <p>
 *  Desde este m&oacute;dulo no se realizan llamadas a interfaces gr&aacute;ficas.<br>
 * </p>
 */
package es.gob.afirma.keystores.capiaddressbook;