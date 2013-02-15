/*
 * $Id: CMYKColor.java 3427 2008-05-24 18:32:31Z xlv $
 *
 * Copyright 2001, 2002 by Paulo Soares.
 *
 * The contents of this file are subject to the Mozilla Public License Version 1.1
 * (the "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the License.
 *
 * The Original Code is 'iText, a free JAVA-PDF library'.
 *
 * The Initial Developer of the Original Code is Bruno Lowagie. Portions created by
 * the Initial Developer are Copyright (C) 1999, 2000, 2001, 2002 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000, 2001, 2002 by Paulo Soares. All Rights Reserved.
 *
 * Contributor(s): all the names of the contributors are added in the source code
 * where applicable.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * LGPL license (the "GNU LIBRARY GENERAL PUBLIC LICENSE"), in which case the
 * provisions of LGPL are applicable instead of those above.  If you wish to
 * allow use of your version of this file only under the terms of the LGPL
 * License and not to allow others to use your version of this file under
 * the MPL, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the LGPL.
 * If you do not delete the provisions above, a recipient may use your version
 * of this file under either the MPL or the GNU LIBRARY GENERAL PUBLIC LICENSE.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MPL as stated above or under the terms of the GNU
 * Library General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library general Public License for more
 * details.
 *
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */

package com.lowagie.text.pdf;

/**
 *
 * @author  Paulo Soares (psoares@consiste.pt)
 */
class CMYKColor extends ExtendedColor {

    private static final long serialVersionUID = 5940378778276468452L;
	private final float cyan;
    private final float magenta;
    private final float yellow;
    private final float black;



    /**
     * Construct a CMYK Color.
     * @param floatCyan
     * @param floatMagenta
     * @param floatYellow
     * @param floatBlack
     */
    CMYKColor(final float floatCyan, final float floatMagenta, final float floatYellow, final float floatBlack) {
        super(TYPE_CMYK, 1f - floatCyan - floatBlack, 1f - floatMagenta - floatBlack, 1f - floatYellow - floatBlack);
        this.cyan = normalize(floatCyan);
        this.magenta = normalize(floatMagenta);
        this.yellow = normalize(floatYellow);
        this.black = normalize(floatBlack);
    }

    /**
     * @return the cyan value
     */
    public float getCyan() {
        return this.cyan;
    }

    /**
     * @return the magenta value
     */
    public float getMagenta() {
        return this.magenta;
    }

    /**
     * @return the yellow value
     */
    public float getYellow() {
        return this.yellow;
    }

    /**
     * @return the black value
     */
    public float getBlack() {
        return this.black;
    }

    @Override
	public boolean equals(final Object obj) {
        if (!(obj instanceof CMYKColor)) {
			return false;
		}
        final CMYKColor c2 = (CMYKColor)obj;
        return this.cyan == c2.cyan && this.magenta == c2.magenta && this.yellow == c2.yellow && this.black == c2.black;
    }

    @Override
	public int hashCode() {
        return Float.floatToIntBits(this.cyan) ^ Float.floatToIntBits(this.magenta) ^ Float.floatToIntBits(this.yellow) ^ Float.floatToIntBits(this.black);
    }

}
