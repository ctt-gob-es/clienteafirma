/*
 * Copyright (c) 2000 - 2011 The Legion Of The Bouncy Castle (http://www.bouncycastle.org)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package es.gob.jmulticard.jse.provider.digest;

/** Interfaz para la clases generadoras de huellas digitales.
 * C&oacute;digo fuente proveniente de
 * <i>The Legion Of The Bouncy Castle (<a href="http://www.bouncycastle.org">http://www.bouncycastle.org</a>)</i> */
public interface Digest {

    /** Obtiene el tama&ntilde;o (en octetos) de la huella digital producida.
     * @return Tama&ntilde;o (en octetos) de la huella digital producida */
    int getDigestSize();

    /** A&ntilde;ade un octeto a los datos sobre los que calcular la huella digital.
     * @param in Octeto a a&ntilde;adir a los datos sobre los que calcular la huella digital */
    void update(byte in);

    /** A&ntilde;ade un conjunto de octetos a los datos sobre los que calcular la huella digital.
     * @param in Array de octetos a a&ntilde;adir a los datos sobre los que calcular la huella digital
     * @param inOff Desplazamiento desde el que empiezan los datos a a&ntilde;adir dentro del array
     * @param len N&uacute;mero de octetos a a&ntilde;adir  */
    void update(byte[] in, int inOff, int len);

    /** Calcula la huella digital final y borra los datos sobre los que se ha calculado.
     * @param out Array de octetos sobre el que copiar la huella digital
     * @param outOff Desplazamiento sobre el que empezar a copiar la huella digital en el array
     * @return Valor final de la huella digital */
    int doFinal(byte[] out, int outOff);

    /** Borra los datos actualmente establecidos sobre los que calcular la huella digital. */
    void reset();
}
