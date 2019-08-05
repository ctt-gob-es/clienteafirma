/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Transparency;
import java.awt.image.BufferedImage;

import javax.swing.JPanel;

/** Panel que redimensiona su imagen de fondo (manteniendo la relaci&oacute;n de aspecto) cuando
 * este es redimensionado. */
final class ScalablePane extends JPanel {

    /** Serial Id. */
	private static final long serialVersionUID = 2907355327204790682L;

	private final Image master;
    private boolean toFit;
    private Image scaled;

    ScalablePane(final Image master) {
        this(master, true);
    }

    ScalablePane(final Image master, final boolean toFit) {
        this.master = master;
        setToFit(toFit);
    }

    @Override
    public Dimension getPreferredSize() {
        return this.master == null ? super.getPreferredSize() : new Dimension(this.master.getWidth(this), this.master.getHeight(this));
    }

    @Override
    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        Image toDraw = null;
        if (this.scaled != null) {
            toDraw = this.scaled;
        } else if (this.master != null) {
            toDraw = this.master;
        }

        if (toDraw != null) {
            final int x = (getWidth() - toDraw.getWidth(this)) / 2;
            final int y = (getHeight() - toDraw.getHeight(this)) / 2;
            g.drawImage(toDraw, x, y, this);
        }
    }

    @Override
    public void invalidate() {
        generateScaledInstance();
        super.invalidate();
    }

    public boolean isToFit() {
        return this.toFit;
    }

    public void setToFit(final boolean value) {
        if (value != this.toFit) {
            this.toFit = value;
            invalidate();
        }
    }

    protected void generateScaledInstance() {
        this.scaled = null;
        final Dimension size = getSize();
        if (size == null || (int) size.getWidth() == 0 || (int) size.getHeight() == 0) {
        	this.scaled = this.master;
        }
        else {
        	if (isToFit()) {
        		this.scaled = getScaledInstanceToFit(this.master, getSize());
        	} else {
        		this.scaled = getScaledInstanceToFill(this.master, getSize());
        	}
        }
    }

    protected BufferedImage toBufferedImage(final Image masterImage) {
        final Dimension masterSize = new Dimension(masterImage.getWidth(this), masterImage.getHeight(this));
        final BufferedImage image = createCompatibleImage(masterSize);
        final Graphics2D g2d = image.createGraphics();
        g2d.drawImage(masterImage, 0, 0, this);
        g2d.dispose();
        return image;
    }

    Image getScaledInstanceToFit(final Image masterImage, final Dimension size) {
        final Dimension masterSize = new Dimension(masterImage.getWidth(this), masterImage.getHeight(this));
        return getScaledInstance(
                        toBufferedImage(masterImage),
                        getScaleFactorToFit(masterSize, size),
                        RenderingHints.VALUE_INTERPOLATION_BILINEAR,
                        true);
    }

    Image getScaledInstanceToFill(final Image masterImage, final Dimension size) {
        final Dimension masterSize = new Dimension(masterImage.getWidth(this), masterImage.getHeight(this));
        return getScaledInstance(
                        toBufferedImage(masterImage),
                        getScaleFactorToFill(masterSize, size),
                        RenderingHints.VALUE_INTERPOLATION_BILINEAR,
                        true);
    }

    public static Dimension getSizeToFit(final Dimension original, final Dimension dim) {
        final double factor = getScaleFactorToFit(original, dim);
        final Dimension size = new Dimension(original);
        size.width *= factor;
        size.height *= factor;
        return size;
    }

    public static Dimension getSizeToFill(final Dimension original, final Dimension dim) {
        final double factor = getScaleFactorToFill(original, dim);
        final Dimension size = new Dimension(original);
        size.width *= factor;
        size.height *= factor;
        return size;
    }

    private static double getScaleFactor(final int iMasterSize, final int iTargetSize) {
        return (double) iTargetSize / (double) iMasterSize;
    }

    private static double getScaleFactorToFit(final Dimension original, final Dimension toFit) {
        double dScale = 1d;
        if (original != null && toFit != null) {
            final double dScaleWidth = getScaleFactor(original.width, toFit.width);
            final double dScaleHeight = getScaleFactor(original.height, toFit.height);
            dScale = Math.min(dScaleHeight, dScaleWidth);
        }
        return dScale;
    }

    private static double getScaleFactorToFill(final Dimension masterSize, final Dimension targetSize) {
        final double dScaleWidth = getScaleFactor(masterSize.width, targetSize.width);
        final double dScaleHeight = getScaleFactor(masterSize.height, targetSize.height);
        return Math.max(dScaleHeight, dScaleWidth);
    }

    private BufferedImage createCompatibleImage(final Dimension size) {
        return createCompatibleImage(size.width, size.height);
    }

    private BufferedImage createCompatibleImage(final int width, final int height) {
        GraphicsConfiguration gc = getGraphicsConfiguration();
        if (gc == null) {
            gc = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
        }

        final BufferedImage image = gc.createCompatibleImage(width, height, Transparency.TRANSLUCENT);
        image.coerceData(true);
        return image;
    }

    protected static BufferedImage getScaledInstance(final BufferedImage img, final double dScaleFactor, final Object hint, final boolean bHighQuality) {
        BufferedImage imgScale = img;
        final int iImageWidth = (int) Math.round(img.getWidth() * dScaleFactor);
        final int iImageHeight = (int) Math.round(img.getHeight() * dScaleFactor);

        if (dScaleFactor <= 1.0d) {
            imgScale = getScaledDownInstance(img, iImageWidth, iImageHeight, hint, bHighQuality);
        } else {
            imgScale = getScaledUpInstance(img, iImageWidth, iImageHeight, hint, bHighQuality);
        }

        return imgScale;
    }

    protected static BufferedImage getScaledDownInstance(final BufferedImage img,
                    final int targetWidth,
                    final int targetHeight,
                    final Object hint,
                    final boolean higherQuality) {

        final int type = img.getTransparency() == Transparency.OPAQUE
                        ? BufferedImage.TYPE_INT_RGB : BufferedImage.TYPE_INT_ARGB;

        BufferedImage ret = img;

        if (targetHeight > 0 || targetWidth > 0) {
            int w, h;
            if (higherQuality) {
                // Use multi-step technique: start with original size, then
                // scale down in multiple passes with drawImage()
                // until the target size is reached
                w = img.getWidth();
                h = img.getHeight();
            } else {
                // Use one-step technique: scale directly from original
                // size to target size with a single drawImage() call
                w = targetWidth;
                h = targetHeight;
            }

            do {
                if (higherQuality && w > targetWidth) {
                    w /= 2;
                    if (w < targetWidth) {
                        w = targetWidth;
                    }
                }
                if (higherQuality && h > targetHeight) {
                    h /= 2;
                    if (h < targetHeight) {
                        h = targetHeight;
                    }
                }

                final BufferedImage tmp = new BufferedImage(Math.max(w, 1), Math.max(h, 1), type);
                final Graphics2D g2 = tmp.createGraphics();
                g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, hint);
                g2.drawImage(ret, 0, 0, w, h, null);
                g2.dispose();

                ret = tmp;
            } while (w != targetWidth || h != targetHeight);
        } else {
            ret = new BufferedImage(1, 1, type);
        }

        return ret;
    }

    protected static BufferedImage getScaledUpInstance(final BufferedImage img,
                    final int targetWidth,
                    final int targetHeight,
                    final Object hint,
                    final boolean higherQuality) {

        final int type = BufferedImage.TYPE_INT_ARGB;

        BufferedImage ret = img;
        int w, h;
        if (higherQuality) {
            // Use multi-step technique: start with original size, then
            // scale down in multiple passes with drawImage()
            // until the target size is reached
            w = img.getWidth();
            h = img.getHeight();
        } else {
            // Use one-step technique: scale directly from original
            // size to target size with a single drawImage() call
            w = targetWidth;
            h = targetHeight;
        }

        do {
            if (higherQuality && w < targetWidth) {
                w *= 2;
                if (w > targetWidth) {
                    w = targetWidth;
                }
            }

            if (higherQuality && h < targetHeight) {
                h *= 2;
                if (h > targetHeight) {
                    h = targetHeight;
                }
            }

            BufferedImage tmp = new BufferedImage(w, h, type);
            final Graphics2D g2 = tmp.createGraphics();
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, hint);
            g2.drawImage(ret, 0, 0, w, h, null);
            g2.dispose();

            ret = tmp;
            tmp = null;
        } while (w != targetWidth || h != targetHeight);
        return ret;
    }

    public Image getMaster() {
    	return this.master;
    }
}