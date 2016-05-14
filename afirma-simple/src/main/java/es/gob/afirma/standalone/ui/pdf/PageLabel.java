package es.gob.afirma.standalone.ui.pdf;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Area;
import java.util.EventListener;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;

final class PageLabel extends JLabel {

	static interface PageLabelListener extends EventListener {
		void selectionAvailable(Properties p);
		void setX(String x);
		void setY(String y);
	}

	private static final long serialVersionUID = 4917110251831788580L;

	private static boolean TRANSLUCENCY_CAPABLE;
	static {
		final GraphicsConfiguration config = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
		try {
			TRANSLUCENCY_CAPABLE = com.sun.awt.AWTUtilities.isTranslucencyCapable(config);
		}
		catch(final Exception | Error e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No ha sido posible determinar si el sistema sorporta superficies translucidas, se asume que si: " + e //$NON-NLS-1$
			);
			TRANSLUCENCY_CAPABLE = true;
		}
	}

	private static final int TRANSPARENCY_LEVEL = 3;

	private final PageLabelListener listener;
	PageLabelListener getPageLabelListener() {
		return this.listener;
	}

	private final float scale;

	private Properties toPdfPosition(final Rectangle original) {

		final int height = original.height + original.y > getHeight() ? getHeight() - original.y : original.height;
		final int width = original.width + original.x > getWidth() ? getWidth() - original.x : original.width;

		final Properties extraParams = new Properties();
		extraParams.put(
			"signaturePositionOnPageLowerLeftX", //$NON-NLS-1$
			Integer.toString(
				Math.round(
					//original.x * (1/this.scale)
						original.x * this.scale
				)
			)
		);
		extraParams.put(
			"signaturePositionOnPageLowerLeftY",  //$NON-NLS-1$
			Integer.toString(
				Math.round(
					//(getHeight() - original.y - height) * (1/this.scale)
						(getHeight() - original.y - height) * this.scale
				)
			)
		);
		extraParams.put(
			"signaturePositionOnPageUpperRightX", //$NON-NLS-1$
			Integer.toString(
				Math.round(
						(original.x + width) * this.scale
				)
			)
		);
		extraParams.put(
			"signaturePositionOnPageUpperRightY",  //$NON-NLS-1$
			Integer.toString(
				Math.round(
						(getHeight() - original.y) * this.scale
				)
			)
		);
		return extraParams;
	}

    private Rectangle selectionBounds = null;
    void setSelectionBounds(final Rectangle r) {
    	this.selectionBounds = r;
    	this.listener.selectionAvailable(r != null ? toPdfPosition(r) : null);
    }
    Rectangle getSelectionBounds() {
    	return this.selectionBounds = null;
    }

    private Point clickPoint;
    Point getClickPoint() {
    	return this.clickPoint;
    }
    void setClickPoint(final Point p) {
    	this.clickPoint = p;
    }

	PageLabel(final Image image,
			  final int width,
			  final int height,
			  final PageLabelListener pll,
			  final float scaleFactor) {

		super(new ImageIcon(image));

		if (pll == null) {
			throw new IllegalArgumentException();
		}
		this.listener = pll;

		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
			"Factor de escala de las paginas: " + scaleFactor //$NON-NLS-1$
		);
		this.scale = scaleFactor;

		setSize(width, height);
		setCursor(new java.awt.Cursor(java.awt.Cursor.CROSSHAIR_CURSOR));

		final MouseAdapter ma = new MouseAdapter() {

            @Override
            public void mouseClicked(final MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount() == 2) {
                    System.exit(0);
                }
            }

            @Override
            public void mousePressed(final MouseEvent e) {
            	setClickPoint(e.getPoint());
                setSelectionBounds(null);
                repaint();
            }

            @Override
            public void mouseReleased(final MouseEvent e) {
            	setClickPoint(null);
            }

            @Override
            public void mouseDragged(final MouseEvent e) {
            	if (getClickPoint() != null) {
	                final Point dragPoint = e.getPoint();
	                final int x = Math.min(getClickPoint().x, dragPoint.x);
	                final int y = Math.min(getClickPoint().y, dragPoint.y);
	                final int rWidth = Math.max(getClickPoint().x - dragPoint.x, dragPoint.x - getClickPoint().x);
	                final int rHeight = Math.max(getClickPoint().y - dragPoint.y, dragPoint.y - getClickPoint().y);
	                setSelectionBounds(new Rectangle(x, y, rWidth, rHeight));
	                repaint();
            	}
            	setPositionValues(e.getX(), e.getY());
            }

            @Override
			public void mouseMoved(final MouseEvent e) {
            	setPositionValues(e.getX(), e.getY());
            }

            @Override
			public void mouseExited(final MouseEvent e) {
            	getPageLabelListener().setX(""); //$NON-NLS-1$
            	getPageLabelListener().setY(""); //$NON-NLS-1$
            }
		};

		addMouseListener(ma);
		addMouseMotionListener(ma);
	}

	void setPositionValues(final int x, final int y) {
    	getPageLabelListener().setX(
			Integer.toString(
				Math.round(
					(x < getWidth() ? x : getWidth()) / this.scale
				)
			)
		);
    	getPageLabelListener().setY(
			Integer.toString(
				Math.round(
					(y < getHeight() ? y : getHeight()) / this.scale
				)
			)
		);
	}

	@Override
	public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        final Graphics2D g2d = (Graphics2D) g.create();
        g2d.setColor(new Color(255, 255, 255, 128));

        final Area fill = new Area(new Rectangle(new Point(0, 0), getSize()));
        if (this.selectionBounds != null) {
            fill.subtract(new Area(this.selectionBounds));
        }
        g2d.fill(fill);
        if (this.selectionBounds != null) {
	        g2d.setColor(Color.BLUE);
	        if (TRANSLUCENCY_CAPABLE) {
		        g2d.setComposite(
		    		AlphaComposite.getInstance(
						AlphaComposite.SRC_OVER,TRANSPARENCY_LEVEL * 0.1f
					)
				);
	        }
            g2d.fill(this.selectionBounds);
        }
        g2d.dispose();
	}

}
