/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 Christopher Tipper
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package es.gob.afirma.standalone.ui;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Created on 06-Jul-2016 10:42:57
 *
 * Abstract: Hooks existing preferences/about/quit functionality from an existing Java app into
 * handlers for the Mac OS X application menu.
 *
 * Uses a Proxy object to dynamically implement the java.awt.desktop.xxxHandler interfaces and register
 * it with the com.apple.eawt.Application object. This allows the complete project to be both built
 * and run on any platform without any stubs or placeholders. Useful for developers looking to
 * implement Mac OS X features while supporting multiple platforms with minimal impact.
 *
 * @version 0.9
 * @author ctipper
 */
public class OSXHandler implements InvocationHandler {

    protected Object targetObject;
    protected Method targetMethod;
    protected String proxySignature;

    static Object macOSXApplication;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /**
     * Each OSXHandler has the name of the EAWT method it intends to listen for (handleAbout, for
     * example), the Object that will ultimately perform the task, and the Method to be called on
     * that Object
     * @param proxySignature Method signature.
     * @param target Target.
     * @param handler Handler.
     */
    protected OSXHandler(final String proxySignature, final Object target, final Method handler) {
        this.proxySignature = proxySignature;
        this.targetObject = target;
        this.targetMethod = handler;
    }

    /**
     * Pass this method an Object and Method equipped to perform application shutdown logic The
     * QuitResponse may be used to respond to a request to quit the application.
     * @param target Objeto que dispone del m&eacute;todo de cierre.
     * @param quitHandler M&eacute;todo que ejecutar para el cierre de la aplicaci&oacute;n.
     */
    public static void setQuitHandler(final Object target, final Method quitHandler) {
        final OSXHandler adapter = new OSXHandler("handleQuitRequestWith", target, quitHandler) { //$NON-NLS-1$

            @Override
			public boolean callTarget(final Object appleEvent, final Object response) {
                if (appleEvent != null) {
                    try {
                        this.targetMethod.invoke(this.targetObject, new Object[] { appleEvent, response });
                    } catch (IllegalAccessException | IllegalArgumentException | SecurityException |
                             InvocationTargetException ex) {
                        LOGGER.severe("Mac OS X Adapter could not talk to EAWT:"); //$NON-NLS-1$
                    }
                }
                return true;
            }
        };
        try {
            final Class<?> applicationClass = Class.forName("com.apple.eawt.Application"); //$NON-NLS-1$
            if (macOSXApplication == null) {
                macOSXApplication = applicationClass.getConstructor((Class[]) null).newInstance((Object[]) null);
            }
            Class<?> quitHandlerClass;
            try {
            	quitHandlerClass = Class.forName("com.apple.eawt.QuitHandler"); //$NON-NLS-1$
            }
            catch (Exception | Error e) {
            	quitHandlerClass = Class.forName("java.awt.desktop.QuitHandler"); //$NON-NLS-1$
			}
            final Method addHandlerMethod = applicationClass.getDeclaredMethod("setQuitHandler", new Class<?>[] { quitHandlerClass }); //$NON-NLS-1$
            // Create a proxy object around this handler that can be reflectively added as an Apple AppEvent handler
            final Object osxAdapterProxy = Proxy.newProxyInstance(OSXHandler.class.getClassLoader(), new Class<?>[] { quitHandlerClass }, adapter);
            addHandlerMethod.invoke(macOSXApplication, new Object[] { osxAdapterProxy });
        } catch (final ClassNotFoundException cnfe) {
            LOGGER.log(Level.SEVERE, "This version of Mac OS X does not support the Apple EAWT. ApplicationEvent handling has been disabled", cnfe); //$NON-NLS-1$
        } catch (NoSuchMethodException | SecurityException | InstantiationException |
                 IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            // Likely a NoSuchMethodException or an IllegalAccessException loading/invoking eawt.Application methods
            LOGGER.log(Level.SEVERE, "Mac OS X Adapter could not talk to EAWT", ex); //$NON-NLS-1$
        }
    }

    /**
     * Pass this method an Object and Method equipped to display application info. They will be
     * called when the About menu item is selected from the application menu
     * @param target Objeto que dispone del m&eacute;todo para mostrar el Acerca de.
     * @param aboutHandler M&eacute;todo que mostrar el di&aacute;logo "Acerca de".
     */
    public static void setAboutHandler(final Object target, final Method aboutHandler) {
        final OSXHandler adapter = new OSXHandler("handleAbout", target, aboutHandler) { //$NON-NLS-1$

            @Override
			public boolean callTarget(final Object appleEvent) {
                if (appleEvent != null) {
                    try {
                        this.targetMethod.invoke(this.targetObject, new Object[] { appleEvent });
                    } catch (IllegalAccessException | IllegalArgumentException | SecurityException |
                             InvocationTargetException ex) {
                        LOGGER.severe("Mac OS X Adapter could not talk to EAWT:"); //$NON-NLS-1$
                    }
                }
                return true;
            }
        };
        try {
            final Class<?> applicationClass = Class.forName("com.apple.eawt.Application"); //$NON-NLS-1$
            if (macOSXApplication == null) {
                macOSXApplication = applicationClass.getConstructor((Class[]) null).newInstance((Object[]) null);
            }
            Class<?> aboutHandlerClass;
            try {
            	aboutHandlerClass = Class.forName("com.apple.eawt.AboutHandler"); //$NON-NLS-1$
            }
            catch (Exception | Error e) {
            	aboutHandlerClass = Class.forName("java.awt.desktop.AboutHandler"); //$NON-NLS-1$
			}
            final Method addHandlerMethod = applicationClass.getDeclaredMethod("setAboutHandler", new Class<?>[] { aboutHandlerClass }); //$NON-NLS-1$
            // Create a proxy object around this handler that can be reflectively added as an Apple AppEvent handler
            final Object osxAdapterProxy = Proxy.newProxyInstance(OSXHandler.class.getClassLoader(), new Class<?>[] { aboutHandlerClass }, adapter);
            addHandlerMethod.invoke(macOSXApplication, new Object[] { osxAdapterProxy });
        } catch (final ClassNotFoundException cnfe) {
            LOGGER.log(Level.SEVERE, "This version of Mac OS X does not support the Apple EAWT. ApplicationEvent handling has been disabled", cnfe); //$NON-NLS-1$
        } catch (NoSuchMethodException | SecurityException | InstantiationException |
                 IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            // Likely a NoSuchMethodException or an IllegalAccessException loading/invoking eawt.Application methods
            LOGGER.log(Level.SEVERE, "Mac OS X Adapter could not talk to EAWT", ex); //$NON-NLS-1$
        }
    }

    /**
     * Pass this method an Object and a Method equipped to display application options. They will be
     * called when the Preferences menu item is selected from the application menu
     * @param target Objeto que dispone del m&eacute;todo para mostrar el di&aacute;logo de preferencias.
     * @param prefsHandler M&eacute;todo que ejecutar para mostrar el di&aacute;logo de preferencias.
     */
    public static void setPreferencesHandler(final Object target, final Method prefsHandler) {
        final OSXHandler adapter = new OSXHandler("handlePreferences", target, prefsHandler) { //$NON-NLS-1$

            @Override
			public boolean callTarget(final Object appleEvent) {
                if (appleEvent != null) {
                    try {
                        this.targetMethod.invoke(this.targetObject, new Object[] { appleEvent });
                    } catch (IllegalAccessException | IllegalArgumentException | SecurityException |
                             InvocationTargetException ex) {
                        LOGGER.severe("Mac OS X Adapter could not talk to EAWT:"); //$NON-NLS-1$
                    }
                }
                return true;
            }
        };
        try {
            final Class<?> applicationClass = Class.forName("com.apple.eawt.Application"); //$NON-NLS-1$
            if (macOSXApplication == null) {
                macOSXApplication = applicationClass.getConstructor((Class[]) null).newInstance((Object[]) null);
            }
            Class<?> prefsHandlerClass;
            try {
            	prefsHandlerClass = Class.forName("com.apple.eawt.PreferencesHandler"); //$NON-NLS-1$
            }
            catch (Exception | Error e) {
            	prefsHandlerClass = Class.forName("java.awt.desktop.PreferencesHandler"); //$NON-NLS-1$
			}
            final Method addHandlerMethod = applicationClass.getDeclaredMethod("setPreferencesHandler", new Class<?>[] { prefsHandlerClass }); //$NON-NLS-1$
            // Create a proxy object around this handler that can be reflectively added as an Apple AppEvent handler
            final Object osxAdapterProxy = Proxy.newProxyInstance(OSXHandler.class.getClassLoader(), new Class<?>[] { prefsHandlerClass }, adapter);
            addHandlerMethod.invoke(macOSXApplication, new Object[] { osxAdapterProxy });
        } catch (final ClassNotFoundException cnfe) {
            LOGGER.log(Level.SEVERE, "This version of Mac OS X does not support the Apple EAWT. ApplicationEvent handling has been disabled", cnfe); //$NON-NLS-1$
        } catch (NoSuchMethodException | SecurityException | InstantiationException |
                 IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            // Likely a NoSuchMethodException or an IllegalAccessException loading/invoking eawt.Application methods
            LOGGER.log(Level.SEVERE, "Mac OS X Adapter could not talk to EAWT", ex); //$NON-NLS-1$
        }
    }

    /**
     * InvocationHandler implementation This is the entry point for our proxy object; it is called
     * every time an AppEvent method is invoked
     */
    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
        // All of the AppEvent methods are void; return null regardless of what happens
        if (isCorrectMethod(method, args)) {
            if (args.length == 1) {
                callTarget(args[0]);
            } else {
                callTarget(args[0], args[1]);
            }
        }
        return null;
    }

    /**
     * Override this method to perform any operations on the event that comes with the various
     * callbacks See setFileHandler above for an example
     * @param appleEvent Evento que desencadena la llamada.
     * @return Indica si la operaci&oacute;n finaliz&oacute; correctamente o no.
     * @throws IllegalAccessException Se intenta ejecutar un metodo no accesible.
     * @throws InvocationTargetException Error de ejecuci&oacute;n intento de del m&eacute;todo indicado.
     */
    public boolean callTarget(final Object appleEvent) throws InvocationTargetException, IllegalAccessException {
        final Object result = this.targetMethod.invoke(this.targetObject, (Object[]) null);
        if (result == null) {
            return true;
        }
        return Boolean.parseBoolean(result.toString());
    }

    /**
     * Override this method to perform any operations on the event that comes with the various
     * callbacks See setQuitHandler above for an example.
     * @param appleEvent Evento que desencadena la llamada.
     * @param response Respuesta de la llamada.
     * @return Indica si la operaci&oacute;n finaliz&oacute; correctamente o no.
     * @throws IllegalAccessException Se intenta ejecutar un metodo no accesible.
     * @throws InvocationTargetException Error de ejecuci&oacute;n intento de del m&eacute;todo indicado.
     */
    public boolean callTarget(final Object appleEvent, final Object response) throws InvocationTargetException, IllegalAccessException {
        final Object result = this.targetMethod.invoke(this.targetObject, (Object[]) null);
        if (result == null) {
            return true;
        }
        return Boolean.parseBoolean(result.toString());
    }

    /**
     * Compare the method that was called to the intended method when the OSXHandler instance was
     * created (e.g. handleAbout, handleQuitRequestWith, openFiles etc.)
     * @param method Method.
     * @param args Arguments.
     * @return {@code true} if it's correct, {@code false} otherwise.
     */
    protected boolean isCorrectMethod(final Method method, final Object[] args) {
        return this.targetMethod != null && this.proxySignature.equals(method.getName()) && args.length > 0;
    }

}
