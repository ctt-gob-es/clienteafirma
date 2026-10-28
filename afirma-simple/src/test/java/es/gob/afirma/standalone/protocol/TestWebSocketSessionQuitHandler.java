package es.gob.afirma.standalone.protocol;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.Proxy;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.java_websocket.WebSocket;
import org.junit.Before;
import org.junit.Test;

/** Tests Quit policy against the existing browser connection state. */
public class TestWebSocketSessionQuitHandler {

    private final AtomicReference<String> decision = new AtomicReference<>();
    private Object response;

    @Before
    public void createResponse() throws Exception {
        Class<?> responseClass;
        try {
            responseClass = Class.forName("java.awt.desktop.QuitResponse");
        }
        catch (final ClassNotFoundException e) {
            try {
                responseClass = Class.forName("com.apple.eawt.QuitResponse");
            }
            catch (final ClassNotFoundException unavailable) {
                org.junit.Assume.assumeNoException(unavailable);
                return;
            }
        }
        this.response = Proxy.newProxyInstance(getClass().getClassLoader(), new Class<?>[] { responseClass },
                (proxy, method, args) -> { this.decision.set(method.getName()); return null; });
    }

    @Test
    public void quitBeforeBrowserConnectsIsAllowed() throws Exception {
        assertDecision(new AfirmaWebSocketServer(0, null), "performQuit");
    }

    @Test
    public void connectedBrowserPreventsQuitEvenBetweenRequests() throws Exception {
        final AfirmaWebSocketServer server = new AfirmaWebSocketServer(0, "session");
        server.onOpen(socket(new AtomicBoolean(true)), null);
        assertDecision(server, "cancelQuit");
        assertDecision(server, "cancelQuit");
    }

    @Test
    public void closedPrimaryConnectionDoesNotBlockQuit() throws Exception {
        final AfirmaWebSocketServer server = new AfirmaWebSocketServer(0, "session");
        final AtomicBoolean open = new AtomicBoolean(true);
        server.onOpen(socket(open), null);
        assertDecision(server, "cancelQuit");
        open.set(false);
        assertDecision(server, "performQuit");
    }

    @Test
    public void secondaryConnectionDoesNotReplaceBrowserOwnership() throws Exception {
        final AfirmaWebSocketServer server = new AfirmaWebSocketServer(0, "session");
        server.onOpen(socket(new AtomicBoolean(false)), null);
        server.onOpen(socket(new AtomicBoolean(true)), null);
        assertDecision(server, "performQuit");
    }

    @Test
    public void version4UsesSameQuitPolicy() throws Exception {
        final AfirmaWebSocketServer server = new AfirmaWebSocketServerV4Sup(0, "session",
                es.gob.afirma.core.misc.protocol.ProtocolVersion.getInstance(
                        es.gob.afirma.core.misc.protocol.ProtocolVersion.VERSION_4));
        server.onOpen(socket(new AtomicBoolean(true)), null);
        assertDecision(server, "cancelQuit");
    }

    private void assertDecision(final AfirmaWebSocketServer server, final String expected) throws Exception {
        server.handleQuitRequest(null, this.response);
        assertEquals(expected, this.decision.get());
    }

    private static WebSocket socket(final AtomicBoolean open) {
        return (WebSocket) Proxy.newProxyInstance(TestWebSocketSessionQuitHandler.class.getClassLoader(),
                new Class<?>[] { WebSocket.class }, (proxy, method, args) -> {
                    if ("isOpen".equals(method.getName())) { return open.get(); }
                    throw new AssertionError("Unexpected socket method: " + method.getName());
                });
    }
}
