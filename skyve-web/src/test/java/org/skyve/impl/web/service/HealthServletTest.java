package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

class HealthServletTest {
    private boolean originalHealthCheck = UtilImpl.HEALTH_CHECK;
    private int originalHealthCacheSeconds = UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS;
    private AtomicReference<StringBuilder> originalCachedResponse = getCachedResponseField();
    private AtomicLong originalResponseInstant = getResponseInstantField();

    @AfterEach
    void tearDown() {
        UtilImpl.HEALTH_CHECK = originalHealthCheck;
        UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS = originalHealthCacheSeconds;
        setCachedResponseField(originalCachedResponse);
        setResponseInstantField(originalResponseInstant);
    }

    @Test
    @SuppressWarnings("static-method")
    void determineResponseContainsExpectedHealthKeys() throws Exception {
        Method determineResponse = HealthServlet.class.getDeclaredMethod("determineResponse");
        determineResponse.setAccessible(true);

        String json = determineResponse.invoke(null).toString();

        assertTrue(json.startsWith("{"));
        assertTrue(json.endsWith("}"));
        assertTrue(json.contains("\"persistence\":"));
        assertTrue(json.contains("\"datastores\":"));
        assertTrue(json.contains("\"repository\":"));
        assertTrue(json.contains("\"addins\":"));
        assertTrue(json.contains("\"content\":"));
        assertTrue(json.contains("\"jobs\":"));
        assertTrue(json.contains("\"caching\":"));
        assertTrue(json.contains("\"percentageSystemLoad\":"));
    }

    @Test
    @SuppressWarnings("static-method")
    void doGetReturns404WhenHealthCheckDisabled() throws Exception {
        UtilImpl.HEALTH_CHECK = false;

        HealthServlet servlet = new HealthServlet();
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        servlet.doGet(request, response);

        verify(response).sendError(HttpServletResponse.SC_NOT_FOUND);
    }

    @Test
    @SuppressWarnings({"static-method", "resource"})
    void doGetUsesCachedPayloadWhenFresh() throws Exception {
        UtilImpl.HEALTH_CHECK = true;
        UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS = 60;
        setCachedResponseField(new AtomicReference<>(new StringBuilder("{\"cached\":true}")));
        setResponseInstantField(new AtomicLong(System.currentTimeMillis()));

        HealthServlet servlet = new HealthServlet();
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);
        StringWriter body = new StringWriter();
        PrintWriter writer = new PrintWriter(body);
        when(response.getWriter()).thenReturn(writer);

        try {
            servlet.doGet(request, response);
        }
        finally {
            writer.close();
        }

        assertTrue(body.toString().contains("\"cached\":true"));
        verify(response).setContentType("application/json");
        verify(response).setCharacterEncoding("UTF-8");
        verify(response).addDateHeader("Expires", 0);
        verify(response).setStatus(HttpServletResponse.SC_OK);
    }

    @Test
    @SuppressWarnings({"static-method", "resource"})
    void doGetRecomputesPayloadWhenCacheIsStale() throws Exception {
        UtilImpl.HEALTH_CHECK = true;
        UtilImpl.HEALTH_CACHE_TIME_IN_SECONDS = 60;
        setCachedResponseField(new AtomicReference<>(new StringBuilder("{\"old\":true}")));
        setResponseInstantField(new AtomicLong(Long.MIN_VALUE));

        HealthServlet servlet = new HealthServlet();
        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);
        StringWriter body = new StringWriter();
        PrintWriter writer = new PrintWriter(body);
        when(response.getWriter()).thenReturn(writer);

        try {
            servlet.doGet(request, response);
        }
        finally {
            writer.close();
        }

        String json = body.toString();
        assertTrue(json.contains("\"persistence\":"));
        verify(response).setStatus(HttpServletResponse.SC_OK);
        verify(response).addDateHeader(eq("Expires"), anyLong());
    }

    private static AtomicReference<StringBuilder> getCachedResponseField() {
        try {
            Field field = HealthServlet.class.getDeclaredField("cachedResponse");
            field.setAccessible(true);
            @SuppressWarnings("unchecked")
            AtomicReference<StringBuilder> value = (AtomicReference<StringBuilder>) field.get(null);
            return value;
        }
        catch (ReflectiveOperationException e) {
            throw new RuntimeException("Unable to read cachedResponse field", e);
        }
    }

    private static AtomicLong getResponseInstantField() {
        try {
            Field field = HealthServlet.class.getDeclaredField("responseInstant");
            field.setAccessible(true);
            return (AtomicLong) field.get(null);
        }
        catch (ReflectiveOperationException e) {
            throw new RuntimeException("Unable to read responseInstant field", e);
        }
    }

    private static void setCachedResponseField(AtomicReference<StringBuilder> value) {
        try {
            Field field = HealthServlet.class.getDeclaredField("cachedResponse");
            field.setAccessible(true);
            field.set(null, value);
        }
        catch (ReflectiveOperationException e) {
            throw new RuntimeException("Unable to set cachedResponse field", e);
        }
    }

    private static void setResponseInstantField(AtomicLong value) {
        try {
            Field field = HealthServlet.class.getDeclaredField("responseInstant");
            field.setAccessible(true);
            field.set(null, value);
        }
        catch (ReflectiveOperationException e) {
            throw new RuntimeException("Unable to set responseInstant field", e);
        }
    }
}