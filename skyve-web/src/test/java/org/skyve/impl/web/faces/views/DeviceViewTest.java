package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class DeviceViewTest {
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	@AfterEach
	void clearFacesContext() {
		FacesContextBridge.setCurrent(null);
	}

	@Test
	void postConstructBuildsStartingAndClickUrls() throws Exception {
		DeviceView view = new DeviceView();
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		Map<String, String> params = new LinkedHashMap<>();
		params.put("m", "admin");
		params.put("d", "User");
		params.put("ua", "tablet");

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(params);
		FacesContextBridge.setCurrent(context);

		Method postConstruct = DeviceView.class.getDeclaredMethod("postConstruct");
		postConstruct.setAccessible(true);
		postConstruct.invoke(view);

		assertTrue(view.getClickDeviceJspUrl().contains("/device.jsp?"));
		assertTrue(view.getClickDeviceJspUrl().contains("m=admin"));
		assertTrue(view.getClickDeviceJspUrl().contains("d=User"));
		assertTrue(view.getClickDeviceJspUrl().endsWith("ua="));
		assertTrue(view.getStartingDeviceJspUrl().endsWith("ua=tablet"));
	}

	@Test
	void postConstructDefaultsUaToPhoneWhenMissing() throws Exception {
		DeviceView view = new DeviceView();
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		Map<String, String> params = new LinkedHashMap<>();
		params.put("m", "admin");

		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterMap()).thenReturn(params);
		FacesContextBridge.setCurrent(context);

		Method postConstruct = DeviceView.class.getDeclaredMethod("postConstruct");
		postConstruct.setAccessible(true);
		postConstruct.invoke(view);

		assertTrue(view.getStartingDeviceJspUrl().endsWith("ua=phone"));
		assertEquals(view.getContextUrl(), view.getContextUrl());
	}
}
