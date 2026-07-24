package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;

@SuppressWarnings({"static-method", "java:S1192", "java:S3011"})
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
	void postConstructBuildsDeviceCommandPreviewPrefix() throws Exception {
		DeviceView view = initialise(Map.of(
				"m", new String[] {"admin"},
				"d", new String[] {"User"}), Locale.forLanguageTag("ar"));

		String prefix = view.getPreviewUrlPrefix();
		assertTrue(prefix.contains("/device.jsp?"));
		assertTrue(prefix.contains("m=admin"));
		assertTrue(prefix.contains("d=User"));
		assertTrue(prefix.endsWith("_ua="));
		assertTrue(view.getEndPreviewUrl().endsWith("/device.jsp?_ua="));
		assertFalse(view.getEndPreviewUrl().contains("m=admin"));
		assertEquals("rtl", view.getDir());
		assertEquals("ar", view.getLanguageTag());
	}

	@Test
	void postConstructUsesOnlyTheCanonicalEmulationParameter() throws Exception {
		Map<String, String[]> params = new LinkedHashMap<>();
		params.put("重复 名", new String[] {"välue", "", "a&b=c#d%", "quote'\\\""});
		params.put("_ua", new String[] {"phone", "tablet"});
		params.put("ua", new String[] {"application-value"});

		DeviceView view = initialise(params, Locale.ENGLISH);
		String prefix = view.getPreviewUrlPrefix();

		assertTrue(prefix.contains("%E9%87%8D%E5%A4%8D+%E5%90%8D=v%C3%A4lue"));
		assertTrue(prefix.contains("%E9%87%8D%E5%A4%8D+%E5%90%8D="));
		assertTrue(prefix.contains("%E9%87%8D%E5%A4%8D+%E5%90%8D=a%26b%3Dc%23d%25"));
		assertTrue(prefix.contains("%E9%87%8D%E5%A4%8D+%E5%90%8D=quote%27%5C%22"));
		assertTrue(prefix.contains("ua=application-value"));
		assertTrue(prefix.endsWith("_ua="));
		assertEquals(prefix.indexOf("_ua="), prefix.lastIndexOf("_ua="));
		assertFalse(prefix.contains("\\\";"));
	}

	private static DeviceView initialise(Map<String, String[]> params, Locale locale) throws Exception {
		DeviceView view = new DeviceView();
		FacesContext context = mock(FacesContext.class);
		ExternalContext externalContext = mock(ExternalContext.class);
		when(context.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestParameterValuesMap()).thenReturn(params);
		when(externalContext.getRequestLocale()).thenReturn(locale);
		FacesContextBridge.setCurrent(context);

		Method postConstruct = DeviceView.class.getDeclaredMethod("postConstruct");
		postConstruct.setAccessible(true);
		postConstruct.invoke(view);
		return view;
	}
}
