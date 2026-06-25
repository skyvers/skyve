package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;

@SuppressWarnings("static-method")
class MetaDataServletViewTitleEscapingTest {
	@Test
	void jsonViewTitleEscapesJsonSyntaxButLeavesHtmlTextRaw() {
		ViewImpl view = new ViewImpl();
		view.setTitle("<b>Title</b> \"quoted\"\nnext");

		assertEquals("<b>Title</b> \\\"quoted\\\"\\nnext", MetaDataServlet.jsonViewTitle(view));
	}

	@Test
	void jsonViewTitleRemainsRawHtmlTextWhenEscapeTitleIsTrue() {
		ViewImpl view = new ViewImpl();
		view.setTitle("<i>Title</i>");
		view.setEscapeTitle(Boolean.TRUE);

		assertEquals("<i>Title</i>", MetaDataServlet.jsonViewTitle(view));
	}

	@Test
	void processParameterizableAppendsLiteralAndBindingParameters() throws Exception {
		StringBuilder json = new StringBuilder();
		Parameter literal = parameter("literal", "value \"quoted\"", null);
		Parameter binding = parameter("binding", null, "customer.name");
		Parameterizable parameterizable = () -> List.of(literal, binding);

		invokeStaticAppender("processParameterizable", Parameterizable.class, parameterizable, json);

		assertEquals(",\"parameters\":[{\"name\":\"literal\",\"value\":\"value \\\"quoted\\\"\"},"
						+ "{\"name\":\"binding\",\"valueBinding\":\"customer.name\"}]",
						json.toString());
	}

	@Test
	void processParameterizableDoesNotAppendWhenThereAreNoParameters() throws Exception {
		StringBuilder json = new StringBuilder("prefix");

		invokeStaticAppender("processParameterizable", Parameterizable.class, (Parameterizable) List::of, json);

		assertEquals("prefix", json.toString());
	}

	@Test
	void processDecoratedAppendsProperties() throws Exception {
		StringBuilder json = new StringBuilder();
		DecoratedMetaData decorated = () -> Map.of("alpha", "one", "beta", "two");

		invokeStaticAppender("processDecorated", DecoratedMetaData.class, decorated, json);

		String result = json.toString();
		assertTrue(result.startsWith(",\"properties\":{"));
		assertTrue(result.contains("\"alpha\":\"one\""));
		assertTrue(result.contains("\"beta\":\"two\""));
		assertTrue(result.endsWith("}"));
	}

	@Test
	void processInvisibleAndDisableableAppendConditionNames() throws Exception {
		StringBuilder json = new StringBuilder();

		invokeStaticAppender("processInvisible", Invisible.class, invisible("hideWhenArchived"), json);
		invokeStaticAppender("processDisableable", Disableable.class, disableable("disableWhenLocked"), json);

		assertEquals(",\"invisibleConditionName\":\"hideWhenArchived\","
						+ "\"disabledConditionName\":\"disableWhenLocked\"",
						json.toString());
	}

	@Test
	void emptyResponseReturnsViewOrMetadataShape() throws Exception {
		Method emptyResponse = MetaDataServlet.class.getDeclaredMethod("emptyResponse", String.class);
		emptyResponse.setAccessible(true);

		assertEquals("{\"type\":\"view\",\"name\":\"edit\",\"contained\":[],\"title\":\"Missing\"}",
						emptyResponse.invoke(null, "Missing"));
		assertEquals("{\"menus\":[],\"dataSources\":[]}", emptyResponse.invoke(null, new Object[] { null }));
	}

	private static void invokeStaticAppender(String methodName, Class<?> parameterType, Object value, StringBuilder json)
	throws Exception {
		Method method = MetaDataServlet.class.getDeclaredMethod(methodName, parameterType, StringBuilder.class);
		method.setAccessible(true);
		method.invoke(null, value, json);
	}

	private static Parameter parameter(String name, String value, String valueBinding) {
		return new Parameter() {
			@Override
			public String getName() {
				return name;
			}

			@Override
			public String getValue() {
				return value;
			}

			@Override
			public String getValueBinding() {
				return valueBinding;
			}
		};
	}

	private static Invisible invisible(String conditionName) {
		return new Invisible() {
			@Override
			public String getInvisibleConditionName() {
				return conditionName;
			}

			@Override
			public void setInvisibleConditionName(String invisibleConditionName) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void setVisibleConditionName(String visibleConditionName) {
				throw new UnsupportedOperationException();
			}
		};
	}

	private static Disableable disableable(String conditionName) {
		return new Disableable() {
			@Override
			public String getDisabledConditionName() {
				return conditionName;
			}

			@Override
			public void setDisabledConditionName(String disabledConditionName) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void setEnabledConditionName(String disabledConditionName) {
				throw new UnsupportedOperationException();
			}
		};
	}
}
