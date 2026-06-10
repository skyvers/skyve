package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings({"static-method", "unchecked"})
class FormatDirectiveTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void executeRejectsMissingParameters() {
		FormatDirective directive = new FormatDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
	}

	@Test
	void executeRejectsLoopVariables() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("binding", scalar("name"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
	}

	@Test
	void executeRejectsNestedBody() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("binding", scalar("name"));
		TemplateDirectiveBody body = env -> {
			// no-op
		};

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], body));
	}

	@Test
	void executeRejectsNullBeanParameter() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = new LinkedHashMap<>();
		params.put("bean", nullModel());

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonBeanParameter() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = new LinkedHashMap<>();
		params.put("bean", scalar("not a bean"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonScalarBinding() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("binding", bool(true));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonScalarExpression() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("expression", bool(true));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsBothBindingAndExpression() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = new LinkedHashMap<>();
		params.put("binding", scalar("name"));
		params.put("expression", scalar("{name}"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsNonBooleanEscape() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("escape", new TemplateModel() {
			// marker model only
		});

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void executeRejectsUnsupportedParameter() {
		FormatDirective directive = new FormatDirective();
		Map<String, TemplateModel> params = Map.of("other", scalar("value"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void templateFormatsExpressionWithEscapingAndLineBreaks() throws Exception {
		bindPersistenceWithCustomer(fixtureCustomer());
		Map<String, Object> values = new HashMap<>();
		values.put("name", "<Ada>\nLovelace");
		DynamicBean bean = new DynamicBean("sales", "Order", values);

		String output = process("<@format bean=bean expression=\"Hello {name}\" />",
				Map.of("bean", bean, "format", new FormatDirective()));

		assertEquals("Hello &lt;Ada&gt;<br/>Lovelace", output);
	}

	@Test
	void templateFormatsExpressionWithoutEscapingWhenDisabled() throws Exception {
		bindPersistenceWithCustomer(fixtureCustomer());
		Map<String, Object> values = new HashMap<>();
		values.put("name", "<Ada>");
		DynamicBean bean = new DynamicBean("sales", "Order", values);

		String output = process("<@format bean=bean expression=\"Hello {name}\" escape=\"false\" />",
				Map.of("bean", bean, "format", new FormatDirective()));

		assertEquals("Hello <Ada>", output);
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateBooleanModel bool(boolean value) {
		return () -> value;
	}

	private static TemplateModel nullModel() {
		return new TemplateModel() {
			// DeepUnwrap treats an unknown marker model as null.
		};
	}

	private static String process(String templateText, Map<String, Object> model) throws Exception {
		Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);
		Template template = new Template("formatDirective", new StringReader(templateText), cfg);
		StringWriter out = new StringWriter();
		template.process(model, out);
		return out.toString();
	}

	private static Customer fixtureCustomer() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("sales");
		document.setName("Order");
		Text attribute = new Text();
		attribute.setName("name");
		attribute.setDisplayName("Name");
		attribute.setLength(100);
		document.putAttribute(attribute);

		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		return customer;
	}

	private static void bindPersistenceWithCustomer(Customer customer) throws Exception {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
