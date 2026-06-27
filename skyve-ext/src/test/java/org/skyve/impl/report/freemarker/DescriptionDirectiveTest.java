package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.HashMap;
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
class DescriptionDirectiveTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void executeRejectsMissingParametersLoopVariablesAndNestedBody() {
		DescriptionDirective directive = new DescriptionDirective();
		Map<String, TemplateModel> params = Map.of("binding", scalar("name"));
		TemplateDirectiveBody body = env -> {
			// no-op
		};

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], body));
	}

	@Test
	void executeRejectsInvalidParameterTypes() {
		DescriptionDirective directive = new DescriptionDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("bean", scalar("not a bean")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("binding", bool(true)), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("escape", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("other", scalar("value")), new TemplateModel[0], null));
	}

	@Test
	void executeAcceptsEscapeParameterTypesBeforeRequiringEnvironment() {
		DescriptionDirective directive = new DescriptionDirective();
		Map<String, TemplateModel> scalarEscape = Map.of("escape", scalar("false"));
		Map<String, TemplateModel> booleanEscape = Map.of("escape", bool(false));

		assertThrows(NullPointerException.class, () -> directive.execute(null, scalarEscape, new TemplateModel[0], null));
		assertThrows(NullPointerException.class, () -> directive.execute(null, booleanEscape, new TemplateModel[0], null));
	}

	@Test
	void templateWritesEscapedAttributeDescriptionByDefault() throws Exception {
		bindPersistenceWithCustomer(fixtureCustomer());
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>());

		String output = process("<@description bean=bean binding=\"customerName\" />",
				Map.of("bean", bean, "description", new DescriptionDirective()));

		assertEquals("Shown &lt;strong&gt;on invoices&lt;/strong&gt;", output);
	}

	@Test
	void templateWritesRawAttributeDescriptionWhenEscapeDisabled() throws Exception {
		bindPersistenceWithCustomer(fixtureCustomer());
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>());

		String output = process("<@description bean=bean binding=\"customerName\" escape=false />",
				Map.of("bean", bean, "description", new DescriptionDirective()));

		assertEquals("Shown <strong>on invoices</strong>", output);
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateBooleanModel bool(boolean value) {
		return () -> value;
	}

	private static TemplateModel marker() {
		return new TemplateModel() {
			// marker model only
		};
	}

	private static String process(String templateText, Map<String, Object> model) throws Exception {
		Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);
		Template template = new Template("descriptionDirective", new StringReader(templateText), cfg);
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
		attribute.setName("customerName");
		attribute.setDisplayName("Customer Name");
		attribute.setDescription("Shown <strong>on invoices</strong>");
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
