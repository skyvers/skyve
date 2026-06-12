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
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings({"static-method", "unchecked"})
class DisplayNameDirectiveTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void executeRejectsMissingParametersLoopVariablesAndNestedBody() {
		DisplayNameDirective directive = new DisplayNameDirective();
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
		DisplayNameDirective directive = new DisplayNameDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("bean", scalar("not a bean")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("binding", marker()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of("other", scalar("value")), new TemplateModel[0], null));
	}

	@Test
	void executeAcceptsBindingBeforeRequiringEnvironment() {
		DisplayNameDirective directive = new DisplayNameDirective();
		Map<String, TemplateModel> params = Map.of("binding", scalar("name"));

		assertThrows(NullPointerException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void templateWritesAttributeDisplayName() throws Exception {
		bindPersistenceWithCustomer(fixtureCustomer());
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>());

		String output = process("<@displayName bean=bean binding=\"customerName\" />",
				Map.of("bean", bean, "displayName", new DisplayNameDirective()));

		assertEquals("Customer Name", output);
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateModel marker() {
		return new TemplateModel() {
			// marker model only
		};
	}

	private static String process(String templateText, Map<String, Object> model) throws Exception {
		Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);
		Template template = new Template("displayNameDirective", new StringReader(templateText), cfg);
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
		attribute.setDescription("Shown on invoices");
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
