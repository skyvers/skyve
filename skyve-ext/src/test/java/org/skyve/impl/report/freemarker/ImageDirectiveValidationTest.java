package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateNumberModel;
import freemarker.template.TemplateScalarModel;

@SuppressWarnings({"static-method", "unchecked", "boxing"})
class ImageDirectiveValidationTest {
	@TempDir
	Path tempDir;

	@Test
	void dynamicImageRejectsDirectiveShapeProblems() {
		DynamicImageDirective directive = new DynamicImageDirective();
		Map<String, TemplateModel> params = Map.of("image", scalar("chart"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], mock(TemplateDirectiveBody.class)));
	}

	@Test
	void dynamicImageRejectsUnsupportedOrWronglyTypedParameters() {
		DynamicImageDirective directive = new DynamicImageDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("other", scalar("x")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("image", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("document", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("module", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("bean", scalar("not-a-bean")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("height", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("height", scalar("abc")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("width", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("width", scalar("abc")), new TemplateModel[0], null));
	}

	@Test
	void dynamicImageRejectsMissingRequiredParametersAfterParsingOptionalNumbers() {
		DynamicImageDirective directive = new DynamicImageDirective();
		Map<String, TemplateModel> params = new HashMap<>();
		params.put("height", number(120));
		params.put("width", number(240));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));

		params.put("image", scalar("chart"));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));

		params.put("document", scalar("Document"));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));

		params.put("module", scalar("module"));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void contentRejectsDirectiveShapeProblems() {
		ContentDirective directive = new ContentDirective();
		Map<String, TemplateModel> params = Map.of("attribute", scalar("image"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], mock(TemplateDirectiveBody.class)));
	}

	@Test
	void contentRejectsUnsupportedOrWronglyTypedParameters() {
		ContentDirective directive = new ContentDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("other", scalar("x")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("bean", scalar("not-a-bean")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("module", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("document", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("attribute", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("height", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("width", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("class", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("style", raw()), new TemplateModel[0], null));
	}

	@Test
	void contentRejectsMissingRequiredParametersAfterParsingOptionalValues() {
		ContentDirective directive = new ContentDirective();
		Map<String, TemplateModel> params = new HashMap<>();
		params.put("height", number(120));
		params.put("width", number(240));
		params.put("class", scalar("avatar"));
		params.put("style", scalar("display:block"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));

		params.put("attribute", scalar("image"));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));

		params.put("document", scalar("Document"));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));

		params.put("module", scalar("module"));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], null));
	}

	@Test
	void imageRejectsDirectiveShapeProblems() {
		ImageDirective directive = new ImageDirective();
		Map<String, TemplateModel> params = Map.of("filename", scalar("logo.png"));

		assertThrows(TemplateModelException.class, () -> directive.execute(null, Map.of(), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[] { scalar("loop") }, null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params, new TemplateModel[0], mock(TemplateDirectiveBody.class)));
	}

	@Test
	void imageRejectsUnsupportedOrWronglyTypedParameters() {
		ImageDirective directive = new ImageDirective();

		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("other", scalar("x")), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("filename", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("module", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("alt", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("height", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("width", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("class", raw()), new TemplateModel[0], null));
		assertThrows(TemplateModelException.class, () -> directive.execute(null, params("style", raw()), new TemplateModel[0], null));
	}

	@Test
	void imageRendersResourceAsDataUrlWithOptionalAttributes() throws Exception {
		Path logo = tempDir.resolve("logo.png");
		Files.write(logo, "png".getBytes());
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.findResourceFile("logo.png", "bizhub", "admin")).thenReturn(logo.toFile());
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("bizhub");

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		try {
			ProvidedRepositoryFactory.set(repository);
			bindPersistenceWithCustomer(customer);

			String output = renderImageDirective("<@image filename='logo.png' module='admin' alt='Logo' height=12 width='34' class='brand' style='display:block' />");

			assertTrue(output.startsWith("<img src='data:image/png;base64,"));
			assertTrue(output.contains("cG5n"));
			assertTrue(output.contains(" alt='Logo'"));
			assertTrue(output.contains(" height='12'"));
			assertTrue(output.contains(" width='34'"));
			assertTrue(output.contains(" class='brand'"));
			assertTrue(output.contains(" style='display:block'"));
		}
		finally {
			restoreRepository(previousRepository);
			unbindPersistenceFromThread();
		}
	}

	@Test
	void dynamicImageRendersGeneratedImageWithDimensions() throws Exception {
		Bean bean = mock(Bean.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DynamicImage<Bean> dynamicImage = mock(DynamicImage.class);
		BufferedImage image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(document.getDynamicImage(customer, "Chart")).thenReturn(dynamicImage);
		when(dynamicImage.getImage(bean, 20, 16, user)).thenReturn(image);
		when(dynamicImage.getFormat()).thenReturn(ImageFormat.png);

		try {
			bindPersistenceWithUser(user);

			String output = renderDirective("dynamicImage",
					new DynamicImageDirective(),
					"<@dynamicImage image='Chart' module='admin' document='User' bean=user height='16' width=20 />",
					Map.of("user", bean));

			assertTrue(output.startsWith("<img src='data:image/png;base64,"));
			assertTrue(output.contains(" height='16'"));
			assertTrue(output.contains(" width='20'"));
		}
		finally {
			unbindPersistenceFromThread();
		}
	}

	private static Map<String, TemplateModel> params(String name, TemplateModel value) {
		return Map.of(name, value);
	}

	private static TemplateModel raw() {
		return mock(TemplateModel.class);
	}

	private static TemplateScalarModel scalar(String value) {
		return () -> value;
	}

	private static TemplateNumberModel number(Number value) {
		return () -> value;
	}

	private static String renderImageDirective(String templateText) throws Exception {
		return renderDirective("image", new ImageDirective(), templateText, Map.of());
	}

	private static String renderDirective(String directiveName, Object directive, String templateText, Map<String, Object> model) throws Exception {
		Configuration cfg = new Configuration(Configuration.VERSION_2_3_32);
		cfg.setLocale(Locale.ROOT);
		cfg.setSharedVariable(directiveName, directive);
		Template template = new Template(directiveName, new StringReader(templateText), cfg);
		StringWriter out = new StringWriter();
		template.process(model, out);
		return out.toString();
	}

	private static void bindPersistenceWithCustomer(Customer customer) throws Exception {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		bindPersistenceWithUser(user);
	}

	private static void bindPersistenceWithUser(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		bindPersistenceToThread(persistence);
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	private static void restoreRepository(ProvidedRepository previousRepository) {
		if (previousRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}
}
