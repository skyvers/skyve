package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.inject.Inject;
import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailTestStrategy;
import modules.admin.domain.ControlPanel.SailUserAgentType;
import util.AbstractH2Test;

/**
 * H2-backed tests for ControlPanel GenerateTestData, DeleteTestData actions,
 * and GenerateSAIL validation.
 */
class ControlPanelGenerateActionsH2Test extends AbstractH2Test {

	@Inject
	private GenerateMenuSAIL generateMenuSAIL;

	private DataBuilder db;
	private ControlPanelExtension bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- GenerateSAIL validation: missing user ----

	@Test
	void generateSAILExecuteWithNullUserThrowsValidationException() {
		bean.setSailUser(null);
		bean.setSailUxUi("sc");
		bean.setSailUserAgentType(SailUserAgentType.desktop);
		bean.setSailTestStrategy(SailTestStrategy.None);

		assertThrows(ValidationException.class, () -> generateMenuSAIL.execute(bean, webContext));
	}

	@Test
	void generateSAILExecuteWithNullUxUiThrowsValidationException() {
		bean.setSailUxUi(null);
		bean.setSailUserAgentType(SailUserAgentType.desktop);
		bean.setSailTestStrategy(SailTestStrategy.None);

		assertThrows(ValidationException.class, () -> generateMenuSAIL.execute(bean, webContext));
	}

	@Test
	void generateSAILExecuteWithNullUserAgentTypeThrowsValidationException() {
		bean.setSailUxUi("sc");
		bean.setSailUserAgentType(null);
		bean.setSailTestStrategy(SailTestStrategy.None);

		assertThrows(ValidationException.class, () -> generateMenuSAIL.execute(bean, webContext));
	}

	@Test
	void generateSAILExecuteWithNullTestStrategyThrowsValidationException() {
		bean.setSailUxUi("sc");
		bean.setSailUserAgentType(SailUserAgentType.desktop);
		bean.setSailTestStrategy(null);

		assertThrows(ValidationException.class, () -> generateMenuSAIL.execute(bean, webContext));
	}

	// ---- GenerateTestData validation ----

	@Test
	void generateTestDataWithNullModuleNameThrowsValidationException() {
		bean.setTestModuleName(null);
		bean.setTestNumberToGenerate(Integer.valueOf(10));

		GenerateTestData action = new GenerateTestData();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	@Test
	void generateTestDataWithEmptyDocumentNamesThrowsValidationException() {
		bean.setTestModuleName("admin");
		bean.getTestDocumentNames().clear();
		bean.setTestNumberToGenerate(Integer.valueOf(10));

		GenerateTestData action = new GenerateTestData();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	@Test
	void generateTestDataWithInvalidNumberToGenerateThrowsValidationException() {
		Integer[] invalidNumbers = {null, Integer.valueOf(0), Integer.valueOf(10001)};
		for (Integer invalidNumber : invalidNumbers) {
			bean.setTestModuleName("admin");
			bean.setTestNumberToGenerate(invalidNumber);

			GenerateTestData action = new GenerateTestData();
			assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
		}
	}

	@Test
	void generateTestDataWithTagFlagButNullTagNameThrowsValidationException() {
		bean.setTestModuleName("admin");
		bean.setTestNumberToGenerate(Integer.valueOf(10));
		bean.setTestTagGeneratedData(Boolean.TRUE);
		bean.setTestTagName(null);

		GenerateTestData action = new GenerateTestData();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	// ---- DeleteTestData validation ----

	@Test
	void deleteTestDataWithNullTagNameThrowsValidationException() {
		bean.setTestTagName(null);

		DeleteTestData action = new DeleteTestData();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	@Test
	void deleteTestDataWithNonExistentTagThrowsValidationException() {
		bean.setTestTagName("non-existent-tag-12345");

		DeleteTestData action = new DeleteTestData();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}
}
