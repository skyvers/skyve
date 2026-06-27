package modules.admin.DataMaintenance.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.DataMaintenance.DataMaintenanceExtension;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.RestoreIndexingOption;
import modules.admin.domain.DataMaintenance.RestorePreProcess;

@SuppressWarnings("static-method")
class RestoreTest {
	@Test
	void executeWithMissingContentRestoreOptionThrowsValidationException() {
		TestableRestore action = new TestableRestore();
		DataMaintenanceExtension bean = validBean();
		bean.setContentRestoreOption(null);

		ValidationException exception = assertThrows(ValidationException.class, () -> action.execute(bean, null));

		assertThat(exception.getMessages().iterator().next().getText(), containsString("contentRestoreOption display"));
		assertEquals(0, action.restoreCount);
	}

	@Test
	void executeWithMissingRestoreIndexingOptionThrowsValidationException() {
		TestableRestore action = new TestableRestore();
		DataMaintenanceExtension bean = validBean();
		bean.setRestoreIndexingOption(null);

		ValidationException exception = assertThrows(ValidationException.class, () -> action.execute(bean, null));

		assertThat(exception.getMessages().iterator().next().getText(), containsString("restoreIndexingOption display"));
		assertEquals(0, action.restoreCount);
	}

	@Test
	void executeWithMissingRestorePreProcessThrowsValidationException() {
		TestableRestore action = new TestableRestore();
		DataMaintenanceExtension bean = validBean();
		bean.setRestorePreProcess(null);

		ValidationException exception = assertThrows(ValidationException.class, () -> action.execute(bean, null));

		assertThat(exception.getMessages().iterator().next().getText(), containsString("restorePreProcess display"));
		assertEquals(0, action.restoreCount);
	}

	@Test
	void executeWithRequiredOptionsStartsRestoreAndGrowls() throws Exception {
		TestableRestore action = new TestableRestore();
		DataMaintenanceExtension bean = validBean();

		ServerSideActionResult<DataMaintenance> result = action.execute(bean, null);

		assertSame(bean, result.getBean());
		assertEquals(1, action.restoreCount);
		assertEquals(1, action.growlCount);
	}

	private static DataMaintenanceExtension validBean() {
		DataMaintenanceExtension bean = new DataMaintenanceExtension();
		bean.setContentRestoreOption(ContentRestoreOption.error);
		bean.setRestoreIndexingOption(RestoreIndexingOption.both);
		bean.setRestorePreProcess(RestorePreProcess.createTablesFromMetadata);
		return bean;
	}

	private static class TestableRestore extends Restore {
		private int restoreCount;
		private int growlCount;

		@Override
		protected String attributeDisplayName(DataMaintenance bean, String attributeName) {
			return attributeName + " display";
		}

		@Override
		protected String i18n(String key, String... args) {
			return key + " " + args[0];
		}

		@Override
		protected void restore(DataMaintenance bean) {
			restoreCount++;
		}

		@Override
		protected void growl(WebContext webContext) {
			growlCount++;
		}
	}
}
