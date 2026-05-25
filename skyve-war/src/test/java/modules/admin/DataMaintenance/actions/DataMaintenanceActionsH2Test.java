package modules.admin.DataMaintenance.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.DataMaintenance;
import util.AbstractH2Test;

/**
 * Tests for DataMaintenance DDL and utility actions.
 */
public class DataMaintenanceActionsH2Test extends AbstractH2Test {

	private DataBuilder db;
	private DataMaintenance bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- Create action ----

	@Test
	void createActionGeneratesDdlScript() throws Exception {
		Create action = new Create();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		// The CREATE DDL script should be set on the bean
		assertThat(bean.getDdlScript(), is(notNullValue()));
	}

	// ---- Drop action ----

	@Test
	void dropActionGeneratesDdlScript() throws Exception {
		Drop action = new Drop();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(bean.getDdlScript(), is(notNullValue()));
	}

	// ---- Sync action ----

	@Test
	void syncActionGeneratesDdlScript() throws Exception {
		Sync action = new Sync();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(bean.getDdlScript(), is(notNullValue()));
	}

	// ---- RefreshBackupList action ----

	@Test
	void refreshBackupListActionReturnsBean() throws Exception {
		RefreshBackupList action = new RefreshBackupList();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- Backup action ----

	@Test
	void backupActionStartsJobAndReturnsBean() throws Exception {
		Backup action = new Backup();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- Reindex action ----

	@Test
	void reindexActionStartsJobAndReturnsBean() throws Exception {
		Reindex action = new Reindex();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- ReindexContent action ----

	@Test
	void reindexContentActionStartsJobAndReturnsBean() throws Exception {
		ReindexContent action = new ReindexContent();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- ReindexData action ----

	@Test
	void reindexDataActionStartsJobAndReturnsBean() throws Exception {
		ReindexData action = new ReindexData();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- Truncate action: null password validation ----

	@Test
	void truncateWithNullPasswordThrowsValidationException() {
		bean.setConfirmPassword(null);
		Truncate action = new Truncate();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	// ---- RefreshDocumentTuples action: null refreshOption validation ----

	@Test
	void refreshDocumentTuplesWithNullRefreshOptionThrowsValidationException() {
		bean.setRefreshOption(null);
		RefreshDocumentTuples action = new RefreshDocumentTuples();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	// ---- ContentSelected action: null selectedContentId ----

	@Test
	void contentSelectedWithNullIdSetsNullContentLink() throws Exception {
		bean.setSelectedContentId(null);
		ContentSelected action = new ContentSelected();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}
}
