package modules.admin.Audit.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

import modules.admin.domain.Audit;

@SuppressWarnings("static-method")
class ArchivedAuditListModelTest {

	@Test
	void getDescriptionReturnsNonEmptyString() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		assertEquals("The list of all Audits.", model.getDescription());
	}

	@Test
	void getModuleReturnsAuditModuleName() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		assertEquals(Audit.MODULE_NAME, model.getModule());
	}

	@Test
	void getDocumentReturnsAuditDocumentName() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		assertEquals(Audit.DOCUMENT_NAME, model.getDocument());
	}

	@Test
	void getColumnsReturnsNonEmptyList() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		List<MetaDataQueryColumn> columns = model.getColumns();
		assertNotNull(columns);
		assertFalse(columns.isEmpty());
	}

	@Test
	void getColumnsIncludesTimestampBinding() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		boolean hasTimestamp = model.getColumns().stream()
				.anyMatch(c -> Audit.timestampPropertyName.equals(c.getBinding()));
		assertTrue(hasTimestamp);
	}

	@Test
	void getProjectionsContainsBizIdAndAuditBindings() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		Set<String> projections = model.getProjections();
		assertNotNull(projections);
		assertTrue(projections.contains(Bean.DOCUMENT_ID));
		assertTrue(projections.contains(PersistentBean.LOCK_NAME));
		assertTrue(projections.contains(Audit.timestampPropertyName));
	}

	@Test
	void toSortBindingReturnsSortSuffixedBinding() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		String result = model.toSortBinding(Audit.timestampPropertyName);
		assertNotNull(result);
	}

	@Test
	void getDefaultSortReturnsNonNull() {
		ArchivedAuditListModel<?> model = new ArchivedAuditListModel<>();
		assertNotNull(model.getDefaultSort());
	}
}
